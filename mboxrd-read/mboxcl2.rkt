#lang racket/base

;; sigh... it turns out dovecot actually uses mboxcl2 format. In this format, there's a
;; Content-Length header that tells you how many bytes are in the body. This should probably
;; actually be faster, because it means you can seek directly without parsing.

;; it appears that the Content-Length header is not necessarily last, but the length counted
;; does not include headers, only the body.


(require racket/contract
         racket/stream
         racket/match
         racket/string
         net/head)

(provide/contract [mboxcl2-parse
                   (-> path-string?
                       (values (-> void?)
                               (stream/c (list/c bytes? (-> bytes?)))))])

;; given a path to an mbox file, return a lazy list of the messages in the 
;; file.  Each file is represented as a list containing a byte-string
;; representing the header and the promise of a byte-string representing 
;; the body.  These byte-strings can be appended to obtain the original
;; message except that every \n in the original is replaced by \r\n to
;; match the RFC 2822 format.
(define (mboxcl2-parse path)
  (define port (open-input-file path))
  (values (λ () (close-input-port port))
          (mboxcl2-parse/port port)))

;; same, but operates on (random-access) ports.
(define (mboxcl2-parse/port ip)
  (with-handlers ([exn?
                   (λ (exn) (raise-argument-error
                             'mboxcl2-parse/port
                             "port with settable file-position"
                             0 ip))])
    (file-position ip 0))
  (cond
    ;; extra check necessary because of add1 below:
    [(eof-object? (peek-char ip))
     empty-stream]
    [else
     (let loop ([port-posn 0])
       (file-position ip port-posn)
       (cond
         [(eof-object? (peek-char ip))
          empty-stream]
         [else
          (define headers-port (open-output-bytes))
          ;; search for the end of the headers
          (define match-result (regexp-match #px#"\n\n|\n$" ip 0 #f headers-port))
          (unless match-result
            (error 'mboxrd-parse/port
                   "couldn't find blank line separating header from body:\n ~a"
                   (get-output-bytes headers-port)))
          (define headers
            (bytes-append
             (regexp-replace* #px#"\n"
                              (get-output-bytes headers-port)
                              #"\r\n")
             #"\r\n\r\n"))
          (unless (regexp-match #px"^From " headers)
            (error 'mboxcl2-parse
                   "header doesn't start with 'From ': ~e"
                   headers))
          (cond
            [(equal? headers #"")
             empty-stream]
            [else
             (match (extract-field #"Content-Length" headers)
               [#f (error 'mboxcl2-parse
                          "no content-length header found in headers: ~v\n"
                          headers)]
               [len-str
                (define body-length (string->number
                                     (string-trim
                                      (bytes->string/utf-8 len-str))))
                (define body-posn (file-position ip))               
                (define (body-thunk)
                  (file-position ip body-posn)
                  (read-bytes body-length ip))
                ;; this extra character is apparently for the \n that separates
                ;; messages. If the whole file is empty this will break.
                (stream-cons (list headers body-thunk)
                             (loop (+ body-posn body-length 1)))])])]))]))



;; TESTING

(module+ test
  (require rackunit)
  (define tstr"From oohc
Content-Length: 22

123456789012345678901

From zabba
Content-Length: 10

123456789

")
  


  (check-equal?
   (stream->list (mboxcl2-parse/port (open-input-string "")))
   null)
  
  
  (check-match
   (let ([ip (open-input-string tstr)])
     (stream->list (mboxcl2-parse/port ip)))
   (list (list #"From oohc\r\nContent-Length: 22\r\n\r\n"
               (? procedure? _p1))
         (list #"From zabba\r\nContent-Length: 10\r\n\r\n"
               (? procedure? _p2))))

  (check-match
   (let ([ip (open-input-string tstr)])
     (map (λ(x) ((cadr x)))
          (stream->list (mboxcl2-parse/port ip))))
   (list #"123456789012345678901\n"
         #"123456789\n"))


  ;; wrong content-length bug:
  (check-exn
   #px"header doesn't start with 'From '"
   (λ() (stream->list (mboxcl2-parse/port (open-input-string "From oohc
Content-Length: 23

123456789012345678901

From zabba
Content-Length: 10

123456789

")))))

  (check-exn
   #px"couldn't find blank line separating header from body"
   (lambda ()
     (mboxcl2-parse/port (open-input-string "abcdefFrom "))))

  (check-exn
   #px"couldn't find blank line"
   (lambda ()
     (mboxcl2-parse/port
      (open-input-string "From my dad
To: Your Mom
Subject: get to work!
From a big brown cow
To: Betsy")))))

