#lang racket/base

;; sigh... it turns out dovecot actually uses mboxcl2 format. In this format, there's a
;; Content-Length header that tells you how many bytes are in the body. This should probably
;; actually be faster, because it means you can seek directly without parsing.

;; big question: is the Content-Length header guaranteed to be the last header? I think so...

(require racket/contract
         racket/stream
         racket/match
         racket/string
         net/head)

(provide/contract [mboxcl2-parse
                   (-> path?
                       (values (-> void?)
                               (stream/c (list/c bytes? (-> bytes?)))))])
  
;; mboxrd-parse : 
;; given a path to an mbox file, return a lazy list of the messages in the 
;; file.  Each file is represented as a list containing a byte-string
;; representing the header and the promise of a byte-string representing 
;; the body.  These byte-strings can be appended to obtain the original
;; message except that every \n in the original is replaced by \r\n to
;; match the RFC 2822 format.
(define (mboxcl2-parse path)
  (define ip (open-input-file path))
  (cond
    ;; extra check necessary because of add1 below:
    [(eof-object? (peek-char ip))
     (close-input-port ip)
     (values (λ () (void))
             empty-stream)]
    [else
     (values
      (lambda ()
        (close-input-port ip))
      (let loop ([port-posn 0])
        ;; this extra character is apparently for the \n that separates
        ;; messages. If the whole file is empty this will break.
        (file-position ip (add1 port-posn))
        (cond
          [(eof-object? (peek-char ip))
           empty-stream]
          [else
           (define headers-port (open-output-bytes))
           ;; search for the end of the headers
           (define match-result (regexp-match #px"\n\n|\n$" ip 0 #f headers-port))
           (unless match-result
             (error 'mboxrd-parse/port
                    "couldn't find blank line separating header from body:\n ~a"
                    (get-output-bytes headers-port)))
           (define empty-body (equal? match-result `(#"\n")))
           (define headers
             (bytes-append
              (regexp-replace* #px#"\n"
                               (get-output-bytes headers-port)
                               #"\r\n")
              #"\r\n\r\n"))
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
                 (stream-cons (list headers body-thunk)
                              (loop (+ body-posn body-length)))])])])))]))





;; TESTING

#;(module+ test
  (require rackunit)
  (define tstr"From oohc
Content-Length: 23

123456789012345678901

From zabba
Content-Length: 9

123456789")
  


  (check-equal?
   (stream->list (mboxrd-parse/port (open-input-string "")))
   null)

  
  ;; this is what passes for a test case:
  (check-equal?
   (let ([ip (open-input-string tstr)])
     (stream->list (mboxrd-parse/port ip)))
   (list (list #"From oohc\r\nlala\r\n\r\n"
               #"tropo\r\n")
         (list #"From 13\r\nFrom obetor\r\n>From oherot\r\n\r\n" 
               #"")
         (list #"From 15\r\n\r\n"
               #"")
         (list #"From 14\r\n\r\n"
               #"a\r\n\r\nb\r\n")))

  (check-exn
   #px"nonempty mbox file did not begin with"
   (lambda ()
     (mboxrd-parse/port (open-input-string "abcdefFrom "))))

  (check-exn
   #px"couldn't find blank line"
   (lambda ()
     (mboxrd-parse/port
      (open-input-string "From my dad
To: Your Mom
Subject: get to work!
From a big brown cow
To: Betsy")))))

