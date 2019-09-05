#lang racket/base

;; okay, now I moved everything to maildir, and I want the same interface. I'm still
;; using dovecot. Hopefully, this should make this version of the code much shorter.

(require racket/contract
         racket/stream
         racket/match
         racket/string
         net/head)

(provide/contract [maildir-parse
                   (->* (path-string?)
                        ()
                        (stream/c (list/c bytes? (-> bytes?))))])

;; given a path to a maildir, return a lazy list of the messages in the 
;; file.  Each file is represented as a list containing a byte-string
;; representing the header and the promise of a byte-string representing 
;; the body.  These byte-strings can be appended to obtain the original
;; message except that every \n in the original is replaced by \r\n to
;; match the RFC 2822 format.
(define (mboxcl2-parse path)
  (unless (and (directory-exists? path)
               (directory-exists? (build-path path "new"))
               (directory-exists? (build-path path "cur")))
    (raise-argument-error 'maildir-parse
                          "name of existing directory with new and cur subdirectories"
                          0 path))
  (define mail-file-list
    (filter file-exists?
            (append
             (directory-list (build-path path "new") #:build #t)
             (directory-list (build-path path "cur") #:build #t))))
  ;; RIGHT HERE
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
          (unless (regexp-match #px"^(\r\n)?From " headers)
            (error 'mboxcl2-parse
                   "header doesn't start with 'From ': ~e"
                   headers))
          (cond
            [(equal? headers #"")
             empty-stream]
            [else
             (define body-posn (file-position ip))
             (define body-length
               (match (extract-field #"Content-Length" headers)
                 [#f
                  (cond [fallback?
                         (- (scan-for-next-from ip)
                            body-posn)]
                        [else
                         (error 'mboxcl2-parse
                                "no content-length header found in headers: ~v\n"
                                headers)])]
                 [len-str
                  (string->number
                   (string-trim
                    (bytes->string/utf-8 len-str)))]))             
             (define (body-thunk)
               (file-position ip body-posn)
               (read-bytes body-length ip))
             ;; this extra character is apparently for the \n that separates
             ;; messages. If the whole file is empty this will break.
             (stream-cons (list headers body-thunk)
                          (loop (+ body-posn body-length 1)))])]))]))

;; given an input port and a loop-continuation function, scan for the next
;; message beginning (a.k.a. the next #px"\rFrom ". Return the position of
;; the end of the body.
(define (scan-for-next-from port)
  (match (regexp-match #px"\nFrom " port)
    [#f (sub1 (file-position port))]
    [other (- (file-position port) 6)]))



;; TESTING


  
  ;; is this a procedure that produces a byte-string of
  ;; the given length?
(define (bytes-thunk-of-len len)
  (λ (p)
    (define b (p))
    (and (bytes? b) (= (bytes-length b) len))))

(module+ test
  (require rackunit
           racket/block)

  
  
  (check-equal?
   (stream->list (mboxcl2-parse/port (open-input-string "") #f))
   null)
  
  (define tstr
    "From oohc
Content-Length: 22

123456789012345678901

From zabba
Content-Length: 10

123456789

")
  
  (check-match
   (let ([ip (open-input-string tstr)])
     (stream->list (mboxcl2-parse/port ip #f)))
   (list (list #"From oohc\r\nContent-Length: 22\r\n\r\n"
               (? (bytes-thunk-of-len 22) _p1))
         (list #"From zabba\r\nContent-Length: 10\r\n\r\n"
               (? (bytes-thunk-of-len 10) _p2))))

  (block
   ;; omitting content-length:
   (define tstr
     "From oohc

123456789012345678901

From zabba

123456789

")
   
   (check-match
    (let ([ip (open-input-string tstr)])
      (stream->list (mboxcl2-parse/port ip #t)))
    (list (list #"From oohc\r\n\r\n"
                (? (bytes-thunk-of-len 22) _p1))
          (list #"From zabba\r\n\r\n"
                (? (bytes-thunk-of-len 10) _p2)))))

  (check-match
   (let ([ip (open-input-string tstr)])
     (map (λ(x) ((cadr x)))
          (stream->list (mboxcl2-parse/port ip #f))))
   (list #"123456789012345678901\n"
         #"123456789\n"))


  ;; wrong content-length bug:
  (check-exn
   #px"header doesn't start with 'From '"
   (λ() (stream->list
         (mboxcl2-parse/port
          (open-input-string "From oohc
Content-Length: 23

123456789012345678901

From zabba
Content-Length: 10

123456789

")
          #f))))

  (check-exn
   #px"couldn't find blank line separating header from body"
   (lambda ()
     (mboxcl2-parse/port (open-input-string "abcdefFrom ") #f)))

  (check-exn
   #px"couldn't find blank line"
   (lambda ()
     (mboxcl2-parse/port
      (open-input-string "From my dad
To: Your Mom
Subject: get to work!
From a big brown cow
To: Betsy")
      #f))))
