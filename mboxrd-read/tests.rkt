#lang racket

(require "main.rkt"
         "mboxcl2.rkt"
         "maildir.rkt"
         net/head
         rackunit
         racket/runtime-path)

;; this really just tests that a simple mbox file comes out with the right
;; number of messages.  Further testing would be nice...

(define-runtime-path here ".")
(define example-mbox (build-path here "test-mbox"))

(define mail-stream
  (mboxrd-parse example-mbox))

(let loop ([n 0] [stream mail-stream])
  (cond [(< n 5)
         (define first-mail (stream-first stream))
         (check-match
          first-mail
          (list (? bytes? b1) (? bytes? b2)))
         (validate-header
          (cadr
           (regexp-match #px"\r\n(.*)$" (first first-mail))))
         (loop (add1 n) (stream-rest stream))]
        [else
         (check-true (stream-empty? stream))]))

(define-values (closer mail-stream2)
  (mboxcl2-parse example-mbox #:fallback #t))

(let loop ([n 0] [stream mail-stream2])
  (cond [(< n 5)
         (define first-mail (stream-first stream))
         (check-match
          first-mail
          (list (? bytes? b1) body-thunk))
         (validate-header
          (cadr
           (regexp-match #px"\r\n(.*)$" (first first-mail))))
         (check-true (bytes? ((cadr first-mail))))
         (loop (add1 n) (stream-rest stream))]
        [else
         (check-true (stream-empty? stream))]))

(closer)

(define test-stream (maildir-parse (build-path here "example-maildir")))

(check-equal? (stream-length test-stream) 3)
(check-not-exn (λ () (validate-header (first (stream-first test-stream)))))
(check-equal? (extract-field #"Message-ID" (first (stream-ref test-stream 2)))
              #"<2005123351228.002YI0i8I9@mailer.johnkerry.com>")
(check-equal? (regexp-match #px"vehemently with"
                            ((second (stream-ref test-stream 1))))
              '(#"vehemently with"))

(check-equal? (stream-length (maildir-parse (build-path here
                                                        "example-maildir"
                                                        ".sub-maildir")))
              2)




