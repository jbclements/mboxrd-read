#lang racket

(require "main.rkt"
         rackunit
         racket/runtime-path)

;; this really just tests that a simple mbox file comes out with the right
;; number of messages.  Further testing would be nice...

(define-runtime-path example-mbox "./test-mbox")

(define mail-stream
  (mboxrd-parse example-mbox))

(let loop ([n 0] [stream mail-stream])
  (cond [(< n 3)
         (define first-mail (stream-first stream))
         (check-true (list? first-mail))
         (check-equal? (length first-mail) 2)
         (check-true (bytes? (first first-mail)))
         (check-true (bytes? (second first-mail)))
         (loop (add1 n) (stream-rest stream))]
        [else
         (check-true (stream-empty? stream))]))


