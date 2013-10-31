#lang racket

(require "mboxrd-read.ss"
         rackunit
         racket/runtime-path)

;; this really just tests that a simple mbox file comes out with the right
;; number of messages.  Further testing would be nice...

(define-runtime-path example-mbox "./test-mbox")

(define mail-stream
  (mboxrd-parse example-mbox))

(let loop ([n 0] [stream mail-stream])
  (check-true (promise? stream))
  (define first-mail (force stream))
  (cond [(< n 3)
         (check-true (cons? first-mail))
         (match (car first-mail)
           [(list headers body-promise)
            (check-true (bytes? headers))
            (check-true (promise? body-promise))
            (loop (add1 n) (cdr first-mail))]
           [other (check-true false)])]
        [else 
         (check-true (empty? first-mail))]))


