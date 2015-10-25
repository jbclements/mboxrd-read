#lang racket

(require "main.rkt"
         "mboxcl2.rkt"
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
         (check-match
          first-mail
          (list (? bytes? b1) (? bytes? b2)))
         (loop (add1 n) (stream-rest stream))]
        [else
         (check-true (stream-empty? stream))]))

(define-values (closer mail-stream2)
  (mboxcl2-parse example-mbox))

(let loop ([n 0] [stream mail-stream2])
  (cond [(< n 3)
         (define first-mail (stream-first stream))
         (check-match
          first-mail
          (list (? bytes? b1) body-thunk))
         (check-true (bytes? ((cadr first-mail))))
         (loop (add1 n) (stream-rest stream))]
        [else
         (check-true (stream-empty? stream))]))

(closer)




