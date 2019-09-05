#lang racket/base

;; okay, now I moved everything to maildir, and I want the same interface. I'm still
;; using dovecot. Hopefully, this should make this version of the code much shorter.

;; a note about maildir and race conditions; this code ignores the race condition issue,
;; which is mostly okay for maildir. The bad things that can happen are that a file may
;; be deleted or renamed after the file list (that is, the message list) is obtained and
;; before the mail file is processed. In the worst case, a rename can mean that a file appears
;; to be gone, but has just been renamed. We'll see if this is an issue.
(require racket/contract
         racket/stream
         racket/match
         racket/string
         net/head)

(provide (contract-out
          [maildir-parse
                   (->* (path-string?)
                        ()
                        (stream/c (list/c (or/c false? bytes?)
                                          (-> (or/c false? input-port?)))))]))

(define (false? v) (eq? v #f))

;; given a path to a maildir, return a lazy list of the messages in the 
;; file.  Each file is represented as a list containing a byte-string
;; representing the header and the promise of a byte-string representing 
;; the body.  These byte-strings can be appended to obtain the original
;; message except that every \n in the original is replaced by \r\n to
;; match the RFC 2822 format.
(define (maildir-parse path)
  (unless (and (directory-exists? path)
               (directory-exists? (build-path path "new"))
               (directory-exists? (build-path path "cur")))
    (raise-argument-error 'maildir-parse
                          "name of existing directory with new and cur subdirectories"
                          0 path))
  (define mail-file-list
    (filter file-exists?
            (append
             (directory-list (build-path path "new") #:build? #t)
             (directory-list (build-path path "cur") #:build? #t))))
  (let loop ([files-list mail-file-list])
    (cond [(null? files-list)
           empty-stream]
          [else (stream-cons
                 (list (message-headers (car files-list))
                       (λ () (message-body-port (car files-list))))
                 (loop (cdr files-list)))])))


;; given a path, return the message's headers. If the file is missing,
;; return #f instead.
(define (message-headers path)
  (with-handlers ([exn:fail:filesystem? (λ (exn) #f)])
    (call-with-input-file path port->headers)))

;; given a path, return a port open and set to read the body. If the file
;; is missing, just return #f
(define (message-body-port path)
  (with-handlers ([exn:fail:filesystem? (λ (exn) #f)])
  (define ip (open-input-file path))
  ;; for effect only:
  (port->headers ip)
  ip))

;; given a port, read the headers and return them. EFFECT: advances the
;; port to the beginning of the body
(define (port->headers ip)
  (define headers-port (open-output-bytes))
  ;; search for the end of the headers
  (define match-result (regexp-match #px#"\n\n|\n$" ip 0 #f headers-port))
  (unless match-result
    (error 'process-message-file/port
           "couldn't find blank line separating header from body:\n ~a"
           (get-output-bytes headers-port)))
  (bytes-append
   (regexp-replace* #px#"\n"
                    (get-output-bytes headers-port)
                    #"\r\n")
   #"\r\n\r\n"))



;; TESTING

(module+ test
  (require rackunit
           racket/block)


  (check-exn #px"expected: name of existing directory" 
             (λ () (maildir-parse "/tmp/not-a-maildir-12374279834918/")))
  (check-equal? (message-headers "/tmp/bogus-file-!!@#$HT#@H")
                #f)

  (check-equal? (message-body-port "/tmp/bogus-file-!!@#$HT#@H")
                #f)
  

  (check-exn
   #px"couldn't find blank line separating header from body"
   (lambda ()
     (port->headers (open-input-string "abcdefFrom "))))

  (check-exn
   #px"couldn't find blank line"
   (lambda ()
     (port->headers
      (open-input-string "From my dad
To: Your Mom
Subject: get to work!
From a big brown cow
To: Betsy")))))

