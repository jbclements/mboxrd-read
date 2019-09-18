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
         net/head
         (only-in racket/list count))

(provide (contract-out
          [maildir-parse
                   (->* (path-string?)
                        ()
                        (stream/c (list/c (or/c false? bytes?)
                                          (-> (or/c false? input-port?)))))]
          [maildir-num-unread
           (-> path-string? exact-nonnegative-integer?)]))

(define (false? v) (eq? v #f))

(define (maildir? path)
  (and (directory-exists? path)
       (directory-exists? (build-path path "new"))
       (directory-exists? (build-path path "cur"))))

;; given a path to a maildir, return a lazy list of the messages in the 
;; file.  Each file is represented as a list containing a byte-string
;; representing the header and the promise of a byte-string representing 
;; the body.  These byte-strings can be appended to obtain the original
;; message except that every \n in the original is replaced by \r\n to
;; match the RFC 2822 format.
(define (maildir-parse path)
  (unless (maildir? path)
    (raise-argument-error 'maildir-parse
                          "name of existing directory with new and cur subdirectories"
                          0 path))
  (define mail-file-list (maildir-files path))
  (let loop ([files-list mail-file-list])
    (cond [(null? files-list)
           empty-stream]
          [else (stream-cons
                 (list (message-headers (car files-list))
                       (λ () (message-body-port (car files-list))))
                 (loop (cdr files-list)))])))

;; return a list of the mail files in a maildir (anything in 'new' or
;; 'cur' that's a file)
(define (maildir-files path)
  (filter file-exists?
          (append
           (directory-list (build-path path "new") #:build? #t)
           (directory-list (build-path path "cur") #:build? #t))))

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

;; count the unread messages in a mailbox. Not worrying about locks,
;; in line with maildir philosophy
(define (maildir-num-unread path)
  (unless (maildir? path)
    (raise-argument-error 'maildir-num-unread
                          "name of existing directory with new and cur subdirectories"
                          0 path))
  (define files (maildir-files path))
  (count unread-message? files))

(define (unread-message? path)
  (define-values (base name must-be-dir?) (split-path path))
  (match (path->string name)
    ;; no colon, must be new:
    [(regexp #px"^[^:]+$") #t]
    ;; note that capital flags must come first, by the spec at
    ;; https://cr.yp.to/proto/maildir.html,
    [(regexp #px"^[^:]+:2,([A-Z]*)" (list _ capital-flags))
     (not (member #\S (string->list capital-flags)))]
    [(regexp #px"^[^:]+:1,")
     (raise-argument-error 'unread-message?
                           "message name not using experimental semantics"
                           0 path)]
    [other
     (raise-argument-error 'unread-message
                           "recognized message name format"
                           0 path)]))


;; TESTING

(module+ test
  (require rackunit
           racket/block)

  (check-equal? (unread-message? (build-path "/abc" "def" "ghi:2,abc"))
                #t)
  (check-equal? (unread-message? (build-path "/abc" "def" "ghi:2,Sabc"))
                #f)
  (check-exn #px"experimental semantics"
             (λ () (unread-message? (build-path "/abc" "def" "ghi:1,Sabc"))))
  (check-exn #px"recognized message name format"
             (λ () (unread-message? (build-path "/abc" "def" "ghi::1,Sabc"))))

  (check-exn #px"expected: name of existing directory" 
             (λ () (maildir-parse "/tmp/not-a-maildir-12374279834918/")))
  (check-equal? (message-headers "/tmp/bogus-file-!!@#$HT#@H")
                #f)

  (check-equal? (message-body-port "/tmp/bogus-file-!!@#$HT#@H")
                #f)

  (check-exn #px"expected: name of existing directory" 
             (λ () (maildir-num-unread "/tmp/not-a-maildir-12374279834918/")))
  

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

