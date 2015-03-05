#lang racket/base

;; read from mboxrd format.  Messages are divided by lines beginning with F R O M <space>. 
;; one open question is the handling of newlines; rfc822 wants crlf, but as I understand it
;; the typical encoding for mboxrd uses simple newlines.  I will translate for the header, 
;; and for the body.

;; I do not expect this code to work under Windows because of the CRLF issue.  However, 
;; I also think that mboxrd format is rarely used under Windows, so this probably won't be
;; a problem.

;; in mboxrd format, lines beginning >+From_ must have one '>' stripped from the beginning 
;; of the line.

(require racket/contract
         racket/stream)

;; can't use stream/c until next release...
(provide/contract [mboxrd-parse (path? . -> . stream? #;(stream/c (list/c bytes? bytes?)))]
                  [mboxrd-parse/port (input-port? . -> . stream? #;(stream/c (list/c bytes? bytes?)))])
  
;; mboxrd-parse : 
;; given a path to an mbox file, return a lazy list of the messages in the 
;; file.  Each file is represented as a list containing a byte-string
;; representing the header and the promise of a byte-string representing 
;; the body.  These byte-strings can be appended to obtain the original
;; message except that every \n in the original is replaced by \r\n to
;; match the RFC 2822 format.
(define (mboxrd-parse path)
  (mboxrd-parse/port (open-input-file path)))

;; mboxrd-parse/port :
;; NB: this procedure assumes that it's the only one reading the port. Bad 
;; stuff will happen if its not; it doesn't leave the "From " of the next 
;; message on the stream.

;; EFFECT: reads from stream, closes it when peek-char returns #<eof>

(define (mboxrd-parse/port ip)
  (if (eof-object? (peek-char ip))
      (begin (close-input-port ip)
             empty-stream)
      (begin
        (unless (regexp-match #px"From " ip)
          (error "nonempty mbox file did not begin with \"From \""))
        (let loop ()
          (let*-values ([(msg-in-pipe msg-out-pipe) (make-pipe)]
                        [(match-result) (regexp-match #px#"\nFrom " ip 0 #f msg-out-pipe)]
                        [(dc) (close-output-port msg-out-pipe)]
                        [(hdr-port) (open-output-bytes)]
                        [(match-result2) (regexp-match #px#"\n\n|\n$" msg-in-pipe 0 #f hdr-port)])
            (unless match-result2 
              (error "couldn't find blank line separating header from body:\n ~a"
                     (get-output-bytes hdr-port)))

            (let* ([empty-body (equal? match-result2 `(#"\n"))]
                   [header (bytes-append #"From " (regexp-replace* #px#"\n>(>*From )"
                                                                   (regexp-replace* #px#"\n"
                                                                                    (get-output-bytes hdr-port)
                                                                                    #"\r\n")
                                                                   #"\n\\1")
                                         #"\r\n\r\n")]
                   [body (let* ([body-port (open-output-bytes)]
                                ;; a quick(?) way to drain the characters into the bytes
                                [dc (regexp-match #px"a^" msg-in-pipe 0 #f body-port)])
                           (bytes-append (regexp-replace* #px#"\n>(>*From )"
                                                          (regexp-replace* #px#"\n"
                                                                           (get-output-bytes body-port)
                                                                           #"\r\n")
                                                          #"\n\\1")
                                         (if empty-body
                                             #""
                                             #"\r\n")))])
              (stream-cons (list header body)
                           (if match-result
                               (loop)
                               empty-stream))))))))






;; TESTING

(module+ test
  (require rackunit)
(define tstr "From oohc
lala

tropo
From 13
>From obetor
>>From oherot

From 15

From 14

a

b")


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
               #"a\r\n\r\nb\r\n"))))

