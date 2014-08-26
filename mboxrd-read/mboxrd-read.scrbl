#lang scribble/manual

@title{mboxrd-read}

@(require (for-label "main.rkt"))

@defmodule[mboxrd-read]{This package parses @deftech{mboxrd} files,
 also known as "normal UNIX mbox files", into lazy lists of messages.}

@defproc[(mboxrd-parse [path path?]) 
         (lazy-listof (list/c bytes? bytes?))]{
  given a path to an mbox file, return a stream of the messages in the 
  file.  Each file is represented as a list containing a byte-string
  representing the header and the promise of a byte-string representing 
  the body.  These byte-strings can be appended to obtain the original
  message except that every \n in the original is replaced by \r\n to
  match the RFC 2822 format.
 }

@defproc[(mboxrd-parse/port [port input-port?])
         (lazy-listof (list/c bytes? bytes?))]{
  given an input port, return a lazy list of the messages in the port.

  NB: this procedure assumes that it's the only one reading the port. Bad 
  stuff will happen if its not; it doesn't leave the "From " of the next 
  message on the stream.
  
  EFFECT: reads from stream, closes it when peek-char returns #<eof>
}

Additionally, you can use the utilities (e.g. "extract-header") in
"net/head.ss" to process the header.

Let me know of any bugs.

John Clements
