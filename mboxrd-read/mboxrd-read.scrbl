#lang scribble/manual

@title{mboxrd-read}

@(require (for-label "main.rkt"))
@(require (for-label net/head))

@defmodule[mboxrd-read]{This package parses @deftech{mboxrd} files,
 also known as "normal UNIX mbox files", into lazy lists of messages.

Oh. Um, actually, it now parses mboxcl2 files, as well. Scope creep, I know.}

@defproc[(mboxrd-parse [path path?]) 
         (stream/c (list/c bytes? bytes?))]{
  given a path to an mbox file, return a stream of the messages in the 
  file.  Each file is represented as a list containing a byte-string
  representing the header and a byte-string representing 
  the body.  These byte-strings can be appended to obtain the original
  message except that every \n in the original is replaced by \r\n to
  match the RFC 2822 format.
 }

@defproc[(mboxrd-parse/port [port input-port?])
         (stream/c (list/c bytes? bytes?))]{
  given an input port, return a lazy list of the messages in the port.

  NB: this procedure assumes that it's the only one reading the port. Bad 
  stuff will happen if its not; it doesn't leave the "From " of the next 
  message on the stream.
  
  EFFECT: reads from stream, closes it when peek-char returns #<eof>
}

@section{mboxcl2}

Well, it turns out that dovecot actually uses mboxcl2. Ah well. In fact,
mboxcl2 looks like a bit of a win; since it uses Content-Length to locate the
next header, it should be possible to parse faster, since you can set the file
position rather than scanning those hideously long base64 body strings looking
for the next line starting with @racket["From "]. The down side is that since
the body strings aren't read eagerly, closing the file port is a separate operation
that you're responsible for.

@defproc[(mboxcl2-parse [path path-string?])
         (values (-> void?) (stream/c (list/c bytes? (-> bytes?))))]{
  given an input port, returns a @racket[closer] function that closes the input port
  associated with the file, and a list of lists containing a header string and a
  thunk that returns the body bytes.

  Note that after the @racket[closer] function is called, it's not possible to
  extend the lazy list *or* to extract bodies.
}

Additionally, you can use the utilities (e.g. @racket[extract-field]) in
"net/head.ss" to process the header.

Let me know of any bugs.

John Clements
