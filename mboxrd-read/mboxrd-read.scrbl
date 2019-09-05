#lang scribble/manual

@title{mboxrd-read}

@(require (for-label "main.rkt"))
@(require (for-label net/head))

@defmodule[mboxrd-read]{This package parses @deftech{mboxrd} and @deftech{mboxcl2} files,
 also known as "normal UNIX mbox files", into lazy lists of messages. It now also handles
 maildir directories.}

@defproc[(mboxrd-parse [path path?]) 
         (stream/c (list/c bytes? bytes?))]{
  given a path to an mbox file, return a stream of the messages in the 
  file.  Each file is represented as a list containing a byte-string
  representing the header and a byte-string representing 
  the body.  These byte-strings can be appended to obtain the original
  message except that every \n in the original is replaced by \r\n to
  match the RFC 2822 format.

  You are responsible for any locking needed to protect the file from modification
  while it's being read. Different MUAs and MDAs have different locking
  protocols. Good luck.
 }

@defproc[(mboxrd-parse/port [port input-port?])
         (stream/c (list/c bytes? bytes?))]{
  given an input port, return a lazy list of the messages in the port.

  NB: this procedure assumes that it's the only one reading the port. Bad 
  stuff will happen if its not; it doesn't leave the @racket["From "] of the next 
  message on the stream.
  
  EFFECT: reads from stream, closes it when peek-char returns @racket[eof].
}

@section{mboxcl2}

Well, it turns out that dovecot actually uses mboxcl2. Ah well. In fact,
mboxcl2 looks like a bit of a win; since it uses Content-Length to locate the
next header, it should be possible to parse faster, since you can set the file
position rather than scanning those hideously long base64 body strings looking
for the next line starting with @racket["From "]. The down side is that since
the body strings aren't read eagerly, closing the file port is a separate operation
that you're responsible for.

@defproc[(mboxcl2-parse [path path-string?]
                        [#:fallback fallback? boolean? #f])
         (values (-> void?) (stream/c (list/c bytes? (-> bytes?))))]{
  given an input port, returns a @racket[closer] function that closes the input port
  associated with the file, and a list of lists containing a header byte-string and a
  thunk that returns the body bytes.

  When @racket[fallback?] is true, a message whose header lacks a Content-Length
  field will instead be processed by searching forward for a line beginning with
  @racket["From "].

  Please note that the header gets rfc822-style newlines, but the body does not.

  Note that after the @racket[closer] function is called, it's not possible to
  extend the lazy list or to extract bodies.

  Again, you are responsible for any locking required to protect this file from
  modification while it's being read.
}

@section{maildir}

Despite its increasingly narrow name, this package now also parses maildir directories,
with a similar interface.

@defproc[(maildir-parse [path path-string?])
         (stream/c (list/c (or/c bytes? false?) (-> (or/c input-port? false?))))]{
 Given a path that refers to a directory containing both @filepath{new} and @filepath{cur}
 subdirectories, return a stream of file results, where each result contains a byte string
 as before and a thunk that returns an input port pointing to the beginning of the body
 text of the given message.

 Maildir was designed to avoid the need for locking, and indeed, it's less problematic in
 this regard than other formats. The most likely problem is that the list of files will change
 while you're looking at the directory. This may mean that the lazily constructed stream will
 contain references to files that aren't there any more by the time you ask for their headers
 or their bodies, which means that both headers and body-ports can wind up being false. Beyond
 this, though, you don't need to worry about locking.
}

@section{leftovers}

Additionally, you can use the utilities (e.g. @racket[extract-field] and @racket[validate-headers])
in @filepath{net/head} to process the header.

Let me know of any bugs.

John Clements
