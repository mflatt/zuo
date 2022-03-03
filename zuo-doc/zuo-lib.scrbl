#lang scribble/manual
@(require (for-label zuo-doc/fake-zuo)
          "real-racket.rkt")

@(define-syntax-rule (defzuomodule zuo/x)
   (begin
     @defmodule[zuo/x #:no-declare #:packages ()]
     @declare-exporting[zuo zuo/x #:packages () #:use-sources (zuo-doc/fake-zuo)]
     @para{The @racketmodname[zuo/x] module is reprovided by @racketmodname[zuo].}))

@title[#:tag "zuo-lib"]{Zuo Libraries}

The @racketmodname[zuo] language includes libraries added to
@racketmodname[zuo/base] to support scripting and build tasks.


@section[#:tag "zuo-cmdline"]{Command-Line Parsing}

@defzuomodule[zuo/cmdline]


@section[#:tag "zuo-build"]{Building with Dependencies}

@defzuomodule[zuo/build]



@section[#:tag "zuo-glob"]{Glob Matching}

@defzuomodule[zuo/glob]

@defproc[(glob->matcher [glob string?]) procedure?]{

Creates a procedure that takes a string and reports @racket[#t] if the
string matches the pattern @racket[glob], @racket[#f] otherwise.

The pattern language of @racket[glob] is based on shell globbing:

@itemlist[

 @item{@litchar{?} matches any character;}

 @item{@litchar{*} matches any squence of characters; and}

 @item{@litchar{[}@italic{range}@litchar{]} matches any character in
       @italic{range}, and @litchar{[^}@italic{range}@litchar{]}
       matches any character not in @italic{range}.}

]

In a @italic{range}, most characters stand for themselves as elements
of the range, including @litchar{*} and @litchar{?}, including
@litchar{]} when it appears first, and including @litchar{-} when it
appears first or last. When @litchar{-} appears between two characters
in @italic{range}, then the second character's value must be at least
as large as the first, and all character in from the first (inclusive)
to the second (inclusive) are included in the range.

A leading @litchar{.} or a @litchar{/} in an input string are not
treated specially. That is, @racket["*"] matches @racket[".apple"] and
@racket["a/pple"] as well as @racket["apple"]. Use @racket[split-path]
and a secondary check for a leading @litchar{.} to imitate shell-like
path-sensitive globbing.}


@defproc[(glob-match? [glob string?] [str string?]) boolean?]{

Equivalent to @racket[((glob->matcher glob) str)].}


@section[#:tag "zuo-thread"]{Cooperative Threads}

@defzuomodule[zuo/thread]


@section[#:tag "zuo-config"]{Configuration Parsing}

@defzuomodule[zuo/config]
