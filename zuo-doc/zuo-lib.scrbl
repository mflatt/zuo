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


@section[#:tag "zuo-thread"]{Cooperative Threads}

@defzuomodule[zuo/thread]


@section[#:tag "zuo-config"]{Configuration Parsing}

@defzuomodule[zuo/config]
