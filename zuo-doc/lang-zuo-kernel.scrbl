#lang scribble/manual

@title{Zuo Kernel Language}

@defmodulelang[zuo/kernel]

The @racketmodname[zuo/kernel] language implemented by
@filepath{zuo.c} has these forms:

@verbatim[#:indent 2]{
 <expr> ::= <variable>
         |  <literal>                  ; number, string, etc.
         |  (<expr> <expr> ...)        ; function call
         |  (lambda <formals> <string>? <expr>) ; optional name
         |  (quote <expr>)
         |  (if <expr> <expr> <expr>)
         |  (let/cc <variable> <expr>)
         |  (let ([<variable> <expr>]) <expr>)
         |  (begin <expr> <expr> ...)
}

Of course, those last two could be encoded with @racket[lambda] easily
enough, but they're useful shortcuts to make explicit internally.
