#lang scribble/manual
@(require (for-label zuo-doc/fake-kernel
                     (except-in zuo-doc/fake-zuo
                                lambda
                                let
                                quote
                                if
                                define
                                begin))
          "real-racket.rkt")

@title{Zuo Kernel Language}

@defmodulelang[zuo/kernel #:no-declare #:packages ()]
@declare-exporting[zuo-doc/fake-kernel #:packages ()]

The body of a @racketmodname[zuo/kernel] module is a single expression
using a set of core @seclink["kernel-syntax"]{syntactic forms}
and @seclink["kernel-primitives"]{primitives}. The expression
must produce a @tech{hash table} that serves as the module's
representation (see @secref["module-protocol"]).


@section[#:tag "kernel-syntax"]{Syntactic Forms}

@deftogether[(
@defform[#:link-target? #f #:id not-id id]
@defform[#:link-target? #f #:id not-literal literal]
@defform[#:link-target? #f #:id not-expr (expr expr ...)]
@defform[(lambda formals expr)
         #:grammar ([formals (id ...)
                             id
                             (id ... . id)])]
@defform[(quote datum)]
@defform[(if expr expr expr)]
@defform[(let ([id expr]) expr)]
@defform[(begin expr ...+)]
)]{

These forms are analogous to a variable reference, literal, procedure
application, @realracket*[lambda quote if let begin] in
@racketmodname[racket], but often restricted to a single expression or
binding clause. Unlike the corresponding @racketmodname[racket] or
@racketmodname[zuo] forms, the names of syntactic forms are not
shadowed by a @racket[lambda] or @racket[let] binding, and they refer
to syntactic forms only at the head of a term. A reference to an
unbound variable is a run-time error. If a @racket[id] appears
multiple times in @racket[formals], the last instance shadows the
others.

Although @racket[let] and @racket[begin] could be encoded with
@racket[lambda] easily enough, they're useful shortcuts to make
explicit internally.}


@section[#:tag "kernel-primitives"]{Primitives}

The following names provided by @racketmodname[zuo] are also available
in @racketmodname[zuo/kernel] (and the values originate there):

@racketblock[

  pair? null? list? cons car cdr list append reverse length

  integer? + - * quotient modulo < <= = >= >
  bitwise-and bitwise-ior bitwise-xor bitwise-not

  string? string-length string-ref string-u32-ref substring string=?

  symbol? symbol->string string->symbol string->uninterned-symbol
  
  hash? hash hash-ref hash-set hash-remove
  hash-keys hash-count hash-keys-subset?

  procedure? procedure-arity-mask apply

  eq? not void

  opaque opaque-ref

  path-string? build-path split-path relative-path?
  module-path? module-path-join

  variable variable-ref variable-set!

  fd-open-input fd-open-output fd-close fd-read fd-write eof

  stat runtime-env

  process process-status process-wait

  read-from-string-all ~v ~a ~s alert error 

  kernel-env kernel-eval module->hash dump-heap-and-exit

]
