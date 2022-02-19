Zuo: a Tiny Racket for Scripting
================================

[Work in progress]

You should use Racket to write scripts. But what if you need something
much smaller than Racket for some reason — or what if you're trying
to script a build of Racket itself? Zuo is a tiny Racket with
primitives for dealing with files and running processes, and it comes
with a `make`-like embedded DSL.

Zuo is a Racket variant in the sense that program files start with
`#lang`, and the module path after `#lang` determines the parsing and
expansion of the file content. That's how the `make`-like DSL is
defined, and even the base Zuo language is defined by layers of
`#lang`s. One of the early layers implements macros.

Kernel Language
---------------

The `zuo/kernel` language implemented by "zuo.c" has these forms:

```
 <expr> ::= <variable>
         |  <literal>                  ; number, string, etc.
         |  (<expr> <expr> ...)        ; function call
         |  (lambda <formals> <string>? <expr>) ; optional name
         |  (quote <expr>)
         |  (if <expr> <expr> <expr>)
         |  (let/cc <variable> <expr>)
         |  (let ([<variable> <expr>]) <expr>)
         |  (begin <expr> <expr> ...)
```

Of course, those last two could be encoded with `lambda` easily
enough, but they're useful shortcuts to make explicit internally.

Zuo data structures are immutable except for "variable" values. A
variable is like a box, but it's set-once, and a variable has a name
that is used to report an error when attempting to get a value of the
variable before it has been set. Variables are used to implement
`letrec`, for example.

An error in Zuo always terminates the program.

Macros
------

The Zuo library includes two variants of the macro expander:

 * The `#lang zuo` expander is non-hygienic by default and offers only
   a limited form of composition by having `quote-syntax` close over
   the enclosing module context. That doesn't support macro-generating
   macros wells, but it's good enough to be useful, and it's fast.

 * The `#lang zuo/hygienic` language implements the set-of-scopes
   model (with no phase separation). It's more expressive, but also
   computationally intensive and runs slowly on the Zuo interpreter.

The two expanders share an implementation that is parameterized over
the implementation of syntax objects. You can mix and match modules in
the two dialects, but you can't use macros from one language from a
context in the other language.
