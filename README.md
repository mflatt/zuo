Zuo: a Tiny Racket for Scripting
--------------------------------

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
`#lang`s. But the macro system of `#lang zuo` is non-hygienic and has
no phase separation. (But you could build a hygienic layer on top. But
maybe you should just build and use Racket at that point.)

The `zuo/kernel` language implemented by "zuo.c" has these forms:

```
 <expr> ::= <variable>
         |  <literal>                  ; number, string, etc.
         |  (<expr> <expr> ...)        ; function call
         |  (lambda <formals> <expr>)  ; name allowed before <expr>
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
