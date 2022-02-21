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


Building and Running Zuo
------------------------

Compile `zuo.c` with a C compiler. No additional are files needed,
other than system and C-library headers. No compiler flags should be
needed, although flags like `-o zuo` or `-O2` are a good idea.

The Zuo executable runs only modules. If you run Zuo with no
command-line arguments, then it loads `zuofile.zuo`. Otherwise, the
argument argument to Zuo is a file to run, and additional arguments
are delivered via the `command-line-arguments` procedure.


Library Modules and Startup Performance
---------------------------------------

Except for the built-in `zuo/kernel` language module, Zuo finds
languages and modules through a collection of libraries. By default,
Zuo looks for a directory `lib` relative to the executable as the root
of the library-collection tree. You can supply an alternate collection
path with the `-X` command-line flag.

You can also create an instance of Zuo with a set of libraries
embedded as a heap image. Embedding a heap image has two advantages:

 * No extra directory of library modules is necessary.

 * Zuo can start even more quickly.

The `embed-heap.zuo` script generates a `.c` file that is a copy of
`zuo.c` plus embedded modules. By default, the `zuo` module and its
dependencies are included, but you can specify others with `++lib`. In
addition, the default collection-root path is disabled in the
generated copy, unless you supply `--keep-collects` to
`embed-heap.zo`.

You can use heap images without embedding. The `dump-heap-and-exit`
Zuo kernel permitive creates a heap image, and a `-B` or `--boot`
command-line flag for Zuo uses the given boot image on startup.

A boot image is machine-independent, whether in a stand-alone file or
embedded in `.c` source.


Kernel Language
---------------

The `zuo/kernel` language implemented by `zuo.c` has these forms:

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

An error in Zuo always terminates the program. If you have a
subroutine that needs to be able to fail, you can run a separate Zuo
process. (The `find-exe` function reports the path to the current Zuo
executable, and you can use that with `process`.)


Macros
------

The Zuo library includes two variants of the macro expander:

 * The `#lang zuo` expander is non-hygienic by default. It offers only
   a limited form of composition by having `quote-syntax` close over
   the enclosing context. That doesn't support macro-generating macros
   well, but it's good enough to be useful, and it's fast.

 * The `#lang zuo/hygienic` language implements the set-of-scopes
   model (with no phase separation). It's more expressive, but also
   runs slowly on the Zuo interpreter.

The two expanders share an implementation that is parameterized over
the implementation of syntax objects. You can mix and match modules in
the two dialects, but you can't use macros from one language in the
other language.
