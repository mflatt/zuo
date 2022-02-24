#lang scribble/manual
@(require (for-label zuo-doc/fake-zuo))

@title{Zuo Overview}

Zuo is a Racket variant in the sense that program files start with
@hash-lang[], and the module path after @hash-lang[] determines the
parsing and expansion of the file content. Zuo, however, has a
completely separate implementation. So, even though its programs start
with @hash-lang[], Zuo programs are not meant to be run via Racket.

The name ``Zuo'' is derived from the Chinese word for ``make.''


@section{Building and Running Zuo}

Compile @filepath{zuo.c} from the Zuo sources with a C compiler. No
additional are files needed for compilation, other than system and
C-library headers. No compiler flags should be needed, although flags
like @exec{-o zuo} or @exec{-O2} are a good idea.

The Zuo executable runs only modules. If you run Zuo with no
command-line arguments, then it loads @filepath{main.zuo} in the
current directory. Otherwise, the first argument to Zuo is a file to
run or a directory containing a @filepath{main.zuo} to run, and
additional arguments are delivered to that program via the
@racket[runtime-env] procedure.

Note that starting Zuo with the argument @filepath{.} equivalent to
the argument @filepath{./main.zuo}, which is a convenient shorthand
for using @exec{zuo} as a replacement for @exec{make} while still
passing arguments. When Zuo receives the empty string (which would be
invalid as a file path) as a first argument, it reads a module from
standard input.


@section{Library Modules and Startup Performance}

Except for the built-in @racketmodname[zuo/kernel] language module,
Zuo finds languages and modules through a collection of libraries. By
default, Zuo looks for a directory @filepath{lib} relative to the
executable as the root of the library-collection tree. You can supply
an alternate collection path with the @Flag{X} command-line flag.

You can also create an instance of Zuo with a set of libraries
embedded as an image. Embedding an image has two advantages:

@itemlist[

 @item{No extra directory of library modules is necessary, as long as
       all relevant libraries are embedded.}

 @item{Zuo can start especially quickly, competitive with the fastest
       command-line programs.}

]

The @filepath{image.zuo} script included with the Zuo sources
generates a @filepath{.c} file that is a copy of @filepath{zuo.c} plus
embedded modules. By default, the @racketmodname[zuo] module and its
dependencies are included, but you can specify others with
@DPFlag{lib}. In addition, the default collection-root path is
disabled in the generated copy, unless you supply
@DFlag{keep-collects} when running @filepath{image.zo}.

You can use images without embedding. The @racket[dump-image-and-exit]
Zuo kernel permitive creates an image containing all loaded modules,
and a @Flag{B} or @DFlag{boot} command-line flag for Zuo uses the
given boot image on startup.

A boot image is machine-independent, whether in a stand-alone file or
embedded in @filepath{.c} source.


@section{Zuo Datatypes}

Zuo's kernel supports the kind kinds of data:

@itemlist[

 @item{booleans;}

 @item{integers as 64-bit two's complement with modular arithmetic}

 @item{string as byte strings;}

 @item{symbols, both interned (never garbage collected) and
       uninterned;}

 @item{lists;}

 @item{@deftech{hash tables}, which are symbol-keyed persistent maps
       (and don't actually employ hashing internally);}

 @item{procedures, including first-class continuations reified as
        procedures;}

 @item{@deftech{variables}, which are named, set-once, single-valued
       containers;}

 @item{opaque objects that pair a key and a value, where the value can
       be accessed only by supplying the key (which is typically kept
       private using lexical scope); and}

 @item{@deftech{handles}, which represent system resources like files
       or processes.}

]

Notable omissions include floating-point numbers, characters, Unicode
strings, and vectors. Paths are represented using byte strings (with
an implied UTF-8 encoding for Windows wide-character paths).


@section{Zuo Implementation and Macros}

The @filepath{zuo.c} source implements @racketmodname[zuo/kernel],
which is a syntactically tiny language plus fewer than 100 primitive
procedures. Since Zuo is intended for scripting, it's heavy on
filesystem, I/O, and process primitives: about 1/4 are for those
tasks, about 1/4 are for numbers, and the rest cover things like
strings, symbols, lists, and hash tables.

Zuo data structures are immutable except for @tech{variable} values,
and even a variable is set-once, and attempting to get a value of the
variable before it has been set is an error. (Variables are used to
implement @racket[letrec], for example.) Zuo is not purely functional,
because it includes imperative I/O and errors, but it actively
discourages gratuitous state by confining imperative actions to
external interactions. Along those lines, an error in Zuo always
terminates the program; there is no exception system, and along those
lines, there's no way within Zuo to detect early use of an unset
variable.

The @racketmodname[zuo] language is built on top of
@racketmodname[zuo/kernel], but not directly. There's an internal
``looper'' language that just adds simple variants of @racket[letrec],
@racket[cond], and @racket[let*], because working without those is
especially tedious. Then there's an internal ``stitcher'' language
that is the only use of the ``looper'' language; it adds its own
@racket[lambda] (which implicit @racket[begin]) @racket[let] (with
multiple clauses), @racket[let*], @racket[letrec] (with multiple
binding clauses), @racket[and], @racket[or], @racket[when],
@racket[unless], and a kind of @racket[define] and @racket[include].

Two macro implementations are built with the ``stitcher'' layer. One
is based on the same set-of-scopes model as Racket, and that macro
system is used for and provided by @racketmodname[zuo/hygienic]. The
other is non-hygienic and uses a less expressive model of scope, which
a programmer might notice if, say, writing macro-generating macros;
that macro system is used for and provided by @racketmodname[zuo],
because it's a lot faster and adequate for most scripting purposes.
The two macro system implementations are mostly the same source, which
is parameterized over the representation of scope and binding, and
implemented through a combination of @racketmodname[zuo/datum] and the
``stitcher'' layer's @racket[include].

Naturally, you can mix and match @racketmodname[zuo] and
@racketmodname[zuo/hygienic] modules in a program, you can't use
macros from one language within the other language. More generally,
Zuo defines a @hash-lang[] protocol that lets you build arbitrary new
languages (from the character/byte level), as long as they ultimately
can be expressed in @racketmodname[zuo/kernel].


@section[#:tag "module-protocol"]{Zuo Module Protocol}

At Zuo's core, a module is represented as a @tech{hash table}. There
are no constraints on the keys of the hash table, and different layers
on top of the core module protocol can assign meanings to keys. For
example, the @racketmodname[zuo] and @racketmodname[zuo/hygienic]
layers use a common key for accessing @racket[provide]d bindings, but
different keys for propagating binding information for macro
expansions.

The core module system assigns a meaning to one key,
@racket['read-and-eval], which is supplied by a module that implements
a @hash-lang[] language. The value of @racket['read-and-eval] is a
procedure of three arguments:

@itemlist[

 @item{a string for the text of a module using the language,}

 @item{a position within the string that starts a module body after
       @hash-lang[] and the language name, and}

 @item{a module path that will be mapped to the result of evaluating
       the module (i.e., the path to the text's source).}

]

The procedure must return a hash table representing the evaluated
module. A @racket['read-and-eval] procedure might use
@racket[read-from-string-all] to read input, it might use
@racket[kernel-eval] to evaluate read or generated terms, and it might
use @racket[module->has] to access other modules in the process of
parsing a new module---but a @racket['read-and-eval] procedure is
under no obligation to use any of those.

A call @racket[(module->hash _M)] primitive checks whether the module
@racket[_M] is already loaded and returns its hash if so. The
@racket[zuo/kernel] module is always preloaded, but other modules may
be preloaded in an image that was created by
@racket[dump-image-and-exit]. If a module @racket[_M] is not already
loaded, @racket[module->hash] reads the beginning of @racket[_M]'s
source to parse the @hash-lang[] specification and get the path of the
language module @racket[_L]; a recursive call @racket[(module->hash
_L)] gets @racket[_L], and @racket[_L]'s @racket['read-and-eval]
procedure is applied to the source of @racket[_M] to get @racket[_M]'s
representation as a hash. That representation is both recorded for
future use and returned from the original @racket[(module->hash _M)]
call.
