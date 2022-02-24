Zuo: A Tiny Racket for Scripting
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
command-line arguments, then it loads `main.zuo`. Otherwise, the first
argument to Zuo is a file to run or a directory containing a
`main.zuo` to run, and additional arguments are delivered to the Zuo
program via the `runtime-env` procedure.


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

 * Zuo can start especially quickly, competitive with the fastest
   command-line programs.

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


More Information
----------------

Install the `zuo-doc` directory as a package in Racket to render the
documentation there.
