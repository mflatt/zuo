#lang scribble/manual
@(require (for-label zuo-doc/fake-zuo)
          "real-racket.rkt")

@title{Zuo Language}

@defmodulelang[zuo #:no-declare #:packages ()]
@declare-exporting[zuo-doc/fake-zuo #:packages ()]

@section{Booleans}

Zuo booleans are written @racket[#t] or @racket[#true] and @racket[#f]
or @racket[#false]. Any value other than @racket[#f] counts as true
for conditionals.

@deftogether[(
@defproc[(boolean? [v any/c]) boolean?]
@defproc[(not [v any/c]) boolean?]
)]{

Just like @realracket*[boolean? not] from @racket[racket].}

@defproc[(eq? [v1 any/c] [v2 any/c]) boolean?]{

Analogous to @realracket[eq?] from @racket[racket], but even small Zao
numbers are not necessarily @racket[eq?] when they are @racket[=].}


@section{Numbers}

A Zuo number corresponds to a 64-bit two's complement representation
with modular arithmetic (i.e., wraparound on overflow). It is always
written in decimal form with a leading @litchar{-} for negative
numbers.

@defproc[(integer? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is an integer, @racket[#f] otherwise.}

@deftogether[(
@defproc[(+ [z integer?] ...) integer?]
@defproc*[([(- [z integer?]) integer?]
           [(- [z integer?] [w integer?] ...+) integer?])]
@defproc[(* [z integer?] ...) integer?]
@defproc[(quotient [n integer?] [m integer?]) integer?]
@defproc[(modulo [n integer?] [m integer?]) integer?]
@defproc[(= [z integer?] [w integer?]) boolean?]
@defproc[(< [x integer?] [y integer?]) boolean?]
@defproc[(<= [x integer?] [y integer?]) boolean?]
@defproc[(> [x integer?] [y integer?]) boolean?]
@defproc[(>= [x integer?] [y integer?] ...) boolean?]
@defproc[(bitwise-ior [n exact-integer?] [m exact-integer?]) exact-integer?]
@defproc[(bitwise-and [n exact-integer?] [m exact-integer?]) exact-integer?]
@defproc[(bitwise-xor [n exact-integer?] [m exact-integer?]) exact-integer?]
@defproc[(bitwise-not [n exact-integer?])  exact-integer?]
)]{

Analogous to @realracket*[+ - * quotient modulo = < <= > >=
bitwise-ior bitwise-and bitwise-xor bitwise-not] from
@racketmodname[racket], but on Zuo integers and sometimes constrained
to two arguments.}


@section{Pairs and Lists}

Zuo pairs and lists work the same as in Racket with the same textual
representation.

@deftogether[(
@defproc[(pair? [v any/c])
         boolean?]
@defproc[(null? [v any/c])
         boolean?]
@defproc[(cons [a any/c] [d any/c])
         pair?]
@defproc[(car [p pair?])
         any/c]
@defproc[(cdr [p pair?])
         any/c]
@defproc[(list? [v any/c])
         boolean?]
@defproc[(list [v any/c] ...)
         list?]
@defproc[(list* [v any/c] ... [tail any/c])
         any/c]
@defproc*[([(append [lst list?] ...) list?]
           [(append [lst list?] ... [v any/c]) any/c])]
@defproc[(length [lst list?]) integer?]
@defproc[(reverse [lst list?]) list?]           
)]{

Just like @realracket*[pair? null? cons car cdr list? list*] from
@racketmodname[racket], except that @racket[list?] takes time
proportional to the length of the list.}


@section{Strings}

Zuo strings are sequences of bytes.

@deftogether[(
@defproc[(string? [v any/c]) boolean?]
@defproc[(string-length [str string?]) integer?]
@defproc[(string-ref [str string?] [k integer?]) integer?]
@defproc[(substring [str string?] 
                    [start integer?]
                    [end integer?]) string?]
@defproc[(string=? [str1 string?] [str2 string?]) boolean?]
)]{

Analogous to @realracket*[string? string-length string-ref substring
string=?] from @racketmodname[racket], or more precisely analogous to
@realracket*[bytes? bytes-length bytes-ref subbytes bytes=?] frmo
@racketmodname[racket].}

@defproc[(string-u32-ref [str string?] [k integer?]) integer?]{

Returns the two's complement interpretation of four bytes in
@racket[str] starting at index @racket[k] using the host machine's
endianness.}


@section{Symbols}

Zuo symbols are @deftech{interned} by the reader, where two interned
symbols are @racket[eq?] when they have the same string content. An
@deftech{uninterned} symbol is @racket[eq?] only to itself. Zuo
symbols are the only kind of value that can be used as a key for a Zuo
@tech{hash table}.

The textual representation of symbols does not include escapes for
special character, analogous to the way @litchar{|} works in Racket.
Symbols with those characters will print in a way that cannot be read
back into Zuo.

@deftogether[(
@defproc[(symbol? [v any/c]) boolean?]
@defproc[(symbol->string [sym symbol?]) string?]
@defproc[(string->symbol [str string?]) symbol?]
@defproc[(string->uninterned-symbol [str string?]) symbol?]
)]{

Analogous to @realracket*[symbol? symbol->string string->symbol
string->uninterned-symbol] from @racketmodname[racket].}


@section{Hash Tables (Persistent Maps)}

Zuo @tech{hash tables} do not actually have anything to do with
hashing, but they're called that for similarly to Racket. A hash table
maps symbols to other values, and updating a hash table produces a new
hash table (which, internally, may share with the original).

Hash table print in a way analogous to Racket, but there is no reader
support to convert the textual form back into a hash table value.

@deftogether[(
@defproc[(hash? [v any/c]) boolean?]
@defproc[(hash [key symbol?] [val any/c] ... ...) hash?]
@defproc[(hash-ref [hash hash?]
                   [key any/c]
                   [failure-value any/c])
         any/c]
@defproc[(hash-set [hash (and/c hash? immutable?)]
                   [key any/c]
                   [v any/c])
         hash?]
@defproc[(hash-remove [hash (and/c hash? immutable?)]
                      [key any/c])
         hash?]
@defproc[(hash-keys [hash hash?])
         @elem{list of @racket[symbol?]}]
@defproc[(hash-count [hash hash?]) integer?]
@defproc[(hash-keys-subset? [hash1 hash?] [hash2 hash?])
         boolean?]
)]{

Analogous to @realracket*[hash? hash hash-ref hash-set hash-remove
hash-keys hash-count hash-keys-subset?] from @racketmodname[racket].
Besides being constrained to symbol keys, there is one additional
difference: the third argument to @racket[hash-ref] must be supplied,
and it is always used as a value to return if a key is missing, as
opposed to a failure thunk.}



@;{

  ZUO_TOP_ENV_SET_PRIMITIVE1("procedure?", zuo_procedure_p);
  ZUO_TOP_ENV_SET_PRIMITIVEN("void", zuo_make_void, 0);

  ZUO_TOP_ENV_SET_VALUE("apply", z.o_apply);
  ZUO_TOP_ENV_SET_PRIMITIVE1("procedure-arity-mask", zuo_procedure_arity_mask);

ZUO_TOP_ENV_SET_PRIMITIVE2("opaque", zuo_opaque);
  ZUO_TOP_ENV_SET_PRIMITIVE3("opaque-ref", zuo_opaque_ref);

  ZUO_TOP_ENV_SET_PRIMITIVE2("build-path", zuo_build_path);
  ZUO_TOP_ENV_SET_PRIMITIVE1("split-path", zuo_split_path);
  ZUO_TOP_ENV_SET_PRIMITIVE2("path->complete-path", zuo_path_to_complete_path);

  ZUO_TOP_ENV_SET_PRIMITIVE1("variable", zuo_variable);
  ZUO_TOP_ENV_SET_PRIMITIVE1("variable-ref", zuo_variable_ref);
  ZUO_TOP_ENV_SET_PRIMITIVE2("variable-set!", zuo_variable_set);

  ZUO_TOP_ENV_SET_PRIMITIVE1("fd-open-input", zuo_fd_open_input);
  ZUO_TOP_ENV_SET_PRIMITIVE1("fd-open-output", zuo_fd_open_output);
  ZUO_TOP_ENV_SET_PRIMITIVE1("fd-close", zuo_fd_close);
  ZUO_TOP_ENV_SET_PRIMITIVE2("fd-read", zuo_fd_read);
  ZUO_TOP_ENV_SET_PRIMITIVE2("fd-write", zuo_fd_write);
  ZUO_TOP_ENV_SET_VALUE("eof", z.o_eof);

  ZUO_TOP_ENV_SET_PRIMITIVE2("stat", zuo_stat);

  ZUO_TOP_ENV_SET_PRIMITIVEN("process", zuo_process, 1);
  ZUO_TOP_ENV_SET_PRIMITIVE1("process-status", zuo_process_status);
  ZUO_TOP_ENV_SET_PRIMITIVE1("process-wait", zuo_process_wait);

  ZUO_TOP_ENV_SET_PRIMITIVEN("error", zuo_error, 0);
  ZUO_TOP_ENV_SET_PRIMITIVEN("alert", zuo_alert, 0);
  ZUO_TOP_ENV_SET_PRIMITIVEN("~v", zuo_tilde_v, 0);
  ZUO_TOP_ENV_SET_PRIMITIVEN("~a", zuo_tilde_a, 0);
  ZUO_TOP_ENV_SET_PRIMITIVEN("~s", zuo_tilde_s, 0);

  ZUO_TOP_ENV_SET_PRIMITIVE2("read-from-string-all", zuo_read_all);
  ZUO_TOP_ENV_SET_PRIMITIVE1("eval", zuo_eval);
  ZUO_TOP_ENV_SET_PRIMITIVE1("dynamic-require", zuo_dynamic_require);
  ZUO_TOP_ENV_SET_PRIMITIVE2("module-path-join", zuo_module_path_join);
  ZUO_TOP_ENV_SET_PRIMITIVE0("kernel-env", zuo_kernel_env);

  ZUO_TOP_ENV_SET_PRIMITIVE0("find-exe", zuo_find_exe);
  ZUO_TOP_ENV_SET_PRIMITIVE0("command-line-arguments", zuo_command_line_arguments);

  ZUO_TOP_ENV_SET_PRIMITIVE1("dump-heap-and-exit", zuo_dump_heap_and_exit);

}
