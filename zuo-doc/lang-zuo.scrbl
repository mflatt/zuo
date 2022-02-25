#lang scribble/manual
@(require (for-label zuo-doc/fake-zuo)
          "real-racket.rkt")

@title{Zuo Language}

@defmodulelang[zuo #:no-declare #:packages ()]
@declare-exporting[zuo-doc/fake-zuo #:packages ()]

The @racketmodname[zuo] language is Zuo's default language. It's meant
to be familiar to Racket programmers, and the description here leans
heavily on comparisons and the Racket documentation, for now. Zuo
forms and functions tend use traditional Racket names, even when a
different choice might be made in a fresh design, and even when the
Zuo construct is not exactly the same. Filesystem operations, however,
tend to use the names of Unix programs, which are much shorter than
Racket's long names.

@section{Syntax and Evaluation Model}

A @racketmodname[zuo] module consists of a sequence of definitions
(e.g., @racket[define]), macro definitions (e.g.,
@racket[define-syntax]), imports (e.g., @racket[require]), exports
(e.g., @racket[provides]), and expressions (e.g., @racket[5]). Loading
the module first @deftech{expands} it, and then @deftech{evaluates}
it. A module is loaded only once, so if a module is demanded more than
once, the result of the first load is used.

The expansion process expands macro uses, loads imported modules, and
evaluates macro definitions as such forms are encountered for the
module body. Expansion creates a binding for each definition as
encountered, but does not expand or evaluate the definition, yet.
Expansion of definitions and expressions is deferred until all forms
in the module body have been processed. Some expression forms have
local definition contexts, which can include further imports and macro
definitions, so expansion at those points nests the same two-step
process as used for the module body.

Evaluation of a module evaluates its definitions and expressions (some
of which may have been introduced by macro expansion) in order.
Definitions bind mutually recursively within the enclosing module or
definition context, and referencing a defined variable before its
evaluation is an error. The value of each expression in a module body
is printed using @racket[alert] compiled with @racket[~v].

A module's provided variables and syntax are made available to other
modules that import it. Variables and macros that are not provided are
completely inaccessible outside of the module.

There are no @defterm{phases} in the sense of Racket. When
@racketmodname[zuo] macro expansion encountered an import, it makes
all of the imported module's exports immediately available for use in
macro implementations, both variables and macros. For example, an
imported macro might be used both to implement a macro body and in
nearby run-time code or even run-time code generated by the macro's
expansion. The absence of a phase separation is related to way that
each module is evaluated only once, and it's made workable in part by
the absence of mutable data structures in Zuo, and in part because
there is no support for compiling a @racketmodname[zuo] module and
saving it separate from it's instantiation in a Zuo process or saved
image.

Zuo macros consume a representation of syntax that uses plain pairs,
numbers, strings, etc., but with an identifier @tech{syntax object}
potentially in place of a symbol. Even for symbols, using a syntax
object is optional; by using @racket[quote-syntax] to create a syntax
object, a macro can generate a term with identifiers bound at the
macro's definition site, instead of a use site's, but the macro
expander does not impose or automate that binding. See
@racket[quote-syntax] for more information.

@; ----------------------------------------

@section{Binding and Control Forms}

A @racketmodname[zuo] syntactic form is either a @deftech{definition}
form or an @deftech{expression forms}. Expressions can appear in
definition contexts, but not vice versa. In descriptions of syntactic
forms @racket[_body ...+] refers to a context that allows definition
forms, but the last form in the expansion of the definition context
must be an expression form.

@subsection{Expression Forms}

@defform[(lambda formals body ...+)
         #:grammar ([formals (id ...)
                             id
                             (id ... . id)])]{

Analogous to @realracket[lambda] in @racketmodname[racket], but
without keyword or optional arguments.}


@defform[#:link-target? #f #:id not-expr (expr expr ...)]{

A function call, where the initial @racket[expr] is not an identifier
bound to a macro.}


@deftogether[(
@defform*[[(let ([id val-expr] ...) body ...+)
           (let proc-id ([id init-expr] ...) body ...+)]]
@defform[(let* ([id val-expr] ...) body ...+)]           
@defform[(letrec ([id val-expr] ...) body ...+)]
)]{

Just like @realracket*[let let* letrec] in @racketmodname[racket].}


@deftogether[(
@defform[(if test-expr then-expr else-expr)]
@defform[(and expr ...)]
@defform[(or expr ...)]
@defform[(when test-expr body ...+)]
@defform[(unless test-expr body ...+)]
@defform[#:literals (else)
         (cond cond-clause ...)
         #:grammar ([cond-clause [test-expr then-body ...+]
                                 [else then-body ...+]])]
@defform[#:id else else]
@defform[(begin expr ...+)]
)]{

Just like @realracket*[if and or when unless cond else begin] in
@racketmodname[racket], except that @racket[cond] is more limited.}


@deftogether[(
@defform[(quote datum)]
@defform[(quasiquote datum)]
@defform[#:id unquote unquote]
@defform[#:id unquote-splicing unquote-splicing]
)]{

Just like @realracket*[quote quasiquote unquote unquote-splicing] from
@racketmodname[racket].}


@defform[(quote-syntax datum)]{

Analogous to @realracket[quote-syntax] from @racketmodname[racket],
but only identifiers have a specialized syntax-object representation
in place of symbols. Tree structure in @racket[datum] represented
using plain pairs, and non-identifier elements of @racket[datums] are
represented with plain numbers, strings, etc.

A Zuo module's representation starts with plain pairs and symbols, a
macro procedure can receive terms containing plain symbols, and it can
return a term with plain symbols. A symbol non-hygienically acquires a
@tech{scope} at the point where its binding is resolved or where it
creates a binding.

A @deftech{scope} corresponds to a particular binding context. It can
be a module context, an internal definition context, or a binding site
for an expression form like the formals of a @racket[lambda] or the
right-hand side of a @racket[letrec].

An identifier @tech{syntax object} created by @racket[quote-syntax] closes
over a binding at the point where it is created, closing over the
enclosing module scope if the identifier is not (yet) bound. The
closure does not change if the identifier is nested in a later
@racket[quote-syntax] form. Identifiers that are introduced by macros
are not automatically given a scope or otherwise distinguished from
identifiers that appeared as input to a macro, and a plain symbol is
implicitly coerced to a syntax object only at the point where it binds
or where its binding is resolved as a reference.

There is no @realracket[quasisyntax], @realracket[unsyntax], or
@realracket[unsyntax-splicing] analog, since @racket[quasiquote],
@racket[unquote], and @racket[unquote-splicing] are already convenient
enough for most purposes. To generate a fresh symbol for the output of
a macro expansion, use @racket[string->uninterned-symbol].}

@defform[(quote-module-path)]{

Returns the module path of the enclosing module.}


@subsection{Definition Forms}

@defform*[[(define id expr)
           (define (id . formals) body ...++)]]{

Like @realracket*[define] from @racketmodname[racket], but without
keyword arguments, optional arguments, or header nesting for curried
functions.}

@defform*[[(define-syntax id expr)
           (define-syntax (id . formals) body ...++)]]{

Analogous to @realracket*[define-syntax] from @racketmodname[racket],
binds @racket[id] as a macro. Although @racket[define-syntax] is not
syntactically constrained to bind a function, a useful @racket[expr]
will produce a function of either one or two arguments.

If the function accepts two arguments, then when @racket[id] is used
for a macro invocation, the second argument to the procedure is a
function that acts like @realracket[free-identifier=?]. (In
@racketmodname[racket], @realracket[free-identifier=?] is implicitly
parameterized over the context of a macro invocation. Explicitly
providing a comparison function to a macro implementation, instead,
avoids the implicit parameterization.)

See @racket[quote-syntax] for more information about the
representation of syntax that a macro function consumes and produces.}


@defform[(include module-path)]{

Splices the content of the module identified by @racket[module-path],
assuming that @racket[module-path] is implemented in a language like
@racketmodname[zuo/datum].}



@; ----------------------------------------

@section{Booleans}

Zuo booleans are written @racket[#t] or @racket[#true] and @racket[#f]
or @racket[#false]. Any value other than @racket[#f] counts as true
for conditionals.

@deftogether[(
@defproc[(boolean? [v any?]) boolean?]
@defproc[(not [v any?]) boolean?]
)]{

Just like @realracket*[boolean? not] from @racket[racket].}

@defproc[(eq? [v1 any?] [v2 any?]) boolean?]{

Analogous to @realracket[eq?] from @racket[racket], but even small Zuo
numbers are not necessarily @racket[eq?] when they are @racket[=].}

@defproc[(equal? [v1 any?] [v2 any?]) boolean?]{

Analogous to @realracket[equal?] from @racket[racket].}


@section{Numbers}

A Zuo number corresponds to a 64-bit two's complement representation
with modular arithmetic (i.e., wraparound on overflow). It is always
written in decimal form with a leading @litchar{-} for negative
numbers.

@defproc[(integer? [v any?]) boolean?]{

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
@defproc[(bitwise-ior [n integer?] [m integer?]) integer?]
@defproc[(bitwise-and [n integer?] [m integer?]) integer?]
@defproc[(bitwise-xor [n integer?] [m integer?]) integer?]
@defproc[(bitwise-not [n integer?])  integer?]
)]{

Analogous to @realracket*[+ - * quotient modulo = < <= > >=
bitwise-ior bitwise-and bitwise-xor bitwise-not] from
@racketmodname[racket], but on Zuo integers and sometimes constrained
to two arguments.}


@section{Pairs and Lists}

Zuo pairs and lists work the same as in Racket with the same textual
representation.

@deftogether[(
@defproc[(pair? [v any?])
         boolean?]
@defproc[(null? [v any?])
         boolean?]
@defproc[(list? [v any?])
         boolean?]
@defproc[(cons [a any?] [d any?])
         pair?]
@defproc[(car [p pair?])
         any?]
@defproc[(cdr [p pair?])
         any?]
@defproc[(list [v any?] ...)
         list?]
@defproc[(list* [v any?] ... [tail any?])
         any?]
@defproc*[([(append [lst list?] ...) list?]
           [(append [lst list?] ... [v any?]) any?])]
@defproc[(reverse [lst list?]) list?]           
@defproc[(length [lst list?]) integer?]
@defproc[(list-ref [lst pair?] [pos integer?]) any?]
@defproc[(list-tail [lst any?] [pos integer?]) any?]
)]{

Just like @realracket*[pair? null? cons car cdr list? list* append
reverse list-ref list-tail] from @racketmodname[racket], except that
@racket[list?] takes time proportional to the length of the list.}

@deftogether[(
@defproc[(caar [p pair?]) any?]
@defproc[(cadr [p pair?]) any?]
@defproc[(cdar [p pair?]) any?]
@defproc[(cddr [p pair?]) any?]
)]{

Just like @realracket*[caar cadr cdar cddr] from @racketmodname[racket].}


@deftogether[(
@defproc[(map [proc procedure?] [lst list?] ...+)
         list?]
@defproc[(for-each [proc procedure?] [lst list?] ...+)
         void?]
@defproc[(foldl [proc procedure?] [init any?] [lst list?] ...+)
         any?]
@defproc[(andmap [proc procedure?] [lst list?])
          any?]
@defproc[(ormap [proc procedure?] [lst list?])
         any?]
)]{

Like @realracket*[map for-each foldl andmap ormap] from
@racketmodname[racket], except that @racket[andmap] and @racket[ormap]
are more restricted and do not apply @racket[proc] to the last
argument in tail position.}

@deftogether[(
@defproc[(member [v any?] [lst list?])
         (or/c pair? #f)]
@defproc[(assoc [v any?] [lst list?])
         (or/c pair? #f)]
)]{

Like @realracket*[member assoc] from @racketmodname[racket].}


@section{Strings}

Zuo @deftech{strings} are sequences of bytes.

@deftogether[(
@defproc[(string? [v any?]) boolean?]
@defproc[(string [char integer?] ...) string?]
@defproc[(string-length [str string?]) integer?]
@defproc[(string-ref [str string?] [k integer?]) integer?]
@defproc[(substring [str string?] 
                    [start integer?]
                    [end integer?]) string?]
@defproc[(string=? [str1 string?] [str2 string?]) boolean?]
)]{

Analogous to @realracket*[string? string string-length string-ref substring
string=?] from @racketmodname[racket], or more precisely analogous to
@realracket*[bytes? bytes-length bytes-ref subbytes bytes=?] frmo
@racketmodname[racket].}

@defproc[(string-u32-ref [str string?] [k integer?]) integer?]{

Returns the two's complement interpretation of four bytes in
@racket[str] starting at index @racket[k] using the host machine's
endianness.}

@defform[(char str)]{

Expands to @racket[(string-ref str 0)], where @racket[str] must be a
string of length 1.}


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
@defproc[(symbol? [v any?]) boolean?]
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
@defproc[(hash? [v any?]) boolean?]
@defproc[(hash [key symbol?] [val any?] ... ...) hash?]
@defproc[(hash-ref [hash hash?]
                   [key symbol?]
                   [failure-value any?])
         any?]
@defproc[(hash-set [hash (and/c hash? immutable?)]
                   [key symbol?]
                   [v any?])
         hash?]
@defproc[(hash-remove [hash (and/c hash? immutable?)]
                      [key symbol?])
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

@defproc[(ref [hash hash?] [key symbol?]) any?]{

Like @racket[hash-ref], but errors is @racket[key] is not mapped in
@racket[hash].}


@section{Procedures}

@deftogether[(
@defproc[(procedure? [v any?]) any?]
@defproc[(apply [proc procedure?] [lst list?]) any?]
@defproc[(procedure-arity-mask [proc procedure?]) integer?]
@defproc[(call/cc [proc procedure?]) any?]
)]{

Like @realracket*[procedure? apply procedure-arity-mask call/cc] from
@racketmodname[racket], but @racket[apply] accepts only two arguments.}


@section{Paths}

A @deftech{path string} is is a @tech{string} that is not non-empty
and to contains no nul bytes.

@defproc[(path-string? [v any?]) boolean?]{

Returns @racket[#t] if @racket[v] is a path string, @racket[#f] otherwise.}

@defproc[(build-path [base path-string?] [rel path-string?] ...) path-string?]{

Combines @racket[base] path (absolute or relative) with the relative
paths @racket[rel], adding path separators as needed.}

@defproc[(split-path [path path-string?]) pair?]{

Splits @racket[path] into its directory (if any) and a final element
components. If @racket[path] has only a single element, the
@racket[car] of the result is @racket[#f], and the @racket[cdr] is
@racket[path] unchanged; otherwise, the final element is returned
without trailing separators.}

@defproc[(relative-path? [path path-string?]) boolean?]{

Returns @racket[#t] if @racket[v] is a relative path, @racket[#f] otherwise.}


@section{Opaque Records}

@defproc[(opaque [key any?] [val any?]) any?]{

Returns an opaque record that encapsulates @racket[val] with access
allowed via @racket[key].}

@defproc[(opaque-ref [key any?] [v any?] [failure-val any?]) any?]{

Returns the value encapsulated in @racket[v] if its is an opaque
object with access allowed via @racket[key], @racket[failure-val] otherwise.}


@section{Variables}

A @tech{variable} is a value with a name that contains an another
value. The contained value is initially undefined, and attempting to
access the contained value before it's set results in an error where
the variable's name is used in the error message. A variable's
contained value can be set only once.

@defproc[(variable? [v any?]) boolean?]{

Returns @racket[#t] if @racket[v] is a variable, @racket[#f] otherwise.}


@defproc[(variable [name symbol?]) variable?]{

Creates a variable named by @racket[name] and without a value until
one is installed with @racket[variable-set!].}

@defproc[(variable-set! [var variable?] [val any?]) void?]{

Sets the value contained by @racket[var] to @racket[val] or errors if
@racket[var] already has a contained value.}

@defproc[(variable-ref [var variable?]) any?]{

Returns the value contained by @racket[var] or errors if @racket[var]
does not yet have a contained value.}


@section{Modules and Evaluation}

A @deftech{module path} is a path string or a symbol, where a symbol
must contain only the letters @litchar{A}-@litchar{Z},
@litchar{a}-@litchar{z}, @litchar{A}-@litchar{Z},
@litchar{0}-@litchar{9}, @litchar{-}, @litchar{+}, @litchar{+}, or
@litchar{/}. Furthermore, @litchar{/} in a symbol module path cannot
be at the start, end, or adjacent to another @litchar{/}.

@defproc[(module-path? [v any?]) boolean?]{

Returns @racket[#t] if @racket[v] is a @tech{module path}, @racket[#f]
otherwise.}

@defproc[(module-path-join [base module-path?] [rel-path path-string?]) module-path?]{

Analogous to @racket[build-path], but for @tech{module paths}. Initial
@filepath{.} and @filepath{..} elements are handles specially to avoid
creating a module that that has @filepath{.} and @filepath{..}
elements. When @racket[base] is a symbol, then the characters of
@racket[rel-path] (after initial @filepath{.} and @filepath{..}
elements) must be allowable in a symbol module path.}

@defproc[(module->hash [mod-path module-path?]) hash?]{

Loads @racket[mod-path] if it has not been loaded already, and returns
the @tech{hash table} representation of the loaded module. See also
Secref["module-protocol"]}

@defproc[(kernel-eval [s-exp any?]) any?]{

Evaluates a term as if it appeared in a @racketmodname[zuo/kernel] module
(but the result does not have to be a @tech{hash table}).}

@defproc[(kernel-env) hash?]{

Returns a @tech{hash table} that maps each primitive and constant name
available in the body of a @racketmodname[zuo/kernel] module to its value.}


@section{Miscellaneous}

@defproc[(void? [v any?]) boolean?]{

Returns @racket[#t] if @racket[v] is the unique @deftech{void} value,
@racket[#f] otherwise.}

@defproc[(void [v any?] ...) void?]{

Accepts any number of arguments and ignored them, returning the void
value.}

@defproc[(any? [v any?]) boolean?]{

Always returns @racket[#t]. This procedure is mostly used just as a
contract in documentation.}


@section{Reading and Writing Objects}

@defproc[(read-from-string-all [str string?]) list?]{

Reads all S-expressions in @racket[str], returning a list of the
S-expressions (in order as they appeared in the string).}


@deftogether[(
@defproc[(~v [v any?] ...) string?]
@defproc[(~a [v any?] ...) string?]
@defproc[(~s [v any?] ...) string?]
)]{

Like @realracket*[~v ~a ~s], but with no formatting options. These
three format options corresponds to @realracket[print] style,
@realracket[display] style, and @realracket[write] style,
respectively.

Unlike uninterned symbols in @racketmodname[racket], Zuo uninterned
symbols format in @realracket[print] and @realracket[write] styles with
@litchar{#<symbol:}...@litchar{>}. @tech{Opaque objects},
@tech{handles}, and @tech{variables} print with
@litchar{#<:}...@litchar{>} notation in all styles.}


@defproc[(alert [v any?] ...) void?]{

Prints to standard output using the same formatting rules as
@racket[error]. This function is useful for simple logging and
debugging tasks.}


@defproc[(error [v any?] ...) void?]{

Exits as an error after printing the @racket[v]s to standard error.

If the first @racket[v] is a string, its character are printed output
@realracket[display]-style, and then @litchar{: } is printed. All
other @racket[v]s (including the first one if it's not a string) are
combined using @racket[~v], and that resulting string is written
@realracket[display]-style.}


@section{Syntax Objects}

A @deftech{syntax object} combines a symbolic with a binding scope,
where the two are used to determine a binding when the identifier is
used in a macro expansion.

@deftogether[(
@defproc[(identifier? [v any?]) boolean?]
@defproc[(syntax-e [v identifier?]) symbol?]
@defproc[(syntax->datum [v any?]) any?]
@defproc[(datum->syntax [ctx identifier?] [v any?]) any?]
@defproc[(bound-identifier=? [id1 identifier?]
                             [id2 identifier?]) boolean?]
)]{

Analogous to @realracket*[identifier? syntax-e syntax->datum
datum->syntax bound-identifier=?] from @racketmodname[racket]. Plain
symbols count as an identifier, however, and for
@racket[bound-identifier=?], a symbol is equal only to itself. The
@racket[datum->syntax] function always just returns its second
argument.}

@defproc[(syntax-error [message string?] [stx any?]) void?]{

Exits as an error after printing @racket[message], @litchar{: }, and
@racket[(~s (syntax->datum stx))].}

@deftogether[(
@defproc[(bad-syntax [stx any?]) void?]
@defproc[(misplaced-syntax [stx any?]) void?]
@defproc[(duplicate-identifier [stx any?]) void?]
)]{

Calls @racket[syntax-error] with a suitable error message and @racket[stx].}


@section{Files, Streams, and Processes}

Files, input and out streams more generally, and processes are all
represented as @tech{handles}.

@defproc[(handle? [v any?]) boolean?]{

Returns @racket[#t] if @racket[v] is a @tech{handle}, @racket[#f]
otherwise.}

@defproc[(fd-open-input [filename (or/c path-string? 'stdin)]) handle?]{

Opens a file for reading or obtains a reference to standard input when
@racket[filename] is @racket['stdin]. The result handle can be used
with @racket[fd-read] and closed with @racket[fd-close].}

@deftogether[(
@defproc[(fd-open-output [filename path-string?]
                         [options hash?]) handle?]
@defthing[:error hash?]
@defthing[:truncate hash?]
@defthing[:must-truncate hash?]
@defthing[:append hash?]
@defthing[:update hash?]
@defthing[:can-update hash?]
)]{

The @racket[fd-open-output] procedure opens a file for writing,
obtains a reference to standard output when @racket[filename] is
@racket['stdout], or obtains a reference to standard error when
@racket[filename] is @racket['stderr]. When opening a file,
@racket[options] specifies options as described below, but
@racket[options] must be empty for @racket['stdout] or
@racket['stderr]. The result handle can be used with @racket[fd-write]
and closed with @racket[fd-close].

In @racket[options], a single key is currently recognized:
@racket['exists]. The mapping for @racket['exists] must be one of the
symbols accepted for @racket[#:exists] by
@realracket[open-output-file] from @racketmodname[racket], but not
@racket['replace] or @racket['truncate/replace], and the default
mapping is @racket['error]. Any other key in @racket[options] is an
error.

The @racket[:error], @racket[:truncate], @racket[:must-truncate],
@racket[:append], @racket[:update], and @racket[:can-update] hash
tables each map @racket['exists] to the corresponding mode.}

@defproc[(fd-close [handle handle?]) void?]{

Closes the file or stream associated with @racket[handle], if it
refers to an open input or output stream. Any other kind of
@racket[handle] triggers an error.}

@defproc[(fd-read [handle handle?] [amount (or/c integer? eof)]) (or/c string? eof)]{

Reads from the input file or input stream associated with
@racket[handle], erroring for any other kind of @racket[handle]. The
@racket[amount] argument can be @racket[eof] to read all content up to
an end-of-file, or a non-negative integer to read up to that many
bytes. The result is @racket[eof] if @racket[amount] is not @racket[0]
and no bytes are available before an end-of-file; otherwise, it is a
string containing the read bytes.

The number of bytes in the returned string can be less than
@racket[amount] if the number of currently available bytes is less
than @racket[amount] but at least one byte.}

@defproc[(fd-write [handle handle?] [str string?]) void?]{

Writes the bytes of @racket[str] to the output file or output stream
associated with @racket[handle], erroring for any other kind of
@racket[handle].}

@defthing[eof any?]{

A constant representing an end-of-file.}


@defproc*[([(process [executable path-string?] [arg string?] ...) hash?]
           [(process [executable path-string?] [arg string?] ... [options hash?]) hash?])]{

Creates a new process to run @racket[executable] with the arguments
@racket[args]. The result is a @tech{hash table} that at least
contains the key @racket['process] mapped to a handle representing the
new process. The process handle can be used with @racket[process-wait]
and @racket[process-status].

If @racket[options] is supplied, it controls the process creation and
may cause additional keys to be mapped in the result. The recognized
keys are as follows, and supplying an unrecognized key in
@racket[options] is an error:

@itemlist[

@item{@racket['dir] mapped to a path string: the working directory of
      the new porcess; if @racket[executable] is a relative path, it
      is relative to this directory}

@item{@racket['env] mapped to a list of pairs of strings: environment
      variables for the new process, where the @racket[car] or each
      pair is an environment variable name and the @racket[cdr] is its
      value}

@item{@racket['stdin] mapped to @racket['pipe]: creates a new output
      stream connected to the new process's standard input; the result
      hash table contains @racket['stdin] mapped to the new stream handle}

@item{@racket['stdin] mapped to an input stream: supplies (a copy of)
      the input stream as the new process's standard input}

@item{@racket['stdout] mapped to @racket['pipe]: creates a new input
      stream connected to the new process's standard output; the
      result hash table contains @racket['stdout] mapped to the new
      stream handle}

@item{@racket['stdout] mapped to an output stream: supplies (a copy
      of) the output stream as the new process's standard input}

@item{@racket['stderr] mapped to @racket['pipe]: creates a new input
      stream connected to the new process's standard error; the result
      hash table contains @racket['stderr] mapped to the new stream
      handle}

@item{@racket['stderr] mapped to an output stream: supplies (a copy
      of) the output stream as the new process's standard error}

]}

@defproc[(process-wait [process handle?]) void?]{

Waits until the process represented by @racket[process] has
terminated, erroring if @racket[process] is any other kind of handle.
Waiting again on the same handle will return immediately.}

@defproc[(process-status [process handle?]) (or/c 'running integer?)]{

Returns @racket['running] if the process represented by
@racket[process] is still running, the exit value if the process has
exited (@racket[0] normally means succes), erroring for any other kind
of handle.}


@section{Filesystem}

@defproc[(stat [name path-string?] [follow-links? any?]) (or/c hash? #f)]{

Returns information about the file, directory, or link referenced by
@racket[name]. If @racket[follow-links?] is @racket[#f], then when
@racket[name] refers to a link, information is reported about the
link; otherwise, information is reported about the target of a link.

If no such file, directory, or link exists, the result is @racket[#f].
Otherwise, the hash table has similar keys and values as
@realracket[file-or-directory-stat] from @racketmodname[racket], but
with only certain keys per platform:

@itemlist[

 @item{Unix: @racket['device-id], @indexed-racket['inode],
       @racket['mode], @racket['type] (abbreviated),
       @racket['hardlink-count], @racket['user-id],
       @racket['group-id], @racket['device-id-for-special-file],
       @racket['size], @racket['block-size], @racket['block-count],
       @racket['access-time-seconds], @racket['modify-time-seconds],
       @racket['change-time-seconds],
       @racket['access-time-nanoseconds],
       @racket['modify-time-nanoseconds], and
       @racket['change-time-nanoseconds]}
 
 @item{Windows: @racket['device-id], @indexed-racket['inode],
       @racket['mode] (read and write bits only), @racket['type]
       (abbreviated), @racket['hardlink-count], @racket['size],
       @racket['access-time-seconds], @racket['modify-time-seconds],
       @racket['creation-time-seconds],
       @racket['access-time-nanoseconds],
       @racket['modify-time-nanoseconds], and
       @racket['creation-time-nanoseconds]}
 
]

The abbreviated @racket['type] field contains @racket['file],
@racket['dir], or @racket['link], with @racket['link] only on Unix and
only when @racket[follow-links?] is @racket[#f].}

@defproc[(ls [dir path-string?]) list?]{

Returns a list of path strings for files in @racket[dir].}

@defproc[(rm [name path-string?]) void?]{

Deletes the file or link @racket[name].}

@defproc[(rm* [name path-string?]) void?]{

Deletes the file, directory, or link @racket[name], including the
directory content if @racket[name] refers to a directory (and not to a
link to a directory). Unlike @racket[rm], it's not an error if
@racket[name] does not refer to an existing file, directory, or link.}

@defproc[(mv [name path-string?] [new-name path-string?]) void?]{

Renames the file, directory, or link @racket[name] to @racket[new-name].}

@defproc[(mkdir [dir path-string?]) void?]{

Creates a directory @racket[dir].}

@defproc[(mkdir* [dir path-string?]) void?]{

Creates a directory @racket[dir] if it does not already exist, along
with its ancector directories.}

@defproc[(rmdir [dir path-string?]) void?]{

Deletes a directory @racket[dir].}

@defproc[(ln [target path-string?] [name path-string?]) void?]{

Creates a link @racket[name] with the content @racket[target]. This
function is not supported on Windows.}

@defproc[(readlink [name path-string?]) void?]{

Gets the content of a link @racket[name]. This function is not
supported on Windows.}

@deftogether[(
@defproc[(file-exists? [name path-string?]) booelan?]
@defproc[(directory-exists? [name path-string?]) booelan?]
@defproc[(link-exists? [name path-string?]) booelan?]
)]{

Uses @racket[stat] to check for a file, directory, or link,
respectively.}


@section{Run Time Configuration}

@defproc[(runtime-env) hash?]{

Returns a @tech{hash table} containing information about the current
Zuo process. The hash table includes the following keys:

@itemlist[

@item{@racket['args]: comment-line arguments provided when the
      process was started, not counting Zuo configuration arguments or
      the name of a script to run}

@item{@racket[dir]: the current directory}

@item{@racket[env]: a list of pairs of strings for environment variables}

@item{@racket['script]: the script provided to Zuo to run, which might
      be @racket[""] to indicate a script read from standard input}

@item{@racket['exe]: an absolute path for the running Zuo executable}

@item{@racket['system-type]: @racket['unix] or @racket['windows]}

]}

@defproc[(current-time) pair?]{

Reports the current wall-clock time as a pair: seconds since January
1, 1970 and additional nanoseconds.}

@defproc[(dump-image-and-exit [output handle?]) void?]{

Writes an image of the current Zuo process to @racket[output], which
must be an open output file or stream, and then exits.

This function is intended to be used after some set of modules has
been loaded, so that the loaded modules are included in the image. The
dump fails if if any @tech{handle} is encountered as reachable from
loaded modules, however.}
