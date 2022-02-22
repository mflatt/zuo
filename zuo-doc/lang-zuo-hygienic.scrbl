#lang scribble/manual
@(require (for-label zuo-doc/fake-zuo)
          "real-racket.rkt")

@title{Zuo with Hygienic Macros}

@defmodulelang[zuo/hygienic]

The @racketmodname[zuo/hygienic] language provides the same set of
bindings as @racketmodname[zuo], but with hygienic macros. Its
macro-expansion protocol uses a different representation of
identifiers and binding scope, and different rules for
@racket[quote-syntax] and macros:

@itemlist[

 @item{A @racketmodname[zuo/hygienic] term's representation always
       uses identifier syntax objects in place of symbols. A macro
       will never receive a plain symbol in its input, and if the
       macro produces a term with plain symbol, it is automatically
       coerced to a syntax object using the scope of the module that
       defines the macro.}

 @item{A syntax object's context includes a @defterm{set of scopes},
       instead of just one @tech{scope}. Before expanding forms in a
       new context, a fresh scope representation is added to every
       identifier appearing within the context. An reference is
       resolved by finding the binding identifier with the most
       specific set of scopes that is a subset of the referencing
       identifier's scopes.}

 @item{In addition to binding contexts, a specific macro invocation is
       also represented by a scope: a fresh scope is added to every
       syntax object introduced by a macro expansion. This fresh scope
       means that an identifier introduced by the expansion can only
       bind identifiers that were introduced by the same expansion.
       Meanwhile, a @racket[quote-syntax]-imposed scope on an
       introduced identifier prevents it from being bound by an
       identifier that's at the macro-use site and not visible at the
       macro-definition site.}

 @item{The @racket[quote-syntax] form produces an identifier syntax
       object with all of its scope intact. That syntax object
       acquires additional scope if it is returned from a macro
       expander into a new context.}

]
