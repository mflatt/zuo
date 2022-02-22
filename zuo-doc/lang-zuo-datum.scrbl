#lang scribble/manual

@title{Zuo Data as Module}

@defmodulelang[zuo/datum]

A module in the @racketmodname[zuo/datum] language ``exports'' its
content as a list of S-expressions. The export is not a
@racket[provide] in the sense of the @racketmodname[zuo] language.
Instead, the module's implementation is just as hash table mapping
@racket['datums] to the list of S-expressions.
