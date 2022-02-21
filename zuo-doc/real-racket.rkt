#lang at-exp racket/base
(require scribble/manual
         (for-syntax racket/base)
         (for-label racket/base))

(provide realracket
         realracket*)

(define-syntax (realracket stx)
  (syntax-case stx ()
    [(_ id) @#`racket[#,(datum->syntax #'here (syntax-e #'id))]]))

(define-syntax (realracket* stx)
  (syntax-case stx ()
    [(_ id) @#'realracket[id]]
    [(_ id1 id2) @#'elem{@realracket[id] and @realracket[id2]}]
    [(_ id0 id ...) @#'elem{@realracket[id0], @realracket*[id ...]}]))


