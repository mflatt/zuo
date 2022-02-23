#lang racket/base

(define-syntax-rule (define-fake id ...)
  (begin
    (provide id ...)
    (define id 'id) ...))

(define-syntax-rule (intro-define-fake)
  (define-fake
    lambda
    let
    let*
    letrec
    if
    and
    or
    when
    unless
    begin
    cond
    quote
    quasiquote
    unquote
    unquote-splicing
    quote-syntax
    
    define
    define-syntax
    
    pair?
    null?
    integer?
    string?
    symbol?
    hash?
    list?
    procedure?
    path-string?
    module-path?
    relative-path?
    handle?
    void

    apply
    procedure-arity-mask

    cons
    car
    cdr
    list
    append
    reverse
    length

    not
    eq?
    any?
    void?

    +
    -
    *
    quotient
    modulo
    <
    <=
    =
    >=
    >
    bitwise-and
    bitwise-ior
    bitwise-xor
    bitwise-not

    string-length
    string-ref
    string-u32-ref
    substring
    string=?
    string->symbol
    string->uninterned-symbol
    symbol->string

    hash
    hash-ref
    hash-set
    hash-remove
    hash-keys
    hash-count
    hash-keys-subset?

    opaque
    opaque-ref

    build-path
    split-path
    path->complete-path

    variable
    variable-ref
    variable-set!

    fd-open-input
    fd-open-output
    fd-close
    fd-read
    fd-write
    eof

    stat

    process
    process-status
    process-wait

    error
    alert
    ~v
    ~a
    ~s

    read-from-string-all
    module->hash
    module-path-join
    kernel-env
    kernel-eval

    runtime-env
    dump-heap-and-exit))

(intro-define-fake)

