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
    include
    require
    provide
    module+
    
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
    boolean?
    void

    apply
    call/cc
    context-consumer
    context-consumer?
    
    cons
    car
    cdr
    list
    append
    reverse
    length
    member
    assoc
    remove

    not
    eq?
    equal?
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
    string-ci=?
    string->symbol
    string->uninterned-symbol
    symbol->string
    string
    char
    string-split string-join string-trim

    hash
    hash-ref
    ref
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
    quote-path

    variable?
    variable
    variable-ref
    variable-set!

    identifier?
    syntax-e
    syntax->datum
    datum->syntax
    bound-identifier=?
    syntax-error
    bad-syntax
    misplaced-syntax
    duplicate-identifier

    fd-open-input
    fd-open-output
    fd-close
    fd-read
    fd-write
    eof
    fd-terminal?
    file->string
    display-to-file

    stat
    ls rm mv mkdir rmdir ln readlink cp
    current-time
    file-exists?
    directory-exists?
    link-exists?
    rm* cp* mkdir*
    :error :truncate :must-truncate :append :update :can-update
    cleanable-file
    cleanable-cancel

    process
    process-status
    process-wait
    find-executable-path
    shell->strings
    string->shell
    shell
    
    error
    alert
    ~v
    ~a
    ~s
    arity-error
    arg-error
    display displayln

    read-from-string-all
    module->hash
    module-path-join
    kernel-env
    kernel-eval

    runtime-env
    dump-image-and-exit
    exit
    suspend-signal resume-signal))

(intro-define-fake)

