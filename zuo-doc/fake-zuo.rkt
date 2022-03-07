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
    quote-module-path
    
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
    list-ref
    list-set

    not
    eq?
    equal?
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
    string-sha1
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
    at-source

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
    system-type
    file-exists?
    directory-exists?
    link-exists?
    explode-path
    simple-form-path
    find-relative-path
    build-raw-path
    path-replace-suffix
    path-only
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
    
    error
    alert
    ~v
    ~a
    ~s
    arity-error
    arg-error
    display displayln

    string-read
    module->hash
    build-module-path
    kernel-env
    kernel-eval

    runtime-env
    dump-image-and-exit
    exit
    suspend-signal resume-signal

    command-line

    target
    rule
    phony-rule
    input-file-target
    input-data-target
    target-path
    target-name
    target?
    token?
    rule?
    phony-rule?
    sha1?
    file-sha1
    no-sha1
    build
    build/command-line
    build/recur
    make-at-dir
    provide-targets
    find-target

    shell
    shell/wait
    build-shell

    call-in-main-thread
    thread? thread channel? channel channel-put channel-get
    thread-process-wait
    config-file->hash))

(intro-define-fake)
