#lang racket/base

(define-syntax-rule (define-fake id ...)
  (begin
    (provide id ...)
    (define id 'id) ...))

(define-fake
  pair?
  null?
  integer?
  string?
  symbol?
  hash?
  list?
  procedure?
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
  eval
  dynamic-require
  module-path-join
  kernel-env

  find-exe
  command-line-arguments

  dump-heap-and-exit)
