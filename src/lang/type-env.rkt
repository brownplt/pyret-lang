#lang racket/base

(require "ast.rkt")
(provide
  (struct-out binding)
  bound?
  update
  lookup
  DEFAULT-ENV
  WHALESONG-ENV
  EMPTY-ENV)

(struct binding (loc ann mutable?))

(define (bound? env id)
  (hash-has-key? env id))
(define (lookup env id)
  (define r (hash-ref env id #f))
  (when (not r) (error (format "Unbound id: ~a" id)))
  r)
(define (update id b env)
  (hash-set env id b))

(define type-check-loc
  (srcloc "builtin-value" #f #f #f #f))

(define (env-binding ann)
  (binding type-check-loc ann #f))

(define EMPTY-ENV (make-immutable-hash))

(define (blankify names)
 (map
  (lambda (s)
    (cons s (env-binding (a-blank))))
  names))

(define runtime-env-list
  (blankify
    (list
     'pi
     'prim-has-field
     'prim-keys
     'prim-num-keys
     'print
     'raise
     'tostring
     'Any
     'Bool
     'Function
     'Method
     'Number
     'Object
     'String
     'brander
     'is-nothing
     'nothing)))

(define builtins-env-list
  (blankify
    (list
      'list
      'error
      'builtins
      'checkers
      'option)))

(define whalesong-env-list
  (blankify
    (list
      'List
      'link
      'empty
      'filter
      'fold
      'map
      'random
      'animate
      'circle
      'rectangle
      'text
      'place-image)))

(define DEFAULT-ENV
  (make-immutable-hash (append runtime-env-list builtins-env-list)))

(define WHALESONG-ENV
  (make-immutable-hash (append runtime-env-list builtins-env-list whalesong-env-list)))
