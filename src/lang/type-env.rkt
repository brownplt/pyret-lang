#lang racket/base

(require "ast.rkt")
(provide
  (struct-out binding)
  bound?
  update
  lookup
  LIBRARY-ENV
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
     'torepr
     'Any
     'Bool
     'Function
     'Method
     'Number
     'Object
     'String
     'Nothing
     'Mutable
     'Placeholder
     'brander
     'check-brand
     'mk-mutable
     'mk-simple-mutable
     'mk-placeholder
     'is-nothing
     'nothing
     'gensym)))

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
      'range
      'repeat
      'filter
      'partition
      'any
      'find
      'map
      'map2
      'map3
      'map4
      'map_n
      'map2_n
      'map3_n
      'map4_n
      'each
      'each2
      'each3
      'each4
      'each_n
      'each2_n
      'each3_n
      'each4_n
      'fold
      'fold2
      'fold3
      'fold4
      'random
      'animate
      'circle
      'rectangle
      'text
      'place-image
      'read-sexpr
      
      'Option
      'some
      'is-some
      'none
      'is-none)))

(define LIBRARY-ENV
  (make-immutable-hash runtime-env-list))

(define DEFAULT-ENV
  (make-immutable-hash (append runtime-env-list builtins-env-list)))

(define WHALESONG-ENV
  (make-immutable-hash (append runtime-env-list builtins-env-list whalesong-env-list)))
