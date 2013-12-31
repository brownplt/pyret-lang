#lang racket/base

(require "ast.rkt")
(provide
  (struct-out binding)
  bound?
  update
  update-list-any
  lookup
  env-to-hash
  runtime-env-list
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
(define (env-to-hash env) env)

(define type-check-loc
  (srcloc "builtin-value" #f #f #f #f))

(define (update-list-any ids env)
  (foldr (lambda (x env) (hash-set env x (binding type-check-loc (a-blank) #f)))
         env
         ids))

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
     'Boolean
     'Function
     'Method
     'Number
     'Object
     'String
     'Nothing
     'Mutable
     'Placeholder
     'Array
     'Opaque
     'is-bool
     'is-boolean
     'is-function
     'is-method
     'is-number
     'is-object
     'is-string
     'is-nothing
     'is-mutable
     'is-placeholder
     'is-array
     'is-nothing
     'brander
     'check-brand
     'mk-mutable
     'mk-simple-mutable
     'mk-placeholder
     'array-of
     'build-array
     'nothing
     'gensym)))

(define builtins-env-list
  (blankify
    (list
      'list
      'error
      'builtins
      'checkers
      'option
      'cs173)))

(define whalesong-env-list
  (blankify
    (list
      'List
      'link
      'empty
      'is-link
      'is-empty
      'filter
      'fold
      'map
      'range
      'range-by
      'repeat
      'filter
      'partition
      'split-at
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
      'index
      
      'array
      'array-set
      'array-get
      'array-length
      'array-to-list

      'random
      'animate
      'circle
      'rectangle
      'text
      'place-image
      'read-sexpr
      'read-sexpr-list

      'Option
      'some
      'is-some
      'none
      'is-none

      'Set
      'sets
      'set
      'list-set
      'tree-set

      'identical
      'string-to-list)))

(define LIBRARY-ENV
  (make-immutable-hash (append runtime-env-list (blankify '(___set-link ___set-empty)))))

(define DEFAULT-ENV
  (make-immutable-hash (append runtime-env-list builtins-env-list)))

(define WHALESONG-ENV
  (make-immutable-hash (append runtime-env-list builtins-env-list whalesong-env-list)))

