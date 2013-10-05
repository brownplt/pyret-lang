#lang racket/base

(require
  racket/runtime-path
  racket/match
  racket/list
  "../ffi-helpers.rkt"
  "../runtime.rkt"
  "../string-map.rkt"
  "../type-env.rkt")
(provide (rename-out [namespaces %PYRET-PROVIDE]))

(define (get-env-dict lang env)
  ;(printf "~a\n" (sort (namespace-mapped-symbols namespace) (lambda (s1 s2) (string>? (symbol->string s1) (symbol->string s2)))))
  (foldr
    (λ (key running-dict)
      (define val (dynamic-require lang key))
      (string-map-set running-dict (symbol->string key) val))
    (make-string-map empty)
    (hash-keys (env-to-hash env))))

(define whalesong-env
  (get-env-dict
    'pyret/lang/pyret-lang-whalesong
    WHALESONG-ENV))
(define pyret-env
  (get-env-dict
    'pyret/lang/pyret-lang-racket
    DEFAULT-ENV))
(define namespaces
  (p:mk-object
    (make-string-map
      (list
        (cons "pyret-env" (p:mk-object pyret-env))
        (cons "whalesong-env" (p:mk-object whalesong-env))))))

