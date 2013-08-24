#lang racket/base

(require
  racket/runtime-path
  "../eval.rkt"
  "../ffi-helpers.rkt"
  "../string-map.rkt"
  (except-in "../runtime.rkt" raise)
  "../type-env.rkt")
(provide (rename-out [eval-lib %PYRET-PROVIDE]))

(define (make-fresh-namespace)
  (define ns (make-empty-namespace))
  (namespace-attach-module (current-namespace) 'pyret/lang/pyret-lang-library ns)
  (parameterize ([current-namespace ns])
    (namespace-require 'pyret/lang/pyret-lang-library)
    (void))
  ns)

(define (extend-namespace-with-dict ns dict)
  (string-map-map dict
    (lambda (key value)
      (define name-of-identifier (string->symbol key))
      (namespace-set-variable-value!
        name-of-identifier
        (ffi-unwrap value)
        #f
        ns)))
  ns)

(define (pyret-eval ast env settings)
  (define (get-or-false name)
    (cond
      [(p:has-field? settings name)
       (ffi-unwrap (p:get-field p:dummy-loc settings name))]
      [else #f]))
  (define check-mode (get-or-false "check-mode")) 
  (define indentation (get-or-false "indentation")) 
  (define pyret-stx (ffi-unwrap ast))
  (define env-dict (p:get-dict env))
  (define racket-stx
    (with-handlers
        ([exn:fail?
          (lambda (e) (raise (p:pyret-error p:dummy-loc "eval-error" "An error occurred during parsing or typechecking in eval")))])
      (stx->racket
        pyret-stx
        #:toplevel #f
        #:check check-mode
        #:indentation indentation
        #:type-env LIBRARY-ENV)))
  (define environment (extend-namespace-with-dict (make-fresh-namespace) env-dict))
  (eval racket-stx environment))

(define eval-lib
  (p:mk-object (make-string-map
    (list
      (cons "eval" (ffi-wrap pyret-eval))))))

