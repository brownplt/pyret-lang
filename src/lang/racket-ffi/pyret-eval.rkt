#lang racket/base

(require
  racket/runtime-path
  racket/match
  racket/list
  "../ast.rkt"
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
        value
        #f
        ns)))
  ns)

(define (run-for-n-seconds thnk n (default #f))
  (define current (current-thread))
  (define (wrapped-thunk)
    (define result (thnk))
    (thread-send current result))
  (define handle (thread wrapped-thunk))
  (define receive (thread-receive-evt))
  (define got-result (sync/timeout n receive))
  (define result (if got-result (thread-receive) default))
  (when (not got-result) (break-thread handle))
  result)

(define (pyret-eval ast env settings)
  (define (compile-and-run pyret-stx)
   (define env-dict (p:get-dict env))
   (define racket-stx
     (with-handlers
         ([exn:fail?
           (lambda (e) (raise (p:pyret-error p:dummy-loc "eval-error" (format "An error occurred during parsing or typechecking in eval: ~a" (exn-message e)))))])
       (stx->racket
         pyret-stx
         #:toplevel #f
         #:check check-mode
         #:indentation indentation
         #:type-env (extend-env-with-dict LIBRARY-ENV env-dict))))
   (define environment (extend-namespace-with-dict (make-fresh-namespace) env-dict))
   (with-handlers
       ([p:exn:fail:pyret?
         (lambda (e) (raise e))]
        [exn:fail?
         (lambda (e)
          (define marks (continuation-mark-set->list (exn-continuation-marks e) 'pyret-mark))
          (raise (p:pyret-error p:dummy-loc "eval-error" (format "An error occurred while evaluating an eval: ~a ~a" (exn-message e) marks))))])
     (eval racket-stx environment)))
  (define (get-or-false name)
    (cond
      [(p:has-field? settings name)
       (ffi-unwrap (p:get-field p:dummy-loc settings name))]
      [else #f]))
  (define check-mode (get-or-false "check-mode")) 
  (define indentation (get-or-false "indentation")) 
  (define timeout (get-or-false "timeout"))
  (define pyret-stx (ffi-unwrap ast))
  (define run (lambda () (compile-and-run pyret-stx)))
  (match pyret-stx
    [(s-prog s (? cons? headers) block)
     (raise (p:pyret-error p:dummy-loc "eval-error" (format "Import and provide not allowed in eval")))]
    [_
     (cond
      [timeout (run-for-n-seconds run timeout (p:mk-bool #f))]
      [else (compile-and-run pyret-stx)])]))
      

(define eval-lib
  (p:mk-object (make-string-map
    (list
      (cons "eval" (ffi-wrap pyret-eval))))))

