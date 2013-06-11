#lang racket/base

(require
  pyret/lang/settings
  pyret/lang/pyret
  pyret/lang/runtime
  pyret/lang/typecheck
  pyret/lang/eval
  ;; pyret/whalesong/lang/reader
  racket/cmdline
  racket/list
  racket/match
  racket/pretty
  racket/runtime-path
  racket/syntax)


(define-runtime-path pyret-lang-racket "lang/pyret-lang-racket.rkt")
(module test-shell "lang/pyret-lang-racket.rkt"
  (r:define-namespace-anchor test-shell-anchor)
  (r:provide test-shell-anchor))
(require (submod "." test-shell))

(define (make-fresh-namespace)
  (define ns (namespace-anchor->empty-namespace test-shell-anchor))
  (parameterize ([current-namespace ns])
    (namespace-require pyret-lang-racket)
    (namespace-require '(only racket/base current-read-interaction current-print void))
    (current-read-interaction repl-eval-pyret)
    (current-print print-pyret))
  ns)

(define (process-pyret-error p)
  (match p
    [(p:exn:fail:pyret message cms srcloc system? val)
     (eprintf "~a\n" message)]
    [(exn:fail:pyret/tc message cms srclocs)
     (eprintf "[pyret] Error in type-checking:\n\n~a\n" message)
     (eprintf "\nAt:\n")
     (define (print-loc l)
      (eprintf "~a:~a:~a\n"
        (srcloc-source l)
        (srcloc-line l)
        (srcloc-column l)))
     (void (map print-loc srclocs))
     ]
    [(exn:fail:contract:variable message cms x)
     (eprintf "~a\n" message)]
    [(exn:fail:syntax:unbound message cms x)
     (eprintf "~a\n" message)]
    [(exn:fail message cms)
     (display "Uncaught Racket-land error that Pyret does not understand yet:\n")
     (display p)
     (display (continuation-mark-set->context cms))
     (display "\n\nPlease copy/paste this exception in an email to joe@cs.brown.edu.\n")]
    ))

(define check-mode #f)
(command-line
  #:once-each
  ("--check" "Run in check mode"
   (set! check-mode #t))
  #:args file-and-maybe-other-stuff
  (define pyret-file (simplify-path (path->complete-path (first file-and-maybe-other-stuff))))
  (define-values (base name dir?) (split-path pyret-file))
  (cond
    [check-mode
     (with-handlers ([exn:fail? process-pyret-error])
       (parameterize ([param-compile-check-mode #t])
         (dynamic-require pyret-file #f)))]
    [else
     (with-handlers ([exn:fail? process-pyret-error])
      (dynamic-require pyret-file #f))]))

