#lang racket/base

(require
  pyret/lang/ffi-helpers
  pyret/lang/pyret
  (only-in pyret/lang/pyret-lang-racket checkers)
  pyret/lang/reader
  pyret/lang/runtime
  pyret/lang/typecheck
  pyret/lang/well-formed
  pyret/lang/indentation
  pyret/lang/eval
  pyret/parameters
  ragg/support
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

(define (process-pyret-error str p)
  (flush-output (current-output-port))
  (define (print-loc l)
   (eprintf "~a:~a:~a\n"
     (srcloc-source l)
     (srcloc-line l)
     (srcloc-column l)))
  (define (print-pyret-locs cms)
    (define marks (continuation-mark-set->list cms 'pyret-mark))
    (for ((mark marks))
      (print-loc mark)))
  (match p
    [(exn:fail:parsing message cms locs)
     (eprintf "[pyret] Error in parsing:\n\n~a\n" message)
     (eprintf "\nAt:\n")
     (void (map print-loc locs))]
    [(exn:fail:pyret/tc message cms srclocs)
     (eprintf "[pyret] Error in type-checking:\n\n~a\n" message)
     (eprintf "\nAt:\n")
     (void (map print-loc srclocs))]
    [(exn:fail:pyret/wf message cms srclocs)
     (eprintf "[pyret] Error in well-formedness checking:\n\n~a\n" message)
     (eprintf "\nAt:\n")
     (void (map print-loc srclocs))]
    [(exn:fail:pyret/indent message cms srclocs)
     (eprintf "[pyret] Error in indentation checking:\n\n~a\n" message)
     (eprintf "\nAt:\n")
     (void (map print-loc srclocs))]
    [(p:exn:fail:pyret message cms srcloc system? val)
     (eprintf "[pyret] Runtime error:\n\n~a\n" message)
     (eprintf "At:\n")
     (print-loc srcloc)
     (print-pyret-locs cms)]
    [(exn:fail:contract:variable message cms x)
     (eprintf "~a\n" message)]
    [(exn:fail:syntax:unbound message cms x)
     (eprintf "~a\n" message)]
    [(exn:fail:filesystem message cms)
     (eprintf "Pyret could not load your program because of a filesystem error.  The system reported:\n\n")
     (eprintf "~a\n" message)]
    [(exn:fail message cms)
     (cond
      [(exn:srclocs? p)
       (define locs ((exn:srclocs-accessor p) p))
       (eprintf "[pyret]\n~a\n" message)
       (eprintf "\nAt:\n")
       (void (map print-loc locs))
       (print-pyret-locs cms)]
      [else
       (display "Uncaught Racket-land error that Pyret does not understand yet:\n")
       (print-pyret-locs cms)
       (display p)
       (display (continuation-mark-set->context cms))
       (display "\n\nPlease copy/paste this exception in an email to joe@cs.brown.edu.\n")])]
    ))

(error-display-handler process-pyret-error)

(define check-mode #t)
(command-line
  #:once-each
  ("--print-racket" path "Print a compiled Racket program on stdout"
   (define pyret-file (open-input-file path))
   (pretty-write (syntax->datum (read-syntax path pyret-file))))
  ("--no-checks" "Run without checks"
   (set! check-mode #f))
  ("--no-indentation" "Run without indentation checking"
   (current-indentation-mode #f))
  ("--allow-shadow" "Run without checking for shadowed vars"
   (current-allow-shadowed-vars #t))
  #:args file-and-maybe-other-stuff
  (when (> (length file-and-maybe-other-stuff) 0)
    (define pyret-file (simplify-path (path->complete-path (first file-and-maybe-other-stuff))))
    (define-values (base name dir?) (split-path pyret-file))
    (define (run)
      (cond
        [check-mode
         (parameterize ([current-check-mode #t]
                        [current-print (print-pyret #t)])
          (dynamic-require pyret-file #f))]
        [else
         (dynamic-require pyret-file #f)]))
    (with-handlers
      ([exn:break?
        (lambda (e)
          (printf "[pyret] User or system break\n")
          (flush-output (current-output-port))
          (flush-output (current-error-port)))])
      (run))))
