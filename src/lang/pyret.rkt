#lang racket

(provide
  (rename-out [my-read read]
              [my-read-syntax read-syntax]))

(require
  racket/runtime-path
  syntax/strip-context
  "tokenizer.rkt"
  "compile.rkt"
  "desugar.rkt"
  "typecheck.rkt"
  "eval.rkt")

(define-runtime-module-path parser "parser.rkt")
(define-runtime-module-path pyret-lang "pyret-lang.rkt")
(define-runtime-module-path full-eval "eval.rkt")


(define (my-read in)
  (syntax->datum (my-read-syntax #f in)))

(define (my-read-syntax src in)
  (with-syntax
     ([pyret-lang-stx (path->string (resolved-module-path-name pyret-lang))]
      [full-eval-stx (path->string (resolved-module-path-name full-eval))]
      [stx (pyret->racket/libs src in)])
        #'(module src (file pyret-lang-stx)
            (require (file full-eval-stx))
            (current-read-interaction repl-eval-pyret)
            (void (current-print (print-pyret (current-print))))
            stx)))

