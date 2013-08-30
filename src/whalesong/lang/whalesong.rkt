#lang racket/base

(require
  racket/runtime-path
  syntax/strip-context
  (only-in rnrs/io/ports-6 port-eof?)
  "../../parameters.rkt"
  "../../lang/type-env.rkt"
  "../../lang/eval.rkt"
  "../../lang/pyret.rkt")

(provide
  bare-read-syntax
  read
  (rename-out [my-read-syntax read-syntax]))

(define-runtime-module-path pyret-lang-whalesong "../../lang/pyret-lang-whalesong.rkt")

(define (my-read-syntax src in)
  (when (current-whalesong-repl-print)
   (void (current-check-mode #t))
   (void (current-read-interaction repl-eval-pyret))
   (void (current-print (print-pyret (current-check-mode)))))
  (cond
    [(port-eof? in) eof]
    [else
      (with-syntax
         ([pyret-lang-whalesong-stx
           (path->string (resolved-module-path-name pyret-lang-whalesong))]
          [src-syntax (src->module-name src)])
            (strip-context
              #`(module src-syntax (file pyret-lang-whalesong-stx)
                  #,(pyret->racket src in #:toplevel #t #:type-env WHALESONG-ENV))))]))

