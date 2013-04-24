#lang racket

(provide
  (rename-out [my-read read]
              [my-read-syntax read-syntax]))

(require
  racket/runtime-path
  syntax/strip-context
  (only-in rnrs/io/ports-6 port-eof?)
  "../../lang/eval.rkt"
  "../../lang/pyret.rkt")

(define-runtime-module-path pyret-lang-racket "../../lang/pyret-lang-racket.rkt")

(define (my-read in)
  (syntax->datum (my-read-syntax #f in)))

(define (my-read-syntax src in)
  (cond
    [(port-eof? in) eof]
    [else
      (with-syntax
         ([pyret-lang-racket-stx
           (path->string (resolved-module-path-name pyret-lang-racket))])
            (strip-context
              #`(module src (file pyret-lang-racket-stx)
                  #,(pyret->racket src in #:libs #f #:toplevel #t))))]))

