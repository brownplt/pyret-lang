#lang racket/base

(require
  racket/runtime-path
  syntax/strip-context
  (only-in rnrs/io/ports-6 port-eof?)
  "../../lang/eval.rkt"
  "../../lang/pyret.rkt")

(provide
  bare-read-syntax
  read
  (rename-out [my-read-syntax read-syntax]))

(define-runtime-module-path pyret-lang-whalesong "../../lang/pyret-lang-whalesong.rkt")

(define (my-read-syntax src in)
  (cond
    [(port-eof? in) eof]
    [else
      (with-syntax
         ([pyret-lang-whalesong-stx
           (path->string (resolved-module-path-name pyret-lang-whalesong))])
            (strip-context
              #`(module src (file pyret-lang-whalesong-stx)
                  #,(pyret->racket src in #:libs #t #:toplevel #t))))]))

