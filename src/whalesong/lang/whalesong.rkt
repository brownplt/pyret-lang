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
           (path->string (resolved-module-path-name pyret-lang-whalesong))]
          [src-syntax (src->module-name src)])
            (strip-context
              #`(module src-syntax (file pyret-lang-whalesong-stx)
                  (r:require (r:rename-in pyret/lang/pyret-lib/list [%PYRET-PROVIDE list]))
                  (r:require (r:rename-in pyret/lang/pyret-lib/error [%PYRET-PROVIDE error]))
                  (r:require (r:rename-in pyret/lang/pyret-lib/builtins [%PYRET-PROVIDE builtins]))
                  #,(pyret->racket src in #:libs 'inline #:toplevel #t))))]))

