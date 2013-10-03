#lang racket/base

(require
 (except-in "../runtime.rkt" raise)
 "../string-map.rkt"
 "../ffi-helpers.rkt")

(provide (rename-out [pyret-format %PYRET-PROVIDE]))

(define pyret-format (ffi-wrap (lambda (template lst)
  (apply format (cons template lst)))))
