#lang racket/base

(require
 (except-in "../runtime.rkt" raise)
 racket/format
 "../string-map.rkt"
 "../ffi-helpers.rkt")

(provide (rename-out [export %PYRET-PROVIDE]))

(define pyret-format (ffi-wrap (lambda (template lst)
  (apply format (cons template lst)))))
(define fix-width (ffi-wrap (lambda (str width)
  (~a str #:width width))))

(define export (p:mk-object
  (make-string-map (list
    (cons "format" pyret-format)
    (cons "fix-width" fix-width)))))
