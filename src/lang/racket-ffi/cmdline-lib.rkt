#lang racket/base

(require
 "../runtime.rkt"
 "../ffi-helpers.rkt"
 "../string-map.rkt"
 pyret/parameters)

(define cl-dict
  (make-string-map
   (list
    (cons "command-line-arguments" (ffi-wrap (lambda () (command-line-arguments)))))))

(define cl-obj (p:mk-object cl-dict))

(provide (rename-out (cl-obj %PYRET-PROVIDE)))
