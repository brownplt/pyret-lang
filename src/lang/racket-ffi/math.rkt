#lang racket/base

(require
  "../runtime.rkt"
  "../string-map.rkt"
  "../ffi-helpers.rkt"
  (only-in math uniform-dist sample))

(define math-dict
  (make-string-map
    (list
      (cons "uniform-dist" (ffi-wrap uniform-dist))
      (cons "random" (ffi-wrap random))
      (cons "sample" (ffi-wrap sample)))))

(define math-obj (p:mk-object math-dict))

(provide (rename-out [math-obj %PYRET-PROVIDE]))

