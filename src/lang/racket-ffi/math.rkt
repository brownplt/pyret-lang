#lang racket/base

(require
  "../runtime.rkt"
  "../string-map.rkt"
  "../ffi-helpers.rkt"
  (only-in math uniform-dist sample))

(define math-dict
  (string-map
    (list
      (cons "uniform-dist" (wrap-racket-fun uniform-dist))
      (cons "random" (wrap-racket-fun random))
      (cons "sample" (wrap-racket-fun sample)))))

(define math-obj (p:mk-object math-dict))

(provide (rename-out [math-obj %PYRET-PROVIDE]))

