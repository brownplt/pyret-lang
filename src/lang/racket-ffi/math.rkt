#lang racket/base

(require
  "../runtime.rkt"
  "../ffi-helpers.rkt"
  (only-in math uniform-dist sample))

(define math-dict
  (make-immutable-hash
    (list
      (cons "uniform-dist" (wrap-racket-fun uniform-dist))
      (cons "sample" (wrap-racket-fun sample)))))

(define math-obj (p:mk-object math-dict))

(provide (rename-out [math-obj %PYRET-PROVIDE]))

