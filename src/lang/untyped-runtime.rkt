#lang racket/base

(provide mk-pyret-exn)

(struct exn:fail:pyret exn:fail (srcloc)
  #:property prop:exn:srclocs
    (lambda (a-struct)
      (list (exn:fail:pyret-srcloc a-struct))))

(define (mk-pyret-exn str loc)
  (exn:fail:pyret str (current-continuation-marks) (apply srcloc loc)))

