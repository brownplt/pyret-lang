#lang racket/base

(provide
 mk-pyret-exn
 mk-opaque
 get-opaque
 p-opaque?
 )

(struct exn:fail:pyret exn:fail (srcloc)
  #:property prop:exn:srclocs
    (lambda (a-struct)
      (list (exn:fail:pyret-srcloc a-struct))))

(define (mk-pyret-exn str loc)
  (exn:fail:pyret str (current-continuation-marks) (apply srcloc loc)))

(struct p-opaque (val))

(define (mk-opaque val)
  (p-opaque val))

(define (get-opaque po)
  (p-opaque-val po))
