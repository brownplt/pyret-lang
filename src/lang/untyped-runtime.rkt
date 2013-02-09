#lang racket/base

(provide
 mk-pyret-exn
 pyret-exn-val
 exn:fail:pyret?
 mk-opaque
 get-opaque
 p-opaque?
 )

(struct exn:fail:pyret exn:fail (srcloc val)
  #:property prop:exn:srclocs
    (lambda (a-struct)
      (list (exn:fail:pyret-srcloc a-struct))))

(define (mk-pyret-exn str loc val)
  (exn:fail:pyret str (current-continuation-marks) (apply srcloc loc) val))

(define (pyret-exn-val e)
  (exn:fail:pyret-val e))

(struct p-opaque (val))

(define (mk-opaque val)
  (p-opaque val))

(define (get-opaque po)
  (p-opaque-val po))
