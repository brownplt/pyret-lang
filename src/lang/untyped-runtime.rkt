#lang racket/base

(provide
 mk-pyret-exn
 (rename-out [exn:fail:pyret-val pyret-exn-val]
	     [exn:fail:pyret-system? pyret-exn-system?]
	     [exn:fail:pyret-srcloc pyret-exn-srcloc])
 exn:fail:pyret? 
 p-opaque?
 apply-racket-fun
 (struct-out p-opaque)
 )

(struct exn:fail:pyret exn:fail (srcloc system? val)
  #:property prop:exn:srclocs
    (lambda (a-struct)
      (list (exn:fail:pyret-srcloc a-struct))))

(define (mk-pyret-exn str loc val sys)
  (exn:fail:pyret str (current-continuation-marks) (apply srcloc loc) sys val))

(struct p-opaque (val))

;; Primitives that are allowed from Pyret land.  Others must be
;; wrapped in opaques.  This may be extended for lists and other
;; Racket built-in types in the future.
(define (allowed-prim? v)
  (or (number? v)
      (string? v)
      (boolean? v)))

(define (apply-racket-fun package-name package-member args)
  (define package (string->symbol package-name))
  (define fun (dynamic-require package (string->symbol package-member)))
  (define (get-val arg)
    (cond
      [(p-opaque? arg) (p-opaque-val arg)]
      [(allowed-prim? arg) arg]
      [else (error (format "apply-racket-fun: Bad argument ~a." arg))]))
  (define result (apply fun (map get-val args)))
  (cond
    [(allowed-prim? result)  result]
    [else (p-opaque result)]))

