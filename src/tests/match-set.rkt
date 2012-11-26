#lang racket

(require (rename-in racket/set [set make-set]) rackunit)
(provide set)

(define-match-expander set
  (lambda (stx)
    (syntax-case stx ()
      [(_ elts ...)
       #`(? set? (app set->list (list-no-order elts ...)))]))
  (lambda (stx)
    (syntax-case stx ()
      [(_ elts ...) #'(make-set elts ...)])))

(check-true
 (match (set 1 2 3)
  [(set _ _ 3) #t]))

(check-true
 (match (set 4 5)
   [(set 4 _) #t]))

(check-exn
 #rx"match"
 (thunk (match (set 1 2 3)
          [(set _ _ 4) #t])))

(check-exn
 #rx"match"
 (thunk (match (set 1 2)
          [(set _ _ 3) 3])))

(check-true (match (set 1 2 (set 3 4) (set (set 2) (set 3)))
              [(set (set _ (set 3)) _ (set 4 _) _) #t]))

(check-true (match (set 1 2 (set 5 4) (set (set 2) (set 3)))
              [(set (set _ (set 2)) _ (set 4 _) _) #t]))

;; why doesn't this one work?
(check-true (match (set 1 2 (set 5 4) (set (set 2) (set 3)))
              [(set (set _ (set 3)) _ (set 5 _) _) #t]))
                              
(check-true (match (set 1 2 (set 3 4) (set (set 2) (set 3)))
              [(set (set _ (set 3)) _ (set 3 _) _) #t]))

(check-equal? (match (set 1 2 (set 3 4) (set (set 2) (set 3)))
                [(set (set x (set 3)) _ (set 3 _) _) x])
              (set 2))