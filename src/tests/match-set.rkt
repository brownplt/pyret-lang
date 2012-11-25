#lang racket

(require racket/set rackunit)

(define-match-expander mset
  (lambda (stx)
    (syntax-case stx ()
      [(_ elts ...)
       #`(? (lambda (s)
              ;; A list of matchers for each pattern
              (define matchers (list (match-lambda [elts #t] [else #f]) ...))
              ;; for each element of the set, find all the matchers that match it
              (define (get-matchers set-elt matcher-list)
                (filter (lambda (f) (f set-elt)) matcher-list))
              ;; for each of those matchers m, copy the matchers list with m removed
              (define (get-next-matchers to-remove current-list)
                (map (lambda (m) (remove m current-list)) to-remove))
              ;; the-set should be the same size as matchers before calling set-matches
              (define (set-matches the-set matchers)
                (display (length matchers))
                (newline)
                (cond
                  [(empty? the-set) #t]
                  [else
                   (define matches-this-element (get-matchers (first the-set) matchers))
                   (cond
                     [(empty? matches-this-element) #f]
                     [(cons? matches-this-element)
                      (empty? (filter false? (map (lambda (next-matchers)
                                                    (set-matches (rest the-set) 
                                                                 next-matchers))
                                                  (get-next-matchers matches-this-element
                                                                     matchers))))])]))
              (and (set? s)
                   (equal? (set-count s) #,(length (syntax->datum #'(elts ...))))
                   (set-matches (set->list s) matchers))))])))

(check-true
 (match (set 1 2 3)
  [(mset _ _ 3) #t]))

(check-exn
 #rx"match"
 (thunk (match (set 1 2 3)
          [(mset _ _ 4) #t])))

(check-exn
 #rx"match"
 (thunk (match (set 1 2)
          [(mset _ _ 3) 3])))

(check-true (match (set 1 2 (set 3 4) (set (set 2) (set 3)))
              [(mset (mset _ (mset 3)) _ (mset 4 _) _) #t]))

;; why doesn't this one work?
(match (set 1 2 (set 3 4) (set (set 2) (set 3)))
  [(mset (mset _ (mset 3)) _ (mset 3 _) _) #t])
                              
