#lang racket/base

(require redex)

(define-language πsync
  [v (λ (x) e)
     (chan number)
     (num number)]

  [e+ø e ø]

  [v+ø v ø]

  [e (make-chan e) (e e) (go e) (seq e e) (e >! e e) (<! e e) x v]
  [(x y z c) (variable-except λ seq go make-chan <! >! qread qwrite qgo)]

  [q (qread number (λ (x) e))
     (qwrite number v (λ (x) e))
     (qgo (λ (x) e))]

  [Q [q ...]]
  [C [(number v+ø) ...]]

  [E hole (E e) (v E) (go E) (seq E e) (seq v E)
          (E >! e e) (v >! E e) (v >! v E)
          (<! E e) (<! v E)]

  [p {Q C e+ø}])

(define-metafunction πsync
  subst : x any e -> e
  [(subst x any (λ (x) e)) (λ (x) e)]
  [(subst x any (λ (y) e)) (λ (y) (subst x any e))]
  [(subst x any x) any]
  [(subst x any y) y]
  [(subst x any (make-chan e)) (make-chan (subst x any e))]
  [(subst x any (num number)) (num number)]
  [(subst x any (chan number)) (chan number)]
  [(subst x any (seq e_1 e_2)) (seq (subst x any e_1) (subst x any e_2))]
  [(subst x any (go e)) (go (subst x any e))]
  [(subst x any (e_1 >! e_2 e_3))
   ((subst x any e_1) >! (subst x any e_2) (subst x any e_3))]
  [(subst x any (<! e_1 e_2)) (<! (subst x any e_1) (subst x any e_2))]
  [(subst x any (e_1 e_2)) ((subst x any e_1) (subst x any e_2))])

(define →πsync
  (reduction-relation
    πsync
    #:domain p

    (==> ((λ (x) e) v) (subst x v e))
    (==> (seq v e) e)

    (--> {Q [(number v+ø) ...] (make-chan (λ (x) e))}
         {Q [(number_0 ø) (number v+ø) ...] ((λ (x) e) (chan number_0))}
         (where number_0 ,(+ 1 (length (term (number ...))))))

    (--> {[q ...] C (v >! (chan number_this) (λ (x) e))}
         {[(qwrite number_this v (λ (x) e))] C ø})
    (--> {[q ...] C (<! (chan number_this) (λ (x) e))}
         {[(qread number_this (λ (x) e))] C ø})

    (--> {[(qread number_this (λ (x) e)) q ...]
          [(number_1 v+ø_1) ...  (number_this v) (number_n v+ø_n) ...]
          ø}
         {[q ...]
          [(number_1 v+ø_1) ...  (number_this ø) (number_n v+ø_n) ...]
          (subst x v e)})
    (--> {[(qwrite number_this v (λ (x) e)) q ...]
          [(number_1 v+ø_1) ...  (number_this ø) (number_n v+ø_n) ...]
          ø}
         {[q ...]
          [(number_1 v+ø_1) ...  (number_this v) (number_n v+ø_n) ...]
          (subst x v e)})

   with 
   [(--> {Q C e_1} {Q C e_2})
    (==> e_1 e_2)]))

;(traces →πsync (term ([] [] (make-chan))))

(define rw
  (term ([] []
         (make-chan (λ (c) ((num 5) >! c (λ (z) (<! c (λ (y) y)))))))))
(traces →πsync rw)
