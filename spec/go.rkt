#lang racket/base

(require redex)

(define-language πsync
  [v (λ (x) e)
     (chan number)
     (num number)]

  [e+⊥ e ⊥]

  [v+ø v ø]

  [e (make-chan e) (e e) (go e e) (seq e e) (e >! e e) (<! e e) x v]
  [(x y z c) (variable-except λ seq go make-chan <! >! qread qwrite qgo)]

  [q (qread number (λ (x) e))
     (qwrite number v (λ (x) e))
     (qgo e)]

  [Q [q ...]]
  [C [(number v+ø) ...]]

  [E hole (E e) (v E) (go E e) (go v E) (seq E e) (seq v E)
          (E >! e e) (v >! E e) (v >! v E)
          (<! E e) (<! v E)]

  [p {Q C e+⊥}])

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
  [(subst x any (go e_1 e_2)) (go (subst x any e_1) (subst x any e_2))]
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
         {[q ... (qwrite number_this v (λ (x) e))] C ⊥})
    (--> {[q ...] C (<! (chan number_this) (λ (x) e))}
         {[q ... (qread number_this (λ (x) e))] C ⊥})

    (--> {[q_0 ... (qread number_this (λ (x) e)) q ...]
          [(number_1 v+ø_1) ...  (number_this v) (number_n v+ø_n) ...]
          ⊥}
         {[q_0 ... q ...]
          [(number_1 v+ø_1) ...  (number_this ø) (number_n v+ø_n) ...]
          (subst x v e)})
    #;(--> {[(qread number_this (λ (x) e)) q ...]
          [(number_1 v+ø_1) ...  (number_this ø) (number_n v+ø_n) ...]
          ⊥}
         {[q ... (qread number_this (λ (x) e))]
          [(number_1 v+ø_1) ...  (number_this ø) (number_n v+ø_n) ...]
          ⊥})
    (--> {[q_0 ... (qwrite number_this v (λ (x) e)) q ...]
          [(number_1 v+ø_1) ...  (number_this ø) (number_n v+ø_n) ...]
          ⊥}
         {[q_0 ... q ...]
          [(number_1 v+ø_1) ...  (number_this v) (number_n v+ø_n) ...]
          (subst x v e)})
    #;(--> {[(qwrite number_this v_0 (λ (x) e)) q ...]
          [(number_1 v+ø_1) ...  (number_this v) (number_n v+ø_n) ...]
          ⊥}
         {[q ... (qwrite number_this v_0 (λ (x) e))]
          [(number_1 v+ø_1) ...  (number_this v) (number_n v+ø_n) ...]
          ⊥})
    (--> {[q_0 ... (qgo e) q ...] C ⊥}
         {[q_0 ... q ...] C e})

    (--> {[q ...] [(number v+ø) ...] (go e_1 (λ (c) e_2))}
         {[q ... (qgo (e_1 (λ (z) (z >! (chan number_new) (λ (x_done) ⊥)))))]
          [(number_new ø) (number v+ø) ...]
          ((λ (c) e_2) (chan number_new))}
         (where number_new ,(+ 1 (length (term (number ...))))))

   with 
   [(--> {Q C e_1} {Q C e_2})
    (==> e_1 e_2)]))

;(traces →πsync (term ([] [] (make-chan))))

#;(define rw
  (term ([] []
         (make-chan (λ (c) ((num 5) >! c (λ (z) (<! c (λ (y) y)))))))))

(define g2 (term (go
                      (λ (k)
                        (<! c (λ (v) (k v))))
                      (λ (c2)
                        (<! c2 (λ (x) x))))))
(define g1 (term (go
                  (λ (k)
                    ((num 5) >! c (λ (y) (k (num 0)))))
                  (λ (z) ,g2))))

(define is-e (term-match πsync [e #t]))

(define g1-matches (is-e g1))
(define g2-matches (is-e g2))
(printf "~a, ~a" g1-matches g2-matches)
(flush-output)

(define gotest
  (term ([] []
          (make-chan
            (λ (c) ,g1)))))
(traces →πsync gotest)
