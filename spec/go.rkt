#lang racket/base

(require redex)

(define-language πsync
  [v (λ (x) e)
     (chan number)
     (num number)]

  [e+⊥ e ⊥]

  [v+ø v ø]

  [e (make-chan) (e e) (go e) (seq e e) (e >! e) (e >! e e) (<! e) (<! e e) x v]
  [(x y z c) (variable-except λ seq go make-chan <! >! qread qwrite qgo)]

  [q (qread number (λ (x) e))
     (qwrite number v (λ (x) e))
     (qgo e)]

  [Q [q ...]]
  [C [(number v+ø) ...]]

  [E hole (E e) (v E) (go E) (seq E e) (seq v E)]

  [p {Q C e+⊥}])

(define-metafunction πsync
  subst : x any e -> e
  [(subst x any (λ (x) e)) (λ (x) e)]
  [(subst x any (λ (y) e)) (λ (y) (subst x any e))]
  [(subst x any x) any]
  [(subst x any y) y]
  [(subst x any (make-chan)) (make-chan)]
  [(subst x any (num number)) (num number)]
  [(subst x any (chan number)) (chan number)]
  [(subst x any (seq e_1 e_2)) (seq (subst x any e_1) (subst x any e_2))]
  [(subst x any (go e)) (go (subst x any e))]
  [(subst x any (e_1 >! e_2 e_3))
   ((subst x any e_1) >! (subst x any e_2) (subst x any e_3))]
  [(subst x any (<! e_1 e_2)) (<! (subst x any e_1) (subst x any e_2))]
  [(subst x any (e_1 e_2)) ((subst x any e_1) (subst x any e_2))])

(define-metafunction πsync
  cps : e -> e
  [(cps (seq e_1 e_2))
   (λ (k)
    ((cps e_1)
     (λ (v1)
      ((cps e_2)
       (λ (v2)
        (k v2))))))]
  [(cps (<! e))
   (λ (k)
    ((cps e)
     (λ (c)
      (<! c k))))]
  [(cps (e_1 >! e_2))
   (λ (k)
    ((cps e_1)
     (λ (val)
      ((cps e_2)
       (λ (c)
        (val >! c k))))))]
  [(cps (e_1 e_2))
   (λ (k)
    ((cps e_1)
     (λ (v1)
      ((cps e_2)
       (λ (v2)
        (k (v1 v2)))))))]
  [(cps e) (λ (k) (k e))])

(define-metafunction πsync
  cps-gos : e -> e
  [(cps-gos (go e)) (go (cps e))]
  [(cps-gos (seq e_1 e_2))
   (seq (cps-gos e_1) (cps-gos e_2))]
  [(cps-gos (λ (x) e))
   (λ (x) (cps-gos e))]
  [(cps-gos (e_1 e_2)) ((cps-gos e_1) (cps-gos e_2))]
  [(cps-gos (<! e)) (err "<! outside go")]
  [(cps-gos (e >! e)) (err ">! outside go")]
  [(cps-gos e) e])

(define →πsync
  (reduction-relation
    πsync
    #:domain p

    (==> ((λ (x) e) v) (subst x v e))
    (==> (seq v e) e)
    (--> {Q C v} {Q C ⊥})

    (--> {Q [(number v+ø) ...] (in-hole E (make-chan))}
         {Q [(number_0 ø) (number v+ø) ...] (in-hole E (chan number_0))}
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
    (--> {[q_0 ... (qwrite number_this v (λ (x) e)) q ...]
          [(number_1 v+ø_1) ...  (number_this ø) (number_n v+ø_n) ...]
          ⊥}
         {[q_0 ... q ...]
          [(number_1 v+ø_1) ...  (number_this v) (number_n v+ø_n) ...]
          (subst x v e)})
    (--> {[q_0 ... (qgo e) q ...] C ⊥}
         {[q_0 ... q ...] C e})

    (--> {[q ...] [(number v+ø) ...] (in-hole E (go e))}
         {[q ... (qgo (e (λ (z) (z >! (chan number_new) (λ (x_done) ⊥)))))]
          [(number_new ø) (number v+ø) ...]
          (in-hole E (chan number_new))}
         (where number_new ,(+ 1 (length (term (number ...))))))

   with 
   [(--> {Q C (in-hole E e_1)} {Q C (in-hole E e_2)})
    (==> e_1 e_2)]))

(define gosendtwice
  (term
    ((λ (c)
      (seq
        (go
          (seq
            ((num 5) >! c)
            ((num 6) >! c)))
        (go
          (seq
            (<! c)
            (<! c)))))
      (make-chan))))
(traces →πsync (term ([] [] (cps-gos ,gosendtwice))))

