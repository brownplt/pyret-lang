#lang racket/base

(require redex)

(define-language πsync
  [v (λ (x) e)
     (chan number)
     (num number)]

  ;; ⊥ means "no running expression", so we'll pick something from the Q
  [e+⊥ e ⊥]

  ;; ø means "empty channel", so the channel can't be read but can be written
  [v+ø v ø]

  [e x v (make-chan) (e e) (go e) (seq e e)
     ;; The versions with fewer arguments are surface syntax, and get desugared
     ;; to the longer forms via CPS
     (e >! e) (e >! e e) (<! e) (<! e e)]

  ;; Entries in the event queue for pending reads/writes/start of new "thread"
  [q (qread number (λ (x) e))
     (qwrite number v (λ (x) e))
     (qgo e)]

  ;; Event queue
  [Q [q ...]]
  ;; Channel states
  [C [(number v+ø) ...]]

  ;; A whole program
  [p {Q C e+⊥}]

  ;; No E for <! or >!, since they always get values post-CPS
  [E hole (E e) (v E) (go E) (seq E e) (seq v E)]

  [(x y z c) (variable-except λ seq go make-chan <! >! qread qwrite qgo)])

;; Boring
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

;; Only CPS <!, >!, and seq fully CPS
;; For values, *including λ*, just apply k
;; For application, CPS the pieces but don't add an extra arg
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
  [(cps e) (λ (k) (k (cps-gos e)))])

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

    (==> ((λ (x) e) v) (subst x v e) "E-App")
    (==> (seq v e) e "E-SeqPop")

    ;; If the current program has reduced to a value, throw it away to let
    ;; the next event fire
    (--> {Q C v} {Q C ⊥} "E-ValPop")

    ;; Create a channel; just allocate it within the channel state as ø
    (--> {Q [(number v+ø) ...] (in-hole E (make-chan))}
         {Q [(number_0 ø) (number v+ø) ...] (in-hole E (chan number_0))}
         "E-CreateChannel"
         (where number_0 ,(+ 1 (length (term (number ...))))))

    ;; Start a go block.  Creates a new channel, and queues a qgo event that
    ;; will evaluate the block (expecting it to be already CPS'd), with a
    ;; continuation that fires a write on the fresh channel.

    ;; More complicated than I'd like because of the need to build a whole
    ;; expression to write to the fresh channel.  But the go block should
    ;; evaluate to *something*, and a channel is the only thing that makes
    ;; sense
    (--> {[q ...] [(number v+ø) ...] (in-hole E (go e))}
         {[q ... (qgo (e (λ (z) (z >! (chan number_new) (λ (x_done) ⊥)))))]
          [(number_new ø) (number v+ø) ...]
          (in-hole E (chan number_new))}
         "E-StartGo"
         (where number_new ,(+ 1 (length (term (number ...))))))

    ;; Enqueue read and write events as their continuation (and the value to
    ;; write in the case of >!).  Note that these are expecting *no
    ;; evaluation context* and leave ⊥ behind
    (--> {[q ...] C (v >! (chan number_this) (λ (x) e))}
         {[q ... (qwrite number_this v (λ (x) e))] C ⊥})
    (--> {[q ...] C (<! (chan number_this) (λ (x) e))}
         {[q ... (qread number_this (λ (x) e))] C ⊥})

    ;; Pop (non-deterministically), any matching event.
    ;; A qread event matches when its channel isn't ø
    ;; A qwrite event matches when its channel *is* ø
    ;; A qgo event always matches
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

   with 
   [(--> {Q C (in-hole E e_1)} {Q C (in-hole E e_2)})
    (==> e_1 e_2)]))

;; At the end, we expect 3 channels:
;; - The "c" channel, holding (num 7)
;; - The first go block channel, holding (num 8)
;; - The second go block channel, holding (num 6)
(define gosendtwice
  (term
    ((λ (c)
      (seq
        (go
          (seq
            ((num 5) >! c)
            (seq
              ((num 6) >! c)
              (seq
                ((num 7) >! c)
                (num 8)))))
        (go
          (seq
            (<! c)
            (<! c)))))
      (make-chan))))
(traces →πsync (term ([] [] (cps-gos ,gosendtwice))))

