#lang racket

(require redex)
(provide (all-defined-out))

(define-language πret
  (σ ((loc ref) ...))
  (loc natural)
  (Σ ((ref objval) ...))
  (ref natural)
  (objval
   (num-obj obj-meta number)
   (str-obj obj-meta string)
   ;; ... others
   (obj-obj obj-meta)
   (fun-obj obj-meta func))
  (func (λ (x ...) e))
  (dict ((string v) ...))
  (obj-meta (dict
             #| seal/meta/brands later |# ))
  
  (a v (err string))
  (v (vref ref))
  
  (e
   (get-field e e)
   (set-field e e e)
   (app e e ...)
   x
   (set! x e)
   string
   number
   (object ((string e) ...))
   (fun (x ...) e)
   (let (x e) e)
   (vloc loc)
   a)
  
  (E
   hole
   (object ((string_1 v) ...
            (string_e E)
            (string_2 e) ...))
   (get-field E e)
   (get-field v E))
    
  (x (variable-not-otherwise-mentioned)))

(define-metafunction πret
  obj-alloc : objval Σ -> (ref Σ)
  [(obj-alloc objval_new ((ref objval) ...))
   ,(term-let ([ref_new (+ 1 (apply max (term (0 ref ...))))])
              (term (ref_new ((ref_new objval_new) (ref objval) ...))))])

(define-metafunction πret
  obj-get-dict : objval -> dict
  [(obj-get-dict (any (dict))) dict])                    

(define-metafunction πret
  obj-get-field : dict objval -> a
  [(obj-get-field ((string_1 v_1) ...(string v) (string_2 v_2) ...) (str-obj (dict) string))
   v]
  [(obj-get-field any_1 any_2)
   (err "get-field")])

(define-metafunction πret
  obj-lookup : ref Σ -> objval
  [(obj-lookup ref ((ref_1 objval_1) ... (ref objval) (ref_2 objval_2) ...)) objval])
  

(define eval-πret
  (reduction-relation
   πret
   [--> (σ Σ (in-hole E (object ((string v) ...))))
        (σ Σ_new (in-hole E (vref ref_new)))
        (where (ref_new Σ_new)
               (obj-alloc (obj-obj ([(string v) ...])) Σ))
        "E-Object"]
   
   [--> (σ Σ (in-hole E string))
        (σ Σ_new (in-hole E (vref ref_new)))
        (where (ref_new Σ_new)
               (obj-alloc (str-obj ([]) string) Σ))
        "E-String"]
   
   [--> (σ Σ (in-hole E (get-field (vref ref_obj) (vref ref_str))))
        (σ Σ (in-hole E (obj-get-field (obj-get-dict (obj-lookup ref_obj Σ))
                                        (obj-lookup ref_str Σ))))
        "E-GetField"]
   ))
        
        