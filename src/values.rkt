#lang typed/racket

(provide
 (struct-out none)
 (struct-out p-object)
 (struct-out p-list)
 (struct-out p-num)
 (struct-out p-str)
 get-dict
 get-seal
 reseal
 seal)


(define-type Value (U p-object p-list p-num p-str))

(define-type Dict (HashTable String Value))
(define-type Seal (U (Setof String) none))

(struct: none () #:transparent)
;; Everything has a seal and a dict
(struct: p-object ((seal : Seal) (dict : Dict)) #:transparent)
(struct: p-list ((l : (Listof Value)) (seal : Seal) (dict : Dict)) #:transparent)
(struct: p-num ((n : Number) (seal : Seal) (dict : Dict)) #:transparent)
(struct: p-str ((s : String) (seal : Seal) (dict : Dict)) #:transparent)

(define: (get-dict (v : Value)) : Dict
  (match v
    [(p-object _ h) h]
    [(p-list _ _ h) h]
    [(p-num _ _ h) h]
    [(p-str _ _ h) h]))

(define: (get-seal (v : Value)) : Seal
  (match v
    [(p-object s _) s]
    [(p-list _ s _) s]
    [(p-num _ s _) s]
    [(p-str _ s _) s]))

(define: (reseal (v : Value) (new-seal : Seal)) : Value
  (match v
    [(p-object _ h) (p-object new-seal h)]
    [(p-list l _ h) (p-list l new-seal h)]
    [(p-num n _ h) (p-num n new-seal h)]
    [(p-str s _ h) (p-str s new-seal h)]))

(define: (seal (object : Value) (fields : Value)) : Value
  (define: (get-strings (strs : (Listof Value))) : (Setof String)
    (foldr (lambda: ((v : Value) (s : (Setof String)))
             (if (p-str? v)
                 (set-add s (p-str-s v))
                 (error "seal: found non-string in constraint list")))
           ((inst set String))
           strs))
  (if (not (p-list? fields))
      (error "seal: found non-list as constraint")
      (local [(define current-seal (get-seal object))
              (define effective-seal (if (none? current-seal)
                                         (apply set (hash-keys (get-dict object)))
                                         current-seal))
              (define fields-seal (get-strings (p-list-l fields)))]
        (begin 
          (when (not (subset? fields-seal effective-seal))
            (error "seal: cannot seal unmentionable fields"))
          (reseal object (set-intersect fields-seal effective-seal))))))

