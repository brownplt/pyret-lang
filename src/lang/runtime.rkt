#lang typed/racket

(provide
 (struct-out none)
 (struct-out p-object)
 (struct-out p-list)
 (struct-out p-num)
 (struct-out p-bool)
 (struct-out p-str)
 (struct-out p-fun)
 mk-object
 mk-list
 mk-num
 mk-bool
 mk-str
 mk-fun
 get-dict
 get-seal
 get-field
 has-field?
 reseal
 (rename-out [seal-pfun seal])
 (rename-out [brander-pfun brander]))


(define-type Value (U p-object p-list p-num p-bool p-str p-fun))

(define-type Dict (HashTable String Value))
(define-type Seal (U (Setof String) none))

(struct: none () #:transparent)
;; Everything has a seal, a set of brands, and a dict
(struct: p-base ((seal : Seal)
                 (brands : (Setof Symbol))
                 (dict : Dict)) #:transparent)
(struct: p-object p-base () #:transparent)
(struct: p-list p-base ((l : (Listof Value))) #:transparent)
(struct: p-num p-base ((n : Number)) #:transparent)
(struct: p-bool p-base ((b : Boolean)) #:transparent)                
(struct: p-str p-base ((s : String)) #:transparent)
(struct: p-fun p-base ((f : Procedure)) #:transparent)

(define: (mk-object (dict : Dict)) : Value
  (p-object (none) (set) dict))

(define: (mk-list (l : (Listof Value))) : Value
  (p-list (none) (set) (make-hash) l))

(define: (mk-num (n : Number)) : Value
  (p-num (none) (set) (make-hash) n))

(define: (mk-bool (b : Boolean)) : Value
  (p-bool (none) (set) (make-hash) b))

(define: (mk-str (s : String)) : Value
  (p-str (none) (set) (make-hash) s))

(define: (mk-fun (f : Procedure)) : Value
  (p-fun (none) (set) (make-hash) f))

(define: (get-dict (v : Value)) : Dict
  (match v
    [(p-object _ _ h) h]
    [(p-list _ _ h _) h]
    [(p-num _ _ h _) h]
    [(p-bool _ _ h _) h]
    [(p-str _ _ h _) h]
    [(p-fun _ _ h _) h]))

(define: (get-seal (v : Value)) : Seal
  (match v
    [(p-object s _ _) s]
    [(p-list s _ _ _) s]
    [(p-num s _ _ _) s]
    [(p-bool s _ _ _) s]
    [(p-str s _ _ _) s]
    [(p-fun s _ _ _) s]))

(define: (get-brands (v : Value)) : (Setof Symbol)
  (match v
    [(p-object _ b _) b]
    [(p-list _ b _ _) b]
    [(p-num _ b _ _) b]
    [(p-bool _ b _ _) b]
    [(p-str _ b _ _) b]
    [(p-fun _ b _ _) b]))

(define: (get-field (v : Value) (f : String)) : Value
  (if (has-field? v f)
      (hash-ref (get-dict v) f)
      (error (string-append "get-field: field not found: " f))))

(define: (reseal (v : Value) (new-seal : Seal)) : Value
  (match v
    [(p-object _ b h) (p-object new-seal b h)]
    [(p-list _ b h l) (p-list new-seal b h l)]
    [(p-num _ b h n) (p-num new-seal b h n)]
    [(p-bool _ b h t) (p-bool new-seal b h t)]
    [(p-str _ b h s) (p-str new-seal b h s)]
    [(p-fun _ b h f) (p-fun new-seal b h f)]))

(define: (add-brand (v : Value) (new-brand : Symbol)) : Value
  (define: bs : (Setof Symbol) (set-union (get-brands v) (set new-brand)))
  (match v
    [(p-object s _ h) (p-object s bs h)]
    [(p-list s _ h l) (p-list s bs h l)]
    [(p-num s _ h n) (p-num s bs h n)]
    [(p-bool s _ h b) (p-bool s bs h b)]
    [(p-str sl _ h s) (p-str sl bs h s)]
    [(p-fun s _ h f) (p-fun s bs h f)]))

(define: (has-brand? (v : Value) (brand : Symbol)) : Boolean
  (set-member? (get-brands v) brand))

(define: (has-field? (v : Value) (f : String)) : Boolean
  (define d (get-dict v))
  (define s (get-seal v))
  (and (or (none? s)
           (set-member? s f))
       (hash-has-key? d f)))

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

(define seal-pfun (mk-fun seal))


(define: (brander) : Value
  (define: sym : Symbol (gensym))
  (mk-object 
   (make-hash 
    `(("brand" .
       ,(mk-fun (lambda: ((v : Value))
                 (add-brand v sym))))
      ("check" .
       ,(mk-fun (lambda: ((v : Value))
                 (mk-bool (has-brand? v sym)))))))))

(define brander-pfun (mk-fun brander))

