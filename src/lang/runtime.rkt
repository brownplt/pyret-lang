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
 meta-null
 get-dict
 get-seal
 get-field
 set-field
 has-field?
 reseal
 flatten
 (rename-out [p-pi pi])
 (rename-out [seal-pfun seal])
 (rename-out [brander-pfun brander]))


(define-type Value (U p-object p-list p-num p-bool p-str p-fun))

(define-type Dict (HashTable String Value))
(define-type Seal (U (Setof String) none))

(struct: none () #:transparent)
;; Everything has a seal, a set of brands, and a dict
(struct: p-base ((seal : Seal)
                 (meta : Dict)
                 (brands : (Setof Symbol))
                 (dict : Dict)) #:transparent)
(struct: p-object p-base () #:transparent)
(struct: p-list p-base ((l : (Listof Value))) #:transparent)
(struct: p-num p-base ((n : Number)) #:transparent)
(struct: p-bool p-base ((b : Boolean)) #:transparent)                
(struct: p-str p-base ((s : String)) #:transparent)
(struct: p-fun p-base ((f : Procedure)) #:transparent)

(define meta-null ((inst make-immutable-hash String Value) '()))

(define: (mk-object (dict : Dict)) : Value
  (p-object (none) meta-null (set) dict))

(define: (mk-list (l : (Listof Value))) : Value
  (p-list (none) meta-null (set) (make-hash) l))

(define: (mk-num (n : Number)) : Value
  (p-num (none) meta-num (set) (make-hash) n))

(define: (mk-bool (b : Boolean)) : Value
  (p-bool (none) meta-null (set) (make-hash) b))

(define: (mk-str (s : String)) : Value
  (p-str (none) meta-str (set) (make-hash) s))

(define: (mk-fun (f : Procedure)) : Value
  (p-fun (none) meta-null (set) (make-hash) f))

(define: (get-dict (v : Value)) : Dict
  (p-base-dict v))

(define: (get-meta (v : Value)) : Dict
  (p-base-meta v))

(define: (get-seal (v : Value)) : Seal
  (p-base-seal v))

(define: (get-brands (v : Value)) : (Setof Symbol)
  (p-base-brands v))

(define: (get-field (v : Value) (f : String)) : Value
  (if (has-field? v f)
      (hash-ref (get-dict v) f (thunk (hash-ref (get-meta v) f)))
      (error (format "get-field: field not found: ~a" f))))

(define: (set-field (o : Value) (f : String) (v : Value)) : Value
  (if (in-seal? o f)
      (begin (hash-set! (get-dict o) f v) v)
      (error (format "set-field: assigned outside seal: ~a" f))))

(define: (reseal (v : Value) (new-seal : Seal)) : Value
  (match v
    [(p-object _ m b h) (p-object new-seal m b h)]
    [(p-list _ m b h l) (p-list new-seal m b h l)]
    [(p-num _ m b h n) (p-num new-seal m b h n)]
    [(p-bool _ m b h t) (p-bool new-seal m b h t)]
    [(p-str _ m b h s) (p-str new-seal m b h s)]
    [(p-fun _ m b h f) (p-fun new-seal m b h f)]))

(define: (add-brand (v : Value) (new-brand : Symbol)) : Value
  (define: bs : (Setof Symbol) (set-union (get-brands v) (set new-brand)))
  (match v
    [(p-object s m _ h) (p-object s m bs h)]
    [(p-list s m _ h l) (p-list s m bs h l)]
    [(p-num s m _ h n) (p-num s m bs h n)]
    [(p-bool s m _ h b) (p-bool s m bs h b)]
    [(p-str sl m _ h s) (p-str sl m bs h s)]
    [(p-fun s m _ h f) (p-fun s m bs h f)]))

(define: (has-brand? (v : Value) (brand : Symbol)) : Boolean
  (set-member? (get-brands v) brand))

(define: (in-seal? (v : Value) (f : String)) : Boolean
  (define s (get-seal v))
  (or (none? s) (set-member? s f)))

(define: (has-field? (v : Value) (f : String)) : Boolean
  (define d (get-dict v))
  (define m (get-meta v))
  (and (in-seal? v f)
       (or (hash-has-key? d f)
           (hash-has-key? m f))))

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
              (define fields-seal (get-strings (p-list-l fields)))
              (define new-seal (if (none? current-seal)
                                   fields-seal
                                   (set-intersect fields-seal current-seal)))]
        (reseal object new-seal))))

(define seal-pfun (mk-fun seal))

(define: (flatten (base : Value)
                  (extension : Dict))
         : Value
  (define m (get-meta base))
  (define d (get-dict base))
  (define s (get-seal base))
  (define existing-keys
    (set-union (list->set (hash-keys m))
               (list->set (hash-keys d))))
  (define keys
    (if (none? s)
        existing-keys
        (set-intersect existing-keys s)))
  (define: (create-member (key : String)) : (Pairof String Value)
    (cons key (hash-ref d key (thunk (hash-ref m key)))))
  (define new-meta
    ((inst make-immutable-hash String Value)
     (set-map keys create-member)))
  (p-object (none) new-meta (set) extension))

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

(define: (mk-num-impl (op : (Number Number -> Number)))
         : (Value Value -> Value)
  (lambda (v1 v2)
    (match (cons v1 v2)
      [(cons (p-num _ _ _ _ n1) (p-num _ _ _ _ n2))
       (mk-num (op n1 n2))]
      [(cons _ _)
       (error (format "num: cannot ~a ~a and ~a" op v1 v2))])))

(define: (mk-single-num-impl (op : (Number -> Value)))
         : (Value -> Value)
  (lambda (v)
    (match v
      [(p-num _ _ _ _ n) (op n)]
      [_
       (error (format "num: cannot ~a ~a" op v))])))

(define: (mk-num-fun (op : (Number Number -> Number))) : Value
  (mk-fun (mk-num-impl op)))

(define: (mk-single-num-fun (op : (Number -> Value))) : Value
  (mk-fun (mk-single-num-impl op)))

(define: (numify (f : (Number -> Number)))
         : (Number -> Value)
  (lambda (n) (mk-num (f n))))

(define: (stringify (f : (Number -> String)))
         : (Number -> Value)
  (lambda (n) (mk-str (f n))))

(define meta-num
  (make-immutable-hash
    `(("add" . ,(mk-num-fun +))
      ("minus" . ,(mk-num-fun -))
      ("divide" . ,(mk-num-fun /))
      ("times" . ,(mk-num-fun *))
      ("sin" . ,(mk-single-num-fun (numify sin)))
      ("cos" . ,(mk-single-num-fun (numify cos)))
      ("sqr" . ,(mk-single-num-fun (numify sqr)))
      ("tostring" . ,(mk-single-num-fun (stringify number->string)))
      ("expt" . ,(mk-num-fun expt)))))

(define p-pi (mk-num pi))

(define: (mk-str-impl (op : (String String -> String)))
         : (Value Value -> Value)
  (lambda (v1 v2)
    (match (cons v1 v2)
      [(cons (p-str _ _ _ _ s1) (p-str _ _ _ _ s2))
       (mk-str (op s1 s2))]
      [(cons _ _)
       (error (format "str: cannot ~a ~a and ~a" op v1 v2))])))

(define: (mk-single-str-impl (op : (String -> Value)))
         : (Value -> Value)
  (lambda (v)
    (match v
      [(p-str _ _ _ _ s) (op s)]
      [_
       (error (format "str: cannot ~a ~a" op v))])))

(define: (mk-str-fun (op : (String String -> String))) : Value
  (mk-fun (mk-str-impl op)))

(define: (mk-single-str-fun (op : (String -> Value))) : Value
  (mk-fun (mk-single-str-impl op)))

(define meta-str
  (make-immutable-hash
    `(("append" . ,(mk-str-fun string-append))
      ("length" . ,(mk-single-str-fun (lambda (s) (mk-num (string-length s)))))
      ("tonumber" . ,(mk-single-str-fun
        (lambda (s)
          (define n (string->number s))
          (if (false? n)
              (error (format "str: non-numeric string ~a" s))
              (mk-num n)))))
  )))

