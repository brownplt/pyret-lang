#lang typed/racket

(require/typed "untyped-runtime.rkt"
  [mk-pyret-exn (String Loc -> Any)])

(provide
  (prefix-out p: (struct-out none))
  (prefix-out p: (struct-out p-base))
  (prefix-out p: (struct-out p-nothing))
  (prefix-out p: (struct-out p-object))
  (prefix-out p: (struct-out p-num))
  (prefix-out p: (struct-out p-bool))
  (prefix-out p: (struct-out p-str))
  (prefix-out p: (struct-out p-fun))
  (prefix-out p: (struct-out p-method))
  (rename-out [mk-object p:mk-object]
              [mk-num p:mk-num]
              [mk-bool p:mk-bool]
              [mk-str p:mk-str]
              [mk-fun p:mk-fun]
              [mk-fun-nodoc p:mk-fun-nodoc]
              [mk-method p:mk-method]
              [empty-dict p:empty-dict]
              [get-dict p:get-dict]
              [get-seal p:get-seal]
              [get-field p:get-field]
              [get-raw-field p:get-raw-field]
              [has-field? p:has-field?]
              [reseal p:reseal]
              [flatten p:flatten]
              [pyret-true? p:pyret-true?])
  (rename-out [p-pi pi]
              [print-pfun print]
              [seal-pfun seal]
              [brander-pfun brander]
              [check-brand-pfun check-brand]
              [keys-pfun prim-keys]
              [raise-pfun raise]
              [p-else else])
  Any?
  Number?
  String?
  Bool?
  Racket
  nothing)

(define-type Value (U p-object p-num p-bool
		      p-str p-fun p-method p-nothing p-opaque))

(define-type-alias MaybeNum (U Number #f))
(define-type-alias Loc
  (List (U Path String) MaybeNum MaybeNum MaybeNum MaybeNum))

(define-type Dict (HashTable String Value))
(define-type Seal (U (Setof String) none))

(struct: none () #:transparent)
;; Everything has a seal, a set of brands, and a dict
(struct: p-base ((seal : Seal)
                 (brands : (Setof Symbol))
                 (dict : Dict)) #:transparent)
(struct: p-nothing p-base () #:transparent)
(struct: p-object p-base () #:transparent)
(struct: p-num p-base ((n : Number)) #:transparent)
(struct: p-bool p-base ((b : Boolean)) #:transparent)                
(struct: p-str p-base ((s : String)) #:transparent)
(struct: p-fun p-base ((f : Procedure)) #:transparent)
(struct: p-method p-base ((f : Procedure)) #:transparent)
(struct: p-opaque p-base ((v : Any)) #:transparent)

(define empty-dict ((inst make-immutable-hash String Value) '()))

(define nothing (p-nothing (set) (set) empty-dict))

(define: (mk-object (dict : Dict)) : Value
  (p-object (none) (set) dict))

(define: (mk-num (n : Number)) : Value
  (p-num (none) (set) meta-num n))

(define: (mk-bool (b : Boolean)) : Value
  (p-bool (none) (set) meta-bool b))

(define: (mk-str (s : String)) : Value
  (p-str (none) (set) meta-str s))

(define: (mk-fun (f : Procedure) (s : String)) : Value
  (p-fun (none) (set) (make-immutable-hash `(("doc" . ,(mk-str s))))
	 (λ (_) f)))

(define: (mk-fun-nodoc (f : Procedure)) : Value
  (p-fun (none) (set) (make-immutable-hash `(("doc" . ,nothing)))
	 (λ (_) f)))

(define: (mk-opaque (v : Any)) : Value
  (p-opaque (none) (set) empty-dict v))

(define: (mk-internal-fun (f : Procedure)) : Value
  (p-fun (none) (set) empty-dict f))

(define: (mk-method (f : Procedure)) : Value
  (p-method (none) (set) empty-dict f))

(define: (get-dict (v : Value)) : Dict
  (p-base-dict v))

(define: (get-seal (v : Value)) : Seal
  (p-base-seal v))

(define: (get-brands (v : Value)) : (Setof Symbol)
  (p-base-brands v))

(define Racket (mk-object empty-dict))

(define Any?
  (mk-fun-nodoc (lambda (o) (mk-bool #t))))

(define Number?
  (mk-fun-nodoc (lambda (n)
	    (mk-bool (p-num? n)))))

(define String?
  (mk-fun-nodoc (lambda (n)
	    (mk-bool (p-str? n)))))

(define Bool?
  (mk-fun-nodoc (lambda (n)
	    (mk-bool (p-bool? n)))))

(define: (get-racket-fun (f : String)) : Value
  (define fun (dynamic-require 'racket (string->symbol f)))
  (mk-fun-nodoc (lambda: (args : Value *)
            (wrap (cast (apply fun (map unwrap args)) Any)))))

(define: (get-raw-field (v : Value) (f : String)) : Value
  (if (has-field? v f)
      (hash-ref (get-dict v) f)
      (error (format "get-field: field not found: ~a" f))))

(define: (get-field (v : Value) (f : String)) : Value
  (if (eq? v Racket)
      (get-racket-fun f)
      (match (get-raw-field v f)
        [(p-method _ _ _ f)
               (mk-fun-nodoc (lambda: (args : Value *)
                         ;; TODO(joe): Can this by typechecked?  I think maybe
                         (cast (apply f (cons v args)) Value )))]
        [non-method non-method])))

(define: (reseal (v : Value) (new-seal : Seal)) : Value
  (match v
    [(p-object _ b h) (p-object new-seal b h)]
    [(p-num _ b h n) (p-num new-seal b h n)]
    [(p-bool _ b h t) (p-bool new-seal b h t)]
    [(p-str _ b h s) (p-str new-seal b h s)]
    [(p-fun _ b h f) (p-fun new-seal b h f)]
    [(p-method _ b h f) (p-method new-seal b h f)]
    [(p-opaque _ b h v) (error "seal: Cannot seal opaque")]
    [(p-nothing _ b h) (error "seal: Cannot seal nothing")]))

(define: (add-brand (v : Value) (new-brand : Symbol)) : Value
  (define: bs : (Setof Symbol) (set-union (get-brands v) (set new-brand)))
  (match v
    [(p-object s _ h) (p-object s bs h)]
    [(p-num s _ h n) (p-num s bs h n)]
    [(p-bool s _ h b) (p-bool s bs h b)]
    [(p-str sl _ h s) (p-str sl bs h s)]
    [(p-fun s _ h f) (p-fun s bs h f)]
    [(p-method s _ h f) (p-method s bs h f)]
    [(p-opaque _ b h v) (error "brand: Cannot brand opaque")]
    [(p-nothing _ b h) (error "brand: Cannot brand nothing")]))

(define: (has-brand? (v : Value) (brand : Symbol)) : Boolean
  (set-member? (get-brands v) brand))

(define: (in-seal? (v : Value) (f : String)) : Boolean
  (define s (get-seal v))
  (or (none? s) (set-member? s f)))

(define: (has-field? (v : Value) (f : String)) : Boolean
  (define d (get-dict v))
  (and (in-seal? v f)
       (hash-has-key? d f)))

(define: (seal (object : Value) (fields : Value)) : Value
  (define: (get-strings (strs : (Listof Value))) : (Setof String)
    (foldr (lambda: ((v : Value) (s : (Setof String)))
             (if (p-str? v)
                 (set-add s (p-str-s v))
                 (error (format "seal: found non-string in constraint list: ~a" v))))
           ((inst set String))
           strs))
  (define fields-seal (get-strings (structural-list->list fields)))
  (local [(define current-seal (get-seal object))
          (define new-seal (if (none? current-seal)
                               fields-seal
                               (set-intersect fields-seal current-seal)))]
    (reseal object new-seal)))

(define seal-pfun (mk-fun-nodoc seal))


(define: (get-visible-keys (d : Dict) (s : Seal)) : (Setof String)
  (define existing-keys (list->set (hash-keys d)))
  (if (none? s)
      existing-keys
      (set-intersect existing-keys s)))

(define: (flatten (base : Value)
                  (extension : Dict))
         : Value
  (when (not (andmap (lambda: ([k : String]) (in-seal? base k)) (hash-keys extension)))
    (error "extend: extending outside seal"))
  (define d (get-dict base))
  (define s (get-seal base))
  (define new-map (foldr (lambda: ([k : String] [d : Dict])
                            (hash-set d k (hash-ref extension k)))
                         d
                         (hash-keys extension)))
  (match base
    [(p-object s _ _) (p-object s (set) new-map)]
    [(p-fun s _ _ f) (p-fun s (set) new-map f)]
    [(p-num s _ _ n) (p-num s (set) new-map n)]
    [(p-str s _ _ str) (p-str s (set) new-map str)]
    [(p-method s _ _ m) (p-method s (set) new-map m)]
    [(p-bool s _ _ t) (p-bool s (set) new-map t)]
    [(p-opaque _ _ _ v) (error "update: Cannot update opaque")]
    [(p-nothing _ _ _) (error "update: Cannot update nothing")]))

(define: (structural-list->list (lst : Value)) : (Listof Value)
  (define d (get-dict lst))
  (cond
    [(and (hash-has-key? d "first")
          (hash-has-key? d "rest"))
     (cons (hash-ref d "first")
           (structural-list->list (hash-ref d "rest")))]
    [else empty]))

(define: (mk-structural-list (lst : (Listof Value))) : Value
  (cond
    [(empty? lst) (mk-object (make-immutable-hash
        `(("is-empty" . ,(mk-bool #t)))))]
    [(cons? lst) (mk-object (make-immutable-hash
        `(("first" . ,(first lst))
          ("is-empty" . ,(mk-bool #f))
          ("rest" . ,(mk-structural-list (rest lst))))))]))

(define: (keys (obj : Value)) : Value
  (define d (get-dict obj))
  (define s (get-seal obj))
  (mk-structural-list (set-map (get-visible-keys d s) mk-str)))

(define keys-pfun (mk-fun-nodoc keys))

(define: (brander) : Value
  (define: sym : Symbol (gensym))
  (mk-object 
   (make-immutable-hash 
    `(("brand" .
       ,(mk-fun-nodoc (lambda: ((v : Value))
                 (add-brand v sym))))
      ("check" .
       ,(mk-fun-nodoc (lambda: ((v : Value))
                 (mk-bool (has-brand? v sym)))))))))

(define brander-pfun (mk-fun-nodoc brander))
(define (pyret-true? v)
  (match v
    [(p-bool _ _ _ #t) #t]
    [else #f]))

(define: (mk-num-impl (op : (Number Number -> Number)))
         : (Value Value -> Value)
  (lambda (v1 v2)
    (match (cons v1 v2)
      [(cons (p-num _ _ _ n1) (p-num _ _ _ n2))
       (mk-num (op n1 n2))]
      [(cons _ _)
       (error (format "num: cannot ~a ~a and ~a" op v1 v2))])))

(define: (mk-num-bool-impl (op : (Number Number -> Boolean)))
          : (Value Value -> Value)
  (lambda (v1 v2)
    (match (cons v1 v2)
      [(cons (p-num _ _ _ n1) (p-num _ _ _ n2))
       (mk-bool (op n1 n2))]
      [(cons _ _)
       (error (format "num cannot ~a ~a and ~a" op v1 v2))])))

(define: (mk-single-num-impl (op : (Number -> Value)))
         : (Value -> Value)
  (lambda (v)
    (match v
      [(p-num _ _ _ n) (op n)]
      [_
       (error (format "num: cannot ~a ~a" op v))])))

(define: (mk-num-fun (op : (Number Number -> Number))) : Value
  (mk-method (mk-num-impl op)))

(define: (mk-single-num-fun (op : (Number -> Value))) : Value
  (mk-method (mk-single-num-impl op)))

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
      ("expt" . ,(mk-num-fun expt))
      ("equals" . ,(mk-method
                   (mk-num-bool-impl
                    (cast = (Number Number -> Boolean)))))
      ("lessthan" . ,(mk-method 
                      (mk-num-bool-impl 
                       (cast < (Number Number -> Boolean)))))
      ("greaterthan" . ,(mk-method 
                      (mk-num-bool-impl 
                       (cast > (Number Number -> Boolean)))))
      ("lessequal" . ,(mk-method 
                      (mk-num-bool-impl 
                       (cast <= (Number Number -> Boolean)))))
      ("greaterequal" . ,(mk-method 
                      (mk-num-bool-impl 
                       (cast >= (Number Number -> Boolean))))))))

(define p-pi (mk-num pi))

(define: (mk-str-impl (op : (String String -> String)))
         : (Value Value -> Value)
  (lambda (v1 v2)
    (match (cons v1 v2)
      [(cons (p-str _ _ _ s1) (p-str _ _ _ s2))
       (mk-str (op s1 s2))]
      [(cons _ _)
       (error (format "str: cannot ~a ~a and ~a" op v1 v2))])))

(define: (mk-str-bool-impl (op : (String String -> Boolean)))
          : (Value Value -> Value)
  (lambda (v1 v2)
    (match (cons v1 v2)
      [(cons (p-str _ _ _ s1) (p-str _ _ _ s2))
       (mk-bool (op s1 s2))]
      [(cons _ _)
       (error (format "str: cannot ~a ~a and ~a" op v1 v2))])))

(define: (mk-single-str-impl (op : (String -> Value)))
         : (Value -> Value)
  (lambda (v)
    (match v
      [(p-str _ _ _ s) (op s)]
      [_
       (error (format "str: cannot ~a ~a" op v))])))

(define: (mk-str-fun (op : (String String -> String))) : Value
  (mk-method (mk-str-impl op)))

(define: (mk-single-str-fun (op : (String -> Value))) : Value
  (mk-method (mk-single-str-impl op)))

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
      ("equals" . ,(mk-method (mk-str-bool-impl string=?)))
      ;; NOTE(dbp): this process of writing library code is ridiculous.
      ;; We need to put this elsewhere.
      ;("starts-with" . ,(mk-method (mk-str-bool-impl (lambda (s ) (substring s ))
      ;("from" . ,(mk-method ,(mk-str-fun (lambda (s n) (substring s n (string-length s))))))
      ;("trim" . ,(mk-method ,(mk-single-str-fun (lambda (s) (mk-str (string-trim s))))))
  )))

(define: (mk-bool-impl (op : (Boolean Boolean -> Boolean)))
         : (Value Value -> Value)
  (lambda (v1 v2)
    (match (cons v1 v2)
      [(cons (p-bool _ _ _ b1) (p-bool _ _ _ b2))
       (mk-bool (op b1 b2))]
      [(cons _ _)
       (error (format "bool: cannot ~a ~a and ~a" op v1 v2))])))

(define: (mk-single-bool-impl (op : (Boolean -> Boolean)))
         : (Value -> Value)
  (lambda (v)
    (match v
      [(p-bool _ _ _ b) (mk-bool (op b))]
      [_
       (error (format "bool: cannot ~a ~a" op v))])))

(define: (mk-bool-fun (op : (Boolean Boolean -> Boolean))) : Value
  (mk-method (mk-bool-impl op)))

(define: (mk-single-bool-fun (op : (Boolean -> Boolean))) : Value
  (mk-method (mk-single-bool-impl op)))

(define meta-bool
  ;; this is silly, but I don't know how to convince typed-racket
  ;; that the types are correct! @dbp
  (let [(my-and (lambda (x y) (if x (if y #t #f) #f)))
        (my-or (lambda (x y) (if x #t (if y #t #f))))]
    (make-immutable-hash
     `(("and" . ,(mk-bool-fun my-and))
       ("or" . ,(mk-bool-fun my-or))
       ("not" . ,(mk-single-bool-fun 
                  (cast not (Boolean -> Boolean))))))))

(define p-else (mk-bool #t))

(define: (to-string (v : Value)) : String
  (match v
    [(or (p-num _ _ _ p)
         (p-bool _ _ _ p)
         (p-str _ _ _ p))
     (format "~a" p)]
    [(p-method _ _ _ f) "[[code]]"]
    [(p-object _ _ h)
     (define: (field-to-string (f : String) (v : Value)) : String
      (format "~a : ~a" f (to-string v)))
     (format "{ ~a }" (string-join (hash-map h field-to-string) ", "))]
    [v (format "~a" v)]))

(define print-pfun (mk-fun-nodoc (λ: ([o : Value]) (begin (printf "~a\n" (to-string o)) nothing))))


(define check-brand
  (λ: ((loc : Loc))
      (λ: ((ck : Value)
	   (o : Value)
	   (s : Value))
  (match (cons ck s)
    [(cons (p-fun _ _ _ f) (p-str _ _ _ typname))
     (let ((check-v (((cast f (Loc -> (Value -> Value))) loc) o)))
       (if (and (p-bool? check-v)
		(p-bool-b check-v))
	   o
	   ;; NOTE(dbp): not sure how to give good reporting
	   (error (format "runtime: typecheck failed; expected ~a and got\n~a"
                          typname (to-string o)))))]
    [(cons _ (p-str _ _ _ _))
     (error "runtime: cannot check-brand with non-function")]
    [(cons (p-fun _ _ _ _) _)
     (error "runtime: cannot check-brand with non-string")]))))

(define check-brand-pfun (mk-internal-fun check-brand))


(define: (unwrap (v : Value)) : Any
  (match v
    [(p-num s _ h n) n]
    [(p-bool s _ h b) b]
    [(p-str sl _ h s) s]
    [(p-opaque _ _ _ v) v]
    [_ (error (format "unwrap: cannot unwrap ~a for Racket" v))]))

(define: (wrap (v : Any)) : Value
  (cond
    [(number? v) (mk-num v)]
    [(string? v) (mk-str v)]
    [(boolean? v) (mk-bool v)]
    [(list? v) (mk-structural-list (map wrap v))]
    [else (mk-opaque v)]))

(define: (exn+loc->message [v : Value] [l : Loc]) : String
  (format
    "~a:~a:~a: Uncaught exception ~a\n"
    (first l)
    (second l)
    (third l)
    (to-string v)))

(define raise-pfun
  (mk-internal-fun
   (λ: ([loc : Loc])
      (λ: ([o : Value]) (raise (mk-pyret-exn (exn+loc->message o loc) loc))))))

