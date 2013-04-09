#lang racket/base

(require/typed srfi/13
  [string-contains (String String -> (U Boolean Number))])

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
              [exn:fail:pyret? p:exn:fail:pyret?]
              [mk-exn p:mk-exn]
              [empty-dict p:empty-dict]
              [get-dict p:get-dict]
              [get-seal p:get-seal]
              [get-field p:get-field]
              [get-raw-field p:get-raw-field]
              [apply-fun p:apply-fun]
              [has-field? p:has-field?]
              [reseal p:reseal]
              [flatten p:flatten]
              [pyret-true? p:pyret-true?])
  (rename-out [p-pi pi]
              [print-pfun print]
              [tostring-pfun tostring]
              [seal-pfun seal]
              [brander-pfun brander]
              [check-brand-pfun check-brand]
              [keys-pfun prim-keys]
              [raise-pfun raise]
              [is-nothing-pfun is-nothing]
              [p-else else])
  Any?
  Number?
  String?
  Bool?
  Racket
  nothing)

;(define-type Value (U p-object p-num p-bool
;                      p-str p-fun p-method p-nothing p-opaque))
;(define-type RacketValue (U Number String Boolean p-opaque))

;(define-type-alias MaybeNum (U Number #f))
;(define-type-alias Loc
;  (List (U Path String) MaybeNum MaybeNum MaybeNum MaybeNum))

(define dummy-loc (list "pyret-internal" #f #f #f #f))

;(define-type Dict (HashTable String Value))
;(define-type Seal (U (Setof String) none))
;(define-type Proc (Value * -> Value))

(struct: none () #:transparent)
;; Everything has a seal, a set of brands, and a dict
;; p-base: Seal SetOf Symbol Dict -> p-base
(struct: p-base (seal brands dict) #:transparent)
(struct: p-nothing p-base () #:transparent)
(struct: p-object p-base () #:transparent)
;; p-num : p-base Number -> p-num
(struct: p-num p-base (n) #:transparent)
;; p-bool : p-base Boolean -> p-bool
(struct: p-bool p-base (b) #:transparent)
;; p-str : p-base String -> p-str
(struct: p-str p-base (s) #:transparent)
;; p-fun : p-base (Loc -> Proc) -> p-fun
(struct: p-fun p-base (f) #:transparent)
;; p-method: p-base Proc -> p-method
(struct: p-method p-base (f) #:transparent)


(struct exn:fail:pyret exn:fail (srcloc system? val)
  #:property prop:exn:srclocs
    (lambda (a-struct)
      (list (exn:fail:pyret-srcloc a-struct))))

(define (mk-pyret-exn str loc val sys)
  (exn:fail:pyret str (current-continuation-marks) (apply srcloc loc) sys val))

(struct p-opaque (val))

;; Primitives that are allowed from Pyret land.  Others must be
;; wrapped in opaques.  This may be extended for lists and other
;; Racket built-in types in the future.
(define (allowed-prim? v)
  (or (number? v)
      (string? v)
      (boolean? v)))

(define (apply-racket-fun package-name package-member args)
  (define package (string->symbol package-name))
  (define fun (dynamic-require package (string->symbol package-member)))
  (define (get-val arg)
    (cond
      [(p-opaque? arg) (p-opaque-val arg)]
      [(allowed-prim? arg) arg]
      [else (error (format "apply-racket-fun: Bad argument ~a." arg))]))
  (define result (apply fun (map get-val args)))
  (cond
    [(allowed-prim? result)  result]
    [else (p-opaque result)]))

;; mk-exn: p-exn -> Value
(define (mk-exn e)
  (define loc (pyret-exn-srcloc e))
  (define maybe-path (srcloc-source loc))
  (define maybe-line (srcloc-line loc))
  (define maybe-col (srcloc-column loc))
  (define path (cond
		[(string? maybe-path) maybe-path]
		[(path? maybe-path) (path->string maybe-path)]
		[else "unnamed-pyret-file"]))
  (define line (if maybe-line maybe-line -1))
  (define column (if maybe-col maybe-col -1))
  (mk-object
   (make-immutable-hash 
    (list (cons "value" (pyret-exn-val e))
	  (cons "system" (mk-bool (pyret-exn-system? e)))
	  (cons "path" (mk-str path))
	  (cons "line" (mk-num line))
	  (cons "column" (mk-num column))))))

;; empty-dict: HashOf String Value
(define empty-dict ((make-immutable-hash) '()))

(define nothing (p-nothing (set) (set) empty-dict))

;; filter-opaque : Value -> p-base
(define (filter-opaque v)
  (if (p-opaque? v)
      (error (format "Got opaque: ~a" v))
      v))

;; get-dict : Value -> Dict
(define: (get-dict v)
  (p-base-dict (filter-opaque v)))

;; get-seal : Value -> Seal
(define (get-seal v)
  (p-base-seal (filter-opaque v)))

;; get-brands : Value -> Setof Symbol
(define (get-brands v)
  (p-base-brands (filter-opaque v)))

;; mk-object : Dict -> Value
(define (mk-object dict)
  (p-object (none) (set) dict))

;; mk-num : Number -> Value
(define (mk-num n)
  (p-num (none) (set) meta-num-store n))

;; mk-bool : Boolean -> Value
(define (mk-bool b)
  (p-bool (none) (set) meta-bool-store b))

;; mk-str : String -> Value
(define (mk-str s)
  (p-str (none) (set) meta-str-store s))

;; mk-fun : Proc String -> Value
(define (mk-fun f s)
  (p-fun (none) (set) (make-immutable-hash `(("doc" . ,(mk-str s))
                                             ("_method" . ,(mk-method-method f))))
         (λ ((a : Loc)) f)))

;; mk-fun-nodoc : Proc -> Value
(define (mk-fun-nodoc f)
  (p-fun (none) (set) (make-immutable-hash `(("doc" . ,nothing)
                                             ("_method" . ,(mk-method-method f))))
         (λ (_) f)))

;; mk-internal-fun : (Loc -> Proc) -> Value
(define (mk-internal-fun f)
  (p-fun (none) (set) empty-dict f))

;; mk-method-method : Proc -> p-method
(define (mk-method-method f)
  (p-method (none) (set)
            (make-immutable-hash `(("doc" . ,nothing)))
            (λ (v) (mk-method f))))

;; mk-fun-method : Proc -> p-method
(define (mk-fun-method f)
  (p-method (none) (set)
            (make-immutable-hash `(("doc" . ,(mk-str "method"))))
            (λ (v) (mk-fun f "method-fun"))))

;; mk-method : Proc -> Value
(define (mk-method f)
  (define d (make-immutable-hash `(("_fun" . ,(mk-fun-method f))
                                   ("doc" . ,(mk-str "method")))))
  (p-method (none) (set) d f))

(define exn-brand (gensym 'exn))

(define Racket (mk-object empty-dict))

(define Any?
  (mk-fun-nodoc (λ (o) (mk-bool #t))))

;; is-number? : Value * -> Value
(define (is-number? n)
  (mk-bool (p-num? (first n))))  
(define Number? (mk-fun-nodoc is-number?))

;; is-string : Value * -> Value
(define (is-string? n)
  (mk-bool (p-str? (first n))))
(define String? (mk-fun-nodoc is-string?))

;; bool? : Value * -> Value
(define (bool? n)
  (mk-bool (p-bool? (first n))))

(define Bool? (mk-fun-nodoc bool?))

;; mk-racket-fun : String -> Value
(define (mk-racket-fun f)
  (mk-fun-nodoc
    (λ (args)
      (match (first args)
        [(p-str _ _ _ s)
         (wrap (apply-racket-fun f s (map unwrap (rest args))))]
        [_ (error (format "Racket: expected string as first argument, got ~a" (first args)))]))))

;; pyret-error : Loc STring String -> p-exn
(define (pyret-error loc type message)
  (define full-error (exn+loc->message (mk-str message) loc))
  (define obj (mk-object (make-immutable-hash 
		       (list (cons "message" (mk-str message))
			     (cons "type" (mk-str type))))))
  (mk-pyret-exn full-error loc obj #t))

;; get-raw-field : Loc Value String -> Value
(define (get-raw-field loc v f)
  (define errorstr (format "~a was not found" f))
  (if (has-field? v f)
      (hash-ref (get-dict v) f)
      (raise (pyret-error loc "field-not-found" errorstr))))

;; get-field : Loc Value String -> Value
(define (get-field loc v f)
  (if (eq? v Racket)
      (mk-racket-fun f)
      (match (get-raw-field loc v f)
        [(p-method _ _ _ f)
               (mk-fun-nodoc (λ (args : Value *)
                         ;; TODO(joe): Can this by typechecked?  I think maybe
                         (cast (apply f (cons v args)) Value )))]
        [non-method non-method])))

;; apply-fun : Value Loc Value * -> Values
(define (apply-fun v l args)
  (match v
    [(p-fun _ _ _ f)
     (apply (f l) args)]
    [_
     (raise
      (pyret-error
        l
        "apply-non-function"
        (format "apply-fun: expected function, got ~a" (to-string v))))]))

;; reseal : Value Seal -> Values
(define (reseal v new-seal)
  (match v
    [(p-object _ b h) (p-object new-seal b h)]
    [(p-num _ b h n) (p-num new-seal b h n)]
    [(p-bool _ b h t) (p-bool new-seal b h t)]
    [(p-str _ b h s) (p-str new-seal b h s)]
    [(p-fun _ b h f) (p-fun new-seal b h f)]
    [(p-method _ b h f) (p-method new-seal b h f)]
    [(? p-opaque?) (error "seal: Cannot seal opaque")]
    [(p-nothing _ b h) (error "seal: Cannot seal nothing")]))

;; add-brand : Value Symbol -> Value
(define (add-brand v new-brand)
  (define bs : (Setof Symbol) (set-union (get-brands v) (set new-brand)))
  (match v
    [(p-object s _ h) (p-object s bs h)]
    [(p-num s _ h n) (p-num s bs h n)]
    [(p-bool s _ h b) (p-bool s bs h b)]
    [(p-str sl _ h s) (p-str sl bs h s)]
    [(p-fun s _ h f) (p-fun s bs h f)]
    [(p-method s _ h f) (p-method s bs h f)]
    [(? p-opaque?) (error "brand: Cannot brand opaque")]
    [(p-nothing _ b h) (error "brand: Cannot brand nothing")]))

;; has-brand? : Value Symbol -> Boolean
(define (has-brand? v brand)
  (set-member? (get-brands v) brand))

;; in-seal? : Value String -> Boolean
(define (in-seal? v f)
  (define s (get-seal v))
  (or (none? s) (set-member? s f)))

;; has-field? : Value String -> Boolean
(define (has-field? v f)
  (define d (get-dict v))
  (and (in-seal? v f)
       (hash-has-key? d f)))

;; seal: Value * -> Value
(define (seal vs)
  (define object (first vs))
  (define fields (second vs))
  ;; get-strings : ListofValue -> Setof String
  (define (get-strings strs)
    (foldr (λ (v s)
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

;; get-visible-keys : Dict Seal -> Setof String
(define (get-visible-keys d s)
  (define existing-keys (list->set (hash-keys d)))
  (if (none? s)
      existing-keys
      (set-intersect existing-keys s)))

;; flatten : Loc Value Dict -> Value
(define (flatten loc base extension)
  (when (not (andmap (λ (k) (in-seal? base k)) (hash-keys extension)))
    (raise (pyret-error loc "extend" "extending outside seal")))
  (define d (get-dict base))
  (define s (get-seal base))
  (define new-map (foldr (λ (k d)
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
    [(? p-opaque?) (error "update: Cannot update opaque")]
    [(p-nothing _ _ _) (error "update: Cannot update nothing")]))

;; structural-list->list : Value -> Listof Value
(define (structural-list->list lst)
  (define d (get-dict lst))
  (cond
    [(and (hash-has-key? d "first")
          (hash-has-key? d "rest"))
     (cons (hash-ref d "first")
           (structural-list->list (hash-ref d "rest")))]
    [else empty]))

;; mk-structural-list : ListOf Value -> Value
(define (mk-structural-list lst)
  (cond
    [(empty? lst) (mk-object (make-immutable-hash
        `(("is-empty" . ,(mk-bool #t)))))]
    [(cons? lst) (mk-object (make-immutable-hash
        `(("first" . ,(first lst))
          ("is-empty" . ,(mk-bool #f))
          ("rest" . ,(mk-structural-list (rest lst))))))]))

;; keys : Value * -> Value
(define (keys vs)
  (define obj (first vs))
  (define d (get-dict obj))
  (define s (get-seal obj))
  (mk-structural-list (set-map (get-visible-keys d s) mk-str)))

(define keys-pfun (mk-fun-nodoc keys))

;; mk-brander : Symbol -> Proc
(define (mk-brander sym)
  (λ (v)
    (add-brand (first v) sym)))

;; mk-checker : Symbol -> Proc
(define (mk-checker sym)
  (λ (v)
    (mk-bool (has-brand? (first v) sym))))

;; brander : Value * -> Value
(define (brander _)
  (define sym : Symbol (gensym))
  (mk-object
   (make-immutable-hash 
    `(("brand" .
       ,(mk-fun-nodoc (mk-brander sym)))
      ("check" .
       ,(mk-fun-nodoc (mk-checker sym)))))))

(define brander-pfun (mk-fun-nodoc brander))
(define (pyret-true? v)
  (match v
    [(p-bool _ _ _ #t) #t]
    [else #f]))

;; mk-num-impl : (Number Number -> Number) -> (Value * -> Value)
(define (mk-num-impl op)
  (λ (vs)
    (match (cons (first vs) (second vs))
      [(cons (p-num _ _ _ n1) (p-num _ _ _ n2))
       (mk-num (op n1 n2))]
      [(cons _ _)
       (error (format "num: cannot ~a ~a and ~a" op (first vs) (second vs)))])))

(define (mk-num-bool-impl (op : (Number Number -> Boolean)))
          : (Value * -> Value)
  (λ (vs : Value *)
    (match (cons (first vs) (second vs))
      [(cons (p-num _ _ _ n1) (p-num _ _ _ n2))
       (mk-bool (op n1 n2))]
      [(cons _ _)
       (error (format "num cannot ~a ~a and ~a" op (first vs) (second vs)))])))

(define (mk-single-num-impl (op : (Number -> Value)))
         : (Value * -> Value)
  (λ (vs : Value *)
    (define v (first vs))
    (match v
      [(p-num _ _ _ n) (op n)]
      [_
       (error (format "num: cannot ~a ~a" op v))])))

(define (mk-num-fun (op : (Number Number -> Number))) : Value
  (mk-method (mk-num-impl op)))

(define (mk-single-num-fun (op : (Number -> Value))) : Value
  (mk-method (mk-single-num-impl op)))

(define (numify (f : (Number -> Number)))
         : (Number -> Value)
  (lambda (n) (mk-num (f n))))

(define (stringify (f : (Number -> String)))
         : (Number -> Value)
  (lambda (n) (mk-str (f n))))

(define meta-num-store ((inst make-immutable-hash String Value) '()))
(define (meta-num)
  (when (= (hash-count meta-num-store) 0)
    (set! meta-num-store
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
                           (cast >= (Number Number -> Boolean)))))))))
  meta-num-store)

(define p-pi (mk-num pi))

(define (mk-str-impl (op : (String String -> String)))
         : (Value * -> Value)
  (λ (vs : Value *)
    (define v1 (first vs))
    (define v2 (second vs))
    (match (cons v1 v2)
      [(cons (p-str _ _ _ s1) (p-str _ _ _ s2))
       (mk-str (op s1 s2))]
      [(cons _ _)
       (error (format "str: cannot ~a ~a and ~a" op v1 v2))])))

(define (mk-str-bool-impl (op : (String String -> Boolean)))
          : (Value * -> Value)
  (λ (vs : Value *)
    (define v1 (first vs))
    (define v2 (second vs))
    (match (cons v1 v2)
      [(cons (p-str _ _ _ s1) (p-str _ _ _ s2))
       (mk-bool (op s1 s2))]
      [(cons _ _)
       (error (format "str: cannot ~a ~a and ~a" op v1 v2))])))

(define (mk-single-str-impl (op : (String -> Value)))
         : (Value * -> Value)
  (λ (vs :  Value *)
    (define v (first vs))
    (match v
      [(p-str _ _ _ s) (op s)]
      [_
       (error (format "str: cannot ~a ~a" op v))])))

(define (mk-str-fun (op : (String String -> String))) : Value
  (mk-method (mk-str-impl op)))

(define (mk-single-str-fun (op : (String -> Value))) : Value
  (mk-method (mk-single-str-impl op)))

(define meta-str-store ((inst make-immutable-hash String Value) '()))
(define (meta-str)
  (when (= 0 (hash-count meta-str-store))
    (set! meta-str-store
      (make-immutable-hash
        `(("append" . ,(mk-str-fun string-append))
          ("contains" . ,(mk-method (mk-str-bool-impl
            (lambda (s1 s2) (if (string-contains s1 s2) #t #f)))))
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
      ))))
  meta-str-store)

(define (mk-bool-impl (op : (Boolean Boolean -> Boolean)))
         : (Value * -> Value)
  (λ (vs : Value *)
    (define v1 (first vs))
    (define v2 (second vs))
    (match (cons v1 v2)
      [(cons (p-bool _ _ _ b1) (p-bool _ _ _ b2))
       (mk-bool (op b1 b2))]
      [(cons _ _)
       (error (format "bool: cannot ~a ~a and ~a" op v1 v2))])))

(define (mk-single-bool-impl (op : (Boolean -> Boolean)))
         : (Value * -> Value)
  (λ (vs : Value *)
    (define v (first vs))
    (match v
      [(p-bool _ _ _ b) (mk-bool (op b))]
      [_
       (error (format "bool: cannot ~a ~a" op v))])))

(define (mk-bool-fun (op : (Boolean Boolean -> Boolean))) : Value
  (mk-method (mk-bool-impl op)))

(define (mk-single-bool-fun (op : (Boolean -> Boolean))) : Value
  (mk-method (mk-single-bool-impl op)))

(define meta-bool-store ((inst make-immutable-hash String Value) '()))
(define (meta-bool)
  (when (= (hash-count meta-bool-store) 0)
    (set! meta-bool-store
      ;; this is silly, but I don't know how to convince typed-racket
      ;; that the types are correct! @dbp
      (let [(my-and (lambda (x y) (if x (if y #t #f) #f)))
            (my-or (lambda (x y) (if x #t (if y #t #f))))
            (my-equals (lambda (x y) (equal? x y)))]
        (make-immutable-hash
         `(("and" . ,(mk-bool-fun my-and))
           ("or" . ,(mk-bool-fun my-or))
           ("equals" . ,(mk-bool-fun my-equals))
           ("not" . ,(mk-single-bool-fun 
                      (cast not (Boolean -> Boolean)))))))))
  meta-bool-store)

(define p-else (mk-bool #t))

;; to-string : Value -> String
(define (to-string v)
  (match v
    [(or (p-num _ _ _ p)
         (p-str _ _ _ p))
     (format "~a" p)]
    [(p-bool _ _ _ p)
     (if p "true" "false")]
    [(p-method _ _ _ f) "[[code]]"]
    [(p-object _ _ h)
     (define (to-string-raw-object (h : Dict)) : String
       (define (field-to-string (f : String) (v : Value)) : String
      (format "~a: ~a" f (to-string v)))
       (format "{ ~a }"
               (string-join (hash-map h field-to-string) ", ")))
     (if (has-field? v "tostring")
         (let [(m (get-raw-field dummy-loc v "tostring"))]
           (if (p-method? m)
               ;; NOTE(dbp): this will fail if tostring isn't defined
               ;; as taking only self.
               (match ((cast (p-method-f m) (Value -> Value)) v)
                 [(p-str _ _ _ s) s]
                 [else (to-string-raw-object h)])
               (to-string-raw-object h)))
         (to-string-raw-object h))]
    [v (format "~a" v)]))

(define tostring-pfun (mk-fun-nodoc (λ (o) (mk-str (to-string (first o))))))

(define print-pfun (mk-fun-nodoc (λ (o) (begin (printf "~a\n" (to-string (first o))) nothing)))
)

;; check-brand : Loc -> Value * -> Value
(define check-brand
  (λ (loc)
    (λ (vs)
      (define ck (first vs))
      (define o (second vs))
      (define s (third vs))
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


;; unwrap : Value -> RacketValue
(define (unwrap v)
  (match v
    [(p-num s _ _ n) n]
    [(p-bool s _ _ b) b]
    [(p-str sl _ _ s) s]
    [(? p-opaque?) (if (p-opaque? v) v (error "unreachable, just satisfying TR"))]
    [_ (error (format "unwrap: cannot unwrap ~a for Racket" v))]))

;; wrap : RacketValue -> Value
(define (wrap v)
  (cond
    [(number? v) (mk-num v)]
    [(string? v) (mk-str v)]
    [(boolean? v) (mk-bool v)]
    ;; TODO(joe): do we need the list case right now? It's asymmetric
    ;; with unwrap above
    #;[(list? v) (mk-structural-list (map wrap v))]
    [(p-opaque? v) v]
    [else (error (format "wrap: Bad return value from Racket: ~a" v))]))

;; exn+loc->message : Value Loc -> String
(define (exn+loc->message v l)
  (format
    "~a:~a:~a: Uncaught exception ~a\n"
    (first l)
    (second l)
    (third l)
    (to-string v)))

(define raise-pfun
  (mk-internal-fun
   (λ (loc)
      (λ (o) (raise (mk-pyret-exn (exn+loc->message (first o) loc) loc (first o) #f))))))

(define is-nothing-pfun
  (mk-internal-fun
    (λ (loc)
      (λ (specimens)
        (mk-bool (equal? (first specimens) nothing))))))

;; tie the knot of mutual state problems
(void
  (meta-num)
  (meta-bool)
  (meta-str))

