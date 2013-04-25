#lang whalesong

;(require racket/set ;; set add union member intersect map)
(require (for-syntax racket/base))

(define (hash-fold f h init)
  (when (not (hash? h)) (error (format "Bad fold: ~a ~a ~a" f h init)))
  (define (_hash-fold f flds init)
    (cond
      [(empty? flds) init]
      [(cons? flds)
       (_hash-fold f
                  (rest flds)
                  (f (car (first flds))
                     (cdr (first flds))
                     init))]))
  (_hash-fold f (hash-map h cons) init))

(define (list->set lst)
  (make-immutable-hash (map (lambda (elt) (cons elt #t)) lst)))
(define (set)
  (make-immutable-hash))
(define (set-add s k)
  (hash-set s k #t))
(define (set-union s1 s2)
  (hash-fold (lambda (k v init) (hash-set init k #t)) s1 s2))
(define (set-member? s k)
  (hash-has-key? s k))
(define (set-intersect s1 s2)
  (list->set (filter (lambda (elt) (set-member? s2 elt)) (hash-keys s1))))
(define (set-map s f)
  (map f (hash-keys s)))

(define (sqr x) (* x x))

(define pi 3.1415926) ;; NOTE(joe): good enough for government work

;; NOTE(joe): slow enough for government work
(define (string-contains str substr)
  (define strlen (string-length str))
  (define sublen (string-length substr))
  (cond
    [(> sublen strlen) #f]
    [(string=? substr (substring str 0 sublen)) #t]
    [else (string-contains (substring str 1 strlen) substr)]))


(define (string-join strs sep)
  (cond
    [(empty? strs) ""]
    [(empty? (rest strs)) (first strs)]
    [(cons? (rest strs))
     (string-append (first strs)
      (string-append sep
       (string-join (rest strs) sep)))]))

(provide
  (prefix-out p: (struct-out none))
  (prefix-out p: (struct-out p-opaque))
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
              [mk-internal-fun p:mk-internal-fun]
              [mk-method p:mk-method]
              [wrap p:wrap]
              [unwrap p:unwrap]
              [exn:fail:pyret? p:exn:fail:pyret?]
              [mk-exn p:mk-exn]
              [pyret-error p:pyret-error]
              [empty-dict p:empty-dict]
              [get-dict p:get-dict]
              [get-seal p:get-seal]
              [get-field p:get-field]
              [get-raw-field p:get-raw-field]
              [apply-fun p:apply-fun]
              [check-fun p:check-fun]
              [has-field? p:has-field?]
              [reseal p:reseal]
              [extend p:extend]
              [to-string p:to-string]
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

(struct none () #:transparent)
;; Everything has a seal, a set of brands, and a dict
;; p-base: Seal SetOf Symbol Dict -> p-base
(struct p-base (seal brands dict) #:transparent)
(struct p-nothing p-base () #:transparent)
(struct p-object p-base () #:transparent)
;; p-num : p-base Number -> p-num
(struct p-num p-base (n) #:transparent)
;; p-bool : p-base Boolean -> p-bool
(struct p-bool p-base (b) #:transparent)
;; p-str : p-base String -> p-str
(struct p-str p-base (s) #:transparent)
;; p-fun : p-base (Loc -> Proc) -> p-fun
(struct p-fun p-base (f) #:transparent)
;; p-method: p-base Proc -> p-method
(struct p-method p-base (f) #:transparent)
(struct p-opaque (val))

(define (value-predicate-for typ)
  (cond
    [(eq? p-nothing typ) p-nothing?]
    [(eq? p-object typ) p-object?]
    [(eq? p-num typ) p-num?]
    [(eq? p-bool typ) p-bool?]
    [(eq? p-str typ) p-str?]
    [(eq? p-fun typ) p-fun?]
    [(eq? p-method typ) p-method?]
    [else
     (error 'get-pred (format "py-match doesn't work over ~a" typ))]))

;; Value -> Listof Any
(define (type-specific-fields val)
  (cond
    [(p-nothing? val) '()]
    [(p-object? val) '()]
    [(p-num? val) `(,(p-num-n val))]
    [(p-str? val) `(,(p-str-s val))]
    [(p-bool? val) `(,(p-bool-b val))]
    [(p-fun? val) `(,(p-fun-f val))]
    [(p-method? val) `(,(p-method-f val))]
    [else '()]))

(define-syntax (py-match stx)
  (syntax-case stx (default)
    [(py-match val) #'(error 'py-match (format "py-match fell through on ~a" val))]
    [(py-match val [(default v) body])
     (with-syntax [(v-id (datum->syntax #'body (syntax->datum #'v)))]
      #'(let ((v-id val))
        body))]
    [(py-match val [(typ s b d id ...) body] other ...)
     (with-syntax [
      (s-id (datum->syntax #'body (syntax->datum #'s)))
      (b-id (datum->syntax #'body (syntax->datum #'b)))
      (d-id (datum->syntax #'body (syntax->datum #'d)))
      ((rest-id ...) (datum->syntax #'body (syntax->datum #'(id ...))))]

     (syntax/loc #'body
       (let ((matchval val))
        (if ((value-predicate-for typ) matchval)
            (apply
              (lambda (s-id b-id d-id rest-id ...) body)
              (append
                (list
                  (p-base-seal matchval)
                  (p-base-brands matchval)
                  (p-base-dict matchval))
                (type-specific-fields matchval)))
            (py-match matchval other ...)))))]))

(struct exn:fail:pyret exn:fail (srcloc system? val)
  #:property prop:exn:srclocs
    (lambda (a-struct)
      (list (exn:fail:pyret-srcloc a-struct))))

(define (mk-pyret-exn str loc val sys)
  (exn:fail:pyret str (current-continuation-marks) (apply srcloc loc) sys val))

;; mk-exn: p-exn -> Value
(define (mk-exn e)
  (define loc (exn:fail:pyret-srcloc e))
  (define maybe-path (srcloc-source loc))
  (define maybe-line (srcloc-line loc))
  (define maybe-col (srcloc-column loc))
  (define path (cond
		[(string? maybe-path) maybe-path]
    ;; TODO(joe): removed for WS
		;[(path? maybe-path) (path->string maybe-path)]
		[else "unnamed-pyret-file"]))
  (define line (if maybe-line maybe-line -1))
  (define column (if maybe-col maybe-col -1))
  (mk-object
   (make-immutable-hash 
    (list (cons "value" (exn:fail:pyret-val e))
	  (cons "system" (mk-bool (exn:fail:pyret-system? e)))
	  (cons "path" (mk-str path))
	  (cons "line" (mk-num line))
	  (cons "column" (mk-num column))))))

;; empty-dict: HashOf String Value
(define empty-dict (make-immutable-hash '()))

(define nothing (p-nothing (set) (set) empty-dict))

;; filter-opaque : Value -> p-base
(define (filter-opaque v)
  (if (p-opaque? v)
      (error (format "Got opaque: ~a" v))
      v))

;; get-dict : Value -> Dict
(define (get-dict v)
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
         (λ (_) f)))

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
            (λ _ (mk-method f))))

;; mk-fun-method : Proc -> p-method
(define (mk-fun-method f)
  (p-method (none) (set)
            (make-immutable-hash `(("doc" . ,(mk-str "method"))))
            (λ _ (mk-fun f "method-fun"))))

;; mk-method : Proc -> Value
(define (mk-method f)
  (define d (make-immutable-hash `(("_fun" . ,(mk-fun-method f))
                                   ("doc" . ,(mk-str "method")))))
  (p-method (none) (set) d f))

(define exn-brand (gensym 'exn))

(define Any?
  (mk-fun-nodoc (λ o (mk-bool #t))))

;; is-number? : Value * -> Value
(define (is-number? . n)
  (mk-bool (p-num? (first n))))  
(define Number? (mk-fun-nodoc is-number?))

;; is-string : Value * -> Value
(define (is-string? . n)
  (mk-bool (p-str? (first n))))
(define String? (mk-fun-nodoc is-string?))

;; bool? : Value * -> Value
(define (bool? . n)
  (mk-bool (p-bool? (first n))))

(define Bool? (mk-fun-nodoc bool?))

;; pyret-error : Loc STring String -> p-exn
(define (pyret-error loc type message)
  (define full-error (exn+loc->message (mk-str message) loc))
  (define obj (mk-object (make-immutable-hash 
    (list (cons "message" (mk-str message))
          (cons "type" (mk-str type))))))
  (mk-pyret-exn full-error loc obj #t))

;; get-raw-field : Loc Value String -> Value
(define (get-raw-field loc v f)
  (if (has-field? v f)
      (hash-ref (get-dict v) f)
      (raise (pyret-error loc "field-not-found" (format "~a was not found" f)))))

;; get-field : Loc Value String -> Value
(define (get-field loc v f)
  (py-match (get-raw-field loc v f)
    [(p-method _ __ ___ f)
     (mk-fun-nodoc (λ args (apply f (cons v args))))]
    [(default non-method) non-method]))

(define (check-fun v l)
  (cond
    [(p-fun? v) ((p-fun-f v) l)]
    [else
     (raise
      (pyret-error
        l
        "apply-non-function"
        (format "apply-fun: expected function, got ~a" (to-string v))))]))


;; apply-fun : Value Loc Value * -> Values
(define (apply-fun v l . args)
  (py-match v
    [(p-fun _ __ ___ f)
     (apply (f l) args)]
    [(default _)
     (raise
      (pyret-error
        l
        "apply-non-function"
        (format "apply-fun: expected function, got ~a" (to-string v))))]))

;; reseal : Value Seal -> Values
(define (reseal v new-seal)
  (py-match v
    [(p-object _ b h) (p-object new-seal b h)]
    [(p-num _ b h n) (p-num new-seal b h n)]
    [(p-bool _ b h t) (p-bool new-seal b h t)]
    [(p-str _ b h s) (p-str new-seal b h s)]
    [(p-fun _ b h f) (p-fun new-seal b h f)]
    [(p-method _ b h f) (p-method new-seal b h f)]
    [(p-nothing _ b h) (error "seal: Cannot seal nothing")]
    [(default _) (error (format "seal: Cannot seal ~a" v))]))

;; add-brand : Value Symbol -> Value
(define (add-brand v new-brand)
  (define bs (set-union (get-brands v) (list->set (list new-brand))))
  (py-match v
    [(p-object s _ h) (p-object s bs h)]
    [(p-num s _ h n) (p-num s bs h n)]
    [(p-bool s _ h b) (p-bool s bs h b)]
    [(p-str sl _ h s) (p-str sl bs h s)]
    [(p-fun s _ h f) (p-fun s bs h f)]
    [(p-method s _ h f) (p-method s bs h f)]
    [(p-nothing _ b h) (error "brand: Cannot brand nothing")]
    [(default _) (error (format "brand: Cannot brand ~a" v))]))

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
(define (seal . vs)
  (define object (first vs))
  (define fields (second vs))
  ;; get-strings : ListofValue -> Setof String
  (define (get-strings strs)
    (foldr (λ (v s)
             (if (p-str? v)
                 (set-add s (p-str-s v))
                 (error (format "seal: found non-string in constraint list: ~a" v))))
           (set)
           strs))
  (define fields-seal (get-strings (structural-list->list fields)))
  (define current-seal (get-seal object))
  (define new-seal (if (none? current-seal)
                       fields-seal
                       (set-intersect fields-seal current-seal)))
  (reseal object new-seal))

(define seal-pfun (mk-fun-nodoc seal))

;; get-visible-keys : Dict Seal -> Setof String
(define (get-visible-keys d s)
  (define existing-keys (list->set (hash-keys d)))
  (if (none? s)
      existing-keys
      (set-intersect existing-keys s)))

;; extend : Loc Value Dict -> Value
(define (extend loc base extension)
  (when (not (andmap (λ (k) (in-seal? base k)) (hash-keys extension)))
    (raise (pyret-error loc "extend" "extending outside seal")))
  (define d (get-dict base))
  (define s (get-seal base))
  (define new-map (foldr (λ (k d)
                            (hash-set d k (hash-ref extension k)))
                         d
                         (hash-keys extension)))
  (py-match base
    [(p-object s _ __) (p-object s (set) new-map)]
    [(p-fun s _ __ f) (p-fun s (set) new-map f)]
    [(p-num s _ __ n) (p-num s (set) new-map n)]
    [(p-str s _ __ str) (p-str s (set) new-map str)]
    [(p-method s _ __ m) (p-method s (set) new-map m)]
    [(p-bool s _ __ t) (p-bool s (set) new-map t)]
    [(p-nothing _ __ ___) (error "update: Cannot update nothing")]
    [(default _) (error (format "update: Cannot update ~a" base))]))

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
          ("rest" . ,(mk-structural-list (rest lst))))))]
    [else (error 'mk-structural-list (format "mk-structural-list got ~a" lst))]))

;; keys : Value * -> Value
(define (keys . vs)
  (define obj (first vs))
  (define d (get-dict obj))
  (define s (get-seal obj))
  (mk-structural-list (set-map (get-visible-keys d s) mk-str)))

(define keys-pfun (mk-fun-nodoc keys))

;; mk-brander : Symbol -> Proc
(define (mk-brander sym)
  (λ vs
    (add-brand (first vs) sym)))

;; mk-checker : Symbol -> Proc
(define (mk-checker sym)
  (λ vs
    (mk-bool (has-brand? (first vs) sym))))

;; brander : Value * -> Value
(define (brander . _)
  (define sym (gensym))
  (mk-object
   (make-immutable-hash 
    `(("brand" .
       ,(mk-fun-nodoc (mk-brander sym)))
      ("check" .
       ,(mk-fun-nodoc (mk-checker sym)))))))

(define brander-pfun (mk-fun-nodoc brander))
(define (pyret-true? v)
  (py-match v
    [(p-bool _ __ ___ b) b]
    [(default _) #f]))

;; mk-prim-fun :
;; ((a1 ... an) -> b)
;; Symbol
;; (b -> Value)
;; (a1 -> Boolean ... am -> Boolean)
;; ((U am+1 ... an) -> Boolean)
;; (∩ (Value1 -> a1) ... (Valuen -> an))
;; -> (Value1 ... Valuen -> Value)
(define (mk-prim-fun op opname [pred-default p-base?] [preds (list)] [wrapper wrap] [unwrapper unwrap])
  (mk-method
    (λ args
      (define args-len (length args))
      (define preds-len (length preds))
      (cond
        [(<= preds-len args-len)
         (define preds-passed (andmap (λ (pred arg) (pred arg)) preds (take args preds-len)))
         (define pred-default-passed (andmap (λ (arg) (pred-default arg)) (drop args preds-len)))
         (cond
          [(and preds-passed pred-default-passed)
           (wrapper (apply op (map unwrapper args)))]
          [else
           (define args-strs (map to-string args))
           (define args-str (string-join args-strs ", "))
           (define error-val (mk-str (format "Bad args to prim: ~a : ~a" opname args-str)))
           (raise (mk-pyret-exn (exn+loc->message error-val dummy-loc) dummy-loc error-val #f))])]
        [else
         (define args-strs (map to-string args))
         (define args-str (string-join args-strs ", "))
         (define error-val (mk-str (format "Too many args to prim: ~a : ~a" opname args-str)))
         (raise (mk-pyret-exn (exn+loc->message error-val dummy-loc) dummy-loc error-val #f))]))))

(define (mk-prim-fixed op opname pred len)
  (mk-prim-fun op opname (λ (v) #f) (build-list len (λ (n) pred))))


(define (mk-num-fun op name)
  (mk-prim-fun op name p-num?))

(define (mk-num-fixed op opname len)
  (mk-prim-fixed op opname p-num? len))

;; meta-num-store (Hashof numing value)
(define meta-num-store (make-immutable-hash '()))
(define (meta-num)
  (when (= (hash-count meta-num-store) 0)
    (set! meta-num-store
      (make-immutable-hash
        `(("add" . ,(mk-num-fun + '+))
          ("plus" . ,(mk-num-fun + '+))
          ("minus" . ,(mk-num-fun - '-))
          ("divide" . ,(mk-num-fun / '/))
          ("times" . ,(mk-num-fun * '*))
          ("sin" . ,(mk-num-fixed sin 'sin 1))
          ("cos" . ,(mk-num-fixed cos 'cos 1))
          ("sqr" . ,(mk-num-fixed sqr 'sqr 1))
          ("sqrt" . ,(mk-num-fixed sqrt 'sqrt 1))
          ("tostring" . ,(mk-num-fixed number->string 'tostring 1))
          ("expt" . ,(mk-num-fixed expt 'expt 1))
          ("equals" . ,(mk-num-fixed = 'equals 2))
          ("lessthan" . ,(mk-num-fixed < 'lessthan 2)) 
          ("greaterthan" . ,(mk-num-fixed > 'greaterthan 2)) 
          ("lessequal" . ,(mk-num-fixed <= 'lessequal 2)) 
          ("greaterequal" . ,(mk-num-fixed >= 'greaterequal 2))))))
  meta-num-store)
(define p-pi (mk-num pi))

(define (mk-str-fun op opname)
  (mk-prim-fun op opname p-str?))
(define (mk-str-fixed op opname len)
  (mk-prim-fixed op opname p-str? len))

;; meta-str-store (Hashof String value)
(define meta-str-store (make-immutable-hash '()))
(define (meta-str)
  (when (= 0 (hash-count meta-str-store))
    (set! meta-str-store
      (make-immutable-hash
        `(("append" . ,(mk-str-fun string-append 'append))
          ("contains" . ,(mk-str-fixed string-contains 'contains 2))
          ("length" . ,(mk-str-fixed string-length 'length 1))
          ("tonumber" . ,(mk-str-fixed string->number 'tonumber 1))
          ("equals" . ,(mk-str-fixed string=? 'equals 2))
          ;; NOTE(dbp): this process of writing library code is ridiculous.
          ;; We need to put this elsewhere.
          ;("starts-with" . ,(mk-method (mk-str-bool-impl (lambda (s ) (substring s ))
          ;("from" . ,(mk-method ,(mk-str-fun (lambda (s n) (substring s n (string-length s))))))
          ;("trim" . ,(mk-method ,(mk-single-str-fun (lambda (s) (mk-str (string-trim s))))))
      ))))
  meta-str-store)

(define (mk-bool-fun op opname)
  (mk-prim-fun op opname p-bool?))
(define (mk-bool-fixed op opname len)
  (mk-prim-fixed op opname p-bool? len))

;; meta-bool-store (Hashof String value)
(define meta-bool-store (make-immutable-hash '()))
(define (meta-bool)
  (when (= (hash-count meta-bool-store) 0)
    (set! meta-bool-store
      ;; this is silly, but I don't know how to convince typed-racket
      ;; that the types are correct! @dbp
      (let [(my-and (lambda (x y) (if x (if y #t #f) #f)))
            (my-or (lambda (x y) (if x #t (if y #t #f))))
            (my-equals (lambda (x y) (equal? x y)))]
        (make-immutable-hash
         `(("and" . ,(mk-bool-fixed my-and 'and 2))
           ("or" . ,(mk-bool-fixed my-or 'or 2))
           ("equals" . ,(mk-bool-fixed equal? 'equals 2))
           ("not" . ,(mk-bool-fixed not 'not 1))))))) 
  meta-bool-store)

(define p-else (mk-bool #t))

;; to-string : Value -> String
(define (to-string v)
  (py-match v
    [(p-num _ __ ___ n) (format "~a" n)]
    [(p-str _ __ ___ s) (format "~a" s)]
    [(p-bool _ __ ___ b) (if b "true" "false")]
    [(p-method _ __ ___ f) "[[code]]"]
    [(p-fun _ __ ___ f) "[[code]]"]
    [(p-object _ __ h)
     (let ()
       (define (to-string-raw-object h)
         (define (field-to-string f v)
           (format "~a: ~a" f (to-string v)))
         (format "{ ~a }"
                 (string-join (hash-map h field-to-string) ", ")))
       (if (has-field? v "tostring")
           (let [(m (get-raw-field dummy-loc v "tostring"))]
             (if (p-method? m)
                 ;; NOTE(dbp): this will fail if tostring isn't defined
                 ;; as taking only self.
                 (py-match ((p-method-f m) v)
                           [(p-str _ __ ___ s) s]
                           [(default _) (to-string-raw-object h)])
                 (to-string-raw-object h)))
           (to-string-raw-object h)))]
    [(default _) (format "~a" v)]))

(define tostring-pfun (mk-fun-nodoc (λ o (mk-str (to-string (first o))))))

(define print-pfun (mk-fun-nodoc (λ o (begin (printf "~a\n" (to-string (first o))) nothing)))
)

;; check-brand : Loc -> Value * -> Value
(define check-brand
  (λ (loc)
    (λ vs
      (define ck (first vs))
      (define o (second vs))
      (define s (third vs))
  (cond
    [(and (p-fun? ck) (p-str? s))
     (define f (p-fun-f ck))
     (define typname (p-str-s s))
     (define check-v ((f loc) o))
     (if (and (p-bool? check-v) (p-bool-b check-v))
	       o
         ;; NOTE(dbp): not sure how to give good reporting
         ;; NOTE(joe): This is better, but still would be nice to highlight
         ;;  the call site as well as the destination
         (let [(val (mk-str (format "runtime: typecheck failed; expected ~a and got\n~a"
                              typname (to-string o))))]
         (raise (mk-pyret-exn (exn+loc->message val loc) loc val #f))))]
    [(p-str? s)
     (error "runtime: cannot check-brand with non-function")]
    [(p-fun? ck)
     (error "runtime: cannot check-brand with non-string")]
    [else
     (error "runtime: check-brand failed")]))))

(define check-brand-pfun (mk-internal-fun check-brand))

;; unwrap : Value -> RacketValue
(define (unwrap v)
  (py-match v
    [(p-num _ __ ___ n) n]
    [(p-bool _ __ ___ b) b]
    [(p-str _ __ ___ s) s]
    [(default _)
     (if (p-opaque? v)
         v
         (error (format "unwrap: cannot unwrap ~a for Racket" v)))]))

;; wrap : RacketValue -> Value
(define (wrap v)
  (cond
    [(number? v) (mk-num v)]
    [(string? v) (mk-str v)]
    [(boolean? v) (mk-bool v)]
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
      (λ o (raise (mk-pyret-exn (exn+loc->message (first o) loc) loc (first o) #f))))))

(define is-nothing-pfun
  (mk-internal-fun
    (λ (loc)
      (λ specimens
        (mk-bool (equal? (first specimens) nothing))))))

;; tie the knot of mutual state problems
(void
  (meta-num)
  (meta-bool)
  (meta-str))

