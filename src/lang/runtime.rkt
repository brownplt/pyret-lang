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
              [pλ p:pλ]
              [mk-fun p:mk-fun]
              [mk-fun-loc p:mk-fun-loc]
              [mk-fun-nodoc p:mk-fun-nodoc]
              [mk-internal-fun p:mk-internal-fun]
              [pμ p:pμ]
              [mk-method p:mk-method]
              [mk-method-loc p:mk-method-loc]
              [mk-structural-list p:mk-structural-list]
              [wrap p:wrap]
              [unwrap p:unwrap]
              [exn:fail:pyret? p:exn:fail:pyret?]
              [mk-exn p:mk-exn]
              [pyret-error p:pyret-error]
              [empty-dict p:empty-dict]
              [get-dict p:get-dict]
              [get-field p:get-field]
              [get-raw-field p:get-raw-field]
              [apply-fun p:apply-fun]
              [arity-error p:arity-error]
              [check-fun p:check-fun]
              [has-field? p:has-field?]
              [extend p:extend]
              [to-string p:to-string]
              [nothing p:nothing]
              [pyret-true? p:pyret-true?])
  (rename-out [p-pi pi]
              [print-pfun print]
              [tostring-pfun tostring]
              [brander-pfun brander]
              [check-brand-pfun check-brand]
              [keys-pfun prim-keys]
              [has-field-pfun prim-has-field]
              [raise-pfun raise]
              [is-nothing-pfun is-nothing]
              [p-else else])
  Any
  Number
  String
  Bool
  nothing)


;(define-type Value (U p-object p-num p-bool
;                      p-str p-fun p-method p-nothing p-opaque))
;(define-type RacketValue (U Number String Boolean p-opaque))

;(define-type-alias MaybeNum (U Number #f))
;(define-type-alias Loc
;  (List (U Path String) MaybeNum MaybeNum MaybeNum MaybeNum))

(define dummy-loc (list "pyret-internal" #f #f #f #f))

(define no-brands (set))

;(define-type Dict (HashTable String Value))
;(define-type Proc (Value * -> Value))

(struct none () #:transparent)
;; p-base: SetOf Symbol Dict -> p-base
(struct p-base (brands dict) #:transparent)
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

(define-syntax (type-specific-bind stx)
  (syntax-case stx (p-nothing p-object p-num p-str p-bool p-fun p-method)
    [(_ p-nothing _ () body) #'body]
    [(_ p-object _ () body) #'body]
    [(_ p-num matchval (n) body)
     (with-syntax [(n-id (datum->syntax #'body (syntax->datum #'n)))]
      #'(let [(n-id (p-num-n matchval))] body))]
    [(_ p-str matchval (s) body)
     (with-syntax [(s-id (datum->syntax #'body (syntax->datum #'s)))]
      #'(let [(s-id (p-str-s matchval))] body))]
    [(_ p-bool matchval (b) body)
     (with-syntax [(b-id (datum->syntax #'body (syntax->datum #'b)))]
      #'(let [(b-id (p-bool-b matchval))] body))]
    [(_ p-fun matchval (f) body)
     (with-syntax [(f-id (datum->syntax #'body (syntax->datum #'f)))]
      #'(let [(f-id (p-fun-f matchval))] body))]
    [(_ p-method matchval (f) body)
     (with-syntax [(f-id (datum->syntax #'body (syntax->datum #'f)))]
      #'(let [(f-id (p-method-f matchval))] body))]))

(define-syntax (py-match stx)
  (syntax-case stx (default)
    [(_ val) #'(error 'py-match (format "py-match fell through on ~a" val))]
    [(_ val [(default v) body])
     (with-syntax [(v-id (datum->syntax #'body (syntax->datum #'v)))]
      #'(let ((v-id val))
        body))]
    [(_ val [(typ b d id ...) body] other ...)
     (with-syntax [
      (b-id (datum->syntax #'body (syntax->datum #'b)))
      (d-id (datum->syntax #'body (syntax->datum #'d)))]

     (syntax/loc #'body
       (let ((matchval val))
        (if ((value-predicate-for typ) matchval)
            (let [(b-id (p-base-brands matchval))
                  (d-id (p-base-dict matchval))]
              (type-specific-bind typ matchval (id ...) body))
            (py-match matchval other ...)))))]))

;; NOTE(joe): the nested syntax/loc below appears necessary to get good
;; profiling and debugging line numbers for the created functions
(define-syntax (pλ stx)
  (syntax-case stx ()
    [(_ (arg ...) doc e ...)
     (quasisyntax/loc stx
      (mk-fun-loc
        (lambda (%loc)
          (case-lambda
            #,(syntax/loc stx [(arg ...) e ...])
            [arity-mismatch-args-list
             (arity-error %loc (quote (arg ...)) arity-mismatch-args-list)]))
        doc))]))

(define-syntax (pλ/internal stx)
  (syntax-case stx ()
    [(_ (loc) (arg ...) e ...)
     (quasisyntax/loc stx
       (mk-internal-fun
         (lambda (loc)
           (case-lambda
             #,(syntax/loc stx [(arg ...) e ...])
             [arity-mismatch-args-list
              (arity-error loc '(arg ...) arity-mismatch-args-list)]))))]))

(define-syntax (pμ stx)
  (syntax-case stx ()
    [(_ (arg ...) doc e ...)
     (quasisyntax/loc stx
      (mk-method-loc
        (lambda (%loc)
          (case-lambda
            #,(syntax/loc stx [(arg ...) e ...])
            [arity-mismatch-args-list
             (arity-method-error %loc '(arg ...) arity-mismatch-args-list)]))
        doc))]))


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

(define nothing (p-nothing no-brands empty-dict))

;; get-dict : Value -> Dict
(define (get-dict v)
  (p-base-dict v))

;; get-brands : Value -> Setof Symbol
(define (get-brands v)
  (p-base-brands v))

;; mk-object : Dict -> Value
(define (mk-object dict)
  (p-object no-brands dict))

;; mk-num : Number -> Value
(define (mk-num n)
  (p-num no-brands meta-num-store n))

;; mk-str : String -> Value
(define (mk-str s)
  (p-str no-brands meta-str-store s))

;; mk-fun : Proc String -> Value
(define (mk-fun f s)
  (p-fun no-brands (make-immutable-hash `(("_doc" . ,(mk-str s))
                                      ("_method" . ,(mk-method-method f s))))
         (λ (_) f)))

;; mk-fun-loc : Proc String -> Value
(define (mk-fun-loc f s)
  (p-fun no-brands
         (make-immutable-hash `(("_doc" . ,(mk-str s))
                                ("_method" . ,(mk-method-method-loc f s))))
         f))

;; mk-fun-nodoc : Proc -> Value
(define (mk-fun-nodoc f)
  (p-fun no-brands (make-immutable-hash `(("_doc" . ,nothing)
                                      ("_method" . ,(mk-method-method-nodoc f))))
         (λ (_) f)))

;; mk-internal-fun : (Loc -> Proc) -> Value
(define (mk-internal-fun f)
  (p-fun no-brands empty-dict f))

;; mk-method-method : Proc String -> p-method
(define (mk-method-method f doc)
  (p-method no-brands
            (make-immutable-hash `(("_doc" . ,(mk-str doc))))
            (λ (_) (λ (self) (mk-method f doc)))))

;; mk-method-method-nodoc : Proc -> p-method
(define (mk-method-method-nodoc f)
  (p-method no-brands
            (make-immutable-hash `(("_doc" . ,nothing)))
            (λ (_) (λ (self) (mk-method-nodoc f)))))

;; mk-method-method-loc : Proc String -> p-method
(define (mk-method-method-loc f doc)
  (p-method no-brands
            (make-immutable-hash `(("_doc" . ,(mk-str doc))))
            (λ (_) (λ (self) (mk-method-loc f doc)))))

;; mk-fun-method : Proc String -> p-method
(define (mk-fun-method f doc)
  (p-method no-brands
            (make-immutable-hash `(("_doc" . ,(mk-str doc))))
            (λ (_) (λ (self) (mk-fun f doc)))))

(define (mk-fun-method-loc f doc)
  (p-method no-brands
            (make-immutable-hash `(("_doc" . ,(mk-str doc))))
            (λ (_) (λ (self) (mk-fun-loc f doc)))))

;; mk-method : Proc String -> Value
(define (mk-method f doc)
  (define d (make-immutable-hash `(("_fun" . ,(mk-fun-method f))
                                   ("_doc" . ,(mk-str doc)))))
  (p-method no-brands d (λ (_) f)))

;; mk-method-nodoc : Proc -> Value
(define (mk-method-nodoc f)
  (define d (make-immutable-hash `(("_fun" . ,(mk-fun-method f))
                                   ("_doc" . ,nothing))))
  (p-method no-brands d (λ (_) f)))

(define (mk-method-loc f doc)
  (define d (make-immutable-hash `(("_fun" . ,(mk-fun-method-loc f doc))
                                   ("_doc" . ,(mk-str doc)))))
  (p-method no-brands d f))

(define exn-brand (gensym 'exn))

;; is-number? : Value * -> Value
(define (is-number? n)
  (mk-bool (p-num? n)))
(define Number (mk-fun-nodoc is-number?))

;; is-string : Value * -> Value
(define (is-string? . n)
  (mk-bool (p-str? (first n))))
(define String (mk-fun-nodoc is-string?))

;; bool? : Value * -> Value
(define (bool? . n)
  (mk-bool (p-bool? (first n))))

(define Bool (mk-fun-nodoc bool?))

;; pyret-error : Loc String String -> p-exn
(define (pyret-error loc type message)
  (define full-error (exn+loc->message (mk-str message) loc))
  (define obj (mk-object (make-immutable-hash 
    (list (cons "message" (mk-str message))
          (cons "type" (mk-str type))))))
  (mk-pyret-exn full-error loc obj #t))

;; get-raw-field : Loc Value String -> Value
(define (get-raw-field loc v f)
  (hash-ref (get-dict v) f
    (lambda()
      (raise (pyret-error loc "field-not-found" (format "~a was not found" f))))))

;; get-field : Loc Value String -> Value
(define (get-field loc v f)
  (define vfield (get-raw-field loc v f))
  (cond
    [(p-method? vfield)
     (mk-fun-loc (λ (loc)
                   (λ args (apply ((p-method-f vfield) loc) (cons v args))))
                 "")]
    [else vfield]))

(define (check-fun v l)
  (cond
    [(p-fun? v) ((p-fun-f v) l)]
    [else
     (raise
      (pyret-error
        l
        "apply-non-function"
        (format "apply-fun: expected function, got ~a" (to-string v))))]))

(define (check-method v l)
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
    [(p-fun _ __ f)
     (apply (f l) args)]
    [(default _)
     (raise
      (pyret-error
        l
        "apply-non-function"
        (format "apply-fun: expected function, got ~a" (to-string v))))]))

(define (arity-method-error loc argnames args)
  (cond
    [(= (length args) 0)
     (raise
      (pyret-error loc
        "Arity mismatch (method): expected ~a arguments and an object, but no arguments or object were provided"
        (length argnames)))]
    [else
     (raise
      (pyret-error
        loc
        "arity-mismatch"
        (format
  "Arity mismatch (method): expected ~a arguments and an object, but the ~a provided argument(s) were:
~a
And the object was:
~a"

          (- (length argnames) 1)
          (- (length args) 1)
          (string-join (map to-string (drop args 1)) "\n")
          (to-string (first args)))))]))

(define (arity-error loc argnames args)
  (raise
    (pyret-error
      loc
      "arity-mismatch"
      (format
"Arity mismatch: expected ~a arguments, but got ~a.  The ~a provided argument(s) were:
~a"
        (length argnames)
        (length args)
        (length args)
        (string-join (map to-string args) "\n")))))

;; add-brand : Value Symbol -> Value
(define (add-brand v new-brand)
  (define bs (set-add (get-brands v) new-brand))
  (py-match v
    [(p-object _ h) (p-object bs h)]
    [(p-num _ h n) (p-num bs h n)]
    [(p-bool _ h b) (p-bool bs h b)]
    [(p-str _ h s) (p-str bs h s)]
    [(p-fun _ h f) (p-fun bs h f)]
    [(p-method _ h f) (p-method bs h f)]
    [(p-nothing b h) (error "brand: Cannot brand nothing")]
    [(default _) (error (format "brand: Cannot brand ~a" v))]))

;; has-brand? : Value Symbol -> Boolean
(define (has-brand? v brand)
  (set-member? (get-brands v) brand))

;; has-field? : Value String -> Boolean
(define (has-field? v f)
  (hash-has-key? (get-dict v) f))

;; extend : Loc Value Dict -> Value
(define (extend loc base extension)
  (define d (get-dict base))
  (define new-map (foldr (λ (p d) (hash-set d (car p) (cdr p))) d extension))
  (py-match base
    [(p-object _ __) (p-object no-brands new-map)]
    [(p-fun _ __ f) (p-fun no-brands new-map f)]
    [(p-num _ __ n) (p-num no-brands new-map n)]
    [(p-str _ __ str) (p-str no-brands new-map str)]
    [(p-method _ __ m) (p-method no-brands new-map m)]
    [(p-bool _ __ t) (p-bool no-brands new-map t)]
    [(p-nothing __ ___) (error "update: Cannot update nothing")]
    [(default _) (error (format "update: Cannot update ~a" base))]))

;; structural-list? : Value -> Boolean
(define (structural-list? v)
  (define d (get-dict v))
  (and (hash-has-key? d "first")
       (hash-has-key? d "rest")))

;; structural-list->list : Value -> Listof Value
(define (structural-list->list lst)
  (define d (get-dict lst))
  (cond
    [(structural-list? lst)
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

;; keys : Value -> Value
(define (keys object)
  (mk-structural-list (map mk-str (hash-keys (get-dict object)))))

(define keys-pfun (mk-fun-nodoc keys))

(define has-field-pfun (pλ/internal (loc) (object field)
  (cond
    [(p-str? field)
     (mk-bool (has-field? object (p-str-s field)))]
    [else
     (raise
      (pyret-error
        loc
        "has-field-non-string"
        (format "has-field: expected string, got ~a" (to-string field))))])))

;; mk-brander : Symbol -> Proc
(define (mk-brander sym)
  (pλ (v)
    "Brands values"
    (add-brand v sym)))

;; mk-checker : Symbol -> Proc
(define (mk-checker sym)
  (pλ (v)
    "Checks brands on values"
    (mk-bool (has-brand? v sym))))

;; brander : -> Value
(define brander-pfun (pλ/internal (_) ()
  (define sym (gensym))
  (mk-object
   (make-immutable-hash 
    `(("brand" .
       ,(mk-brander sym))
      ("test" .
       ,(mk-checker sym)))))))

(define (pyret-true? v)
  (and (p-bool? v) (p-bool-b v)))

(define-syntax-rule (mk-prim-fun op opname wrapper unwrapper (arg ...) (pred ...))
  (pμ (arg ...) ""
    (define preds-passed (and (pred arg) ...))
    (cond
      [preds-passed (wrapper (op (unwrapper arg) ...))]
      [else 
        (define args-strs (list (to-string arg) ...))
        (define args-str (string-join args-strs ", "))
        (define error-val (mk-str (format "Bad args to prim: ~a : ~a" opname args-str)))
        (raise (mk-pyret-exn (exn+loc->message error-val dummy-loc) dummy-loc error-val #f))])))

(define-syntax-rule (mk-num-1 op opname)
  (mk-prim-fun op opname mk-num p-num-n (n) (p-num?)))
(define-syntax-rule (mk-num-2 op opname)
  (mk-prim-fun op opname mk-num p-num-n (n1 n2) (p-num? p-num?)))
(define-syntax-rule (mk-num-2-bool op opname)
  (mk-prim-fun op opname mk-bool p-num-n (n1 n2) (p-num? p-num?)))

;; meta-num-store (Hashof numing value)
(define meta-num-store (make-immutable-hash '()))
(define (meta-num)
  (when (= (hash-count meta-num-store) 0)
    (set! meta-num-store
      (make-immutable-hash
        `(("plus" . ,(mk-num-2 + 'plus))
          ("add" . ,(mk-num-2 + 'add))
          ("minus" . ,(mk-num-2 - 'minus))
          ("divide" . ,(mk-num-2 / 'divide))
          ("times" . ,(mk-num-2 * 'times))
          ("sin" . ,(mk-num-1 sin 'sin))
          ("cos" . ,(mk-num-1 cos 'cos))
          ("sqr" . ,(mk-num-1 sqr 'sqr))
          ("sqrt" . ,(mk-num-1 sqrt 'sqrt))
          ("floor" . ,(mk-num-1 floor 'floor))
          ("tostring" . ,(mk-prim-fun number->string 'tostring mk-str p-num-n (n) (p-num?)))
          ("expt" . ,(mk-num-2 expt 'expt))
          ("equals" . ,(mk-num-2-bool = 'equals))
          ("lessthan" . ,(mk-num-2-bool < 'lessthan))
          ("greaterthan" . ,(mk-num-2-bool > 'greaterthan))
          ("lessequal" . ,(mk-num-2-bool <= 'lessequal))
          ("greaterequal" . ,(mk-num-2-bool >= 'greaterequal))))))
  meta-num-store)
(define p-pi (mk-num pi))

;; meta-str-store (Hashof String value)
(define meta-str-store (make-immutable-hash '()))
(define (meta-str)
  (when (= 0 (hash-count meta-str-store))
    (set! meta-str-store
      (make-immutable-hash
        `(("append" . ,(mk-prim-fun string-append 'append mk-str p-str-s (s1 s2) (p-str? p-str?)))
          ("plus" . ,(mk-prim-fun string-append 'plus mk-str p-str-s (s1 s2) (p-str? p-str?)))
          ("contains" . ,(mk-prim-fun string-contains 'contains mk-bool p-str-s (s1 s2) (p-str? p-str?)))
          ("length" . ,(mk-prim-fun string-length 'length mk-num p-str-s (s) (p-str?)))
          ("tonumber" . ,(mk-prim-fun string->number 'tonumber mk-num p-str-s (s) (p-str?)))
          ("equals" . ,(mk-prim-fun string=? 'equals mk-bool p-str-s (s1 s2) (p-str? p-str?)))
      ))))
  meta-str-store)

(define-syntax-rule (mk-bool-1 op opname)
  (mk-prim-fun op opname mk-bool p-bool-b (b) (p-bool?)))
(define-syntax-rule (mk-bool-2 op opname)
  (mk-prim-fun op opname mk-bool p-bool-b (b1 b2) (p-bool? p-bool?)))

(define (bool->string b) (if b "true" "false"))

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
         `(("and" . ,(mk-bool-2 my-and 'and))
           ("or" . ,(mk-bool-2 my-or 'or))
           ("tostring" . ,(mk-prim-fun bool->string 'tostring mk-str p-bool-b (b) (p-bool?)))
           ("equals" . ,(mk-bool-2 equal? 'equals))
           ("not" . ,(mk-bool-1 not 'not))))))) 
  meta-bool-store)

;; to-string : Value -> String
(define (to-string v)
  (py-match v
    [(p-nothing _ __) "nothing"]
    [(p-num _ __ n) (format "~a" n)]
    [(p-str _ __ s) (format "~a" s)]
    [(p-bool _ __ b) (if b "true" "false")]
    [(p-method _ __ f) "[[code]]"]
    [(p-fun _ __ f) "[[code]]"]
    [(p-object _ h)
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
                 (py-match (((p-method-f m) dummy-loc) v)
                           [(p-str _ __ s) s]
                           [(default _) (to-string-raw-object h)])
                 (to-string-raw-object h)))
           (to-string-raw-object h)))]
    [(default _) (format "~a" v)]))

(define tostring-pfun (mk-fun-nodoc (λ o (mk-str (to-string (first o))))))

(define print-pfun (mk-fun-nodoc (λ o (begin (printf "~a\n" (to-string (first o))) nothing))))


;; check-brand-pfun : Loc -> Value * -> Value
(define check-brand-pfun (pλ/internal (loc) (ck o s)
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
     (error "runtime: check-brand failed")])))

;; unwrap : Value -> RacketValue
(define (unwrap v)
  (py-match v
    [(p-num _ __ n) n]
    [(p-bool _ __ b) b]
    [(p-str _ __ s) s]
    [(default _)
     (cond
      [(p-opaque? v) v]
      [(and (p-object? v) (structural-list? v))
       (map unwrap (structural-list->list v))]
      [else
       (error (format "unwrap: cannot unwrap ~a for Racket" (to-string v)))])]))

;; wrap : RacketValue -> Value
(define (wrap v)
  (cond
    [(number? v) (mk-num v)]
    [(string? v) (mk-str v)]
    [(boolean? v) (mk-bool v)]
    [(list? v) (mk-structural-list v)]
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

(define p-true (p-bool no-brands meta-bool-store #t))
(define p-false (p-bool no-brands meta-bool-store #f))
;; mk-bool : Boolean -> Value
(define (mk-bool b)
  (if b p-true p-false))
(define p-else p-true)

(define Any
  (mk-fun-nodoc (λ o (mk-bool #t))))

