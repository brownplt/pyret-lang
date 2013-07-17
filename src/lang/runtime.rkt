#lang whalesong

;(require racket/set ;; set add union member intersect map)
(require (for-syntax racket/base))
(require "string-map.rkt")

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
  (prefix-out p:
    (combine-out
      make-string-map
      py-match
      (struct-out none)
      (struct-out p-opaque)
      (struct-out p-base)
      (struct-out p-nothing)
      (struct-out p-object)
      (struct-out p-num)
      (struct-out p-bool)
      p-true
      p-false
      (struct-out p-str)
      (struct-out p-fun)
      (struct-out p-method)
      (struct-out exn:fail:pyret)
      mk-object
      mk-num
      mk-bool
      mk-str
      arity-catcher
      pλ
      mk-fun
      mk-fun-nodoc
      mk-fun-nodoc-slow
      pμ
      mk-method
      mk-structural-list
      structural-list?
      structural-list->list
      mk-exn
      pyret-error
      empty-dict
      get-dict
      get-field
      get-raw-field
      apply-fun
      arity-error
      check-str
      has-field?
      extend
      to-string
      nothing
      pyret-true?
      dummy-loc))
  (rename-out [p-pi pi]
              [print-pfun print]
              [tostring-pfun tostring]
              [brander-pfun brander]
              [check-brand-pfun check-brand]
              [keys-pfun prim-keys]
              [num-keys-pfun prim-num-keys]
              [has-field-pfun prim-has-field]
              [raise-pfun raise]
              [is-nothing-pfun is-nothing]
              [p-else else])
  Any
  Number
  String
  Bool
  Object
  Function
  Method
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
;; p-base: (SetOf Symbol) StringMap (Value ... -> Value) -> p-base
(struct p-base (brands dict app method) #:transparent)
(struct p-nothing p-base () #:transparent)
(struct p-object p-base () #:transparent)
;; p-num : p-base Number -> p-num
(struct p-num p-base (n) #:transparent)
;; p-bool : p-base Boolean -> p-bool
(struct p-bool p-base (b) #:transparent)
;; p-str : p-base String -> p-str
(struct p-str p-base (s) #:transparent)
;; p-fun : p-base (Loc -> Proc) -> p-fun
(struct p-fun p-base () #:transparent)
;; p-method: p-base Proc -> p-method
(struct p-method p-base () #:transparent)
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

(define-syntax (maybe-bind stx)
  (syntax-case stx ()
    [(_ [] e) #'e]
    [(_ [(x val) (y val-rest) ...] e)
     (cond
      [(equal? (syntax->datum #'x) '_) #'(maybe-bind [(y val-rest) ...] e)]
      [else
       #'(let [(x val)] (maybe-bind [(y val-rest) ...] e))])]))

(define-syntax (type-specific-bind stx)
  (syntax-case stx (p-nothing p-object p-num p-str p-bool p-fun p-method)
    [(_ p-nothing _ () body) #'body]
    [(_ p-object _ () body) #'body]
    [(_ p-fun _ () body) #'body]
    [(_ p-method _ () body) #'body]
    [(_ p-num matchval (n) body)
     (with-syntax [(n-id (datum->syntax #'body (syntax->datum #'n)))]
      #'(maybe-bind [(n-id (p-num-n matchval))] body))]
    [(_ p-str matchval (s) body)
     (with-syntax [(s-id (datum->syntax #'body (syntax->datum #'s)))]
      #'(maybe-bind [(s (p-str-s matchval))] body))]
    [(_ p-bool matchval (b) body)
     (with-syntax [(b-id (datum->syntax #'body (syntax->datum #'b)))]
      #'(maybe-bind [(b-id (p-bool-b matchval))] body))]))

(define-syntax (py-match stx)
  (syntax-case stx (default)
    [(_ val) #'(error 'py-match (format "py-match fell through on ~a" val))]
    [(_ val [(default v) body])
     (with-syntax [(v-id (datum->syntax #'body (syntax->datum #'v)))]
      #'(let ((v-id val)) body))]
    [(_ val [(typ b d f m id ...) body] other ...)
     (with-syntax
      [(b-id (datum->syntax #'body (syntax->datum #'b)))
       (d-id (datum->syntax #'body (syntax->datum #'d)))
       (f-id (datum->syntax #'body (syntax->datum #'f)))
       (m-id (datum->syntax #'body (syntax->datum #'m)))]
     (syntax/loc #'body
       (let ((matchval val))
        (if ((value-predicate-for typ) matchval)
          (maybe-bind [(b-id (p-base-brands matchval))
                       (d-id (p-base-dict matchval))
                       (f-id (p-base-app matchval))
                       (m-id (p-base-method matchval))]
              (type-specific-bind typ matchval (id ...) body))
            (py-match matchval other ...)))))]))

(define (loc-list loc)
  (define (serialize-source e)
    (cond
      [(symbol? e) (symbol->string e)]
      [(string? e) e]
      [(false? e) "unknown source"]
      [else (error (format "Non-symbol, non-string, non-path value for
                            source: ~a" e))]))
  (list (serialize-source (srcloc-source loc))
        (srcloc-line loc)
        (srcloc-column loc)
        (srcloc-position loc)
        (srcloc-span loc)))

(define (get-top-loc)
  (define cms (continuation-mark-set->list (current-continuation-marks) 'pyret-mark))
  (cond
    [(> (length cms) 0) (loc-list (first cms))]
    [else dummy-loc]))

(define-syntax-rule (arity-catcher (arg ...) e ...)
  (case-lambda
    [(arg ...) e ...]
    [arity-mismatch-args-list
     (arity-error (get-top-loc) (quote (arg ...)) arity-mismatch-args-list)]))

;; NOTE(joe): the nested syntax/loc below appears necessary to get good
;; profiling and debugging line numbers for the created functions
(define-syntax (pλ stx)
  (syntax-case stx ()
    [(_ (arg ...) doc e ...)
     (quasisyntax/loc stx
      (mk-fun
        #,(syntax/loc stx (arity-catcher (arg ...) e ...))
        #,(syntax/loc stx (arity-catcher (_ arg ...) e ...))
        doc))]))

(define-syntax (pλ/internal stx)
  (syntax-case stx ()
    [(_ (loc) (arg ...) e ...)
     (quasisyntax/loc stx
      (mk-fun-nodoc
        #,(syntax/loc stx (arity-catcher (arg ...) e ...))
        #,(syntax/loc stx (arity-catcher (_ arg ...) e ...))))]))

(define-syntax (pμ stx)
  (syntax-case stx ()
    [(_ (arg ...) doc e ...)
     (quasisyntax/loc stx
      (mk-method
        (case-lambda
          #,(syntax/loc stx [(arg ...) e ...])
          [arity-mismatch-args-list
           (arity-method-error (get-top-loc) '(arg ...) arity-mismatch-args-list)])
       doc))]))

(define-syntax (pμ/internal stx)
  (syntax-case stx ()
    [(_ (loc) (arg ...) doc e ...)
     (quasisyntax/loc stx
      (mk-method
        (case-lambda
          #,(syntax/loc stx [(arg ...) e ...])
          [arity-mismatch-args-list
           (arity-method-error (get-top-loc) '(arg ...) arity-mismatch-args-list)])
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
   (make-string-map 
    (list (cons "value" (exn:fail:pyret-val e))
	  (cons "system" (mk-bool (exn:fail:pyret-system? e)))
	  (cons "path" (mk-str path))
	  (cons "line" (mk-num line))
	  (cons "column" (mk-num column))))))

;; empty-dict: HashOf String Value
(define empty-dict (make-string-map '()))

(define (bad-app type)
  (lambda args
    (raise
      (pyret-error
        (get-top-loc)
        "apply-non-function"
        (format "check-fun: expected function, got ~a" (to-string type))))))

(define (bad-meth type)
  (lambda args
    (raise
      (pyret-error
        (get-top-loc)
        "apply-non-method"
        (format "check-method: expected method, got ~a" (to-string type))))))

(define nothing (p-nothing no-brands empty-dict (bad-app "nothing") (bad-app "nothing")))

;; get-dict : Value -> Dict
(define (get-dict v)
  (p-base-dict v))

;; get-brands : Value -> Setof Symbol
(define (get-brands v)
  (p-base-brands v))

(define obj-bad-app (bad-app "object"))
(define obj-bad-meth (bad-meth "object"))
;; mk-object : Dict -> Value
(define (mk-object dict)
  (p-object no-brands dict obj-bad-app obj-bad-meth))

(define num-bad-app (bad-app "number"))
(define num-bad-meth (bad-meth "number"))
;; mk-num : Number -> Value
(define (mk-num n)
  (p-num no-brands meta-num-store num-bad-app num-bad-meth n))

(define str-bad-app (bad-app "string"))
(define str-bad-meth (bad-meth "string"))
;; mk-str : String -> Value
(define (mk-str s)
  (p-str no-brands meta-str-store str-bad-app str-bad-meth s))

;; mk-fun : (Value ... -> Value) String -> Value
(define (mk-fun f m s)
  (p-fun no-brands (make-string-map `(("_doc" . ,(mk-str s))
                                      ("_method" . ,(mk-method-method f s))))
         f
         m))

;; mk-fun-nodoc : (Value ... -> Value) -> Value
(define (mk-fun-nodoc f m)
  (p-fun no-brands (make-string-map `(("_doc" . ,nothing)
                                      ("_method" . ,(mk-method-method-nodoc f))))
         f
         m))

(define (mk-fun-nodoc-slow f)
  (p-fun no-brands empty-dict f (lambda (_ . args) (apply f args))))
  
(define method-bad-app (bad-app "method"))
;; mk-method-method : (Value ... -> Value) String -> p-method
(define (mk-method-method f doc)
  (p-method no-brands
            (make-string-map `(("_doc" . ,(mk-str doc))))
            method-bad-app
            (λ (self) (mk-method f doc))))

;; mk-method-method-nodoc : (Value ... -> Value) -> p-method
(define (mk-method-method-nodoc f)
  (p-method no-brands
            (make-string-map `(("_doc" . ,nothing)))
            method-bad-app
            (λ (self) (mk-method-nodoc f))))

;; mk-fun-method : (Value ... -> Value) String -> p-method
(define (mk-fun-method f doc)
  (p-method no-brands
            (make-string-map `(("_doc" . ,(mk-str doc))))
            method-bad-app
            (λ (self) (mk-fun f (lambda args (apply f (rest args))) doc))))

;; mk-method : (Value ... -> Value) String -> Value
(define (mk-method f doc)
  (define d (make-string-map `(("_fun" . ,(mk-fun-method f doc))
                                   ("_doc" . ,(mk-str doc)))))
  (p-method no-brands d method-bad-app f))

;; mk-method-nodoc : Proc -> Value
(define (mk-method-nodoc f)
  (define d (make-string-map `(("_fun" . ,(mk-fun-method f ""))
                                   ("_doc" . ,nothing))))
  (p-method no-brands d method-bad-app f))

(define exn-brand (gensym 'exn))

;; pyret-error : Loc String String -> p-exn
(define (pyret-error loc type message)
  (define full-error (exn+loc->message (mk-str message) loc))
  (define obj (mk-object (make-string-map 
    (list (cons "message" (mk-str message))
          (cons "type" (mk-str type))))))
  (mk-pyret-exn full-error loc obj #t))

;; get-raw-field : Loc Value String -> Value
(define (get-raw-field loc v f)
  (string-map-ref (get-dict v) f
    (lambda()
      (raise (pyret-error loc "field-not-found" (format "~a was not found on ~a" f (to-string v)))))))

;; get-field : Loc Value String -> Value
(define (get-field loc v f)
  (define vfield (get-raw-field loc v f))
  (cond
    [(p-method? vfield)
     (let [(curried
            (mk-fun (λ args (apply (p-base-method vfield)
                                   (cons v args)))
                    (λ args (apply (p-base-method vfield)
                                   (cons v (rest args))))
                    (get-field loc vfield "_doc")))]
       (if (has-field? vfield "tostring")
           (extend loc curried
                   (list
                    (cons "tostring"
                          (get-field loc vfield "tostring"))))
           curried))]
    [else vfield]))

(define (check-str v l)
  (cond
    [(p-str? v) (p-str-s v)]
    [else
     (raise
       (pyret-error
        l
        "field-non-string"
        (format "field: expected string, got ~a" (to-string v))))]))

;; apply-fun : Value Loc Value * -> Values
(define (apply-fun v l . args)
  (py-match v
    [(p-fun _ _ f _)
     (apply f args)]
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
    [(p-object _ h f m) (p-object bs h f m)]
    [(p-num _ h f m n) (p-num bs h f m n)]
    [(p-bool _ h f m b) (p-bool bs h f m b)]
    [(p-str _ h f m s) (p-str bs h f m s)]
    [(p-fun _ h f m) (p-fun bs h f m)]
    [(p-method _ h f m) (p-method bs h f m)]
    [(p-nothing b h f m) (error "brand: Cannot brand nothing")]
    [(default _) (error (format "brand: Cannot brand ~a" v))]))

;; has-brand? : Value Symbol -> Boolean
(define (has-brand? v brand)
  (set-member? (get-brands v) brand))

;; has-field? : Value String -> Boolean
(define (has-field? v f)
  (string-map-has-key? (get-dict v) f))

;; extend : Loc Value Dict -> Value
(define (extend loc base extension)
  (define d (get-dict base))
  (define new-map (string-map-set* d extension))
  (py-match base
    [(p-object _ _ f m) (p-object no-brands new-map f m)]
    [(p-fun _ _ f m) (p-fun no-brands new-map f m)]
    [(p-num _ _ f m n) (p-num no-brands new-map f m n)]
    [(p-str _ _ f m str) (p-str no-brands new-map f m str)]
    [(p-method _ _ f m) (p-method no-brands new-map f m)]
    [(p-bool _ _ f m t) (p-bool no-brands new-map f m t)]
    [(p-nothing _ _ _ _) (error "update: Cannot update nothing")]
    [(default _) (error (format "update: Cannot update ~a" base))]))

;; structural-list? : Value -> Boolean
(define (structural-list? v)
  (define d (get-dict v))
  (and (string-map-has-key? d "first")
       (string-map-has-key? d "rest")))

;; structural-list->list : Value -> Listof Value
(define (structural-list->list lst)
  (define d (get-dict lst))
  (cond
    [(structural-list? lst)
     (cons (string-map-ref d "first")
           (structural-list->list (string-map-ref d "rest")))]
    [else empty]))

;; mk-structural-list : ListOf Value -> Value
(define (mk-structural-list lst)
  (cond
    [(empty? lst) (mk-object (make-string-map
        `(("is-empty" . ,(mk-bool #t)))))]
    [(cons? lst) (mk-object (make-string-map
        `(("first" . ,(first lst))
          ("is-empty" . ,(mk-bool #f))
          ("rest" . ,(mk-structural-list (rest lst))))))]
    [else (error 'mk-structural-list (format "mk-structural-list got ~a" lst))]))

;; keys : Value -> Value
(define keys-pfun (pλ/internal (loc) (object)
  (mk-structural-list (map mk-str (string-map-keys (get-dict object))))))

;; TODO(joe): Quickify
(define num-keys-pfun (pλ/internal (loc) (object)
  (mk-num (string-map-count (get-dict object)))))

(define has-field-pfun (pλ/internal (loc) (object field)
  (cond
    [(p-str? field)
     (mk-bool (has-field? object (p-str-s field)))]
    [else
     (raise
      (pyret-error
        (get-top-loc)
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
   (make-string-map 
    `(("brand" .
       ,(mk-brander sym))
      ("test" .
       ,(mk-checker sym)))))))

(define (pyret-true? v)
  (or (eq? v p-true) (and (p-bool? v) (p-bool-b v))))

(define-syntax-rule (mk-prim-fun op opname wrapper (unwrapper ...) (arg ...) (pred ...))
  (mk-prim-fun-default op opname wrapper (unwrapper ...) (arg ...) (pred ...)
    (let ()
      (define args-strs (list (to-string arg) ...))
      (define args-str (string-join args-strs ", "))
      (define error-val (mk-str (format "Bad args to prim: ~a : ~a" opname args-str)))
      (raise (mk-pyret-exn (exn+loc->message error-val (get-top-loc)) (get-top-loc) error-val #f)))))

(define-syntax-rule (mk-prim-fun-default op opname wrapper (unwrapper ...) (arg ...) (pred ...) default)
  (pμ/internal (loc) (arg ...) ""
    (define preds-passed (and (pred arg) ...))
    (cond
      [preds-passed (wrapper (op (unwrapper arg) ...))]
      [else default])))

(define-syntax-rule (mk-lazy-prim op opname wrapper unwrapper (arg1 arg2 ...)
                                                              (pred1 pred2 ...))
  (pμ/internal (loc) (arg1 arg2 ...) ""
    (define (error)
      (define args-strs (list (to-string arg1) (to-string arg2) ...))
      (define args-str (string-join args-strs ", "))
      (define error-val (mk-str (format "Bad args to prim: ~a : ~a" opname args-str)))
      (raise (mk-pyret-exn (exn+loc->message error-val (get-top-loc)) (get-top-loc) error-val #f)))
    (define (check1 arg1) (if (pred1 arg1) arg1 (error)))
    (define (check2 arg2) (if (pred2 arg2) arg2 (error)))
    ...
    (wrapper (op (unwrapper (check1 arg1))
                 (unwrapper (check2 ((p-base-app arg2)))) ...))))

(define-syntax-rule (mk-num-1 op opname)
  (mk-prim-fun op opname mk-num (p-num-n) (n) (p-num?)))
(define-syntax-rule (mk-num-2 op opname)
  (mk-prim-fun op opname mk-num (p-num-n p-num-n) (n1 n2) (p-num? p-num?)))
(define-syntax-rule (mk-num-2-bool op opname)
  (mk-prim-fun op opname mk-bool (p-num-n p-num-n) (n1 n2) (p-num? p-num?)))

;; meta-num-store (Hashof numing value)
(define meta-num-store #f)
(define (meta-num)
  (when (not meta-num-store)
    (set! meta-num-store
      (make-string-map
        `(("_plus" . ,(mk-num-2 + 'plus))
          ("_add" . ,(mk-num-2 + 'plus))
          ("_minus" . ,(mk-num-2 - 'minus))
          ("_divide" . ,(mk-num-2 / 'divide))
          ("_times" . ,(mk-num-2 * 'times))
          ("sin" . ,(mk-num-1 sin 'sin))
          ("cos" . ,(mk-num-1 cos 'cos))
          ("sqr" . ,(mk-num-1 sqr 'sqr))
          ("sqrt" . ,(mk-num-1 sqrt 'sqrt))
          ("floor" . ,(mk-num-1 floor 'floor))
          ("tostring" . ,(mk-prim-fun number->string 'tostring mk-str (p-num-n) (n) (p-num?)))
          ("expt" . ,(mk-num-2 expt 'expt))
          ("_equals" . ,(mk-prim-fun-default = 'equals mk-bool (p-num-n p-num-n) (n1 n2) (p-num? p-num?) (mk-bool #f)))
          ("_lessthan" . ,(mk-num-2-bool < 'lessthan))
          ("_greaterthan" . ,(mk-num-2-bool > 'greaterthan))
          ("_lessequal" . ,(mk-num-2-bool <= 'lessequal))
          ("_greaterequal" . ,(mk-num-2-bool >= 'greaterequal))))))
  meta-num-store)

;; Pyret's char-at just returns a single character string
(define (char-at s n)
  (substring s n (+ n 1)))

(define (string-repeat s n)
  (cond
   [(= n 0) ""]
   [else (string-append s (string-repeat s (- n 1)))]))

;; meta-str-store (Hashof String value)
(define meta-str-store #f)
(define (meta-str)
  (when (not meta-str-store)
    (set! meta-str-store
      (make-string-map
        `(("append" . ,(mk-prim-fun string-append 'append mk-str (p-str-s p-str-s) (s1 s2) (p-str? p-str?)))
          ("_plus" . ,(mk-prim-fun string-append 'plus mk-str (p-str-s p-str-s) (s1 s2) (p-str? p-str?)))
          ("contains" . ,(mk-prim-fun string-contains 'contains mk-bool (p-str-s p-str-s) (s1 s2) (p-str? p-str?)))
          ("substring" . ,(mk-prim-fun substring 'substring mk-str (p-str-s p-num-n p-num-n) (s n1 n2) (p-str? p-num? p-num?)))
          ("char-at" . ,(mk-prim-fun char-at 'char-at mk-str (p-str-s p-num-n) (s n) (p-str? p-num?)))
          ("repeat" . ,(mk-prim-fun string-repeat 'repeat mk-str (p-str-s p-num-n) (s n) (p-str? p-num?)))
          ("length" . ,(mk-prim-fun string-length 'length mk-num (p-str-s) (s) (p-str?)))
          ("tonumber" . ,(mk-prim-fun string->number 'tonumber mk-num (p-str-s) (s) (p-str?)))
          ("_lessequals" . ,(mk-prim-fun string<=? 'lessequals mk-bool (p-str-s p-str-s) (s1 s2) (p-str? p-str?)))
          ("_lessthan" . ,(mk-prim-fun string<? 'lessthan mk-bool (p-str-s p-str-s) (s1 s2) (p-str? p-str?)))
          ("_greaterthan" . ,(mk-prim-fun string>? 'greaterthan mk-bool (p-str-s p-str-s) (s1 s2) (p-str? p-str?)))
          ("_greaterequals" . ,(mk-prim-fun string>=? 'greaterequals mk-bool (p-str-s p-str-s) (s1 s2) (p-str? p-str?)))
          ("_equals" . ,(mk-prim-fun-default string=? 'equals mk-bool (p-str-s p-str-s) (s1 s2) (p-str? p-str?) (mk-bool #f)))
      ))))
  meta-str-store)

(define-syntax-rule (mk-bool-1 op opname)
  (mk-prim-fun op opname mk-bool (p-bool-b) (b) (p-bool?)))
(define-syntax-rule (mk-bool-2 op opname)
  (mk-prim-fun op opname mk-bool (p-bool-b p-bool-b) (b1 b2) (p-bool? p-bool?)))
(define-syntax-rule (mk-lazy-bool-2 op opname)
  (mk-lazy-prim op opname mk-bool p-bool-b (b1 b2) (p-bool? p-bool?)))

(define (bool->string b) (if b "true" "false"))

;; meta-bool-store (Hashof String value)
(define meta-bool-store #f)
(define (meta-bool)
  (when (not meta-bool-store)
    (set! meta-bool-store
      (make-string-map
       `(("_and" . ,(mk-lazy-bool-2 and 'and))
         ("_or" . ,(mk-lazy-bool-2 or 'or))
         ("tostring" . ,(mk-prim-fun bool->string 'tostring mk-str (p-bool-b) (b) (p-bool?)))
         ("_equals" . ,(mk-prim-fun-default equal? 'equals mk-bool (p-bool-b p-bool-b) (b1 b2) (p-bool? p-bool?) (mk-bool #f)))
         ("_not" . ,(mk-bool-1 not 'not))))))
  meta-bool-store)

;; to-string : Value -> String
(define (to-string v)
  (define (call-tostring v fallback)
    (if (has-field? v "tostring")
        (let [(m (get-raw-field dummy-loc v "tostring"))]
          (if (p-method? m)
              ;; NOTE(dbp): this will fail if tostring isn't defined
              ;; as taking only self.
              (py-match ((p-base-method m) v)
                        [(p-str _ _ _ _ s) s]
                        [(default _) (fallback)])
              (fallback)))
        (fallback)))
  (py-match v
    [(p-nothing _ _ _ _) "nothing"]
    [(p-num _ _ _ _ n) (format "~a" n)]
    [(p-str _ _ _ _ s) (format "~a" s)]
    [(p-bool _ _ _ _ b) (if b "true" "false")]
    [(p-method _ _ _ _) (call-tostring v (λ () "[[code]]"))]
    [(p-fun _ _ _ _) (call-tostring v (λ () "[[code]]"))]
    [(p-object _ h _ _)
     (let ()
       (define (to-string-raw-object h)
         (define (field-to-string f v)
           (format "~a: ~a" f (to-string v)))
         (format "{ ~a }"
                 (string-join (string-map-map h field-to-string) ", ")))
       (call-tostring
        v
        (λ () (to-string-raw-object h))))]
    [(default _) (format "~a" v)]))

(define tostring-pfun (pλ/internal (loc) (o)
  (mk-str (to-string o))))

(define print-pfun (pλ/internal (loc) (o)
  (begin (printf "~a\n" (to-string o)) nothing)))

;; check-brand-pfun : Loc -> Value * -> Value
(define check-brand-pfun (pλ/internal (loc) (ck o s)
  (cond
    [(p-str? s)
     (define f (p-base-app ck))
     (define check-v (f o))
     (if (pyret-true? check-v)
         o
         ;; NOTE(dbp): not sure how to give good reporting
         ;; NOTE(joe): This is better, but still would be nice to highlight
         ;;  the call site as well as the destination
         (let*
          ([typname (p-str-s s)]
           [val (mk-str (format "runtime: typecheck failed; expected ~a and got\n~a"
                              typname (to-string o)))])
         (raise (mk-pyret-exn (exn+loc->message val (get-top-loc)) (get-top-loc) val #f))))]
    [(p-str? s)
     (error "runtime: cannot check-brand with non-function")]
    [(p-fun? ck)
     (error "runtime: cannot check-brand with non-string")]
    [else
     (error "runtime: check-brand failed")])))

;; exn+loc->message : Value Loc -> String
(define (exn+loc->message v l)
  (format
    "~a:~a:~a: Uncaught exception ~a\n"
    (first l)
    (second l)
    (third l)
    (to-string v)))

(define raise-pfun (pλ/internal (loc) (o)
  (raise (mk-pyret-exn (exn+loc->message o (get-top-loc)) (get-top-loc) o #f))))

(define is-nothing-pfun (pλ/internal (loc) (specimen)
  (mk-bool (equal? specimen nothing))))

;; tie the knot of mutual state problems
(void
  (meta-num)
  (meta-bool)
  (meta-str))

(define p-true (p-bool no-brands meta-bool-store (bad-app "true") (bad-meth "true") #t))
(define p-false (p-bool no-brands meta-bool-store (bad-app "false") (bad-meth "false") #f))
;; mk-bool : Boolean -> Value
(define (mk-bool b)
  (if b p-true p-false))
(define p-else p-true)
(define p-pi (mk-num pi))

(define Any (pλ/internal (loc) (_) p-true))

(define-syntax-rule (mk-pred name test)
  (define name
    (pλ (arg)
      (format "Built-in predicate for ~a" 'name)
      (mk-bool (test arg)))))

(mk-pred Number p-num?)
(mk-pred String p-str?)
(mk-pred Bool p-bool?)
(mk-pred Object p-object?)
(mk-pred Function p-fun?)
(mk-pred Method p-method?)

