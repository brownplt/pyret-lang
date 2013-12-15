#lang whalesong

;(require racket/set ;; set add union member intersect map)
(require (for-syntax racket/base))
(require (only-in racket/string string-replace))
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

;; Adapted from https://en.wikipedia.org/wiki/Knuth%E2%80%93Morris%E2%80%93Pratt_algorithm
(define (string-index haystack needle)
  (define nlen (string-length needle))
  (define hlen (string-length haystack))
  (define (fold-min-max f init min max)
    (define (help i acc)
      (cond
       [(>= i max) acc]
       [else (help (+ 1 i) (f i acc))]))
    (help min init))
  (define (make-table)
    (define kmp-table (make-vector nlen))
    (vector-set! kmp-table 0 -1)
    (define (help pos cnd)
      (cond
       [(>= pos nlen) #f]
       [(char=? (string-ref needle (- pos 1)) (string-ref needle cnd))
        (vector-set! kmp-table pos (+ 1 cnd))
        (+ 1 cnd)]
       [(> cnd 0) (vector-ref kmp-table cnd)]
       [else
        (vector-set! kmp-table pos 0)
        cnd]))
    (fold-min-max help 0 2 nlen)
    kmp-table)
  (define (search)
    (define T (make-table))
    (define (help m i)
      (if (>= (+ m i) hlen)
          #f
          (if (char=? (string-ref needle i) (string-ref haystack (+ m i)))
              (if (= i (- nlen 1))
                  m
                  (help m (+ 1 i)))
              (let ((Ti (vector-ref T i)))
                (help (- (+ m i) Ti)
                      (if (> Ti -1) Ti 0))))))
    (help 0 0))
  (cond
    [(string=? needle "") 0]
    [else (search)]))
    
(define (string-contains str substr)
  (not (not (string-index str substr))))

(define (string-split s sep repeated)
  (define sep-len (string-length sep))
  (define (help s)
    (define i (string-index s sep))
    (cond
     [i
      (let [(front (mk-str (substring s 0 i)))
            (back (substring s (+ i sep-len) (string-length s)))]
        (if repeated
            (cons front (help back))
            (list front (mk-str back))))]
     [else (list (mk-str s))]))
  (cond
   [(string=? sep "") (map (λ(c) (mk-str (string c))) (string->list s))]
   [else (help s)]))

(define (string-explode s)
  (map (λ(c) (mk-str (string c))) (string->list s)))

(define (interleave sep lst acc)
  (cond
   [(empty? lst) (reverse (rest acc))]
   [else (interleave sep (rest lst) (cons sep (cons (first lst) acc)))]))
(define (string-join strs sep)
  (if (string=? sep "")
      (apply string-append strs)
      ;; Better to allocate lists and string-append once than to repeatedly allocate strings
      (apply string-append (interleave sep strs (list "")))))

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
      arity-catcher-loc
      pλ
      pλ/loc
      mk-fun
      mk-fun-nodoc
      mk-fun-nodoc-slow
      pμ
      pμ/loc
      mk-method
      mk-exn
      pyret-error
      empty-dict
      get-dict
      get-field
      get-raw-field
      get-mutable-field
      apply-fun
      arity-error
      check-str
      has-field?
      extend
      update
      to-string
      to-repr
      nothing
      pyret-true?
      dummy-loc
      loc-list))
  (rename-out [p-pi pi]
              [print-pfun print]
              [tostring-pfun tostring]
              [torepr-pfun torepr]
              [brander-pfun brander]
              [check-brand-pfun check-brand]
              [keys-pfun prim-keys]
              [num-keys-pfun prim-num-keys]
              [has-field-pfun prim-has-field]
              [raise-pfun raise]
              [mk-mutable-pfun mk-mutable]
              [mk-simple-mutable-pfun mk-simple-mutable]
              [mk-placeholder-pfun mk-placeholder]
              [mk-array-of-pfun array-of]
              [mk-build-array-pfun build-array]
              [gensym-pfun gensym]
              [p-else else])
  Any
  Number
  String
  Bool
  Boolean
  Object
  Nothing
  Function
  Method
  Mutable
  Placeholder
  Array
  Opaque
  is-number
  is-string
  is-bool
  is-boolean
  is-object
  is-nothing
  is-function
  is-method
  is-mutable
  is-placeholder
  is-array
  ___set-link
  ___set-empty
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
;; p-mutable p-base Box (Listof (Value -> Value)) (Listof (Value -> Value))
(struct p-mutable p-base (b read-wrappers write-wrappers))
;; p-placeholder p-base Box (Listof (Value -> Value))
(struct p-placeholder p-base (b wrappers) #:mutable)
;; p-array p-base Array
(struct p-array p-base (v) #:mutable)
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
    [(eq? p-mutable typ) p-mutable?]
    [(eq? p-placeholder typ) p-placeholder?]
    [(eq? p-array typ) p-array?]
    [(eq? p-base typ) p-base?]
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
  (syntax-case stx (p-nothing p-object p-num p-str p-bool p-fun p-method p-array)
    [(_ p-nothing _ () body) #'body]
    [(_ p-object _ () body) #'body]
    [(_ p-base _ () body) #'body]
    [(_ p-fun _ () body) #'body]
    [(_ p-method _ () body) #'body]
    [(_ p-mutable matchval (b rw ww) body)
     (with-syntax [(b-id (datum->syntax #'body (syntax->datum #'b)))
                   (rw-id (datum->syntax #'body (syntax->datum #'rw)))
                   (ww-id (datum->syntax #'body (syntax->datum #'ww)))]
       #'(maybe-bind [(b-id (p-mutable-b matchval))
                      (rw-id (p-mutable-read-wrappers matchval))
                      (ww-id (p-mutable-write-wrappers matchval))]
           body))]
    [(_ p-mutable matchval (b w) body)
     (with-syntax [(b-id (datum->syntax #'body (syntax->datum #'b)))
                   (w-id (datum->syntax #'body (syntax->datum #'w)))]
       #'(maybe-bind [(b-id (p-placeholder-b matchval))
                      (w-id (p-placeholder-wrappers matchval))]
           body))]
    [(_ p-num matchval (n) body)
     (with-syntax [(n-id (datum->syntax #'body (syntax->datum #'n)))]
      #'(maybe-bind [(n-id (p-num-n matchval))] body))]
    [(_ p-str matchval (s) body)
     (with-syntax [(s-id (datum->syntax #'body (syntax->datum #'s)))]
      #'(maybe-bind [(s (p-str-s matchval))] body))]
    [(_ p-bool matchval (b) body)
     (with-syntax [(b-id (datum->syntax #'body (syntax->datum #'b)))]
      #'(maybe-bind [(b-id (p-bool-b matchval))] body))]
    [(_ p-array matchval (v) body)
     (with-syntax [(v-id (datum->syntax #'body (syntax->datum #'v)))]
      #'(maybe-bind [(v-id (p-array-v matchval))]
          body))]))

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

(define-syntax-rule (arity-catcher-loc (arg ...) e ... loc)
  (case-lambda
    [(arg ...) e ...]
    [arity-mismatch-args-list
     (arity-error loc (quote (arg ...)) arity-mismatch-args-list)]))

(define-syntax-rule (lambda-arity-catcher (arg ...) e ...)
  (case-lambda
    [(arg ...) e ...]
    [arity-mismatch-args-list
     (arity-error (get-top-loc) (rest (quote (arg ...))) (rest arity-mismatch-args-list))]))
(define-syntax-rule (lambda-arity-catcher-loc (arg ...) e ... loc)
  (case-lambda
    [(arg ...) e ...]
    [arity-mismatch-args-list
     (arity-error loc (rest (quote (arg ...))) (rest arity-mismatch-args-list))]))


;; NOTE(joe): the nested syntax/loc below appears necessary to get good
;; profiling and debugging line numbers for the created functions
(define-syntax (pλ stx)
  (syntax-case stx ()
    [(_ (arg ...) doc e ...)
     (quasisyntax/loc stx
      (mk-fun
        #,(syntax/loc stx (arity-catcher (arg ...) e ...))
        #,(syntax/loc stx (lambda-arity-catcher (_ arg ...) e ...))
        doc))]))

(define-syntax (pλ/loc stx)
  (syntax-case stx ()
    [(_ (arg ...) doc e ... loc)
     (quasisyntax/loc stx
      (mk-fun
        #,(syntax/loc stx (arity-catcher-loc (arg ...) e ... loc))
        #,(syntax/loc stx (lambda-arity-catcher-loc (_ arg ...) e ... loc))
        doc))]))

(define-syntax (pλ/internal stx)
  (syntax-case stx ()
    [(_ (loc) (arg ...) e ...)
     (quasisyntax/loc stx
      (mk-fun-nodoc
        #,(syntax/loc stx (arity-catcher (arg ...) e ...))
        #,(syntax/loc stx (lambda-arity-catcher (_ arg ...) e ...))))]))

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

(define-syntax (pμ/loc stx)
  (syntax-case stx ()
    [(_ (arg ...) doc e ... loc)
     (quasisyntax/loc stx
      (mk-method
        (case-lambda
          #,(syntax/loc stx [(arg ...) e ...])
          [arity-mismatch-args-list
           (arity-method-error loc '(arg ...) arity-mismatch-args-list)])
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
  (define (mk-loc l)
    (define maybe-path (srcloc-source l))
    (define maybe-line (srcloc-line l))
    (define maybe-col (srcloc-column l))
    (define path (cond
      [(string? maybe-path) maybe-path]
      ;; TODO(joe): removed for WS
      ;[(path? maybe-path) (path->string maybe-path)]
      [else "unnamed-pyret-file"]))
    (define line (if maybe-line maybe-line -1))
    (define column (if maybe-col maybe-col -1))
    (list
      (cons "path" (mk-str path))
      (cons "line" (mk-num line))
      (cons "column" (mk-num column))))
  (define trace-locs
    (cons loc (continuation-mark-set->list (exn-continuation-marks e) 'pyret-mark)))
  (define trace
    (mk-list
      (map (lambda (l) (mk-object (make-string-map (mk-loc l))))
      trace-locs)))
  (mk-object
   (make-string-map
    (append
      (mk-loc loc)
      (list
        (cons "value" (exn:fail:pyret-val e))
	      (cons "system" (mk-bool (exn:fail:pyret-system? e)))
        (cons "trace" trace))))))

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

(define (_mk-fun f m d to-m)
  (p-fun no-brands (make-string-map `(("_doc" . ,d)
                                      ("_method" . ,to-m)))
         f
         m))

;; mk-fun : (Value ... -> Value) String -> Value
(define (mk-fun f m s)
  (_mk-fun f m (mk-str s) (mk-method-method f s)))

;; mk-fun-nodoc : (Value ... -> Value) -> Value
(define (mk-fun-nodoc f m)
  (_mk-fun f m nothing (mk-method-method-nodoc f)))

(define (mk-fun-nodoc-slow f)
  (_mk-fun f (lambda (_ . args) (apply f args)) nothing nothing))

(define method-bad-app (bad-app "method"))

(define (_mk-method m doc to-f)
  (p-method no-brands
            (make-string-map
              `(("_doc" . ,doc)
                ("_fun" . ,to-f)))
            method-bad-app
            m))

;; mk-method-method : (Value ... -> Value) String -> p-method
(define (mk-method-method f doc)
  (_mk-method (λ (self) (mk-method f doc)) (mk-str doc) nothing))

;; mk-method-method-nodoc : (Value ... -> Value) -> p-method
(define (mk-method-method-nodoc f)
  (_mk-method (λ (self) (mk-method f nothing)) nothing nothing))

;; mk-fun-method : (Value ... -> Value) String -> p-method
(define (mk-fun-method f doc)
  (_mk-method
    (λ (self) (mk-fun f (lambda args (apply f (rest args))) doc))
    (mk-str doc)
    nothing))

;; mk-method : (Value ... -> Value) String -> Value
(define (mk-method f doc)
  (_mk-method f (mk-str doc) (mk-fun-method f doc)))

;; mk-method-nodoc : Proc -> Value
(define (mk-method-nodoc f)
  (_mk-method f nothing (mk-fun-method f "")))

(define mutable-bad-app (bad-app "mutable"))
(define mutable-bad-meth (bad-meth "mutable"))
(define (mk-mutable v reads writes)
  (p-mutable no-brands mutable-dict mutable-bad-app mutable-bad-meth (box v) reads writes))

(define mk-mutable-pfun (pλ/internal (loc) (val read write)
  (define check (p-base-app check-brand-pfun))
  (define checked-read (check Function read (mk-str "Function")))
  (define checked-write (check Function write (mk-str "Function")))
  (mk-mutable val (list (p-base-app checked-read)) (list (p-base-app checked-write)))))

(define mk-simple-mutable-pfun (pλ/internal (loc) (val)
  (mk-mutable val (list) (list))))

(define placeholder-bad-app (bad-app "placeholder"))
(define placeholder-bad-meth (bad-meth "placeholder"))
(define (mk-placeholder)
  (p-placeholder no-brands placeholder-dict placeholder-bad-app placeholder-bad-meth (box #f) empty))

(define mk-placeholder-pfun (pλ/internal (loc) ()
  (mk-placeholder)))

(define array-bad-app (bad-app "array"))
(define array-bad-meth (bad-meth "array"))
(define mk-array-of-pfun (pλ/internal (loc) (val num)
  (when (not (p-num? num)) (throw-type-error! "Number" num))
  (p-array no-brands array-dict array-bad-app array-bad-meth (make-vector (p-num-n num) val))))
(define mk-build-array-pfun (pλ/internal (loc) (fun len)
  (when (not (p-num? len)) (throw-type-error! "Number" len))
  (p-array no-brands array-dict array-bad-app array-bad-meth
           (build-vector (p-num-n len) (λ(i) (apply-fun fun dummy-loc (mk-num i)))))))

(define exn-brand (gensym 'exn))

;; pyret-error : Loc String String -> p-exn
(define (pyret-error loc type message)
  (define full-error (exn+loc->message (mk-str message) loc))
  (define obj (mk-object (make-string-map
    (list (cons "message" (mk-str message))
          (cons "type" (mk-str type))))))
  (mk-pyret-exn full-error loc obj #t))

;; get-raw-field : Loc Value String -> Value
(define in-get-raw-field empty)
(define (get-raw-field loc v f)
  (string-map-ref (get-dict v) f
   (lambda()
     ;; (printf "get-raw-field failed for ~a and ~a\n" (raw-repr v) f)
     (if (empty? in-get-raw-field)
         (begin
           (set! in-get-raw-field (cons f in-get-raw-field))
           (with-handlers
               ([(λ(e) (not (empty? in-get-raw-field)))
                 (λ(e)
                   ;; (printf "In-g-r-f, in-g-r-f isn't empty and to-repr failed,\ne = ~s\nin-g-r-f = ~a\nf = ~a" e in-get-raw-field f)
                   (let* ((outer-f (first in-get-raw-field))
                          (e-loc (exn:fail:pyret-srcloc e))
                          (e-loc-list (if (srcloc? e-loc) (loc-list e-loc) e-loc))
                          (e-val (exn:fail:pyret-val e))
                          (e-msg (if (p-str? e-val) (p-str-s e-val)
                                     (p-str-s (get-raw-field loc e-val "message"))))
                          (msg (format "While reporting that ~a was not found in ~a, to-repr raised an error: ~a"
                                       outer-f (raw-repr v) e-msg)))
                     (set! in-get-raw-field (rest in-get-raw-field))
                     (with-continuation-mark
                      'pyret-mark (apply srcloc loc)
                      (raise (pyret-error e-loc-list "field-not-found" msg)))))])
             ;; (printf "In-g-r-f, in-g-r-f is empty, field ~a wasn't found on ~a, trying to-repr\n" f (raw-repr v))
             (let ((v-repr (to-repr v)))
               (set! in-get-raw-field (rest in-get-raw-field))
               ;; (printf "In-g-r-f, to-repr succeeded: ~a" v-repr)
               (raise (pyret-error loc "field-not-found" (format "~a was not found on ~a" f v-repr))))))
         (begin
           ;; (printf "In-g-r-f, field ~a wasn't found, in-g-r-f isn't empty: ~a\n" f in-get-raw-field)
           (raise (pyret-error loc "field-not-found" (format "~a was not found on ~a" f (raw-repr v))))))
     )))

;; get-field : Loc Value String -> Value
(define (get-field loc v f)
 (define vfield (get-raw-field loc v f))
 (cond
   [(p-mutable? vfield)
    (raise (pyret-error loc "lookup-mutable" (format "Cannot look up mutable field \"~a\" using dot or bracket" f)))]
   [(p-placeholder? vfield)
    (get-placeholder-value loc vfield)]
   [(p-method? vfield)
    (let [(curried
           (mk-fun (λ args (apply (p-base-method vfield)
                                  (cons v args)))
                   (λ args (apply (p-base-method vfield)
                                  (cons v (rest args))))
                   ;; NOTE(dbp 2013-08-09): If _doc isn't a string, this will blow up...
                   ""))]
          curried)]
   [else vfield]))

;; get-mutable-field : Loc Value String -> Value
(define (get-mutable-field loc v f)
  (define vfield (get-raw-field loc v f))
  (cond
    [(p-mutable? vfield)
     (define checks (p-mutable-read-wrappers vfield))
     (foldr (lambda (c v) (c v)) (unbox (p-mutable-b vfield)) checks)]
    [else (get-field loc v f)]))

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
          (string-join (map to-repr (drop args 1)) "\n")
          (to-string (first args)))))]))

(define (arity-error loc argnames args)
  (define (pluralize str lst)
    (string-append str (if (= (length lst) 1) "" "s")))
  (define expected-names (pluralize "argument" argnames))
  (define expected-args (pluralize "argument" args))
  (define were/was (if (= (length args) 1) "was" "were"))
  (cond
    [(= (length args) 0)
     (raise
        (pyret-error
          loc
          "arity-mismatch"
          (format
"Expected ~a ~a, but got none."
            (length argnames)
            expected-names)))]
    [else
     (raise
       (pyret-error
         loc
         "arity-mismatch"
         (format
"Expected ~a ~a, but got ~a.  The ~a provided ~a ~a:
~a"
           (length argnames)
           expected-names
           (length args)
           (length args)
           expected-args
           were/was
           (string-join (map to-repr args) "\n"))))]))

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

(define (update loc base extension)
  (define d (get-dict base))
  (void (map
    (lambda (k)
      (define (not-found-error)
        (raise (pyret-error loc "field-not-found" (format "Updating non-existent field ~a" k))))
      (when (not (p-mutable? (string-map-ref d k not-found-error)))
        (raise (pyret-error loc "update-immutable" (format "Updating immutable field (~a) disallowed" k)))))
    (map car extension)))
  (void (map
    (lambda (pair)
      (define mutable (string-map-ref d (car pair)))
      (define checks (p-mutable-write-wrappers mutable))
      (define value (foldr (lambda (c v) (c v)) (cdr pair) checks))
      (set-box! (p-mutable-b mutable) (cdr pair)))
    extension))
  nothing)

;; extend : Loc Value Dict -> Value
(define (extend loc base extension)
  (define d (get-dict base))
  (define new-map (string-map-set* d extension))
  (define loses-brands?
    (ormap (lambda (k) (string-map-has-key? d k)) (map car extension)))
  (define new-brands (if loses-brands? no-brands (get-brands base)))
  (py-match base
    [(p-object _ _ f m) (p-object new-brands new-map f m)]
    [(p-fun _ _ f m) (p-fun new-brands new-map f m)]
    [(p-num _ _ f m n) (p-num new-brands new-map f m n)]
    [(p-str _ _ f m str) (p-str new-brands new-map f m str)]
    [(p-method _ _ f m) (p-method new-brands new-map f m)]
    [(p-bool _ _ f m t) (p-bool new-brands new-map f m t)]
    [(p-nothing _ _ _ _) (error "update: Cannot update nothing")]
    [(default _) (error (format "update: Cannot update ~a" base))]))

;; keys : Value -> Value
(define keys-pfun (pλ/internal (loc) (object)
  (mk-list (map mk-str (string-map-keys (get-dict object))))))

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

(define (protect-div n1 n2)
  (cond
    [(= n2 0) (raise (pyret-error (get-top-loc) "div-0" "Division by zero"))]
    [else (/ n1 n2)]))

;; meta-num-store (Hashof numing value)
(define meta-num-store #f)
(define (meta-num)
  (when (not meta-num-store)
    (set! meta-num-store
      (make-string-map
        `(("_plus" . ,(mk-num-2 + 'plus))
          ("_add" . ,(mk-num-2 + 'plus))
          ("_minus" . ,(mk-num-2 - 'minus))
          ("_divide" . ,(mk-num-2 protect-div 'divide))
          ("_times" . ,(mk-num-2 * 'times))
          ("_torepr" . ,(mk-prim-fun number->string '_torepr mk-str (p-num-n) (n) (p-num?)))
          ("_equals" . ,(mk-prim-fun-default = 'equals mk-bool (p-num-n p-num-n) (n1 n2) (p-num? p-num?) (mk-bool #f)))
          ("_lessthan" . ,(mk-num-2-bool < 'lessthan))
          ("_greaterthan" . ,(mk-num-2-bool > 'greaterthan))
          ("_lessequal" . ,(mk-num-2-bool <= 'lessequal))
          ("_greaterequal" . ,(mk-num-2-bool >= 'greaterequal))
          ("tostring" . ,(mk-prim-fun number->string 'tostring mk-str (p-num-n) (n) (p-num?)))
          ("tostring-fixed" . ,(mk-prim-fun real->decimal-string 'tostring-fixed mk-str (p-num-n p-num-n) (n places) (p-num? p-num?)))
          ("modulo" . ,(mk-num-2 modulo 'modulo))
          ("truncate" . ,(mk-num-1 truncate 'truncate))
          ("abs" . ,(mk-num-1 abs 'abs))
          ("max" . ,(mk-num-2 max 'max))
          ("min" . ,(mk-num-2 min 'min))
          ("sin" . ,(mk-num-1 sin 'sin))
          ("cos" . ,(mk-num-1 cos 'cos))
          ("tan" . ,(mk-num-1 tan 'tan))
          ("asin" . ,(mk-num-1 asin 'asin))
          ("acos" . ,(mk-num-1 acos 'acos))
          ("atan" . ,(mk-num-1 atan 'atan))
          ("sqr" . ,(mk-num-1 sqr 'sqr))
          ("sqrt" . ,(mk-num-1 sqrt 'sqrt))
          ("ceiling" . ,(mk-num-1 ceiling 'ceiling))
          ("floor" . ,(mk-num-1 floor 'floor))
          ("log" . ,(mk-num-1 log 'log))
          ("exp" . ,(mk-num-1 exp 'exp))
          ("exact" . ,(mk-num-1 inexact->exact 'exact))
          ("is-integer" . ,(mk-prim-fun integer? 'is-integer mk-bool (p-num-n) (n) (p-num?)))
          ("expt" . ,(mk-num-2 expt 'expt))))))
  meta-num-store)

;; Pyret's char-at just returns a single character string
(define (char-at s n)
  (cond
    [(>= n (string-length s))
     (raise (pyret-error (get-top-loc) "char-at"
      (format "char-at: Index too large for string.  Index was ~a, length was ~a" n (string-length s))))]
    [else (substring s n (+ n 1))]))

(define (safe-substring s start end)
  (define (err message)
     (raise (pyret-error (get-top-loc) "substring" message)))
  (define l (string-length s))
  (cond
    [(< start 0) (err (format "substring: Requires a non-negative start value; ~a was provided" start))]
    [(> start l) (err (format "substring:  Start index is past the length of the string: start was ~a, length was ~a" start l))]
    [(< end start) (err (format "substring: Requires end to be greater than start, got start of ~a and end of ~a" start end))]
    [else (substring s start (min l end))]))

(define (string-repeat s n)
  (cond
   [(= n 0) ""]
   [else (string-append s (string-repeat s (- n 1)))]))

;; meta-str-store (Hashof String value)
(define (mk-num-or-nothing v)
  (if v (mk-num v) nothing))
(define meta-str-store #f)
(define (meta-str)
  (when (not meta-str-store)
    (set! meta-str-store
      (make-string-map
         `(("_plus" . ,(mk-prim-fun string-append 'plus mk-str (p-str-s p-str-s) (s1 s2) (p-str? p-str?)))
          ("_lessequal" . ,(mk-prim-fun string<=? 'lessequals mk-bool (p-str-s p-str-s) (s1 s2) (p-str? p-str?)))
          ("_lessthan" . ,(mk-prim-fun string<? 'lessthan mk-bool (p-str-s p-str-s) (s1 s2) (p-str? p-str?)))
          ("_greaterthan" . ,(mk-prim-fun string>? 'greaterthan mk-bool (p-str-s p-str-s) (s1 s2) (p-str? p-str?)))
          ("_greaterequal" . ,(mk-prim-fun string>=? 'greaterequals mk-bool (p-str-s p-str-s) (s1 s2) (p-str? p-str?)))
          ("_equals" . ,(mk-prim-fun-default string=? 'equals mk-bool (p-str-s p-str-s) (s1 s2) (p-str? p-str?) (mk-bool #f)))
          ("append" . ,(mk-prim-fun string-append 'append mk-str (p-str-s p-str-s) (s1 s2) (p-str? p-str?)))
          ("contains" . ,(mk-prim-fun string-contains 'contains mk-bool (p-str-s p-str-s) (s1 s2) (p-str? p-str?)))
          ("replace" . ,(mk-prim-fun string-replace 'replace mk-str (p-str-s p-str-s p-str-s) (s1 s2 s3) (p-str? p-str? p-str?)))
          ("index-of" . ,(mk-prim-fun string-index 'index-of mk-num-or-nothing (p-str-s p-str-s) (s1 s2) (p-str? p-str?)))
          ("substring" . ,(mk-prim-fun safe-substring 'substring mk-str (p-str-s p-num-n p-num-n) (s n1 n2) (p-str? p-num? p-num?)))
          ("split" . ,(mk-prim-fun string-split 'string-split mk-list (p-str-s p-str-s p-bool-b) (s sep b) (p-str? p-str? p-bool?)))
          ("explode" . ,(mk-prim-fun string-explode 'explode mk-list (p-str-s) (s) (p-str?)))
          ("char-at" . ,(mk-prim-fun char-at 'char-at mk-str (p-str-s p-num-n) (s n) (p-str? p-num?)))
          ("repeat" . ,(mk-prim-fun string-repeat 'repeat mk-str (p-str-s p-num-n) (s n) (p-str? p-num?)))
          ("length" . ,(mk-prim-fun string-length 'length mk-num (p-str-s) (s) (p-str?)))
          ("to-lower" . ,(mk-prim-fun string-downcase 'string-downcase mk-str (p-str-s) (s) (p-str?)))
          ("to-upper" . ,(mk-prim-fun string-upcase 'string-upcase mk-str (p-str-s) (s) (p-str?)))
          ("tonumber" . ,(mk-prim-fun string->number 'tonumber mk-num-or-nothing (p-str-s) (s) (p-str?)))
          ("tostring" . ,(mk-prim-fun (lambda (x) x) 'tostring mk-str (p-str-s) (s) (p-str?)))
          ("_torepr" . ,(mk-prim-fun (lambda (x) (format "~s" x)) '_torepr mk-str (p-str-s) (s) (p-str?)))
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
         ("_torepr" . ,(mk-prim-fun bool->string '_torepr mk-str (p-bool-b) (b) (p-bool?)))
         ("_equals" . ,(mk-prim-fun-default equal? 'equals mk-bool (p-bool-b p-bool-b) (b1 b2) (p-bool? p-bool?) (mk-bool #f)))
         ("_not" . ,(mk-bool-1 not 'not))))))
  meta-bool-store)

;; serialize : Value String (method name) -> String
(define (serialize v method override)
  (define (serialize-internal v fallback)
    (if (and (has-field? v method) (not override))
        (let [(m (get-raw-field dummy-loc v method))]
          (if (p-method? m)
              ;; NOTE(dbp): this will fail if tostring isn't defined
              ;; as taking only self.
              (py-match ((p-base-method m) v)
                        [(p-str _ _ _ _ s) s]
                        [(default _) (fallback)])
              (if (p-fun? m)
                  ;; NOTE(dbp 2013-08-09): This will fail if tostring takes arguments
                  (py-match ((p-base-app m))
                            [(p-str _ _ _ _ s) s]
                            [(default _) (fallback)])
                  (fallback))))
        (fallback)))
  (define (type-sanity-check pred typename val otherwise)
    (if (not (pred val))
        (raise (format "INTERNAL ERROR: Got a non-~a inside a pyret ~a: ~a." typename typename val))
        otherwise))
  (py-match v
    [(p-nothing _ _ _ _) "nothing"]
    [(p-method _ _ _ _) (serialize-internal v (λ () "method(): end"))]
    [(p-fun _ _ _ _) (serialize-internal v (λ () "fun(): end"))]
    [(p-mutable _ _ _ _ _ _ _) (serialize-internal v (λ () "mutable-field"))]
    [(p-placeholder _ _ _ _ _ _) (serialize-internal v (λ () "cyclic-field"))]
    [(p-str _ _ _ _ s) (if (string=? method "tostring") s (format "~s" s))]
    [(p-num _ _ _ _ n) (number->string n)]
    [(p-base _ h _ _)
     (let ()
       (define (serialize-raw-object h)
         (define (serialize-field f v)
           (format "~a: ~a" f (serialize v method override)))
         (format "{~a}"
                 (string-join (string-map-map h serialize-field) ", ")))
       (serialize-internal
        v
        (λ () (serialize-raw-object h))))]
    [(default _) (format "~a" v)]))

(define to-string (lambda (o) (serialize o "tostring" #f)))
(define to-repr (lambda (o) (serialize o "_torepr" #f)))
(define raw-repr (lambda (o) (serialize o "_torepr" #t)))

(define tostring-pfun (pλ/internal (loc) (o)
  (mk-str (to-string o))))
(define torepr-pfun (pλ/internal (loc) (o)
  (mk-str (to-repr o))))

(define (pyret-print o)
  (define str
    (cond
      [(p-str? o) (p-str-s o)]
      [else (to-repr o)]))
  (begin
    (printf "~a\n" str)
    o))

(define print-pfun (pλ/internal (loc) (o) (pyret-print o)))

(define (truncate-str str n) (substring str 0 (min n (string-length str))))

(define (throw-type-error! typname o)
  (raise (pyret-error
          (get-top-loc)
           "type-error"
           (format "typecheck failed; expected ~a and got\n~a"
                              typname (truncate-str (to-repr o) 100)))))

;; check-brand-pfun : Loc -> Value * -> Value
(define check-brand-pfun (pλ/internal (loc) (ck o s)
  (cond
    [(and (p-str? s) (p-fun? ck))
     (define f (p-base-app ck))
     (define check-v (f o))
     (if (pyret-true? check-v)
         o
         ;; NOTE(dbp): not sure how to give good reporting
         ;; NOTE(joe): This is better, but still would be nice to highlight
         ;;  the call site as well as the destination
         (let ([typname (p-str-s s)])
          (throw-type-error! typname o)))]
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

;; tie the knot of mutual state problems
(void
  (meta-num)
  (meta-bool)
  (meta-str))

(define (mutable-to-repr v)
  (py-match v
    [(p-str _ _ _ _ s) (mk-str (format "mutable(~s)" s))]
    [(p-num _ _ _ _ n) (mk-str (format "mutable(~a)" n))]
    [(p-bool _ _ _ _ b) (mk-str (format "mutable(~a)" (if b "true" "false")))]
    [(p-nothing _ _ _ _) (mk-str "mutable(nothing)")]
    [(default v) (mk-str "mutable-field")]))
(define mutable-dict
  (make-string-map
    (list
      (cons "_equals" (pμ/internal (loc) (self other)
        "Check equality of this mutable field with another"
        (mk-bool (eq? self other))))
      (cons "_torepr" (pμ/internal (loc) (self)
        "Print this mutable field"
        (mutable-to-repr (unbox (p-mutable-b self)))))
      (cons "tostring" (pμ/internal (loc) (self)
        "Print this mutable field"
        (mutable-to-repr (unbox (p-mutable-b self)))))
      (cons "get" (pμ/internal (loc) (self)
        "Get the value in this mutable field"
        (when (not (p-mutable? self))
          (throw-type-error! "Mutable" self))
        (define checks (p-mutable-read-wrappers self))
        (foldr (lambda (c v) (c v)) (unbox (p-mutable-b self)) checks))))))

(define (get-placeholder-value loc p)
  (when (not (p-placeholder? p))
    (throw-type-error! "Placeholder" p))
  (define value (unbox (p-placeholder-b p)))
  (if value value (raise (pyret-error loc "get-uninitialized-placeholder"
                          (format "Tried to get value from uninitialized placeholder")))))

(define placeholder-dict
  (make-string-map
    (list
      (cons "_equals" (pμ/internal (loc) (self other)
        "Check equality of this placeholder with another"
        (mk-bool (eq? self other))))
      (cons "_torepr" (pμ/internal (loc) (self)
        "Print this placeholder"
        (mk-str "cyclic-field")))
      (cons "tostring" (pμ/internal (loc) (self)
        "Print this placeholder"
        (mk-str "cyclic-field")))
      (cons "get" (pμ/internal (loc) (self)
        "Get the value in the placeholder"
        (get-placeholder-value dummy-loc self)))
      (cons "set" (pμ/internal (loc) (self new-value)
        "Set the value in the placeholder"
        (when (not (p-placeholder? self))
          (throw-type-error! "Placeholder" self))
        (define value (unbox (p-placeholder-b self)))
        (when value (raise (pyret-error (get-top-loc) "set-initialized-placeholder"
                      (format "Tried to set value in already-initialized placeholder"))))
        (define wrappers (p-placeholder-wrappers self))
        (define value-checked (foldr (lambda (c v) (c v)) new-value wrappers))
        (set-box! (p-placeholder-b self) value-checked)
        nothing))
      (cons "guard" (pμ/internal (loc) (self pred)
        "Add a guard to the placeholder for when it is set"
        (when (not (p-placeholder? self))
          (throw-type-error! "Placeholder" self))
        (define check (p-base-app check-brand-pfun))
        (define checked-pred (check Function pred (mk-str "Function")))
        (define value (unbox (p-placeholder-b self)))
        (when value (raise (pyret-error (get-top-loc) "guard-initialized-placeholder"
                          (format "Tried to add guard on an already-initialized placeholder"))))
        (define wrappers (p-placeholder-wrappers self))
        (set-p-placeholder-wrappers! self (cons (p-base-app pred) wrappers))
        nothing)))))

(define array-dict
  (make-string-map
    (list
      (cons "_equals"
        (pμ/internal (loc) (self other)
          "Check equality of this array with another"
          (mk-bool (eq? self other))))
      (cons "eq"
        (pμ/internal (loc) (self other)
          "Check equality of this array with another"
          (mk-bool (eq? self other))))
      (cons "_torepr"
        (pμ/internal (loc) (self)
          "Get a printable representation of the array"
          (when (not (p-array? self)) (throw-type-error! "Array" self))
          (mk-str (format "array(~a)" (serialize (mk-list (vector->list (p-array-v self))) "_torepr" #f)))))
      (cons "tostring"
        (pμ/internal (loc) (self)
          "Get a printable representation of the array"
          (when (not (p-array? self)) (throw-type-error! "Array" self))
          (mk-str (format "array(~a)" (serialize (mk-list (vector->list (p-array-v self))) "tostring" #f)))))
      (cons "get"
        (pμ/internal (loc) (self n)
          "Get the value at index n"
          (when (not (p-array? self)) (throw-type-error! "Array" self))
          (when (not (p-num? n)) (throw-type-error! "Number" n))
          (define index (p-num-n n))
          (define vec (p-array-v self))
          (when (not (exact-nonnegative-integer? index))
            (raise (pyret-error (get-top-loc) "array-get-non-integer"
              (format "Index ~a is negative or a non-integer value, which cannot be used for a array access" index))))
          (when (> index (sub1 (vector-length vec)))
            (raise (pyret-error (get-top-loc) "array-get-too-large"
              (format "Index ~a too large for array ~a in array-get" index (to-repr self)))))
          (vector-ref vec index)))
      (cons "set"
        (pμ/internal (loc) (self n v)
          "Set the value at index n to v"
          (when (not (p-array? self)) (throw-type-error! "Array" self))
          (when (not (p-num? n)) (throw-type-error! "Number" n))
          (define index (p-num-n n))
          (define vec (p-array-v self))
          (when (not (exact-nonnegative-integer? index))
            (raise (pyret-error (get-top-loc) "array-set-non-integer"
              (format "Index ~a is negative or a non-integer value, which cannot be used for a array update" index))))
          (when (> index (sub1 (vector-length vec)))
            (raise (pyret-error (get-top-loc) "array-set-too-large"
              (format "Index ~a too large for array ~a in array-set" index (to-repr self)))))
          (vector-set! vec index v)
          self))
      (cons "length"
        (pμ/internal (loc) (self)
          "Get the length of this array"
          (when (not (p-array? self)) (throw-type-error! "Array" self))
          (mk-num (vector-length (p-array-v self)))))
      (cons "to-list"
        (pμ/internal (loc) (self)
          "Get a list of the elements in this array"
          (when (not (p-array? self)) (throw-type-error! "Array" self))
          (mk-list (vector->list (p-array-v self))))))))

(define gensym-pfun (pλ (s)
  "Generate a random string with the given prefix"
  (cond
    [(p-str? s)
     (mk-str (symbol->string (gensym (p-str-s s))))]
    [else (throw-type-error! "String" s)])))

(define p-true (p-bool no-brands meta-bool-store (bad-app "true") (bad-meth "true") #t))
(define p-false (p-bool no-brands meta-bool-store (bad-app "false") (bad-meth "false") #f))
;; mk-bool : Boolean -> Value
(define (mk-bool b)
  (if b p-true p-false))
(define p-else p-true)
(define p-pi (mk-num pi))
(define p-e (mk-num e))

(define Any (pλ/internal (loc) (_) p-true))

(define-syntax-rule (mk-pred name test)
  (define name
    (pλ (arg)
      (format "Built-in predicate for ~a" 'name)
      (mk-bool (test arg)))))

(mk-pred Number p-num?)
(mk-pred String p-str?)
(mk-pred Bool p-bool?)
(mk-pred Boolean p-bool?)
(mk-pred Object p-object?)
(mk-pred Nothing p-nothing?)
(mk-pred Function p-fun?)
(mk-pred Method p-method?)
(mk-pred Mutable p-mutable?)
(mk-pred Placeholder p-placeholder?)
(mk-pred Array p-array?)
(mk-pred Opaque p-opaque?)

(mk-pred is-number p-num?)
(mk-pred is-string p-str?)
(mk-pred is-bool p-bool?)
(mk-pred is-boolean p-bool?)
(mk-pred is-object p-object?)
(mk-pred is-nothing p-nothing?)
(mk-pred is-function p-fun?)
(mk-pred is-method p-method?)
(mk-pred is-mutable p-mutable?)
(mk-pred is-placeholder p-placeholder?)
(mk-pred is-array p-array?)

(define py-link #f)
(define py-empty #f)

(define ___set-link (pλ (link) "Set the internal function for creating links"
  (when py-link
    (raise (format "Runtime link is already set to ~a, and someone tried to update it to ~a." py-link link)))
  (set! py-link link)
  nothing))
(define ___set-empty (pλ (empty) "Set the internal function for creating emptys"
  (when py-empty
    (raise (format "Runtime empty is already set to ~a, and someone tried to update it to ~a." py-empty empty)))
  (set! py-empty empty)
  nothing))

(define (mk-list lst)
  (define link py-link)
  (define empty py-empty)
  (foldl (λ (elt acc) (apply-fun link dummy-loc elt acc)) empty (reverse lst)))

