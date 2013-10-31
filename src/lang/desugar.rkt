#lang racket

(provide
  desugar-pyret
  desugar-internal
  build-location)
(require
  racket/runtime-path
  "ast.rkt"
  "pretty.rkt"
  "load.rkt")

(define (build-location s)
  (define (serialize-source e)
    (cond
      [(symbol? e) (symbol->string e)]
      [(string? e) e]
      [(path? e) (path->string e)]
      [(false? e) "unknown source"]
      [else (error (format "Non-symbol, non-string, non-path value for
                            source: ~a" e))]))
  (s-app s
    (s-bracket s (s-id s 'error) (s-str s "location"))
    (list
      (s-str s (serialize-source (srcloc-source s)))
      (s-num s (srcloc-line s))
      (s-num s (srcloc-column s)))))

(define (lam s args body)
  (s-lam s empty (map (lambda (sym) (s-bind s sym (a-blank))) args) (a-blank) "" body (s-block s empty)))

(define (meth s args body)
  (s-method s (map (lambda (sym) (s-bind s sym (a-blank))) args) (a-blank) "" body (s-block s empty)))

(define (desugar-graph s bindings)
  (define names (map s-bind-id (map s-let-name bindings)))
  (define placeholder-names (map gensym names))
  (define (subst-expr e)
   (foldr (lambda (id new-id expr)
           (subst expr id (s-id s new-id)))
          e
          names
          placeholder-names))
  (define subbed-statements (map subst-expr (map desugar-internal bindings)))
  (s-block s
    (append
     (map (lambda (id) (s-let s (s-bind s id (a-blank)) (s-app s (s-id s 'mk-placeholder) (list)))) placeholder-names)
     subbed-statements
     (map (lambda (id ph-id)
            (s-app s (s-bracket s (s-id s ph-id) (s-str s "set")) (list (s-id s id))))
          names
          placeholder-names))))

(define ((ds-data mixins-names) base-fields variant)
  (define local-mixins-names
    (map (lambda (m) (gensym "mixin")) mixins-names))
  (define (local-bind-mixins s)
    (map (lambda (local-name name)
           (s-let s (s-bind s local-name (a-blank))
                  (s-if-else s
                             (list
                              (s-if-branch s (s-app s (s-id s 'Function) (list (s-id s name)))
                                           (s-app s (s-id s name) (list))))
                             (s-id s name)))) local-mixins-names mixins-names))
  (define (fold-mixins s method base-obj)
    (foldl (lambda (mixin obj)
             (s-app s (s-bracket s (s-id s mixin) (s-str s method)) (list obj)))
           base-obj local-mixins-names))
  (define (variant-constructor s with-members)
    (s-datatype-constructor
       s 'self
       (s-block s
         (flatten (list
           (local-bind-mixins s)
           (fold-mixins s "brand"
            (fold-mixins s "extend"
            ;; NOTE(dbp 2013-10-27): generative, in order to get method fields desugared
             (desugar-internal
              (s-extend s (s-id s 'self) base-fields)))))))))
  (match variant
    [(s-singleton-variant s name with-members)
     (s-datatype-singleton-variant s name (variant-constructor s with-members))]
    [(s-variant s name variant-members with-members)
     (s-datatype-variant s name variant-members (variant-constructor s with-members))]))

(define (ds-member ast-node)
    (match ast-node
      [(s-mutable-field s name ann value)
       (s-mutable-field s name ann (desugar-internal value))]
      [(s-once-field s name ann value)
       (s-once-field s name ann (desugar-internal value))]
      [(s-data-field s name value)
       (s-data-field s (desugar-internal name) (desugar-internal value))]
      [(s-method-field s name args ann doc body check)
       (s-data-field s (desugar-internal name)
          (s-method s args ann doc (desugar-internal body) (desugar-internal check)))]))

(define (ds-curry-args s args)
  (let ((params-and-args
         (foldl
          (lambda (arg l)
            (cond
             [(and (s-id? arg) (equal? (s-id-id arg) '_))
              (let ((next-arg (gensym "arg-")))
                (list (cons (s-bind s next-arg (a-blank)) (first l))
                      (cons (s-id s next-arg) (second l))))]
             [else
              (list (first l) (cons arg (second l)))]))
          (list (list) (list))
          args)))
    (list (reverse (first params-and-args)) (reverse (second params-and-args)))))

(define (ds-curry-binop s e1 e2 rebuild)
  (define params-and-args (ds-curry-args s (list e1 e2)))
  (define params (first params-and-args))
  (cond
   [(null? params)
    (rebuild e1 e2)]
   [else
    (define curry-args (second params-and-args))
    (s-lam s (list) params (a-blank) ""
           (rebuild (first curry-args) (second curry-args)) (s-block s empty))]))
(define (ds-curry-unop s e1 rebuild)
  (define params-and-args (ds-curry-args s (list e1)))
  (define params (first params-and-args))
  (cond
   [(null? params)
    (rebuild e1)]
   [else
    (define curry-args (second params-and-args))
    (s-lam s (list) params (a-blank) ""
           (rebuild (first curry-args)) (s-block s empty))]))

(define (ds-curry ast-node)
  (match ast-node
    [(s-app s f args)
     (define params-and-args (ds-curry-args s args))
     (define params (first params-and-args))
     (cond
        [(null? params)
         ast-node]
        [else
         (s-lam s (list) params (a-blank) ""
              (s-app s f (second params-and-args)) (s-block s empty))])]
    [_ ast-node]))

(define (desugar-ann ann)
  (match ann
    [(a-pred s a pred) (a-pred s (desugar-ann a) (desugar-internal pred))]
    [(? a-ann?) ann]
    [_ (error 'desugar-ann "Not an annotation: ~a" ann)]))


(define op-method-table
  (make-immutable-hash
   `((,op+ . "_plus")
     (,op- . "_minus")
     (,op* . "_times")
     (,op/ . "_divide")
     (,op<= . "_lessequal")
     (,op< . "_lessthan")
     (,op>= . "_greaterequal")
     (,op> . "_greaterthan")
     ;; NOTE(joe): we deal with equals specially, since it is
     ;; builtins.equiv(..., ...)
     ;(,op== . "equals")
     ;; NOTE(dbp): we deal with noteq specially, since it is .equals(...).not()
     ;(,op<> . "")
     ;; NOTE(dbp): 'and' and 'or' are special, because they thunk
     (,opand . "_and")
     (,opor . "_or")
     )))

(define (is-lazy-method? meth)
  (match meth
    [(or 'opand 'opor) #t]
    [_ #f]))

(define (desugar-internal ast)
  (define ds desugar-internal)
  (define (ds-== s e1 e2)
    (ds-curry-binop s (ds e1) (ds e2)
                    (lambda (ds-e1 ds-e2)
                      (s-app s (s-bracket s (s-id s 'builtins) (s-str s "equiv"))
                             (list ds-e1 ds-e2)))))
  (define (ds-bind b)
    (match b
      [(s-bind s id a) (s-bind s id (desugar-ann a))]))
  (define (ds-args binds)
    (map ds-bind binds))
  (define (ds-if branch)
    (match branch
      [(s-if-branch s tst blk) (s-if-branch s (ds tst) (ds blk))]))

  (define ((ds-datatype-variant typarams) v)
    (define (ds-constructor c)
      (match c
        [(s-datatype-constructor s self body)
         (s-datatype-constructor s self (ds body))]))
    (match v
      [(s-datatype-variant s name members constructor)
       (s-datatype-variant s name members (ds-constructor constructor))]
      [(s-datatype-singleton-variant s name constructor)
       (s-datatype-singleton-variant s name (ds-constructor constructor))]))
  (define (ds-cases-branch b)
    (match b
        [(s-cases-branch s name args body)
         (s-cases-branch s name (map ds-bind args) (ds body))]))
  (match ast
    [(s-block s stmts)
     (s-block s (flatten-blocks (map ds stmts)))]
    [(s-data s name params mixins-no-eq variants share-members check-ignored)
     (define mixins (cons (s-bracket s (s-id s 'builtins) (s-str s "Eq")) mixins-no-eq))
     (define mixins-names
       (map (lambda (m) (gensym (string-append (symbol->string name) "-mixins"))) mixins))
     (define bind-mixins
       (map (lambda (m-name m) (s-let s (s-bind s m-name (a-blank)) (ds m))) mixins-names mixins))
     (define shared-id (gensym 'data-shared))
     (define base-names (map (lambda (v) (gensym 'variant)) variants))
     (define (get-with variant)
       (match variant
         [(s-singleton-variant _ _ with) with]
         [(s-variant _ _ _ with) with]))
     (define bind-base-objs
       (map (lambda (v-name v)
              (s-let s (s-bind s v-name (a-blank))
                     (ds (s-extend s (s-id s shared-id) (get-with v)))))
            base-names variants))
     (define (member-name m)
       (match m
         [(s-data-field _ n _) n]
         [(s-method-field _ n _ _ _ _ _) n]))
     (define ((wrap-field obj-id) f)
       (s-data-field s (member-name f)
                     (s-colon-bracket s (s-id s obj-id)
                                (member-name f))))
     (define base-fields
       (map (lambda (b-name v)
              (append
               (map (wrap-field shared-id) share-members)
               (map (wrap-field b-name) (get-with v))))
            base-names variants))
     (s-block
      s
      (flatten
       (list
        bind-mixins
        (s-let s (s-bind s shared-id (a-blank)) (ds (s-obj s share-members)))
        bind-base-objs
        (s-datatype s name params (map (ds-data mixins-names) base-fields variants) check-ignored))))]

    [(s-datatype s name params variants check-ignored)
     (s-datatype s name params (map (ds-datatype-variant params) variants) check-ignored)]

    [(s-for s iter bindings ann body)
     (define (expr-of b) (match b [(s-for-bind _ _ e) (ds e)]))
     (define (bind-of b) (match b [(s-for-bind _ b _) b]))
     (define the-function
      (s-lam s (list) (map bind-of bindings) ann "" (ds body) (s-block s empty)))
     (s-app s (ds iter) (cons the-function (map expr-of bindings)))]

    [(s-var s name val)
     (s-var s (ds-bind name) (ds val))]
    [(s-let s name val)
     (s-let s (ds-bind name) (ds val))]

    [(s-graph s bindings) (desugar-graph s bindings)]

    [(s-user-block s body) (s-user-block s (ds body))]

    [(s-fun s name typarams args ann doc body check)
     (s-let s (s-bind s name (a-blank))
            (s-lam s typarams (ds-args args)
                   (desugar-ann ann)
                   doc (ds body) (ds check)))]

    [(s-check s body) (s-id s 'nothing)]

    [(s-lam s typarams args ann doc body check)
     (s-lam s typarams (ds-args args)
            (desugar-ann ann)
            doc (ds body) (ds check))]

    [(s-method s args ann doc body check)
     (s-method s args ann doc (ds body) (ds check))]

    [(s-when s test body)
     (s-if-else s (list (s-if-branch s (ds test) (ds body)))
      (s-id s 'nothing))]

    [(s-if-else s cases else)
     (s-if-else s (map ds-if cases) (ds else))]

    [(s-if s cases)
     ;; TODO(joe): call some constructor from error.arr for better error
     (define if-fallthrough
       (s-block s
                (list
                 (s-app s
                        (s-id s 'raise)
                        (list (s-str s "if: no tests matched"))))))
     (s-if-else s (map ds-if cases) if-fallthrough)]

    [(s-cases s type val cases)
     (s-cases s (desugar-ann type) (ds val) (map ds-cases-branch cases))]
    [(s-cases-else s type val cases else-block)
     (s-cases-else s (desugar-ann type) (ds val) (map ds-cases-branch cases) (ds else-block))]

    [(s-try s try exn catch)
     (define exn-id (gensym))
     (define make-error (s-app s (s-bracket s (s-id s 'error)
			                      (s-str s "make-error"))
			         (list (s-id s exn-id))))
     (s-try s (ds try) (s-bind (s-bind-syntax exn) exn-id (s-bind-ann exn))
	    (s-block s
		     (list
		      (s-app s (s-lam s (list) (list exn) (a-blank) "" (ds catch) (s-block s empty)) (list make-error)))))]

    [(s-assign s name expr) (s-assign s name (ds expr))]

    [(s-app s fun args) (ds-curry (s-app s (ds fun) (map ds args)))]

    [(s-left-app s target fun args)
     (ds-curry (s-app s (ds fun) (cons (ds target) (map ds args))))]

    [(s-extend s super fields) (s-extend s (ds super) (map ds-member fields))]

    [(s-update s super fields) (s-update s (ds super) (map ds-member fields))]

    [(s-obj s fields) (s-obj s (map ds-member fields))]

    [(s-list s elts)
     (define (get-lib name)
       (s-bracket s (s-id s 'list) (s-str s name)))
     (define (make-link elt acc)
       (s-app s (get-lib "link") (list elt acc)))
     (foldr make-link (get-lib "empty") (map ds elts))]

    [(s-dot s val field) (s-bracket s (ds val) (s-str s (symbol->string field)))]

    [(s-get-bang s val field) (s-get-bang s (ds val) field)]

    [(s-bracket s val field) (s-bracket s (ds val) (ds field))]

    [(s-colon s obj field) (s-colon-bracket s (ds obj) (s-str s (symbol->string field)))]

    [(s-colon-bracket s obj field) (s-colon-bracket s (ds obj) (ds field))]

    [(s-paren _ e) (ds e)]


    [(s-not s e)
     (ds-curry-unop
      s (ds e)
      (lambda (ds-e)
        (define e-curry (s-bracket s ds-e (s-str s "_not")))
        (s-app s e-curry (list))))]

    [(s-check-test s 'opis e1 e2)
     (s-app
      s
      (s-bracket s (s-id s 'checkers) (s-str s "check-is"))
      (list
        (s-str s (pretty (s-op s 'opis e1 e2)))
        (lam s (list) (ds e1))
        (lam s (list) (ds e2))
        (build-location s)))]

    [(s-check-test s 'opraises e1 e2)
     (s-app
      s
      (s-bracket s (s-id s 'checkers) (s-str s "check-raises"))
      (list
        (s-str s (pretty (s-op s 'opraises e1 e2)))
        (lam s (list) (ds e1))
        (ds e2)
        (build-location s)))]

    [(s-check-test s 'opsatisfies e1 e2)
     (s-app
      s
      (s-bracket s (s-id s 'checkers) (s-str s "check-satisfies"))
      (list
        (s-str s (pretty (s-op s 'opsatisfies e1 e2)))
        (lam s (list) (ds e1))
        (lam s (list) (ds e2))
        (build-location s)))]


    [(s-op s 'op== e1 e2) (ds-== s e1 e2)]

    [(s-op s 'op<> e1 e2)
     (ds-curry-binop
      s (ds e1) (ds e2)
      (lambda (ds-e1 ds-e2)
        (s-app s (s-bracket s (ds-== s ds-e1 ds-e2) (s-str s "_not")) (list))))]

    [(s-op s op e1 e2)
     (ds-curry-binop
      s (ds e1) (ds e2)
      (lambda (ds-e1 ds-e2)
        (define e2-maybe-thunked
          (if (is-lazy-method? op)
              (s-lam s empty empty (a-blank) "" ds-e2 (s-block s empty))
              ds-e2))
        (s-app s (s-bracket s ds-e1 (s-str s (hash-ref op-method-table op)))
               (list e2-maybe-thunked))))]

    [(or (s-num _ _)
         (s-bool _ _)
         (s-str _ _)
         (s-id _ _)) ast]

    [else (error (format "Missed a case in desugaring: ~a" ast))]))

(define-runtime-path FFI "racket-ffi/")

(define (add-lam-tostring loc type name args obj)
  (define prefix
    (format "~a ~a(~a): '" type name
            (string-join
             (map
              (compose symbol->string s-bind-id) args)
             ", ")))
  (define end (s-str loc "' end"))
  (s-extend loc obj
            (list (s-data-field loc (s-str loc "tostring")
                   (s-method
                   loc (list (s-bind loc 'self (a-blank)))
                   (a-blank) ""
                   (s-block loc (list
                    (s-app loc
                    (s-bracket loc
                     (s-app loc
                      (s-bracket loc (s-str loc prefix)
                             (s-str loc "_plus"))
                      (list (s-bracket loc (s-id loc 'self)
                                   (s-str loc "_doc"))))
                     (s-str loc "_plus"))
                    (list end))))
                   (s-block loc empty))))))

(define (desugar-pyret ast)
  ;; This is the magic that turns `import foo as bar` into
  ;; `import "/path/to/racket-ffi/foo.rkt" as bar`
  (define (desugar-imp imp body)
    (match imp
      [(s-import l (? symbol? f) n)
       (s-import l (path->string (path->complete-path
                      (build-path FFI (string-append (symbol->string f) ".rkt")))) n)]
      [(s-import l (? string? f) n) imp]
      [(s-provide-all l)
       (define fields
        (for/list ((id (top-level-ids body)))
          (s-data-field l (s-str l (symbol->string id)) (s-id l id))))
       (s-provide l (desugar-internal (s-obj l fields)))]
      [(s-provide l stmt)
       (s-provide l (desugar-internal stmt))]))
  (match ast
    [(s-prog s imps block)
     (s-prog s (map (lambda (imp) (desugar-imp imp block)) imps)
               (desugar-internal block))]))
