#lang racket

(provide
  desugar-pyret)
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

;; variant checker name
(define (make-checker-name s)
    (string->symbol (string-append "is-" (symbol->string s))))

(define (make-checker s name tyname brander)
  (s-let s (s-bind s name (a-blank)) (s-dot s brander 'test)))

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

(define (variant-defs/list super-brand mixins-names super-fields variants)
  (define (apply-brand s brander-name arg)
    (s-app s (s-dot s (s-id s brander-name) 'brand) (list arg)))
  (define (variant-defs v)
    (define (member->string m)
      (match m
        [(s-bind s2 m-name _) (s-str s2 (symbol->string m-name))]))
    (define (make-equals s brander fields)
      (meth s (list 'self 'other)
        (s-app s (s-dot s (s-id s 'builtins) 'data-equals)
          (append
            (list
            (s-id s 'self)
            (s-id s 'other)
            (s-id s brander)
            (s-list s (map member->string fields)))))))
    (define (make-match s case-name fields)
      (define call-match-case (gensym (string-append "call-" (symbol->string case-name))))
      (meth s (list 'self 'cases-funs 'else-clause)
        (s-if-else s
         (list
          (s-if-branch s
            (s-app s (s-dot s (s-id s 'builtins) 'has-field)
                   (list (s-id s 'cases-funs) (s-str s (symbol->string case-name))))
            (s-block s
              (list
                (s-let s (s-bind s call-match-case (a-blank)) (s-dot s (s-id s 'cases-funs) case-name))
                (s-app s (s-id s call-match-case)
                       (map (lambda (field-name) (s-dot s (s-id s 'self) (s-bind-id field-name))) fields))))))
         (s-app s (s-id s 'else-clause) (list)))))
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
               (s-app s (s-dot s (s-id s mixin) method) (list obj))) base-obj local-mixins-names))
    
    (match v
      [(s-singleton-variant s name with-members)
       (define torepr
        (meth s (list 'self)
          (s-str s (symbol->string name))))
       (define brander-name (gensym name))
       (define equals (make-equals s (make-checker-name name) (list)))
       (define matcher (make-match s name (list)))
       (define base-name (gensym (string-append (symbol->string name) "_base")))
       (define base-obj
         (s-obj s (append (list
                            (s-data-field s (s-str s "_torepr") torepr)
                            (s-data-field s (s-str s "_equals") equals)
                            (s-data-field s (s-str s "_match") matcher))
                          super-fields
                          with-members)))
       (s-block s
         (flatten (list
           (s-let s (s-bind s base-name (a-blank)) base-obj)
           (s-let s (s-bind s brander-name (a-blank))
                    (s-app s (s-id s 'brander) (list)))
           (local-bind-mixins s)
           (make-checker s (make-checker-name name) name
                         (s-id s brander-name))
           (s-let s (s-bind s name (a-blank))
                    (apply-brand s super-brand
                      (apply-brand s brander-name
                       (fold-mixins s 'brand
                         (fold-mixins s 'extend
                           (s-id s base-name)))))))))]
      [(s-variant s name variant-members with-members)
       (define (member->field m val)
        (match m
          [(s-variant-member s member-type (s-bind s2 name ann))
           (define name-str (s-str s2 (symbol->string name)))
           (match member-type
             ['mutable (s-mutable-field s2 name-str ann val)]
             ['normal (s-data-field s2 name-str val)]
             ['cyclic (s-once-field s2 name-str ann val)]
             [_ (error (format "Bad variant type: ~a" member-type))])]))
       (define (member->constructor-arg m new-id)
        (match m
          [(s-variant-member s member-type (s-bind s2 name ann))
           (define name-str (s-str s2 (symbol->string name)))
           (match member-type
             ['mutable (s-bind s2 new-id ann)]
             ['normal (s-bind s2 new-id ann)]
             ['cyclic (s-bind s2 new-id (a-blank))]
             [_ (error (format "Bad variant type: ~a" member-type))])]))
       (define id-members (map s-variant-member-bind variant-members))
       (define torepr
        (meth s (list 'self)
          (s-app s (s-dot s (s-id s 'builtins) 'data-to-repr)
             (list (s-id s 'self)
                   (s-str s (symbol->string name))
                   (s-list s (map member->string id-members))))))
       (define equals (make-equals s (make-checker-name name) id-members))
       (define matcher (make-match s name id-members))
       (define brander-name (gensym name))
       (define base-name (gensym (string-append (symbol->string name) "_base")))
       (define args (map gensym (map s-bind-id id-members)))
       (define constructor-args (map member->constructor-arg variant-members args))
       (define base-obj
         (s-obj s (append (list
                            (s-data-field s (s-str s "_torepr") torepr)
                            (s-data-field s (s-str s "_equals") equals)
                            (s-data-field s (s-str s "_match") matcher))
                          super-fields
                          with-members)))
       (define obj
         (s-extend s (s-id s base-name)
          (map member->field
               variant-members
               (map (lambda (id) (s-id s id)) args))))
       (s-block s
         (list
           (s-let s (s-bind s base-name (a-blank)) base-obj)
           (s-let s (s-bind s brander-name (a-blank))
                    (s-app s (s-id s 'brander) (list)))
           (make-checker s (make-checker-name name) name
                         (s-id s brander-name))
           (s-let s (s-bind s name (a-blank))
             (s-lam s
                    (list)
                    constructor-args
                    (a-blank)
                    (format
                     "~a: Creates an instance of ~a"
                     (symbol->string name)
                     (symbol->string name))
                    (s-block s
                     (flatten (list
                      (local-bind-mixins s)
                      (apply-brand s super-brand
                       (apply-brand s brander-name
                       (fold-mixins s 'brand
                         (fold-mixins s 'extend
                           obj)))))))
                    (s-block s empty)))))]))
  (map variant-defs variants))

(define (ds-member ast-node)
    (match ast-node
      [(s-mutable-field s name ann value)
       (s-mutable-field s name ann (desugar-internal value))]
      [(s-once-field s name ann value)
       (s-once-field s name ann (desugar-internal value))]
      [(s-data-field s name value)
       (s-data-field s (desugar-internal name) (desugar-internal value))]
      [(s-method-field s name args ann doc body check)
       ;; NOTE(dbp): we could make the tostring more expensive and
       ;; pass this around as a value, but most of the time it
       ;; should just be a string.
       (let [(best-guess-name (if (s-str? name) (s-str-s name)
                                  ""))]
       (s-data-field s (desugar-internal name)
          (s-method s args ann doc (desugar-internal body) (desugar-internal check))))]))

(define (desugar-ann ann)
  (match ann
    [(a-pred s a pred) (a-pred s (desugar-ann a) (desugar-internal pred))]
    [(? a-ann?) ann]
    [_ (error 'desugar-ann "Not an annotation: ~a" ann)]))

;; NOTE(dbp): these functions are a temporary hack;
;; they are just stripping out parametric annotations, so
;; that code will compile with them present
(define (replace-typarams typarams)
  (lambda (ann)
    (match ann
      [(a-name s name)
       (if (member name typarams)
           (a-any)
           ann)]
      [_ ann])))
(define (replace-typarams-binds typarams)
  (lambda (bind)
    (match bind
      [(s-bind s1 id (a-name s2 name))
       (if (member name typarams)
           (s-bind s1 id (a-any)) bind)]
      [_ bind])))

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
    (s-app s (s-bracket s (s-id s 'builtins) (s-str s "equiv"))
              (list (ds e1) (ds e2))))
  (define (ds-bind b)
    (match b
      [(s-bind s id a) (s-bind s id (desugar-ann a))]))
  (define (ds-args binds)
    (map ds-bind binds))
  (define (ds-if branch)
    (match branch
      [(s-if-branch s tst blk) (s-if-branch s (ds tst) (ds blk))]))
  (define (ds-cases s type val cases else)
    (define (ds-cases-branch b)
      (match b
        [(s-cases-branch s2 name args body)
         (s-data-field s2 (s-str s2 (symbol->string name))
                       (s-lam s2 empty args (a-blank) "" body (s-block s2 empty)))]))
    (define else-fun
      (s-lam (get-srcloc else) empty empty (a-blank) "" else (s-block (get-srcloc else) empty)))
    (define cases-object
      (s-obj s (map ds-cases-branch cases)))
    (define val-temp-name (gensym "cases-value"))
    (ds
      (s-block s
        (list
          (s-let s (s-bind s val-temp-name type) val)
          (s-app s (s-dot s (s-id s val-temp-name) '_match) (list cases-object else-fun))))))

  (match ast
    [(s-block s stmts)
     (s-block s (flatten-blocks (map ds stmts)))]
    ;; NOTE(joe): generative...
    [(s-data s name params mixins variants share-members check-ignored)
     (define brander-name (gensym name))
     (define mixins-names
       (map (lambda (m) (gensym (string-append (symbol->string name) "-mixins"))) mixins))
     (define bind-mixins
       (map (lambda (m-name m) (s-let s (s-bind s m-name (a-blank)) m)) mixins-names mixins))
     (ds (s-block s
                  (flatten (list
                   (s-let s (s-bind s brander-name (a-blank))
                                (s-app s (s-id s 'brander) (list)))
                   bind-mixins
                   (variant-defs/list brander-name mixins-names share-members variants)
                   (s-let s (s-bind s name (a-blank))
                                  (s-dot s (s-id s brander-name) 'test))))))]

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
            (s-lam s typarams (map (replace-typarams-binds typarams)
                                   (ds-args args))
                   ((replace-typarams typarams) (desugar-ann ann))
                   doc (ds body) (ds check)))]

    [(s-check s body) (s-id s 'nothing)]

    [(s-lam s typarams args ann doc body check)
     (s-lam s typarams (map (replace-typarams-binds typarams)
                            (ds-args args))
            ((replace-typarams typarams) (desugar-ann ann))
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
     ;; TODO(joe): call `cases-miss` from error.arr
     (define cases-fallthrough
       (s-block s
                (list
                 (s-app s
                        (s-id s 'raise)
                        (list
                          (s-app s
                            (s-bracket s (s-id s 'error) (s-str s "cases-miss"))
                            (list
                              (s-str s "cases: no cases matched")
                              (build-location s)
                              (s-list s (list)))))))))
     (ds-cases s type val cases cases-fallthrough)]

    [(s-cases-else s type val cases else-block)
     (ds-cases s type val cases else-block)]

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

    [(s-app s fun args) (s-app s (ds fun) (map ds args))]

    [(s-left-app s target fun args)
     (s-app s (ds fun) (cons (ds target) (map ds args)))]

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


    [(s-not s e) (s-app s (s-bracket s (ds e)
                                     (s-str s "_not")) (list))]

    [(s-op s 'opis e1 e2)
     (s-app
      s
      (s-bracket s (s-id s 'checkers) (s-str s "check-is"))
      (list
        (s-str s (pretty (s-op s 'opis e1 e2)))
        (lam s (list) (ds e1))
        (lam s (list) (ds e2))
        (build-location s)))]

    [(s-op s 'opraises e1 e2)
     (s-app
      s
      (s-bracket s (s-id s 'checkers) (s-str s "check-raises"))
      (list
        (s-str s (pretty (s-op s 'opraises e1 e2)))
        (lam s (list) (ds e1))
        e2
        (build-location s)))]

    [(s-op s 'op== e1 e2) (ds-== s e1 e2)]

    [(s-op s 'op<> e1 e2)
     (s-app s (s-bracket s (ds-== s e1 e2) (s-str s "_not")) (list))]

    [(s-op s op e1 e2)
     (define e2-maybe-thunked
      (if (is-lazy-method? op)
          (s-lam s empty empty (a-blank) "" (ds e2) (s-block s empty))
          (ds e2)))
     (s-app s (s-bracket s (ds e1) (s-str s (hash-ref op-method-table op)))
                      (list e2-maybe-thunked))]

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

(define (top-level-ids block)
  (define (_top-level-ids expr)
    (define (variant-ids variant)
      (match variant
        [(s-variant _ name _ _)
         (list name (make-checker-name name))]
        [(s-singleton-variant _ name _)
         (list name (make-checker-name name))]))
    (match expr
      [(s-let _ (s-bind _ x _) _) (list x)]
      [(s-fun _ name _ _ _ _ _ _) (list name)]
      [(s-data s name _ _ variants _ _)
       (cons name (flatten (map variant-ids variants)))]
      [else (list)]))
  (flatten (map _top-level-ids (s-block-stmts block))))

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
