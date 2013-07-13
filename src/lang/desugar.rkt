#lang racket

(provide
  desugar-pyret)
(require
  racket/runtime-path
  "ast.rkt"
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

(define (add-matcher s base name variants)
  (define (app-with-fields s f members)
    (define (member->access m)
      (s-bracket s (s-id s 'val) (s-str s (symbol->string (s-bind-id m)))))
    (s-app s f (map member->access members)))
  (define (helper-for-variant v)
    (match v
      [(s-singleton-variant s _ _)
       (lam s (list 'f) (s-app s (s-id s 'f) (list)))]
      [(s-variant s _ members _)
       (lam s (list 'f) (app-with-fields s (s-id s 'f) members))]))
  (define (pred-entry-for v)
    (match v
      [(or (s-singleton-variant s name _) (s-variant s name _ _))
       (s-data-field s
        (s-str s (symbol->string name))
        (s-id s (make-checker-name name)))]))
  (define (helper-entry-for v)
    (match v
      [(or (s-singleton-variant s name _) (s-variant s name _ _))
       (s-data-field s
        (s-str s (symbol->string name))
        (helper-for-variant v))]))
  (define pred-dict (s-obj s (map pred-entry-for variants)))
  (define helpers-dict (s-obj s (map helper-entry-for variants)))
  (define loop-body
    (s-if-else s
      (list
        (s-if-branch s
          (s-not s
            (s-app s (s-dot s (s-id s 'builtins) 'has-field)
              (list
                (s-id s 'preds)
                (s-dot s (s-id s 'elt) 'key))))
          (s-app s (s-id s 'raise)
            (list
              (s-app s (s-dot s (s-id s 'error) 'invalid-case)
                (list
                  (s-op s 'op+
                    (s-str s "Case does not exist: ")
                    (s-dot s (s-id s 'elt) 'key))
                  (build-location s)))))))
        (s-app s
          (s-bracket s (s-id s 'preds)
                       (s-dot s (s-id s 'elt) 'key))
          (list (s-id s 'val)))))
  (define loop
    (s-for s (s-dot s (s-id s 'list) 'filter)
      (list (s-for-bind s (s-bind s 'elt (a-blank)) (s-id s 'cases)))
      (a-blank)
      loop-body))
  (define post-loop
    (s-if-else s
      (list
        (s-if-branch s
          (s-app s (s-dot s (s-id s 'list) 'is-empty)
                   (list (s-id s 'matched)))
          (s-app s (s-id s 'else-fun) (list))))
      (s-app s
        (s-bracket s (s-id s 'helpers)
                     (s-dot s (s-dot s (s-id s 'matched) 'first) 'key))
        (list (s-dot s (s-dot s (s-id s 'matched) 'first) 'action)))))
  (define matcher-fun
    (s-lam s empty
           (list (s-bind s 'val (a-name s name))
                 (s-bind s 'cases (a-blank))
                 (s-bind s 'else-fun (a-blank)))
           (a-blank)
           ""
           (s-block s
            (list
              (s-let s (s-bind s 'preds (a-blank)) pred-dict)
              (s-let s (s-bind s 'helpers (a-blank)) helpers-dict)
              (s-let s (s-bind s 'matched (a-blank)) loop)
              post-loop))
           (s-block s empty)))
  (s-let s (s-bind s name (a-blank))
    (s-extend s base
      (list (s-data-field s (s-str s "case_matcher") (desugar-internal matcher-fun))))))

(define (variant-defs/list super-brand super-fields variants)
  (define (member->field m val)
    (s-data-field (s-bind-syntax m)
             (s-str (s-bind-syntax m) (symbol->string (s-bind-id m)))
             val))
  (define (apply-brand s brander-name arg)
    (s-app s (s-dot s (s-id s brander-name) 'brand) (list arg)))
  (define (variant-defs v)
    (match v
      [(s-singleton-variant s name with-members)
       (define brander-name (gensym name))
       (define base-name (gensym (string-append (symbol->string name) "_base")))
       (define base-obj
         (s-obj s (append super-fields with-members)))
       (s-block s
         (list
           (s-let s (s-bind s base-name (a-blank)) base-obj)
           (s-let s (s-bind s brander-name (a-blank))
                    (s-app s (s-id s 'brander) (list)))
           (make-checker s (make-checker-name name) name
                         (s-id s brander-name))
           (s-let s (s-bind s name (a-blank))
                    (apply-brand s super-brand
                      (apply-brand s brander-name
                        (s-id s base-name))))))]
      [(s-variant s name members with-members)
       (define brander-name (gensym name))
       (define base-name (gensym (string-append (symbol->string name) "_base")))
       (define args (map s-bind-id members))
       (define constructor-args members)
       (define base-obj
         (s-obj s (append super-fields with-members)))
       (define obj
         (s-extend s (s-id s base-name)
          (map member->field
               members
               (map (lambda (id) (s-id s id)) args))))
       (s-block s
         (list
           (s-let s (s-bind s base-name (a-blank)) base-obj)
           (s-let s (s-bind s brander-name (a-blank))
                    (s-app s (s-id s 'brander) (list)))
           (make-checker s (make-checker-name name) name
                         (s-id s brander-name))
           (s-fun s name
                    (list)
                    constructor-args
                    (a-blank)
                    (format
                     "~a: Creates an instance of ~a"
                     (symbol->string name)
                     (symbol->string name))
                    (s-block s
                     (list
                      (apply-brand s super-brand
                       (apply-brand s brander-name
                        obj))))
                    (s-block s empty))))]))
  (map variant-defs variants))

(define (ds-member ast-node)
    (match ast-node
      [(s-data-field s name value)
       (s-data-field s (desugar-internal name) (desugar-internal value))]
      [(s-method-field s name args ann doc body check)
       ;; NOTE(dbp): we could make the tostring more expensive and
       ;; pass this around as a value, but most of the time it
       ;; should just be a string.
       (let [(best-guess-name (if (s-str? name) (s-str-s name)
                                  ""))]
       (s-data-field s (desugar-internal name)
          (add-lam-tostring s "method" best-guess-name args
          (s-method s args ann doc (desugar-internal body) (desugar-internal check)))))]))

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
    (define matcher-fun (s-dot s type 'case_matcher))
    (define (ds-cases-branch b)
      (match b
        [(s-cases-branch s2 name args body)
         (s-obj s
          (list
            (s-data-field s2 (s-str s2 "key") (s-str s2 (symbol->string name)))
            (s-data-field s2 (s-str s2 "action")
              (s-lam s2 empty args (a-blank) "" body (s-block s2 empty)))))]))
    (define else-fun
      (s-lam s empty empty (a-blank) "" else (s-block s empty)))
    (ds (s-app s matcher-fun (list val (s-list s (map ds-cases-branch cases)) else-fun))))

  (match ast
    [(s-block s stmts)
     (s-block s (flatten-blocks (map ds stmts)))]
    ;; NOTE(joe): generative...
    [(s-data s name params variants share-members check-ignored)
     (define brander-name (gensym name))
     (ds (s-block s
                  (append
                   (list (s-let s (s-bind s brander-name (a-blank))
                                (s-app s (s-id s 'brander) (list)))
                         (add-matcher s (s-dot s (s-id s brander-name) 'test) name variants))
                   (variant-defs/list brander-name share-members variants))))]

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

    [(s-fun s name typarams args ann doc body check)
     (s-let s
            (s-bind s name
                    (a-arrow s (map (replace-typarams typarams)
                                    (map desugar-ann
                                         (map s-bind-ann args)))
                             ((replace-typarams typarams)
                              (desugar-ann ann))))
            (add-lam-tostring s "fun" name args
            (s-lam s typarams (map (replace-typarams-binds typarams)
                                   (ds-args args))
                   ((replace-typarams typarams) (desugar-ann ann))
                   doc (ds body) (ds check))))]

    [(s-lam s typarams args ann doc body check)
     (add-lam-tostring s "fun" "" args
     (s-lam s typarams (map (replace-typarams-binds typarams)
                            (ds-args args))
            ((replace-typarams typarams) (desugar-ann ann))
            doc (ds body) (ds check)))]

    [(s-method s args ann doc body check)
     (add-lam-tostring s "method" "" args
     (s-method s args ann doc (ds body) (ds check)))]

    [(s-when s test body)
     (s-case s (list
      (s-case-branch s (ds test) (ds body))
      (s-case-branch s (s-bool s #t) (s-id s 'p:nothing))))]

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
                              (build-location s))))))))
     (ds-cases s type val cases cases-fallthrough)]

    [(s-cases-else s type val cases else-block)
     (ds-cases s type val cases else-block)]

    [(s-case s c-bs)
     (define (ds-case branch)
       (match branch
         [(s-case-branch s tst blk) (s-case-branch s (ds tst) (ds blk))]))
     (define case-fallthrough
       (s-block s
                (list
                 (s-app s
                        (s-id s 'raise)
                        (list (s-str s "case: no cases matched"))))))
     (s-case s
             (append (map ds-case c-bs)
                     (list (s-case-branch s (s-bool s #t) case-fallthrough))))]

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

    [(s-obj s fields) (s-obj s (map ds-member fields))]

    [(s-list s elts)
     (define (get-lib name)
       (s-bracket s (s-id s 'list) (s-str s name)))
     (define (make-link elt acc)
       (s-app s (get-lib "link") (list elt acc)))
     (foldr make-link (get-lib "empty") (map ds elts))]

    [(s-dot s val field) (s-bracket s (ds val) (s-str s (symbol->string field)))]

    [(s-bracket s val field) (s-bracket s (ds val) (ds field))]

    [(s-colon s obj field) (s-colon-bracket s (ds obj) (s-str s (symbol->string field)))]

    [(s-colon-bracket s obj field) (s-colon-bracket s (ds obj) (ds field))]

    [(s-paren _ e) (ds e)]


    [(s-not s e) (s-app s (s-bracket s (ds e)
                                     (s-str s "_not")) (list))]
    
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
      [(s-data s name _ variants _ _)
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
      [(s-provide-all l)
       (define fields
        (for/list ((id (top-level-ids body)))
          (s-data-field l (s-str l (symbol->string id)) (s-id l id))))
       (s-provide l (s-obj l fields))]
      [_ imp]))
  (match ast
    [(s-prog s imps block)
     (s-prog s (map (lambda (imp) (desugar-imp imp block)) imps)
               (desugar-internal block))]))

