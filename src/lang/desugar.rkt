#lang racket

(provide
  desugar-pyret
  desugar-pyret/libs)
(require
  racket/runtime-path
  "ast.rkt"
  "load.rkt")

;; variant checker name
(define (make-checker-name s)
    (string->symbol (string-append "is-" (symbol->string s))))

(define (make-checker s name tyname brander)
  (s-let s (s-bind s name (a-blank)) (s-dot s brander 'test)))

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
       (define dsg-with-members (map ds-member with-members))
       (define base-obj
         (s-obj s (append super-fields dsg-with-members)))
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
       (define dsg-with-members (map ds-member with-members))
       (define args (map s-bind-id members))
       (define constructor-args members)
       (define base-obj
         (s-obj s (append super-fields dsg-with-members)))
       (define obj
         (s-onion s (s-id s base-name)
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

(define (flatten-blocks maybe-blocks)
  (foldr (λ (stmt block-stmts)
           (match stmt
             [(s-block s stmts) (append (flatten-blocks stmts) block-stmts)]
             [else (cons stmt block-stmts)]))
         empty
         maybe-blocks))

(define (ds-member ast-node)
    (match ast-node
      [(s-data-field s name value)
       (s-data-field s (desugar-internal name) (desugar-internal value))]
      [(s-method-field s name args ann doc body check)
       (s-data-field s (desugar-internal name) (s-method s args ann doc (desugar-internal body) (desugar-internal check)))]))

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
   `((,op+ . "plus")
     (,op- . "minus")
     (,op* . "times")
     (,op/ . "divide")
     (,op<= . "lessequal")
     (,op< . "lessthan")
     (,op>= . "greaterequal")
     (,op> . "greaterthan")
     (,op== . "equals")
     ;; NOTE(dbp): we deal with not specially, since it is .equals(...).not() 
     ;(,op<> . "")
     )))

(define (desugar-internal ast)
  (define ds desugar-internal)
  (define (ds-args binds)
    (map (lambda (b)
      (match b
        [(s-bind s id a) (s-bind s id (desugar-ann a))])) binds))
  (match ast
    [(s-block s stmts)
     (s-block s (flatten-blocks (map ds stmts)))]
    ;; NOTE(joe): generative...
    [(s-data s name params variants share-members)
     (define brander-name (gensym name))
     (define super-fields (map ds-member share-members))
     (ds (s-block s
                  (append
                   (list (s-let s (s-bind s brander-name (a-blank))
                                (s-app s (s-id s 'brander) (list)))
                         (make-checker s name name
                                       (s-id s brander-name)))
                   (variant-defs/list brander-name super-fields variants))))]
    [(s-do s fun args)
     (define (functionize b)
       (s-lam s (list) (list) (a-blank) "" (ds b) (s-block s empty)))
     (s-app s fun (map functionize args))]

    [(s-for s iter bindings ann body)
     (define (expr-of b) (match b [(s-for-bind _ _ e) (ds e)]))
     (define (bind-of b) (match b [(s-for-bind _ b _) b]))
     (define the-function
      (s-lam s (list) (map bind-of bindings) ann "" (ds body) (s-block s empty)))
     (s-app s (ds iter) (cons the-function (map expr-of bindings)))]

    [(s-var s name val)
     (s-var s name (ds val))]
    [(s-let s name val)
     (s-let s name (ds val))]

    [(s-fun s name typarams args ann doc body check)
     (s-let s
            (s-bind s name
                    (a-arrow s (map (replace-typarams typarams)
                                    (map desugar-ann
                                         (map s-bind-ann args)))
                             ((replace-typarams typarams)
                              (desugar-ann ann))))
            (s-lam s typarams (map (replace-typarams-binds typarams)
                                   (ds-args args))
                   ((replace-typarams typarams) (desugar-ann ann))
                   doc (ds body) (ds check)))]

    [(s-lam s typarams args ann doc body check)
     (s-lam s typarams (map (replace-typarams-binds typarams)
                            (ds-args args))
            ((replace-typarams typarams) (desugar-ann ann))
            doc (ds body) (ds check))]

    [(s-method s args ann doc body check)
     (s-method s args ann doc (ds body) (ds check))]

    [(s-when s test body)
     (s-case s (list
      (s-case-branch s (ds test) (ds body))
      (s-case-branch s (s-bool s #t) (s-id s 'p:nothing))))]

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

    ;; NOTE(joe): This is a hack that needs to be cleaned up. It avoids re-desugaring
    ;; catch blocks that already have their call to "make-error" added
    [(s-try _ _ _
	    (s-block _
		     (list
		      (s-app _ _
			     (list (s-app _ (s-bracket _ (s-id _ 'error)
						       (s-str _ "make-error"))
					  _))))))
     ast]

    [(s-try s try exn catch)
     ;; NOTE(joe & dbp): The identifier in the exn binding of the try is carefully
     ;; shadowed here to avoid capturing any names in Pyret.  It is both
     ;; the name that the compiler will use for the exc, and the name
     ;; that desugaring uses to provide the wrapped exception from the error
     ;; library.
     (define make-error (s-app s (s-bracket s (s-id s 'error)
			                      (s-str s "make-error"))
			         (list (s-id s (s-bind-id exn)))))
     (s-try s (ds try) exn
	    (s-block s
		     (list
		      (s-app s (s-lam s (list) (list exn) (a-blank) "" (ds catch) (s-block s empty)) (list make-error)))))]

    [(s-assign s name expr) (s-assign s name (ds expr))]

    [(s-app s fun args) (s-app s (ds fun) (map ds args))]

    [(s-left-app s target fun args)
     (s-app s (ds fun) (cons (ds target) (map ds args)))]

    [(s-onion s super fields) (s-onion s (ds super) (map ds-member fields))]

    [(s-obj s fields) (s-obj s (map ds-member fields))]

    [(s-list s elts)
     (define (get-lib name)
       (s-bracket s (s-id s 'list) (s-str s name)))
     (define (make-link elt acc)
       (s-app s (get-lib "link") (list elt acc)))
     (foldr make-link (get-lib "empty") (map ds elts))]

    [(s-dot s val field) (s-bracket s (ds val) (s-str s (symbol->string field)))]

    [(s-bracket s val field) (s-bracket s (ds val) (ds field))]

    [(s-dot-method s obj field) (s-bracket-method s (ds obj) (s-str s (symbol->string field)))]

    [(s-bracket-method s obj field) (s-bracket-method s (ds obj) (ds field))]

    [(s-paren _ e) (ds e)]

    ;; NOTE(dbp): notequals is special because it requires two method
    [(s-op s 'op<> e1 e2)
     (s-app s (s-bracket s
               (s-app s (s-bracket s (ds e1) (s-str s "equals"))
                      (list (ds e2)))
               (s-str s "not"))
            (list))]

    [(s-op s op e1 e2)
     (s-app s (s-bracket s (ds e1) (s-str s (hash-ref op-method-table op)))
                      (list (ds e2)))]

    [(or (s-num _ _)
         (s-bool _ _)
         (s-str _ _)
         (s-id _ _)) ast]

    [else (error (format "Missed a case in desugaring: ~a" ast))]))

(define (desugar-pyret/libs ast)
  (match ast
    [(s-prog s imps block)
     (desugar-pyret (s-prog s imps block))]))

(define-runtime-path FFI "racket-ffi/")

(define (desugar-pyret ast)
  ;; This is the magic that turns `import foo as bar` into
  ;; `import "/path/to/racket-ffi/foo.rkt" as bar`
  (define (desugar-imp imp)
    (match imp
      [(s-import l (? symbol? f) n)
       (s-import l (path->string (path->complete-path
                      (build-path FFI (string-append (symbol->string f) ".rkt")))) n)]
      [_ imp]))
  (match ast
    [(s-prog s imps block)
     (s-prog s (map desugar-imp imps) (desugar-internal block))]))
