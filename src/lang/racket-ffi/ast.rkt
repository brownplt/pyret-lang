#lang racket/base

(require
  "../ast.rkt"
  "../load.rkt"
  "../desugar.rkt"
  "../desugar-check.rkt"
  (except-in "../runtime.rkt" raise)
  "../ffi-helpers.rkt"
  "../string-map.rkt"
  pyret/lang/well-formed
  pyret/lang/indentation
  pyret/lang/eval
  pyret/lang/typecheck
  pyret/lang/type-env
  ragg/support
  racket/match
  racket/list
  racket/set
  (rename-in "ast.arr" [%PYRET-PROVIDE ast]))

(define-syntax-rule (build type arg ...)
  (p:apply-fun (p:get-field p:dummy-loc ast (symbol->string (quote type))) p:dummy-loc (ffi-wrap arg) ...))

(define-syntax-rule (const type)
  (p:get-field p:dummy-loc ast (symbol->string (quote type))))

(define (tp-loc l)
  (match l
    [(srcloc path line col start span)
     (build loc path line col)]))

(define (to-pyret ast)
  (define tp to-pyret)
  (define (tp-if-branch b)
   (match b
     [(s-if-branch s tst blk)
      (build s_if_branch (tp-loc s) (tp tst) (tp blk))]
     [_ (error (format "Not an if-branch: ~a" b))]))
  (define (tp-cases-branch b)
   (match b
     [(s-cases-branch s name args blk)
      (build s_cases_branch (tp-loc s) (symbol->string name) (map tp-bind args) (tp blk))]
     [_ (error (format "Not a cases-branch: ~a" b))]))
  (define (tp-variant variant)
    (define (tp-variant-member vm)
      (match vm
        [(s-variant-member l member-type bind)
         (build s_variant_member (tp-loc l) (symbol->string member-type) (tp-bind bind))]))
    (match variant
      [(s-variant l name members with-members)
       (build s_variant
          (tp-loc l)
          (symbol->string name)
          (map tp-variant-member members)
          (map tp-member with-members))]
      [(s-singleton-variant l name members)
       (build s_singleton_variant
          (tp-loc l)
          (symbol->string name)
          (map tp-member members))]
      [_ (error (format "Not a variant: ~a" variant))]))
  (define (tp-member m)
    (match m
      [(s-data-field s name e)
       (build s_data_field (tp-loc s) (tp name) (tp e))]
      [(s-mutable-field s name ann e)
       (build s_mutable_field (tp-loc s) (tp name) (tp-ann ann) (tp e))]
      [(s-once-field s name ann e)
       (build s_once_field (tp-loc s) (tp name) (tp-ann ann) (tp e))]
      [(s-method-field s name args ann doc body check)
       (build s_method_field
          (tp-loc s)
          (tp name)
          (map tp-bind args)
          (tp-ann ann)
          doc
          (tp body)
          (tp check))]
       [_ (error (format "Not a member: ~a" m))]))
  (define (tp-bind b)
    (match b
      [(s-bind s id a) (build s_bind (tp-loc s) (symbol->string id) (tp-ann a))]
      [_ (error (format "Not a bind: ~a" b))]))
  (define (tp-header header)
    (match header
      [(s-import s (? symbol? path) name)
       (build s_import (tp-loc s) (build s_const_import (symbol->string path)) (symbol->string name))]
      [(s-import s (? string? path) name)
       (build s_import (tp-loc s) (build s_file_import path) (symbol->string name))]
      [(s-provide s block)
       (build s_provide (tp-loc s) (tp block))]
      [(s-provide-all s)
       (build s_provide_all (tp-loc s))]
      [_ (error (format "Couldn't match header: ~a\n" header))]))
  (match ast
    [(s-prog s imports block)
     (build s_program (tp-loc s) (map tp-header imports) (tp block))]
    [(s-block s stmts)
     (build s_block (tp-loc s) (map tp stmts))]
    [(s-user-block s body)
     (build s_user_block (tp-loc s) (tp body))]
    [(s-data s name params mixins variants share-members check)
     (build s_data
        (tp-loc s)
        (symbol->string name)
        (map symbol->string params)
        (map tp mixins)
        (map tp-variant variants)
        (map tp-member share-members)
        (tp check))]

    [(s-for s iter bindings ann body)
     (define (tp-for-bind b)
      (match b
        [(s-for-bind s b v)
         (build s_for_bind (tp-loc s) (tp-bind b) (tp v))]))
     (build s_for
        (tp-loc s)
        (tp iter)
        (map tp-for-bind bindings)
        (tp-ann ann)
        (tp body))]

    [(s-var s name val)
     (build s_var (tp-loc s) (tp-bind name) (tp val))]
    [(s-let s name val)
     (build s_let (tp-loc s) (tp-bind name) (tp val))]

    [(s-graph s bindings)
     (build s_graph (tp-loc s) (map tp bindings))]

    [(s-fun s name typarams args ann doc body check)
     (build s_fun
        (tp-loc s)
        (symbol->string name)
        (map symbol->string typarams)
        (map tp-bind args)
        (tp-ann ann)
        doc
        (tp body)
        (tp check))]

    [(s-lam s typarams args ann doc body check)
     (build s_lam
        (tp-loc s)
        (map symbol->string typarams)
        (map tp-bind args)
        (tp-ann ann)
        doc
        (tp body)
        (tp check))]

    [(s-method s args ann doc body check)
     (build s_method
        (tp-loc s)
        (map tp-bind args)
        (tp-ann ann)
        doc
        (tp body)
        (tp check))]

    [(s-when s test body)
     (build s_when
        (tp-loc s)
        (tp test)
        (tp body))]

    [(s-check s body)
     (build s_check
            (tp-loc s)
            (tp body))]

    [(s-if s if-bs)
     (build s_if (tp-loc s) (map tp-if-branch if-bs))]

    [(s-if-else s if-bs else)
     (build s_if_else (tp-loc s) (map tp-if-branch if-bs) (tp else))]

    [(s-cases s type val c-bs)
     (build s_cases (tp-loc s)
                    (tp-ann type)
                    (tp val)
                    (map tp-cases-branch c-bs))]

    [(s-cases-else s type val c-bs else)
     (build s_cases_else (tp-loc s)
                         (tp-ann type)
                         (tp val)
                         (map tp-cases-branch c-bs) (tp else))]

    [(s-try s try exn catch)
     (build s_try
        (tp-loc s)
        (tp try)
        (tp-bind exn)
        (tp catch))]

    [(s-assign s name expr)
     (build s_assign
        (tp-loc s)
        (symbol->string name)
        (tp expr))]

    [(s-app s fun args)
     (build s_app
        (tp-loc s)
        (tp fun)
        (map tp args))]

    [(s-left-app s target fun args)
     (build s_left_app
        (tp-loc s)
        (tp target)
        (tp fun)
        (map tp args))]

    [(s-extend s super fields)
     (build s_extend
        (tp-loc s)
        (tp super)
        (map tp-member fields))]

    [(s-update s super fields)
     (build s_update
        (tp-loc s)
        (tp super)
        (map tp-member fields))]

    [(s-obj s fields)
     (build s_obj (tp-loc s) (map tp-member fields))]

    [(s-list s elts)
     (build s_list (tp-loc s) (map tp elts))]

    [(s-dot s val field)
     (build s_dot
        (tp-loc s)
        (tp val)
        (symbol->string field))]

    [(s-get-bang s val field)
     (build s_get_bang
        (tp-loc s)
        (tp val)
        (symbol->string field))]

    [(s-bracket s val field)
     (build s_bracket
        (tp-loc s)
        (tp val)
        (tp field))]

    [(s-colon s obj field)
     (build s_colon
        (tp-loc s)
        (tp obj)
        (symbol->string field))]

    [(s-colon-bracket s obj field)
     (build s_colon_bracket
        (tp-loc s)
        (tp obj)
        (tp field))]

    [(s-paren s e) (build s_paren (tp-loc s) (tp e))]

    [(s-not s e) (build s_not (tp-loc s) (tp e))]

    [(s-op s op e1 e2)
     (build s_op (tp-loc s) (symbol->string op) (tp e1) (tp e2))]

    [(s-check-test s op e1 e2)
     (build s_check_test (tp-loc s) (symbol->string op) (tp e1) (tp e2))]

    [(s-num s n) (build s_num (tp-loc s) n)]
    [(s-str s str) (build s_str (tp-loc s) str)]
    [(s-bool s b) (build s_bool (tp-loc s) b)]
    [(s-id s x) (build s_id (tp-loc s) (symbol->string x))]
    [(s-data-field s name e)
     (build s_data_field (tp-loc s) (tp name) (tp e))]
    [(s-method-field s name args ann doc body check)
     (build s_method_field
        (tp-loc s)
        (tp name)
        (map tp-bind args)
        (tp-ann ann)
        doc
        (tp body)
        (tp check))]
    [_ (error "No transformation for ~a" ast)]))

(define (tp-ann ann)
  (match ann
    [(a-name s id)
     (build a_name (tp-loc s) (symbol->string id))]
    [(a-dot s obj fld)
     (build a_dot (tp-loc s) (symbol->string obj) (symbol->string fld))]
    [(a-blank) (const a_blank)]
    [(a-any) (const a_any)]
    [(a-arrow s args result)
     (build a_arrow (tp-loc s) (map tp-ann args) (tp-ann result))]
    [(a-method s args result)
     (build a_method (tp-loc s) (map tp-ann args) (tp-ann result))]
    [(a-app s ann parameters)
     (build a_app (tp-loc s) (tp-ann ann) (map tp-ann parameters))]
    [(a-pred s ann pred)
     (build a_pred (tp-loc s) (tp-ann ann) (to-pyret pred))]
    [(a-record s fields)
     (define (tp-a-field field)
      (match field
        [(a-field s name ann)
         (build a_field (tp-loc s) name (tp-ann ann))]))
     (build a_record (tp-loc s) (map tp-a-field fields))]
    [else
     (error (format "ast: don't know how to convert ann: ~a" ann))]))

(define (get-desugared str src check-mode?)
  (define ast (parse-pyret (string-append " " str) src))
  (define desugar
    (cond
      [check-mode? (lambda (e) (desugar-pyret (desugar-check e)))]
      [else desugar-pyret]))
  (desugar ast))

(define (pyret/tc str src options)
  (define check-mode? (ffi-unwrap (p:get-field p:dummy-loc options "check")))
  (define env (p:get-dict (p:get-field p:dummy-loc options "env")))
  (define desugared (get-desugared str src check-mode?))
  (define with-contracts (contract-check-pyret desugared (extend-env-with-dict LIBRARY-ENV env)))
  (to-pyret with-contracts))

(define (pyret-pair-from-string str src options)
  (define ast (parse-pyret (string-append " " str) src))
  (define check-mode? (ffi-unwrap (p:get-field p:dummy-loc options "check")))
  (p:mk-object
    (make-string-map
      (list
        (cons "pre-desugar" (to-pyret ast))
        (cons "post-desugar" (to-pyret (get-desugared str src check-mode?)))))))

(define-syntax-rule (has-brand obj brand)
  (ffi-unwrap (p:apply-fun (p:get-field p:dummy-loc ast (string-append "is-" (symbol->string (quote brand)))) p:dummy-loc (ffi-wrap obj))))
(define-syntax-rule (has-type obj brand)
  (ffi-unwrap (p:apply-fun (p:get-field p:dummy-loc ast (symbol->string (quote brand))) p:dummy-loc (ffi-wrap obj))))

(define-syntax-rule (tr-obj obj constr (trans args ... field) ...)
  (begin
    ;; (printf "\nconstr is ~a\n" constr)
    ;; (printf "actual obj keys are ~a\n" (map ffi-unwrap (p:structural-list->list (p:apply-fun prim-keys p:dummy-loc obj))))
    ;; (printf "expected keys are ~a\n" (list (symbol->string (quote field)) ...))
    ;; (let ((argval (p:get-field p:dummy-loc (ffi-unwrap obj) (symbol->string (quote field)))))
    ;;   (printf "trans is ~a, args are ~a, field is ~a ==> ~a\n"
    ;;           trans (list args ...) (symbol->string (quote field)) (p:to-string argval))) ...
    ;; (printf "\n")
    (constr (trans args ... (ffi-unwrap (p:get-field p:dummy-loc (ffi-unwrap obj) (symbol->string (quote field))))) ...)))

(define (noop x) x)

(define (tr-loc l)
  (define (mini-srcloc f l c) (srcloc f l c #f #f))
  (cond
   [(has-type l Loc)
    (tr-obj l mini-srcloc (noop file) (noop line) (noop column))]))

(define (to-racket ast)
  (define (tr-ifBranch b)
    (cond
     [(has-brand b s_if_branch)
      (tr-obj b s-if-branch (tr-loc l) (tr-expr test) (tr-expr body))]
     [else (error (format "Couldn't match if-branch: ~a" (p:to-string (ffi-unwrap b))))]))
  (define (tr-casesBranch b)
    (cond
     [(has-brand b s_cases_branch)
      (tr-obj b s-cases-branch (tr-loc l) (string->symbol name) (map tr-bind args) (tr-expr body))]
     [else (error (format "Couldn't match cases-branch: ~a" (p:to-string (ffi-unwrap b))))]))
  (define (tr-variant variant)
    (define (tr-variant-member vm)
      (cond
        [(has-brand vm s_variant_member)
         (tr-obj vm s-variant-member (tr-loc l) (string->symbol member_type) (tr-bind bind))]))
    (cond
     [(has-brand variant s_variant)
      (tr-obj variant s-variant (tr-loc l) (string->symbol name) (map tr-variant-member members) (map tr-member with_members))]
     [(has-brand variant s_singleton_variant)
      (tr-obj variant s-singleton-variant (tr-loc l) (string->symbol name) (map tr-member with_members))]
     [else (error (format "Couldn't match variant: ~a" (p:to-string (ffi-unwrap variant))))]))
  (define (tr-member m)
    (cond
     [(has-brand m s_data_field)
      (tr-obj m s-data-field (tr-loc l) (tr-expr name) (tr-expr value))]
     [(has-brand m s_mutable_field)
      (tr-obj m s-mutable-field (tr-loc l) (tr-expr name) (tr-ann ann) (tr-expr value))]
     [(has-brand m s_once_field)
      (tr-obj m s-once-field (tr-loc l) (tr-expr name) (tr-ann ann) (tr-expr value))]
     [(has-brand m s_method_field)
      (tr-obj m s-method-field
              (tr-loc l) (tr-expr name) (map tr-bind args) (tr-ann ann) (noop doc) (tr-expr body) (tr-expr check))]
     [else (error (format "Couldn't match member: ~a" (p:to-string (ffi-unwrap m))))]))
  (define (tr-bind b)
    (cond
     [(has-brand b s_bind)
      (tr-obj b s-bind (tr-loc l) (string->symbol id) (tr-ann ann))]
     [else (error (format "Couldn't match bind: ~a" (p:to-string (ffi-unwrap b))))]))
  (define (tr-forBind b)
    (cond
     [(has-brand b s_for_bind)
      (tr-obj b s-for-bind (tr-loc l) (tr-bind bind) (tr-expr value))]
     [else (error (format "Couldn't match for-bind: ~a" (p:to-string (ffi-unwrap b))))]))
  (define (tr-header h)
    (cond
      [(has-brand h s_import)
       (tr-obj h s-import (tr-loc l) (tr-importType file) (string->symbol name))]
      [(has-brand h s_provide)
       (tr-obj h s-provide (tr-loc l) (tr-expr block))]
      [(has-brand h s_provide_all)
       (tr-obj h s-provide-all (tr-loc l))]
      [else (error (format "Couldn't match header: ~a\n" (p:to-string (ffi-unwrap h))))]))
  (define (tr-importType i)
    (cond
     [(has-brand i s_file_import)
      (tr-obj i noop (noop file))]
     [(has-brand i s_const_import)
      (tr-obj i string->symbol (noop module))]
     [else (error (format "Couldn't match importType: ~a\n" (p:to-string (ffi-unwrap i))))]))
  (define (tr-program p)
    (cond
     [(has-brand ast s_program)
      (tr-obj p s-prog (tr-loc l) (map tr-header imports) (tr-expr block))]
     [else (error (format "Couldn't match program: ~a\n" (p:to-string (ffi-unwrap p))))]))
  (define (tr-ann a)
    (cond
     [(has-brand a a_blank)
      (tr-obj a a-blank)]
     [(has-brand a a_any)
      (tr-obj a a-any)]
     [(has-brand a a_name)
      (tr-obj a a-name (tr-loc l) (string->symbol id))]
     [(has-brand a a_arrow)
      (tr-obj a a-arrow (tr-loc l) (map tr-ann args) (tr-ann ret))]
     [(has-brand a a_method)
      (tr-obj a a-method (tr-loc l) (map tr-ann args) (tr-ann ret))]
     [(has-brand a a_record)
      (tr-obj a a-record (tr-loc l) (map tr-afield fields))]
     [(has-brand a a_app)
      (tr-obj a a-app (tr-loc l) (tr-ann ann) (map tr-ann args))]
     [(has-brand a a_pred)
      (tr-obj a a-pred (tr-loc l) (tr-ann ann) (tr-expr exp))]
     [(has-brand a a_dot)
      (tr-obj a a-dot (tr-loc l) (string->symbol obj) (string->symbol field))]
     [else (error (format "Couldn't match ann: ~a" (p:to-string (ffi-unwrap a))))]))
  (define (tr-afield a)
    (cond
     [(has-brand a a_field)
      (tr-obj a a-field (tr-loc l) (noop name) (tr-ann ann))]
     [else (error (format "Couldn't match afield: ~a" (p:to-string (ffi-unwrap a))))]))
  (define (tr-expr e)
    (cond
     [(list? e)
      (map tr-expr e)]
     [(has-brand e s_block)
      (tr-obj e s-block (tr-loc l) (tr-expr stmts))]
     [(has-brand e s_user_block)
      (tr-obj e s-user-block (tr-loc l) (tr-expr body))]
     [(has-brand e s_fun)
      (tr-obj e s-fun
              (tr-loc l) (string->symbol name) (map symbol->string params) (map tr-bind args) (tr-ann ann) (noop doc) (tr-expr body) (tr-expr check))]
     [(has-brand e s_var)
      (tr-obj e s-var (tr-loc l) (tr-bind name) (tr-expr value))]
     [(has-brand e s_let)
      (tr-obj e s-let (tr-loc l) (tr-bind name) (tr-expr value))]
     [(has-brand e s_graph)
      (tr-obj e s-graph (tr-loc l) (map tr-expr bindings))]
     [(has-brand e s_when)
      (tr-obj e s-when (tr-loc l) (tr-expr test) (tr-expr block))]
     [(has-brand e s_assign)
      (tr-obj e s-assign (tr-loc l) (string->symbol id) (tr-expr value))]
     [(has-brand e s_if)
      (tr-obj e s-if (tr-loc l) (map tr-ifBranch branches))]
     [(has-brand e s_if_else)
      (tr-obj e s-if-else (tr-loc l) (map tr-ifBranch branches) (tr-expr _else))]
     [(has-brand e s_cases)
      (tr-obj e s-cases (tr-loc l) (tr-ann type) (tr-expr val) (map tr-casesBranch branches))]
     [(has-brand e s_cases_else)
      (tr-obj e s-cases-else (tr-loc l) (tr-ann type) (tr-expr val) (map tr-casesBranch branches) (tr-expr _else))]
     [(has-brand e s_try)
      (tr-obj e s-try (tr-loc l) (tr-expr body) (tr-bind id) (tr-expr _except))]
     [(has-brand e s_op)
      (tr-obj e s-op (tr-loc l) (string->symbol op) (tr-expr left) (tr-expr right))]
     [(has-brand e s_check_test)
      (tr-obj e s-check-test (tr-loc l) (string->symbol op) (tr-expr left) (tr-expr right))]
     [(has-brand e s_not)
      (tr-obj e s-not (tr-loc l) (tr-expr expr))]
     [(has-brand e s_paren)
      (tr-obj e s-paren (tr-loc l) (tr-expr expr))]
     [(has-brand e s_lam)
      (tr-obj e s-lam (tr-loc l) (map string->symbol params) (map tr-bind args) (tr-ann ann) (noop doc) (tr-expr body) (tr-expr check))]
     [(has-brand e s_method)
      (tr-obj e s-method (tr-loc l) (map tr-bind args) (tr-ann ann) (noop doc) (tr-expr body) (tr-expr check))]
     [(has-brand e s_extend)
      (tr-obj e s-extend (tr-loc l) (tr-expr super) (map tr-member fields))]
     [(has-brand e s_update)
      (tr-obj e s-update (tr-loc l) (tr-expr super) (map tr-member fields))]
     [(has-brand e s_obj)
      (tr-obj e s-obj (tr-loc l) (map tr-member fields))]
     [(has-brand e s_list)
      (tr-obj e s-list (tr-loc l) (map tr-expr values))]
     [(has-brand e s_app)
      (tr-obj e s-app (tr-loc l) (tr-expr _fun) (map tr-expr args))]
     [(has-brand e s_left_app)
      (tr-obj e s-left-app (tr-loc l) (tr-expr obj) (tr-expr _fun) (map tr-expr args))]
     [(has-brand e s_id)
      (tr-obj e s-id (tr-loc l) (string->symbol id))]
     [(has-brand e s_num)
      (tr-obj e s-num (tr-loc l) (noop n))]
     [(has-brand e s_bool)
      (tr-obj e s-bool (tr-loc l) (noop b))]
     [(has-brand e s_str)
      (tr-obj e s-str (tr-loc l) (noop s))]
     [(has-brand e s_dot)
      (tr-obj e s-dot (tr-loc l) (tr-expr obj) (string->symbol field))]
     [(has-brand e s_get_bang)
      (tr-obj e s-get-bang (tr-loc l) (tr-expr obj) (string->symbol field))]
     [(has-brand e s_bracket)
      (tr-obj e s-bracket (tr-loc l) (tr-expr obj) (tr-expr field))]
     [(has-brand e s_colon)
      (tr-obj e s-colon (tr-loc l) (tr-expr obj) (string->symbol field))]
     [(has-brand e s_colon_bracket)
      (tr-obj e s-colon-bracket (tr-loc l) (tr-expr obj) (tr-expr field))]
     [(has-brand e s_data)
      (tr-obj e s-data
              (tr-loc l) (string->symbol name) (map string->symbol params) (map tr-expr mixins) (map tr-variant variants) (map tr-member shared_members) (tr-expr check))]
     [(has-brand e s_for)
      (tr-obj e s-for (tr-loc l) (tr-expr iterator) (map tr-forBind bindings) (tr-ann ann) (tr-expr body))]
     [(has-brand e s_check)
      (tr-obj e s-check (tr-loc l) (tr-expr body))]
     [else (error (format "Couldn't match expr: ~a" (p:to-repr (ffi-unwrap e))))]))
  (cond
   [(has-type ast Program) (tr-program ast)]
   [(has-type ast Header) (tr-header ast)]
   [(has-type ast ImportType) (tr-importType ast)]
   [(has-type ast Expr) (tr-expr ast)]
   [(has-type ast Bind) (tr-bind ast)]
   [(has-type ast Member) (tr-member ast)]
   [(has-type ast ForBind) (tr-forBind ast)]
   [(has-type ast Variant) (tr-variant ast)]
   [(has-type ast IfBranch) (tr-ifBranch ast)]
   [(has-type ast CasesBranch) (tr-casesBranch ast)]
   [(has-type ast Ann) (tr-ann ast)]
   [else (error (format "Unknown AST expression: ~a" (p:to-string (ffi-unwrap ast))))]))



(define (parse-error-wrap f)
  (define (single-or-first l)
    (cond
      [(list? l) (first l)]
      [else l]))
  (lambda args
    (with-handlers
      ([(lambda (e) #t)
        (lambda (e)
         (match e
           [(exn:fail:parsing message cms locs)
            (raise (p:pyret-error (p:loc-list (first locs)) "parse-error" message))]
           [(exn:fail:pyret/wf message cms locs)
            (raise (p:pyret-error (p:loc-list (first locs)) "wf-error" message))]
           [(exn:fail:pyret/indent message cms locs)
            (raise (p:pyret-error (p:loc-list (first locs)) "indent-error" message))]
           [(exn:fail message cms)
            (raise (p:pyret-error p:dummy-loc "other-parse-error" message))]))])
    (apply f args))))


(define export
  (p:extend
    p:dummy-loc
    ast
    (list
      (cons "free-ids"
            (ffi-wrap (lambda (ast) (map symbol->string (set->list (free-ids ast))))))
      (cons "parse"
            (ffi-wrap (parse-error-wrap pyret-pair-from-string)))
      (cons "parse-tc"
            (ffi-wrap (parse-error-wrap pyret/tc)))
      (cons "to-native"
            (ffi-wrap to-racket))
      (cons "to-pyret"
            (ffi-wrap to-pyret)))))

(provide (rename-out [export %PYRET-PROVIDE]))

