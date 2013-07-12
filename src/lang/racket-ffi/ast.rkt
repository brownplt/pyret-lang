#lang racket/base

(require
  "../ast.rkt"
  "../load.rkt"
  "../desugar.rkt"
  "../desugar-check.rkt"
  "../runtime.rkt"
  "../ffi-helpers.rkt"
  "../string-map.rkt"
  racket/match
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
      (build s_if_branch (tp-loc s) (tp tst) (tp blk))]))
  (define (tp-variant variant)
    (match variant
      [(s-variant l name binds members)
       (build s_variant
          (tp-loc l)
          (symbol->string name) 
          (map tp-bind binds)
          (map tp-member members))]
      [(s-singleton-variant l name members)
       (build s_singleton_variant
          (tp-loc l)
          (symbol->string name)
          (map tp-member members))]))
  (define (tp-member m)
    (match m
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
          (tp check))]))
  (define (tp-bind b)
    (match b
      [(s-bind s id a) (build s_bind (tp-loc s) (symbol->string id) (tp-ann a))]))
  (define (tp-args binds)
    (map tp-bind binds))
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
    [(s-data s name params variants share-members check)
     (build s_data
        (tp-loc s)
        (symbol->string name)
        (map symbol->string params)
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

    [(s-case s c-bs)
     (define (tp-case-branch b)
      (match b
        [(s-case-branch s tst blk)
         (build s_case_branch (tp-loc s) (tp tst) (tp blk))]))
     (build s_case
        (tp-loc s)
        (map tp-case-branch c-bs))]

    [(s-if s if-bs)
     (build s_if (tp-loc s) (map tp-if-branch if-bs))]

    [(s-if-else s if-bs else)
     (build s_if_else (tp-loc s) (map tp-if-branch if-bs) (tp else))]

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

    [(s-obj s fields)
     (build s_obj (tp-loc s) (map tp-member fields))]

    [(s-list s elts)
     (build s_list (tp-loc s) (map tp elts))]

    [(s-dot s val field)
     (build s_dot
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

    [(s-num s n) (build s_num (tp-loc s) n)]
    [(s-str s str) (build s_str (tp-loc s) str)]
    [(s-bool s b) (build s_bool (tp-loc s) b)]
    [(s-id s x) (build s_id (tp-loc s) (symbol->string x))]))
    
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
    [(a-app s name parameters)
     (build a_app (tp-loc s) (symbol->string name) (map tp-ann parameters))]
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

(define (pyret-pair-from-string str src options)
  (define ast (parse-pyret (string-append " " str) src))
  (define check-mode? (ffi-unwrap (p:get-field p:dummy-loc options "check")))
  (define desugar
    (cond
      [check-mode? (lambda (e) (desugar-pyret (desugar-check e)))]
      [else desugar-pyret]))
  (p:mk-object
    (make-string-map
      (list
        (cons "pre-desugar" (to-pyret ast))
        (cons "post-desugar" (to-pyret (desugar ast)))))))

(define export
  (p:extend
    p:dummy-loc
    ast
    (list
      (cons "parse"
            (ffi-wrap pyret-pair-from-string)))))

(provide (rename-out [export %PYRET-PROVIDE]))

