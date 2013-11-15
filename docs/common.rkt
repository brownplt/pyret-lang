#lang racket/base

(require
  (only-in racket collection-path)
  racket/list
  racket/file
  racket/bool
  scribble/core
  scribble/manual
  (only-in racket/string string-join)
  pyret/lang/load
  pyret/lang/runtime
  pyret/lang/ffi-helpers
  pyret/lang/ast
  pyret/lang/desugar-check
  racket/match
  (rename-in "renderer.arr" (%PYRET-PROVIDE renderer)))
(provide (all-defined-out) parse-pyret)

(define (get-pyret-lib lib-name)
  (parse-pyret (file->string (collection-file-path lib-name "pyret"))))

(define (prod . word)
 (apply tt word))
(define (file . name)
 (apply tt name))
(define (in-code . code)
 (apply tt code))

(define (pretty ast)
 (define lines 
   (ffi-unwrap ((p:p-base-app (p:get-raw-field p:dummy-loc renderer "get-pretty-str"))
         (p:p-opaque ast))))
 (apply pyret-lines lines))
(define (get-decl ast name)
 (define (name-matches? sym) (equal? sym name))
 (match ast
   [(s-prog _ _ (s-block _ stmts))
    (define result (findf (lambda (s)
     (match s
       [(s-fun _ (? name-matches? test-name) _ _ _ _ _ _) s]
       [(s-data _ (? name-matches? test-name) _ _ _ _ _) s]
       [_ #f])) stmts))
    (when (false? result)
      (error (format "Declaration not found: ~a" name)))
    result]))

(define (label name)
 (toc-target-element #f (bold name) (list (string->symbol name) name)))

(define (get-method-name m)
  (match m
    [(s-method-field l (s-str _ name) _ _ _ _ _) name]))

(define (pretty-fun fun)
 (match fun
   [(s-fun loc name params args ann doc _ check)
    (pretty (s-fun loc name params args ann "" (s-block loc '()) check))]))
(define (label-fun fun (prefix ""))
 (match fun
   [(s-fun loc name params args ann doc _ check)
    (toc-target-element #f (bold (string-append prefix (symbol->string name))) (list name (symbol->string name)))]))

(define (pretty-functions block names)
  (flatten (for/list ((name names))
    (define f (get-decl block name))
    (list (label-fun f)
          (nested (pretty-fun f)
                  (para (s-fun-doc f)))))))

(define (pretty-data data)
 (define (simplify-variant v)
   (match v
     [(s-singleton-variant loc name with) (s-singleton-variant loc name empty)]
     [(s-variant loc name args with) (s-variant loc name args empty)]))
 (match data
   [(s-data loc name params mixins variants _ _)
    (pretty (s-data loc name params mixins (map simplify-variant variants) empty (s-block loc empty)))]))

(define (get-all-methods data variant-name)
 (match data
   [(s-data loc name params mixins variants _ _)
    (define with-members (filter-map (lambda (v) (variant-matches v variant-name)) variants))
    (when (< (length with-members) 1)
     (error "No such variant: ~a\n" variant-name))
    (first with-members)]))
 
(define (variant-matches v variant-name)
 (match v
   [(s-singleton-variant loc name with)
    (if (equal? name variant-name) with #f)]
   [(s-variant loc name args with)
    (if (equal? name variant-name) with #f)]
   [_ #f]))
(define (get-method data variant-name method-name)
 (define (member-matches v)
   (match v
     [(s-method-field loc (s-str _ name) args ann doc body check)
      (if (equal? name method-name)
          (s-method loc args ann doc body check)
          #f)]
     [_ #f]))
 (match data
   [(s-data loc name params mixins variants sharing _)
    (define with-members (filter-map (lambda (v) (variant-matches v variant-name)) variants))
    (when (< (length with-members) 1)
     (error "No such variant: ~a\n" variant-name))
    (define search-members (append (first with-members) sharing))
    (define method-fields (filter-map member-matches search-members))
    (when (< (length method-fields) 1)
     (error (format "No such field: ~a ~a\n" method-name search-members)))
    (first method-fields)]))

(define (pretty-method method-field)
 (match method-field
  [(s-method loc args ann doc body check)
   (pretty (s-method loc args ann "" (s-block loc empty) check))]
  [(s-method-field loc name args ann doc body check)
   (pretty (s-method-field loc name args ann "" (s-block loc empty) check))]
  [_ (error (format "Not a method: ~a\n" method-field))]))

(define (pretty-method-with-doc method-field)
 (match method-field
  [(s-method loc args ann doc body check)
   (pretty (s-method loc args ann doc (s-block loc empty) check))]
  [(s-method-field loc name args ann doc body check)
   (pretty (s-method-field loc name args ann doc (s-block loc empty) check))]
  [_ (error (format "Not a method: ~a\n" method-field))]))
 
(define (label-data data (prefix ""))
 (match data
   [(s-data loc name _ _ _ _ _)
    (toc-target-element #f (bold (string-append prefix (symbol->string name))) (list name (symbol->string name)))]))

(define (pyret-lines . stx)
 (nested #:style 'code-inset
  (verbatim (string-join stx "\n"))))

(define (joincode . stx)
 (nested #:style 'code-inset
  (verbatim (string-join stx "\n"))))
(define (justcode . stx)
 (nested #:style 'code-inset
  (verbatim (string-join stx ""))))

(define (subsection-title title tag)
  (toc-target-element #f title (make-section-tag tag)))
  
(define (method-doc-list decl prefix constructor names)
  (flatten (for/list ((name names))
    (list
      (label (string-append prefix name))
      (pretty-method (get-method decl constructor name))
      (para (s-method-doc (get-method decl constructor name)))))))

(define moorings-ast (get-pyret-lib "lang/pyret-lib/moorings.arr"))

