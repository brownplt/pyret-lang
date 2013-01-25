#lang racket

(provide
  desugar-pyret
  desugar-pyret/libs)
(require
  racket/runtime-path
  "ast.rkt"
  "load.rkt")

;; internal name
(define (make-checker-type-name s)
    (string->symbol (string-append (symbol->string s) "?")))
;; user-visible name
(define (make-checker-name s)
    (string->symbol (string-append "is-" (symbol->string s))))

(define (make-checker s name brander)
  (s-block
   s
   (list
    (s-fun s (make-checker-name name) (list) 
           (list (s-bind s 'specimen (a-any)))
           (a-blank)
           (format
            "~a: This function checks that its argument is an
             instance of the ~a type."
            (symbol->string (make-checker-name name))
            (symbol->string name))
           (s-block s
                    (list
                     (s-app s (s-dot s brander 'check)
                            (list (s-id s 'specimen))))))
    (s-var s
           (s-bind s (make-checker-type-name name) (a-blank))
           (s-id s (make-checker-name name))))))
           
(define (variant-defs/list super-brand super-fields variants)
  (define (member->field m val)
    (s-data-field (s-member-syntax m)
             (symbol->string (s-member-name m))
             val))
  (define (apply-brand s brander-name arg)
    (s-app s (s-dot s (s-id s brander-name) 'brand) (list arg)))
  (define (variant-defs v)
    (match v
      [(s-variant s name members with-members)
       (define brander-name (gensym name))
       (define dsg-with-members (map ds-member with-members))
       (define args (map s-member-name members))
       ;; TODO(joe): annotations on args
       (define constructor-args
        (map (lambda (id) (s-bind s id (a-blank))) args))
       (define obj
         (s-onion s
                  (s-obj s (map member->field
                                members
                                (map (lambda (id) (s-id s id)) args)))
                  (append super-fields dsg-with-members)))
       (s-block s
         (list 
           (s-var s (s-bind s brander-name (a-blank))
                    (s-app s (s-id s 'brander) (list)))
           (make-checker s name (s-id s brander-name))
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
                        obj)))))))]))
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
       (s-data-field s name (desugar-internal value))]
      [(s-method-field s name args ann body)
       (s-data-field s name (s-method s args ann (desugar-internal body)))]))

(define (desugar-internal ast)
  (define ds desugar-internal) 
  (match ast
    [(s-block s stmts)
     (s-block s (flatten-blocks (map ds stmts)))]
    ;; NOTE(joe): generative...
    [(s-data s name params variants share-members)
     (define brander-name (gensym name))
     (define super-fields (map ds-member share-members))
     (ds (s-block s
                  (append
                   (list (s-var s (s-bind s brander-name (a-blank))
                                (s-app s (s-id s 'brander) (list)))
                         (make-checker s name (s-id s brander-name)))
                   (variant-defs/list brander-name super-fields variants))))]
    [(s-do s fun args)
     (define (functionize b)
       (s-lam s (list) (list) (a-blank) "" (ds b)))
     (s-app s fun (map functionize args))]
        
    [(s-var s name val)
     (s-var s name (ds val))]

    [(s-fun s name typarams args ann doc body)
     (s-var s
            (s-bind s name (a-arrow s (map s-bind-ann args) ann))
            (s-lam s typarams args ann doc (ds body)))]

    [(s-lam s typarams args ann doc body)
     (s-lam s typarams args ann doc (ds body))]
    
    [(s-method s args ann body)
     (s-method s args ann (ds body))]
    
    [(s-cond s c-bs)
     (define (ds-cond branch)
       (match branch
         [(s-cond-branch s tst blk) (s-cond-branch s (ds tst) (ds blk))]))
     (define cond-fallthrough
       (s-block s
                (list
                 (s-app s
                        (s-id s 'raise)
                        (list (s-str s "cond: no cases matched"))))))
     (s-cond s
             (append (map ds-cond c-bs)
                     (list (s-cond-branch s (s-bool s #t) cond-fallthrough))))]

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
      
     (foldr make-link
            (s-app s (get-lib "empty") (list))
            (map ds elts))]
    
    [(s-dot s val field) (s-bracket s (ds val) (s-str s (symbol->string field)))]
    
    [(s-bracket s val field) (s-bracket s (ds val) (ds field))]
    
    [(s-dot-method s obj field) (s-dot-method s (ds obj) field)]
    
    [(or (s-num _ _)
         (s-bool _ _)
         (s-str _ _)
         (s-id _ _)) ast]
    
    [else (error (format "Missed a case in desugaring: ~a" ast))]))

(define (desugar-pyret/libs ast)
  (match ast
    [(s-prog s imps block)
     (define imps-with-libs (append (get-prelude s) imps))
     (desugar-pyret (s-prog s imps-with-libs block))]))

(define (desugar-pyret ast)
  (match ast
    [(s-prog s imps block)
     (s-block s (flatten-blocks
                  (append (map (compose desugar-internal desugar-header)
                               imps)
                          (map desugar-internal (s-block-stmts block)))))]))

(define (get-prelude s)
  (define (mk-import p)
    (define-values (_ filename __) (split-path p))
    (define modname (string->symbol
                     (first
                      (string-split
                       (path->string filename) "."))))
    (s-import s (path->string (path->complete-path p)) modname))
  (map mk-import libs))

(define-runtime-path-list libs
  '(
    "pyret-lib/list.arr"
    "pyret-lib/builtins.arr"
   ))

(define (desugar-header hd)
  (define (desugar-module ast)
    (match ast
      [(s-prog s imps block)
        (match (desugar-pyret ast)
          [(s-block s stmts)
            (s-block s (append stmts (list (s-app s (s-id s '%provide) (list)))))])]))
  (match hd
    [(s-provide s exp)
      (s-fun s '%provide (list) (list) (a-blank) "" (s-block s (list exp)))]
    [(s-import s file name)
     (define full-path (path->complete-path file))
     (define-values (base relative-file root?) (split-path full-path))
     (define mod-ast (parse-pyret (file->string full-path)))
     (parameterize [(current-directory base)]
       (s-var s (s-bind s name (a-blank)) (desugar-module mod-ast)))]))

