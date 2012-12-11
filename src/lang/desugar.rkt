#lang racket

(provide
  desugar-pyret)
(require
  "ast.rkt")

(define (variant-defs/list super-brand variants)
  (define (make-checker-name s)
    (string->symbol (string-append "is-" (symbol->string s))))
  (define (member->field m val)
    (s-field (s-member-syntax m)
             (symbol->string (s-member-name m))
             val))
  (define (apply-brand s brander-name arg)
    (s-app s (s-dot s (s-id s brander-name) 'brand) (list arg)))
  (define (variant-defs v)
    (match v
      [(s-variant s name members)
       (define brander-name (gensym name))
       (define args (map s-member-name members))
       ;; TODO(joe): annotations on args
       (define constructor-args
        (map (lambda (id) (s-bind s id (a-blank))) args))
       (define obj
        (s-obj s (map member->field
                      members
                      (map (lambda (id) (s-id s id)) args))))
       (s-block s
         (list 
           (s-def s (s-bind s brander-name (a-blank))
                    (s-app s (s-id s 'brander) (list)))
           (s-fun s (make-checker-name name)
                    (list (s-bind s 'specimen (a-any)))
                    (a-blank)
                    (s-block s
                     (list
                      (s-app s (s-dot s (s-id s brander-name) 'check)
                               (list (s-id s 'specimen))))))
           (s-fun s name
                    constructor-args
                    (a-blank)
                    (s-block s
                     (list
                      (apply-brand s super-brand
                       (apply-brand s brander-name
                        obj)))))))]))
  (map variant-defs variants))

(define (desugar-pyret ast)
  (define ds desugar-pyret)
  (define (ds-member ast-node)
    (match ast-node
      [(s-field s name value) (s-field s name (ds value))]
      [(s-method s name args body) (s-method s name args (ds body))]))
  (match ast
    [(s-block s stmts)
     (s-block s (map ds stmts))]
    ;; NOTE(joe): generative...
    [(s-data s name params variants)
     (define brander-name (gensym name))
     (ds (s-block s
                  (cons
                   (s-def s (s-bind s brander-name (a-blank))
                          (s-app s (s-id s 'brander) (list)))
                   (variant-defs/list brander-name variants))))]
    [(s-do s fun args)
     (define (functionize b)
       (s-lam s (list) (a-blank) (ds b)))
     (s-app s fun (map functionize args))]
    [(s-fun s name args ann body)
     (s-def s
            (s-bind s name (a-arrow s (map s-bind-ann args) ann))
            (s-lam s args ann (ds body)))]

    [(s-lam s args ann body)
     (s-lam s args ann (ds body))]
    
    [(s-cond s c-bs)
     (define (ds-cond branch)
       (match branch
         [(s-cond-branch s tst blk) (s-cond-branch s (ds tst) (ds blk))]))
     (s-cond s (map ds-cond c-bs))]

    [(s-assign s name expr) (s-assign s name (ds expr))]

    [(s-app s fun args) (s-app s (ds fun) (map ds args))]

    [(s-onion s super fields) (s-onion s (ds super) (map ds-member fields))]

    [(s-obj s fields) (s-obj s (map ds-member fields))]
    
    [(s-list s elts) (s-list s (map ds elts))]
    
    [(s-dot s val field) (s-bracket s (ds val) (s-str s (symbol->string field)))]
    
    [(s-bracket s val field) (s-bracket s (ds val) (ds field))]
    
    [(s-dot-assign s obj field val) (s-dot-assign s (ds obj) field (ds val))]

    [(s-dot-method s obj field args) (s-dot-method s (ds obj) field (map ds args))]
    [else ast]))
