#lang racket

(require "ast.rkt")
(provide desugar-pyret)

(define (variant-defs/list super-brand variants)
  (define (make-checker-name s)
    (string->symbol (string-append "is_" (symbol->string s))))
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
  (match ast
    [(s-block s stmts)
     (s-block s (map desugar-pyret stmts))]
    [(s-data s name variants)
     (define brander-name (gensym name))
     (s-block s
       (cons
         (s-def s (s-bind s brander-name (a-blank))
                  (s-app s (s-id s 'brander) (list)))
         (variant-defs/list brander-name variants)))]
    [else ast]))
