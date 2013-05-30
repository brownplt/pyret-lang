#lang racket/base

(define (desugar-check/ann ast)
  (match ast
    [(pred-ann s e) (desugar-check e)]
    [_ ast]))

(struct check-info (srcloc name check-body))

(define (get-checks stmts)
  (define (get-check stmt)
    (match stmt
      [(s-fun s name _ _ _ _ _ check)
       (check-info s name check)]
      [_ 

(define (desugar-check ast)
  (define ds desugar-check)
  (match ast
    [(s-block s stmts)
     (define checks-to-perform (get-checks stmts))
     (define ds-stmts (map ds stmts))
     (s-block s (append stmts (create-checks checks-to-perform)))] 
    
    ...))
