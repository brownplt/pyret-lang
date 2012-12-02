#lang racket

(require
  redex/reduction-semantics
  "../lang/ast.rkt")

(provide redex-compile-pyret)

(define (redex-compile-pyret ast-node)
  (define rcp redex-compile-pyret)
  (define (redex-compile-member ast-node)
    (match ast-node
      [(s-data _ name value) (term (,name ,(rcp value)))]))
  (match ast-node
    [(s-num _ n) (term ,n)]
    [(s-str _ s) (term ,s)]
    [(s-block _ b)
     (term-let ([(e ...) (map rcp b)])
               (term (seq e ...)))]
    [(s-obj _ fields)
     (term (object ,(map redex-compile-member fields)))]
    [(s-dot _ object field)
     (term (get-field ,(rcp object) ,(symbol->string field)))]
    [_ (error (format "redex-compile: Haven't handled a case yet: ~a"
                      ast-node))]))

