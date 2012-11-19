#lang racket

(require racket/match)
(require "ast.rkt" "../values.rkt")
(provide compile-pyret)

(define (compile-pyret ast-node)
  (define (d->stx stx) (datum->syntax #'compile-pyret stx))
  (define (compile-member ast-node)
    (match ast-node
      [(s-data _ name value)
       (with-syntax ([name-stx (d->stx name)]
                     [val-stx (compile-pyret value)]) 
         #'(cons name-stx val-stx))]))
  (match ast-node

    [(s-num _ n) #`(p-num #,(d->stx n) (none) (make-hash))]
    [(s-str _ s) #`(p-str #,(d->stx s) (none) (make-hash))]

    [(s-block _ l)
     (with-syntax ([(stmt ...) (map compile-pyret l)])
       #`(begin stmt ...))]

    [(s-fun _ name args body)
     (with-syntax ([name-stx (datum->syntax #'#%module-begin name)]
                   [(arg ...) (datum->syntax #'#%module-begin args)]
                   [body-stx (compile-pyret body)])
       #`(define (name-stx arg ...) body-stx))]

    [(s-id _ name)
     (with-syntax ([name-stx (datum->syntax #'#%module-begin name)])
       #`name-stx)]

    [(s-app _ fun args)
     (with-syntax ([fun (compile-pyret fun)]
                   [(arg ...) (map compile-pyret args)])
       #'(fun arg ...))]

    [(s-obj _ fields)
     (with-syntax ([(member ...) (map compile-member fields)])
       #'(p-object (none) (make-hash (list member ...))))]

    [(s-list _ elts)
     (with-syntax ([(elt ...) (map compile-pyret elts)])
       #'(p-list (list elt ...) (none) (make-hash)))] 

    [else (error (format "Missed a case: ~a" ast-node))]))

