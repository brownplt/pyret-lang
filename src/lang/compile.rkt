#lang racket

(require racket/match)
(require "ast.rkt" "runtime.rkt")
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

    [(s-num _ n) #`(mk-num #,(d->stx n))]
    [(s-bool _ b) #`(mk-bool #,(d->stx b))]
    [(s-str _ s) #`(mk-str #,(d->stx s))]

    [(s-block _ l)
     (with-syntax ([(stmt ...) (map compile-pyret l)])
       #`(let () stmt ...))]

    [(s-fun _ name args body)
     (with-syntax ([name-stx (d->stx name)]
                   [(arg ...) (d->stx args)]
                   [body-stx (compile-pyret body)])
       #`(define name-stx (mk-fun (lambda (arg ...) body-stx))))]

    [(s-def _ name val)
     #`(define #,(d->stx name) #,(compile-pyret val))]

    [(s-id _ name)
     (with-syntax ([name-stx (d->stx name)])
       #`name-stx)]
    
    [(s-assign _ name expr)
     (with-syntax ([name-stx (d->stx name)])
       #`(set! name-stx #,(compile-pyret expr)))]

    [(s-app _ fun args)
     (with-syntax ([fun (compile-pyret fun)]
                   [(arg ...) (map compile-pyret args)])
       #'((p-fun-f fun) arg ...))]

    [(s-obj _ fields)
     (with-syntax ([(member ...) (map compile-member fields)])
       #'(mk-object (make-hash (list member ...))))]
    
    [(s-list _ elts)
     (with-syntax ([(elt ...) (map compile-pyret elts)])
       #'(mk-list (list elt ...)))]
    
    [(s-dot _ val field)
     #`(get-field #,(compile-pyret val) #,(d->stx (symbol->string field)))]

    [else (error (format "Missed a case: ~a" ast-node))]))

