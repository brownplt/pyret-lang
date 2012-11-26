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

    [(s-num _ n) #`(p-num #,(d->stx n) (none) (set) (make-hash))]
    [(s-bool _ b) #`(p-bool #,(d->stx b) (none) (set) (make-hash))]
    [(s-str _ s) #`(p-str #,(d->stx s) (none) (set) (make-hash))]

    [(s-block _ l)
     (with-syntax ([(stmt ...) (map compile-pyret l)])
       #`(begin stmt ...))]

    [(s-fun _ name args body)
     (with-syntax ([name-stx (d->stx name)]
                   [(arg ...) (d->stx args)]
                   [body-stx (compile-pyret body)])
       #`(define name-stx (p-fun (lambda (arg ...) body-stx) (none) (set) (make-hash))))]

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
       #'(p-object (none) (set) (make-hash (list member ...))))]
    
    [(s-list _ elts)
     (with-syntax ([(elt ...) (map compile-pyret elts)])
       #'(p-list (list elt ...) (none) (set) (make-hash)))]
    
    [(s-dot _ val field)
     #`(get-field #,(compile-pyret val) #,(d->stx (symbol->string field)))]

    [else (error (format "Missed a case: ~a" ast-node))]))

