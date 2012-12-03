#lang racket

(require racket/match racket/splicing)
(require "ast.rkt" "runtime.rkt")
(provide compile-pyret)


(define (compile-pyret ast-node)
  (define (d->stx stx) (datum->syntax #f stx))
  (define (compile-member ast-node)
    (match ast-node
      [(s-field _ name value)
       (with-syntax ([name-stx (d->stx name)]
                     [val-stx (compile-pyret value)]) 
         #'(cons name-stx val-stx))]))
  (match ast-node

    [(s-num _ n) #`(mk-num #,(d->stx n))]
    [(s-bool _ b) #`(mk-bool #,(d->stx b))]
    [(s-str _ s) #`(mk-str #,(d->stx s))]

    [(s-block _ l)
     (with-syntax ([(stmt ...) (map compile-pyret l)])
       #`(begin stmt ...))]

    [(s-fun _ name args ann body)
     (with-syntax ([name-stx (d->stx name)]
                   [(arg ...) (d->stx (map compile-pyret args))]
                   [body-stx (compile-pyret body)])
       #`(define name-stx (mk-fun (lambda (arg ...) body-stx))))]

    [(s-def _ bind val)
     #`(define #,(compile-pyret bind) #,(compile-pyret val))]
    
    [(s-cond _ c-bs)
     (with-syntax ([(branch ...) (d->stx (map compile-pyret c-bs))])
       #`(cond branch ...))]
    
    [(s-cond-branch _ tst blk)
     #`(#,(compile-pyret tst) #,(compile-pyret blk))]

    [(s-bind _ name ann)
     (with-syntax ([name-stx (d->stx name)])
       #`name-stx)]
    
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

    [(s-onion _ super fields)
     (with-syntax ([(member ...) (map compile-member fields)]
                   [super (compile-pyret super)])
      #'(flatten super (make-hash (list member ...))))]

    [(s-obj _ fields)
     (with-syntax ([(member ...) (map compile-member fields)])
       #'(mk-object (make-hash (list member ...))))]
    
    [(s-list _ elts)
     (with-syntax ([(elt ...) (map compile-pyret elts)])
       #'(mk-list (list elt ...)))]
    
    [(s-dot _ val field)
     #`(get-field #,(compile-pyret val) #,(d->stx (symbol->string field)))]

    [(s-dot-assign _ obj field val)
     #`(set-field #,(compile-pyret obj)
                  #,(d->stx (symbol->string field))
                  #,(compile-pyret val))]

    [(s-dot-method _ obj field args)
     (with-syntax ([(arg ...) (map compile-pyret args)])
       #`(let ((^obj #,(compile-pyret obj)))
          (apply (p-fun-f (get-field ^obj #,(d->stx (symbol->string field))))
                 (list ^obj arg ...))))]

    [else (error (format "Missed a case: ~a" ast-node))]))

