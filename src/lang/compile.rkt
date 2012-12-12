#lang racket

(provide
  compile-pyret)
(require
  racket/match
  racket/splicing
  "ast.rkt"
  "runtime.rkt")

(define (d->stx stx) (datum->syntax #f stx))

;; Stmt -> letrec-clause
;; Does special work for creating def bindings, others get gensymed names.
(define (compile-stmt ast-node)
  (match ast-node
    [(s-def s (s-bind _ id _) val)
       (cons id (compile-expr val))]
    [_ (cons (gensym) (compile-expr ast-node))]))

(define (compile-expr ast-node)
  (define (compile-member ast-node)
    (match ast-node
      [(s-field _ name value)
       (with-syntax ([name-stx (d->stx name)]
                     [val-stx (compile-pyret value)]) 
         #'(r:cons name-stx val-stx))]
      [(s-method _ name args body)
       (with-syntax ([name-stx (d->stx name)]
                     [(arg ...) (d->stx (map s-bind-id args))]
                     [body-stx (compile-pyret body)]) 
         #'(r:cons name-stx
                 (p:mk-method (r:λ (arg ...) body-stx))))]))
  (match ast-node
    
    [(s-block _ l)
     (define id-expr-pairs (map compile-stmt l))
     (define ids (map car id-expr-pairs))
     (with-syntax ([(id ...) ids]
                   [body-id (d->stx (if (cons? ids) (last ids) 'nothing))]
                   [(expr ...) (map cdr id-expr-pairs)])
       #`(r:letrec [(id expr) ...] body-id))]

    [(s-num _ n) #`(p:mk-num #,(d->stx n))]
    [(s-bool _ b) #`(p:mk-bool #,(d->stx b))]
    [(s-str _ s) #`(p:mk-str #,(d->stx s))]

    [(s-lam _ args ann body)
     (with-syntax ([(arg ...) (d->stx (map s-bind-id args))]
                   [body-stx (compile-pyret body)])
       #`(p:mk-fun (r:lambda (arg ...) body-stx)))]
    
    [(s-cond _ c-bs)
     (with-syntax ([(branch ...) (d->stx (map compile-pyret c-bs))])
       #`(r:cond branch ... [r:else (r:error "cond: no cases matched")]))]
    
    [(s-cond-branch _ tst blk)
     #`((p:pyret-true? #,(compile-pyret tst)) #,(compile-pyret blk))]
    
    [(s-id _ name)
     (with-syntax ([name-stx (d->stx name)])
       #`name-stx)]
    
    [(s-assign _ name expr)
     (with-syntax ([name-stx (d->stx name)]
                   [temp (gensym name)])
       #`(r:let [(temp #,(compile-pyret expr))]
           (r:set! name-stx temp)
           temp))]

    [(s-app _ fun args)
     (with-syntax ([fun (compile-pyret fun)]
                   [(arg ...) (map compile-pyret args)])
       #'((p:p-fun-f fun) arg ...))]

    [(s-onion _ super fields)
     (with-syntax ([(member ...) (map compile-member fields)]
                   [super (compile-pyret super)])
      #'(p:flatten super (r:make-hash (r:list member ...))))]

    [(s-obj _ fields)
     (with-syntax ([(member ...) (map compile-member fields)])
       #'(p:mk-object (r:make-hash (r:list member ...))))]
    
    [(s-list _ elts)
     (with-syntax ([(elt ...) (map compile-pyret elts)])
       #'(p:mk-list (r:list elt ...)))]
    
    [(s-dot _ val field)
     #`(p:get-field #,(compile-pyret val) #,(d->stx (symbol->string field)))]
    
    [(s-bracket _ val field)
     #`(p:get-field #,(compile-pyret val) (p:p-str-s #,(compile-pyret field)))]
    
    [(s-dot-assign _ obj field val)
     #`(p:set-field #,(compile-pyret obj)
                    #,(d->stx (symbol->string field))
                    #,(compile-pyret val))]

    [(s-dot-method _ obj field args)
     (with-syntax ([(arg ...) (map compile-pyret args)])
       #`(r:let ((^obj #,(compile-pyret obj)))
                (r:apply (p:p-fun-f (p:get-field ^obj #,(d->stx (symbol->string field))))
                   (r:list ^obj arg ...))))]

    [else (error (format "Missed a case in compile: ~a" ast-node))]))

(define compile-pyret compile-expr)
