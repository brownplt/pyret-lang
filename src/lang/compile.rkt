#lang racket

(provide
  compile-pyret
  compile-expr)
(require
  racket/match
  racket/splicing
  "ast.rkt"
  "runtime.rkt")

(define (loc-list loc)
  (define (serialize-source e)
    (cond
      [(symbol? e) (symbol->string e)]
      [(string? e) e]
      [(path? e) e]
      [else (error (format "Non-symbol, non-string, non-path value for
                            source: ~a" e))]))
  (list (serialize-source (srcloc-source loc))
        (srcloc-line loc)
        (srcloc-column loc)
        (srcloc-position loc)
        (srcloc-span loc)))

(define (d->stx stx loc) (datum->syntax #f stx (loc-list loc)))

(define (attach loc stx)
  (datum->syntax #f (syntax-e stx) (loc-list loc)))

;; Stmt -> letrec-clause
;; Does special work for creating def bindings, others get gensymed names.
(define (compile-stmt ast-node)
  (match ast-node
    [(s-var s (s-bind _ id _) val)
       (cons id (compile-expr val))]
    [_ (cons (gensym) (compile-expr ast-node))]))

(define (compile-expr ast-node)
  (define (compile-member ast-node)
    (match ast-node
      [(s-data-field l name value)
       (attach l
         (with-syntax ([name-stx (compile-expr name)]
                       [val-stx (compile-expr value)]) 
           #'(r:cons (p:p-str-s name-stx) val-stx)))]))
  (match ast-node
    
    [(s-block l stmts)
     (define id-expr-pairs (map compile-stmt stmts))
     (define ids (map car id-expr-pairs))
     (with-syntax ([(id ...) ids]
                   [body-id (d->stx (if (cons? ids) (last ids) 'nothing) l)]
                   [(expr ...) (map cdr id-expr-pairs)])
      (attach l
       #`(r:letrec [(id expr) ...] body-id)))]

    [(s-num l n) #`(p:mk-num #,(d->stx n l))]
    [(s-bool l b) #`(p:mk-bool #,(d->stx b l))]
    [(s-str l s) #`(p:mk-str #,(d->stx s l))]

    [(s-lam l params args ann doc body)
     (attach l
       (with-syntax ([(arg ...) (d->stx (map s-bind-id args) l)]
                     [body-stx (compile-expr body)])
         #`(p:mk-fun (r:λ (arg ...) body-stx) #,doc)))]
    
    [(s-method l args ann body)
     (attach l
       (with-syntax ([(arg ...) (d->stx (map s-bind-id args) l)]
                     [body-stx (compile-expr body)]) 
         #'(p:mk-method (r:λ (arg ...) body-stx))))]
    
    [(s-cond l c-bs)
     (define (compile-cond-branch b)
       (match b
         [(s-cond-branch s test block)
          (attach l
                  #`((p:pyret-true? #,(compile-expr test)) #,(compile-expr block)))]))
     (attach l
       (with-syntax ([(branch ...) (d->stx (map compile-cond-branch c-bs) l)])
         #`(r:cond branch ...)))]
    
    [(s-id l name)
     (attach l
       (with-syntax ([name-stx (d->stx name l)])
         #`name-stx))]
    
    [(s-assign l name expr)
     (attach l
       (with-syntax ([name-stx (d->stx name l)]
                     [temp (gensym name)])
         #`(r:let [(temp #,(compile-expr expr))]
             (r:set! name-stx temp)
             temp)))]

    [(s-app l fun args)
     (attach l
        (with-syntax ([fun (compile-expr fun)]
                      [(arg ...) (map compile-expr args)]
		      [(loc-param ...) (loc-list l)])
          #'(((p:p-fun-f fun) (r:list loc-param ...)) arg ...)))]

    [(s-onion l super fields)
     (attach l
       (with-syntax ([(member ...) (map compile-member fields)]
                     [super (compile-expr super)])
        #'(p:flatten super (r:make-hash (r:list member ...)))))]

    [(s-obj l fields)
     (attach l
       (with-syntax ([(member ...) (map compile-member fields)])
         #'(p:mk-object (r:make-immutable-hash (r:list member ...)))))]
    
    [(s-list l elts)
     (attach l
       (with-syntax ([(elt ...) (map compile-expr elts)])
         #'(p:mk-list (r:list elt ...))))]
    
    [(s-dot l val field)
     (attach l
       #`(p:get-field #,(compile-expr val) #,(d->stx (symbol->string field) l)))]
    
    [(s-bracket l val field)
     (attach l
       #`(p:get-field #,(compile-expr val) (p:p-str-s #,(compile-expr field))))]
    
    [(s-dot-method l obj field)
     (attach l
       #`(p:get-raw-field #,(compile-expr obj) #,(d->stx (symbol->string field) l)))]

    [else (error (format "Missed a case in compile: ~a" ast-node))]))

(define (compile-pyret ast)
  (match ast
    [(s-block l stmts)
     (define id-expr-pairs (map compile-stmt stmts))
     (define ids (map car id-expr-pairs))
     (with-syntax ([(id ...) ids]
                   [body-id (d->stx (if (cons? ids) (last ids) 'nothing) l)]
                   [(expr ...) (map cdr id-expr-pairs)])
      (attach l
       #`(r:begin (r:define id expr) ... body-id)))]
    [else (error (format "Didn't match a case in compile-pyret: ~a" ast))]))

