#lang racket

(provide
  compile-pyret
  compile-expr)
(require
  racket/match
  racket/splicing
  racket/syntax
  "ast.rkt"
  "runtime.rkt")

(define (loc-list loc)
  (define (serialize-source e)
    (cond
      [(symbol? e) (symbol->string e)]
      [(string? e) e]
      [(path? e) (path->string e)]
      [(false? e) "unknown source"]
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
    [(s-let s (s-bind _ id _) val)
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
         #`(p:mk-fun #,(attach l #'(r:λ (arg ...) body-stx)) #,doc)))]
    
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

    [(s-try l try (s-bind l2 id ann) catch)
     (attach l
       #`(r:with-handlers
            ([p:exn:fail:pyret?
              (r:lambda (%exn)
		 (r:define #,(d->stx id l2) (p:mk-exn %exn))
		 #,(compile-expr catch))])
             #,(compile-expr try)))]

    [(s-id l name)
     (attach l
       (with-syntax ([name-stx (d->stx name l)])
         #'name-stx))]

    [(s-assign l name expr)
     (attach l
       (with-syntax ([name-stx (d->stx name l)]
                     [temp (gensym name)])
         #`(r:let [(temp #,(compile-expr expr))]
             (r:set! name-stx temp)
             temp)))]

    [(s-app l (s-bracket l2 obj field) args)
        (with-syntax ([obj (compile-expr obj)]
                      [field (match field
                                [(s-str _ s) (d->stx s l)]
                                [else #'(p:p-str-s #,(compile-expr field))])]
                      [(arg ...) (map compile-expr args)]
                      [(argid ...) (map (λ (_) (format-id #'obj "~a" #`#,(gensym 'arg))) args)]
              	      [(loc-param ...) (loc-list l)])
          #'(r:let* ([%obj obj]
                     [%field (p:get-raw-field (r:list loc-param ...) %obj field)]
                     [argid arg] ...)
              (r:cond
               [(p:p-method? %field) ((p:p-method-f %field) %obj argid ...)]
               [else ((p:check-fun %field (r:list loc-param ...)) argid ...)])))]


    [(s-app l fun args)
     (attach l
        (with-syntax ([fun (compile-expr fun)]
                      [(arg ...) (map compile-expr args)]
		      [(loc-param ...) (loc-list l)])
          #'((p:check-fun fun (r:list loc-param ...)) arg ...)))]

    [(s-obj l fields)
     (attach l
       (with-syntax ([(member ...) (map compile-member fields)])
         #'(p:mk-object (r:make-immutable-hash (r:list member ...)))))]
    
    [(s-onion l super fields)
     (attach l
      (with-syntax
		    ([(loc-param ...) (loc-list l)])
       (with-syntax ([(member ...) (map compile-member fields)]
                     [super (compile-expr super)])
        #'(p:extend (r:list loc-param ...)
                    super
                    (r:make-hash (r:list member ...))))))]
    
    [(s-bracket l val field)
     (attach l
      (with-syntax
         ([field (match field
                 [(s-str _ s) (d->stx s l)]
                 [else #'(p:p-str-s #,(compile-expr field))])]
		      [(loc-param ...) (loc-list l)])
       #`(p:get-field (r:list loc-param ...) #,(compile-expr val) field)))]
    
    [(s-bracket-method l obj field)
     (attach l
      (with-syntax
		    ([(loc-param ...) (loc-list l)])
       #`(p:get-raw-field (r:list loc-param ...)
                          #,(compile-expr obj)
                          (p:p-str-s #,(compile-expr field)))))]

    [(s-prog l headers block) (compile-prog l headers block)]

    [else (error (format "Missed a case in compile: ~a" ast-node))]))

(define (compile-prog l headers block)
  (attach l
   (with-syntax ([(req ...) (map compile-header (filter s-import? headers))]
                 [(prov ...) (map compile-header (filter s-provide? headers))])
     #`(r:begin req ... #,(compile-pyret block) prov ...))))

(define (compile-header header)
  (match header
    [(s-import l file name)
     (attach l
       (with-syntax
        ([file-stx file])
       (with-syntax
         ([name-stx name]
          [req-stx (if (relative-path? file) #'file-stx #'(r:file file-stx))])
        #`(r:require (r:rename-in req-stx [%PYRET-PROVIDE name-stx])))))]

    [(s-provide l exp)
     (attach l
      (with-syntax [(temp-stx (gensym 'module-provide))]
        #`(r:begin
            (r:define temp-stx #,(compile-expr exp))
            (r:provide (r:rename-out [temp-stx %PYRET-PROVIDE])))))]))

(define (compile-pyret ast)
  (match ast
    [(s-prog l headers block) (compile-prog l headers block)]
    [(s-block l stmts)
     (define (compile-top-stmt stmt)
      (match stmt
        [(s-var s (s-bind _ id _) val)
         (with-syntax ([id-stx id])
          #`(r:define id-stx #,(compile-expr val)))]
        ;; TODO(joe): Can set! immutable vars at the REPL
        [(s-let s (s-bind _ id _) val)
         (with-syntax ([id-stx id])
          #`(r:define id-stx #,(compile-expr val)))]
        [else (compile-expr stmt)]))
     (with-syntax ([(expr ...) (map compile-top-stmt stmts)])
     (attach l #`(r:begin expr ...)))]
    [else (error (format "Didn't match a case in compile-pyret: ~a" ast))]))

