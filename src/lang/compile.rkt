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
       (cons (discard-_ id) (compile-expr val))]
    [(s-let s (s-bind _ id _) val)
       (cons (discard-_ id) (compile-expr val))]
    [_ (cons (gensym) (compile-expr ast-node))]))

(define (compile-expr ast-node)
  (define (compile-body l body)
    (with-syntax [((loc-param ...) (loc-list l))]
      #`(r:with-continuation-mark (r:quote pyret-mark) (r:srcloc loc-param ...) #,(compile-expr body))))
  (define (compile-lookup l obj field lookup-type)
     (attach l
      (with-syntax*
         ([(loc-param ...) (loc-list l)]
          [loc #'(r:list loc-param ...)]
          [field-stx (match field
                 [(s-str _ s) (d->stx s l)]
                 [else #`(p:check-str #,(compile-expr field) loc)])]
		      )
       #`(#,lookup-type loc #,(compile-expr obj) field-stx))))
  (define (compile-member ast-node)
    (match ast-node
      [(s-data-field l name value)
       (attach l
         (with-syntax*
          ([(loc-param ...) (loc-list l)]
           [loc #'(r:list loc-param ...)]
           [name-stx (compile-expr name)]
           [val-stx (compile-expr value)]) 
           #'(r:cons (p:check-str name-stx loc) val-stx)))]))
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

    [(s-lam l params args ann doc body _)
     (attach l
       (with-syntax ([(arg ...) (d->stx (map discard-_ (map s-bind-id args)) l)]
                     [body-stx (compile-body l body)])
         #`(p:pλ (arg ...) #,doc body-stx)))]
    
    [(s-method l args ann doc body _)
     (attach l
       (with-syntax ([(arg ...) (d->stx (map discard-_ (map s-bind-id args)) l)]
                     [body-stx (compile-body l body)])
         #`(p:pμ (arg ...) #,doc body-stx)))]
    
    [(s-case l c-bs)
     (define (compile-case-branch b)
       (match b
         [(s-case-branch s test block)
          (attach l
                  #`((p:pyret-true? #,(compile-expr test)) #,(compile-expr block)))]))
     (attach l
       (with-syntax ([(branch ...) (d->stx (map compile-case-branch c-bs) l)])
         #`(r:cond branch ...)))]

    [(s-try l try (s-bind l2 id ann) catch)
     (attach l
       #`(r:with-handlers
            ([p:exn:fail:pyret?
              (r:lambda (%exn)
               (r:define #,(d->stx (discard-_ id) l2) (p:mk-exn %exn))
               #,(compile-expr catch))])
            #,(compile-expr try)))]

    [(s-id l name)
     (attach l
       (with-syntax ([name-stx (d->stx (discard-_ name) l)])
         #'name-stx))]

    [(s-assign l name expr)
     (attach l
       (with-syntax ([name-stx (d->stx name l)]
                     [temp (gensym name)])
         #`(r:let [(temp #,(compile-expr expr))]
             (r:set! name-stx temp)
             temp)))]

    [(s-app l (s-bracket l2 obj field) args)
        (with-syntax* ([obj (compile-expr obj)]
                       [(arg ...) (map compile-expr args)]
                       [(argid ...) (map (λ (_) (format-id #'obj "~a" #`#,(gensym 'arg))) args)]
              	       [(loc-param ...) (loc-list l)]
                       [loc #'(r:list loc-param ...)]
                       [field (match field
                                [(s-str _ s) (d->stx s l)]
                                [else #'(p:check-str #,(compile-expr field) loc)])])
          #'(r:let* ([%obj obj]
                     [%field (p:get-raw-field loc %obj field)]
                     [%is-method (p:p-method? %field)]
                     [%fun (r:cond
                            [%is-method (p:p-method-f %field)]
                            [else (p:check-fun %field loc)])]
                     [argid arg] ...)
              (r:cond
               [%is-method (%fun %obj argid ...)]
               [else (%fun argid ...)])))]


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
    
    [(s-extend l super fields)
     (attach l
      (with-syntax
		    ([(loc-param ...) (loc-list l)])
       (with-syntax ([(member ...) (map compile-member fields)]
                     [super (compile-expr super)])
        #'(p:extend (r:list loc-param ...)
                    super
                    (r:list member ...)))))]
    
    [(s-bracket l obj field)
     (compile-lookup l obj field #'p:get-field)]
    
    [(s-colon-bracket l obj field)
     (compile-lookup l obj field #'p:get-raw-field)]

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
         (with-syntax ([id-stx (discard-_ id)])
          #`(r:define id-stx #,(compile-expr val)))]
        ;; TODO(joe): Can set! immutable vars at the REPL
        [(s-let s (s-bind _ id _) val)
         (with-syntax ([id-stx (discard-_ id)])
          #`(r:define id-stx #,(compile-expr val)))]
        [else (compile-expr stmt)]))
     (with-syntax ([(expr ...) (map compile-top-stmt stmts)])
     (attach l #`(r:begin expr ...)))]
    [else (error (format "Didn't match a case in compile-pyret: ~a" ast))]))

(define (discard-_ name)
  (if (equal? name '_) (gensym) name))
