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

(define (loc-stx loc)
  (with-syntax ([(loc-param ...) (loc-list loc)])
    #'(r:list loc-param ...)))

(define (args-stx l args)
  (d->stx (map discard-_ (map s-bind-id args)) l))

(struct compile-env (functions-to-inline) #:transparent)

(define (d->stx stx loc) (datum->syntax #f stx (loc-list loc)))

(define (attach loc stx)
  (datum->syntax #f (syntax-e stx) (loc-list loc)))

(define (block-fun-ids stmts)
  (define (stmt-id stmt)
    (match stmt
      [(s-let _ (s-bind _ id _) (s-lam _ _ _ _ _ _ _)) id]
      [_ #f]))
  (list->set (filter-map stmt-id stmts)))

(define (block-ids stmts)
  (define (stmt-id stmt)
    (match stmt
      [(s-let _ (s-bind _ id _) _) id]
      [(s-var _ (s-bind _ id _) _) id]
      [_ #f]))
  (filter-map stmt-id stmts))

(define (make-immediate-id id)
  (string->symbol (format "~a##inline" id)))

(define (compile-block l stmts env)
  (define (compile-stmt ast-node env)
    (match ast-node
      [(s-var s (s-bind _ id _) val)
        (list 
          #`(r:define #,(discard-_ id) #,(compile-expr/internal val env)))]
      [(s-let s (s-bind _ id _) val)
       (match val
        [(s-lam l _ args _ doc body _)
         (list
          (with-syntax ([(arg ...) (args-stx l args)])
            #`(r:define #,(make-immediate-id id)
               (p:arity-catcher (arg ...) #,(compile-expr/internal body env))))
          (with-syntax ([(arg ...) (args-stx l args)]
                        [f-id (make-immediate-id id)])
            #`(r:define #,(discard-_ id) (p:pλ (arg ...) #,doc (f-id arg ...)))))]
        [_ (list #`(r:define #,(discard-_ id) #,(compile-expr/internal val env)))])]
      [_ (list (compile-expr/internal ast-node env))]))
  (define ids (block-ids stmts))
  (define fun-ids (block-fun-ids stmts))
  (define old-fun-ids (compile-env-functions-to-inline env))
  (define avoid-shadowing (set-subtract old-fun-ids (list->set ids)))
  (define new-env (compile-env (set-union avoid-shadowing fun-ids)))
  (define stmts-stx (append* (map (curryr compile-stmt new-env) stmts)))
  (if (empty? stmts-stx) (list #'nothing) stmts-stx))

(define (compile-expr/internal ast-node env)
  (define compile-expr compile-expr/internal)
  (define (mark l expr)
    (with-syntax [((loc-param ...) (loc-list l))]
      #`(r:with-continuation-mark (r:quote pyret-mark) (r:srcloc loc-param ...) #,expr)))
  (define (compile-body l body)
    (mark l (compile-expr body env)))
  (define (compile-string-literal l e)
    (match e
      [(s-str _ s) (d->stx s l)]
      [else #`(p:check-str #,(compile-expr e env) #,(loc-stx l))]))
  (define (compile-lookup l obj field lookup-type)
     (attach l
      (with-syntax*
         ([field-stx (compile-string-literal l field)])
       #`(#,lookup-type #,(loc-stx l) #,(compile-expr obj env) field-stx))))
  (define (compile-member ast-node)
    (match ast-node
      [(s-data-field l name value)
       (attach l
         (with-syntax*
          ([name-stx (compile-string-literal l name)]
           [val-stx (compile-expr value env)]) 
           #`(r:cons name-stx val-stx)))]))
  (match ast-node
    
    [(s-block l stmts)
     (with-syntax ([(stmt ...) (compile-block l stmts env)])
       (attach l #'(r:let () stmt ...)))]

    [(s-num l n) #`(p:mk-num #,(d->stx n l))]
    [(s-bool l #t) #`p:p-true]
    [(s-bool l #f) #`p:p-false]
    [(s-str l s) #`(p:mk-str #,(d->stx s l))]

    [(s-lam l params args ann doc body _)
     (attach l
       (with-syntax ([(arg ...) (args-stx l args)]
                     [body-stx (compile-body l body)])
         #`(p:pλ (arg ...) #,doc body-stx)))]
    
    [(s-method l args ann doc body _)
     (attach l
       (with-syntax ([(arg ...) (args-stx l args)]
                     [body-stx (compile-body l body)])
         #`(p:pμ (arg ...) #,doc body-stx)))]
    
    [(s-case l c-bs)
     (define (compile-case-branch b)
       (match b
         [(s-case-branch s test block)
          (attach l
                  #`((p:pyret-true? #,(compile-expr test env))
                     #,(compile-expr block env)))]))
     (attach l
       (with-syntax ([(branch ...) (d->stx (map compile-case-branch c-bs) l)])
         #`(r:cond branch ...)))]

    [(s-try l try (s-bind l2 id ann) catch)
     (attach l
       #`(r:with-handlers
            ([p:exn:fail:pyret?
              (r:lambda (%exn)
               (r:define #,(d->stx (discard-_ id) l2) (p:mk-exn %exn))
               #,(compile-expr catch env))])
            #,(compile-expr try env)))]

    [(s-id l name)
     (attach l
       (with-syntax ([name-stx (d->stx (discard-_ name) l)])
         #'name-stx))]

    [(s-assign l name expr)
     (attach l
       (with-syntax ([name-stx (d->stx name l)]
                     [temp (gensym name)])
         #`(r:let [(temp #,(compile-expr expr env))]
             (r:set! name-stx temp)
             temp)))]

    [(s-app l (s-bracket l2 obj field) args)
     (with-syntax* ([obj (compile-expr obj env)]
                    [(arg ...) (map (curryr compile-expr env) args)]
                    [(argid ...) (map (λ (_) (format-id #'obj "~a" #`#,(gensym 'arg))) args)]
                    [field (compile-string-literal l2 field)])
         (mark l
          #`(r:let* ([%obj obj]
                     [%field (p:get-raw-field #,(loc-stx l) %obj field)]
                     [argid arg] ...)
              ((p:p-base-method %field) %obj argid ...))))]


    [(s-app l fun args)
     (define (compile-fun-expr fun)
      (match fun
        [(s-id l2 (? (λ (s) (set-member? (compile-env-functions-to-inline env) s)) id))
         (make-immediate-id id)]
        [(s-lam l _ args _ doc body _)
         (with-syntax ([(arg ...) (args-stx l args)])
           #`(p:arity-catcher (arg ...) #,(compile-expr/internal body env)))]
        [_ #`(p:p-base-app #,(compile-expr fun env))]))
     (attach l
        (with-syntax ([fun (compile-fun-expr fun)]
                      [(arg ...) (map (curryr compile-expr env) args)])
          (mark l #'(fun arg ...))))]

    [(s-obj l fields)
     (attach l
       (with-syntax ([(member ...) (map compile-member fields)])
         #'(p:mk-object (p:make-string-map (r:list member ...)))))]
    
    [(s-extend l super fields)
     (attach l
       (with-syntax ([(member ...) (map compile-member fields)]
                     [super (compile-expr super env)])
        #`(p:extend #,(loc-stx l)
                    super
                    (r:list member ...))))]
    
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

(define (compile-expr ast)
  (compile-expr/internal ast (compile-env (set))))

(define (compile-pyret ast)
  (match ast
    [(s-prog l headers block) (compile-prog l headers block)]
    [(s-block l stmts)
     (with-syntax ([(stmt ...) (compile-block l stmts (compile-env (set)))])
       (attach l #'(r:begin stmt ...)))]
     #|
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
     |#
    [else (error (format "Didn't match a case in compile-pyret: ~a" ast))]))

(define (discard-_ name)
  (if (equal? name '_) (gensym) name))

