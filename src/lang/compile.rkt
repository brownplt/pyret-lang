#lang racket

(provide
  compile-pyret
  compile-expr)
(require
  racket/match
  racket/splicing
  racket/syntax
  "helpers.rkt"
  "../parameters.rkt"
  "ast.rkt"
  "compile-helpers/find.rkt"
  "compile-helpers/lift-constants.rkt")


(define (loc-stx loc)
  (with-syntax ([(loc-param ...) (loc-list loc)])
    #'(r:list loc-param ...)))

(define (args-stx l args)
  (d->stx (map discard-_ (map s-bind-id args)) l))

(struct compile-env (functions-to-inline toplevel?) #:transparent)

(define (d->stx stx loc) (datum->syntax #f stx (loc-list loc)))

(define (attach loc stx)
  (datum->syntax #f (syntax-e stx) (loc-list loc)))

(define (block-fun-ids stmts)
  (define (stmt-id stmt)
    (match stmt
      [(s-let _ (s-bind _ _ id _) (s-lam _ _ _ _ _ _ _)) id]
      [(s-let _ (s-bind _ _ id _) (s-extend s (s-lam _ _ _ _ _ _ _) fields)) id]
      [_ #f]))
  (list->set (filter-map stmt-id stmts)))

(define (block-ids stmts)
  (define (stmt-id stmt)
    (match stmt
      [(s-let _ (s-bind _ _ id _) _) id]
      [(s-var _ (s-bind _ _ id _) _) id]
      [_ #f]))
  (filter-map stmt-id stmts))

(define (make-immediate-id id)
  (string->symbol (format "~a##inline" id)))

(define (compile-block l stmts env)
  (define (compile-stmt ast-node env)
    (match ast-node
      [(s-var s (s-bind _ _ id _) val)
        (list
          #`(r:define #,(discard-_ id) #,(compile-expr/internal val env)))]
      [(s-let s (s-bind _ _ id _) val)
       (define (match-id-use e)
        (match e
          [(s-app s (s-id s2 (? (lambda (x) (equal? id x)) x)) args)
           (s-id s2 x)]
          [(s-id s (? (lambda (x) (equal? id x)) x))
           (s-id s x)]
          [_ #f]))
       (define ids (find (s-block l stmts) match-id-use))
       (define id-used (or (> (length (remove-duplicates ids)) 1)
                           (= (length ids) 1)))
       (define (help-λ inline-binding l args doc body)
         (cond
          [(or (compile-env-toplevel? env) id-used)
            (list inline-binding
                  (with-syntax ([(arg ...) (args-stx l args)]
                                [f-id (make-immediate-id id)])
                    #`(r:define #,(discard-_ id)
                          (p:pλ (arg ...) #,doc (f-id arg ...)))))]
          [else (list inline-binding)]))
       (define (help-extend inline-binding l args doc body fields)
         (cond
          [(or (compile-env-toplevel? env) id-used)
           (list inline-binding
                 (with-syntax ([(arg ...) (args-stx l args)]
                               [f-id (make-immediate-id id)]
                               [(field ...) (map (curryr compile-member env) fields)])
                   #`(r:define #,(discard-_ id)
                               (p:extend
                                #,(loc-stx s)
                                (p:pλ (arg ...) #,doc (f-id arg ...))
                                (r:list field ...)))))]
          [else (list inline-binding)]))
       (define (uses-loc hints)
         (findf h-use-loc? hints))
       (match val
        [(s-hint-exp _ (? uses-loc hints) (s-lam l _ args _ doc body _))
         (define loc (h-use-loc-loc (uses-loc hints)))
         (define inline-binding
          (with-syntax ([(arg ...) (args-stx l args)])
            #`(r:define #,(make-immediate-id id)
               (p:arity-catcher-loc (arg ...) #,(compile-expr/internal body env) loc))))
         (help-λ inline-binding l args doc body)]
        [(s-lam l _ args _ doc body _)
         (define inline-binding
          (with-syntax ([(arg ...) (args-stx l args)])
            #`(r:define #,(make-immediate-id id)
               (p:arity-catcher (arg ...) #,(compile-expr/internal body env)))))
         (help-λ inline-binding l args doc body)]
        [_ (list #`(r:define #,(discard-_ id) #,(compile-expr/internal val env)))])]
      [_ (list (compile-expr/internal ast-node env))]))
  (define ids (block-ids stmts))
  (define fun-ids (block-fun-ids stmts))
  (define old-fun-ids (compile-env-functions-to-inline env))
  (define avoid-shadowing (set-subtract old-fun-ids (list->set ids)))
  (define new-env (compile-env (set-union avoid-shadowing fun-ids)
                               (compile-env-toplevel? env)))
  (define stmts-stx (append* (map (curryr compile-stmt new-env) stmts)))
  (if (empty? stmts-stx) (list #'nothing) stmts-stx))

(define (compile-member ast-node env)
  (match ast-node
    [(s-data-field l name value)
     (attach l
       (with-syntax*
        ([name-stx (compile-string-literal l name env)]
         [val-stx (compile-expr/internal value env)])
         #`(r:cons name-stx val-stx)))]
    [else (error (format "Unexpected ast node passed to compile-member: ~a\n" ast-node))]))
(define (compile-string-literal l e env)
  (match e
    [(s-str _ s) (d->stx s l)]
    [else #`(p:check-str #,(compile-expr/internal e env) #,(loc-stx l))]))

(define (compile-expr/internal ast-node env)
  (define compile-expr compile-expr/internal)
  (define (mark l expr)
    (with-syntax [((loc-param ...) (loc-list l))]
      #`(r:with-continuation-mark (r:quote pyret-mark) (r:srcloc loc-param ...) #,expr)))
  (define (mark-if mark-mode l expr)
    (if mark-mode (mark l expr) expr))
  (define (compile-body l body new-env)
    (mark l (compile-expr body new-env)))
  (define (compile-lookup l obj field lookup-type)
     (attach l
      (with-syntax*
         ([field-stx (compile-string-literal l field env)])
       #`(#,lookup-type #,(loc-stx l) #,(compile-expr obj env) field-stx))))
  (define (uses-loc hints)
    (findf h-use-loc? hints))
  (match ast-node

    [(s-block l stmts)
     (define new-env (compile-env (compile-env-functions-to-inline env) #f))
     (with-syntax ([(stmt ...) (compile-block l stmts new-env)])
       (attach l #'(r:let () stmt ...)))]

    [(s-user-block l body) (compile-expr body env)]

    [(s-num l n) #`(p:mk-num #,(d->stx n l))]
    [(s-bool l #t) #`p:p-true]
    [(s-bool l #f) #`p:p-false]
    [(s-str l s) #`(p:mk-str #,(d->stx s l))]

    [(s-hint-exp _ (? uses-loc hints) (s-lam l params args ann doc body _))
     (define loc (h-use-loc-loc (uses-loc hints)))
     (define new-env (compile-env (compile-env-functions-to-inline env) #f))
     (attach l
       (with-syntax ([(arg ...) (args-stx l args)]
                     [body-stx (compile-body l body new-env)])
         #`(p:pλ/loc (arg ...) #,doc body-stx #,(loc-stx loc))))]
    [(s-lam l params args ann doc body _)
     (define new-env (compile-env (compile-env-functions-to-inline env) #f))
     (attach l
       (with-syntax ([(arg ...) (args-stx l args)]
                     [body-stx (compile-body l body new-env)])
         #`(p:pλ (arg ...) #,doc body-stx)))]

    [(s-hint-exp _ (? uses-loc hints) (s-method l args ann doc body _))
     (define loc (h-use-loc-loc (uses-loc hints)))
     (define new-env (compile-env (compile-env-functions-to-inline env) #f))
     (attach l
       (with-syntax ([(arg ...) (args-stx l args)]
                     [body-stx (compile-body l body new-env)])
         #`(p:pμ/loc (arg ...) #,doc body-stx #,(loc-stx loc))))]
    [(s-method l args ann doc body _)
     (define new-env (compile-env (compile-env-functions-to-inline env) #f))
     (attach l
       (with-syntax ([(arg ...) (args-stx l args)]
                     [body-stx (compile-body l body new-env)])
         #`(p:pμ (arg ...) #,doc body-stx)))]

    [(s-if-else l c-bs else-block)
     (define (compile-if-branch b)
       (match b
         [(s-if-branch s test block)
          (attach l
                  #`((p:pyret-true? #,(compile-expr test env))
                     #,(compile-expr block env)))]))
     (attach l
       (with-syntax ([(branch ...) (d->stx (map compile-if-branch c-bs) l)])
         #`(r:cond branch ... [#t #,(compile-expr else-block env)])))]

    [(s-try l try (s-bind l2 shadow id ann) catch)
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
                    [field (compile-string-literal l2 field env)])
          (mark-if (current-mark-mode) l
          #`(r:let* ([%obj obj]
                     [%field (p:get-raw-field #,(loc-stx l) %obj field)]
                     [argid arg] ...)
              ((p:p-base-method %field) %obj argid ...))))]


    [(s-app l fun args)
     (define (compile-fun-expr fun)
      (match fun
        [(s-id l2 (? (λ (s) (set-member? (compile-env-functions-to-inline env) s)) id))
         (make-immediate-id id)]
        [(s-hint-exp _ (? uses-loc hints) (s-lam l _ args _ doc body _))
         (define loc (h-use-loc-loc (uses-loc hints)))
         (with-syntax ([(arg ...) (args-stx l args)])
           #`(p:arity-catcher-loc (arg ...) #,(compile-expr/internal body env) loc))]
        [(s-lam l _ args _ doc body _)
         (with-syntax ([(arg ...) (args-stx l args)])
           #`(p:arity-catcher (arg ...) #,(compile-expr/internal body env)))]
        [_ #`(p:p-base-app #,(compile-expr fun env))]))
     (mark-if (current-mark-mode) l
     (attach l
        (with-syntax ([fun (compile-fun-expr fun)]
                      [(arg ...) (map (curryr compile-expr env) args)])
          #'(fun arg ...))))]

    [(s-obj l fields)
     (attach l
       (with-syntax ([(member ...) (map (curryr compile-member env) fields)])
         #'(p:mk-object (p:make-string-map (r:list member ...)))))]

    [(s-extend l super fields)
     (attach l
       (with-syntax ([(member ...) (map (curryr compile-member env) fields)]
                     [super (compile-expr super env)])
        #`(p:extend #,(loc-stx l)
                    super
                    (r:list member ...))))]

    [(s-update l super fields)
     (attach l
       (with-syntax ([(member ...) (map (curryr compile-member env) fields)]
                     [super (compile-expr super env)])
        #`(p:update #,(loc-stx l)
                    super
                    (r:list member ...))))]

    [(s-bracket l obj field)
     (compile-lookup l obj field #'p:get-field)]

    [(s-get-bang l obj field)
     (attach l
      #`(p:get-mutable-field #,(loc-stx l) #,(compile-expr obj env) #,(symbol->string field)))]

    [(s-colon-bracket l obj field)
     (compile-lookup l obj field #'p:get-raw-field)]

    [(s-prog l headers block) (compile-prog l headers block)]

    [(s-hint-exp _ h e)
     (error (format "Saw a hint that wasn't used: ~a" h))]

    [else (error (format "Missed a case in compile: ~a" ast-node))]))

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



(define (compile-prog l headers block)
  (attach l
   (with-syntax ([(req ...) (map compile-header (filter s-import? headers))]
                 [(prov ...) (map compile-header (filter s-provide? headers))])
     #`(r:begin req ... #,(compile-pyret block) prov ...))))

(define (maybe-lift-constants ast)
  (cond
    [(current-compile-lift-constants) (lift-constants ast)]
    [else ast]))

(define (compile-pyret ast)
  (match ast
    [(s-prog l headers block) (compile-prog l headers block)]
    [(s-block l stmts)
     (match-define (s-block l2 new-stmts) (maybe-lift-constants ast))
     (with-syntax ([(stmt ...) (compile-block l2 new-stmts (compile-env (set) #t))])
       (attach l #'(r:begin stmt ...)))]
    [else (error (format "Didn't match a case in compile-pyret: ~a" ast))]))

(define (compile-expr pre-ast)
  (define ast (maybe-lift-constants pre-ast))
  (compile-expr/internal ast (compile-env (set) #f)))

(define (discard-_ name)
  (if (equal? name '_) (gensym) name))
