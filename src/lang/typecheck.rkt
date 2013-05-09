#lang racket

(require "ast.rkt" "pretty.rkt")
(provide contract-check-pyret)

(define (wrap-ann-check loc ann e)
  (s-app loc (ann-check loc ann) (list e)))

(define (mk-lam loc args result doc body)
  (s-lam loc empty args result doc (s-block loc (list body))))
(define (mk-method loc args result doc-unused body)
  (s-method loc args result (s-block loc (list body))))

(define (ann-check loc ann)
  (define (code-wrapper s args result type get-fun)
    (define funname (gensym "contract"))
    (define wrapargs (map (lambda (a) (s-bind s (gensym "arg") a)) args))
    (define (check-arg bind)
      (match bind
        [(s-bind s id ann) (wrap-ann-check s ann (s-id s id))]))
    (mk-lam s (list (s-bind s funname ann)) ann
     (mk-contract-doc ann)
     (s-onion
       s
       (type s wrapargs result
        (mk-contract-doc ann)
        (wrap-ann-check s result 
         (s-app s (get-fun (s-id s funname)) (map check-arg wrapargs))))
       (list (s-data-field s (s-str s "doc")
                             (s-bracket s
                                        (s-id s funname)
                                        (s-str s "doc")))))))
  (define (mk-contract-doc ann)
    (format "internal contract for ~a" (pretty-ann ann)))
  (define ann-str (s-str loc (pretty-ann ann)))
  (define (mk-flat-checker checker)
    (define argname (gensym "specimen"))
    (mk-lam loc (list (s-bind loc argname (a-blank))) ann
            (mk-contract-doc ann)
            (s-app
             loc
             (s-id loc 'check-brand)
             (list checker
                   (s-id loc argname)
                   ann-str))))
  (match ann
    [(a-name s id)
     (mk-flat-checker
      (s-id s (string->symbol
               (string-append
                (symbol->string id) "?"))))]
    [(a-blank)
     (mk-flat-checker (s-id loc 'Any?))]
    [(a-any)
     (mk-flat-checker (s-id loc 'Any?))]
    [(a-arrow s args result)
     (code-wrapper s args result mk-lam (λ (e) e))]
    [(a-method s args result)
     (define (get-fun e)
       (s-bracket s e (s-str s "_fun")))
     (code-wrapper s args result mk-method get-fun)]
    [(a-pred s ann pred)
     (define ann-wrapper (ann-check s ann))
     (define argname (gensym "pred-arg"))
     (define tempname (gensym "pred-temp"))
     (define result (gensym "pred-result"))
     (mk-lam loc (list (s-bind loc argname (a-blank))) (a-blank)
             (mk-contract-doc ann)
             (s-block s
               (list
                 (s-var s (s-bind s tempname (a-blank))
                          (s-app loc
                                 ann-wrapper
                                 (list (s-id loc argname))))
                 (s-var s (s-bind s result (a-blank))
                          (s-app loc
                                 pred
                                 (list (s-id s tempname))))
                 (s-cond s
                    (list
                      (s-cond-branch s (s-id s result)
                        (s-block s (list (s-id s tempname))))
                      (s-cond-branch s (s-id s 'else)
                        (s-block s
                          (list
                            (s-app s (s-id s 'raise)
                                     (list (s-str s "contract failure"))))))))
               )))]
    [else
     (error
      (format "typecheck: don't know how to check ann: ~a"
              ann))]))

(define (bound? env id)
  (hash-has-key? env id))
(define (lookup env id)
  (define r (hash-ref env id #f))
  (when (not r) (error (format "Unbound id: ~a" id)))
  r)
(struct binding (ann mutable?))
(define (update id b env)
  (hash-set env id b))

(define (check-consistent env id mutable?)
  (cond
    [(not (bound? env id)) (void)]
    [else
     (match (cons (binding-mutable? (lookup env id)) mutable?)
       [(cons #f #t)
        (error (format "~a declared as both a variable and identifier" id))]
       [(cons #t #f)
        (error (format "~a declared as both a variable and identifier" id))]
       [_ (void)])]))

(define ((update-for-bind mutable?) bind env)
  (match bind
    [(s-bind _ id ann)
     (check-consistent env id mutable?)
     (update id (binding ann mutable?) env)]
    [_ (error (format "Expected a bind and got something else: ~a" bind))]))


(define (cc-block-env stmts env)
  (define (update-for-node node env)
    (match node
      [(s-var _ (s-bind _ id ann) _)
       (check-consistent env id #t)
       (update id (binding ann #t) env)]
      [(s-let _ (s-bind _ id ann) _)
       (check-consistent env id #f)
       (update id (binding ann #f) env)]
      [_ env]))
  (foldr update-for-node env stmts))

(define (get-arrow s args ann)
  (a-arrow s (map s-bind-ann args) ann))
  
(define (cc-env ast env)
  (define cc (curryr cc-env env))
  (define (cc-member ast env)
    (match ast
      [(s-data-field s name value) (s-data-field s name (cc-env value env))]))
  (match ast
    [(s-block s stmts)
     (define new-env (cc-block-env stmts env))
     (s-block s (map (curryr cc-env new-env) stmts))]
    [(s-var s bnd val)
     (s-var s bnd (wrap-ann-check s (s-bind-ann bnd) (cc val)))]
    [(s-let s bnd val)
     (s-let s bnd (wrap-ann-check s (s-bind-ann bnd) (cc val)))]

    [(s-lam s typarams args ann doc body)
     (define body-env (foldr (update-for-bind #f) env args))
     (wrap-ann-check s
                     (get-arrow s args ann)
                     (s-lam s typarams args ann doc (cc-env body body-env)))]
    
    ;; TODO(joe): give methods an annotation position for result
    [(s-method s args ann body)
     (define body-env (foldr (update-for-bind #f) env args))
     (s-method s args ann (cc-env body body-env))]
    
    [(s-cond s c-bs)
     (define (cc-branch branch)
       (match branch
         [(s-cond-branch s test expr)
          (s-cond-branch s (cc test) (cc expr))]))
     (s-cond s (map cc-branch c-bs))]

    [(s-try s try bind catch)
     (define catch-env ((update-for-bind #f) bind env))
     (s-try s (cc try) bind (cc-env catch catch-env))]
    
    [(s-assign s name expr)
     (match (lookup env name)
      [(binding _ #f)
       (error (format "Assignment to identifier ~a, which is not a variable" name))]
      [(binding ann #t)
       (s-assign s name (wrap-ann-check s ann (cc expr)))])]

    [(s-app s fun args)
     (s-app s (cc fun) (map cc args))]

    [(s-onion s super fields)
     (s-onion s (cc super) (map (curryr cc-member env) fields))]

    [(s-obj s fields)
     (s-obj s (map (curryr cc-member env) fields))]
    
    [(s-list s elts)
     (s-list s (map cc elts))]
    
    [(s-dot s val field)
     (s-dot s (cc val) field)]
    
    [(s-bracket s val field)
     (s-bracket s (cc val) (cc field))]
    
    [(s-dot-method s obj field)
     (s-dot-method s (cc obj) field)]
    
    [(s-bracket-method s obj field)
     (s-bracket-method s (cc obj) (cc field))]

    [(or (s-num _ _)
         (s-bool _ _)
         (s-str _ _)
         (s-id _ _)) ast]
    
    [else (error (format "Missed a case in type-checking: ~a" ast))]))

(define (contract-check-pyret ast)
  (match ast
    ;; TODO(joe): typechecking provides expressions?
    [(s-prog s imps ast)
     (s-prog s imps (cc-env ast (make-immutable-hash)))]
    [else (cc-env ast (make-immutable-hash))]))
  
