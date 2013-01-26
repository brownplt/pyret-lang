#lang racket

(require "ast.rkt")
(provide contract-check-pyret)

(define (wrap-ann-check loc ann e)
  (s-app loc (ann-check loc ann) (list e)))

(define (mk-lam loc args result doc body)
  (s-lam loc empty args result doc (s-block loc (list body))))
(define (mk-method loc args result doc-unused body)
  (s-method loc args result (s-block loc (list body))))

(define (string-of-ann ann)
  (match ann
    [(a-name _ id) (symbol->string id)]
    [(a-arrow _ t1 t2) (format "~a -> ~a" (map string-of-ann t1) (string-of-ann t2))]
    [(a-blank) "Any"]
    [(a-any) "Any"]
    [(a-app _ base args) (format "~a~a" base (map string-of-ann args))]))

(define (ann-check loc ann)
  (define (code-wrapper s args result type get-fun)
    (define funname (gensym))
    (define wrapargs (map (lambda (a) (s-bind s (gensym) a)) args))
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
    (format "internal contract for ~a" ann))
  (define ann-str (s-str loc (string-of-ann ann)))
  (define (mk-flat-checker checker)
    (define argname (gensym))
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
    [else
     (error
      (format "typecheck: don't know how to check ann: ~a"
              ann))]))

(define (lookup env id)
  (define r (hash-ref env id #f))
  (when (not r) (error (format "Unbound id: ~a" id)))
  r)
(define (update bind env)
  (match bind
    [(s-bind _ id ann)
     (hash-set env id ann)]))


(define (cc-block-env stmts env)
  (foldr update env (map s-var-name (filter s-var? stmts))))

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

    [(s-lam s typarams args ann doc body)
     (define body-env (foldr update env args))
     (wrap-ann-check s
                     (get-arrow s args ann)
                     (s-lam s typarams args ann doc (cc-env body body-env)))]
    
    ;; TODO(joe): give methods an annotation position for result
    [(s-method s args ann body)
     (define body-env (foldr update env args))
     (s-method s args ann (cc-env body body-env))]
    
    [(s-cond s c-bs)
     (define (cc-branch branch)
       (match branch
         [(s-cond-branch s test expr)
          (s-cond-branch s (cc test) (cc expr))]))
     (s-cond s (map cc-branch c-bs))]
    
    [(s-assign s name expr)
     (s-assign s name (wrap-ann-check s (lookup env name) (cc expr)))]

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

    [(or (s-num _ _)
         (s-bool _ _)
         (s-str _ _)
         (s-id _ _)) ast]
    
    [else (error (format "Missed a case in type-checking: ~a" ast))]))

(define (contract-check-pyret ast)
  (cc-env ast (make-immutable-hash)))
  
