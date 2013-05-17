#lang racket

(require "ast.rkt" "pretty.rkt")
(provide contract-check-pyret)

(struct exn:fail:pyret/tc exn:fail (srclocs)
  #:property prop:exn:srclocs
    (lambda (a-struct)
      (exn:fail:pyret/tc-srclocs a-struct)))

(define (tc-error str . locs)
  (raise (exn:fail:pyret/tc str (current-continuation-marks) locs)))

(define VAR-REMINDER "(Identifiers are declared with = and as the names of function arguments.  Variables are declared with var.)")

(define (bad-assign-msg name)
  (format "Assignment to identifier ~a, which is not a variable. ~a" name VAR-REMINDER))

(define (mixed-id-type-msg name)
  (format "~a declared as both a variable and identifier. ~a" name VAR-REMINDER))

(define (wrap-ann-check loc ann e)
  (match ann
    [(a-blank) e]
    [(a-arrow _ (list (a-blank)) (a-blank)) e]
    [_ (s-app loc (ann-check loc ann) (list e))]))

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
      (s-id s id))]
    [(a-dot s obj fld)
     (mk-flat-checker (s-bracket s (s-id s obj)
                                 (s-str s (symbol->string fld))))]
    [(a-blank)
     (mk-flat-checker (s-id loc 'Any))]
    [(a-any)
     (mk-flat-checker (s-id loc 'Any))]
    [(a-arrow s args result)
     (code-wrapper s args result mk-lam (λ (e) e))]
    [(a-method s args result)
     (define (get-fun e)
       (s-bracket s e (s-str s "_fun")))
     (code-wrapper s args result mk-method get-fun)]
    [(a-app s name parameters)
     ;; NOTE(dbp): right now just checking the outer part, as if
     ;; everything past the name weren't included.
     (mk-flat-checker (s-id loc name))]
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
(struct binding (loc ann mutable?))
(define (update id b env)
  (hash-set env id b))

(define (check-consistent env loc id mutable?)
  (cond
    [(not (bound? env id)) (void)]
    [else
     (match (cons (lookup env id) mutable?)
       [(cons (binding other-loc _ #f) #t)
        (tc-error (mixed-id-type-msg id) loc other-loc)]
       [(cons (binding other-loc _ #t) #f)
        (tc-error (mixed-id-type-msg id) loc other-loc)]
       [_ (void)])]))

(define ((update-for-bind mutable?) bind env)
  (match bind
    [(s-bind loc id ann)
     (check-consistent env loc id mutable?)
     (update id (binding loc ann mutable?) env)]
    [_ (error (format "Expected a bind and got something else: ~a" bind))]))


(define (cc-block-env stmts env)
  (define (update-for-node node env)
    (match node
      [(s-var loc (s-bind _ id ann) _)
       (check-consistent env loc id #t)
       (update id (binding loc ann #t) env)]
      [(s-let loc (s-bind _ id ann) _)
       (check-consistent env loc id #f)
       (update id (binding loc ann #f) env)]
      [_ env]))
  (foldl update-for-node env stmts))

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
     (define body-env (foldl (update-for-bind #f) env args))
     (wrap-ann-check s
                     (get-arrow s args ann)
                     (s-lam s typarams args ann doc (cc-env body body-env)))]

    ;; TODO(joe): give methods an annotation position for result
    [(s-method s args ann body)
     (define body-env (foldl (update-for-bind #f) env args))
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
      [(binding s-def _ #f)
       (tc-error (bad-assign-msg name) s s-def)]
      [(binding _ ann #t)
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
