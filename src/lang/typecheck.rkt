#lang racket

(require "ast.rkt" "pretty.rkt" "type-env.rkt" "../parameters.rkt")
(provide contract-check-pyret (struct-out exn:fail:pyret/tc))

(define (build-location s)
  (define (serialize-source e)
    (cond
      [(symbol? e) (symbol->string e)]
      [(string? e) e]
      [(path? e) (path->string e)]
      [(false? e) "unknown source"]
      [else (error (format "Non-symbol, non-string, non-path value for
                            source: ~a" e))]))
  (s-app s
    (s-bracket s (s-id s 'error) (s-str s "location"))
    (list
      (s-str s (serialize-source (srcloc-source s)))
      (s-num s (srcloc-line s))
      (s-num s (srcloc-column s)))))


(struct exn:fail:pyret/tc exn:fail (srclocs)
  #:property prop:exn:srclocs
    (lambda (a-struct)
      (exn:fail:pyret/tc-srclocs a-struct)))

(define (tc-error str . locs)
  (raise (exn:fail:pyret/tc (format "~a ~a"  locs str) (continuation-marks #f) locs)))

(define VAR-REMINDER "(Identifiers are declared with = and as the names of function arguments.  Variables are declared with var.)")

(define (bad-assign-msg name)
  (format "Assignment to identifier ~a, which is not a variable. ~a" name VAR-REMINDER))

(define (mixed-id-type-msg name)
  (format "~a declared as both a variable and identifier. ~a" name VAR-REMINDER))

(define (shadow-id-msg name)
  (format "The name ~a cannot be used in two nested scopes.  Rename one of them to avoid confusion." name))

(define (duplicate-identifier name)
  (format "~a defined twice" name))

(define (skippable? a)
  (or (a-blank? a) (a-any? a) (and (a-name? a) (equal? (a-name-id a) 'Any))))
(define (wrap-ann-check loc ann e)
  (match ann
    [(? skippable? a) e]
    [(a-arrow _  (list (? skippable? arg) ...) (? skippable? return)) e]
    [(a-method _  (list (? skippable? arg) ...) (? skippable? return)) e]
    [_ (s-app loc (ann-check loc ann) (list e))]))

(define (mk-lam loc args result doc body)
  (s-lam loc empty args result doc (s-block loc (list body)) (s-block loc empty)))
(define (mk-method loc args result doc body)
  (s-method loc args result doc (s-block loc (list body)) (s-block loc empty)))

(define (ann-check loc ann)
  (define ann-str (s-str loc (pretty-ann ann)))
  (define (code-wrapper s args result type get-fun initial-check)
    (define funname (gensym "contract"))
    (define gotten-funname (gensym "fun"))
    (define wrapargs (map (lambda (a) (s-bind s (gensym "arg") a)) args))
    (define (check-arg bind)
      (match bind
        [(s-bind s id ann) (wrap-ann-check s ann (s-id s id))]))
    (mk-lam s (list (s-bind s funname ann)) ann
     (mk-contract-doc ann)
     (s-block s
      (list
       (s-app loc
              (s-id loc 'check-brand)
              (list initial-check
                    (s-id loc funname)
                    ann-str))
       (s-let s (s-bind s gotten-funname (a-blank)) (get-fun (s-id s funname)))
       (s-extend
         s
         (type s wrapargs result
          (mk-contract-doc ann)
          (wrap-ann-check s result
           (s-app s (s-id s gotten-funname) (map check-arg wrapargs))))
         (list (s-data-field s (s-str s "_doc")
                               (s-bracket s
                                          (s-id s funname)
                                          (s-str s "_doc")))))))))
  (define (mk-contract-doc ann)
    (format "internal contract for ~a" (pretty-ann ann)))
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
     (mk-lam loc (list (s-bind loc '_ (a-blank))) (a-blank)
             (mk-contract-doc ann)
             (s-id loc '_))]
    [(a-any)
     (mk-lam loc (list (s-bind loc '_ (a-blank))) (a-blank)
             (mk-contract-doc ann)
             (s-id loc '_))]
    [(a-arrow s args result)
     (code-wrapper s args result mk-lam (λ (e) e) (s-id s 'Function))]
    [(a-method s args result)
     (define (get-fun e)
       (s-app s (s-bracket s e (s-str s "_fun")) (list)))
     (code-wrapper s args result mk-method get-fun (s-id s 'Method))]
    [(a-app s ann parameters)
     ;; NOTE(dbp): right now just checking the outer part, as if
     ;; everything past the name weren't included.
     (ann-check loc ann)]
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
                 (s-if-else s
                    (list
                      (s-if-branch s (s-id s result)
                        (s-block s (list (s-id s tempname)))))
                      (s-block s
                        (list
                          (s-app s (s-id s 'raise)
                            (list
                              (s-app s
                              (s-bracket s (s-id s 'error) (s-str s "user-contract-failure"))
                              (list
                                (s-app s
                                  (s-bracket s
                                    (s-str s "contract check: value did not match predicate: ")
                                    (s-str s "_plus"))
                                  (list
                                    (s-app s (s-id s 'torepr)
                                      (list (s-id s tempname)))))
                                  
                                (build-location s)
                                (s-bracket s (s-id s 'list) (s-str s "empty")))))))))
               )))]
    [else
     (error
      (format "typecheck: don't know how to check ann: ~a"
              ann))]))

(define (check-consistent env loc id mutable?)
  (cond
    [(not (bound? env id)) (void)]
    ;; NOTE(dbp): this is a little bit unpleasant. Later on (in compile),
    ;; _ in id positions is turned into a gensym. Thus, it never will conflict
    ;; with another binding with the same name. So if we block it here, this is
    ;; confusing, but equally, to do the conversion before here (ie, in the parser)
    ;; seems odd. 
    [(equal? id '_) (void)]
    [else
     (match (cons (lookup env id) mutable?)
       [(cons (binding other-loc _ #f) #t)
        (tc-error (mixed-id-type-msg id) loc other-loc)]
       [(cons (binding other-loc _ #t) #f)
        (tc-error (mixed-id-type-msg id) loc other-loc)]
       [(cons (binding other-loc _ b) b)
        (if (or (current-allow-shadowed-vars) (equal? id 'self))
            (void)
            (tc-error (shadow-id-msg id) loc other-loc))])]))

(define ((update-for-bind mutable?) bind env)
  (match bind
    [(s-bind loc id ann)
     (check-consistent env loc id mutable?)
     (update id (binding loc ann mutable?) env)]
    [_ (error (format "Expected a bind and got something else: ~a" bind))]))


(define (cc-block-env stmts env)
  (define (get-bind stmt)
    (match stmt
      [(s-var _ _ _) stmt]
      [(s-let _ _ _) stmt]
      [_ #f]))
  (define bind-stmts (filter-map get-bind stmts))
  (define (update-for-node node env)
    (match node
      [(s-var loc (s-bind _ id ann) _)
       (check-consistent env loc id #t)
       (update id (binding loc ann #t) env)]
      [(s-let loc (s-bind _ id ann) _)
       (check-consistent env loc id #f)
       (update id (binding loc ann #f) env)]))
  (find-duplicate bind-stmts empty)
  (foldl update-for-node env bind-stmts))

(define (get-id stmt)
  (match stmt
    [(s-var _ (s-bind _ id _) _) id]
    [(s-let _ (s-bind _ id _) _) id]))
(define (get-loc stmt)
  (match stmt
    [(s-var loc (s-bind _ _ _) _) loc]
    [(s-let loc (s-bind _ _ _) _) loc]))
(define (find-duplicate stmts stmts-seen)
  (define ((matching-bind stmt-chk) stmt)
    (define id-chk (get-id stmt-chk))
    (define id (get-id stmt))
    (and (not (symbol=? '_ id-chk))
         (not (symbol=? '_ id))
         (symbol=? id-chk id)))
  (cond
    [(empty? stmts) #f]
    [(cons? stmts)
     (define stmt (first stmts))
     (define found (findf (matching-bind stmt) stmts-seen))
     (cond
      [found (tc-error (duplicate-identifier (get-id stmt))
             (get-loc stmt)
             (get-loc found))]
      [else (find-duplicate (rest stmts) (cons stmt stmts-seen))])]))

(define (get-arrow s args ann)
  (a-arrow s (map s-bind-ann args) ann))

(define (cc-env ast env)
  (define cc (curryr cc-env env))
  (define (cc-member ast env)
    (match ast
      [(s-data-field s name value) (s-data-field s name (cc-env value env))]
      [(s-once-field s name ann value)
       (cond
        [(skippable? ann)
         (s-data-field s name (cc-env value env))]
        [else
         (define checker (ann-check s ann))
         (define val-id (gensym 'maybe-placeholder))
         (define val-expr (s-id s val-id))
         (define is-placeholder (s-app s (s-id s 'Placeholder) (list val-expr)))
         (define do-guard (s-app s (s-bracket s val-expr (s-str s "guard"))
                                 (list checker)))
         (define field-block
          (s-block s
            (list
              (s-let s (s-bind s val-id (a-blank)) (cc-env value env))
              (s-if-else s
                (list (s-if-branch s is-placeholder (s-block s (list do-guard val-expr))))
                (s-block s (list (s-app s checker (list val-expr))))))))
         (s-data-field s name field-block)])]
      [(s-mutable-field s name ann value)
       (cond
        [(skippable? ann)
         (s-data-field s name (s-app s (s-id s 'mk-simple-mutable) (list (cc-env value env))))]
        [else
         (define check-read-expr (ann-check s ann))
         (define check-write-expr (ann-check s ann))
         (s-data-field s name (s-app s (s-id s 'mk-mutable)
          (list (cc-env value env) check-read-expr check-write-expr)))])]))
  (match ast
    [(s-block s stmts)
     (define new-env (cc-block-env stmts env))
     (s-block s (map (curryr cc-env new-env) stmts))]
    [(s-var s bnd val)
     (s-var s bnd (wrap-ann-check s (s-bind-ann bnd) (cc val)))]
    [(s-let s bnd val)
     (s-let s bnd (wrap-ann-check s (s-bind-ann bnd) (cc val)))]

    [(s-lam s typarams args ann doc body check)
     (define (new-arg b)
      (match b
        [(s-bind s id ann) (s-bind s (gensym id) (a-blank))]))
     (define new-args (map new-arg args))
     (define new-argnames (map s-bind-id new-args))
     (define new-locs (map s-bind-syntax new-args))
     (define body-env (foldl (update-for-bind #f) env (append args new-args)))
     (define wrapped-body
      (wrap-ann-check s ann (cc-env body body-env)))
     (define (check-arg bind new-id new-loc) (cc-env (s-let new-loc bind (s-id new-loc new-id)) body-env))
     (define checked-args (map check-arg args new-argnames new-locs))
     ;; NOTE(joe): Just doing the checking of checked-args for the error-checking,
     ;; all the necessary wrapping is done
     (find-duplicate checked-args empty)
     (define full-body
      (s-block s
        (append checked-args (list wrapped-body))))
     (s-lam s typarams new-args ann doc full-body (cc check))]

    ;; TODO(joe): give methods an annotation position for result
    [(s-method s args ann doc body check)
     (define body-env (foldl (update-for-bind #f) env args))
     (wrap-ann-check s
      (a-method s (map s-bind-ann args) (a-blank))
      (s-method s args ann doc (cc-env body body-env) (cc-env check body-env)))]

    [(s-if-else s if-bs else-block)
     (define (cc-branch branch)
       (match branch
         [(s-if-branch s test expr)
          (s-if-branch s (cc test) (cc expr))]))
     (s-if-else s (map cc-branch if-bs) (cc else-block))]

    [(s-try s try bind catch)
     (define catch-env ((update-for-bind #f) bind env))
     (s-try s (cc try) bind (cc-env catch catch-env))]

    [(s-assign s name expr)
     (if (bound? env name)
         (match (lookup env name)
          [(binding s-def _ #f)
           (tc-error (bad-assign-msg name) s s-def)]
          [(binding _ ann #t)
           (s-assign s name (wrap-ann-check s ann (cc expr)))])
         (tc-error (format "Assigning to unbound variable: ~a" name) s))]


    [(s-app s fun args)
     (s-app s (cc fun) (map cc args))]

    [(s-extend s super fields)
     (s-extend s (cc super) (map (curryr cc-member env) fields))]

    [(s-update s super fields)
     (s-update s (cc super) (map (curryr cc-member env) fields))]

    [(s-obj s fields)
     (s-obj s (map (curryr cc-member env) fields))]

    [(s-list s elts)
     (s-list s (map cc elts))]

    [(s-dot s val field)
     (s-dot s (cc val) field)]

    [(s-get-bang s val field)
     (s-get-bang s (cc val) field)]

    [(s-bracket s val field)
     (s-bracket s (cc val) (cc field))]

    [(s-colon s obj field)
     (s-colon s (cc obj) field)]

    [(s-colon-bracket s obj field)
     (s-colon-bracket s (cc obj) (cc field))]

    [(s-id s x)
     (if (bound? env x)
         ast
         (tc-error (format "Unbound identifier: ~a" x) s))]

    [(or (s-num _ _)
         (s-bool _ _)
         (s-str _ _)) ast]

    [else (error (format "Missed a case in type-checking: ~a" ast))]))

(define (contract-check-pyret ast env)
  (define (bind-imports imp env)
    (match imp
      [(s-import l f n)
       (update n (binding l (a-blank) #f) env)]
      [_ env]))
  (match ast
    ;; TODO(joe): typechecking provides expressions?
    [(s-prog s imps ast)
     (define imported-env (foldr bind-imports env imps))
     (s-prog s imps (cc-env ast imported-env))]
    [else (cc-env ast env)]))
