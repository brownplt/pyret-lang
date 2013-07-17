#lang racket/base

(require
  racket/match
  racket/list
  "ast.rkt")

(provide well-formed (struct-out exn:fail:pyret/wf))

(struct exn:fail:pyret/wf exn:fail (srclocs)
  #:property prop:exn:srclocs
    (lambda (a-struct)
      (exn:fail:pyret/wf-srclocs a-struct)))

(define (wf-error str . locs)
  (raise (exn:fail:pyret/wf (string-append "well-formedness: " str) (continuation-marks #f) locs)))

;; NOTE(dbp): `well-formed` operates over surface syntax
;;
;; We are not transforming anything, so this function returns nothing.
;;
;; Also, currently it will fail early, reporting the first error it encounters.
;; Possibly this should be changed to report more?
;;
;; The well-formed conditions we are currently checking for:
;;
;; - non-mixed operators - the reachable portion of the ast tree from
;;   an s-op, where reachable is defined by edges that connect
;;   to s-op or s-not, must all have the same op type.
;;
;; - methods with zero arguments - since the object itself will be passed as
;;   the first argument, to have a zero argument method is an error.

(define (well-formed ast)
  (match ast
    [(s-prog s imps ast)
     (s-prog s imps (well-formed/internal ast))]
    [else (well-formed/internal ast)])
  ast)

(define (well-formed/internal ast)
  (define wf well-formed/internal)
    (define (wf-if-branch branch)
    (match branch
      [(s-if-branch s tst blk) (begin (wf tst) (wf blk))]))
  (define (wf-cases-branch branch)
    (match branch
      [(s-cases-branch s name args blk)
       (begin (map wf-bind args) (wf blk))]))
  (define (wf-ann ast)
    (match ast
      [(a-pred s t e) (wf e)]
      [_ ast]))
  (define (wf-bind b)
    (match b
     [(s-bind s name ann) (wf-ann ann)]))
  (define (wf-variant var)
    (match var
     [(s-singleton-variant s name members)
      (map wf-member members)]
     [(s-variant s name binds members)
      (begin (map wf-bind binds) (map wf-member members))]))
  (define (wf-member mem)
    (match mem
     [(s-data-field s name val) (begin (wf name) (wf val))]
     [(s-method-field s name args ann doc body check)
      (if (= (length args) 0) (wf-error "Cannot have a method with zero arguments." s)
          (begin (map wf-bind args) (wf-ann ann) (wf body)))]))

  (define (reachable-ops s op ast)
    (define (op-name op) (hash-ref reverse-op-lookup-table op))
    (match ast
      [(s-not s1 _)
       (wf-error
        (format "Cannot have nested bare `not` with a `~a` operator. Include parenthesis around it." (op-name op))
        s s1)]
      [(s-op s1 op1 e1 e2)
       (if (equal? op op1) (begin (reachable-ops s op e1)
                                  (reachable-ops s op e2))
           (wf-error
            (format "Cannot mix binary operators of different types: `~a` and `~a`. Use parenthesis to disambiguate."
                    (op-name op) (op-name op1))
            s s1))]
      [else (wf ast)]))
  
  (match ast
    ;; NOTE(dbp): the grammar prevents e from being a binop or a not, so s-not is always correct.
    [(s-not s e) (wf e)]
    
    [(s-op s op e1 e2) (begin (reachable-ops s op e1)
                              (reachable-ops s op e2))]

    [(s-block s stmts)
     (map well-formed stmts)]
    [(s-data s name params variants shares check)
     (begin
       (map wf-variant variants)
       (map wf-member shares))]

    [(s-for s iter bindings ann body)
     (define (wf-for-bind b)
      (match b
        [(s-for-bind s bind value)
         (begin (wf-bind bind) (wf value))]
        [else (error (format "FATAL: Not an s-for-bind: ~a" b))]))
     (begin (wf iter)
            (map wf-for-bind bindings)
            (wf-ann ann)
            (wf body))]

    [(s-var s name val) (begin (wf-bind name) (wf val))]
    [(s-let s name val) (begin (wf-bind name) (wf val))]

    [(s-fun s name typarams args ann doc body check)
     (begin (map wf-bind args) (wf-ann ann) (wf body))]

    [(s-lam s typarams args ann doc body check)
     (begin (map wf-bind args)
            (wf-ann ann)
            (wf body))]

    [(s-method s args ann doc body check)
     (if (= (length args) 0) (wf-error "well-formedness: Cannot have a method with zero arguments." s)
         (begin (map wf-bind args)
                (wf-ann ann)
                (wf body)))]

    [(s-when s test body)
     (begin (wf test) (wf body))]

    [(s-if s if-bs) (map wf-if-branch if-bs)]
    [(s-if-else s if-bs else) (begin
                                (map wf-if-branch if-bs)
                                (wf else))]

    [(s-cases type val s c-bs)
     (begin (wf type) (wf val) (map wf-cases-branch c-bs))]
    [(s-cases-else type val s c-bs else)
     (begin (wf type)
            (wf val)
            (map wf-cases-branch c-bs)
            (wf else))]

    [(s-case s c-bs)
     (define (wf-case branch)
       (match branch
         [(s-case-branch s tst blk) (begin (wf tst) (wf blk))]))
     (map wf-case c-bs)]

    [(s-try s try x exn) (begin (wf try) (wf exn))]

    [(s-assign s name expr) (wf expr)]

    [(s-app s fun args) (begin (wf fun) (map wf args))]

    [(s-left-app s target fun args)
     (begin (wf target) (wf fun) (map wf args))]

    [(s-extend s super fields) (begin (wf super)
                                      (map wf-member fields))]

    [(s-obj s fields) (map wf-member fields)]

    [(s-list s elts) (map wf elts)]

    [(s-dot s val field) (wf val)]

    [(s-bracket s val field) (begin (wf val) (wf field))]

    [(s-colon s obj field) (wf obj)]

    [(s-colon-bracket s obj field) (begin (wf obj) (wf field))]

    [(s-paren s e) (wf e)]

    [(or (s-num _ _)
         (s-bool _ _)
         (s-str _ _)
         (s-id _ _)) #t]

    [else (error (format "Missed a case in well-formed: ~a"
                         ast))]))
