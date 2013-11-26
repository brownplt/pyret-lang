#lang racket/base

(require
  racket/match
  racket/list
  "../parameters.rkt"
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
;;
;; - duplicated identifiers in arguments lists
;;
;; - duplicated constructor names in data(type)
;;
;; - duplicated cases
;;
;; - a cases expression with a case named "_", rather than using else
;;
;; - all blocks end in a non-binding form
;;
;; - check and where identifiers
;;
;; - if has at least two branches
;;
;; - `is` outside of a check block
;;
;; - misplaced `where:` blocks

(define (well-formed ast)
  (match ast
    [(s-prog s imps ast)
     (match ast
      [(s-block s stmts) (map (λ (ast) (well-formed/internal ast #f)) stmts)]
      [_ (well-formed/internal ast)])]
    [(s-block s stmts) (map (λ (ast) (well-formed/internal ast #f)) stmts)]
    [else (well-formed/internal ast #f)])
  ast)

(define (ensure-empty-block loc type check)
  (when (not (current-where-everywhere))
    (match check
      [(s-block s (list)) (void)]
      [(s-block s _)
       (wf-error (format "where: blocks only allowed on named function declarations and data, not on ~a" type) loc)])))

(define (ensure-unique-cases cases)
  (define (help cases seen-cases)
    (cond
      [(empty? cases) (void)]
      [(cons? cases)
       (define f (first cases))
       (define loc (s-cases-branch-syntax f))
       (define name (s-cases-branch-name f))
       (define seen (findf (lambda (c) (equal? (car c) name)) seen-cases))
       (when seen
        (wf-error (format "Duplicate case for ~a" name) (cdr seen) loc))
       (help (rest cases) (cons (cons name loc) seen-cases))]))
  (help cases empty))

(define (ensure-unique-ids bindings)
  (cond
    [(empty? bindings) (void)]
    [(cons? bindings)
     (define this-binding (first bindings))
     (define this-id (s-bind-id (first bindings)))
     (cond
      [(equal? this-id '_)
       (void)]
      [else
       (define (ids-match other) (equal? (s-bind-id other) this-id))
       (define found (findf ids-match (rest bindings)))
       (cond
        [found
         (wf-error (format "Found duplicate id ~a in list of bindings" this-id)
          (s-bind-syntax this-binding)
          (s-bind-syntax found))]
        [else
         (ensure-unique-ids (rest bindings))])])]))

(define (well-formed/internal ast in-check-block)
  (define wf (λ (ast) (well-formed/internal ast in-check-block)))
  (define (wf-if-branch branch)
    (match branch
      [(s-if-branch s tst blk) (begin (wf tst) (wf blk))]))
  (define (wf-last-stmt stmt)
    (match stmt
      [(s-let s bind e) (wf-error "Cannot end a block in a let-binding." s)]
      [(s-var s bind e) (wf-error "Cannot end a block in a var-binding." s)]
      [(s-fun s _ _ _ _ _ _ _) (wf-error "Cannot end a block in a fun-binding." s)]
      [(s-data s _ _ _ _ _ _) (wf-error "Cannot end a block with a data definition." s)]
      [(s-datatype s _ _ _ _) (wf-error "Cannot end a block with a data definition." s)]
      [(s-graph s _) (wf-error "Cannot end a block with a graph definition." s)]
      [else #t]))
  (define (wf-cases-branch branch)
    (match branch
      [(s-cases-branch s name args blk)
       (begin
        (when (eq? name '_)
          (wf-error "Found a cases branch using _ rather than a constructor name; use 'else' instead." s))
        (ensure-unique-ids args)
        (map wf-bind args)
        (wf blk))]))
  (define (wf-ann ast)
    (match ast
      [(a-pred s t e) (wf e)]
      [_ ast]))
  (define (wf-bind b)
    (match b
     [(s-bind s name ann) (wf-ann ann)]))
  (define (wf-variant-member vm)
    (match vm
     [(s-variant-member s mutable? bind) (wf-bind bind)]))
  (define (wf-dt-variant-names vs)
    (define (help vs names locs)
     (cond
      [(empty? vs) (void)]
      [(cons? vs)
       (match (first vs)
        [(or
           (s-datatype-singleton-variant s name _)
           (s-datatype-variant s name _ _))
         (if (member name names)
           (wf-error (format "Constructor name ~a appeared more than once." name)
                     (first locs)
                     s)
           (help (rest vs) (cons name names) (cons s locs)))]
        [else (error (format "Should not happen, email joe@cs.brown.edu.  An invalid variant type was found: ~a" (first vs)))])]))
    (help vs empty empty))

  (define (wf-variant-names vs)
    (define (help vs names locs)
     (cond
      [(empty? vs) (void)]
      [(cons? vs)
       (match (first vs)
        [(or
           (s-singleton-variant s name _)
           (s-variant s name _ _))
         (if (member name names)
           (wf-error (format "Constructor name ~a appeared more than once." name)
                     (first locs)
                     s)
           (help (rest vs) (cons name names) (cons s locs)))]
        [else (error (format "Should not happen, email joe@cs.brown.edu.  An invalid variant type was found: ~a" (first vs)))])]))
    (help vs empty empty))
  (define (static-names-of-fields fields)
    (remove*
     (list #f)
     (map (λ(m) (match m
                  [(s-data-field s (s-str _ name) _) (s-bind s (string->symbol name) (a-blank))]
                  [(s-mutable-field s (s-str _ name) _ _) (s-bind s (string->symbol name) (a-blank))]
                  [(s-once-field s (s-str _ name) _ _) (s-bind s (string->symbol name) (a-blank))]
                  [(s-method-field s (s-str _ name) _ _ _ _ _) (s-bind s (string->symbol name) (a-blank))]
                  [else #f]))
          fields)))
  (define (wf-variant var shared)
    (match var
     [(s-singleton-variant s name members)
      (begin
        (ensure-unique-ids (append (static-names-of-fields members) shared))
        (map wf-member members))]
     [(s-variant s name binds members)
      (begin
        (ensure-unique-ids (append (static-names-of-fields members) (map s-variant-member-bind binds) shared))
        (map wf-variant-member binds)
        (map wf-member members))]))
  (define (wf-dt-variant var)
    (match var
     [(s-datatype-singleton-variant s name constructor)
      (wf-constructor constructor)]
     [(s-datatype-variant s name binds constructor)
      (begin
        (ensure-unique-ids (map s-variant-member-bind binds))
        (map wf-variant-member binds)
        (wf-constructor constructor))]))
  (define (wf-constructor c)
    (match c
      [(s-datatype-constructor s self body)
       (wf body)]))
  (define (wf-member mem)
    (match mem
     [(s-data-field s name val) (begin (wf name) (wf val))]
     [(s-mutable-field s name ann val) (begin (wf name) (wf val))]
     [(s-method-field s name args ann doc body check)
      (if (= (length args) 0) (wf-error "Cannot have a method with zero arguments." s)
          (begin
           (ensure-unique-ids args)
           (ensure-empty-block s "methods" check)
           (map wf-bind args)
           (wf-ann ann)
           (wf body)
           (well-formed/internal check #t)))]))

  (define (reachable-ops s op ast)
    (define (op-name op) (hash-ref reverse-op-lookup-table op))
    (match ast
      [(s-not s1 _)
       (wf-error
        (format "Cannot have nested bare `not` with a `~a` operator. Include parentheses around it." (op-name op))
        s s1)]
      [(s-op s1 op1 e1 e2)
       (if (equal? op op1) (begin (reachable-ops s op e1)
                                  (reachable-ops s op e2))
           (wf-error
            (format "Cannot mix binary operators of different types: `~a` and `~a`. Use parentheses to disambiguate."
                    (op-name op) (op-name op1))
            s s1))]
      [else (wf ast)]))

  (match ast
    ;; NOTE(dbp): the grammar prevents e from being a binop or a not, so s-not is always correct.
    [(s-not s e) (wf e)]

    [(s-hint-exp s h e) (wf e)]

    [(s-check-test s op e1 e2)
     (if (not in-check-block)
         (if (eq? op 'opis)
             (wf-error "Cannot use `is` outside of a `check` or `where` block." s)
             (wf-error "Cannot use a check-test form outside of a `check` or `where` block." s))
         (begin (wf e1)
                (wf e2)))]
    [(s-op s op e1 e2) (begin (reachable-ops s op e1)
                              (reachable-ops s op e2))]

    [(s-block s stmts)
     (begin
       (or (empty? stmts) (wf-last-stmt (last stmts)))
       (map wf stmts))]
    [(s-data s name params mixins variants shares check)
     (begin
       (wf-variant-names variants)
       (map wf mixins)
       (define shared (static-names-of-fields shares))
       (map (λ(v) (wf-variant v shared)) variants)
       (map wf-member shares)
       (well-formed/internal check #t))]

    [(s-datatype s name params variants check)
     (begin
       (wf-dt-variant-names variants)
       (map wf-dt-variant variants)
       (well-formed/internal check #t))]

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

    [(s-graph s bindings) (begin (map wf bindings))]
    [(s-user-block s body) (wf body)]

    [(s-fun s name typarams args ann doc body check)
     (begin (ensure-unique-ids args)
            (map wf-bind args)
            (wf-ann ann)
            (wf body)
            (well-formed/internal check #t))]

    [(s-lam s typarams args ann doc body check)
     (begin (ensure-unique-ids args)
            (ensure-empty-block s "anonymous functions" check)
            (map wf-bind args)
            (wf-ann ann)
            (wf body)
            (well-formed/internal check #t))]

    [(s-method s args ann doc body check)
     (if (= (length args) 0) (wf-error "Cannot have a method with zero arguments." s)
         (begin (ensure-unique-ids args)
                (ensure-empty-block s "methods" check)
                (map wf-bind args)
                (wf-ann ann)
                (wf body)
                (well-formed/internal check #t)))]

    [(s-when s test body)
     (begin (wf test) (wf body))]

    [(s-check s body)
     (well-formed/internal body #t)]


    [(s-if s if-bs)
     (when (= (length if-bs) 1)
      (wf-error "Cannot have an if with a single branch." s))
     (map wf-if-branch if-bs)]

    [(s-if-else s if-bs else) (begin
                                (map wf-if-branch if-bs)
                                (wf else))]

    [(s-cases s type val c-bs)
     (begin (wf-ann type) (wf val) (ensure-unique-cases c-bs) (map wf-cases-branch c-bs))]

    [(s-cases-else s type val c-bs else)
     (begin (wf-ann type)
            (wf val)
            (ensure-unique-cases c-bs)
            (map wf-cases-branch c-bs)
            (wf else))]

    [(s-try s try x exn) (begin (wf try) (wf exn))]

    [(s-assign s name expr) (wf expr)]

    [(s-app s params fun args) (begin (wf fun) (map wf args))]

    [(s-left-app s target fun args)
     (begin (wf target) (wf fun) (map wf args))]

    [(s-extend s super fields) (begin (wf super)
                                      (map wf-member fields))]
    [(s-update s super fields) (begin (wf super)
                                      (map wf-member fields))]

    [(s-obj s fields) (map wf-member fields)]

    [(s-list s elts) (map wf elts)]

    [(s-dot s val field) (wf val)]

    [(s-get-bang s val field) (wf val)]

    [(s-bracket s val field) (begin (wf val) (wf field))]

    [(s-colon s obj field) (wf obj)]

    [(s-colon-bracket s obj field) (begin (wf obj) (wf field))]

    [(s-paren s e) (wf e)]

    [(s-id s id)
     (cond
      [(equal? id 'check)
       (wf-error "Cannot use `check` as an identifier." s)]
      [(equal? id 'where)
       (wf-error "Cannot use `where` as an identifier." s)]
      [else #t])]

    [(or (s-num _ _)
         (s-bool _ _)
         (s-str _ _)) #t]

    [else (error (format "Missed a case in well-formed: ~a"
                         ast))]))
