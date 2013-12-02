#lang racket/base

(require
  racket/match
  racket/list
  "ast.rkt")

(provide indentation-check (struct-out exn:fail:pyret/indent))

(struct exn:fail:pyret/indent exn:fail (srclocs)
  #:property prop:exn:srclocs
    (lambda (a-struct)
      (exn:fail:pyret/indent-srclocs a-struct)))

(define (indent-error str . locs)
  (raise (exn:fail:pyret/indent (format "indentation: ~a" str) (continuation-marks #f) locs)))

;; NOTE(dbp 2013-08-14): Currently, the following things are prohibited:
;;
;; - multiple statements on one line

(define (indentation-check ast)
  (match ast
    [(s-prog s _ ast)
     (indentation-check/internal ast)]
    [_ (indentation-check/internal ast)])
  ast)


(define (verify-distinct-lines stmts)
  (define start-loc (get-srcloc (first stmts)))
  (foldl (λ (prev-loc cur-loc)
            (when (equal? (srcloc-line cur-loc)
                          (srcloc-line prev-loc))
                (indent-error
                 "Expected to find one statement per line, but found multiple statements on this line."
                 prev-loc
                 cur-loc))
             prev-loc)
         start-loc
         (map get-srcloc (rest stmts))))

(define (indentation-check/internal ast)
  (define (ic ast) (indentation-check/internal ast))
  (match ast
    [(s-block s stmts) (begin (cond
                               [(empty? stmts) #t]
                               [else (verify-distinct-lines stmts)])
                              (map ic stmts))]
    ;; NOTE(dbp 2013-08-14): The rest is just structural recursion
    [(s-bind syntax id ann) syntax]
    [(s-fun syntax name params args ann doc body check)
     (begin (ic body) (ic check))]
    [(s-check syntax body) (ic body)]
    [(s-var syntax name value) (ic value)]
    [(s-let syntax name value) (ic value)]
    [(s-user-block s body) (ic body)]
    [(s-when syntax test block) (begin (ic test) (ic block))]
    [(s-if syntax branches) (map ic branches)]
    [(s-if-else syntax branches else) (begin (map ic branches)
                                             (ic else))]
    [(s-if-branch syntax expr body) (begin (ic expr) (ic body))]
    [(s-try syntax body id except) (begin (ic body) (ic except))]
    [(s-cases syntax type val branches) (begin (ic val)
                                               (map ic branches))]
    [(s-cases-else syntax type val branches else)
     (begin (ic val)
            (map ic branches)
            (ic else))]
    [(s-cases-branch syntax name args body) (ic body)]
    [(s-op syntax op left right) (begin (ic left) (ic right))]
    [(s-check-test syntax op left right) (begin (ic left) (ic right))]
    [(s-not syntax expr) (ic expr)]
    [(s-paren syntax expr) (ic expr)]
    [(s-lam syntax typarams args ann doc body check)
     (begin (ic body) (ic check))]
    [(s-method syntax args ann doc body check)
     (begin (ic body) (ic check))]
    [(s-data-field syntax name value) (ic name) (ic value)]
    [(s-mutable-field syntax name ann value) (ic name) (ic value)]
    [(s-method-field syntax name args ann doc body check)
     (begin (ic name) (ic body) (ic check))]
    [(s-extend syntax super fields) (begin (ic super) (map ic fields))]
    [(s-update syntax super fields) (begin (ic super) (map ic fields))]
    [(s-obj syntax fields) (map ic fields)]
    [(s-list syntax values) (map ic values)]
    [(s-app syntax params fun args) (begin (ic fun) (map ic args))]
    [(s-left-app syntax obj fun args) (begin (ic obj) (ic fun)
                                             (map ic args))]
    [(s-assign syntax id value) (ic value)]
    [(s-dot syntax obj field) (ic obj)]
    [(s-get-bang syntax obj field) (ic obj)]
    [(s-bracket syntax obj field) (begin (ic obj) (ic field))]
    [(s-colon syntax obj field) (ic obj)]
    [(s-colon-bracket syntax obj field) (begin (ic obj) (ic field))]
    [(s-data syntax name params mixins variants shared-members check)
     (begin (map ic variants) (map ic mixins) (map ic shared-members) (ic check))]
    [(s-variant syntax name binds with-members)
     (map ic with-members)]
    [(s-singleton-variant syntax name with-members)
     (map ic with-members)]
    [(s-for-bind syntax bind value) (ic value)]
    [(s-for syntax iterator bindings ann body)
     (begin (ic iterator) (map ic bindings) (ic body))]
    [_ #t]))
