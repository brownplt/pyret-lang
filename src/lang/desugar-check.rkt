#lang racket/base

(provide desugar-check)
(require
  racket/list
  racket/match
  racket/function
  "ast.rkt")

(struct check-info (srcloc name check-body))

(define (get-checks stmts)
  (define (add-check stmt lst)
    (match stmt
      [(s-fun s name _ _ _ _ _ check)
       (cons (check-info s name check) lst)]
      [(s-data s name _ _ _ check)
       (cons (check-info s name check) lst)]
      [_ lst]))
  (foldr add-check empty stmts))

;; srcloc (listof check-info) -> s-block
(define (create-check-block s checks)
  (define (create-checker check)
    (match check
      [(check-info s name body)
       (define source (srcloc-source s))
       (define line (srcloc-line s))
       (define col (srcloc-column s))
       (define srcloc-str (format "~a:~a:~a" source line col))
       (define check-fun
        (s-lam s
               empty
               empty
               (a-blank)
               (format "~a: Tests for ~a" srcloc-str name)
               body
               (s-block s empty)))
       (s-obj s (list (s-data-field s (s-str s "name") (s-str s (symbol->string name)))
                      (s-data-field s (s-str s "run") check-fun)
                      (s-data-field s (s-str s "location")
                        (s-obj s (list
                                  (s-data-field s (s-str s "file") (s-str s (symbol->string (src->module-name source))))
                                  (s-data-field s (s-str s "line") (s-num s line))
                                  (s-data-field s (s-str s "column") (s-num s col)))))))]))
  (define checkers (map create-checker checks))
  (s-block s
    (list
      (s-app s (s-bracket s (s-id s 'checkers) (s-str s "run-checks"))
               (list (s-list s checkers))))))
  

(define (desugar-check/internal ast)
  (define ds desugar-check/internal)
  (define (ds-if-branch branch)
    (match branch
      [(s-if-branch s tst blk) (s-if-branch s (ds tst) (ds blk))]))
  (define (ds-cases-branch branch)
    (match branch
      [(s-cases-branch s name args blk)
       (s-cases-branch s name (map ds-bind args) (ds blk))]))
  (define (ds-ann ast)
    (match ast
      [(a-pred s t e) (a-pred s t (ds e))]
      [_ ast]))
  (define (ds-bind b)
    (match b
     [(s-bind s name ann) (s-bind s name (ds-ann ann))]))
  (define (ds-variant var)
    (match var
     [(s-singleton-variant s name members)
      (s-singleton-variant s name (map ds-member members))]
     [(s-variant s name binds members)
      (s-variant s name (map ds-bind binds) (map ds-member members))]))
  (define (ds-member mem)
    (match mem
     [(s-data-field s name val) (s-data-field s (ds name) (ds val))]
     [(s-method-field s name args ann doc body check)
      (s-method-field s name (map ds-bind args) (ds-ann ann) doc (ds body) (s-block s (list)))]))
  (match ast
    [(s-block s stmts)
     (define flat-stmts (flatten-blocks stmts))
     (define checks-to-perform (get-checks flat-stmts))
     (define ds-stmts (map ds flat-stmts))
     (define do-checks (create-check-block s checks-to-perform))
     (cond
      [(empty? ds-stmts) 
       (s-block s (list do-checks (s-id s 'nothing)))]
      [(cons? ds-stmts)
       (define id-result (gensym 'result-after-checks))
       (define last-expr (last ds-stmts))
       (s-block s
        (append
          (take ds-stmts (- (length ds-stmts) 1))
          (list
              (s-let s (s-bind s id-result (a-blank)) last-expr)
              do-checks
              (s-id s id-result))))])]
    [(s-data s name params variants shares check)
     (s-data s name params
             (map ds-variant variants)
             (map ds-member shares)
             (s-block s (list)))]

    [(s-for s iter bindings ann body)
     (define (ds-for-bind b)
      (match b
        [(s-for-bind s bind value)
         (s-for-bind s (ds-bind bind) (ds value))]
        [else (error (format "FATAL: Not an s-for-bind: ~a" b))]))
     (s-for s (ds iter) (map ds-for-bind bindings) (ds-ann ann) (ds body))]

    [(s-var s name val) (s-var s (ds-bind name) (ds val))]
    [(s-let s name val) (s-let s (ds-bind name) (ds val))]

    [(s-fun s name typarams args ann doc body check)
     (s-fun s name typarams (map ds-bind args) (ds-ann ann) doc (ds body) (s-block s (list)))]

    [(s-lam s typarams args ann doc body check)
     (s-lam s typarams (map ds-bind args) (ds-ann ann) doc (ds body) (s-block s (list)))]

    [(s-method s args ann doc body check)
     (s-method s (map ds-bind args) (ds-ann ann) doc (ds body) (s-block s (list)))]

    [(s-when s test body)
     (s-when s (ds test) (ds body))]

    [(s-if s if-bs) (s-if s (map ds-if-branch if-bs))]
    [(s-if-else s if-bs else) (s-if-else s (map ds-if-branch if-bs) (ds else))]

    [(s-cases s type val c-bs)
     (s-cases s (ds type) (ds val) (map ds-cases-branch c-bs))]
    [(s-cases-else s type val c-bs else)
     (s-cases-else s (ds type) (ds val) (map ds-cases-branch c-bs) (ds else))]

    [(s-try s try x exn) (s-try s (ds try) x (ds exn))]

    [(s-assign s name expr) (s-assign s name (ds expr))]

    [(s-app s fun args) (s-app s (ds fun) (map ds args))]

    [(s-left-app s target fun args)
     (s-left-app s (ds target) (ds fun) (map ds args))]

    [(s-extend s super fields) (s-extend s (ds super) (map ds-member fields))]

    [(s-obj s fields) (s-obj s (map ds-member fields))]

    [(s-list s elts) (s-list s (map ds elts))]

    [(s-dot s val field) (s-dot s (ds val) field)]

    [(s-bracket s val field) (s-bracket s (ds val) (ds field))]

    [(s-colon s obj field) (s-colon s (ds obj) field)]

    [(s-colon-bracket s obj field) (s-colon-bracket s (ds obj) (ds field))]

    [(s-paren s e) (s-paren s (ds e))]

    [(s-not s e) (s-not s (ds e))]
    
    [(s-op s op e1 e2) (s-op s op (ds e1) (ds e2))]

    [(or (s-num _ _)
         (s-bool _ _)
         (s-str _ _)
         (s-id _ _)) ast]

    [else (error (format "Missed a case in desugaring checks: ~a" ast))]))

(define (desugar-check ast)
  (match ast
    [(s-prog s imports (s-block s2 stmts))
     (define no-provides (filter (negate s-provide?) imports))
     ;; NOTE(joe, dbp): This is somewhere between a hack and a reasonable solution.
     ;; The toplevel may end in a statement that we cannot let-bind (which is
     ;; what desugar-check/internal will try to do), so we add a nothing at
     ;; the end
     (define with-checks (desugar-check/internal (s-block s2 (append stmts (list (s-id s2 'nothing))))))
     (define get-results (s-app s (s-dot s (s-id s 'checkers) 'get-results) empty))
     (define clear (s-app s (s-dot s (s-id s 'checkers) 'clear-results) empty))
     (s-prog s no-provides (s-block s (append (list clear) (s-block-stmts with-checks) (list get-results))))]
    [ast (desugar-check/internal ast)]))

