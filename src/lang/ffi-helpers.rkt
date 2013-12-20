#lang whalesong

(require "runtime.rkt"
         "string-map.rkt"
         "type-env.rkt"
         "ast.rkt"
         (rename-in "pyret-lib/moorings.rkt" (%PYRET-PROVIDE pyret-moorings)))
(provide (all-defined-out))

(define pyret-list (p:get-field p:dummy-loc pyret-moorings "list"))

(define (ffi-wrap val)
  (cond
    [(procedure? val)
     (p:mk-fun-nodoc-slow
      (lambda args
        (ffi-wrap (apply val (map ffi-unwrap args)))))]
    [(number? val) (p:mk-num val)]
    [(string? val) (p:mk-str val)]
    [(boolean? val) (p:mk-bool val)]
    [(list? val) (create-pyret-list val)]
    [(p:p-base? val) val]
    [(p:p-opaque? val) val]
    [else (p:p-opaque val)]))

(define (ffi-unwrap val)
  (cond
    [(p:p-opaque? val) (p:p-opaque-val val)]
    [else
     (p:py-match val
       [(p:p-fun _ _ f _)
        (lambda args (ffi-unwrap (apply f (map ffi-wrap args))))]
       [(p:p-num _ _ _ _ n) n]
       [(p:p-str _ _ _ _ s) s]
       [(p:p-bool _ _ _ _ b) b]
       [(p:p-object _ _ _ _)
        (if (pyret-list? val)
            (map ffi-unwrap (pyret-list->list val))
            val)]
       [(default _) val])]))

(define (pyret-list->list v)
  (define d (p:get-dict pyret-list))
  (define is-link (string-map-ref d "is-link"))
  (define is-link? (λ (v) (ffi-unwrap (p:apply-fun is-link p:dummy-loc v))))
  (define (help acc val)
    (cond
     [(is-link? val)
      (define val-dict (p:get-dict val))
      (help (cons (string-map-ref val-dict "first") acc) (string-map-ref val-dict "rest"))]
     [else (reverse acc)]))
  (help (list) v))

(define (create-pyret-list l (wrapper ffi-wrap))
  (define d (p:get-dict pyret-list))
  (define link (string-map-ref d "link"))
  (define empty (string-map-ref d "empty"))
  (foldr (λ (elem lst) (p:apply-fun link p:dummy-loc (wrapper elem) lst)) empty l))

(define (pyret-list? l)
  (define is-list (p:get-raw-field p:dummy-loc pyret-list "List"))
  (ffi-unwrap (p:apply-fun is-list p:dummy-loc l)))

(define (extend-env-with-dict env dict)
  (foldr
    (lambda (key running-env)
      (update (string->symbol key) (binding p:dummy-loc (a-any) #f) running-env))
    env
    (string-map-keys dict)))

