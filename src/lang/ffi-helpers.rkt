#lang whalesong

(require "runtime.rkt"
         (rename-in "pyret-lib/moorings.rkt" (%PYRET-PROVIDE pyret-moorings)))
(provide (all-defined-out))

(define pyret-list (p:get-field p:dummy-loc pyret-moorings "list"))

(define (allowed-prim? v)
  (or (number? v)
      (string? v)
      (boolean? v)))

(define (wrap-racket-value val)
  (cond
   [(allowed-prim? val)  val]
   [(list? val) (map wrap-racket-value val)]
   [else (p:p-opaque val)]))

(define (ffi-wrap val)
  (cond
    [(procedure? val)
     (p:mk-fun-nodoc
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
       [(p:p-fun _ __ f)
        (lambda args (ffi-unwrap (apply (f p:dummy-loc) (map ffi-wrap args))))]
       [(p:p-num _ __ n) n]
       [(p:p-str _ __ s) s]
       [(p:p-bool _ __ b) b]
       [(p:p-object _ __)
        (if (pyret-list? val)
            (map ffi-unwrap (p:structural-list->list val))
            val)]
       [(default _) val])]))

(define (wrap-racket-fun f)
  (p:mk-fun-nodoc (λ args (ffi-wrap (wrap-racket-value (apply f (map ffi-unwrap args)))))))

(define (get-val arg)
  (cond
    [(p:p-opaque? arg) (p:p-opaque-val arg)]
    [(list? arg) (map get-val arg)]
    [(allowed-prim? arg) arg]
    [else (error (format "apply-racket-fun: Bad argument ~a." arg))]))

(define (create-pyret-list l)
  (define d (p:get-dict pyret-list))
  (define link (hash-ref d "link"))
  (define empty (hash-ref d "empty"))
  (foldr (λ (elem lst) (p:apply-fun link p:dummy-loc (ffi-wrap elem) lst)) empty l))

(define (pyret-list? l)
  (define is-list (p:get-raw-field p:dummy-loc pyret-list "List"))
  (ffi-unwrap (p:apply-fun is-list p:dummy-loc l)))
