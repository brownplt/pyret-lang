#lang racket/base

(require
  racket/list
  racket/match
  (only-in math uniform-dist sample)
  "runtime.rkt")

(provide Racket
         Imports
         (except-out (all-from-out "runtime.rkt") p:get-field)
         (rename-out
          [get-field-ext p:get-field]))

(define Racket (p:mk-object p:empty-dict))

;; Primitives that are allowed from Pyret land.  Others must be
;; wrapped in opaques.  This may be extended for lists and other
;; Racket built-in types in the future.
(define (allowed-prim? v)
  (or (number? v)
      (string? v)
      (boolean? v)))

(define (wrap-racket-value val)
  (cond
   [(allowed-prim? val)  val]
   [else (p:p-opaque val)]))
(define (get-val arg)
  (cond
    [(p:p-opaque? arg) (p:p-opaque-val arg)]
    [(allowed-prim? arg) arg]
    [else (error (format "apply-racket-fun: Bad argument ~a." arg))]))

(define (apply-racket-fun package-name package-member args)
  (define package (string->symbol package-name))
  (define fun (dynamic-require package (string->symbol package-member)))
  (define result (apply fun (map get-val args)))
  (wrap-racket-value result))


(define (wrap-racket-fun f)
  (p:mk-fun-nodoc (λ args (p:wrap (wrap-racket-value (apply f (map get-val (map p:unwrap args))))))))

;; mk-racket-fun : String -> Value
(define (mk-racket-fun f)
  (p:mk-fun-nodoc
    (λ args
      (match (first args)
        [(p:p-str _ _ _ s)
         (p:wrap (apply-racket-fun f s (map p:unwrap (rest args))))]
        [else
         (error (format "Racket: expected string as first argument, got ~a" (first args)))]))))

;; get-field : Loc Value String -> Value
(define (get-field-ext loc v f)
  (if (eq? v Racket)
      (mk-racket-fun f)
      (p:get-field loc v f)))

(define math-dict
  (make-immutable-hash
    (list
      (cons "uniform-dist" (wrap-racket-fun uniform-dist))
      (cons "sample" (wrap-racket-fun sample)))))

(define imports-dict
  (make-immutable-hash
    (list
      (cons "math" (p:mk-object math-dict)))))

(define Imports (p:mk-object imports-dict))

