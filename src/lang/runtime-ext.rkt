#lang racket/base

(require
  racket/list
  racket/match
  "runtime.rkt")

(provide Racket
         (except-out (all-from-out "runtime.rkt") p:get-field)
         (rename-out [get-field-ext p:get-field]))

(define Racket (p:mk-object p:empty-dict))

;; Primitives that are allowed from Pyret land.  Others must be
;; wrapped in opaques.  This may be extended for lists and other
;; Racket built-in types in the future.
(define (allowed-prim? v)
  (or (number? v)
      (string? v)
      (boolean? v)))

(define (apply-racket-fun package-name package-member args)
  (define package (string->symbol package-name))
  (define fun (dynamic-require package (string->symbol package-member)))
  (define (get-val arg)
    (cond
      [(p:p-opaque? arg) (p:p-opaque-val arg)]
      [(allowed-prim? arg) arg]
      [else (error (format "apply-racket-fun: Bad argument ~a." arg))]))
  (define result (apply fun (map get-val args)))
  (cond
    [(allowed-prim? result)  result]
    [else (p:p-opaque result)]))

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

