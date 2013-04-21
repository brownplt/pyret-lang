#lang racket/base

(require
  racket/list
  racket/match
  "big-bang.rkt"
  "runtime.rkt")

(provide Racket
         (except-out (all-from-out "runtime.rkt") p:get-field)
         (rename-out
          [get-field-ext p:get-field]
          [big-bang-pfun big-bang]))

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


(define (big-bang loc)
  (lambda args
    (define (wrap-for-racket-callback k f)
      (define ((f-for-racket unwrapper) . callback-args)
        (unwrapper
          (apply p:apply-fun
            (append (list f loc) callback-args))))
      (cond
        [(equal? k "to-draw") (f-for-racket p:p-opaque-val)]
        [(equal? k "stop-when") (f-for-racket p:unwrap)]
        [else (f-for-racket (lambda (x) x))]))
    (match (second args)
      [(p:p-object _ _ d)
       (define hash-for-bb
         (for/hash ((k (hash-keys d)))
          (values (string->symbol k) (wrap-for-racket-callback k (hash-ref d k)))))
       (define my-world (my-bb (first args) hash-for-bb))
       (run-it my-world)]
      [v (raise (p:pyret-error loc "big-bang-non-object"
                     (format "Non-object given to big bang: ~a" (p:to-string v))))])))

(define big-bang-pfun (p:mk-internal-fun big-bang))

