#lang racket/base

(require
  racket/list
  racket/match
  (only-in math uniform-dist sample)
  (only-in 2htdp/image
    place-image
    circle
    rectangle
    star
    triangle
    isosceles-triangle
    rotate)
  "big-bang.rkt"
  "runtime.rkt")

(provide Racket
         Imports
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

;; mk-racket-fun : String -> Value
(define (mk-racket-fun f)
  (printf "mkking racket-fun: ~a\n" f)
  (p:mk-fun-nodoc
    (λ args
      (printf "calling racket-fun: ~a\n" f)
      (match (first args)
        [(p:p-str _ _ _ s)
         (p:wrap (apply-racket-fun f s (map p:unwrap (rest args))))]
        [else
         (error (format "Racket: expected string as first argument, got ~a" (first args)))]))))

(define (wrap-racket-fun f)
  (p:mk-fun-nodoc (λ args (p:wrap (wrap-racket-value (apply f (map get-val (map p:unwrap args))))))))

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

(define image-dict
  (make-immutable-hash
    (list
      (cons "place-image" (wrap-racket-fun place-image))
      (cons "circle" (wrap-racket-fun circle))
      (cons "star" (wrap-racket-fun star))
      (cons "isosceles-triangle" (wrap-racket-fun isosceles-triangle))
      (cons "triangle" (wrap-racket-fun triangle))
      (cons "rotate" (wrap-racket-fun rotate))
      (cons "rectangle" (wrap-racket-fun rectangle)))))

(define imports-dict
  (make-immutable-hash
    (list
      (cons "image" (p:mk-object image-dict))
      (cons "math" (p:mk-object math-dict)))))

(define Imports (p:mk-object imports-dict))

(define (big-bang loc)
  (lambda args
    (define (wrap-for-racket-callback k f)
      (cond
        [(equal? k "to-draw")
         (lambda (world) (p:p-opaque-val ((p:check-fun f loc) world)))]
        [(equal? k "stop-when")
         (lambda (world) (p:unwrap ((p:check-fun f loc) world)))] 
        [(equal? k "on-tick")
         (lambda (world) ((p:check-fun f loc) world))]
        [else (raise (p:pyret-error loc "big-bang-no-impl"
                      (format "No implementation for big-bang handler ~a" k)))]))
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

