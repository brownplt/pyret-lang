#lang racket/base

(require
  (for-syntax racket/base)
  (except-in "../runtime.rkt" raise)
  "../string-map.rkt"
  "../ffi-helpers.rkt")

(provide (rename-out [export %PYRET-PROVIDE]))

(struct s:immutable-string-dict (d))
(struct s:string-dict (d))

(define (get-racket-dict d)
  (cond
    [(s:immutable-string-dict? d) (s:immutable-string-dict-d d)]
    [(s:string-dict? d) (s:string-dict-d d)]
    [else (p:throw-type-error! "StringDict" d)]))

(define (gf obj)
  (p:p-opaque-val (p:get-raw-field p:dummy-loc obj "the-dict")))

(define-syntax (wrap-method stx)
  (syntax-case stx ()
    [(_ (arg ...) e ...)
     #'(p:pμ (arg ...) "" (let [(arg (ffi-unwrap arg)) ...] e ...))]))

(define (string-dict-obj the-dict)
  (p:mk-object
    (make-string-map (list
      (cons "_equals" (wrap-method (v other)
        (ffi-wrap (and
          (p:has-field? v "the-dict")
          (p:has-field? other "the-dict")
          (foldl (lambda (pair acc)
            (and acc
              (p:apply-fun
                (p:get-field p:dummy-loc (cdr pair) "_equals")
                p:dummy-loc
                (hash-ref (get-racket-dict (gf other)) (car pair)))))
            #t
            (hash->list (get-racket-dict (gf v))))))))
      (cons "the-dict" (p:p-opaque the-dict))
      (cons "has-key"
        (wrap-method (v s)
          (dict-has-key (gf v) s)))
      (cons "get"
        (wrap-method (v s)
          (dict-get (gf v) s)))
      (cons "set"
        (wrap-method (v s nv)
          (dict-set (gf v) s nv)))
      (cons "keys"
        (wrap-method (v)
          (dict-keys (gf v))))))))

(define to-dict (ffi-wrap (lambda (v)
  (define the-hash (make-immutable-hash (list)))
  (define internal-dict (string-map-map (p:get-dict v) (lambda (k v)
    (set! the-hash (hash-set the-hash k v)))))
  (string-dict-obj (s:immutable-string-dict the-hash)))))

(define immutable-string-dict (ffi-wrap (lambda ()
  (define the-dict (s:immutable-string-dict (make-immutable-hash (list))))
  (string-dict-obj the-dict))))

(define string-dict (ffi-wrap (lambda ()
  (define the-dict (s:string-dict (make-hash (list))))
  (string-dict-obj the-dict))))

(define (dict-get v s)
  (when (not (string? s))
    (p:throw-type-error! "String" (ffi-wrap s)))
  (cond
    [(s:immutable-string-dict? v)
     (hash-ref (s:immutable-string-dict-d v) s)]
    [(s:string-dict? v)
     (hash-ref (s:string-dict-d v) s)]))

(define (dict-set v s nv)
  (when (not (string? s))
    (p:throw-type-error! "String" (ffi-wrap s)))
  (cond
    [(s:immutable-string-dict? v)
     (string-dict-obj (s:immutable-string-dict (hash-set (s:immutable-string-dict-d v) s (ffi-wrap nv))))]
    [(s:string-dict? v)
     (hash-set! (s:string-dict-d v) s (ffi-wrap nv))
     nothing]
    [else (p:throw-type-error! "StringDict" v)]))

(define (dict-has-key v s)
  (when (not (string? s))
    (p:throw-type-error! "String" (ffi-wrap s)))
  (cond
    [(s:immutable-string-dict? v) (ffi-wrap (hash-has-key? (s:immutable-string-dict-d v) s))]
    [(s:string-dict? v) (ffi-wrap (hash-has-key? (s:string-dict-d v) s))]
    [else (p:throw-type-error! "StringDict" (ffi-wrap v))]))

(define (dict-keys v)
  (cond
    [(s:immutable-string-dict? v) (ffi-wrap (hash-keys (s:immutable-string-dict-d v)))]
    [(s:string-dict? v) (ffi-wrap (hash-keys (s:string-dict-d v)))]))

(define StringDict (ffi-wrap (lambda (v)
  (and
    (p:has-field? v "the-dict")
    (let [(val (gf v))]
      (or (s:immutable-string-dict? val) (s:string-dict? val)))))))

(define export (p:mk-object
  (make-string-map (list
    (cons "StringDict" StringDict)
    (cons "to-dict" to-dict)
    (cons "immutable-string-dict" immutable-string-dict)
    (cons "string-dict" string-dict)))))

