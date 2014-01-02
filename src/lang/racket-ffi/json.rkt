#lang racket/base

(require
 (except-in "../runtime.rkt" raise)
 "../string-map.rkt"
 "../ffi-helpers.rkt"
 json
 racket/set
 racket/port)

(provide (rename-out [json %PYRET-PROVIDE]))

(define (stringify val)
  (define jsexpr (to-jsexpr val))
  (ffi-wrap (jsexpr->string jsexpr)))

(define (parse str)
  (define jsexpr (string->jsexpr (p:p-str-s str)))
  (from-jsexpr jsexpr))

(define json
  (p:mk-object
    (make-string-map
      (list
        (cons "stringify" (p:mk-fun-nodoc-slow stringify))
        (cons "parse" (p:mk-fun-nodoc-slow parse))))))

(define (from-jsexpr val)
  (cond
    [(or
      (number? val)
      (string? val)
      (boolean? val)) (ffi-wrap val)]
    [(list? val) (create-pyret-list val from-jsexpr)]
    [(hash? val)
     (p:mk-object
      (make-string-map
        (hash-map val
          (lambda (key v)
            (cons (symbol->string key) (from-jsexpr v))))))]
    [else
     (raise (format "Cannot turn into value: ~a" val))]))


(define (to-jsexpr val)
  (define seen-vals (seteq))
  (define (check-and-add)
    (when (set-member? seen-vals val)
      (raise (format "Cyclic value in json.stringify: ~a" val)))
    (set! seen-vals (set-add seen-vals val)))
  (cond
    [(or
      (p:p-num? val)
      (p:p-str? val)
      (p:p-bool? val)) (ffi-unwrap val)]
    [(pyret-list? val)
     (check-and-add)
     (map to-jsexpr (pyret-list->list val))]
    [(p:p-object? val)
     (check-and-add)
     (define fields
       (string-map-map
         (p:get-dict val)
         (lambda (key val)
           (cons (string->symbol key) (to-jsexpr val)))))
     (make-hash fields)]
    [else
     (raise (format "Cannot stringify: ~a" val))]))

