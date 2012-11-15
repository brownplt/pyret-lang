#lang racket

(require "../values.rkt")
(provide (all-from-out racket) (all-defined-out))



(define-syntax (program stx)
  (syntax-case stx ()
    [(_ the-program endmarker-ignored) #'the-program]))

(define-syntax (expr stx)
  (syntax-case stx ()
    [(_ the-expr) #'the-expr]))

(define-syntax (stmt stx)
  (syntax-case stx ()
    [(_ expr ...) #'(begin expr ...)]))

(define-syntax (prim-expr stx)
  (syntax-case stx ()
    [(_ expr) #'expr]))

;; Amusingly, we get "'foo'" for the string 'foo' in the source, so cut off
;; the surrounding quotes
(define-syntax (string-expr stx)
  (syntax-case stx ()
    [(_ str)
     (let* [(str-val (syntax->datum #'str))
            (real-str (substring str-val 1 (sub1 (string-length str-val))))]
       (with-syntax ([s (datum->syntax #'id-expr real-str)])
         #'(p-str s (none) (make-hash))))]))

(define-syntax (num-expr stx)
  (syntax-case stx ()
    [(_ num) (with-syntax ([n (datum->syntax #'id-expr (string->number (syntax->datum #'num)))])
               #'(p-num n (none) (make-hash)))]))

(define-syntax (obj-expr stx)
  (syntax-case stx ()
    [(_ "{" (list-field (field1 key ":" value) ",") ... (field2 lastkey ":" lastvalue) "}")
     #'(p-object (none) (make-hash (append (list (cons key value) ...)
                                           (list (cons lastkey lastvalue)))))]
    [(_ "{" "}") #'(p-object (none) (make-hash))]))

(define-syntax (list-expr stx)
  (syntax-case stx (list-elt)
    [(_ "[" (list-elt expr ",") ... lastexpr "]")
     #'(p-list (list expr ... lastexpr) (none) (make-hash))]
    [(_ "[" "]") #'(p-list empty (none) (make-hash))]))



(define-syntax (id-expr stx)
  (syntax-case stx ()
    [(_ x)
     (with-syntax ([x-id (datum->syntax #'id-expr (string->symbol (syntax->datum #'x)))])
       #'x-id)]))

(define-syntax (app-expr stx)
  (syntax-case stx (app-arg-elt)
    [(_ fun-expr (app-args "(" (app-arg-elt arg ",") ... lastarg ")"))
     #'(fun-expr arg ... lastarg)]
    [(_ fun-expr (app-args "(" ")")) #'(fun-expr)]))


#;(define-syntax (fun-expr stx)
  (syntax-case stx (args arg-elt)
    [(_ "fun" fun-name (args "(" (arg-elt arg ",") ... lastarg ")") ":" body "end")
     #'(define (fun-name arg ... lastarg) body)]
    [(_ "fun" fun-name (args "(" ")") ":" body "end")
     (with-syntax ([fun-name-stx (string->symbol (syntax->datum (syntax fun-name)))])
       #'(define (fun-name-stx) body))]))

