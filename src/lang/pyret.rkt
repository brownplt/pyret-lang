#lang racket

(require "ast.rkt")
(provide (all-from-out racket) (all-defined-out))

;; borrowed from dyoo's brainfudge
(define-for-syntax (srcloc-of-syntax stx)
  (quasisyntax/loc stx
    (srcloc '#,(syntax-source stx)
            '#,(syntax-line stx)
            '#,(syntax-column stx)
            '#,(syntax-position stx)
            '#,(syntax-span stx))))
  
(define-syntax (program stx)
  (syntax-case stx ()
    [(_ block endmarker-ignored)
     #'block]))

(define-syntax (block stx)
  (syntax-case stx ()
    [(_ stmt ...)
     #`(s-block #,(srcloc-of-syntax stx) (list stmt ...))]))

(define-syntax (expr stx)
  (syntax-case stx ()
    [(_ the-expr) #'the-expr]))

(define-syntax (stmt stx)
  (syntax-case stx ()
    [(_ stmt) #'stmt]))

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
         #`(s-str #,(srcloc-of-syntax stx) s)))]))

(define-syntax (num-expr stx)
  (syntax-case stx ()
    [(_ num) (with-syntax ([n (datum->syntax #'id-expr (string->number (syntax->datum #'num)))])
               #`(s-num #,(srcloc-of-syntax stx) n))]))

;; We don't parse the special method sugar yet
(define-syntax (obj-expr stx)
  (syntax-case stx (field list-field)
    [(_ "{" (list-field (field key ":" value) ",") ... (field lastkey ":" lastvalue) "}")
     #`(s-obj #,(srcloc-of-syntax stx)
              (append (list (data-field key value) ...)
                      (list (data-field lastkey lastvalue))))]
    [(_ "{" "}") #`(s-obj #,(srcloc-of-syntax stx)
                          empty)]))

(define-syntax (data-field stx)
  (syntax-case stx ()
    [(_ key value)
     #`(s-data #,(srcloc-of-syntax stx) key value)]))

(define-syntax (id-expr stx)
  (syntax-case stx ()
    [(_ x)
     (with-syntax ([x-id (datum->syntax #'#%module-begin (string->symbol (syntax->datum #'x)))])
       #`(s-id #,(srcloc-of-syntax stx) 'x-id))]))

;; TODO(joe): there's extra crap in here
(define-syntax (app-expr stx)
  (syntax-case stx (app-arg-elt)
    [(_ fun-expr (app-args "(" (app-arg-elt arg ",") ... lastarg ")"))
     #`(s-app #,(srcloc-of-syntax stx) fun-expr (list arg ... lastarg))]
    [(_ fun-expr (app-args "(" ")"))
     #`(s-app #,(srcloc-of-syntax stx) fun-expr empty)]))

(define-syntax (fun-expr stx)
  (syntax-case stx (args arg-elt)
    [(_ "fun" fun-name (args "(" (arg-elt arg ",") ... lastarg ")") ":" body "end")
      (with-syntax ([f-id (datum->syntax #'#%module-begin (string->symbol (syntax->datum #'fun-name)))]
                    [(the-args ...) (datum->syntax #'#%module-begin (map string->symbol (syntax->datum #'(arg ...))))]
                    [the-last-arg (datum->syntax #'#%module-begin (string->symbol (syntax->datum #'lastarg)))])
        #`(s-fun #,(srcloc-of-syntax stx) 'f-id (list 'the-args ... 'the-last-arg) body))]
    [(_ "fun" fun-name (args "(" ")") ":" body "end")
      (with-syntax ([f-id (datum->syntax #'fun-expr (string->symbol (syntax->datum #'fun-name)))])
        #`(s-fun #,(srcloc-of-syntax stx) 'f-id empty body))]))

(define-syntax (list-expr stx)
  (syntax-case stx (list-elt)
    [(_ "[" (list-elt expr ",") ... lastexpr "]")
     #`(s-list #,(srcloc-of-syntax stx) (list expr ... lastexpr))]
    [(_ "[" "]") #`(s-list #,(srcloc-of-syntax stx) empty)]))
