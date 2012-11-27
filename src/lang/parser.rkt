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

(define-for-syntax (parse-name stx)
  (string->symbol (syntax->datum stx)))

(define-for-syntax (parse-id stx)
  (datum->syntax #'#%module-begin (string->symbol (syntax->datum stx))))

(define-for-syntax (parse-ids stx)
  (datum->syntax #'#%module-begin (map string->symbol (syntax->datum stx))))

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

(define-syntax (bool-expr stx)
  (syntax-case stx ()
    [(_ "true") #`(s-bool #,(srcloc-of-syntax stx) #t)]
    [(_ "false") #`(s-bool #,(srcloc-of-syntax stx) #f)]))

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
     (with-syntax ([x-id (parse-id #'x)])
       #`(s-id #,(srcloc-of-syntax stx) 'x-id))]))

(define-syntax (assign-expr stx)
  (syntax-case stx ()
    [(_ x "=" expr)
     (with-syntax ([x-id (parse-id #'x)])
       #`(s-assign #,(srcloc-of-syntax stx) 'x-id expr))]))

;; TODO(joe): there's extra crap in here
(define-syntax (app-expr stx)
  (syntax-case stx (app-arg-elt)
    [(_ fun-expr (app-args "(" (app-arg-elt arg ",") ... lastarg ")"))
     #`(s-app #,(srcloc-of-syntax stx) fun-expr (list arg ... lastarg))]
    [(_ fun-expr (app-args "(" ")"))
     #`(s-app #,(srcloc-of-syntax stx) fun-expr empty)]))

(define-syntax (def-expr stx)
  (syntax-case stx ()
    [(_ "def" id ":" value-expr)
     #`(s-def #,(srcloc-of-syntax stx) '#,(parse-id #'id) value-expr)]))

(define-syntax (fun-expr stx)
  (syntax-case stx (args arg-elt)
    [(_ "fun" fun-name (args "(" (arg-elt arg ",") ... lastarg ")") ":" body "end")
      (with-syntax ([f-id (parse-id #'fun-name)]
                    [(the-args ...) (parse-ids #'(arg ...))]
                    [the-last-arg (parse-id #'lastarg)])
        #`(s-fun #,(srcloc-of-syntax stx) 'f-id (list 'the-args ... 'the-last-arg) body))]
    [(_ "fun" fun-name (args "(" ")") ":" body "end")
      (with-syntax ([f-id (parse-id #'fun-name)])
        #`(s-fun #,(srcloc-of-syntax stx) 'f-id empty body))]))

(define-syntax (list-expr stx)
  (syntax-case stx (list-elt)
    [(_ "[" (list-elt expr ",") ... lastexpr "]")
     #`(s-list #,(srcloc-of-syntax stx) (list expr ... lastexpr))]
    [(_ "[" "]") #`(s-list #,(srcloc-of-syntax stx) empty)]))

(define-syntax (dot-expr stx)
  (syntax-case stx ()
    [(_ obj "." field) #`(s-dot #,(srcloc-of-syntax stx) obj '#,(parse-name #'field))]))

(define-syntax (bracket-expr stx)
  (syntax-case stx ()
    [(_ obj "." "[" field "]") #`(s-bracket #,(srcloc-of-syntax stx) obj field)]))

(define-syntax (dot-assign-expr stx)
  (syntax-case stx ()
    [(_ obj "." field "=" expr) 
        #`(s-dot-assign #,(srcloc-of-syntax stx) obj '#,(parse-name #'field) expr)]))

(define-syntax (bracket-assign-expr stx)
  (syntax-case stx ()
    [(_ obj "." "[" field "]" "=" expr) #`(s-bracket-assign #,(srcloc-of-syntax stx) obj field expr)]))
