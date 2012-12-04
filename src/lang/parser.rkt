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

(define-for-syntax (parse-names stx)
  (map string->symbol (syntax->datum stx)))

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
                          empty)]
    [(_ "{" "extend" super-expr "with"
            (list-field (field key ":" value) ",") ...
            (field lastkey ":" lastvalue) "}")
     #`(s-onion #,(srcloc-of-syntax stx)
                super-expr
                (append (list (data-field key value) ...)
                        (list (data-field lastkey lastvalue))))]
    [(_ "{" "extend" super-expr "}")
     #`(s-onion #,(srcloc-of-syntax stx) super-expr empty)]))

(define-syntax (data-field stx)
  (syntax-case stx ()
    [(_ key value)
     #`(s-field #,(srcloc-of-syntax stx) key value)]))

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
     #`(s-def #,(srcloc-of-syntax stx) 
              (s-bind #,(srcloc-of-syntax #'id) '#,(parse-id #'id) (a-blank)) 
              value-expr)]
    [(_ "def" id "::" ann ":" value-expr)
     #`(s-def #,(srcloc-of-syntax stx) 
              (s-bind #,(srcloc-of-syntax #'id) '#,(parse-id #'id) ann) 
              value-expr)]))

(define-syntax (fun-expr stx)
  (syntax-case stx (args arg-elt)
    [(_ "fun" fun-name (args "(" arg ... lastarg ")") ":" body "end")
      (with-syntax ([f-id (parse-id #'fun-name)])
        #`(s-fun #,(srcloc-of-syntax stx) 'f-id (list arg ... lastarg) (a-blank) body))]
    [(_ "fun" fun-name (args "(" ")") ":" body "end")
      (with-syntax ([f-id (parse-id #'fun-name)])
        #`(s-fun #,(srcloc-of-syntax stx) 'f-id empty (a-blank) body))]
    [(_ "fun" fun-name (args "(" arg ... lastarg ")") "->" ann ":" body "end")
      (with-syntax ([f-id (parse-id #'fun-name)])
        #`(s-fun #,(srcloc-of-syntax stx) 'f-id (list arg ... lastarg) ann body))]
    [(_ "fun" fun-name (args "(" ")") "->" ann ":" body "end")
      (with-syntax ([f-id (parse-id #'fun-name)])
        #`(s-fun #,(srcloc-of-syntax stx) 'f-id empty ann body))]))

(define-syntax (lambda-expr stx)
  (syntax-case stx (lambda-args)
    [(_ "\\" (lambda-args arg ... lastarg) ":" "(" body ")")
      #`(s-lam #,(srcloc-of-syntax stx) (list arg ... lastarg) (a-blank) body)]
    [(_ "\\" "(" body ")")
     #`(s-lam #,(srcloc-of-syntax stx) empty (a-blank) body)]
    [(_ "\\" (lambda-args arg ... lastarg) "->" ann ":" "(" body ")")
     #`(s-lam #,(srcloc-of-syntax stx) (list arg ... lastarg) ann body)]
    [(_ "\\" "->" ann ":" "(" body ")")
     #`(s-lam #,(srcloc-of-syntax stx) empty ann body)]))


(define-syntax (arg-elt stx)
  (syntax-case stx (arg-elt)
    [(_ x ",")
     (with-syntax ([x-id (parse-id #'x)])
       #`(s-bind #,(srcloc-of-syntax stx) 'x-id (a-blank)))]))

(define-syntax (last-arg-elt stx)
  (syntax-case stx (last-arg-elt)
    [(_ x)
     (with-syntax ([x-id (parse-id #'x)])
       #`(s-bind #,(srcloc-of-syntax stx) 'x-id (a-blank)))]))

(define-syntax (ann-arg-elt stx)
  (syntax-case stx (arg-elt)
    [(_ x "::" ann ",")
     (with-syntax ([x-id (parse-id #'x)])
       #`(s-bind #,(srcloc-of-syntax stx) 'x-id ann))]))

(define-syntax (ann-last-arg-elt stx)
  (syntax-case stx (last-arg-elt)
    [(_ x "::" ann)
     (with-syntax ([x-id (parse-id #'x)])
       #`(s-bind #,(srcloc-of-syntax stx) 'x-id ann))]))

(define-syntax (list-expr stx)
  (syntax-case stx (list-elt)
    [(_ "[" (list-elt expr ",") ... lastexpr "]")
     #`(s-list #,(srcloc-of-syntax stx) (list expr ... lastexpr))]
    [(_ "[" "]") #`(s-list #,(srcloc-of-syntax stx) empty)]))

(define-syntax (cond-expr stx)
  (syntax-case stx (cond-branch)
    [(_ "cond" ":" (cond-branch _ exp _ blck) ... "end")
     #`(s-cond #,(srcloc-of-syntax stx) 
               ;; FIXME(dbp): the srcloc should be of the exp, not stx, but my macro-foo
               ;; is not sufficient to pull this off.
               (list (s-cond-branch #,(srcloc-of-syntax stx) exp blck) ...))]))

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

(define-syntax (dot-method-expr stx)
  (syntax-case stx ()
   [(_ obj ":" field (app-args "(" ")"))
    #`(s-dot-method #,(srcloc-of-syntax stx)
                    obj
                    '#,(parse-name #'field)
                    empty)]
    [(_ obj ":" field (app-args "(" (app-arg-elt arg ",") ... lastarg ")"))
     #`(s-dot-method #,(srcloc-of-syntax stx)
                     obj
                     '#,(parse-name #'field)
                     (list arg ... lastarg))]))

(define-syntax (data-member stx)
  (syntax-case stx ()
    [(_ member-name)
     #`(s-member #,(srcloc-of-syntax stx)
                 '#,(parse-name #'member-name)
                 (a-blank))]
    [(_ member-name "::" ann)
     #`(s-member #,(srcloc-of-syntax stx)
                 '#,(parse-name #'member-name)
                 ann)]))

(define-syntax (data-variant stx)
  (syntax-case stx (data-member-elt)
    [(_ "|" variant-name)
     #`(s-variant #,(srcloc-of-syntax stx)
                  '#,(parse-name #'variant-name)
                  (list))]
    [(_ "|" variant-name ":" (data-member-elt member ",") ... last-member)
     #`(s-variant #,(srcloc-of-syntax stx)
                  '#,(parse-name #'variant-name)
                  (list member ... last-member))]))

(define-syntax (data-expr stx)
  (syntax-case stx (data-param-elt data-params)
    [(_ "data" data-name (data-params "(" (data-param-elt name ",") ... last-name ")") variant ... "end")
     #`(s-data #,(srcloc-of-syntax stx) 
               '#,(parse-name #'data-name)
               '#,(parse-names #'(name ... last-name))
               (list variant ...))]
    [(_ "data" data-name variant ... "end")
     #`(s-data #,(srcloc-of-syntax stx) 
               '#,(parse-name #'data-name) 
               (list)
               (list variant ...))]))

(define-syntax (ann stx)
  (syntax-case stx ()
    [(_ "Number") #`(a-num #,(srcloc-of-syntax stx))]
    [(_ "Bool") #`(a-bool #,(srcloc-of-syntax stx))]
    [(_ "String") #`(a-str #,(srcloc-of-syntax stx))]
    [(_ constructed-ann) #'constructed-ann]))

(define-syntax (name-ann stx)
  (syntax-case stx ()
    [(_ name)
     #`(a-name #,(srcloc-of-syntax stx) '#,(parse-name #'name))]))

(define-syntax (ann-field stx)
  (syntax-case stx ()
    [(_ key ":" value)
     #`(a-field #,(srcloc-of-syntax stx) key value)]))

(define-syntax (record-ann stx)
  (syntax-case stx ()
    [(_ "{" (list-ann-field field ",") ... last-field "}")
     #`(a-record #,(srcloc-of-syntax stx)
                 (list field ... last-field))]
    [( _ "{" "}")
     #`(a-record #,(srcloc-of-syntax stx) (list))]))

(define-syntax (arrow-ann stx)
  (syntax-case stx (arrow-ann-elt)
    [(_ "(" (arrow-ann-elt arg ",") ... last-arg "->" result ")")
     #`(a-arrow #,(srcloc-of-syntax stx) (list arg ... last-arg) result)]))

(define-syntax (app-ann stx)
  (syntax-case stx (name-ann)
    [(_ (name-ann name) "(" (app-ann-elt param ",") ... last-param ")")
     #`(a-app #,(srcloc-of-syntax stx) '#,(parse-name #'name) (list param ... last-param))]))
     

