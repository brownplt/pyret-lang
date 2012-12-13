#lang racket

(provide (all-defined-out))
(require
  "ast.rkt")

;; borrowed from dyoo's brainfudge
(define-for-syntax (loc stx)
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
  (datum->syntax #f (string->symbol (syntax->datum stx))))

(define-for-syntax (parse-ids stx)
  (datum->syntax #f (map string->symbol (syntax->datum stx))))

(define-syntax (program stx)
  (syntax-case stx ()
    [(_ block endmarker-ignored)
     #'block]))

(define-syntax (block stx)
  (syntax-case stx ()
    [(_ stmt ...)
     #`(s-block #,(loc stx) (list stmt ...))]))

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
       (with-syntax ([s (datum->syntax #'string-expr real-str)])
         #`(s-str #,(loc stx) s)))]))

(define-syntax (num-expr stx)
  (syntax-case stx ()
    [(_ num) (with-syntax ([n (datum->syntax #'num-expr (string->number (syntax->datum #'num)))])
               #`(s-num #,(loc stx) n))]))

(define-syntax (bool-expr stx)
  (syntax-case stx ()
    [(_ "true") #`(s-bool #,(loc stx) #t)]
    [(_ "false") #`(s-bool #,(loc stx) #f)]))

(define-syntax (field stx)
  (syntax-case stx ()
    [(_ key ":" value) #`(s-data-field #,(loc stx) key value)]
    [(_ key args ":" body)
     #`(s-method-field #,(loc stx) key args body)]
    [(_ key args ":" body "end")
     #`(s-method-field #,(loc stx) key args body)]))

;; We don't parse the special method sugar yet
(define-syntax (obj-expr stx)
  (syntax-case stx (list-field)
    [(_ "{" (list-field field ",") ... lastfield "}")
     #`(s-obj #,(loc stx) (list field ... lastfield))]
    [(_ "{" "}") #`(s-obj #,(loc stx)
                          empty)]
    [(_ "{" "extend" super-expr "with" (list-field field ",") ...  lastfield "}")
     #`(s-onion #,(loc stx) super-expr (list field ... lastfield))]
    [(_ "{" "extend" super-expr "}")
     #`(s-onion #,(loc stx) super-expr empty)]))

(define-syntax (id-expr stx)
  (syntax-case stx ()
    [(_ x)
     (with-syntax ([x-id (parse-id #'x)])
       #`(s-id #,(loc stx) 'x-id))]))

(define-syntax (assign-expr stx)
  (syntax-case stx ()
    [(_ x "=" expr)
     (with-syntax ([x-id (parse-id #'x)])
       #`(s-assign #,(loc stx) 'x-id expr))]))

;; TODO(joe): there's extra crap in here
(define-syntax (app-expr stx)
  (syntax-case stx (app-arg-elt)
    [(_ fun-expr (app-args "(" (app-arg-elt arg ",") ... lastarg ")"))
     #`(s-app #,(loc stx) fun-expr (list arg ... lastarg))]
    [(_ fun-expr (app-args "(" ")"))
     #`(s-app #,(loc stx) fun-expr empty)]))

(define-syntax (def-expr stx)
  (syntax-case stx ()
    [(_ "def" id ":" value-expr)
     #`(s-def #,(loc stx) 
              (s-bind #,(loc #'id) '#,(parse-id #'id) (a-blank)) 
              value-expr)]
    [(_ "def" id "::" ann ":" value-expr)
     #`(s-def #,(loc stx) 
              (s-bind #,(loc #'id) '#,(parse-id #'id) ann) 
              value-expr)]))

(define-syntax (args stx)
  (syntax-case stx ()
    [(_ "(" arg ... lastarg ")") #'(list arg ... lastarg)]
    [(_ "(" ")") #'empty]))

(define-syntax (fun-body stx)
  (syntax-case stx ()
    [(_ block "end")
     #'block]
    [(_ "(" block ")")
     #'block]))

(define-syntax (fun-ty-params stx)
  (syntax-case stx (fun-ty-param fun-ty-param-elt)
    [(_ "(" (fun-ty-param
	     (fun-ty-param-elt param) ",") ...
	     (fun-ty-param-elt last) ")")
     #`(quote #,(parse-names #'(param ... last)))]))

(define-syntax (fun-expr stx)
  (syntax-case stx ()
    [(_ "fun" (fun-header fun-name args) ":" body )
      (with-syntax ([f-id (parse-id #'fun-name)])
        #`(s-fun #,(loc stx) 'f-id (list) args (a-blank) body))]
    [(_ "fun" (fun-header fun-name args "->" ann) ":" body)
      (with-syntax ([f-id (parse-id #'fun-name)])
        #`(s-fun #,(loc stx) 'f-id (list) args ann body))]
    [(_ "fun" (fun-header params fun-name args) ":" body)
      (with-syntax ([f-id (parse-id #'fun-name)])
        #`(s-fun #,(loc stx) 'f-id params args (a-blank) body))]
    [(_ "fun" (fun-header params fun-name args "->" ann) ":" body)
      (with-syntax ([f-id (parse-id #'fun-name)])
        #`(s-fun #,(loc stx) 'f-id params args ann body))]))

(define-syntax (lambda-expr stx)
  (syntax-case stx (lambda-args)
    [(_ "\\" (lambda-args arg ... lastarg) ":" "(" body ")")
      #`(s-lam #,(loc stx) empty (list arg ... lastarg) (a-blank) body)]
    [(_ "\\" "(" body ")")
     #`(s-lam #,(loc stx) empty empty (a-blank) body)]
    [(_ "\\" (lambda-args arg ... lastarg) "->" ann ":" "(" body ")")
     #`(s-lam #,(loc stx) empty (list arg ... lastarg) ann body)]
    [(_ "\\" "->" ann ":" "(" body ")")
     #`(s-lam #,(loc stx) empty empty ann body)]))


(define-syntax (arg-elt stx)
  (syntax-case stx (arg-elt)
    [(_ x ",")
     (with-syntax ([x-id (parse-id #'x)])
       #`(s-bind #,(loc stx) 'x-id (a-blank)))]))

(define-syntax (last-arg-elt stx)
  (syntax-case stx (last-arg-elt)
    [(_ x)
     (with-syntax ([x-id (parse-id #'x)])
       #`(s-bind #,(loc stx) 'x-id (a-blank)))]))

(define-syntax (ann-arg-elt stx)
  (syntax-case stx (arg-elt)
    [(_ x "::" ann ",")
     (with-syntax ([x-id (parse-id #'x)])
       #`(s-bind #,(loc stx) 'x-id ann))]))

(define-syntax (ann-last-arg-elt stx)
  (syntax-case stx (last-arg-elt)
    [(_ x "::" ann)
     (with-syntax ([x-id (parse-id #'x)])
       #`(s-bind #,(loc stx) 'x-id ann))]))

(define-syntax (list-expr stx)
  (syntax-case stx (list-elt)
    [(_ "[" (list-elt expr ",") ... lastexpr "]")
     #`(s-list #,(loc stx) (list expr ... lastexpr))]
    [(_ "[" "]") #`(s-list #,(loc stx) empty)]))

(define-syntax (cond-expr stx)
  (syntax-case stx (cond-branch)
    [(_ "cond" ":" (cond-branch _ exp _ blck) ... "end")
     #`(s-cond #,(loc stx) 
               ;; FIXME(dbp): the srcloc should be of the exp, not stx, but my macro-foo
               ;; is not sufficient to pull this off.
               (list (s-cond-branch #,(loc stx) exp blck) ...))]))

(define-syntax (dot-expr stx)
  (syntax-case stx ()
    [(_ obj "." field) #`(s-dot #,(loc stx) obj '#,(parse-name #'field))]))

(define-syntax (bracket-expr stx)
  (syntax-case stx ()
    [(_ obj "." "[" field "]") #`(s-bracket #,(loc stx) obj field)]))

(define-syntax (dot-assign-expr stx)
  (syntax-case stx ()
    [(_ obj "." field "=" expr) 
        #`(s-dot-assign #,(loc stx) obj '#,(parse-name #'field) expr)]))

(define-syntax (bracket-assign-expr stx)
  (syntax-case stx ()
    [(_ obj "." "[" field "]" "=" expr) #`(s-bracket-assign #,(loc stx) obj field expr)]))

(define-syntax (dot-method-expr stx)
  (syntax-case stx ()
   [(_ obj ":" field (app-args "(" ")"))
    #`(s-dot-method #,(loc stx)
                    obj
                    '#,(parse-name #'field)
                    empty)]
    [(_ obj ":" field (app-args "(" (app-arg-elt arg ",") ... lastarg ")"))
     #`(s-dot-method #,(loc stx)
                     obj
                     '#,(parse-name #'field)
                     (list arg ... lastarg))]))

(define-syntax (data-member stx)
  (syntax-case stx ()
    [(_ member-name)
     #`(s-member #,(loc stx)
                 '#,(parse-name #'member-name)
                 (a-blank))]
    [(_ member-name "::" ann)
     #`(s-member #,(loc stx)
                 '#,(parse-name #'member-name)
                 ann)]))

(define-syntax (data-variant stx)
  (syntax-case stx (data-member-elt)
    [(_ "|" variant-name)
     #`(s-variant #,(loc stx)
                  '#,(parse-name #'variant-name)
                  (list))]
    [(_ "|" variant-name ":" (data-member-elt member ",") ... last-member)
     #`(s-variant #,(loc stx)
                  '#,(parse-name #'variant-name)
                  (list member ... last-member))]))

(define-syntax (data-expr stx)
  (syntax-case stx (data-param-elt data-params)
    [(_ "data" data-name (data-params "(" (data-param-elt name ",") ... last-name ")") variant ... "end")
     #`(s-data #,(loc stx) 
               '#,(parse-name #'data-name)
               '#,(parse-names #'(name ... last-name))
               (list variant ...))]
    [(_ "data" data-name variant ... "end")
     #`(s-data #,(loc stx) 
               '#,(parse-name #'data-name) 
               (list)
               (list variant ...))]))

(define-syntax (do-expr stx)
  (syntax-case stx (do-stmt)
    [(_ "do" fun-stmt (do-stmt stmt ";") ... last-stmt "end")
     #`(s-do #,(loc stx) fun-stmt (list stmt ... last-stmt))]))

(define-syntax (ann stx)
  (syntax-case stx ()
    [(_ constructed-ann) #'constructed-ann]))

(define-syntax (name-ann stx)
  (syntax-case stx ()
    [(_ name)
     #`(a-name #,(loc stx) '#,(parse-name #'name))]))

(define-syntax (ann-field stx)
  (syntax-case stx ()
    [(_ key ":" value)
     #`(a-field #,(loc stx) key value)]))

(define-syntax (record-ann stx)
  (syntax-case stx ()
    [(_ "{" (list-ann-field field ",") ... last-field "}")
     #`(a-record #,(loc stx)
                 (list field ... last-field))]
    [( _ "{" "}")
     #`(a-record #,(loc stx) (list))]))

(define-syntax (arrow-ann stx)
  (syntax-case stx (arrow-ann-elt)
    [(_ "(" (arrow-ann-elt arg ",") ... last-arg "->" result ")")
     #`(a-arrow #,(loc stx) (list arg ... last-arg) result)]))

(define-syntax (app-ann stx)
  (syntax-case stx (name-ann)
    [(_ (name-ann name) "(" (app-ann-elt param ",") ... last-param ")")
     #`(a-app #,(loc stx) '#,(parse-name #'name) (list param ... last-param))]))
     

