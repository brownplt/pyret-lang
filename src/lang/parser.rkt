#lang racket

(provide (all-defined-out) #%datum)
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

(define-for-syntax (parse-string stx)
  (let [(str-val (syntax->datum stx))]
    (substring str-val 1 (sub1 (string-length str-val)))))

(define-syntax (#%top-interaction stx)
  (syntax-case stx ()
    [(_ exprs ...) #'(exprs ...)]))

(define-syntax (program stx)
  (syntax-case stx ()
    [(_ (imports import ...) block endmarker-ignored)
     #`(s-prog #,(loc stx) (list import ...) block)]))

(define-syntax (block stx)
  (syntax-case stx ()
    [(_ stmt ...)
     #`(s-block #,(loc stx) (list stmt ...))]))

(define-syntax (expr stx)
  (syntax-case stx ()
    [(_ the-expr) #'the-expr]))

(define-syntax (stmt stx)
  (syntax-case stx ()
    [(_ stmt "") #'stmt]
    [(_ stmt) #'stmt]))

(define-syntax (import-string stx)
  (syntax-case stx ()
    [(_ str) #`#,(parse-string #'str)]))

(define-syntax (import-name stx)
  (syntax-case stx ()
    [(_ name) #`(quote #,(parse-id #'name))]))

(define-syntax (import-stmt stx)
  (syntax-case stx ()
    [(_ "import" file "as" name)
      #`(s-import #,(loc stx) file '#,(parse-name #'name))]))

(define-syntax (provide-stmt stx)
  (syntax-case stx ()
    [(_ "provide" stmt "end")
     #`(s-provide #,(loc stx) stmt)]))

(define-syntax (prim-expr stx)
  (syntax-case stx ()
    [(_ expr) #'expr]))

;; Amusingly, we get "'foo'" for the string 'foo' in the source, so cut off
;; the surrounding quotes
(define-syntax (string-expr stx)
  (syntax-case stx ()
    [(_ str)
     (with-syntax ([s (datum->syntax #'string-expr (parse-string #'str))])
       #`(s-str #,(loc stx) s))]))

(define-syntax (num-expr stx)
  (syntax-case stx ()
    [(_ num) (with-syntax ([n (datum->syntax #'num-expr (string->number (syntax->datum #'num)))])
               #`(s-num #,(loc stx) n))]
    [(_ "-" num) (with-syntax ([n (datum->syntax #'num-expr (string->number (syntax->datum #'num)))])
               #`(s-num #,(loc stx) (- n)))]))

(define-syntax (bool-expr stx)
  (syntax-case stx ()
    [(_ "true") #`(s-bool #,(loc stx) #t)]
    [(_ "false") #`(s-bool #,(loc stx) #f)]))

(define-syntax (field stx)
  (syntax-case stx ()
    [(_ key ":" value) #`(s-data-field #,(loc stx) (s-str #,(loc stx) key) value)]
    [(_ "[" key "]" ":" value) #`(s-data-field #,(loc stx) key value)]
    [(_ key args ret ":" body)
     #`(s-method-field #,(loc stx) (s-str #,(loc stx) key) args ret body)]
    [(_ key args ret ":" body "end")
     #`(s-method-field #,(loc stx) (s-str #,(loc stx) key) args ret body)]
    [(_ "[" key "]" args ret ":" body)
     #`(s-method-field #,(loc stx) key args ret body)]
    [(_ "[" key "]" args ret ":" body "end")
     #`(s-method-field #,(loc stx) #,(loc stx) args ret body)]))

(define-syntax (fields stx)
  (syntax-case stx ()
    [(_ (list-field field ",") ... lastfield)
     #'(list field ... lastfield)]
    [(_ (list-field field ",") ... lastfield ",")
     #'(list field ... lastfield)]))

(define-syntax (obj-expr stx)
  (syntax-case stx (list-field)
    [(_ "{" fields "}") #`(s-obj #,(loc stx) fields)]
    [(_ "{" "}") #`(s-obj #,(loc stx) empty)]))

(define-syntax (id-expr stx)
  (syntax-case stx ()
    [(_ x)
     (with-syntax ([x-id (parse-id #'x)])
       #`(s-id #,(loc stx) 'x-id))]))

(define-syntax (assign-expr stx)
  (syntax-case stx ()
    [(_ x ":=" expr)
     (with-syntax ([x-id (parse-id #'x)])
       #`(s-assign #,(loc stx) 'x-id expr))]))

(define-syntax (app-args stx)
  (syntax-case stx (app-arg-elt)
    [(_ "(" ")") #'empty]
    [(_ "(" (app-arg-elt arg ",") ... lastarg ")")
     #'(list arg ... lastarg)]))

(define-syntax (app-expr stx)
  (syntax-case stx (app-arg-elt)
    [(_ fun-expr app-args)
     #`(s-app #,(loc stx) fun-expr app-args)]))

(define-syntax (left-app-fun-expr stx)
  (syntax-case stx ()
    [(_ id-expr) #'id-expr]
    [(_ id-expr "." name)
     #`(s-dot #,(loc stx) id-expr '#,(parse-name #'name))]))

(define-syntax (left-app-expr stx)
  (syntax-case stx ()
    [(_ target-expr "^" fun-expr app-args)
     #`(s-left-app #,(loc stx) target-expr fun-expr app-args)]))

(define-syntax (var-expr stx)
  (syntax-case stx ()
    [(_ "var" id "=" value-expr)
     #`(s-var #,(loc stx) 
              (s-bind #,(loc #'id) '#,(parse-id #'id) (a-blank)) 
              value-expr)]
    [(_ "var" id "::" ann "=" value-expr)
     #`(s-var #,(loc stx) 
              (s-bind #,(loc #'id) '#,(parse-id #'id) ann) 
              value-expr)]))

(define-syntax (let-expr stx)
  (syntax-case stx ()
    [(_ id "=" value-expr)
     #`(s-let #,(loc stx) 
              (s-bind #,(loc #'id) '#,(parse-id #'id) (a-blank)) 
              value-expr)]
    [(_ id "::" ann "=" value-expr)
     #`(s-let #,(loc stx) 
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
    [(_) #'(list)]
    [(_ "<" (fun-ty-param
	     (fun-ty-param-elt param) ",") ...
	     (fun-ty-param-elt last) ">")
     #`(quote #,(parse-names #'(param ... last)))]))

(define-syntax (return-ann stx)
  (syntax-case stx ()
    [(_) #'(a-blank)]
    [(_ "->" ann) #'ann]))

(define-syntax (fun-expr stx)
  (syntax-case stx (block stmt expr prim-expr string-expr)
    [(_ "fun" (fun-header params fun-name args return) ":"
        (fun-body (block (stmt (expr (prim-expr (string-expr s))))
                         stmt2
                         stmts ...)
                  "end"))
     (with-syntax ([f-id (parse-id #'fun-name)])
        #`(s-fun #,(loc stx) 'f-id params args return
                 #,(parse-string #'s)
                 (s-block #,(loc stx) (list stmt2 stmts ...))))]
    [(_ "fun" (fun-header params fun-name args return) ":" body)
     (with-syntax ([f-id (parse-id #'fun-name)])
        #`(s-fun #,(loc stx) 'f-id params args return "" body))]))

(define-syntax (lambda-expr stx)
  (syntax-case stx (lambda-args)
    [(_ "\\" ty-params (lambda-args arg ... lastarg) ":" body)
      #`(s-lam #,(loc stx) ty-params (list arg ... lastarg) (a-blank) "" body)]
    [(_ "\\" body)
     #`(s-lam #,(loc stx) empty empty (a-blank) "" body)]
    [(_ "\\" ty-params (lambda-args arg ... lastarg) "->" ann ":" body)
     #`(s-lam #,(loc stx) ty-params (list arg ... lastarg) ann "" body)]
    [(_ "\\" ty-params "->" ann ":" body)
     #`(s-lam #,(loc stx) ty-params empty ann "" body)]))


(define-syntax (arg-elt stx)
  (syntax-case stx (arg-elt)
    [(_ x ",")
     (with-syntax ([x-id (parse-id #'x)])
       #`(s-bind #,(loc stx) 'x-id (a-blank)))]
    [(_ x "::" ann ",")
     (with-syntax ([x-id (parse-id #'x)])
       #`(s-bind #,(loc stx) 'x-id ann))]))

(define-syntax (last-arg-elt stx)
  (syntax-case stx (last-arg-elt)
    [(_ x)
     (with-syntax ([x-id (parse-id #'x)])
       #`(s-bind #,(loc stx) 'x-id (a-blank)))]
    [(_ x "::" ann)
     (with-syntax ([x-id (parse-id #'x)])
       #`(s-bind #,(loc stx) 'x-id ann))]))

(define-syntax (list-expr stx)
  (syntax-case stx (list-elt)
    [(_ "[" (list-elt expr ",") ... lastexpr "]")
     #`(s-list #,(loc stx) (list expr ... lastexpr))]
    [(_ "[" "]") #`(s-list #,(loc stx) empty)]))

(define-syntax (when-expr stx)
  (syntax-case stx ()
    [(_ "when" test ":" block "end")
     #`(s-when #,(loc stx) test block)]))

(define-syntax (cond-expr stx)
  (syntax-case stx (cond-branch)
    [(_ "cond" ":" (cond-branch _ exp _ blck) ... "end")
     #`(s-cond #,(loc stx) 
               ;; FIXME(dbp): the srcloc should be of the exp, not stx, but my macro-foo
               ;; is not sufficient to pull this off.
               (list (s-cond-branch #,(loc stx) exp blck) ...))]))

(define-syntax (try-expr stx)
  (syntax-case stx ()
    [(_ "try" ":" t-block "except" "(" last-arg-elt ")" ":" e-block "end")
     #`(s-try #,(loc stx) t-block last-arg-elt e-block)]))

(define-syntax (extend-expr stx)
  (syntax-case stx ()
    [(_ obj "." "{" fields "}")
     #`(s-onion #,(loc stx) obj fields)]))

(define-syntax (dot-expr stx)
  (syntax-case stx ()
    [(_ obj "." field) #`(s-dot #,(loc stx) obj '#,(parse-name #'field))]))

(define-syntax (bracket-expr stx)
  (syntax-case stx ()
    [(_ obj "." "[" field "]") #`(s-bracket #,(loc stx) obj field)]))

(define-syntax (dot-method-expr stx)
  (syntax-case stx ()
    [(_ obj ":" field)
     #`(s-dot-method #,(loc stx)
                     obj
                     '#,(parse-name #'field))]))

(define-syntax (bracket-method-expr stx)
  (syntax-case stx ()
    [(_ obj ":" "[" field "]")
     #`(s-bracket-method #,(loc stx)
                     obj
                     field)]))

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

(define-syntax (data-with stx)
  (syntax-case stx ()
    [(_ "with" fields) #'fields]
    [(_) #'(list)]))

(define-syntax (data-fields stx)
  (syntax-case stx ()
    [(_ "(" (data-member-elt member ",") ... last-member ")")
     #'(list member ... last-member)]
    [(_ "(" ")") #'(list)]))

(define-syntax (data-variant stx)
  (syntax-case stx (data-member-elt data-members)
    [(_ "|" variant-name fields-part with-part)
     #`(s-variant #,(loc stx)
                  '#,(parse-name #'variant-name)
                  fields-part
                  with-part)]
    [(_ "|" variant-name with-part)
     #`(s-singleton-variant #,(loc stx)
                  '#,(parse-name #'variant-name)
                  with-part)]))

(define-syntax (data-params stx)
  (syntax-case stx (data-param-elt)
    [(_ "<" (data-param-elt name ",") ... last-name ">")
     #`(quote #,(parse-names #'(name ... last-name)))]
    [(_) #'(list)]))
      

(define-syntax (data-sharing stx)
  (syntax-case stx ()
    [(_ "sharing" fields "end") #'fields]
    [(_ "end") #'(list)]))

(define-syntax (data-expr stx)
  (syntax-case stx ()
    [(_ "data" data-name data-params ":" variant ...  sharing-part)
     #`(s-data #,(loc stx) 
               '#,(parse-name #'data-name)
               data-params
               (list variant ...)
               sharing-part)]))

(define-syntax (do-expr stx)
  (syntax-case stx (do-stmt)
    [(_ "do" fun-stmt (do-stmt stmt ";") ... last-stmt "end")
     #`(s-do #,(loc stx) fun-stmt (list stmt ... last-stmt))]))

(define-syntax (for-name stx)
  (syntax-case stx ()
    [(_ x) #`(s-bind #,(loc stx) x (a-blank))]
    [(_ x "::" ann) #`(s-bind #,(loc stx) x ann)]))

(define-syntax (for-bind stx)
  (syntax-case stx ()
    [(_ name "from" expr) #`(s-for-bind #,(loc stx) name expr)]))

(define-syntax (for-expr stx)
  (syntax-case stx (for-bind-elt)
    [(_ "for" iter "(" (for-bind-elt binds ",") ... last-bind ")" return-ann ":" block "end")
     #`(s-for #,(loc stx) iter (list binds ... last-bind) return-ann block)]
    [(_ "for" iter "(" ")" return-ann ":" block "end")
     #`(s-for #,(loc stx) iter (list) return-ann block)]))

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
    [(_ (name-ann name) "<" (app-ann-elt param ",") ... last-param ">")
     #`(a-app #,(loc stx) '#,(parse-name #'name) (list param ... last-param))]))

(define-syntax (pred-ann stx)
  (syntax-case stx ()
    [(_ ann "(" expr ")")
     #`(a-pred #,(loc stx) ann expr)]))

(define-syntax (dot-ann stx)
  (syntax-case stx ()
    [(_ obj "." field)
     #`(a-dot #,(loc stx) '#,(parse-name #'obj)
              '#,(parse-name #'field))]))

