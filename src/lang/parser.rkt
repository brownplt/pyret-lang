#lang racket/base

(provide (all-defined-out))
(require
  (only-in racket/list empty)
  syntax/parse
  "ast.rkt")

;; borrowed from dyoo's brainfudge
(define (loc stx)
    (srcloc (syntax-source stx)
            (syntax-line stx)
            (syntax-column stx)
            (syntax-position stx)
            (syntax-span stx)))

(define (parse-name n) (string->symbol (syntax->datum n)))

(define (parse-string stx)
  (let [(str-val (syntax->datum stx))]
    (substring str-val 1 (sub1 (string-length str-val)))))

(define (parse-num stx)
  (string->number (syntax->datum stx)))

;; NOTE(joe): syntax->datum followed by datum->syntax loses location
;; information if used naively, so do the unpacking with syntax/parse
(define (map/stx f stx)
  (syntax-parse stx
    [() empty]
    [(elt elts ...) (cons (f #'elt) (map/stx f #'(elts ...)))]))

(define (parse-program stx)
  (syntax-parse stx
    #:datum-literals (program imports)
    [(program (imports import ...) body)
     (s-prog (loc stx)
             (map/stx parse-import #'(import ...))
             (parse-block #'body))]))

(define (parse-import stx)
  (syntax-parse stx
    #:datum-literals (import-stmt provide-stmt)
    [(provide-stmt "provide" stmt "end") (s-provide (loc stx) (parse-stmt #'stmt))]
    [(import-stmt "import" import-module "as" name)
     (s-import (loc stx) (parse-import-module #'import-module) (parse-name #'name))]))

(define (parse-import-module stx)
  (syntax-parse stx
    #:datum-literals (import-name import-string)
    [(import-name n) (parse-name #'n)]
    [(import-string s) (parse-string #'s)]))


(define (parse-block stx)
  (syntax-parse stx
    #:datum-literals (block)
    [(block stmts ...)
     (s-block (loc stx) (map/stx parse-stmt-wrapper #'(stmts ...)))]))

(define (parse-stmt-wrapper stx)
  (syntax-parse stx
    #:datum-literals (stmt)
    [(stmt s) (parse-stmt #'s)]))

(define (parse-with stx)
  (syntax-parse stx
    #:datum-literals (data-with)
    [(data-with) empty]
    [(data-with "with" fields) (parse-fields #'fields)]))

(define (parse-variant stx)
  (syntax-parse stx
    #:datum-literals (data-variant)
    [(data-variant "|" name args with)
     (s-variant (loc stx)
                (parse-name #'name)
                (parse-args #'args)
                (parse-with #'with))]
    [(data-variant "|" name with)
     (s-singleton-variant (loc stx) (parse-name #'name) (parse-with #'with))]))

(define (parse-sharing stx)
  (syntax-parse stx
    #:datum-literals (data-sharing)
    [(data-sharing "sharing" fields "end") (parse-fields #'fields)]
    [(data-sharing "end") empty]))


(define (parse-stmt stx)
  (syntax-parse stx
    #:datum-literals (
      var-expr
      let-expr
      fun-expr fun-header fun-body
      data-expr
      do-expr do-stmt
      assign-expr
      when-expr
      try-expr
      stmt
      expr
    )
    [(var-expr "var" bind "=" e)
     (s-var (loc stx) (parse-arg-elt #'bind) (parse-binop-expr #'e))]
    [(let-expr bind "=" e)
     (s-let (loc stx) (parse-arg-elt #'bind) (parse-binop-expr #'e))]
    [(fun-expr "fun" (fun-header params fun-name args return) ":"
        (fun-body (block (stmt (binop-expr (expr (prim-expr (string-expr doc)))))
                         stmt2
                         stmts ...)
                  "end"))
     (s-fun (loc stx)
            (parse-name #'fun-name)
            (parse-ty-params #'params)
            (parse-args #'args)
            (parse-return-ann #'return)
            (parse-string #'doc)
            (s-block (loc stx) (map/stx parse-stmt #'(stmt2 stmts ...))))]
    [(fun-expr "fun" (fun-header params fun-name args return) ":" body)
     (s-fun (loc stx)
            (parse-name #'fun-name)
            (parse-ty-params #'params)
            (parse-args #'args)
            (parse-return-ann #'return)
            ""
            (parse-fun-body #'body))]
    [(data-expr "data" name params ":" variant ... sharing-part)
     (s-data (loc stx)
             (parse-name #'name)
             (parse-ty-params #'params)
             (map/stx parse-variant #'(variant ...))
             (parse-sharing #'sharing-part))]

    [(do-expr "do" fun-stmt (do-stmt stmts ";") ... last-stmt "end")
     (s-do (loc stx)
           (parse-stmt #'fun-stmt)
           (map/stx parse-block #'(stmts ... last-stmt)))]

    [(assign-expr id ":=" e)
     (s-assign (loc stx) (parse-name #'id) (parse-binop-expr #'e))]

    [(when-expr "when" test ":" body "end")
     (s-when (loc stx) (parse-binop-expr #'test) (parse-block #'body))]

    [(try-expr "try" ":" body "except" "(" arg-elt ")" ":" except "end")
     (s-try (loc stx)
            (parse-block #'body)
            (parse-arg-elt #'arg-elt)
            (parse-block #'except))]
      
    [(stmt s) (parse-stmt #'s)]
    [(binop-expr e) (parse-binop-expr #'e)]
    [(binop-expr left op right) (s-op (loc stx) (parse-op #'op)
                                      (parse-binop-expr #'left)
                                      (parse-binop-expr #'right))]))

(define (parse-binop-expr stx)
  (syntax-parse stx
    #:datum-literals (binop-expr expr paren-expr)
    [(binop-expr _ _ _) (parse-stmt stx)]
    [(paren-expr "(" e ")") (s-paren (loc stx) (parse-binop-expr #'e))]
    [(expr e) (parse-expr #'e)]
    [(binop-expr e) (parse-binop-expr #'e)]))

(define (parse-op stx)
  (syntax-parse stx
    #:datum-literals (binop)
    [(binop str) (hash-ref op-lookup-table (syntax->datum #'str))]))

(define (parse-return-ann stx)
  (syntax-parse stx
    #:datum-literals (return-ann)
    [(return-ann) (a-blank)]
    [(return-ann "->" ann) (parse-ann #'ann)]))

(define (parse-arg-elt stx)
  (syntax-parse stx
    #:datum-literals (arg-elt)
    [(arg-elt name) (s-bind (loc stx) (parse-name #'name) (a-blank))]
    [(arg-elt name "::" ann)
     (s-bind (loc stx) (parse-name #'name) (parse-ann #'ann))]))

(define (parse-args stx)
  (syntax-parse stx
    #:datum-literals (args list-arg-elt)
    [(args "(" ")") empty]
    [(args "(" (list-arg-elt arg1 ",") ... lastarg ")") 
     (map/stx parse-arg-elt #'(arg1 ... lastarg))]))

(define (parse-field stx)
  (syntax-parse stx
    #:datum-literals (field)
    [(field key ":" value)
     (s-data-field (loc stx)
                   (s-str (loc stx) (symbol->string (parse-name #'key)))
                   (parse-binop-expr #'value))]
    [(field "[" key "]" ":" value)
     (s-data-field (loc stx)
                   (parse-binop-expr #'key)
                   (parse-binop-expr #'value))]
    [(field key args ret ":" body "end")
     (s-method-field (loc stx)
                   (s-str (loc stx) (symbol->string (parse-name #'key)))
                     (parse-args #'args)
                     (parse-return-ann #'ret)
                     (parse-block #'body))]
    [(field "[" key "]" args ret ":" body "end")
     (s-method-field (loc stx)
                     (parse-binop-expr #'key)
                     (parse-args #'args)
                     (parse-return-ann #'ret)
                     (parse-block #'body))]))

(define (parse-fields stx)
  (syntax-parse stx
    #:datum-literals (fields list-field)
    [(fields (list-field f1 ",") ... lastfield)
     (map/stx parse-field #'(f1 ... lastfield))]
    [(fields (list-field f1 ",") ... lastfield ",")
     (map/stx parse-field #'(f1 ... lastfield))]))

(define (parse-app-args stx)
  (syntax-parse stx
    #:datum-literals (app-args app-arg-elt)
    [(app-args "(" ")") empty]
    [(app-args "(" (app-arg-elt e1 ",") ... elast ")")
     (map/stx parse-binop-expr #'(e1 ... elast))]))

(define (parse-cond-branch stx)
  (syntax-parse stx
    #:datum-literals (cond-branch)
    [(cond-branch "|" test "=>" body)
     (s-cond-branch (loc stx) (parse-binop-expr #'test) (parse-block #'body))]))


(define (parse-ty-params stx)
  (syntax-parse stx
    #:datum-literals (ty-params list-ty-param)
    [(ty-params) empty]
    [(ty-params "<" (list-ty-param param ",") ... last ">")
     (map/stx parse-name #'(param ... last))]))

(define (parse-fun-body stx)
  (syntax-parse stx
    #:datum-literals (fun-body paren-expr)
    [(fun-body block "end") (parse-block #'block)]
    [(fun-body (paren-expr "(" e ")"))
     (s-block (loc #'e) (list (parse-binop-expr #'e)))]))

(define (parse-left-app-fun-expr stx)
  (syntax-parse stx
    #:datum-literals (left-app-fun-expr)
    [(left-app-fun-expr id) (parse-expr #'id)]
    [(left-app-fun-expr id "." name)
     (s-dot (loc stx) (parse-expr #'id) (parse-name #'name))]))

(define (parse-for-bind stx)
  (syntax-parse stx
    #:datum-literals (for-bind)
    [(for-bind name "from" expr)
     (s-for-bind (loc stx) (parse-arg-elt #'name) (parse-binop-expr #'expr))]))

(define (parse-expr stx)
  (syntax-parse stx
    #:datum-literals (
      prim-expr
      obj-expr 
      list-expr list-elt
      app-expr 
      id-expr 
      dot-expr 
      bracket-expr 
      dot-method-expr 
      bracket-method-expr
      cond-expr 
      for-expr
      lambda-expr 
      extend-expr 
      left-app-expr
      expr
    )
    [(prim-expr e) (parse-prim #'e)]
    [(obj-expr "{" "}") (s-obj (loc stx) empty)]
    [(obj-expr "{" fields "}") (s-obj (loc stx) (parse-fields #'fields))]
    [(list-expr "[" "]") (s-list (loc stx) empty)]
    [(list-expr "[" (list-elt e1 ",") ... elast "]")
     (s-list (loc stx) (map/stx parse-binop-expr #'(e1 ... elast)))]
    [(app-expr efun eargs)
     (s-app (loc stx) (parse-expr #'efun) (parse-app-args #'eargs))]
    [(id-expr x) (s-id (loc stx) (parse-name #'x))]
    [(dot-expr obj "." field)
     (s-dot (loc stx) (parse-expr #'obj) (parse-name #'field))]
    [(bracket-expr obj "." "[" field "]")
     (s-bracket (loc stx) (parse-expr #'obj) (parse-binop-expr #'field))]
    [(dot-method-expr obj ":" field)
     (s-dot-method (loc stx) (parse-expr #'obj) (parse-name #'field))]
    [(bracket-method-expr obj ":" "[" field "]")
     (s-bracket-method (loc stx) (parse-expr #'obj) (parse-binop-expr #'field))]
    [(cond-expr "cond" ":" branch ... "end")
     (s-cond (loc stx) (map/stx parse-cond-branch #'(branch ...)))]
    [(for-expr "for" iter "(" (for-bind-elt binds ",") ... last-bind ")" return-ann ":" body "end")
     (s-for (loc stx)
            (parse-expr #'iter)
            (map/stx parse-for-bind #'(binds ... last-bind))
            (parse-return-ann #'return-ann)
            (parse-block #'body))]
    [(for-expr "for" iter "(" ")" return-ann ":" body "end")
     (s-for (loc stx)
            (parse-expr #'iter)
            empty
            (parse-return-ann #'return-ann)
            (parse-block #'body))]
    [(lambda-expr "fun" ty-params args return-ann ":" fun-body)
     (s-lam (loc stx)
            (parse-ty-params #'ty-params)
            (parse-args #'args)
            (parse-return-ann #'return-ann)
            ""
            (parse-fun-body #'fun-body))]
    [(lambda-expr "fun" ty-params return-ann ":" fun-body)
     (s-lam (loc stx)
            (parse-ty-params #'ty-params)
            (list)
            (parse-return-ann #'return-ann)
            ""
            (parse-fun-body #'fun-body))]
    [(extend-expr e "." "{" fields "}")
     (s-onion (loc stx) (parse-expr #'e) (parse-fields #'fields))]
    [(left-app-expr e "^" fun-expr app-args)
     (s-left-app (loc stx)
                 (parse-expr #'e)
                 (parse-left-app-fun-expr #'fun-expr)
                 (parse-app-args #'app-args))]
    [(expr e) (parse-expr #'e)]
    ))

(define (parse-prim stx)
  (syntax-parse stx
    #:datum-literals (
      bool-expr
      num-expr
      string-expr
    )
    [(bool-expr "true") (s-bool (loc stx) #t)]
    [(bool-expr "false") (s-bool (loc stx) #f)]
    [(num-expr n) (s-num (loc stx) (parse-num #'n))]
    [(num-expr "-" n) (s-num (loc stx) (- (parse-num #'n)))]
    [(string-expr s) (s-str (loc stx) (parse-string #'s))]))

(define (parse-ann-field stx)
  (syntax-parse stx
    #:datum-literals (ann-field)
    [(ann-field n ":" ann)
     (a-field (loc stx) (symbol->string (parse-name #'n)) (parse-ann #'ann))]))

(define (parse-ann stx)
  (syntax-parse stx
    #:datum-literals (
      name-ann
      record-ann list-ann-field
      arrow-ann arrow-ann-elt
      app-ann app-ann-elt
      pred-ann
      dot-ann
      ann
    )
    [(name-ann n) (a-name (loc stx) (parse-name #'n))]
    [(record-ann "{" "}") (a-record (loc stx) empty)]
    [(record-ann "{" (list-ann-field fields ",") ... lastfield "}")
     (a-record (loc stx) (map/stx parse-ann-field #'(fields ... lastfield)))]
    [(arrow-ann "(" (arrow-ann-elt anns ",") ... last-ann "->" result ")")
     (a-arrow (loc stx)
              (map/stx parse-ann #'(anns ... last-ann))
              (parse-ann #'result))]
    [(app-ann (name-ann n) "<" (app-ann-elt anns ",") ... last-ann ">")
     (a-app (loc stx)
            (parse-name #'n)
            (map/stx parse-ann #'(anns ... last-ann)))]
    [(pred-ann annbase "(" expr ")")
     (a-pred (loc stx) (parse-ann #'annbase) (parse-binop-expr #'expr))]
    [(dot-ann n1 "." n2)
     (a-dot (loc stx) (parse-name #'n1) (parse-name #'n2))]
    [(ann a) (parse-ann #'a)]))

