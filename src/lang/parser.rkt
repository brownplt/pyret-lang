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
  (parameterize ([read-decimal-as-inexact #f])
    (string->number (syntax->datum stx))))

;; NOTE(joe): syntax->datum followed by datum->syntax loses location
;; information if used naively, so do the unpacking with syntax/parse
(define (map/stx f stx)
  (syntax-parse stx
    [() empty]
    [(elt elts ...) (cons (f #'elt) (map/stx f #'(elts ...)))]))

(define (parse-program stx)
  (syntax-parse stx
    #:datum-literals (program prelude)
    [(program (prelude prelude-stmt ...) body)
     (s-prog (loc stx)
             (map/stx parse-prelude #'(prelude-stmt ...))
             (parse-block #'body))]))

(define (parse-prelude stx)
  (syntax-parse stx
    #:datum-literals (import-stmt provide-stmt)
    [(provide-stmt "provide" "*") (s-provide-all (loc stx))]
    [(provide-stmt "provide" stmt (end (~or "end" ";"))) (s-provide (loc stx) (parse-stmt #'stmt))]
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

(define (parse-check-block stx)
  (syntax-parse stx
    #:datum-literals (check-block)
    [(check-block stmts ...)
     (s-block (loc stx) (map/stx parse-stmt-wrapper #'(stmts ...)))]))

(define (parse-stmt-wrapper stx)
  (syntax-parse stx
    #:datum-literals (stmt check-test)
    [(stmt s) (parse-stmt #'s)]))

(define (parse-with stx)
  (syntax-parse stx
    #:datum-literals (data-with)
    [(data-with) empty]
    [(data-with "with:" fields) (parse-fields #'fields)]))

(define (parse-variant stx)
  (syntax-parse stx
    #:datum-literals (data-variant first-data-variant)
    [(data-variant "|" name args with)
     (s-variant (loc stx)
                (parse-name #'name)
                (parse-variant-members #'args)
                (parse-with #'with))]
    [(data-variant "|" name with)
     (s-singleton-variant (loc stx) (parse-name #'name) (parse-with #'with))]
    [(first-data-variant name args with)
     (s-variant (loc stx)
                (parse-name #'name)
                (parse-variant-members #'args)
                (parse-with #'with))]
    [(first-data-variant name with)
     (s-singleton-variant (loc stx) (parse-name #'name) (parse-with #'with))]
    ))

(define (parse-datatype-variant stx)
  (syntax-parse stx
    #:datum-literals (datatype-variant first-datatype-variant)
    [(datatype-variant "|" name args constructor)
     (s-datatype-variant (loc stx)
                         (parse-name #'name)
                         (parse-variant-members #'args)
                         (parse-constructor #'constructor))]
    [(datatype-variant "|" name constructor)
     (s-datatype-singleton-variant (loc stx) (parse-name #'name) (parse-constructor #'constructor))]
    [(first-datatype-variant name args constructor)
     (s-datatype-variant (loc stx)
                         (parse-name #'name)
                         (parse-variant-members #'args)
                         (parse-constructor #'constructor))]
    [(first-datatype-variant name constructor)
     (s-datatype-singleton-variant (loc stx) (parse-name #'name) (parse-constructor #'constructor))]
    ))


(define (parse-sharing stx)
  (syntax-parse stx
    #:datum-literals (data-sharing)
    [(data-sharing "sharing:" fields) (parse-fields #'fields)]
    [(data-sharing) empty]))

(define (parse-constructor stx)
  (syntax-parse stx
    #:datum-literals (constructor-clause)
    [(constructor-clause "with constructor" "(" name ")" ":" block (end (~or "end" ";")))
     (s-datatype-constructor (loc stx) (parse-name #'name) (parse-block #'block))]))

(define (parse-stmt stx)
  (syntax-parse stx
    #:datum-literals (
      var-expr
      let-expr
      fun-expr fun-header fun-body
      data-expr
      datatype-expr
      assign-expr
      when-expr
      check-expr check-test
      stmt
      expr
    )
    [(var-expr "var" bind "=" e)
     (s-var (loc stx) (parse-binding #'bind) (parse-binop-expr #'e))]
    [(let-expr bind "=" e)
     (s-let (loc stx) (parse-binding #'bind) (parse-binop-expr #'e))]
    [(graph-expr "graph:" binding ... (end (~or "end" ";")))
     (s-graph (loc stx) (map/stx parse-stmt #'(binding ...)))]
    [(fun-expr "fun" (fun-header params fun-name args return) ":"
               doc body check (end (~or "end" ";")))
     (s-fun (loc stx)
            (parse-name #'fun-name)
            (parse-ty-params #'params)
            (parse-args #'args)
            (parse-return-ann #'return)
            (parse-doc-string #'doc)
            (parse-block #'body)
            (parse-where-clause #'check))]
    [(data-expr "data" name params mixins ":" variant ... sharing-part check (end (~or "end" ";")))
     (s-data (loc stx)
             (parse-name #'name)
             (parse-ty-params #'params)
             (parse-mixins #'mixins)
             (map/stx parse-variant #'(variant ...))
             (parse-sharing #'sharing-part)
             (parse-where-clause #'check))]

    [(datatype-expr "datatype" name params ":" variant ... check (end (~or "end" ";")))
     (s-datatype (loc stx)
                 (parse-name #'name)
                 (parse-ty-params #'params)
                 (map/stx parse-datatype-variant #'(variant ...))
                 (parse-where-clause #'check))]

    [(assign-expr id ":=" e)
     (s-assign (loc stx) (parse-name #'id) (parse-binop-expr #'e))]

    [(when-expr "when" test ":" body (end (~or "end" ";")))
     (s-when (loc stx) (parse-binop-expr #'test) (parse-block #'body))]

    [(check-expr "check:" body (end (~or "end" ";")))
     (s-check (loc stx) (parse-block #'body))]
    [(check-test expr) (parse-stmt #'expr)]
    [(check-test left op right) (s-check-test (loc stx) (parse-check-op #'op)
                                              (parse-binop-expr #'left)
                                              (parse-binop-expr #'right))]
    [(stmt s) (parse-stmt #'s)]
    [(binop-expr e) (parse-binop-expr #'e)]
    [(binop-expr left op right) (s-op (loc stx) (parse-op #'op)
                                      (parse-binop-expr #'left)
                                      (parse-binop-expr #'right))]))

(define (parse-doc-string stx)
  (syntax-parse stx
    #:datum-literals (doc-string)
    [(doc-string) ""]
    [(doc-string "doc:" str) (parse-string #'str)]))

(define (parse-where-clause stx)
  (syntax-parse stx
    #:datum-literals (where-clause)
    [(where-clause) (s-block (loc stx) empty)]
    [(where-clause "where:" block) (parse-block #'block)]))

(define (parse-check-op stx)
  (syntax-parse stx
    #:datum-literals (check-op)
    [(check-op str) (hash-ref op-lookup-table (syntax->datum #'str))]))

(define (parse-binop-expr stx)
  (syntax-parse stx
    #:datum-literals (binop-expr not-expr expr)
    [(binop-expr _ _ _) (parse-stmt stx)]
    [(expr e) (parse-expr #'e)]
    [(not-expr "not" e) (s-not (loc stx) (parse-expr #'e))]
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

(define (parse-binding stx)
  (syntax-parse stx
    #:datum-literals (binding)
    [(binding name) (s-bind (loc stx) #f (parse-name #'name) (a-blank))]
    [(binding name "::" ann)
     (s-bind (loc stx) #f (parse-name #'name) (parse-ann #'ann))]
    [(binding "shadow" name) (s-bind (loc stx) #t (parse-name #'name) (a-blank))]
    [(binding "shadow" name "::" ann)
     (s-bind (loc stx) #t (parse-name #'name) (parse-ann #'ann))]))

(define (parse-args stx)
  (syntax-parse stx
    #:datum-literals (args list-arg-elt)
    [(args "(" ")") empty]
    [(args "(" (list-arg-elt arg1 ",") ... lastarg ")")
     (map/stx parse-binding #'(arg1 ... lastarg))]))

(define (parse-variant-member stx)
  (syntax-parse stx
    #:datum-literals (variant-member)
    [(variant-member b)
     (s-variant-member (loc stx) 'normal (parse-binding #'b))]
    [(variant-member "mutable" b)
     (s-variant-member (loc stx) 'mutable (parse-binding #'b))]
    [(variant-member "cyclic" b)
     (s-variant-member (loc stx) 'cyclic (parse-binding #'b))]))

(define (parse-variant-members stx)
  (syntax-parse stx
    #:datum-literals (variant-members list-variant-member)
    [(variant-members "(" ")") empty]
    [(variant-members "(" (list-variant-member vm1 ",") ... lastvm ")")
     (map/stx parse-variant-member #'(vm1 ... lastvm))]))

(define (parse-key stx)
  (syntax-parse stx
    #:datum-literals (key)
    [(key "[" expr "]") (parse-binop-expr #'expr)]
    [(key name) (s-str (loc stx) (symbol->string (parse-name #'name)))]))

(define (parse-obj-field stx)
  (syntax-parse stx
    #:datum-literals (obj-field)
    [(obj-field "mutable" key ":" value)
     (s-mutable-field (loc stx)
                      (parse-key #'key)
                      (a-blank)
                      (parse-binop-expr #'value))]
    [(obj-field "mutable" key "::" ann ":" value)
     (s-mutable-field (loc stx)
                      (parse-key #'key)
                      (parse-ann #'ann)
                      (parse-binop-expr #'value))]
    [(obj-field key ":" value)
     (s-data-field (loc stx)
                   (parse-key #'key)
                   (parse-binop-expr #'value))]
    [(obj-field key args ret ":" doc body check (end (~or "end" ";")))
     (s-method-field (loc stx)
                     (parse-key #'key)
                     (parse-args #'args)
                     (parse-return-ann #'ret)
                     (parse-doc-string #'doc)
                     (parse-block #'body)
                     (parse-where-clause #'check))]))

(define (parse-obj-fields stx)
  (syntax-parse stx
    #:datum-literals (obj-fields list-obj-field)
    [(obj-fields (list-obj-field f1 ",") ... lastfield)
     (map/stx parse-obj-field #'(f1 ... lastfield))]
    [(obj-fields (list-obj-field f1 ",") ... lastfield ",")
     (map/stx parse-obj-field #'(f1 ... lastfield))]))


(define (parse-field stx)
  (syntax-parse stx
    #:datum-literals (field)
    [(field key ":" value)
     (s-data-field (loc stx)
                   (parse-key #'key)
                   (parse-binop-expr #'value))]
    [(field key args ret ":" doc body check (end (~or "end" ";")))
     (s-method-field (loc stx)
                     (parse-key #'key)
                     (parse-args #'args)
                     (parse-return-ann #'ret)
                     (parse-doc-string #'doc)
                     (parse-block #'body)
                     (parse-where-clause #'check))]))

(define (parse-fields stx)
  (syntax-parse stx
    #:datum-literals (fields list-field)
    [(fields (list-field f1 ",") ... lastfield)
     (map/stx parse-field #'(f1 ... lastfield))]
    [(fields (list-field f1 ",") ... lastfield ",")
     (map/stx parse-field #'(f1 ... lastfield))]))

(define (parse-mixins stx)
  (syntax-parse stx
    #:datum-literals (data-mixins)
    [(data-mixins) empty]
    [(data-mixins "deriving" mixins) (parse-all-mixins #'mixins)]))
(define (parse-all-mixins stx)
  (syntax-parse stx
    #:datum-literals (mixins list-mixin)
    [(mixins (list-mixin m1 ",") ... lastmixin)
     (map/stx parse-binop-expr #'(m1 ... lastmixin))]))

(define (parse-app-args stx)
  (syntax-parse stx
    #:datum-literals (app-args app-arg-elt)
    [(app-args "(" ")") empty]
    [(app-args "(" (app-arg-elt e1 ",") ... elast ")")
     (map/stx parse-binop-expr #'(e1 ... elast))]))

(define (parse-cases-branch stx)
  (syntax-parse stx
    #:datum-literals (cases-branch)
    [(cases-branch "|" name "=>" body)
     (s-cases-branch (loc stx) (parse-name #'name) empty (parse-block #'body))]
    [(cases-branch "|" name args "=>" body)
     (s-cases-branch (loc stx) (parse-name #'name) (parse-args #'args) (parse-block #'body))]))


(define (parse-else-if stx)
  (syntax-parse stx
    #:datum-literals (else-if)
    [(else-if "else if" test ":" body)
     (s-if-branch (loc stx) (parse-binop-expr #'test) (parse-block #'body))]))

(define (parse-else stx)
  (syntax-parse stx
    #:datum-literals (else)
    [(else "else:" body) (parse-block #'body)]))

(define (parse-ty-params stx)
  (syntax-parse stx
    #:datum-literals (ty-params list-ty-param)
    [(ty-params) empty]
    [(ty-params "<" (list-ty-param param ",") ... last ">")
     (map/stx parse-name #'(param ... last))]))


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
     (s-for-bind (loc stx) (parse-binding #'name) (parse-binop-expr #'expr))]))


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
      colon-expr
      colon-bracket-expr
      for-expr
      try-expr
      let-expr let-binding
      lambda-expr
      method-expr
      extend-expr
      left-app-expr
      paren-expr
      not-expr
      expr
    )
    [(prim-expr e) (parse-prim #'e)]
    [(obj-expr "{" "}") (s-obj (loc stx) empty)]
    [(obj-expr "{" obj-fields "}") (s-obj (loc stx) (parse-obj-fields #'obj-fields))]
    [(list-expr "[" "]") (s-list (loc stx) empty)]
    [(list-expr "[" (list-elt e1 ",") ... elast "]")
     (s-list (loc stx) (map/stx parse-binop-expr #'(e1 ... elast)))]
    [(app-expr efun eargs)
     (s-app (loc stx) (parse-expr #'efun) (parse-app-args #'eargs))]
    [(id-expr x) (s-id (loc stx) (parse-name #'x))]
    [(dot-expr obj "." field)
     (s-dot (loc stx) (parse-expr #'obj) (parse-name #'field))]
    [(get-bang-expr obj "!" field)
     (s-get-bang (loc stx) (parse-expr #'obj) (parse-name #'field))]
    [(bracket-expr obj "." "[" field "]")
     (s-bracket (loc stx) (parse-expr #'obj) (parse-binop-expr #'field))]
    [(colon-expr obj ":" field)
     (s-colon (loc stx) (parse-expr #'obj) (parse-name #'field))]
    [(colon-bracket-expr obj ":" "[" field "]")
     (s-colon-bracket (loc stx) (parse-expr #'obj) (parse-binop-expr #'field))]
    [(cases-expr "cases" "(" type ")" val ":" branch ... "|" "else" "=>" else-block (end (~or "end" ";")))
     (s-cases-else (loc stx) (parse-ann #'type) (parse-expr #'val)
      (map/stx parse-cases-branch #'(branch ...))
      (parse-block #'else-block))]
    [(cases-expr "cases" "(" type ")" val ":" branch ... (end (~or "end" ";")))
     (s-cases (loc stx) (parse-ann #'type) (parse-expr #'val)
      (map/stx parse-cases-branch #'(branch ...)))]
    [(if-expr "if" test ":" body branch ... "else:" else-block (end (~or "end" ";")))
     (s-if-else (loc stx)
       (cons
         (s-if-branch (loc #'test) (parse-binop-expr #'test) (parse-block #'body))
         (map/stx parse-else-if #'(branch ...)))
       (parse-block #'else-block))]
    [(if-expr "if" test ":" body branch ... (end (~or "end" ";")))
     (s-if (loc stx)
       (cons
         (s-if-branch (loc #'test) (parse-binop-expr #'test) (parse-block #'body))
         (map/stx parse-else-if #'(branch ...))))]
    [(for-expr "for" iter "(" (for-bind-elt binds ",") ... last-bind ")" return-ann ":" body (end (~or "end" ";")))
     (s-for (loc stx)
            (parse-expr #'iter)
            (map/stx parse-for-bind #'(binds ... last-bind))
            (parse-return-ann #'return-ann)
            (parse-block #'body))]
    [(for-expr "for" iter "(" ")" return-ann ":" body (end (~or "end" ";")))
     (s-for (loc stx)
            (parse-expr #'iter)
            empty
            (parse-return-ann #'return-ann)
            (parse-block #'body))]
    [(try-expr "try:" body "except" "(" arg-elt ")" ":" except (end (~or "end" ";")))
     (s-try (loc stx)
            (parse-block #'body)
            (parse-binding #'arg-elt)
            (parse-block #'except))]
    [(user-block-expr "block:" body (end (~or "end" ";")))
     (s-user-block (loc stx) (parse-block #'body))]
    [(lambda-expr "fun" ty-params args return-ann ":" doc body check (end (~or "end" ";")))
     (s-lam (loc stx)
            (parse-ty-params #'ty-params)
            (parse-args #'args)
            (parse-return-ann #'return-ann)
            (parse-doc-string #'doc)
            (parse-block #'body)
            (parse-where-clause #'check))]
    [(lambda-expr "fun" ty-params return-ann ":" doc body check (end (~or "end" ";")))
     (s-lam (loc stx)
            (parse-ty-params #'ty-params)
            (list)
            (parse-return-ann #'return-ann)
            (parse-doc-string #'doc)
            (parse-block #'body)
            (parse-where-clause #'check))]
    [(method-expr "method" args return-ann ":" doc body check (end (~or "end" ";")))
     (s-method (loc stx)
            (parse-args #'args)
            (parse-return-ann #'return-ann)
            (parse-doc-string #'doc)
            (parse-block #'body)
            (parse-where-clause #'check))]
    [(extend-expr e "." "{" fields "}")
     (s-extend (loc stx) (parse-expr #'e) (parse-fields #'fields))]
    [(update-expr e "!" "{" fields "}")
     (s-update (loc stx) (parse-expr #'e) (parse-fields #'fields))]
    [(left-app-expr e "^" fun-expr app-args)
     (s-left-app (loc stx)
                 (parse-expr #'e)
                 (parse-left-app-fun-expr #'fun-expr)
                 (parse-app-args #'app-args))]
    [(paren-expr "(" e ")") (s-paren (loc stx) (parse-binop-expr #'e))]
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
     (a-field (loc stx) (symbol->string (parse-name #'n)) (parse-ann #'ann))]
    [(ann-field n "::" ann)
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
    [(name-ann "Any") (a-any)]
    [(name-ann n) (a-name (loc stx) (parse-name #'n))]
    [(record-ann "{" "}") (a-record (loc stx) empty)]
    [(record-ann "{" (list-ann-field fields ",") ... lastfield "}")
     (a-record (loc stx) (map/stx parse-ann-field #'(fields ... lastfield)))]
    [(arrow-ann "(" "->" result ")")
     (a-arrow (loc stx)
              empty
              (parse-ann #'result))]
    [(arrow-ann "(" (arrow-ann-elt anns ",") ... last-ann "->" result ")")
     (a-arrow (loc stx)
              (map/stx parse-ann #'(anns ... last-ann))
              (parse-ann #'result))]
    [(app-ann the-ann "<" (app-ann-elt anns ",") ... last-ann ">")
     (a-app (loc stx)
            (parse-ann #'the-ann)
            (map/stx parse-ann #'(anns ... last-ann)))]
    [(pred-ann annbase "(" expr ")")
     (a-pred (loc stx) (parse-ann #'annbase) (parse-binop-expr #'expr))]
    [(dot-ann n1 "." n2)
     (a-dot (loc stx) (parse-name #'n1) (parse-name #'n2))]
    [(ann a) (parse-ann #'a)]))
