#lang racket/base

(provide (all-defined-out))
(require
  (only-in racket/list empty)
  (for-syntax racket/base)
  ;; We need to define the `expr` literal
  (except-in syntax/parse expr)
  syntax/strip-context
  "ast.rkt")

;; borrowed from dyoo's brainfudge
(define (loc stx)
    (srcloc (syntax-source stx)
            (syntax-line stx)
            (syntax-column stx)
            (syntax-position stx)
            (syntax-span stx)))


(define-syntax-rule (define-pyret-syntax-tools (id ...))
  (define-syntaxes (id ...)
    (values (lambda (stx) #'(raise-syntax-error #f (format "~a used out of context") 'id)) ...))
  )

;; NOTE(joe):  When you add a new piece of syntax that is used in a
;; #:literals declaration, you need to add it here.  syntax-parse will
;; complain that the literal is not bound.  There should be one of these for
;; every named production in grammar.rkt.
(define-pyret-syntax-tools (
  program block stmt expr
  imports import-stmt provide-stmt import-name import-string
  var-expr
  let-expr
  fun-expr fun-header fun-body args arg-elt list-arg-elt return-ann
  data-expr data-sharing data-variant
  do-expr do-stmt
  assign-expr
  when-expr
  try-expr
  obj-expr fields list-field field
  list-expr list-elt
  app-expr app-args app-arg-elt
  id-expr 
  prim-expr
  dot-expr 
  bracket-expr 
  dot-method-expr 
  bracket-method-expr
  cond-expr cond-branch
  lambda-expr lambda-args ty-params list-ty-param
  extend-expr 
  left-app-expr left-app-fun-expr
  bool-expr
  num-expr
  string-expr
))

(define-syntax-rule (pyret-parse stx body ...)
  (syntax-parse (replace-context #'here stx)
    body ...))


(define (parse-name n) (string->symbol (syntax->datum n)))

(define (parse-string stx)
  (let [(str-val (syntax->datum stx))]
    (substring str-val 1 (sub1 (string-length str-val)))))

(define (parse-num stx)
  (string->number (syntax->datum stx)))

(define (map/stx f stx)
  (map (λ (s) (f (datum->syntax #'here s))) (syntax->datum stx)))

(define (parse-program stx)
  (pyret-parse stx
    #:literals (program imports)
    [(program (imports import ...) body "")
     (s-prog (loc stx)
             (map/stx parse-import #'(import ...))
             (parse-block #'body))]))

(define (parse-import stx)
  (pyret-parse stx
    #:literals (import-stmt provide-stmt)
    [(provide-stmt "provide" stmt "end") (s-provide (loc stx) (parse-block #'stmt))]
    [(import-stmt "import" import-module "as" name)
     (s-import (loc stx) (parse-import-module #'import-module) (parse-name #'name))]))

(define (parse-import-module stx)
  (pyret-parse stx
    #:literals (import-name import-string)
    [(import-name n) (parse-name #'n)]
    [(import-string s) (parse-string #'s)]))


(define (parse-block stx)
  (pyret-parse stx
    #:literals (block)
    [(block stmts ...)
     (s-block (loc stx) (map/stx parse-stmt-wrapper #'(stmts ...)))]))

(define (parse-stmt-wrapper stx)
  (pyret-parse stx
    #:literals (stmt)
    [(stmt s) (parse-stmt #'s)]))

(define (parse-with stx)
  (pyret-parse stx
    #:literals (data-with)
    [(data-with) empty]
    [(data-with "with" fields) (parse-fields #'fields)]))

(define (parse-variant stx)
  (pyret-parse stx
    #:literals (data-variant)
    [(data-variant "|" name args with)
     (s-variant (parse-name #'name) (parse-args #'args) (parse-with #'with))]
    [(data-variant "|" name with)
     (s-singleton-variant (parse-name #'name) (parse-with #'with))]))

(define (parse-sharing stx)
  (pyret-parse stx
    #:literals (data-sharing)
    [(data-sharing "sharing" fields "end") (parse-fields #'fields)]
    [(data-sharing "end") empty]))


(define (parse-stmt stx)
  (pyret-parse stx
    #:literals (
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
     (s-var (loc stx) (parse-arg-elt #'bind) (parse-expr #'e))]
    [(let-expr bind "=" e)
     (s-let (loc stx) (parse-arg-elt #'bind) (parse-expr #'e))]
    [(fun-expr "fun" (fun-header params fun-name args return) ":"
        (fun-body (block (stmt (expr (prim-expr (string-expr doc))))
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

    [(do-expr fun-stmt (do-stmt stmts ";") ... last-stmt "end")
     (s-do (loc stx)
           (parse-stmt #'fun-stmt)
           (map/stx parse-block #'(stmts ... last-stmt)))]

    [(assign-expr id ":=" e)
     (s-assign (loc stx) (parse-name #'id) (parse-expr #'e))]

    [(when-expr "when" test ":" body "end")
     (s-when (loc stx) (parse-expr #'test) (parse-block #'body))]

    [(try-expr "try" ":" body "except" "(" arg-elt ")" ":" except "end")
     (s-try (loc stx)
            (parse-block #'body)
            (parse-arg-elt #'arg-elt)
            (parse-block #'except))]
      
    [(stmt s) (parse-stmt #'s)]
    [(expr e) (parse-expr #'e)]))

(define (parse-return-ann stx)
  (pyret-parse stx
    #:literals (return-ann)
    [(return-ann) (a-blank)]
    [(return-ann "->" ann) (parse-ann #'ann)]))

(define (parse-arg-elt stx)
  (pyret-parse stx
    #:literals (arg-elt)
    [(arg-elt name) (s-bind (loc stx) (parse-name #'name) (a-blank))]
    [(arg-elt name "::" ann)
     (s-bind (loc stx) (parse-name #'name) (parse-ann #'ann))]))

(define (parse-args stx)
  (pyret-parse stx
    #:literals (args list-arg-elt)
    [(args "(" ")") empty]
    [(args "(" (list-arg-elt arg1 ",") ... lastarg ")") 
     (map/stx parse-arg-elt #'(arg1 ... lastarg))]))

(define (parse-field stx)
  (pyret-parse stx
    #:literals (field)
    [(field key ":" value)
     (s-data-field (loc stx)
                   (s-str (loc stx) (symbol->string (parse-name #'key)))
                   (parse-stmt #'value))]
    [(field "[" key "]" ":" value)
     (s-data-field (loc stx)
                   (parse-expr #'key)
                   (parse-expr #'value))]
    [(field key args ret ":" body "end")
     (s-method-field (loc stx)
                   (s-str (loc stx) (symbol->string (parse-name #'key)))
                     (parse-args #'args)
                     (parse-return-ann #'ret)
                     (parse-block #'body))]
    [(field "[" key "]" args ret ":" body "end")
     (s-method-field (loc stx)
                     (parse-expr #'key)
                     (parse-args #'args)
                     (parse-return-ann #'ret)
                     (parse-block #'body))]))

(define (parse-fields stx)
  (pyret-parse stx
    #:literals (fields list-field)
    [(fields (list-field f1 ",") ... lastfield)
     (map/stx parse-field #'(f1 ... lastfield))]
    [(fields (list-field f1 ",") ... lastfield ",")
     (map/stx parse-field #'(f1 ... lastfield))]))

(define (parse-app-args stx)
  (pyret-parse stx
    #:literals (app-args app-arg-elt)
    [(app-args "(" ")") empty]
    [(app-args "(" (app-arg-elt e1 ",") ... elast ")")
     (map/stx parse-expr #'(e1 ... elast))]))

(define (parse-cond-branch stx)
  (pyret-parse stx
    #:literals (cond-branch)
    [(cond-branch "|" test "=>" body)
     (s-cond-branch (loc stx) (parse-expr #'test) (parse-block #'body))]))

(define (parse-lambda-args stx)
  (pyret-parse stx
    #:literals (lambda-args list-arg-elt)
    [(lambda-args ) empty]
    [(lambda-args (list-arg-elt arg1 ",") ... lastarg) 
     (map/stx parse-arg-elt #'(arg1 ... lastarg))]))

(define (parse-ty-params stx)
  (pyret-parse stx
    #:literals (ty-params list-ty-param)
    [(ty-params) empty]
    [(ty-params "<" (list-ty-param param ",") ... last ">")
     (map/stx parse-name #'(param ... last))]))

(define (parse-fun-body stx)
  (pyret-parse stx
    #:literals (fun-body)
    [(fun-body block "end") (parse-block #'block)]
    [(fun-body "(" block ")") (parse-block #'block)]))

(define (parse-left-app-fun-expr stx)
  (pyret-parse stx
    #:literals (left-app-fun-expr)
    [(left-app-fun-expr id) (parse-expr #'id)]
    [(left-app-fun-expr id "." name)
     (s-dot (loc stx) #'id (parse-name #'name))]))

(define (parse-expr stx)
  (pyret-parse stx
    #:literals (
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
     (s-list (loc stx) (map/stx parse-expr #'(e1 ... elast)))]
    [(app-expr efun eargs)
     (s-app (loc stx) (parse-expr #'efun) (parse-app-args #'eargs))]
    [(id-expr x) (s-id (loc stx) (parse-name #'x))]
    [(dot-expr obj "." field)
     (s-dot (loc stx) (parse-expr #'obj) (parse-name #'field))]
    [(bracket-expr obj "." "[" field "]")
     (s-bracket (loc stx) (parse-expr #'obj) (parse-expr #'field))]
    [(dot-method-expr obj ":" field)
     (s-dot-method (loc stx) (parse-expr #'obj) (parse-name #'field))]
    [(bracket-method-expr obj ":" "[" field "]")
     (s-bracket-method (loc stx) (parse-expr #'obj) (parse-expr #'field))]
    [(cond-expr "cond" ":" branch ... "end")
     (s-cond (loc stx) (map/stx parse-cond-branch #'(branch ...)))]
    [(lambda-expr "\\" ty-params args return-ann ":" fun-body)
     (s-lam (loc stx)
            (parse-ty-params #'ty-params)
            (parse-lambda-args #'args)
            (parse-return-ann #'return-ann)
            ""
            (parse-fun-body #'fun-body))]
    [(lambda-expr "\\" fun-body)
     (s-lam (loc stx) empty empty (a-blank) "" (parse-fun-body #'fun-body))]
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
    #:literals (
      bool-expr
      num-expr
      string-expr
    )
    [(bool-expr "true") (s-bool (loc stx) #t)]
    [(bool-expr "false") (s-bool (loc stx) #f)]
    [(num-expr n) (s-num (loc stx) (parse-num #'n))]
    [(num-expr "-" n) (s-num (loc stx) (- (parse-num #'n)))]
    [(string-expr s) (s-str (loc stx) (parse-string #'s))]))


(define (parse-ann stx)
  (pyret-parse stx
    #:literals ()
    [_ (a-blank)]))

(require "get-syntax.rkt")
(parse-program (get-syntax 'foo (open-input-string "'hi'")))

