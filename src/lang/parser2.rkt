#lang racket/base

(provide (all-defined-out))
(require
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
;; complain that the literal is not bound.
(define-pyret-syntax-tools (
  program block stmt expr
  imports import-stmt provide-stmt import-name import-string
  var-expr
  let-expr
  fun-expr
  data-expr
  do-expr
  assign-expr
  when-expr
  try-expr
  obj-expr 
  list-expr 
  app-expr 
  id-expr 
  prim-expr
  dot-expr 
  bracket-expr 
  dot-method-expr 
  bracket-method-expr
  cond-expr 
  lambda-expr 
  extend-expr 
  left-app-expr
  string-expr
))

(define-syntax-rule (pyret-parse stx body ...)
  (syntax-parse (replace-context #'here stx)
    body ...))


(define (parse-name n) (string->symbol (syntax->datum n)))

(define (parse-string stx)
  (let [(str-val (syntax->datum stx))]
    (substring str-val 1 (sub1 (string-length str-val)))))

(define (parse-program stx)
  (pyret-parse stx
    #:literals (program imports)
    [(program (imports import ...) body "")
     (s-prog (loc stx)
             (map parse-import (syntax->datum #'(import ...)))
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
     (s-block (loc stx)
              (map (λ (s) (parse-stmt-wrapper (datum->syntax #'here s)))
                   (syntax->datum #'(stmts ...))))]))

(define (parse-stmt-wrapper stx)
  (pyret-parse (replace-context #'here stx)
    #:literals (stmt)
    [(stmt s) (parse-stmt #'s)]))

(define (parse-stmt stx)
  (syntax-parse stx
    #:literals (
      var-expr
      let-expr
      fun-expr
      data-expr
      do-expr
      assign-expr
      when-expr
      try-expr
      expr
    )
    [(expr e) (parse-expr #'e)]))

(define (parse-expr stx)
  (syntax-parse stx
    #:literals (
      obj-expr 
      list-expr 
      app-expr 
      id-expr 
      prim-expr
      dot-expr 
      bracket-expr 
      dot-method-expr 
      bracket-method-expr
      cond-expr 
      lambda-expr 
      extend-expr 
      left-app-expr
    )
    [(prim-expr e) (parse-prim #'e)]))

(define (parse-prim stx)
  (syntax-parse stx
    #:literals (
      string-expr
    )
    [(string-expr s) (s-str (loc stx) (parse-string #'s))]))

(require "get-syntax.rkt")
(parse-program (get-syntax 'foo (open-input-string "'hi'")))

