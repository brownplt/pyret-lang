#lang racket

(require "ast.rkt")
(provide typecheck-pyret)

(define (ann-check ann)
  (match ann
    [(a-name s id) (string->symbol (string-append (symbol->string id)
						  "?"))]
    [else (error 'typecheck (format "don't know how to check ann: ~a"
				    ann))]))

(define (typecheck-pyret ast)
  (match ast
    [(s-block s stmts)
     (s-block s (map typecheck-pyret stmts))]
    [(s-def s1 (s-bind s2 nm ann) val)
     (s-block
      s1
      (list
       (s-def s1 (s-bind s1 nm (a-any)) val)
       (s-app s2 (s-id s2 'check) (list (s-id s2 (ann-check ann))
					(s-id s2 nm)))))]
    [else ast]))
