#lang racket

(require "ast.rkt")
(provide typecheck-pyret)

(define (bind-strip-ann bnd)
  (match bnd
    [(s-bind s id ann)
     (s-bind s id (a-any))]
    [else (error (format "typecheck: expected s-bind, received ~a"
			 bnd))]))

(define (ann-check loc ann expr)
  (s-app
   loc
   (s-id loc 'check-brand)
   (list
    (s-id loc
	  (match ann
	    [(a-name s id)
	     (string->symbol
	      (string-append
	       (symbol->string id) "?"))]
	    [(a-blank) 'Any?]
	    [(a-any) 'Any?]
	    [else
	     (error
	      (format "typecheck: don't know how to check ann: ~a"
		      ann))]))
    expr)))

(define (typecheck-pyret ast)
  (match ast
    [(s-block s stmts)
     (s-block s (map typecheck-pyret stmts))]
    [(s-def s bnd val)
     (s-def s (bind-strip-ann bnd) (ann-check s (s-bind-ann bnd) val))]
    [(s-bind s id ann)
     (ann-check s ann (s-id s id))]
    [(s-fun s nm args ann bdy)
     (s-fun
      s nm (map bind-strip-ann args) (a-any)
      (if (empty? args) bdy 
      (s-block
       s
       (append
	;; on entry, check that args are correct
	(map typecheck-pyret args)
	;; check that body returns the right type
	(s-block-stmts bdy)
	;; want to check return, but body is Stmt, not Expr,
	;; and changing it causes all sorts of tests to blow up
	#;(list
	(ann-check s ann bdy))))))]
    [else ast]))
