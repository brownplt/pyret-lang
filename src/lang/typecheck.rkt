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
  (match ann
    [(a-name s id)
     (s-app loc (s-id s 'check)
	    (list (s-id s (string->symbol
			   (string-append
			    (symbol->string id) "?")))
		  expr))]
    [(a-blank) (s-block loc empty)]
    [(a-any) (s-block loc empty)]
    [else (error (format "typecheck: don't know how to check ann: ~a"
			 ann))]))

(define (typecheck-pyret ast)
  (match ast
    [(s-block s stmts)
     (s-block s (map typecheck-pyret stmts))]
    [(s-def s bnd val)
     (s-block
      s
      (list
       (s-def s (bind-strip-ann bnd) val)
       (ann-check s (s-bind-ann bnd) (s-id s (s-bind-id bnd)))))]
    [(s-bind s id ann)
     (ann-check s ann (s-id s id))]
    [(s-fun s nm args ann bdy)
     (s-block
      s
      (list
       (s-fun
	s nm (map bind-strip-ann args) (a-any)
	(s-block
	 s
	 (append
	  ;; on entry, check that args are correct
	  (map typecheck-pyret args)
	  ;; check that body returns the right type
	  (list
	   (ann-check s ann bdy)))))))]
    [else ast]))
