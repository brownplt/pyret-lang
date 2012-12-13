#lang typed/racket

(provide
  (struct-out s-block)
  (struct-out s-fun)
  (struct-out s-def)
  (struct-out s-bind)
  (struct-out s-cond)
  (struct-out s-cond-branch)

  (struct-out s-lam)

  (struct-out s-data-field)
  (struct-out s-method-field)
  (struct-out s-obj)
  (struct-out s-onion)
  
  (struct-out s-method)

  (struct-out s-id)
  (struct-out s-assign)
  (struct-out s-app)

  (struct-out s-list)

  (struct-out s-num)
  (struct-out s-bool)
  (struct-out s-str)

  (struct-out s-dot)
  (struct-out s-bracket)
  (struct-out s-dot-assign)
  (struct-out s-bracket-assign)
  (struct-out s-dot-method)
  (struct-out s-bracket-method)

  (struct-out s-data)
  (struct-out s-variant)
  (struct-out s-member)
  
  (struct-out s-do)
  
  (struct-out a-blank)
  (struct-out a-any)
  (struct-out a-name)
  (struct-out a-arrow)
  (struct-out a-record)
  (struct-out a-field)
  (struct-out a-app)
)

#|
(define (f-member ast-node)
    (match ast-node
      [(s-data-field _ name value) ]
      [(s-method-field _ name args body) ]))
  (match ast-node
    
    [(s-block _ l) ]

    [(s-num _ n) ]
    [(s-bool _ b) ]
    [(s-str _ s) ]

    [(s-lam _ args ann body) ]
    
    [(s-cond _ c-bs) ]
    
    [(s-cond-branch _ tst blk) ]
    
    [(s-id _ name) ]
    
    [(s-assign _ name expr) ]

    [(s-app _ fun args) ]

    [(s-onion _ super fields) ]

    [(s-obj _ fields) ]
    
    [(s-list _ elts) ]
    
    [(s-dot _ val field) ]
    
    [(s-bracket _ val field) ]
    
    [(s-dot-assign _ obj field val) ]

    [(s-dot-method _ obj field args) ]
    
|#

#|

The concrete AST for surface Pyret.

Each syntactic form has a srcloc object associated with it, for error
reporting and keeping track of source locations.  The srcloc object
should not be required for evaluating the ast node, and only used for
these metadata purposes.

|#

(define-type Block (Listof Stmt))
(struct: s-block ((syntax : srcloc) (stmts : Block)) #:transparent)

(struct: s-bind ((syntax : srcloc) (id : Symbol) (ann : Ann))
	 #:transparent)


(define-type Stmt (U s-fun s-def s-cond s-data s-do Expr))
(struct: s-fun ((syntax : srcloc)
		(name : Symbol)
		(args : (Listof s-bind))
		(ann : Ann)
		(body : s-block))
	 #:transparent)
(struct: s-def ((syntax : srcloc)
		(name : s-bind)
		(value : Expr))
	 #:transparent)
(struct: s-cond ((syntax : srcloc)
		 (branches : (Listof s-cond-branch)))
	 #:transparent)
(struct: s-cond-branch ((syntax : srcloc)
			(expr : Expr)
			(body : s-block))
	 #:transparent)

(define-type Expr (U s-obj s-onion s-list s-app s-id
		     s-assign s-num s-bool s-str
                     s-dot s-bracket s-dot-assign s-bracket-assign
                     s-dot-method s-bracket-method s-lam
                     s-block))

(struct: s-lam ((syntax : srcloc)
		(args : (Listof s-bind))
		(ann : Ann)
		(body : s-block))
	 #:transparent)

(struct: s-method ((syntax : srcloc)
                   (args : (Listof s-bind))
                   (ann : Ann)
                   (body : s-block))
	 #:transparent)

(define-type Member (U s-data-field s-method-field))
(struct: s-data-field ((syntax : srcloc)
		  (name : String)
		  (value : Expr))
	 #:transparent)
(struct: s-method-field ((syntax : srcloc)
		   (name : String)
		   (args : (Listof s-bind))
		   (body : s-block))
	 #:transparent)

(struct: s-onion ((syntax : srcloc)
		  (super : Expr)
		  (fields : (Listof Member)))
	 #:transparent)
(struct: s-obj ((syntax : srcloc)
		(fields : (Listof Member)))
	 #:transparent)

(struct: s-list ((syntax : srcloc)
		 (values : (Listof Expr)))
	 #:transparent)

(struct: s-app ((syntax : srcloc)
		(fun : Expr)
		(args : (Listof Expr)))
	 #:transparent)

(struct: s-id ((syntax : srcloc)
	       (id : Symbol))
	 #:transparent)

(struct: s-assign ((syntax : srcloc)
		   (id : Symbol)
		   (value : Expr))
	 #:transparent)

(struct: s-num ((syntax : srcloc) (n : Number)) #:transparent)
(struct: s-bool ((syntax : srcloc) (b : Boolean)) #:transparent)
(struct: s-str ((syntax : srcloc) (s : String)) #:transparent)

(struct: s-dot ((syntax : srcloc)
		(obj : Expr)
		(field : Symbol))
	 #:transparent)
(struct: s-bracket ((syntax : srcloc)
		    (obj : Expr)
		    (field : Expr))
	 #:transparent)

(struct: s-dot-assign ((syntax : srcloc)
		       (obj : Expr)
		       (field : Symbol)
		       (value : Expr))
	 #:transparent)
(struct: s-bracket-assign ((syntax : srcloc)
			   (obj : Expr)
			   (field : Expr)
			   (value : Expr))
	 #:transparent)

(struct: s-dot-method ((syntax : srcloc)
		       (obj : Expr)
		       (field : Symbol)
		       (args : (Listof Expr)))
	 #:transparent)
(struct: s-bracket-method ((syntax : srcloc)
			   (obj : Expr)
			   (field : Expr)
			   (args : (Listof Expr)))
	 #:transparent)

(struct: s-data ((syntax : srcloc)
                 (name : Symbol)
                 (params : (Listof Symbol))
                 (variants : (Listof s-variant)))
                #:transparent)

(struct: s-variant ((syntax : srcloc)
                    (name : Symbol)
                    (members : (Listof s-member)))
                   #:transparent)

(struct: s-member ((syntax : srcloc)
                   (name : Symbol)
                   (ann : Ann))
                  #:transparent)

(struct: s-do ((syntax : srcloc)
               (init : Stmt)
               (args : (Listof Stmt)))
              #:transparent)

(define-type Ann (U a-blank a-any a-name a-arrow a-method a-record a-app))
(struct: a-blank () #:transparent)
(struct: a-any () #:transparent)
(struct: a-name ((syntax : srcloc) (id : Symbol)) #:transparent)
(struct: a-arrow ((syntax : srcloc)
		  (args : (Listof Ann))
		  (ret : Ann))
	 #:transparent)
(struct: a-method ((syntax : srcloc)
                   (args : (Listof Ann))
                   (ret : Ann))
	 #:transparent)
(struct: a-field ((syntax : srcloc)
		  (name : String)
		  (ann : Ann))
	 #:transparent)
(struct: a-record ((syntax : srcloc)
		   (fields : (Listof a-field)))
	 #:transparent)
(struct: a-app ((syntax : srcloc)
		(name : Symbol)
		(parameters : (Listof Ann)))
	 #:transparent)

