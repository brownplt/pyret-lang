#lang typed/racket

(provide
  (struct-out s-block)
  (struct-out s-fun)
  (struct-out s-def)

  (struct-out s-data)
  (struct-out s-method)
  (struct-out s-obj)

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

)

#|

The concrete AST for surface Pyret.

Each syntactic form has a srclo object associated with it, for error
reporting and keeping track of source locations.  The srcloc object
should not be required for evaluating the ast node, and only used for
these metadata purposes.

|#

(define-type Block (Listof Stmt))
(struct: s-block ((syntax : srcloc) (stmts : Block)) #:transparent)

(define-type Stmt (U s-fun s-def Expr))
(struct: s-fun ((syntax : srcloc) (name : Symbol) (args : (Listof Symbol)) (body : s-block)) #:transparent)
(struct: s-def ((syntax : srcloc) (name : Symbol) (value : Expr)) #:transparent)

(define-type Expr (U s-obj s-list s-app s-id s-assign s-num s-bool s-str s-dot 
                     s-bracket s-dot-assign s-bracket-assign))

(define-type Member (U s-data s-method))
(struct: s-data ((syntax : srcloc) (name : String) (value : Expr)) #:transparent)
(struct: s-method ((syntax : srcloc) (name : String) (args : (Listof Symbol)) (body : Block)) #:transparent)
(struct: s-obj ((syntax : srcloc) (fields : (Listof Member))) #:transparent)

(struct: s-list ((syntax : srcloc) (values : (Listof Expr))) #:transparent)

(struct: s-app ((syntax : srcloc) (fun : Expr) (args : (Listof Expr))) #:transparent)

(struct: s-id ((syntax : srcloc) (id : Symbol)) #:transparent)

(struct: s-assign ((syntax : srcloc) (id : Symbol) (value : Expr)) #:transparent)

(struct: s-num ((syntax : srcloc) (n : Number)) #:transparent)
(struct: s-bool ((syntax : srcloc) (b : Boolean)) #:transparent)
(struct: s-str ((syntax : srcloc) (s : String)) #:transparent)

(struct: s-dot ((syntax : srcloc) (obj : Expr) (field : Symbol)) #:transparent)
(struct: s-bracket ((syntax : srcloc) (obj : Expr) (field : Expr)) #:transparent)

(struct: s-dot-assign ((syntax : srcloc) (obj : Expr) (field : Symbol) (value : Expr)) #:transparent)
(struct: s-bracket-assign ((syntax : srcloc) (obj : Expr) (field : Expr) (value : Expr)) #:transparent)

;; used for creating ad hoc AST nodes that didn't come from surface
;; syntax
(define dummy #'dummy-syntax)

