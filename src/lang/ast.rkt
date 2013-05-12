#lang whalesong

(provide (all-defined-out))

#|

The concrete AST for surface Pyret.

Each syntactic form has a srcloc object associated with it, for error
reporting and keeping track of source locations.  The srcloc object
should not be required for evaluating the ast node, and only used for
these metadata purposes.

|#

;; s-prog : srcloc (Listof Header) s-block -> s-prog
(struct s-prog (syntax imports block) #:transparent)

;; A Header is a (U s-import s-provide)
;; s-import : srcloc (U String Symbol) Symbol -> srcloc
(struct s-import (syntax file name) #:transparent)
;; s-provide : srcloc expr -> srcloc
(struct s-provide (syntax expr) #:transparent)


;; A Block is a (Listof Stmt)
;; s-block : srcloc Block -> s-block
(struct s-block (syntax stmts) #:transparent)

;; s-bind : srcloc Symbol Ann -> s-bind
(struct s-bind (syntax id ann) #:transparent)

;; A Stmt is a (U s-fun s-var s-cond s-try s-data s-do s-import Expr)
;; s-fun : srcloc Symbol (Listof Symbol) (Listof s-bind) Ann String s-block
(struct s-fun (syntax name params args ann doc body) #:transparent)

;; s-var : srcloc bind Expr -> s-var
(struct s-var (syntax name value) #:transparent)
;; s-let : srcloc bind Expr -> s-let
(struct s-let (syntax name value) #:transparent)
;; s-when : srcloc (Listof Expr s-block) -> s-cond
(struct s-when (syntax test block) #:transparent)
;; s-cond : srcloc (Listof s-cond-branch) -> s-cond
(struct s-cond (syntax branches) #:transparent)
;; s-cond-branch : srcloc Expr s-block -> s-cond-branch
(struct s-cond-branch (syntax expr body) #:transparent)
;; s-try : srcloc Expr s-bind Expr -> s-try
(struct s-try (syntax body id except) #:transparent)


;; An Expr is a
;; (U s-obj s-onion s-list s-app s-left-app s-id
;;    s-assign s-num s-bool s-str
;;    s-dot s-bracket
;;    s-dot-method s-bracket-method s-lam
;;    s-block s-method))

;; s-lam : srcloc (Listof Symbol) (Listof s-bind) Ann String s-block -> s-lam
(struct s-lam (syntax typarams args ann doc body) #:transparent)

;; s-method : srcloc (Listof s-bind) Ann s-block
(struct s-method (syntax args ann body) #:transparent)

;; A Member is a (U s-data-field s-method-field)
;; s-data-field : srcloc Expr Expr
(struct s-data-field (syntax name value) #:transparent)
;; s-method-field : srcloc Expr (Listof s-bind) Ann s-block
(struct s-method-field (syntax name args ann body) #:transparent)

;; s-onion : srcloc Expr (Listof Member)
(struct s-onion (syntax super fields) #:transparent)
;; s-obj : srcloc (Listof Member)
(struct s-obj (syntax fields) #:transparent)

;; s-list : srcloc (Listof Expr)
(struct s-list (syntax values) #:transparent)

;; s-app : srcloc Expr (Listof Expr)
(struct s-app (syntax fun args) #:transparent)

;; s-left-app : srcloc Expr Expr (Listof Expr)
(struct s-left-app (syntax obj fun args) #:transparent)

;; s-id : srcloc Symbol
(struct s-id (syntax id) #:transparent)

;; s-assign : srcloc Symbol Expr
(struct s-assign (syntax id value) #:transparent)

;; s-num : srcloc Number
(struct s-num (syntax n) #:transparent)
;; s-bool : srcloc Boolean
(struct s-bool (syntax b) #:transparent)
;; s-str : srcloc String
(struct s-str (syntax s) #:transparent)

;; s-dot : srcloc Expr Symbol
(struct s-dot (syntax obj field) #:transparent)
;; s-bracket : srcloc Expr Expr
(struct s-bracket (syntax obj field) #:transparent)

;; s-dot-assign : srcloc Expr Symbol Expr
(struct s-dot-assign (syntax obj field value) #:transparent)
;; s-bracket-assign : srcloc Expr Expr Expr
(struct s-bracket-assign (syntax obj field value) #:transparent)

;; s-dot-method : srcloc Expr Symbol
(struct s-dot-method (syntax obj field) #:transparent)
;; s-bracket-method : srcloc Expr Expr
(struct s-bracket-method (syntax obj field) #:transparent)

;; s-data : srcloc Symbol (Listof Symbol) (Listof s-variant) (Listof Member)
(struct s-data (syntax name params variants shared-members) #:transparent)

;; s-variant : srcloc Symbol (Listof s-member) (Listof Member)
(struct s-variant (syntax name members with-members) #:transparent)
;; s-variant : srcloc Symbol (Listof Member)
(struct s-singleton-variant (syntax name with-members) #:transparent)

;; s-member : srcloc Symbol Ann
(struct s-member (syntax name ann) #:transparent)

;; s-do : srcloc Stmt (Listof Stmt)
(struct s-do (syntax init args) #:transparent)

;; An Ann is a (U a-blank a-any a-name a-arrow a-method a-record a-app a-pred))
(struct a-ann () #:transparent)
(struct a-blank a-ann () #:transparent)
(struct a-any a-ann () #:transparent)
;; a-name : srcloc Symbol
(struct a-name a-ann (syntax id) #:transparent)
;; a-arrow : srcloc (Listof a-ann) a-ann
(struct a-arrow a-ann (syntax args ret) #:transparent)
;; a-method : srcloc (Listof a-ann) a-ann
(struct a-method a-ann (syntax args ret) #:transparent)
;; a-field : srcloc String a-ann
(struct a-field a-ann (syntax name ann) #:transparent)
;; a-record : srcloc (Listof a-field)
(struct a-record a-ann (syntax fields) #:transparent)
;; a-app : srcloc Symbol (Listof a-ann)
(struct a-app a-ann (syntax name parameters) #:transparent)
;; a-pred : srcloc a-ann Expr
(struct a-pred a-ann (syntax ann exp) #:transparent)

