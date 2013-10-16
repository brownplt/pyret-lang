#lang racket/base

(provide (all-defined-out))
(require racket/match racket/path racket/bool racket/list racket/set)

#|

The concrete AST for surface Pyret.

Each syntactic form has a srcloc object associated with it, for error
reporting and keeping track of source locations.  The srcloc object
should not be required for evaluating the ast node, and only used for
these metadata purposes.

|#
(define (ok-last-stmt stmt)
  (match stmt
    [(s-let _ _ _) #f]
    [(s-var _ _ _) #f]
    [(s-fun _ _ _ _ _ _ _ _) #f]
    [(s-data _ _ _ _ _ _ _) #f]
    [(s-graph _ _) #f]
    [(s-check _ _) #f]
    [else #t]))

(define (src->module-name e)
  (cond
    [(symbol? e) e]
    [(string? e) (string->symbol e)]
    [(path? e) (string->symbol (path->string e))]
    [(false? e) 'unknown-pyret-source]
    [else (error (format "Non-symbol, non-string, non-path value for
                          source: ~a" e))]))

;; s-prog : srcloc (Listof Header) s-block -> s-prog
(struct s-prog (syntax imports block) #:transparent)

;; A Header is a (U s-import s-provide)
;; s-import : srcloc (U String Symbol) Symbol -> srcloc
(struct s-import (syntax file name) #:transparent)
;; s-provide : srcloc expr -> srcloc
(struct s-provide (syntax expr) #:transparent)
;; s-provide-all : srcloc -> srcloc
(struct s-provide-all (syntax) #:transparent)


;; A Block is a (Listof Stmt)
;; s-block : srcloc Block -> s-block
(struct s-block (syntax stmts) #:transparent)

(define (flatten-blocks maybe-blocks)
  (cond
    [(or (cons? maybe-blocks) (empty? maybe-blocks))
     (foldr (λ (stmt block-stmts)
              (match stmt
                [(s-block s stmts) (append (flatten-blocks stmts) block-stmts)]
                [else (cons stmt block-stmts)]))
            empty
            maybe-blocks)]
    [else (list maybe-blocks)]))


;; s-bind : srcloc Symbol Ann -> s-bind
(struct s-bind (syntax id ann) #:transparent)

;; A Stmt is a (U s-fun s-var s-if s-try s-data s-import Expr)

;; s-fun : srcloc Symbol (Listof Symbol) (Listof s-bind) Ann String s-block s-block
(struct s-fun (syntax name params args ann doc body check) #:transparent)

;; s-check: srcloc s-block
(struct s-check (syntax body) #:transparent)

;; s-var : srcloc bind Expr -> s-var
(struct s-var (syntax name value) #:transparent)
;; s-let : srcloc bind Expr -> s-let
(struct s-let (syntax name value) #:transparent)

(struct s-graph (syntax bindings) #:transparent)

(struct s-user-block (syntax body) #:transparent)


;; s-when : srcloc (Listof Expr s-block) -> s-when
(struct s-when (syntax test block) #:transparent)
;; s-if : srcloc (Listof s-if-branch) -> s-if
(struct s-if (syntax branches) #:transparent)
;; s-if-else : srcloc (Listof s-if-branch) s-block -> s-if-else
(struct s-if-else (syntax branches else) #:transparent)
;; s-if-branch : srcloc Expr s-block -> s-if-branch
(struct s-if-branch (syntax expr body) #:transparent)
;; s-try : srcloc Expr s-bind Expr -> s-try
(struct s-try (syntax body id except) #:transparent)

;; s-cases : srcloc Expr Expr (Listof s-cases-branch) -> s-cases
(struct s-cases (syntax type val branches) #:transparent)
;; s-cases-else : srcloc (Listof s-cases-branch) s-block -> s-cases-else
(struct s-cases-else (syntax type val branches else) #:transparent)
;; s-cases-branch : srcloc symbol (ListOf s-bind) s-block -> s-cases-branch
(struct s-cases-branch (syntax name args body) #:transparent)

(define op+ 'op+)
(define op- 'op-)
(define op* 'op*)
(define op/ 'op/)
(define op<= 'op<=)
(define op< 'op<)
(define op>= 'op>=)
(define op> 'op>)
(define op== 'op==)
(define op<> 'op<>)
(define opand 'opand)
(define opor 'opor)
(define opis 'opis)
(define opraises 'opraises)

(define op-lookup-table
  (make-immutable-hash
   `(("+" . ,op+)
     ("-" . ,op-)
     ("*" . ,op*)
     ("/" . ,op/)
     ("<=" . ,op<=)
     ("<" . ,op<)
     (">=" . ,op>=)
     (">" . ,op>)
     ("==" . ,op==)
     ("<>" . ,op<>)
     ("and" . ,opand)
     ("or" . ,opor)
     ("is" . ,opis)
     ("raises" . ,opraises))))

(define reverse-op-lookup-table
  (make-immutable-hash
   (map (λ (k) (cons (hash-ref op-lookup-table k) k))
        (hash-keys op-lookup-table))))

;; s-op: srcloc op Expr Expr -> s-op
(struct s-op (syntax op left right) #:transparent)

;; NOTE(dbp): our only unary op, so not generalized
;; s-not: srcloc Expr
(struct s-not (syntax expr) #:transparent)

;; s-paren: srcloc Expr -> s-paren
(struct s-paren (syntax expr) #:transparent)

;; An Expr is a
;; (U s-obj s-extend s-list s-app s-left-app s-id
;;    s-assign s-num s-bool s-str
;;    s-dot s-bracket
;;    s-colon s-colon-bracket s-lam
;;    s-block s-method))

;; s-lam : srcloc (Listof Symbol) (Listof s-bind) Ann String s-block s-block -> s-lam
(struct s-lam (syntax typarams args ann doc body check) #:transparent)

;; s-method : srcloc (Listof s-bind) Ann String s-block s-block
(struct s-method (syntax args ann doc body check) #:transparent)

;; A Member is a (U s-data-field s-method-field)
;; s-data-field : srcloc Expr Expr
(struct s-data-field (syntax name value) #:transparent)
;; s-mutable-field : srcloc Expr Ann Expr
(struct s-mutable-field (syntax name ann value) #:transparent)
;; s-once-field : srcloc Expr Ann Expr
(struct s-once-field (syntax name ann value) #:transparent)
;; s-method-field : srcloc Expr (Listof s-bind) Ann String s-block s-block
(struct s-method-field (syntax name args ann doc body check) #:transparent)

;; s-extend : srcloc Expr (Listof Member)
(struct s-extend (syntax super fields) #:transparent)
;; s-update : srcloc Expr (Listof Member)
(struct s-update (syntax super fields) #:transparent)
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

;; s-dot : srcloc Expr Symbol
(struct s-get-bang (syntax obj field) #:transparent)

;; s-colon : srcloc Expr Symbol
(struct s-colon (syntax obj field) #:transparent)
;; s-colon-bracket : srcloc Expr Expr
(struct s-colon-bracket (syntax obj field) #:transparent)

;; s-data : srcloc Symbol (Listof Symbol) (Listof Expr) (Listof s-variant) (Listof Member) block
(struct s-data (syntax name params mixins variants shared-members check) #:transparent)

;; member-type is 'normal, 'mutable, or 'cyclic

(struct s-variant-member (syntax member-type bind))

;; s-variant : srcloc Symbol (Listof s-variant-bind) (Listof Member)
(struct s-variant (syntax name binds with-members) #:transparent)
;; s-variant : srcloc Symbol (Listof Member)
(struct s-singleton-variant (syntax name with-members) #:transparent)

;; s-for-bind : srcloc s-bind Expr
(struct s-for-bind (syntax bind value) #:transparent)
;; s-for : srcloc Expr (Listof s-for-bind) ann s-block
(struct s-for (syntax iterator bindings ann body) #:transparent)

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
;; a-app : srcloc (Symbol or a-dot) (Listof a-ann)
(struct a-app a-ann (syntax ann parameters) #:transparent)
;; a-pred : srcloc a-ann Expr
(struct a-pred a-ann (syntax ann exp) #:transparent)
;; a-dot : srcloc Symbol Symbol
(struct a-dot a-ann (syntax obj field) #:transparent)


(define (subst expr1 sub-id expr2)
  (define (sub e)
    (subst e sub-id expr2))
  (match expr1
    [(s-id s id)
     (cond
      [(equal? sub-id id) expr2]
      [else expr1])]
    [(s-prog s imports block)
     (s-prog s (map sub imports) (sub block))]
    [(s-import s file name) expr1]
    [(s-provide s expr) (s-provide s (sub expr))]
    [(s-provide-all s) expr1]
    [(s-block s stmts) (s-block s (map sub stmts))]
    [(s-bind s id ann) expr1]
    [(s-fun s name params args ann doc body check)
     (define shadow? (member sub-id (map s-bind-id args)))
     (cond
      [shadow? (s-fun s name params args ann doc body (sub check))]
      [else (s-fun s name params args ann doc (sub body) (sub check))])]
    [(s-lam s typarams args ann doc body check)
     (define shadow? (member sub-id (map s-bind-id args)))
     (cond
      [shadow? (s-lam s typarams args ann doc body (sub check))]
      [else (s-lam s typarams args ann doc (sub body) (sub check))])]
    [(s-method s args ann doc body check)
     (define shadow? (member sub-id (map s-bind-id args)))
     (cond
      [shadow? (s-method s args ann doc body (sub check))]
      [else (s-method s args ann doc (sub body) (sub check))])]
    [(s-method-field s name args ann doc body check)
     (define shadow? (member sub-id (map s-bind-id args)))
     (cond
      [shadow? (s-method-field s name args ann doc body (sub check))]
      [else (s-method s name args ann doc (sub body) (sub check))])]
    [(s-for s iterator bindings ann body)
     (define shadow? (member sub-id (map s-bind-id (map s-for-bind-bind bindings))))
     (cond
      [shadow? (s-for s (sub iterator) (map sub bindings) ann body)]
      [else (s-for s (sub iterator) (map sub bindings) ann (sub body))])]
    [(s-graph s bindings)
     (s-graph (map sub bindings))]
    [(s-user-block s body) (s-user-block s (sub body))]
    [(s-check s body) (s-check s (sub body))]
    [(s-var s name value) (s-var s name (sub value))]
    [(s-let s name value) (s-let s name (sub value))]
    [(s-when s test block) (s-when s (sub test) (sub block))]
    [(s-if s branches) (s-if s (map sub branches))]
    [(s-if-else s branches else) (s-if-else s (map sub branches) (sub else))]
    [(s-if-branch s expr body) (s-if-branch s (sub expr) (sub body))]
    [(s-try s body id except)
     (define shadow? (equal? sub-id id))
     (cond
      [shadow? (s-try s (sub body) id except)]
      [else (s-try s (sub body) id (sub except))])]
    [(s-cases s type val branches) (s-cases s type (sub val) (map sub branches))]
    [(s-cases-else s type val branches else) (s-cases-else s type (sub val) (map sub branches) (sub else))]
    [(s-cases-branch s name args body) (s-cases-branch s name args (sub body))]
    [(s-op s op left right) (s-op s op (sub left) (sub right))]
    [(s-not s expr) (s-not s (sub expr))]
    [(s-paren s expr) (s-paren s (sub expr))]
    [(s-data-field s name value) (s-data-field s name (sub value))]
    [(s-mutable-field s name ann value) (s-mutable-field s name ann (sub value))]
    [(s-once-field s name ann value) (s-once-field s name ann (sub value))]
    [(s-extend s super fields) (s-extend s (sub super) (map sub fields))]
    [(s-update s super fields) (s-update s (sub super) (map sub fields))]
    [(s-obj s fields) (s-obj s (map sub fields))]
    [(s-list s values) (s-list s (map sub values))]
    [(s-app s fun args) (s-app s (sub fun) (map sub args))]
    [(s-left-app s obj fun args) (s-left-app s (sub obj) (sub fun) (map sub args))]
    [(s-assign s id value) (error "Can't substitute into a mutable variable")]
    [(s-num s n) expr1]
    [(s-bool s b) expr1]
    [(s-str s str) expr1]
    [(s-dot s obj field) (s-dot s (sub obj) field)]
    [(s-get-bang s obj field) (s-get-bang s (sub obj) field)]
    [(s-bracket s obj field) (s-bracket s (sub obj) (sub field))]
    [(s-colon s obj field) (s-colon s (sub obj) field)]
    [(s-colon-bracket s obj field) (s-colon-bracket s (sub obj) (sub field))]
    [(s-data s name params mixins variants shared-members check)
     (s-data s name params (map sub mixins) (map sub variants) (map sub shared-members) (sub check))]
    [(s-variant s name binds with-members)
     (s-variant s name binds (map sub with-members))]
    [(s-singleton-variant s name with-members) expr1]
    [(s-for-bind s bind value) (s-for-bind s bind (sub value))]
    [_ (error (format "Cannot substitute into: ~a\n" expr1))]))

(define (get-srcloc ast)
  (match ast
    [(s-prog syntax imports block) syntax]
    [(s-import syntax file name) syntax]
    [(s-provide syntax expr) syntax]
    [(s-provide-all syntax) syntax]
    [(s-block syntax stmts) syntax]
    [(s-bind syntax id ann) syntax]
    [(s-fun syntax name params args ann doc body check) syntax]
    [(s-check syntax body) syntax]
    [(s-var syntax name value) syntax]
    [(s-let syntax name value) syntax]
    [(s-graph syntax bindings) syntax]
    [(s-user-block syntax _) syntax]
    [(s-when syntax test block) syntax]
    [(s-if syntax branches) syntax]
    [(s-if-else syntax branches else) syntax]
    [(s-if-branch syntax expr body) syntax]
    [(s-try syntax body id except) syntax]
    [(s-cases syntax type val branches) syntax]
    [(s-cases-else syntax type val branches else) syntax]
    [(s-cases-branch syntax name args body) syntax]
    [(s-op syntax op left right) syntax]
    [(s-not syntax expr) syntax]
    [(s-paren syntax expr) syntax]
    [(s-lam syntax typarams args ann doc body check) syntax]
    [(s-method syntax args ann doc body check) syntax]
    [(s-data-field syntax name value) syntax]
    [(s-method-field syntax name args ann doc body check) syntax]
    [(s-extend syntax super fields) syntax]
    [(s-update syntax super fields) syntax]
    [(s-obj syntax fields) syntax]
    [(s-list syntax values) syntax]
    [(s-app syntax fun args) syntax]
    [(s-left-app syntax obj fun args) syntax]
    [(s-id syntax id) syntax]
    [(s-assign syntax id value) syntax]
    [(s-num syntax n) syntax]
    [(s-bool syntax b) syntax]
    [(s-str syntax s) syntax]
    [(s-dot syntax obj field) syntax]
    [(s-get-bang syntax obj field) syntax]
    [(s-bracket syntax obj field) syntax]
    [(s-colon syntax obj field) syntax]
    [(s-colon-bracket syntax obj field) syntax]
    [(s-data syntax name params mixins variants shared-members check) syntax]
    [(s-variant syntax name binds with-members) syntax]
    [(s-singleton-variant syntax name with-members) syntax]
    [(s-for-bind syntax bind value) syntax]
    [(s-for syntax iterator bindings ann body) syntax]
    [(a-ann) (list "pyret-internal" #f #f #f #f)]
    [(a-blank) (list "pyret-internal" #f #f #f #f)]
    [(a-any) (list "pyret-internal" #f #f #f #f)]
    [(a-name syntax id) syntax]
    [(a-arrow syntax args ret) syntax]
    [(a-method syntax args ret) syntax]
    [(a-field syntax name ann) syntax]
    [(a-record syntax fields) syntax]
    [(a-app syntax ann parameters) syntax]
    [(a-pred syntax ann exp) syntax]
    [(a-dot syntax obj field) syntax]))

;; variant checker name
(define (make-checker-name s)
    (string->symbol (string-append "is-" (symbol->string s))))

(define (top-level-ids block)
  (define (_top-level-ids expr)
    (define (variant-ids variant)
      (match variant
        [(s-variant _ name _ _)
         (list name (make-checker-name name))]
        [(s-singleton-variant _ name _)
         (list name (make-checker-name name))]))
    (match expr
      [(s-let _ (s-bind _ x _) _) (list x)]
      [(s-fun _ name _ _ _ _ _ _) (list name)]
      [(s-data s name _ _ variants _ _)
       (cons name (flatten (map variant-ids variants)))]
      [else (list)]))
  (flatten (map _top-level-ids (s-block-stmts block))))

(define (free-ids expr)
  (define (unions l)
    (apply set-union (cons (set) l)))
  (define (free-ids-member m)
    (match m
      [(s-data-field _ name value) (free-ids value)]
      [(s-method-field _ name args ann doc body check)
       (free-ids-fun args ann body check)]))
  (define (free-ids-variant-member m)
    (match m
      [(s-variant-member _ type bind) (free-ids-bind bind)]))
  (define (free-ids-bind b)
    (match b
      [(s-bind _ id ann) (free-ids-ann ann)]))
  (define (free-ids-for-binding b)
    (match b
      [(s-for-bind _ bind value)
       (set-union (free-ids-bind bind) (free-ids value))]))
  (define (free-ids-if-branch b)
    (match b
      [(s-if-branch _ expr body) (set-union (free-ids expr) (free-ids body))]))
  (define (free-ids-cases-branch cb)
    (match cb
      [(s-cases-branch _ name args body)
       (define bound-args (list->set (map s-bind-id args)))
       (define body-free (set-subtract (free-ids body) bound-args))
       (set-union (unions (map free-ids-bind args)) body-free)]))
  (define (free-ids-variant v)
    (match v
      [(s-variant _ name binds with-members)
       (set-union
        (unions (map free-ids-variant-member binds))
        (unions (map free-ids-member with-members)))]
      [(s-singleton-variant _ name with-members)
       (unions (map free-ids-member with-members))]))
  (define (free-ids-ann a)
    (match a
      [(a-any) (set)]
      [(a-blank) (set)]
      [(a-name _ id) (set id)]
      [(a-pred _ a pred) (set-union (free-ids-ann a) (free-ids pred))]
      [(a-arrow _ args ret)
       (set-union (unions (map free-ids-ann args)) ret)]
      [(a-method _ args ret)
       (set-union (unions (map free-ids-ann args)) ret)]
      [(a-field _ name ann) (free-ids-ann ann)]
      [(a-record _ fields) (unions (map free-ids-ann a-field))]
      [(a-app _ ann parameters)
       (set-union (free-ids-ann ann) (unions (map free-ids-ann parameters)))]
      [(a-dot _ obj field) (set obj)]
      [_ (error (format "NYI ann: ~a" a))]))
  (define (free-ids-fun args ann body check (name (set)))
     (define args-bound (list->set (map s-bind-id args)))
     (define arg-ids (unions (map free-ids-bind args)))
     (define ann-ids (free-ids-ann ann))
     (define body-ids-init (free-ids body))
     (define body-ids (set-subtract body-ids-init (set-union args-bound name)))
     (define check-ids (free-ids check))
     (set-union arg-ids ann-ids body-ids check-ids))
    
  (match expr
    [(s-prog _ imports block)
     (unions (cons (free-ids block) (map free-ids imports)))]
    [(s-import _ file name) (set)]
    [(s-provide _ expr) (free-ids expr)]
    [(s-provide-all _) (set)]
    [(s-block _ stmts)
     (define bound-ids (list->set (top-level-ids expr)))
     (set-subtract (unions (map free-ids stmts)) bound-ids)]
    [(s-bind _ id ann) (free-ids-ann ann)]
    [(s-fun _ name params args ann doc body check)
     (free-ids-fun args ann body check (set name))]
    [(s-lam _ typarams args ann doc body check)
     (free-ids-fun args ann body check)]
    [(s-method _ args ann doc body check)
     (free-ids-fun args ann body check)]
    [(s-check _ body) (free-ids body)]
    [(s-var _ name value) (set-union (free-ids-bind name) (free-ids value))]
    [(s-let _ name value) (set-union (free-ids-bind name) (free-ids value))]
    [(s-id _ id) (set id)]
    [(s-graph s bindings)
     (define bound-ids (list->set (top-level-ids (s-block s bindings))))
     (set-subtract (unions (map free-ids bindings)) bound-ids)]
    [(s-list _ values) (unions (map free-ids values))]
    [(s-op _ _ left right) (set-union (free-ids left) (free-ids right))]
    [(s-user-block _ block) (free-ids block)]
    [(s-when _ test block) (set-union (free-ids test) (free-ids block))]
    [(s-if _ branches) (unions (map free-ids-if-branch branches))]
    [(s-if-else _ branches else)
     (set-union (unions (map free-ids-if-branch branches)) (free-ids else))]
    [(s-try _ body id except)
     (set-union (free-ids body) (set-subtract (free-ids except) (set id)))]
    [(s-cases _ type val branches)
     (set-union
      (free-ids-ann type)
      (free-ids val)
      (unions (map free-ids-cases-branch branches)))]
    [(s-cases-else _ type val branches else)
     (set-union
      (free-ids-ann type)
      (free-ids val)
      (unions (map free-ids-cases-branch branches))
      (free-ids else))]
    [(s-not _ expr) (free-ids expr)]
    [(s-paren _ expr) (free-ids expr)]
    [(s-extend _ super fields)
     (set-union (free-ids super) (unions (map free-ids-member fields)))]
    [(s-update _ super fields)
     (set-union (free-ids super) (unions (map free-ids-member fields)))]
    [(s-obj _ fields)
     (unions (map free-ids-member fields))]
    [(s-app _ fun args)
     (set-union (free-ids fun) (unions (map free-ids args)))]
    [(s-left-app _ obj fun args)
     (set-union (free-ids obj) (free-ids fun) (unions (map free-ids args)))]
    [(s-assign _ id value) (free-ids value)]
    [(s-dot _ obj field) (free-ids obj)]
    [(s-get-bang _ obj field) (free-ids obj)]
    [(s-bracket _ obj field) (set-union (free-ids obj) (free-ids field))]
    [(s-colon _ obj field) (free-ids obj)]
    [(s-colon-bracket _ obj field) (set-union (free-ids obj) (free-ids field))]
    [(s-for _ iterator bindings ann body)
     (define iterator-ids (free-ids iterator))
     (define bindings-ids (unions (map free-ids-for-binding bindings)))
     (define bindings (list->set (map s-bind-id (map s-for-bind-bind bindings))))
     (define ann-ids (free-ids-ann ann))
     (define body-ids (set-subtract (free-ids body) bindings))
     (set-union iterator-ids bindings-ids ann-ids body-ids)]
     
    [(s-data _ name params mixins variants shared-members check)
     (set-union
       (unions (map free-ids mixins))
       (unions (map free-ids-variant variants))
       (unions (map free-ids-member shared-members))
       (free-ids check))]

    [(s-num _ _) (set)]
    [(s-bool _ b) (set)]
    [(s-str _ s) (set)]
    [_ (error (format "NYI: ~a" expr))]))
