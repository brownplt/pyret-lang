#lang racket/base

(require
  racket/match
  racket/list
  racket/function
  "../ast.rkt")
(provide find)

;; Expr * (Expr -> α U #f) -> List<α>
;; finds all Exprs that matches matcher, and returns the matched values
;; TODO(joe): descend into annotations --- is it necessary post-desugar?
(define (find expr matcher)
  (define (find-member m)
   (match m
     [(s-data-field l name val) (append (find name matcher) (find val matcher))]))
  (define result (matcher expr))
  (define other-matches
   (match expr
     [(s-num s n) '()]
     [(s-str s str) '()]
     [(s-bool s b) '()]
     [(s-id s x) '()]

     [(s-block l stmts)
      (append* (map (curryr find matcher) stmts))]

     [(s-hint-exp s h e) (find e matcher)]
     
     [(s-lam l params args ann doc body check)
      (append (find body matcher) (find check matcher))]
     
     [(s-method l args ann doc body check)
      (append (find body matcher) (find check matcher))]

     [(s-if-else l c-bs else)
      (define (find-if-branch b)
        (match b
          [(s-if-branch s test block)
           (append (find test matcher) (find block matcher))]))
      (define branch-results (map find-if-branch c-bs))
      (append (append* branch-results) (find else matcher))]
     
     [(s-try l try b catch)
      (append (find try matcher) (find catch matcher))]

     [(s-assign l name expr)
      (find expr matcher)]

     [(s-app l f args)
      (append (find f matcher) (append* (map (curryr find matcher) args)))]

     [(s-obj l fields)
      (append* (map find-member fields))]
     
     [(s-extend l super fields)
      (append (find super matcher) (append* (map find-member fields)))]

     [(s-update l super fields)
      (append (find super matcher) (append* (map find-member fields)))]
     
     [(s-bracket l obj field)
      (append (find obj matcher) (find field matcher))]

     [(s-get-bang l obj field)
      (find obj matcher)]
     
     [(s-colon-bracket l obj field)
      (append (find obj matcher) (find field matcher))]

     [(s-let l bind expr)
      (find expr matcher)]

     [(s-user-block l body)
      (find body matcher)]

     [(s-var l bind expr)
      (find expr matcher)]
     
     [_ (error (format "pyret-internal: Missed a case in find: ~a" expr))]))
   (cond
    [result (cons result other-matches)]
    [else other-matches]))

