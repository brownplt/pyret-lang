#lang racket/base

(require
  racket/match
  racket/list
  "../ast.rkt")

(provide lift-constants)

(define (lift-constants ast)
  (define (num-name n) (string->symbol (string-append "%num" (number->string n))))
  (define (str-name s) (string->symbol (string-append "%str" s)))
  (define (num-id s n) (s-id s (num-name n)))
  (define (str-id s str) (s-id s (str-name str)))
  (define (get-and-replace-constants ast)
    (define (process-member m)
     (match m
       [(s-data-field l (s-str l2 s) val)
        (match-define (cons val-list new-val) (get-and-replace-constants val))
        (cons val-list
              (s-data-field l (s-str l2 s) new-val))]
       [(s-data-field l name val)
        (match-define (cons name-list new-name) (get-and-replace-constants name))
        (match-define (cons val-list new-val) (get-and-replace-constants val))
        (cons (append name-list val-list)
              (s-data-field l new-name new-val))]))
    (match ast
      [(s-num s n) (cons (list n) (num-id s n))]
      [(s-str s str) (cons (list str) (str-id s str))]
      [(s-bool s b) (cons empty ast)]
      [(s-id s x) (cons empty ast)]

      [(s-block l stmts)
       (define stmts-results (map get-and-replace-constants stmts))
       (cons (flatten (map car stmts-results))
             (s-block l (map cdr stmts-results)))]
      
      [(s-hint-exp s h e)
       (match-define (cons val-list new-val) (get-and-replace-constants e))
       (cons val-list (s-hint-exp s h new-val))]
      
      [(s-lam l params args ann doc body check)
       (match-define (cons lst new-body) (get-and-replace-constants body))
       (cons lst (s-lam l params args ann doc new-body check))]
      
      [(s-method l args ann doc body check)
       (match-define (cons lst new-body) (get-and-replace-constants body))
       (cons lst (s-method l args ann doc new-body check))]

      [(s-if-else l c-bs else)
       (define (process-if-branch b)
         (match b
           [(s-if-branch s test block)
            (match-define
              (cons test-list new-test) (get-and-replace-constants test))
            (match-define
              (cons block-list new-block) (get-and-replace-constants block))
            (cons (append test-list block-list)
                  (s-if-branch s new-test new-block))]))
       (define branch-results (map process-if-branch c-bs))
       (match-define (cons else-list new-else) (get-and-replace-constants else))
       (cons (flatten (cons else-list (map car branch-results)))
             (s-if-else l (map cdr branch-results) new-else))]
      
      [(s-try l try (s-bind l2 id ann) catch)
       (match-define (cons try-list new-try) (get-and-replace-constants try))
       (match-define (cons catch-list new-catch) (get-and-replace-constants catch))
       (cons (append try-list catch-list)
             (s-try l new-try (s-bind l2 id ann) new-catch))]

      [(s-assign l name expr)
       (match-define (cons lst new-expr) (get-and-replace-constants expr))
       (cons lst (s-assign l name new-expr))]

      [(s-app l f args)
       (match-define (cons f-list new-f) (get-and-replace-constants f))
       (define args-results (map get-and-replace-constants args))
       (cons (flatten (cons f-list (map car args-results)))
             (s-app l new-f (map cdr args-results)))]

      [(s-obj l fields)
       (define fields-results (map process-member fields))
       (cons (flatten (map car fields-results))
             (s-obj l (map cdr fields-results)))]
      
      [(s-extend l super fields)
       (match-define
        (cons super-list new-super) (get-and-replace-constants super))
       (define fields-results (map process-member fields))
       (cons (flatten (cons super-list (map car fields-results)))
             (s-extend l new-super (map cdr fields-results)))]

      [(s-update l super fields)
       (match-define
        (cons super-list new-super) (get-and-replace-constants super))
       (define fields-results (map process-member fields))
       (cons (flatten (cons super-list (map car fields-results)))
             (s-update l new-super (map cdr fields-results)))]
      
      [(s-bracket l obj field)
       (match-define (cons obj-list new-obj) (get-and-replace-constants obj))
       (match-define (cons field-list new-field) (get-and-replace-constants field))
       (cons (append obj-list field-list)
             (s-bracket l obj field))]

      [(s-get-bang l obj field)
       (match-define (cons obj-list new-obj) (get-and-replace-constants obj))
       (cons obj-list (s-get-bang l obj field))]
      
      [(s-colon-bracket l obj field)
       (match-define (cons obj-list new-obj) (get-and-replace-constants obj))
       (match-define (cons field-list new-field) (get-and-replace-constants field))
       (cons (append obj-list field-list)
             (s-colon-bracket l obj field))]

      [(s-user-block l body)
       (match-define (cons body-list new-body) (get-and-replace-constants body))
       (cons body-list (s-user-block l new-body))]

      [(s-let l bind expr)
       (match-define (cons expr-list new-expr) (get-and-replace-constants expr))
       (cons expr-list (s-let l bind new-expr))]

      [(s-var l bind expr)
       (match-define (cons expr-list new-expr) (get-and-replace-constants expr))
       (cons expr-list (s-var l bind new-expr))]

      [else (error (format "Unknown ast node type to get-and-replace-constants: ~a" ast))]))

  (define (wrap l vals-and-ast)
    (define (binding-for v)
      (cond
        [(number? v) (s-let l (s-bind l (num-name v) (a-blank)) (s-num l v))]
        [(string? v) (s-let l (s-bind l (str-name v) (a-blank)) (s-str l v))]))
    (define bindings (map binding-for (remove-duplicates (car vals-and-ast))))
    (match (cdr vals-and-ast)
      [(s-block l stmts) (s-block l (append bindings stmts))]
      [ast (s-block l (append bindings (list ast)))]))
  (match ast
    [(s-prog l imports block)
     (s-prog l imports (wrap l (get-and-replace-constants block)))]
    [_ (wrap (srcloc 'pyret-compile #f #f #f #f) (get-and-replace-constants ast))]))

