#lang racket/base
(require ragg/examples/0n1n
         ragg/support
         rackunit)

(define (lex ip)
  (port-count-lines! ip)
  (lambda ()
    (define next-char (read-char ip))
    (cond [(eof-object? next-char)
           (token eof)]
          [(char=? next-char #\0)
           (token "0" "0")]
          [(char=? next-char #\1)
           (token "1" "1")])))


;; The only rule in the grammar is:
;;
;;      rule-0n1n: ["0" rule-0n1n "1"]
;;
;; It makes use of the "maybe" pattern.  The result type of the
;; grammar rule is:
;;
;; rule-0n1n: (U #f
;;               (list "0" rule-0n1n "1"))

(check-equal? (syntax->datum (parse #f (lex (open-input-string "0011"))))
              '(rule-0n1n "0" (rule-0n1n "0" (rule-0n1n) "1") "1"))

(check-equal? (syntax->datum (parse #f (lex (open-input-string "01"))))
              '(rule-0n1n "0" (rule-0n1n) "1"))

(check-equal? (syntax->datum (parse #f (lex (open-input-string ""))))
              '(rule-0n1n))

(check-equal? (syntax->datum (parse #f (lex (open-input-string "000111"))))
              '(rule-0n1n "0" (rule-0n1n "0" (rule-0n1n "0" (rule-0n1n) "1") "1") "1"))



(check-exn exn:fail:parsing?
           (lambda () (parse #f (lex (open-input-string "0001111")))))

(check-exn exn:fail:parsing?
           (lambda () (parse #f (lex (open-input-string "0001110")))))

(check-exn exn:fail:parsing?
           (lambda () (parse #f (lex (open-input-string "10001110")))))
