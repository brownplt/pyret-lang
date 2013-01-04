#lang racket/base

(require ragg/examples/0n1
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


(check-equal? (syntax->datum (parse #f (lex (open-input-string "1"))))
              '(rule "1"))


(check-equal? (syntax->datum (parse #f (lex (open-input-string "01"))))
              '(rule "0" "1"))


(check-equal? (syntax->datum (parse #f (lex (open-input-string "001"))))
              '(rule "0" "0" "1"))


(check-exn exn:fail:parsing?
           (lambda ()
             (parse #f (lex (open-input-string "0")))))

(check-exn exn:fail:parsing?
           (lambda ()
             (parse #f (lex (open-input-string "10")))))

(check-exn exn:fail:parsing?
           (lambda ()
             (parse #f (lex (open-input-string "010")))))

