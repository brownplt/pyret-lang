#lang racket/base

(provide get-syntax-errors get-stmt-syntax-errors)
(require "untokenizer.rkt" "ungrammar.rkt")

(define (get-syntax-errors name input-port)
  (parse name (tokenize input-port)))

(define (get-stmt-syntax-errors name input-port)
  (define parse-stmt (make-rule-parser stmt))
  (parse-stmt (tokenize input-port)))

(define (get-string-syntax str)
  (get-syntax-errors str (open-input-string str)))

