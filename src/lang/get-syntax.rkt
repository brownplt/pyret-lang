#lang racket/base

;; NOTE(joe): This has been ripped from Danny's test cases for
;; autogrammar
(provide
 get-syntax
 get-stmt-syntax)
(require
  racket/set
  racket/match
  racket/generator
  parser-tools/lex
  ragg/support
  "tokenizer.rkt"
  "grammar.rkt")

(define (get-syntax name input-port)
  (parse name (tokenize input-port name)))

(define (get-stmt-syntax name input-port)
  (define parse-stmt (make-rule-parser stmt))
  (parse-stmt (tokenize input-port name)))

(define (get-string-syntax str)
  (get-syntax str (open-input-string str)))

