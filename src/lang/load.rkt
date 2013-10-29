#lang racket

(provide parse-pyret (rename-out [py-eval parse-eval] [parse-stmt-str parse-stmt]))

(require
  "get-syntax.rkt"
  "parser.rkt"
  racket/runtime-path)

(define (py-eval stx)
  (parse-program stx))

(define (parse-pyret str (name "unnamed-pyret-file"))
  (parse-program (get-syntax name (open-input-string str))))

(define (parse-stmt-str str (name "unnamed-pyret-file"))
  (parse-stmt (get-stmt-syntax name (open-input-string str))))
