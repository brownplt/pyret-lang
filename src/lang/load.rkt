#lang racket

(provide parse-pyret/port parse-pyret (rename-out [py-eval parse-eval] [parse-stmt-str parse-stmt]))

(require
  (rename-in (only-in "unparser.rkt" parse-program parse-stmt)
    [parse-program unparse-program]
    [parse-stmt unparse-stmt])
  "get-syntax-errors.rkt"
  "get-syntax.rkt"
  "parser.rkt"
  ragg/support
  racket/runtime-path)

(define-syntax with-syntax-handlers
  (syntax-rules ()
    [(_ body reparse)
     (with-handlers
      ([exn:fail:parsing? (lambda (e) reparse)])
       body)]))
        
(define (py-eval stx) (parse-program stx))

(define (parse-pyret/port ip (name "unnamed-pyret-file"))
  (let [(pyret-str (port->string ip))]
    (parse-pyret pyret-str name)))

(define (parse-pyret str (name "unnamed-pyret-file"))
  (with-syntax-handlers
    (parse-program (get-syntax name (open-input-string str)))
    (unparse-program (get-syntax-errors name (open-input-string str)))))

(define (parse-stmt-str str (name "unnamed-pyret-file"))
  (with-syntax-handlers
    (parse-stmt (get-stmt-syntax name (open-input-string str)))
    (unparse-stmt (get-stmt-syntax-errors name (open-input-string str)))))

