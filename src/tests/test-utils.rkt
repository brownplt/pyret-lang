#lang racket

(provide parse-pyret eval-pyret check-match check-pyret-match)
(require rackunit
         "../lang/compile.rkt"
         "../lang/tokenizer.rkt")


(dynamic-require "../lang/parser.rkt" 0)
(define ns (module->namespace "../lang/parser.rkt"))

(define (eval-pyret str)
  (eval
    (compile-str str)
    ns))

(define (compile-str str)
  (compile-pyret (parse-pyret str)))

;; note - using eval-syntax below misses an important "enrichment" step:
;; http://docs.racket-lang.org/reference/eval.html?q=eval-syntax&q=eval-syntax&q=%23%25datum#(def._((quote._~23~25kernel)._eval-syntax))
;;
;; NB(joe):  I have no idea what that means
(define (parse-pyret str)
  (eval
   (get-syntax "parse-tests.rkt" (open-input-string str))
   ns))

(define (check-parse-exn str message)
  (check-exn (regexp (regexp-quote message)) (lambda () (parse-pyret str))))

(define-syntax check-match
  (syntax-rules ()
    [(_ actual expected pred)
     (let ([actual-val actual])
       (with-check-info* (list (make-check-actual actual-val)
                               (make-check-expected 'expected))
                         (thunk (check-true (match actual-val
                                              [expected pred]
                                              [_ false])))))]
    [(_ actual expected)
     (check-match actual expected true)]))

(define-syntax check-pyret-match
  (syntax-rules ()
    [(_ str expected)
      (check-match (eval-pyret str) expected)]))
