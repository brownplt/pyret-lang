#lang racket

(require "../parser.rkt" rackunit "../lang/ast.rkt")

(dynamic-require "../lang/pyret.rkt" 0)
(define ns (module->namespace "../lang/pyret.rkt"))

;; note - using eval-syntax below misses an important "enrichment" step:
;; http://docs.racket-lang.org/reference/eval.html?q=eval-syntax&q=eval-syntax&q=%23%25datum#(def._((quote._~23~25kernel)._eval-syntax))
;;
;; NB(joe):  I have no idea what that means
(define (pyret-parse str)
  (eval
   (get-syntax "parse-tests.rkt" (open-input-string str))
   ns))

(define (check-pyret str expected)
  (check-equal? (pyret-parse str) expected))

(define (check-pyret-exn str message)
  (check-exn (regexp (regexp-quote message)) (lambda () (pyret-parse str))))

(define-syntax test/match
  (syntax-rules ()
    [(test/match actual expected pred)
     (let ([actual-val (pyret-eval actual)])
       (with-check-info* (list (make-check-actual actual-val)
                               (make-check-expected 'expected))
                         (thunk (check-equal? (match actual-val
                                                [expected pred]
                                                [_ false])
                                              true))))]
    [(test/match actual expected)
     (test/match actual expected true)]))

(test/match "'str'" (s-block _ (list (s-str _ "str"))))
(test/match "5" (s-block _ (list (s-num _ 5))))

(test/match "5 'foo'" (s-block _ (list (s-num _ 5) (s-str _ "foo"))))

(test/match "{}" (s-block _ (list (s-obj _ empty))))
(test/match "{x:5}" (s-block _ (list (s-obj _ (list (s-data _ "x" (s-num _ 5)))))))
(test/match "{x:5,y:'foo'}" (s-block _ (list (s-obj _ (list (s-data _ "x" (s-num _ 5))
                                                            (s-data _ "y" (s-str _ "foo")))))))
(test/match "{x:{}}" (s-block _ (list (s-obj _ (list (s-data _ "x" (s-obj _ empty)))))))

(test/match "x" (s-block _ (list (s-id _ 'x))))
(test/match "{x:x}" (s-block _ (list (s-obj _ (list (s-data _ "x" (s-id _ 'x)))))))

(test/match "f()" (s-block _ (list (s-app _ (s-id _ 'f) empty))))
(test/match "f(5)" (s-block _ (list (s-app _ (s-id _ 'f) (list (s-num _ 5))))))

(test/match "f(5,'foo')" (s-block _ (list (s-app _ (s-id _ 'f) (list (s-num _ 5) (s-str _ "foo"))))))

(test/match "fun f(): 5 end"
            (s-block _ (list (s-fun _ 'f empty (s-block _ (list (s-num _ 5)))))))

(test/match "fun g(g): 5 end"
            (s-block _ (list (s-fun _ 'g (list 'g)
                                    (s-block _ (list (s-num _ 5)))))))

(test/match "fun g(g,f,x): 5 end"
            (s-block _ (list (s-fun _ 'g (list 'g 'f 'x)
                                    (s-block _ (list (s-num _ 5)))))))

(test/match "[]" (s-block _ (list (s-list _ empty))))
(test/match "[5]" (s-block _ (list (s-list _ (list (s-num _ 5))))))