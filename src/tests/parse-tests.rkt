#lang racket

(require "../lang/parser.rkt" rackunit "../lang/ast.rkt")

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

(define (check-pyret-exn str message)
  (check-exn (regexp (regexp-quote message)) (lambda () (pyret-parse str))))

(define-syntax test/match
  (syntax-rules ()
    [(_ actual expected pred)
     (let ([actual-val actual])
       (with-check-info* (list (make-check-actual actual-val)
                               (make-check-expected 'expected))
                         (thunk (check-equal? (match actual-val
                                                [expected pred]
                                                [_ false])
                                              true))))]
    [(_ actual expected)
     (test/match actual expected true)]))

(define-syntax check/block
  (syntax-rules ()
    [(_ str stmt ...)
     (test/match (pyret-parse str) (s-block _ (list stmt ...)))]))

(check/block "'str'" (s-str _ "str"))
(check/block "5" (s-num _ 5))

(check/block "5 'foo'" (s-num _ 5) (s-str _ "foo"))

(check/block "{}" (s-obj _ empty))
(check/block "{x:5}" (s-obj _ (list (s-data _ "x" (s-num _ 5)))))
(check/block "{x:5,y:'foo'}" (s-obj _ (list (s-data _ "x" (s-num _ 5))
                                                            (s-data _ "y" (s-str _ "foo")))))
(check/block "{x:{}}" (s-obj _ (list (s-data _ "x" (s-obj _ empty)))))

(check/block "x" (s-id _ 'x))
(check/block "{x:x}" (s-obj _ (list (s-data _ "x" (s-id _ 'x)))))

(check/block "f()" (s-app _ (s-id _ 'f) empty))
(check/block "f(5)" (s-app _ (s-id _ 'f) (list (s-num _ 5))))

(check/block "f(5,'foo')" (s-app _ (s-id _ 'f) (list (s-num _ 5) (s-str _ "foo"))))

(check/block "fun f(): 5 end"
            (s-fun _ 'f empty (s-block _ (list (s-num _ 5)))))

(check/block "fun g(g): 5 end"
            (s-fun _ 'g (list 'g)
                   (s-block _ (list (s-num _ 5)))))

(check/block "fun g(g,f,x): 5 end"
             (s-fun _ 'g (list 'g 'f 'x)
                    (s-block _ (list (s-num _ 5)))))

(check/block "[]" (s-list _ empty))
(check/block "[5]" (s-list _ (list (s-num _ 5))))

(check/block "o.x" (s-dot _ (s-id _ 'o) 'x))
(check/block "seal({x:5}, []).x"
             (s-dot _ (s-app _ (s-id _ 'seal) (list (s-obj _ (list (s-data _ "x" (s-num _ 5))))
                                                    (s-list _ empty)))
                    'x))

(check/block "o.('x')" (s-bracket _ (s-id _ 'o) (s-str _ "x")))

(check/block "x = 1" (s-assign _ 'x (s-num _ 1)))

(check/block "o.x = 1" (s-dot-assign _ (s-id _ 'o) 'x (s-num _ 1)))

(check/block "o.('x') = 1" 
            (s-bracket-assign _ (s-id _ 'o) (s-str _ "x") (s-num _ 1)))