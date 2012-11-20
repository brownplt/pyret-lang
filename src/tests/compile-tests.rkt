#lang racket

(require rackunit
         "../lang/compile-pyret.rkt" 
         "../lang/parser.rkt" 
         "../lang/values.rkt")

(dynamic-require "../lang/pyret.rkt" 0)
(define ns (module->namespace "../lang/pyret.rkt"))

;; note - using eval-syntax below misses an important "enrichment" step:
;; http://docs.racket-lang.org/reference/eval.html?q=eval-syntax&q=eval-syntax&q=%23%25datum#(def._((quote._~23~25kernel)._eval-syntax))
;;
;; NB(joe):  I have no idea what that means
(define (pyret-eval str)
  (eval
    (compile-str str)
    ns))

(define (compile-str str)
  (compile-pyret
   (eval
    (get-syntax "cmdline" (open-input-string str))
    ns)))

(define (check-pyret str expected)
  (check-equal? (pyret-eval str) expected))

(define (check-pyret-fail str expected)
  (check-not-equal? (pyret-eval str) expected))

(define (check-pyret-exn str message)
  (check-exn (regexp (regexp-quote message)) (lambda () (pyret-eval str))))


(define (mk-num n)
  (p-num n (none) (make-hash)))

(define (mk-str s)
  (p-str s (none) (make-hash)))

(define five (mk-num 5))
(define two (mk-num 2))
(define ten (mk-num 10))

(check-pyret "5" five)

(check-pyret-fail "2" five)

(check-pyret "fun f(): 2 end f()" two)

(check-pyret "fun f(x): x end f(2)" two)

(check-pyret "fun f(x): x end fun g(x): x end f(2) g(10) f(2)" two)

(check-pyret "fun f(x): fun g(x): x end g(x) end f(5)" five)

(check-pyret-fail "fun f(x): x end f(3)" two)

(check-pyret "{}" (p-object (none) (make-hash)))

(check-pyret "'5'" (mk-str "5"))

(check-pyret "{x:5}" (p-object (none) (make-hash (list (cons "x" five)))))

(check-pyret "[]" (p-list (list) (none) (make-hash)))

(check-pyret "seal({}, [])" (p-object (set) (make-hash)))

(check-pyret "seal({x:5}, ['x'])" (p-object (set "x") (make-hash (list (cons "x" five)))))

(check-pyret "seal(seal({x:5, y:2}, ['y']), ['y'])"
             (p-object (set "y") (make-hash `(("x" . ,five) ("y" . ,two)))))

(check-pyret "seal(seal({x:5, y:2, z:10}, ['y', 'z']), ['y'])"
             (p-object (set "y") (make-hash `(("x" . ,five) ("y" . ,two) ("z" . ,ten)))))

(check-pyret "seal({x:5, y:2, z:10}, ['y', 'z'])"
             (p-object (set "y" "z") (make-hash `(("x" . ,five) ("y" . ,two) ("z" . ,ten)))))

(check-pyret-exn "seal(seal({x:5, y:2}, ['y']), ['x'])" "seal:")

(check-pyret-exn "seal({x:5}, ['y'])" "seal:")

(check-pyret-exn "seal({x:5}, 'y')" "seal:")

(check-pyret-exn "seal({}, ['y'])" "seal:")

(check-pyret-exn "seal(5, ['y'])" "seal:")

(check-pyret "fun foo(): 5 end foo()" five)

(check-pyret-exn "seal({x:5}, []).x" "get-field:")
(check-pyret "seal({x:5}, ['x']).x" five)
(check-pyret "{x:5}.x" five)
(check-pyret-exn "{x:5}.y" "get-field:")
(check-pyret-exn "seal(seal({x:5, y:5}, ['y']), ['y']).x" "get-field:")
(check-pyret "seal(seal({x:5, y:5}, ['x', 'y']), ['y']).y" five)
(check-pyret-exn "seal(seal({x:5, y:5, z:5}, ['x', 'y']), ['y']).z" "get-field:")

(check-pyret "fun f(x): x = 2 x end f(1)" two)
(check-pyret "fun f(x): x = 2 x = 5 x end f(1)" five)
(check-pyret-exn "fun f(x): y = 2 x end f(1)" "set!:")
(check-pyret "fun f(x): fun g(): x = 2 end g() x end f(1)" two)
(check-pyret "fun f(x): fun g(x): x = 2 end g(1) x end f(5)" five)
(check-pyret "fun do(o):
                fun f(x):
                  fun g(): x = 2 end
                  fun h(): x end
                  {g: g, h: h} 
                end
                o = f(1)
                o.g()
                o.h()
              end 
              do({})" two)