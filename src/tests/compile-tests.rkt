#lang racket

(require rackunit "test-utils.rkt" "../lang/runtime.rkt" "match-set.rkt")

(define (check-pyret str expected)
  (check-equal? (eval-pyret str) expected))

(define (check-pyret-fail str expected)
  (check-not-equal? (eval-pyret str) expected))

(define (check-pyret-exn str message)
  (check-exn (regexp (regexp-quote message)) (lambda () (eval-pyret str))))


(define five (mk-num 5))
(define two (mk-num 2))
(define ten (mk-num 10))

(check-pyret-match "5" (p-num _ _ (set) x 5))

(check-pyret "5" five)

(check-pyret-fail "2" five)

(check-pyret "fun f(): 2 end f()" two)

(check-pyret "fun f(x): x end f(2)" two)

(check-pyret "fun f(x): x end fun g(x): x end f(2) g(10) f(2)" two)

(check-pyret "fun f(x): fun g(x): x end g(x) end f(5)" five)

(check-pyret-fail "fun f(x): x end f(3)" two)

(check-pyret "{}" (p-object (none) meta-null (set) (make-hash)))

(check-pyret "'5'" (mk-str "5"))

(check-pyret-match "true" (p-bool _ _ _ _ #t))

(check-pyret-match "false" (p-bool _ _ _ _ #f))

(check-pyret "{x:5}" (p-object (none) meta-null (set) (make-hash (list (cons "x" five)))))

(check-pyret "[]" (p-list (none) meta-null (set) (make-hash) (list)))

(check-pyret "seal({}, [])" (p-object (set) meta-null (set) (make-hash)))

(check-pyret "seal({x:5}, ['x'])" (p-object (set "x") meta-null (set) (make-hash (list (cons "x" five)))))

(check-pyret "seal(seal({x:5, y:2}, ['y']), ['y'])"
             (p-object (set "y") meta-null (set) (make-hash `(("x" . ,five) ("y" . ,two)))))

(check-pyret "seal(seal({x:5, y:2, z:10}, ['y', 'z']), ['y'])"
             (p-object (set "y") meta-null (set) (make-hash `(("x" . ,five) ("y" . ,two) ("z" . ,ten)))))

(check-pyret "seal({x:5, y:2, z:10}, ['y', 'z'])"
             (p-object (set "y" "z") meta-null (set) (make-hash `(("x" . ,five) ("y" . ,two) ("z" . ,ten)))))

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
(check-pyret "fun fundo(o):
                fun f(x):
                  fun g(): x = 2 end
                  fun h(): x end
                  {g: g, h: h} 
                end
                o = f(1)
                o.g()
                o.h()
              end
              fundo({})" two)

(check-pyret-match "brander()" (p-object _ _ (set) (hash-table ("brand" _) ("check" _))))
(check-pyret-match "fun f(x, y): x = brander() y = x.brand(y) y end f(1,2)"
    (p-num _ _ (set _) _ 2))
(check-pyret-match "fun f(x,y): x = brander() y = x.brand(y) x.check(y) end f(1,2)"
    (p-bool _ _ _ _ #t))
(check-pyret-match "fun f(x,y): x = brander() x.check(y) end f(1,2)"
    (p-bool _ _ _ _ #f))
(check-pyret-match "fun f(x,y,z): x = brander() y = brander() z = x.brand(z) y.check(z) end f(1,2,3)"
    (p-bool _ _ _ _ #f))
(check-pyret-match "fun f(x,y,z): x = brander() y = brander() z = x.brand(z) z = y.brand(z) x.check(z) end f(1,2,3)"
    (p-bool _ _ _ _ #t))

(check-pyret "3.add(3, 2)" five)
(check-pyret "10.subtract(10, 5)" five)

