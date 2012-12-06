#lang racket

(require rackunit "test-utils.rkt" "../lang/runtime.rkt" "match-set.rkt")

(define-simple-check (check-pyret str expected)
  (equal? (eval-pyret str) expected))

(define-simple-check (check-pyret-fail str expected)
  (not (equal? (eval-pyret str) expected)))

(define (check-pyret-exn str message)
  (check-exn (regexp (regexp-quote message)) (lambda () (eval-pyret str))))

(define-syntax check-pyret-match
  (syntax-rules ()
    [(_ str expected)
      (check-match (eval-pyret str) expected)]))

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
(check-pyret "fun foo(): 5 end foo()" five)
(check-pyret-fail "fun f(x): x end f(3)" two)

(check-pyret "\\x: (x)(2)" two)
(check-pyret "def x: 2 \\(x = 10)() x" ten)
(check-pyret "def x: 2 \\x: (x = 10)(5) x" two)
(check-pyret "def x: 2 fun f(g): g() end f(\\(x = 10)) x" ten)

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
(check-pyret-match "seal({x:5}, ['y'])" (p-object (set "y") _ _ (hash-table ("x" _))))
(check-pyret-match "seal(seal({x:5, y:2}, ['y']), ['x'])" (p-object (set) _ _ (hash-table ("x" _) ("y" _))))
(check-pyret-match "seal({}, ['y'])" (p-object (set "y") _ _ (hash-table)))
(check-pyret-match "seal(5, ['y'])" (p-num (set "y") _ _ (hash-table) 5))

(check-pyret-exn "seal({x:5}, 'y')" "seal:")

(check-pyret-exn "seal({x:5}, []).x" "get-field:")
(check-pyret "seal({x:5}, ['x']).x" five)
(check-pyret "{x:5}.x" five)
(check-pyret-exn "{x:5}.y" "get-field:")
(check-pyret-exn "seal(seal({x:5, y:5}, ['y']), ['y']).x" "get-field:")
(check-pyret "seal(seal({x:5, y:5}, ['x', 'y']), ['y']).y" five)
(check-pyret-exn "seal(seal({x:5, y:5, z:5}, ['x', 'y']), ['y']).z" "get-field:")

(check-pyret-exn "seal(2, ['subtract']).add(2, 3)" "get-field:")

(check-pyret "def o: {} o.x = 5 o.x" five)
(check-pyret "def o: {x:2} o.x = 5 o.x" five)
(check-pyret "fun f(o): o.x = 5 end def o2: {} f(o2) o2.x" five)
(check-pyret-exn "def sealed: seal({}, []) sealed.x = 4" "set-field:")
(check-pyret-exn "def sealed: seal({x:37}, []) sealed.x = 4" "set-field:")
(check-pyret "def sealed: seal({}, ['x']) sealed.x = 2 sealed.x" two)
(check-pyret "def sealed: seal({x:37}, ['x']) sealed.x = 5 sealed.x" five)

;; you can add fields that overwrite meta-fields (though this use of
;; number-objects is dubious).  Perhaps number-objects should be frozen
;; by default to not allow this.  For now, using this as a test of a 
;; non-number-specific property
(check-pyret "fun badadd(n1, n2): 2 end
              def snum: 5
              snum.add = badadd
              snum.add(snum, 3)" two)

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

(check-pyret "def x: 5 x" five)
(check-pyret "def x: 5 def y: x y" five)
;(check-pyret-exn "def x: 5 def x: 10 x" "duplicate")

; TODO(joe): Why don't unassigned variables work the way I want?  I expect
; this to say "undefined identifier used before defined", but it just evaluates
; to <#undefined>
;(check-pyret-exn "def w: zoot def zoot: 5 w" "undefined")

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
(check-pyret "10.minus(10, 5)" five)

(check-pyret "3:add(2)" five)

;; two not three because side effects should happen only once
(check-pyret "def x: 0 fun f(): x = x:add(1) x end f():add(1)" two)

(check-pyret-exn "{extend seal({x:5},[]) with y:6}.x" "get-field:")

(check-pyret-match "{extend {x:2} with y:10 }"
                   (p-object _ (hash-table ("x" (p-num _ _ _ _ 2))) _ (hash-table ("y" (p-num _ _ _ _ 10)))))


(check-pyret "{extend {x:5} with y:6 }.x" five)
(check-pyret "{extend 5 with y:6}.add(2,3)" five)
(check-pyret "{extend seal({x:5}, []) with x:10 }.x" ten)
(check-pyret-match "{extend seal({x:5},[]) with x:10 }"
                   (p-object _ (hash-table) _ (hash-table ("x" (p-num _ _ _ _ 10)))))
(check-pyret "{extend {x:5} with x:10 }.x" ten)
(check-pyret-match "{extend {extend {x:1} with y:2} with z:7}"
                   (p-object _
                             (hash-table ("x" (p-num _ _ _ _ 1)) ("y" (p-num _ _ _ _ 2)))
                             _
                             (hash-table ("z" (p-num _ _ _ _ 7)))))
(check-pyret-exn "def o: seal({extend {x:1} with x:2}, []) o.x" "get-field:")

(check-pyret "cond: | true => 2 | false => 1 end" two)
(check-pyret "cond: | false => 2 | true => 10 end" ten)
(check-pyret "cond: | true => 2 | true => 1 end" two)
(check-pyret "cond: | 3:lessthan(2) => 10 | true => 2 end" two)
(check-pyret "cond: | 2:lessthan(3) => 10 end" ten)
(check-pyret-exn "cond: | 4:lessthan(3) => 10 end" "cond:")

;; shouldn't lift defs out of cond
(check-pyret-exn "cond: | true => def zed: 5 zed end zed" "undefined")

#;(check-pyret-exn "def x :: Number: true" "type:")
#;(check-pyret "def x :: Number: 5 x" five)
#;(check-pyret-exn "def x :: Number: 5 x = 'not-a-num'" "type:")


