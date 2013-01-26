#lang racket

(require rackunit
	 "test-utils.rkt"
	 "../lang/runtime.rkt"
	 "match-set.rkt")

(verbose! #f)

(define five (p:mk-num 5))
(define two (p:mk-num 2))
(define ten (p:mk-num 10))

(check-pyret-match "5" (p:p-num _ (set) x 5))

(check-pyret "5" five)
(check-pyret-fail "2" five)

(check-pyret "fun f(): 2 end f()" two)
(check-pyret "fun f(x): x end f(2)" two)
(check-pyret "fun f(x): x end fun g(x): x end f(2) g(10) f(2)" two)
(check-pyret "fun f(x): fun g(x): x end g(x) end f(5)" five)
(check-pyret "fun foo(): 5 end foo()" five)
(check-pyret-fail "fun f(x): x end f(3)" two)

(check-pyret "\\x: (x)(2)" two)
(check-pyret "var x: 2 \\(x = 10)() x" ten)
(check-pyret "var x: 2 \\x: (x = 10)(5) x" two)
(check-pyret "var x: 2 fun f(g): g() end f(\\(x = 10)) x" ten)

(check-pyret "{}" (p:p-object (p:none) (set) p:empty-dict))

(check-pyret "'5'" (p:mk-str "5"))

(check-pyret-match "true" (p:p-bool _ _ _ #t))
(check-pyret-match "false" (p:p-bool _ _ _ #f))

(check-pyret "{x:5}" (p:p-object (p:none) (set)
                                 (make-immutable-hash (list (cons "x" five)))))
(check-pyret "{['x']:5}" (p:p-object (p:none) (set)
                                 (make-immutable-hash (list (cons "x" five)))))
(check-pyret "{['x'.append('y')]:5}" (p:p-object (p:none) (set)
                                                 (make-immutable-hash (list (cons "xy" five)))))
(check-pyret "var f: 'x' {[f]:5}" (p:p-object (p:none) (set)
                                              (make-immutable-hash (list (cons "x" five)))))

#;(check-pyret-match/libs "[]" (p:p-list (p:none) (set) _ (list)))
(check-pyret-match/libs "list.is-empty([]).and(list.is-List([]))"
                        (p:p-bool _ _ _ #t))

(check-pyret "seal({}, [])" (p:p-object (set) (set) p:empty-dict))
(check-pyret "seal({x:5}, ['x'])" (p:p-object (set "x") (set)
                                              (make-immutable-hash (list (cons "x" five)))))
(check-pyret "seal(seal({x:5, y:2}, ['y']), ['y'])"
             (p:p-object (set "y") (set)
                         (make-immutable-hash `(("x" . ,five) ("y" . ,two)))))
(check-pyret "seal(seal({x:5, y:2, z:10}, ['y', 'z']), ['y'])"
             (p:p-object (set "y") (set)
                         (make-immutable-hash `(("x" . ,five) ("y" . ,two) ("z" . ,ten)))))
(check-pyret "seal({x:5, y:2, z:10}, ['y', 'z'])"
             (p:p-object (set "y" "z") (set)
                         (make-immutable-hash `(("x" . ,five) ("y" . ,two) ("z" . ,ten)))))
(check-pyret-match "seal({x:5}, ['y'])" (p:p-object (set "y") _ (hash-table ("x" _))))
(check-pyret-match "seal(seal({x:5, y:2}, ['y']), ['x'])" (p:p-object (set) _ (hash-table ("x" _) ("y" _))))
(check-pyret-match "seal({}, ['y'])" (p:p-object (set "y") _ (hash-table)))
(check-pyret-match "seal(5, ['y'])" (p:p-num (set "y") _ _ 5))

;; TODO(joe): we should make a wrapper for seal, the current implementation just
;; checks if first and rest are present so 'y' appears to be empty
#;(check-pyret-exn "seal({x:5}, 'y')" "seal:")

(check-pyret-exn "seal({x:5}, []).x" "get-field:")
(check-pyret "seal({x:5}, ['x']).x" five)
(check-pyret "{x:5}.x" five)
(check-pyret-exn "{x:5}.y" "get-field:")
(check-pyret-exn "seal(seal({x:5, y:5}, ['y']), ['y']).x" "get-field:")
(check-pyret "seal(seal({x:5, y:5}, ['x', 'y']), ['y']).y" five)
(check-pyret-exn "seal(seal({x:5, y:5, z:5}, ['x', 'y']), ['y']).z" "get-field:")

(check-pyret-exn "seal(2, ['subtract']).add(2, 3)" "get-field:")

(check-pyret "fun f(x): x = 2 x end f(1)" two)
(check-pyret "fun f(x): x = 2 x = 5 x end f(1)" five)
(check-pyret-exn "fun f(x): y = 2 x end f(1)" "Unbound id")
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

(check-pyret "var x: 5 x" five)
(check-pyret "var x: 5 var y: x y" five)
;(check-pyret-exn "var x: 5 var x: 10 x" "duplicate")

; TODO(joe): Why don't unassigned variables work the way I want?  I expect
; this to say "undefined identifier used before defined", but it just evaluates
; to <#undefined>
;(check-pyret-exn "var w: zoot var zoot: 5 w" "undefined")

(check-pyret-match "brander()" (p:p-object _ (set) (hash-table ("brand" _) ("check" _))))
(check-pyret-match "fun f(x, y): x = brander() y = x.brand(y) y end f(1,2)"
                   (p:p-num _ (set _) _ 2))
(check-pyret-match "fun f(x,y): x = brander() y = x.brand(y) x.check(y) end f(1,2)"
                   (p:p-bool _ _ _ #t))
(check-pyret-match "fun f(x,y): x = brander() x.check(y) end f(1,2)"
                   (p:p-bool _ _ _ #f))
(check-pyret-match "fun f(x,y,z): x = brander() y = brander() z = x.brand(z) y.check(z) end f(1,2,3)"
                   (p:p-bool _ _ _ #f))
(check-pyret-match "fun f(x,y,z): x = brander() y = brander() z = x.brand(z) z = y.brand(z) x.check(z) end f(1,2,3)"
                   (p:p-bool _ _ _ #t))

;; can extract raw methods
(check-pyret-match "3:add" (p:p-method _ _ _ (? procedure?)))
(check-pyret-match "{f(x): 5}:f" (p:p-method _ _ _ (? procedure?)))

;; can put raw methods on other objects and use them
(check-pyret "var o: {x:5} var o2: {f(self): self.x} o = o.{g : o2:f} o.g()" five)

;; cannot apply raw methods (better error messages plz)
(check-pyret-exn "3:add()" "violation")


(check-pyret "3.add(2)" five)

;; two not three because side effects should happen only once
(check-pyret "var x: 0 fun f(): x = x.add(1) x end f().add(1)" two)

(check-pyret-exn "{extend seal({x:5},[]) with y:6}" "extend:")

(check-pyret-match "{extend {x:2} with y:10 }"
                   (p:p-object _ _ (hash-table ("x" (p:p-num _ _ _ 2))("y" (p:p-num _ _ _ 10)))))


(check-pyret "{extend {x:5} with y:6 }.x" five)
;; TODO(joe): change this to use : for method extraction
#;(check-pyret "{extend 5 with y:6}.add(2,3)" five)
(check-pyret-exn "{extend seal({x:5}, []) with x:10 }" "extend:")
(check-pyret-exn "seal({x:5}, []).{ x:10 }" "extend:")
(check-pyret "{extend {x:5} with x:10 }.x" ten)
(check-pyret "{x:5}.{x:10}.x" ten)
(check-pyret-match "{extend {extend {x:1} with y:2} with z:7}"
                   (p:p-object
                     _
                     _
                     (hash-table ("x" (p:p-num _ _ _ 1))
                                 ("y" (p:p-num _ _ _ 2))
                                 ("z" (p:p-num _ _ _ 7)))))
(check-pyret-exn "var o: seal({extend {x:1} with x:2}, []) o.x" "get-field:")
(check-pyret-exn "var o: seal({x:1}.{x:2}, []) o.x" "get-field:")

(check-pyret "cond: | true => 2 | false => 1 end" two)
(check-pyret "cond: | false => 1 | else => 2 end" two) 
(check-pyret "cond: | else => 2 end" two) 
(check-pyret "cond: | false => 2 | true => 10 end" ten)
(check-pyret "cond: | true => 2 | true => 1 end" two)
(check-pyret "cond: | 3.lessthan(2) => 10 | true => 2 end" two)
(check-pyret "cond: | 2.lessthan(3) => 10 end" ten)
(check-pyret-exn "cond: | 4.lessthan(3) => 10 end" "cond:")

;; shouldn't lift vars out of cond
(check-pyret-exn "cond: | true => var zed: 5 zed end zed" "undefined")

(check-pyret "{f(self): self.x, x:5}.f()" five)
(check-pyret "{f(self,y): self.x.add(y), x:4}.f(6)" ten)
(check-pyret "{extend {f(s): s.x, x:10} with x:5}.f()" five)

(check-pyret "{f(s): s.x, x:10}.{x:5}.f()" five)

(check-pyret "Racket.['+'](2, 3)" five)
(check-pyret-match "Racket.string-append('four', 'ty', 'two')" (p:p-str _ _ _ "fourtytwo"))
(check-pyret-exn "Racket.map(4,5)" "map")

(check-pyret
 "var x:0
  do \\f,g: (f() g()) x = 5; x end" five)

;; check expansions of or and and with do
(check-pyret
 "fun or(a,b):
    cond:
      | a() => true
      | true => b()
    end
  end
  var x: 5
  do or true; x = 2 end
  x" five)

(check-pyret
 "fun and(a,b):
    cond:
      | a() => b()
      | true => false
    end
  end
  var x: 5
  do and false; x = 2 end
  x" five)

(check-pyret
 "fun and(a,b):
    cond:
      | a() => b()
      | true => false
    end
  end
  var x: 5
  do and true; x = 2 end
  x" two)

(check-pyret
 "fun while(test, body):
    cond:
      | test() => body() while(test, body)
      | true => 'while base case'
    end
  end
  var x: 0
  do while x.lessthan(10); x = x.add(1) end
  x" ten)

(check-pyret
 "fun for(init, test, update, body):
    init()
    cond:
      | test() =>
          body()
          update()
          for(\\(), test, update, body)
      | true => 'for base case'
    end
  end
  var x: 0
  var sum: 0
  do for x = 0; x.lessthan(5); x = x.add(1);
    sum = sum.add(x)
  end
  sum" ten)


(check-pyret-exn
 "raise('error')"
 "error")

(check-pyret-exn
 "raise(2)"
 "2")

(check-pyret
 "data List
    | cons : first, rest with
        length(self): 1.add(self.rest.length())
    | empty with
        length(self): 0
  end
  cons(1, cons(2, empty())).length()"
 two)

(check-pyret
 "data List
    | cons : first, rest
    | empty
  sharing
    length(self):
      cond:
        | is-cons(self) => 1.add(self.rest.length())
        | is-empty(self) => 0
      end
  end
  cons(1, cons(2, empty())).length()"
  two)


(check-pyret-match
 "import '../lang/pyret-lib/file.arr' as file
  file.file"
 (p:p-fun _ _ _ _))

(check-pyret
 "var x: 5
  var y: x
  y"
 five)

;; TODO(joe): This is broken until we can come up with a 
;; better way to encapsulate test runs/not lift defines.
;; The test env is just different than what you get running a file
#;(check-pyret-exn
 "var x: 5
  var x: x
  y"
 "duplicate")

;; two nested directories deep, the string "inner" is provided
(check-pyret
 "import 'modules/nested-dir.arr' as result
  result"
 (p:mk-str "inner"))
 
(check-pyret-match
  "data Foo | bar end bar.doc"
  (p:p-str _ _ _ _))

(check-pyret-match
  "prim-keys({x : 5})"
  (p:p-object _ _ _))

(check-pyret
  "[5].first"
  five)

(check-pyret
  "[5].push(4).first"
  (p:mk-num 4))

(check-pyret
  "[5,6].rest.first"
  (p:mk-num 6))

(check-pyret
  "list.is-empty([])"
  (p:mk-bool #t))

(check-pyret
  "list.is-empty([5])"
  (p:mk-bool #f))

(check-pyret/libs
  "
  fun map(l, f):
    cond:
      | list.is-empty(l) => []
      | else => map(l.rest, f).push(f(l.first))
    end
  end
  var l1: map([5], \\x: (x.add(1))).first
  var l2: map([5,6,7], \\x: (x.add(1))).rest.rest.first
  l1.add(l2)" (p:mk-num 14))

;; TODO(joe): this doesn't work because first and rest are fields
;; and we cannot proxy them this way unless we define list differently
#;(check-pyret/libs
  "
fun mklist(l):
  l.{
    is-empty(self): is-empty(l),
    rest(self): mklist(l.rest()),
    first(self): l.first(),
    push(self, elt): mklist(l.push(elt)),
    map(self, f):
      cond:
        | self.is-empty() => mklist([])
        | else => self.rest().map(f).push(f(l.first()))
      end
  }
end
var l1: mklist([5]).map(\\x :: Number: (x.add(1))).first()
var l2: mklist([5,6,7]).map(\\x :: Number: (x.add(1))).rest().rest().first()
l1.add(l2)
  " (p:mk-num 14))

(check-pyret
  "
  import '../lang/pyret-lib/list.arr' as L
  5^L.link(L.empty()).first
  "
  five)

(check-pyret-match/libs
  "list.empty()"
  (p:p-object _ _ _))

(check-pyret/libs
 "builtins.keys({x:5}).first"
 (p:mk-str "x"))

;; NOTE(dbp): this is just testing that what we get back is
;; a proper list, which means it can .tostring() without erroring!
(check-pyret-match/libs
 "builtins.keys({x:5, y:6}).rest.tostring()"
 (p:p-str _ _ _ _))

(check-pyret
 "var x: 1
  fun f(): x = x.add(1) end
  var l: [f(), f(), f()]
  l.equals([2,3,4])"
 (p:mk-bool #t))

(check-pyret
 "seal([1], ['first']).{first: 2}.first"
 two)

(check-pyret-exn
 "seal([1], ['first']).{rest: 2}.rest"
 "extend:")

(check-pyret
  "var o: { m(self): self }
   var m: o:m
   m._fun(5)"
  five)

(check-pyret "tostring(1)" (p:mk-str "1"))
(check-pyret "[1,2,3].tostring()" (p:mk-str "[1, 2, 3]"))
(check-pyret "tostring('hello')" (p:mk-str "hello"))
(check-pyret "tostring({a: true})" (p:mk-str "{ a: true }"))
(check-pyret "tostring({a: 5, tostring(self): 'hello!'})"
	     (p:mk-str "hello!"))
