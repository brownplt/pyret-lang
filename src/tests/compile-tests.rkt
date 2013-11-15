#lang racket

(require
  rackunit
  rackunit/text-ui
	 "test-utils.rkt"
   "../parameters.rkt"
	 "../lang/runtime.rkt")

(verbose! #f)

(define five (p:mk-num 5))
(define two (p:mk-num 2))
(define ten (p:mk-num 10))

(define true (p:mk-bool #t))
(define false (p:mk-bool #f))

(define CONFLICT-MESSAGE "variable and identifier")

(define constants (test-suite
  "constants and literals"

  (check-pyret-match "5" (p:p-num (hash-table) x _ _ 5))
  (check-pyret "5" five)
  (check-pyret-fail "2" five)

  (check-pyret "{}" (p:mk-object (make-immutable-hash)))

  (check-pyret "'5'" (p:mk-str "5"))

  (check-pyret-match "true" (p:p-bool _ _ _ _ #t))
  (check-pyret-match "false" (p:p-bool _ _ _ _ #f))

  (check-pyret "{x:5}" (p:mk-object (make-immutable-hash (list (cons "x" five)))))
  (check-pyret "{['x']:5}" (p:mk-object (make-immutable-hash (list (cons "x" five)))))
  (check-pyret "f = 'x' {[f]:5}" (p:mk-object (make-immutable-hash (list (cons "x" five)))))

  (check-pyret "{['x'.append('y')]:5}" (p:mk-object (make-immutable-hash (list (cons "xy" five)))))

  (check-pyret "'fod'.substring(1,2)" (p:mk-str "o"))

  (check-pyret "'fod'.char-at(1)" (p:mk-str "o"))

  (check-pyret "'fod'.repeat(1)" (p:mk-str "fod"))
  (check-pyret "'fod'.repeat(0)" (p:mk-str ""))
  (check-pyret "'fod'.repeat(3)" (p:mk-str "fodfodfod"))
  ))


(define functions (test-suite
  "simple functions"

  (check-pyret "fun f(): 2 end f()" two)
  (check-pyret "fun f(x): x end f(2)" two)
  (check-pyret "fun f(x): x end fun g(x): x end f(2) g(10) f(2)" two)
  (check-pyret-exn "fun f(x): fun g(x): x end g(x) end f(5)" "nested scopes")
  (check-pyret "fun foo(): 5 end foo()" five)

  (check-pyret-fail "fun f(x): x end f(3)" two)

  (check-pyret "fun(x): x end(2)" two)
  (check-pyret "var x = 2 fun: x := 10 end() x" ten)
  (check-pyret-exn "var x = 2 fun(x): x := 10 end(5) x" CONFLICT-MESSAGE)
  (check-pyret "var x = 2 fun f(g): g() end f(fun: x := 10 end) x" ten)

  (check-pyret-exn "fun f(x): x = 2 x end f(1)" "nested scopes")
  (check-pyret "fun f(): var x = 1 x := 2 x := 5 x end f()" five)
  (check-pyret-exn "fun f(x): y := 2 x end f(1)" "Assigning to unbound variable")
  (check-pyret "fun f(): var x = 1 fun g(): x := 2 end g() x end f()" two)
  (check-pyret-exn "fun f(x, y): x end f(3,4,5)" "Expected 2")
  (check-pyret-exn "fun f(x, y): x end f(3)" "Expected 2")
  (check-pyret "fun fundo():
                  var o = {}
                  var x = 1
                  fun f():
                    fun g(): x := 2 end
                    fun h(): x end
                    {g: g, h: h}
                  end
                  o := f()
                  o.g()
                  o.h()
                end
                fundo()" two)

  ;; regression for order-of-operations; non-function error happens
  ;; before evaluating args
  ;; UPDATE(joe): matching Racket on this for efficiency, so args are
  ;; evaluated even if function position is illegal
  (check-pyret-exn
    "5(raise('foo'))"
    "foo")

  ;; tostring on functions
  #;(check-pyret-match "fun f(x): x end f.tostring()"
             (p:p-str _ _ _ _ "fun f(x): '' end"))

  #;(check-pyret-match "fun f(x): doc: 'great' x end f.tostring()"
               (p:p-str _ _ _ _ "fun f(x): 'great' end"))

  #;(check-pyret-match "(fun(x): x end).tostring()"
               (p:p-str _ _ _ _ "fun (x): '' end"))

  #;(check-pyret-match "(fun(x): doc: 'great' x end).tostring()"
               (p:p-str _ _ _ _ "fun (x): 'great' end"))

  #;(check-pyret-match "(fun(x :: Number): doc: 'great' x end).tostring()"
               (p:p-str _ _ _ _ "fun (x): 'great' end"))

))


(define brands (test-suite "brands"

  (check-pyret-match "brander()" (p:p-object (hash-table) (hash-table ("brand" _) ("test" _)) _ _))
  (check-pyret-match "fun f(z): x = brander() y = x.brand(z) y end f(2)"
                     (p:p-num (hash-table _) _ _ _ 2))
  (check-pyret-match "fun f(z): x = brander() y = x.brand(z) x.test(y) end f(2)"
                     (p:p-bool _ _ _ _ #t))
  (check-pyret-match "fun f(y): x = brander() x.test(y) end f(2)"
                     (p:p-bool _ _ _ _ #f))
  (check-pyret-match "fun f(z): x = brander() y = brander() u = x.brand(z) y.test(u) end f(3)"
                     (p:p-bool _ _ _ _ #f))
  (check-pyret-match "fun f(z): x = brander() y = brander() u = x.brand(z) w = y.brand(u) x.test(w) end f(3)"
                     (p:p-bool _ _ _ _ #t))

  (check-pyret
    "b = brander()
     true-branded = b.brand(true)
     when true-branded: 5 end"
     five)

  (check-pyret
    "b = brander()
     b.test(b.brand({}).{x: 5})"
    true)
  (check-pyret
    "b = brander()
     b.test(b.brand({x:10}).{x: 5})"
    false)
  (check-pyret
    "b = brander()
     b.test(b.brand([4]).{ bonus-field: {} })"
    true)
  (check-pyret
    "b = brander()
     b.test(b.brand([4]).{ first: [] })"
    false)

  (check-pyret
    "check-brand(fun(o): true end, 5, 'Num')"
    (p:mk-num 5))
  (check-pyret-exn
    "check-brand(5, {}, 'foo')"
    "cannot check-brand with non-function")
  (check-pyret-exn
    "check-brand(fun(): end, {}, 5)"
    "cannot check-brand with non-string")
  (check-pyret-exn
    "check-brand(2, {}, 5)"
    "check-brand failed")

))






(define cases (test-suite "cases"

  (check-pyret "
  when true: 5 end
  " five)

  (check-pyret "
  when true: when true: 5 end end
  " five)

  (check-pyret "
  when true: when false: 5 end end
  " nothing)

  (check-pyret "
  when false: 5 end
  "
  nothing)

  ;; TODO(joe): when we switch to no shadowing, this should be an appropriate exn
  #;(check-pyret "
  nothing = 42
  when false: 5 end
  "
  nothing)

  (check-pyret "if false: 5 else if true: 6 end" (p:mk-num 6))
  (check-pyret "if true: 5 else if true: 6 end" five)
  (check-pyret "if true: 5 else: 6 end" five)
  (check-pyret "if false: 5 else if false: 6 else: 7 end" (p:mk-num 7))
  (check-pyret "if false: 5 else if false: 6 else if false: 8 else: 7 end"
               (p:mk-num 7))
  (check-pyret "if false: 5 else if false: 6 else if true: 8 else: 7 end"
               (p:mk-num 8))
  (check-pyret "if false: 5 else if false: 6 else if true: 8 end"
               (p:mk-num 8))

  (check-pyret-match/check "pyret/cases/cases1.arr" _ 4)
  (check-pyret-match/check "pyret/cases/cases-list.arr" _ 4)
  (check-pyret-match/check "pyret/cases/cases-raw-fun.arr" _ 2)
))


(define for-block (test-suite "for-block"

  (check-pyret
   "strs = for list.map(elt from [1,2,3]): elt.tostring() end
    strs._equals(['1','2','3'])"
   true)

  (check-pyret
   "str = for list.find(elt from ['1','2','3']): elt == '2' end
    str.value"
   (p:mk-str "2"))

  (check-pyret
   "str = for list.find(elt from ['1','2','3']): elt == 'not-in-list' end
    option.is-none(str)"
   true)

  (check-pyret
   "for list.fold(acc from 0, elt from [1,2,3]): acc._plus(elt) end"
   (p:mk-num 6))

  (check-pyret-exn
   "for list.map(elt :: Number from [1, '2', 3]): nothing end"
   "expected Number and got")

  (check-pyret-exn
   "for list.map(elt :: Number from [true, 2, 3]): nothing end"
   "expected Number and got")

  (check-pyret-exn
   "for list.fold(acc from '', elt from ['1','2']) -> Number: elt end"
   "expected Number and got")

))

(define data (test-suite "data"

  ;; Regression tests for double-desugar in data definitions
  (check-pyret
    "data Foo:
      | test with:
        bar(self):
          try:
            raise(42)
          except(e):
            5
          end
        end
    end
    test.bar()"
    five)

  (check-pyret
    "data Foo:
      | test() with:
        bar(self):
          try:
            raise(42)
          except(e):
            5
          end
        end
    end
    test().bar()"
    five)


  (check-pyret-match
    "data Foo: | bar() end bar._doc"
    (p:p-str _ _ _ _ _))

  (check-pyret
   "data MyList:
      | cons(first, rest) with: length(self): 1._add(self.rest.length()) end
      | mt() with: length(self): 0 end
    end
    cons(1, cons(2, mt())).length()"
   two)

  (check-pyret
   "data MyList:
      | cons(first, rest)
      | mt()
    sharing:
      length(self):
        cases(MyList) self:
          | cons(_, rest) => 1._add(rest.length())
          | mt => 0
        end
      end
    end
    cons(1, cons(2, mt())).length()"
    two)


  (check-pyret
   "data MyList:
     | mt with: length(self): 0 end
    end
    mt.length()
    "
    (p:mk-num 0))

  (check-pyret
   "data MyList:
     | mt
    sharing:
      length(self): 0 end
    end
    mt.length()
    "
    (p:mk-num 0))


  (check-pyret-match
   "data Foo:
     | singleton
    end
    singleton"
    (? p:p-object? _))

  (check-pyret
   "data Foo:
     | singleton
    end
    is-singleton(singleton) and Foo(singleton)"
    (p:mk-bool #t))

  (check-pyret
   "data Foo:
     | single
    end
    fun f(s :: is-single): when is-single(s): true end end
    f(single)"
    (p:mk-bool #t))

  (check-pyret-exn
   "data Foo:
     | single
    end
    fun f(s :: is-single): when is-single(s): true end end
    f(4)"
    "expected is-single")

  (check-pyret
   "data Color:
     | red with: asRGB(_): { r: 256, g: 0, b: 0 } end
     | green with: asRGB(_): { r: 0, g: 256, b: 0 } end
     | blue with: asRGB(_): { r: 0, g: 0, b: 256 } end
     | rgb(r :: Number, g :: Number, b :: Number) with:
        asRGB(self): { r: self.r, g: self.g, b: self.b } end
    end
    fun asRGB(obj): obj.asRGB() end
    fun getR(obj): obj.r end
    [rgb(100,100,100), red, green].map(asRGB).map(getR)._equals([100, 256, 0])
    "
    (p:mk-bool #t))

  (check-pyret
   "data D: | foo sharing:
      meth(self):
        try: raise(22)
        except(e): e
        end
      end
    end
    foo.meth()
    "
    (p:mk-num 22))

  (check-pyret-exn
   "data Test:
     | test(x, x)
    end"
   "duplicate")

  (check-pyret-match/check "pyret/data/data-equals.arr" _ 27)
  (check-pyret-match/check "pyret/data/data-eq.arr" _ 24)
  (check-pyret-match/check "pyret/data/params.arr" _ 4)
  (check-pyret-match/check "pyret/data/data-shared-mutable.arr" _ 4)


  (check-pyret
   "datatype D: | foo with constructor(self): self end end
    is-foo(foo)"
   (p:mk-bool #t))

  (check-pyret
   "datatype D: | foo(a) with constructor(self): self end end
    foo(10).a"
   (p:mk-num 10))

  (check-pyret
   "datatype D<T>: | foo(a :: T) with constructor(self): self end
                   | bar(f :: D<Number>) with constructor(self): self end
    end
    bar(foo(10)).f.a"
   (p:mk-num 10))



  ))

(define modules (test-suite "modules"

  (check-pyret
    "import 'modules/provide-desugar-regression.arr' as pdr
     pdr.x"
    (p:mk-num 55))

  (check-pyret-match
   "import file as f
    f.input-file"
   (? p:p-fun? _))
  ;; two nested directories deep, the string "inner" is provided
  (check-pyret
   "import 'modules/nested-dir.arr' as result
    result"
   (p:mk-str "inner"))

  (check-pyret
   "import 'modules/nested-uses-list.arr' as result
    result"
   (p:mk-str "list-exists"))

  (check-pyret
   "import 'modules/importing-data.arr' as d1
    import 'modules/importing-data.arr' as d2
    var a-d = d1.d()
    d2.is-d(a-d)"
    (p:mk-bool #t))

  (check-pyret
    "import 'modules/dependency/B.arr' as b
     import 'modules/dependency/C.arr' as c
     import 'modules/dependency/F.arr' as f
     from-b = b.mk()
     from-c = c.mk()
     from-f = f.mk()
     brand_tests = [b.test(from-b), b.test(from-c), b.test(from-f)
                ,c.test(from-b), c.test(from-c), c.test(from-f)
                ,f.test(from-b), f.test(from-c), f.test(from-f)]
     list.is-empty(brand_tests.filter(fun(x): not x end))
  " (p:mk-bool #t))

  (check-pyret
    "import 'modules/provide-star.arr' as P
    (prim-num-keys(P) == 8) and
    (for list.fold(all from true, field from
              ['Thing',
               'is-singleton', 'singleton',
               'is-non-singleton', 'non-singleton',
               'foo', 'x', 'z']):
      all and builtins.has-field(P, field)
     end)"
   (p:mk-bool #t))
  ))


(define built-in-libraries (test-suite "built-in-libraries"

  (check-match-file "pyret/print.arr" _
"5
{}
Looks shipshape, all 2 tests passed, mate!
" 2)

  (check-pyret-match "list.is-empty([]) and list.List([])"
                          (? p:pyret-true? _))

  (check-pyret-match/check "pyret/libs/list-tests.arr" _ 8)

  (check-pyret-match/check "pyret/libs/json.arr" _ 8)

  (check-pyret-match
    "prim-keys({x : 5})"
    (? p:p-object? _))

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


  (check-pyret
     "option.is-some(option.some(2))"
     (p:mk-bool #t))

  (check-pyret
     "option.Option(option.none)"
     (p:mk-bool #t))

  (check-pyret
     "option.is-some(option.none)"
     (p:mk-bool #f))

  (check-pyret
    "
    fun ourmap(l, f):
      if list.is-empty(l): []
      else: ourmap(l.rest, f).push(f(l.first))
      end
    end
    l1 = ourmap([5], fun(x): x._add(1) end).first
    l2 = ourmap([5,6,7], fun(x): x._add(1) end).rest.rest.first
    l1._add(l2)" (p:mk-num 14))

  (check-pyret "import Racket as R
                R('racket')('+',2, 3)" five)
  (check-pyret-match "import Racket as R
                      R('racket')('string-append','four', 'ty', 'two')" (p:p-str _ _ _ _ "fourtytwo"))
  (check-pyret-exn "import Racket as R
                    R('racket')('map',4,5)" "map")
  (check-pyret "import Racket as R
                img = R('2htdp/image')
                scene = img('empty-scene', 10, 10)
                img('image-width', scene)"
                ten)
  ;; TODO(joe): this doesn't work because first and rest are fields
  ;; and we cannot proxy them this way unless we define list differently
  #;(check-pyret
    "
  fun mklist(l):
    l.{
      is-empty(self): is-empty(l),
      rest(self): mklist(l.rest()),
      first(self): l.first(),
      push(self, elt): mklist(l.push(elt)),
      map(self, f):
        if self.is-empty(): mklist([])
        else: self.rest().map(f).push(f(l.first()))
        end
    }
  end
  l1 = mklist([5]).map(fun(x :: Number): x._add(1) end).first()
  l2 = mklist([5,6,7]).map(fun(x :: Number): x._add(1) end).rest().rest().first()
  l1._add(l2)
    " (p:mk-num 14))

  (check-pyret
    "
    L = list
    5^L.link(L.empty).first
    "
    five)

  (check-pyret-match
    "list.empty"
    (? p:p-object? _))

  (check-pyret
   "builtins.keys({x:5}).first"
   (p:mk-str "x"))

  (check-pyret
   "builtins.has-field({x:5},'x')"
   true)

  (check-pyret
   "builtins.has-field(5.{x:10},'x')"
   true)

  (check-pyret
   "builtins.has-field({x:10},'y')"
   false)

  (check-pyret-exn
   "builtins.has-field({}, {})"
   "expected string")

  (check-pyret
   "list.List(builtins.keys({y:5, x:6}).foldr(list.link, []))"
   (p:mk-bool #t))

  (check-pyret
   "var x = 1
    fun f(): x := x._add(1) end
    l = [f(), f(), f()]
    l._equals([2,3,4])"
   (p:mk-bool #t))

  (check-pyret "[1,2].sort-by(fun(e1,e2): e1 < e2 end,
                              fun(e1,e2): e1 == e2 end) == [1,2]"
               (p:mk-bool #t))

  (check-pyret "[3,1,2,3,4].sort-by(fun(e1,e2): e1 < e2 end,
                                    fun(e1,e2): e1 == e2 end) == [1,2,3,3,4]"
               (p:mk-bool #t))

  (check-pyret "[].sort-by(fun(e1,e2): e1 < e2 end,
                           fun(e1,e2): e1 == e2 end) == []"
               (p:mk-bool #t))

  (check-pyret "[1,2,3].sort-by(fun(e1,e2): e1 < e2 end,
                                fun(e1,e2): e1 == e2 end) == [1,2,3]"
               (p:mk-bool #t))

  (check-pyret "[1,2].sort() == [1,2]"
               (p:mk-bool #t))

  (check-pyret "[3,1,2,3,4].sort() == [1,2,3,3,4]"
               (p:mk-bool #t))

  (check-pyret "[].sort() == []"
               (p:mk-bool #t))

  (check-pyret "[1,2,3].sort() == [1,2,3]"
               (p:mk-bool #t))

  (check-pyret "option.some(4).orelse(5)" (p:mk-num 4))

  (check-pyret "option.none.orelse(5)" (p:mk-num 5))

  ;; NOTE(joe): allow this here because checkers
  #;(parameterize [(current-allow-shadowed-vars #t) (current-mark-mode #f)]
    (check-pyret-match/check "../lang/pyret-lib/moorings.arr" _ 35))

  (check-pyret "prim-num-keys({})" (p:mk-num 0))
  (check-pyret "prim-num-keys({x:5})" (p:mk-num 1))
  (check-pyret "prim-num-keys({x:5}.{y:6})" (p:mk-num 2))
  (check-pyret "prim-num-keys({x:5}.{x:6})" (p:mk-num 1))
  (check-pyret "prim-num-keys({x:5, y:6, z:7})" (p:mk-num 3))
  (check-pyret "prim-num-keys({x(self): end, y:'', z: fun: end})" (p:mk-num 3))

  (check-pyret "'foobar'.contains('foo')" true)
  (check-pyret "'foobar'.contains('')" true)
  (check-pyret "''.contains('foo')" false)
  (check-pyret "''.contains('')" true)

  (check-pyret "'blahblah'.split('a', false) == ['bl', 'hblah']" true)
  (check-pyret "'blahblah'.split('a', true) == ['bl', 'hbl', 'h']" true)
  (check-pyret "String('blahblah'.split('a', false).first)" true)
  (check-pyret "'blahblah'.split('z', false) == ['blahblah']" true)
  (check-pyret "'blahblah'.split('z', true) == ['blahblah']" true)
  (check-pyret "String('blahblah'.split('z', false).first)" true)
  (check-pyret "'blah'.split('a', false).map(_.length()) == [2, 1]" true)

  (check-pyret "''.explode() == []" true)
  (check-pyret "'abcde'.explode() == ['a', 'b', 'c', 'd', 'e']" true)
  

  (check-pyret "gensym('foo').contains('foo')" true)
  (check-pyret "gensym('foo').length() > 3" true)
  (check-pyret "gensym('foo') <> gensym('foo')" true)
  (check-pyret "String(gensym('foo'))" true)

  (check-pyret-match/check "pyret/libs/math-libs.arr" _ 7)

  (check-pyret-match/check "pyret/libs/sets.arr" _ 177)
  (check-pyret-match/check "pyret/libs/vector.arr" _ 66)

  (check-pyret-match/check "pyret/libs/strings.arr" _ 35)

  (check-pyret "5.is-integer()" true)
  (check-pyret "5.5.is-integer()" false)
  (check-pyret "((1 / 3) * 3).is-integer()" true)
  (check-pyret "(5.5 * 2).is-integer()" true)
  (check-pyret "0.is-integer()" true)
  (check-pyret "(1.2 * -5).is-integer()" true)
  (check-pyret "(78689768976987698762538756293847569384752693487652943785.5 * 2).is-integer()" true)
  (check-pyret "(2987509274358762538756293847569384752693487652943785.5 * 2.7).is-integer()" false)

))

(define tag-tests (test-suite "tag-tests"
  (check-pyret "Function(fun: nothing end)" true)
  (check-pyret "Function(method(self): nothing end)" false)
  (check-pyret "Method(fun: nothing end)" false)
  (check-pyret "Method(method(self): nothing end)" true)
  (check-pyret "Object(method(self): nothing end)" false)
  (check-pyret "Object({})" true)
  (check-pyret "String('')" true)
  (check-pyret "String(5)" false)
  (check-pyret "Number(5)" true)
  (check-pyret "Number('str')" false)
  (check-pyret "Bool(true)" true)
  (check-pyret "Bool(false)" true)
  (check-pyret "Bool({})" false)

  (check-pyret "is-function(fun: nothing end)" true)
  (check-pyret "is-function(method(self): nothing end)" false)
  (check-pyret "is-method(fun: nothing end)" false)
  (check-pyret "is-method(method(self): nothing end)" true)
  (check-pyret "is-object(method(self): nothing end)" false)
  (check-pyret "is-object({})" true)
  (check-pyret "is-string('')" true)
  (check-pyret "is-string(5)" false)
  (check-pyret "is-number(5)" true)
  (check-pyret "is-number('str')" false)
  (check-pyret "is-bool(true)" true)
  (check-pyret "is-bool(false)" true)
  (check-pyret "is-bool({})" false)
  (check-pyret "is-mutable(mk-mutable(4, fun: end, fun: end))" true)
  (check-pyret "is-placeholder(mk-placeholder())" true)

  (check-pyret "Number(5.{ x: 'some-new-field' })" true)
  (check-pyret "String('str'.{ x: 'some-new-field' })" true)
  (check-pyret "Bool(true.{ x: 'some-new-field' })" true)
  (check-pyret "Function(fun: nothing end.{ x: 'some-new-field' })" true)
  (check-pyret "Method(method(self): nothing end.{ x: 'some-new-field' })" true)
  (check-pyret "Object({}.{ x: 'some-new-field' })" true)
))

(define conversions (test-suite "conversions"
  (check-pyret "tostring(1)" (p:mk-str "1"))
  (check-pyret "tostring(true)" (p:mk-str "true"))
  (check-pyret "tostring(false)" (p:mk-str "false"))
  (check-pyret "[1,2,3].tostring()" (p:mk-str "[1, 2, 3]"))
  (check-pyret "tostring('hello')" (p:mk-str "hello"))
  (check-pyret "tostring({a: true})" (p:mk-str "{a: true}"))
  (check-pyret "tostring({a: 5, tostring(self): 'hello!' end})"
         (p:mk-str "hello!"))
  #;(check-pyret "tostring(fun(x): x end)" (p:mk-str "fun (x): '' end"))
  #;(check-pyret "tostring(method(x): doc: 'yay' x end)" (p:mk-str "method (x): 'yay' end"))

  (check-pyret-match/check "pyret/repr.arr" _ 26)
))

(define methods (test-suite "methods"
  (check-pyret "{f(self): self.x end, x:5}.f()" five)
  (check-pyret "{f(self,y): self.x._plus(y) end, x:4}.f(6)" ten)
  (check-pyret "{f(s): s.x end, x:10}.{x:5}.f()" five)

  ;; can extract raw methods
  (check-pyret-match "3:_add" (p:p-method _ _ _ (? procedure?)))
  (check-pyret-match "{f(x): 5 end}:f" (p:p-method _ _ _ (? procedure?)))

  ;; can put raw methods on other objects and use them
  (check-pyret "var o = {x:5} var o2 = {f(self): self.x end} o := o.{g : o2:f} o.g()" five)

  ;; cannot apply raw methods (better error messages plz)
  (check-pyret-exn "3:_add()" "check-fun: expected function")
  (check-pyret
    "o = { m(self): self end }
     m = o:m
     m._fun()(5)"
    five)
  (check-pyret "o = {fff(self, x): x end} o:['f'.append('ff')]._fun()(nothing, 5)" five)
  (check-pyret "fun f(self): self.x end o = { x: 5 }.{ m : f._method() } o.m()" five)

  ;; can create raw methods and stick them on objects
  (check-pyret "var o = {x:3} o := o.{f: method(self): self.x end} o.f()"
               (p:mk-num 3))
  ;; can't apply raw methods
  (check-pyret-exn "method(self): 1 end()" "check-fun: expected function")

  (check-pyret "
o = {}
o2 = o.{ m: {m(self): self.{ called : true } end}.m}
o2.m().called" true)

  ;; methods have doc strings!
  (check-pyret "method(self): doc: 'hello' 1 end._doc" (p:mk-str "hello"))
  (check-pyret "method(self): doc: 'hello' 1 end._fun()._doc" (p:mk-str "hello"))
  (check-pyret "fun(self): doc: 'hello' 1 end._method()._doc" (p:mk-str "hello"))

  ;; tostring on methods
  #;(check-pyret-match "(method (x): x end).tostring()"
             (p:p-str _ _ _ _ "method (x): '' end"))

  #;(check-pyret-match "(method (x): doc: 'cool' x end).tostring()"
             (p:p-str _ _ _ _ "method (x): 'cool' end"))

  #;(check-pyret-match "o = {foo(x): x end} o:foo.tostring()"
               (p:p-str _ _ _ _ "method foo(x): '' end"))

  #;(check-pyret-match "o = {foo(x): doc: 'cool' x end} o:foo.tostring()"
               (p:p-str _ _ _ _ "method foo(x): 'cool' end"))


  ;; when curried, tostring() should still be the same
  #;(check-pyret-match "o = {foo(x): doc: 'cool' x end} o.foo.tostring()"
               (p:p-str _ _ _ _ "method foo(x): 'cool' end"))
  #;(check-pyret-match "o = {foo(x): doc: 'cool' x end} tostring(o.foo)"
               (p:p-str _ _ _ _ "method foo(x): 'cool' end"))

))

(define mutables (test-suite "mutable fields"
  (check-pyret-match/check "pyret/update.arr" _ 38)
  (check-pyret-match/check "pyret/placeholder.arr" _ 15)
  (check-pyret-match/check "pyret/graph.arr" _ 11)
  ))

(define user-blocks (test-suite "user blocks"
  (check-pyret-match/check "pyret/user-block.arr" _ 11)
  (check-pyret "f = block:
      var x = 0
      fun():
        x := x + 1
        x
      end
    end
    f()
    f()"
    (p:mk-num 2))))



(define exceptions (test-suite "exceptions"
  (check-pyret-exn
   "raise('error')"
   "error")

  (check-pyret-exn
   "raise(2)"
   "2")

  (check-pyret "try: raise(5) except(e): e end" five)
  (check-pyret "fun f(): raise({x:5}) end try: f() except(e): e.x end" five)
  (check-pyret "fun f(): g() end
                fun g(): raise(5) end
                fun h(): try: f() except(e): e end end
                h()" five)
  (check-pyret "fun f(): try: g() except(e): raise(e._plus(5)) end end
                fun g(): raise(5) end
                fun h(): try: f() except(e): e end end
                h()" ten)

  (check-pyret-exn "{}.[5]" "expected string, got 5")
  (check-pyret-exn "{[5]: 'foo'}" "expected string, got 5")
  (check-pyret-exn "{}.f" "f was not found")

  (check-pyret "try: raise(5) except(_): 3 end" (p:mk-num 3))
  (check-pyret-exn "try: raise(5) except(_): _ end" "undefined")

  (check-pyret "try: {}.not-a-field except(e): e.trace.length() end" (p:mk-num 1))
  (check-pyret "try: fun f(): {}.not-a-field end f() except(e): e.trace.length() end" (p:mk-num 2))

  (check-pyret "try: 1 / 0 except(e): error.is-div-0(e) end" true)
))

(define ids-and-vars (test-suite "variables and identifiers"

  (check-pyret "var x = 5 x" five)
  (check-pyret "var x = 5 var y = x y" five)

  (check-pyret "
  x = 5
  x
  " five)

  (check-pyret-exn "
  x = 5
  x := 5
  "
  "Assignment to identifier x")

  (check-pyret-exn "
  x = 5
  var x = 5
  "
  "x is defined twice")

  (check-pyret-exn "
  var x = 5
  x = 5
  "
  "x is defined twice")

  (check-pyret-exn "
  var x = 5
  fun f(x):
    nothing
  end
  "
  CONFLICT-MESSAGE)

  (check-pyret-exn "
  x = 5
  fun f(x):
    x
  end
  f(x)
  "
  "nested scope")

  (check-pyret-exn "
  x = 5
  fun f(x):
    var x = x
    x
  end
  "
  "nested scope")

  (check-pyret-exn "
  var x = 5
  try:
    nothing
  except(x):
    nothing
  end
  "
  CONFLICT-MESSAGE)

  (check-pyret-exn "
  var should_notice_method_bodies = 5
  o = { meth(self): should_notice_method_bodies = 3 nothing end }
  "
  CONFLICT-MESSAGE)

  (check-pyret-exn "
  var x = 5
  fun f():
    var x = 10
    x
  end
  f()
  "
  "nested scope")

  (check-pyret "
  x :: Number = 5
  fun f(y :: Number):
    y
  end
  f(x)
  "
  five)

  ;; two not three because side effects should happen only once
  (check-pyret "var x = 0 fun f(): x := x._add(1) x end f()._add(1)" two)

  ;(check-pyret-exn "var x = 5 var x = 10 x" "duplicate")

  ; TODO(joe): Why don't unassigned variables work the way I want?  I expect
  ; this to say "undefined identifier used before defined", but it just evaluates
  ; to <#undefined>
  ;(check-pyret-exn "var w: zoot var zoot: 5 w" "undefined")


  (check-pyret
   "var x = 5
    var y = x
    y"
   five)

  (check-pyret-exn
   "var x = 5
    var x = x
    y"
   "x is defined twice")

  ;; check behavior of _, which should always disappear
  (check-pyret
   "f = fun(_,_,_): 5 end
    f(1,2,3)"
    five)

  ;; should not trigger normal binding errors
  (check-pyret
   "var _ = 5
    _ = 4
    5"
   five)

  (check-pyret-exn
   "_ = 5
    _"
   "undefined")

  (check-pyret
   "_foo = 10
    _foo"
   (p:mk-num 10))

  (check-pyret-exn "x = 4 x = 5" "x is defined twice")
  (check-pyret-exn "x = 4 y = 6 x = 5" "x is defined twice")
  (check-pyret-exn "x = 4 fun x(): 5 end" "x is defined twice")
  (check-pyret-exn "fun x(): 4 end y = 7 x = 3" "x is defined twice")
  (check-pyret-exn "fun x(): 4 end fun x(): 3 end" "x is defined twice")
  (check-pyret-exn "var x = 3 var x = 7" "x is defined twice")
))

(define binary-operators (test-suite "binary-operators"
  (check-pyret "6 - 4 - 1" (p:mk-num 1))
  (check-pyret "6 - (4 - 1)" (p:mk-num 3))
  (check-pyret "5 + 5" (p:mk-num 10))
  (check-pyret "5 * 5" (p:mk-num 25))
  (check-pyret "5 / 5" (p:mk-num 1))
  (check-pyret "4 <= 5" (p:mk-bool #t))
  (check-pyret "4 < 5" (p:mk-bool #t))
  (check-pyret "4 >= 5" (p:mk-bool #f))
  (check-pyret "4 > 5" (p:mk-bool #f))
  (check-pyret "4 <> 5" (p:mk-bool #t))
  (check-pyret "4 == 6" (p:mk-bool #f))
  (check-pyret "fun f(y):
                  y
                end
                f(1+2)" (p:mk-num 3))
  (check-pyret "fun f(y):
                  y
                end
                f((1+2))" (p:mk-num 3))
  (check-pyret "'hello' + ' world'" (p:mk-str "hello world"))
  (check-pyret-exn "5 + 'foo'" "Bad args to prim")
  (check-pyret "x = {_lessequal(s,o): 3 end} x <= 5" (p:mk-num 3))
  (check-pyret-exn "x = {_lessthan: fun(s,o): 3 end} x < 5" "Expected 2")
  (check-pyret-exn "x = {_greaterthan: 3} x > 5" "expected method")
  (check-pyret-exn "x = {} x <= 5" "lessequal was not found")
  (check-pyret "a = 1 b = 2 (a == b) or (true)" (p:mk-bool #t))
  (check-pyret "true and false" (p:mk-bool #f))
  (check-pyret "(1 < 2) or false" (p:mk-bool #t))
  (check-pyret "((1 < 2) and true) or false" (p:mk-bool #t))
  (check-pyret "true and true and false" (p:mk-bool #f))
  (check-pyret "true or (1 > 2) or false" (p:mk-bool #t))
  (check-pyret "not true" (p:mk-bool #f))
  (check-pyret "not (1 > 2)" (p:mk-bool #t))
  (check-pyret "not (true or (1 > 2) or false)" (p:mk-bool #f))
  (check-pyret "true or raise('should be lazy')" (p:mk-bool #t))
  (check-pyret "false and raise('should be lazy')" (p:mk-bool #f))
  (check-pyret-exn "true and 5" "Bad args to prim")
  (check-pyret-exn "false or 42" "Bad args to prim")
  (check-pyret "true._and(fun: true end)" (p:mk-bool #t))
  (check-pyret "false._or(fun: true end)" (p:mk-bool #t))

))

(define ffi (test-suite "ffi"
  (check-pyret-match/check "pyret/libs/test-ast.arr" _ 10)
  (check-pyret-match/check "pyret/libs/eval.arr" _ 21)
  (check-pyret-match/check "pyret/parse-types.arr" _ 3)
  (check-pyret-match/check "../lang/racket-ffi/http.rkt" _ 5)
  (check-pyret-match/check "../lang/racket-ffi/url.rkt" _ 3)
  (check-pyret-exn "___set-link" "Unbound identifier")
  (check-pyret-exn "___set-empty" "Unbound identifier")
))

(define mixins (test-suite "mixins"
  (check-pyret-match/check "pyret/data/mixins.arr" _ 9)
))

(define currying (test-suite "currying"
  (check-pyret-match/check "pyret/currying.arr" _ 14)
))

(define nested-errors (test-suite "nested errors"
  (check-pyret-match/check "pyret/nested-errors.arr" _ 20)
))

(define checks (test-suite "checks"

  (let ()
    (check-pyret-match/check "pyret/check/check1.arr" _ 3)
    (check-pyret-match/check "pyret/check/check-method.arr" _ 3)
    (check-pyret-match/check "pyret/check/check2.arr" _ 4)
    (check-pyret-match/check "pyret/check/check3.arr" _ 36)
    (check-pyret-match/check "pyret/check/check4.arr" _ 2 1 1 0 0)
    (check-pyret-match/check "pyret/check/check-error.arr" _ 2 1 1 0 1)
    (check-pyret-match/check "pyret/check/check-error2.arr" _ 5 2 3 0 1)
    (check-pyret-match/check "pyret/check/check-error3.arr" _ 4 3 1 0 1)
    (check-pyret-match/check "pyret/check/check-error4.arr" _ 2 1 1 0 0)
    (check-pyret-match/check "pyret/check/check-in-pred-ann.arr" _ 1)
    (check-pyret-match/check "pyret/check/check-identifier-after.arr" _ 1)
    (check-pyret-match/check "pyret/check/nested-called-twice.arr" _ 2)

    (check-pyret-match/check "pyret/check/check-data1.arr" _ 1)
    (check-pyret-match/check "pyret/check/check-data2.arr" _ 2 1 1 0 0)
    (check-pyret-match/check "pyret/check/check-data3.arr" _ 3 2 1 0 0)
    (check-pyret-match/check "pyret/check/check-data4.arr" _ 2)
    (check-pyret-match/check "pyret/check/check-with-import.arr" _ 1)
    (check-pyret-match/check "pyret/check/check-is.arr" _ 3 2 1 0 0))

    (check-pyret-match/check "pyret/check/standalone.arr" _ 4 2 2 0 0)

    (check-pyret-match/check "pyret/check/raises.arr" _ 5 3 2 0 0)

    (check-pyret-match/check "pyret/check/satisfies.arr" _ 2)

    (check-pyret-match/check "pyret/errors/arity.arr" _ 11)

))

;; NOTE(dbp): private-run just means that it won't fail if the file is
;; missing. some of these should always be present.
(define examples (test-suite "examples"
  (let
    ([example-path (lambda (sub) (build-path "../../examples/" sub))]
     [private-run (lambda (filename passing)
      (printf "Running : ~a\n" filename)
      (when (file-exists? filename)
       (define-values (base name _) (split-path (simplify-path filename)))
       (parameterize [(current-directory base)]
         (check-pyret-match/check name _ passing))))])

    (check-pyret-match/check "pyret/semis-examples.arr" _ 11)

    (private-run (example-path "queue.arr") 9)
    (private-run (example-path "point.arr") 5)
    (private-run (example-path "ralist.arr") 110)

    ;; NOTE(dbp): just syntax checking, no tests, for now.
    (private-run (example-path "htdp/arithmetic.arr") 0)
    (private-run (example-path "htdp/boolean.arr") 0)
    (private-run (example-path "htdp/conversions.arr") 0)
    (private-run (example-path "htdp/def.arr") 0)
    (private-run (example-path "htdp/more-arithmetic.arr") 0)
    (private-run (example-path "htdp/string.arr") 0)
    (private-run (example-path "htdp/struct.arr") 3)
    (private-run (example-path "htdp/types.arr") 0)

    (private-run (example-path "object-patterns/mixin-java.arr") 15)
  (private-run (example-path "object-patterns/mixin-class.arr") 3)
  (private-run (example-path "object-patterns/mixin-extend.arr") 2)
  (private-run (example-path "object-patterns/mixin-seal.arr") 3)
  (private-run (example-path "object-patterns/mixin-mutable.arr") 2)
  (let ()
    (private-run "../../examples/pyret-lang-private/cs019/sortacle.arr" 2)
    (private-run "../../examples/pyret-lang-private/cs019/filesystem.arr" 2)
    (private-run "../../examples/pyret-lang-private/cs019/seam-carving.arr" 34)
    (private-run "../../examples/pyret-lang-private/cs019/simple-updater.arr" 14)))
    ))

(define annotations (test-suite "annotations"
  (check-pyret-match/check "pyret/annotations/arrow.arr" _ 4)
  (check-pyret-match/check "pyret/annotations/app.arr" _ 2)
  (check-pyret-match/check "pyret/annotations/pred.arr" _ 1)
  (check-pyret-match/check "pyret/annotations/record.arr" _ 1)
))

(define all (test-suite "all"
  constants
  functions
  brands
  cases
  data
  modules
  conversions
  tag-tests
  built-in-libraries
  for-block
  user-blocks
  methods
  mutables
  exceptions
  ids-and-vars
  binary-operators
  ffi
  mixins
  currying
  nested-errors
  checks
  examples
  annotations))

(run-tests all 'normal)


;; TODO(joe): decide on the shape of exceptions for builtins
#;(check-pyret "try: {}.x except(e): builtins.is-exception(e)" true)
#;(check-pyret "try: {}() except(e): builtins.is-exception(e)" true)
#;(check-pyret "try: fun(x) -> (x).x except(e): builtins.is-exception(e) end" true)
