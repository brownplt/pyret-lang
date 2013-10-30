#lang racket/base

(require
  rackunit
  racket/file
  racket/match
  racket/set
  racket/string
  pyret/lang/ast
  pyret/lang/load)

(define (not-equiv-ast e1 e2) (not (equiv-ast e1 e2)))
(define-binary-check (check-equiv-ast equiv-ast actual expected))
(define-binary-check (check-not-equiv-ast not-equiv-ast actual expected))

(check-equiv-ast
  (subst (parse-stmt "x") 'x (parse-stmt "5"))
  (parse-stmt "5"))

(check-equiv-ast
  (subst
    (parse-stmt "datatype D: | foo(a) with constructor(self): self + x;;")
    'x
    (parse-stmt "5"))
  (parse-stmt "datatype D: | foo(a) with constructor(self): self + 5;;"))

(check-equiv-ast
  (subst
    (parse-stmt "datatype D: | foo(a :: X(pred)) with constructor(self): self;;")
    'pred
    (parse-stmt "fun(x): false;"))
  (parse-stmt "datatype D: | foo(a :: X(fun(x): false;)) with constructor(self): self;;"))

(check-equiv-ast
  (subst
    (parse-stmt "data D: | foo(a :: X(pred));")
    'pred
    (parse-stmt "fun(x): false;"))
  (parse-stmt "data D: | foo(a :: X(fun(x): false;));"))

(check-exn
  #rx"Substitution into annotation names"
  (lambda () (subst (parse-stmt "x :: ANAME = 5") 'ANAME (parse-stmt "22"))))
(check-exn
  #rx"Substitution into annotation names"
  (lambda () (subst (parse-stmt "var x :: ANAME = 5") 'ANAME (parse-stmt "22"))))
(check-exn
  #rx"Substitution into annotation names"
  (lambda () (subst (parse-stmt "fun(x :: ANAME): 5;") 'ANAME (parse-stmt "22"))))

(define simple-for (parse-stmt "for each(i :: A(x) from y): print(i);"))

(check-equiv-ast
  (subst simple-for 'x (parse-stmt "5"))
  (parse-stmt "for each(i :: A(5) from y): print(i);"))

(check-equiv-ast
  (subst simple-for 'y (parse-stmt "5"))
  (parse-stmt "for each(i :: A(x) from 5): print(i);"))

(check-equal?
  (free-ids (parse-pyret "
    datatype D: | foo with constructor(self): self end end
    is-foo(foo)"))
  (set))

(define datatype-variants
    "datatype Foo:
      | foo() with constructor(self): self end
      | bar(a) with constructor(self): self end
     end
     Foo(bar(foo()))")

(check-equal?
  (free-ids (parse-pyret datatype-variants))
  (set))

(define datatypeT "datatype Foo<T>: | foo() with constructor(self): self + x;;")

(check-equal?
  (free-ids (parse-pyret datatypeT))
  (set 'x))

(check-equal?
  (free-ids (parse-pyret "x = y y = x"))
  (set))

(check-equal?
  (free-ids (parse-pyret "fun f(): f where: f end"))
  (set))

(check-equal?
  (free-ids (parse-pyret "
  p = mk-placeholder()
  try: p.get() except(e): test-print(e.message) end
  p.set(5)
  p.get()"))
  (set 'test-print 'mk-placeholder))

(check-equal?
  (free-ids (parse-pyret "var x = 3 fun: x := 5;() x"))
  (set))

(check-equal?
  (free-ids (parse-pyret "fun f(): f where: g end"))
  (set 'g))

(check-equal?
  (free-ids (parse-pyret "fun f(x :: Number(p)): f where: g end"))
  (set 'Number 'p 'g))

(check-equal?
  (free-ids (parse-pyret "provide a end"))
  (set 'a))

(check-equal?
  (free-ids (parse-pyret "fun (x :: Number(z)): x + y end"))
  (set 'Number 'z 'y))

(check-equal?
  (free-ids (parse-pyret "
graph:
x = [y, z]
y = [z]
z = [x, fun(): f end]
end"))
  (set 'f))

(check-equal?
  (free-ids (parse-pyret "x :: o.foo = 5"))
  (set 'o))

(check-equal?
  (free-ids (parse-pyret "cases(List) o: | bar(x :: Number(foo)) => x + zed end"))
  (set 'List 'o 'Number 'foo 'zed))

(check-equal?
  (free-ids (parse-pyret "data D: | var1 end D var1"))
  (set))

(check-true (set? (free-ids (parse-pyret (file->string "../lang/pyret-lib/moorings.arr")))))

(check-equiv-ast (parse-pyret "x = 5") (parse-pyret "   x = 5"))

(check-equiv-ast (parse-pyret "x = 5 y = 10" "some-filename")
  (parse-pyret "x = 5



  y = 10" "another-filename"))


(define equiv1 #<<EOF
fun equiv(obj1, obj2):
  doc: "Check if two objects are equal via an _equals method, or
        have all the same keys with equiv fields"
  if Opaque(obj1) or Opaque(obj2):
    false
  else:
    fun all_same(o1, o2):
      if Method(o1) or Function(o1):
        false
      else:
        left_keys = keys(o1)
        for fold(same from true, key from left_keys):
          if not (has-field(o2, key)): false
          else:
            left_val = o1:[key]
            right_val = o2:[key]
            same and equiv(left_val, right_val)
          end
        end
      end
    end
    if has-field(obj1, "_equals"):
      obj1._equals(obj2)
    else if num-keys(obj1)._equals(num-keys(obj2)):
      all_same(obj1, obj2)
    else:
      false
    end
  end
where:
  eq = checkers.check-equals
  eq("empty objects", equiv({}, {}), true)
  eq("",equiv({x : 5}, {y : 6}), false)
  eq("",equiv({x : 5}, {x : 6}), false)
  eq("",equiv({x : 5}, {x : 5}), true)
  eq("",equiv({x : 5, y : 6}, {y : 6, x : 5}), true)
  eq("",equiv({x : {z: "foo"}, y : 6}, {y : 6, x : {z: "foo"}}), true)
  eq("",equiv({x : {z: "foo"}, y : [true, 6]}, {y : [true, 6], x : {z: "foo"}}), true)
  eq("",equiv(fun: end, fun: end), false)

  f = fun: end
  eq("functions in objects aren't ever equal", equiv({my_fun:f}, {my_fun:f}), false)
  m = method(self): end
  eq("methods in objects aren't ever equal", equiv({my_meth:m}, {my_meth:m}), false)

  eq("lists of objects", equiv([{}], [{}]), true)
  eq("lists of prims", equiv([5], [5]), true)
end
EOF
)

(define equiv2 #<<EOF
fun equiv(obj1,     obj2):
  doc: "Check if two objects are equal via an _equals method, or
        have all the same keys with equiv fields"




  if Opaque(obj1) or Opaque(obj2):



    false
  else:
    fun all_same(o1, o2):
      if Method(o1)     or Function(o1):
        false
      else:
        left_keys = keys(o1)
        for fold(same from true, key from left_keys):


          if not (has-field(o2, key)): false

          
else:
            left_val = o1:[key]
            right_val = o2:[key]
            same and equiv(left_val, right_val)
          end

        end
      end
    end
    if has-field(obj1,            "_equals"):
      obj1._equals(obj2)


    else if num-keys(obj1)._equals(num-keys(obj2)):   
      all_same(obj1, obj2)
    else:   
      

false
    end
  end
where:
  eq = checkers.check-equals
  eq("empty objects", equiv({}, {}), true)
  eq("",equiv({x : 5}, {y : 6}), false)
  eq("",equiv({x : 5}, {x : 6}), false)
  eq("",equiv({x : 5}, {x : 5}), true)
  eq("",equiv({x : 5, y : 6}, {y : 6, x : 5}), true)
  eq("",equiv({x : {z: "foo"}, y : 6}, {y : 6, x : {z: "foo"}}), true)
  eq("",equiv({x : {z: "foo"}, y : [true, 6]},    {y : [true, 6], x : {z: "foo"}}), true)
  eq("",equiv(fun: end, fun: end), false)

  f = fun: end
  eq("functions in objects aren't ever equal"   , equiv({my_fun:f}, {my_fun:    f}), false)
  m = method(self): end
  eq("methods in objects aren't ever equal", equiv({my_meth    :m}, {my_meth:m}), false)



  eq("lists of objects",   equiv([{  }], [{  }]), true)
  eq("lists of prims", equiv([5], [5]), true)
end
EOF
)

(check-equiv-ast (parse-pyret equiv1) (parse-pyret equiv2))


(define list1 #<<EOF
data List:
  | empty with:

    length(self) -> Number: 0 end,

    each(self, f :: (Any -> Nothing)) -> Nothing: nothing end,

    map(self, f :: (Any -> Any)) -> List: empty end,

    filter(self, f :: (Any -> Bool)) -> List: empty end,

    find(self, f :: (Any -> Bool)) -> Option: none end,

    partition(self, f): { is-true: empty, is-false: empty } end,

    foldr(self, f, base): base end,

    foldl(self, f, base): base end,

    member(self, elt): false end,

    append(self, other): other end,

    last(self): raise('last: took last of empty list') end,

    take(self, n): take-help(self, n) end,

    drop(self, n): drop-help(self, n) end,

    reverse(self): self end,

    get(self, n): get-help(self, n) end,

    set(self, n, e): set-help(self, n, e) end,

    _equals(self, other): is-empty(other) end,

    tostring(self): "[]" end,

    _torepr(self): "[]" end,

    sort-by(self, cmp, eq): self end,

    sort(self): self end,

    join-str(self, str): "" end

  | link(cyclic first :: Any, cyclic rest :: List) with:

    length(self): 1 + self.rest.length() end,

    each(self, f):
      f(self.first)
      self.rest.each(f)
    end,

    map(self, f): f(self.first)^link(self.rest.map(f)) end,

    filter(self, f):
      if f(self.first): self.first^link(self.rest.filter(f))
      else:             self.rest.filter(f)
      end
    end,

    partition(self, f): partition(f, self) end,

    find(self, f): find(f, self) end,

    member(self, elt): (elt == self.first) or self.rest.member(elt) end,

    foldr(self, f, base): f(self.first, self.rest.foldr(f, base)) end,

    foldl(self, f, base): self.rest.foldl(f, f(self.first, base)) end,

    append(self, other): self.first^link(self.rest.append(other)) end,

    last(self):
      if is-empty(self.rest): self.first
      else: self.rest.last()
      end
    end,

    reverse(self): reverse-help(self, empty) end,

    take(self, n): take-help(self, n) end,

    drop(self, n): drop-help(self, n) end,

    get(self, n): get-help(self, n) end,

    set(self, n, e): set-help(self, n, e) end,

    _equals(self, other):
      if is-link(other):
        others-equal = (self:first == other:first)
        others-equal and (self:rest == other:rest)
      else:
        false
      end
    end,

    tostring(self):
      "[" +
        for raw-fold(combined from tostring(self:first), elt from self:rest):
          combined + ", " + tostring(elt)
        end
      + "]"
    end,

    _torepr(self):
      "[" +
        for raw-fold(combined from torepr(self:first), elt from self:rest):
          combined + ", " + torepr(elt)
        end
      + "]"
    end,

    sort-by(self, cmp, eq):
      doc: "Takes a comparator to check for elements that are strictly greater
        or less than one another, and an equality procedure for elements that are
        equal, and sorts the list accordingly."
      pivot = self.first
      less = self.filter(fun(e): cmp(e,pivot) end).sort-by(cmp, eq)
      equal = self.filter(fun(e): eq(e,pivot) end)
      greater = self.filter(fun(e): cmp(pivot,e) end).sort-by(cmp, eq)
      less.append(equal).append(greater)
    end,

    sort(self):
      self.sort-by(fun(e1,e2): e1 < e2 end, fun(e1,e2): e1 == e2 end)
    end,

    join-str(self, str):
      if is-link(self.rest):
         tostring(self.first) + str + self.rest.join-str(str)
      else:
         tostring(self.first)
      end
    end

sharing:
  push(self, elt):
    doc: "Adds an element to the front of the list, returning a new list"
    link(elt, self)
  end,
  _plus(self :: List, other :: List): self.append(other) end,
  to-set(self :: List): list-to-set(self) end

where:
  eq = checkers.check-equals

  eq("list set", [1,2,3].set(1, 5), [1,5,3])

  o1 = {
    _lessthan(self, other): self.x < other.x end,
    _equals(self, other): self.x == other.x end,
    x: 5
  }
  o2 = o1.{
    _lessthan(self, other): self.x < other.x end,
    _equals(self, other): self.x == other.x end,
    x: 10
  }
  [o2, o1].sort() is [o1, o2]
end

EOF
)

(check-equiv-ast (parse-pyret list1) (parse-pyret list1))

(check-false (equiv-ast (parse-pyret "x = 10") (parse-pyret "x :: Any = 11")))
(check-false (equiv-ast (parse-pyret "import 'file' as f") (parse-pyret "import file as f")))

(define d1 "data D: |var1 end")
(define d2 "data D: |var1(x) end")

(check-false (equiv-ast (parse-pyret d1) (parse-pyret d2)))

(define o1 "{x:5}")
(define o2 "{x(self):5;}")

(check-false (equiv-ast (parse-pyret o1) (parse-pyret o2)))

(define f1 "fun(x): end")
(define f2 "fun(x, y): end")

(check-false (equiv-ast (parse-pyret f1) (parse-pyret f2)))

(define b1 "block: 4 5 end")
(define b2 "block: 4 5 6 end")

(check-false (equiv-ast (parse-pyret b1) (parse-pyret b2)))

(define moorings-lines (file->lines "../lang/pyret-lib/moorings.arr"))

(check-equiv-ast (parse-pyret (string-join moorings-lines "\n") "moorings")
                 (parse-pyret (string-join moorings-lines "\n") "another-filename"))

(check-equiv-ast (parse-pyret datatypeT) (parse-pyret datatypeT))

(define not-datatypeT "datatype Foo<T>: | foo() with constructor(bar): bar + x;;")
(check-not-equiv-ast (parse-pyret datatypeT) (parse-pyret not-datatypeT))

(define not-datatype-variants 
    "datatype Foo:
      | foo() with constructor(z): z end
      | bar(a) with constructor(self): self end
     end
     Foo(bar(foo()))")
(check-not-equiv-ast (parse-pyret datatype-variants) (parse-pyret not-datatype-variants))

