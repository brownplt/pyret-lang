#lang pyret

import equality as E

check "numbers":
  identical(4/5, ~0.8) is false
  identical3(4/5, ~0.8) satisfies E.is-NotEqual
  identical(4/5, 4/5) is true
  identical(4/5, 8/10) is true
  identical(4/5, 4/10 + 4/10) is true
  identical(4/5, 3/5) is false
  identical(1, 1) is true
  identical(1, 2) is false
  equal-always(1, 1) is true
  equal-always(1, 2) is false
  equal-always3(4/5, ~0.8) satisfies E.is-Unknown
  equal-now(1, 1) is true
  equal-now(1, 2) is false
  equal-always3(~3, ~3) satisfies E.is-Unknown
  roughly-equal(~3, ~3) is true
  roughly-equal(~3, ~3.000003) is true
  roughly-equal(~3, ~4) is false
  roughly-equal(~3, ~3.00004) is false
  roughly-equal(3, 3) is true
  roughly-equal(3, 3.000003) is true
  roughly-equal-now(3, 3.000003) is true
  roughly-equal-now(3, 3.000004) is false
  roughly-equal-now3(3, 3.000003) satisfies E.is-Equal
  roughly-equal-now3(3, 3.000004) satisfies E.is-NotEqual
  roughly-equal-always(3, 3.000003) is true
  roughly-equal-always(3, 3.000004) is false
  roughly-equal-always3(3, 3.000003) satisfies E.is-Equal
  roughly-equal-always3(3, 3.000004) satisfies E.is-NotEqual
end

data Nat:
  | Z
  | S(n)
end

check "datatypes (no ref)":
  identical(Z, Z) is true
  identical(Z, S(Z)) is false
  identical(S(Z), S(Z)) is false
  equal-always(Z, Z) is true
  equal-always(Z, S(Z)) is false
  equal-always(S(Z), S(Z)) is true
  equal-now(Z, Z) is true
  equal-now(Z, S(Z)) is false
  equal-now(S(Z), S(Z)) is true
end

check "functional extension":
  identical({x:1, y:2}.{y:3}, {x:1, y:3}) is false
  equal-always({x:1, y:2}.{y:3}, {x:1, y:3}) is true
  equal-now({x:1, y:2}.{y:3}, {x:1, y:3}) is true
  {x:1, y:2}.{y:3} is {x:1, y:3}
  {x:1, y:2}.{y:3} is-not<=> {x:1, y:3}
  {x:1, y:2}.{y:3} is== {x:1, y:3}
  {x:1, y:2}.{y:3} is=~ {x:1, y:3}
end

data Box:
  | box(ref v)
end

check "datatypes (with ref)":
  x = box(5)
  y = box(5)
  identical(x, x) is true
  identical(x, y) is false
  equal-always(x, x) is true
  equal-always(x, y) is false
  equal-now(x, x) is true
  equal-now(x, y) is true
end

data MLink:
  | mlink(ref n, ref l)
  | mempty
end

# Sets
# Lists
# Trees

check "sets":
  s1 = [tree-set: 1, 2, 3]
  s2 = [tree-set: 2, 1, 3]
  s3 = [tree-set: 1, 2, 5]

  identical(s1, s2) is false
  identical(s1, s3) is false
  equal-always(s1, s2) is true
  equal-always(s1, s3) is false
  equal-now(s1, s2) is true
  equal-now(s1, s3) is false
end

check "lists":
  l1 = [list: 1, 2, 3]
  l2 = [list: 1, 2, 3]
  l3 = [list: 3, 1, 2]

  identical(l1, l2) is false
  identical(l1, l3) is false
  equal-always(l1, l2) is true
  equal-always(l1, l3) is false
  equal-now(l1, l2) is true
  equal-now(l1, l3) is false
end

eq-all = { method _equals(_, _, _): E.Equal end }
eq-none = { method _equals(_, _, _): E.NotEqual("just because", 0, 1) end }

check "identical pre-check overrides method in true case, but not in false case":
  identical(eq-none, eq-none) is true
  equal-always(eq-none, eq-none) is true
  equal-now(eq-none, eq-none) is true

  identical(eq-all, eq-none) is false
  eq-all is-not<=> eq-none
  identical(eq-none, eq-all) is false
  eq-all is-not<=> eq-none

  equal-always(eq-all, eq-none) is true
  eq-all is== eq-none
  equal-always(eq-none, eq-all) is false
  eq-none is-not== eq-all
  equal-now(eq-all, eq-none) is true
  eq-all is=~ eq-none
  equal-now(eq-none, eq-all) is false
  eq-none is-not=~ eq-all
end

f-func-err = "Functions"
f-meth-err = "Methods"
f = lam(): "no-op" end
m = method(self): "no-op" end

check "eq-all is not equal to values with different tags":
  eq-all is-not== f
  eq-all is-not== m
  eq-all is-not== 0
  eq-all is-not== "a"
  eq-all is-not== nothing
  eq-all is-not== true
  eq-all is-not== [list: 1, 2, 3]
  eq-all is== eq-none
end

check "eq-none is equal nothing never always":
  eq-none is-not== f
  eq-none is-not== m
  eq-none is-not== 0
  eq-none is-not== "a"
  eq-none is-not== nothing
  eq-none is-not== true
  eq-none is-not== [list: 1, 2, 3]
  eq-none is-not== eq-all
end

check "error on bare fun and meth and roughnum":
  identical(f, f) raises f-func-err
  equal-always(f, f) raises f-func-err
  equal-now(f, f) raises f-func-err

  identical(m, m) raises f-meth-err
  equal-always(m, m) raises f-meth-err
  equal-now(m, m) raises f-meth-err

  identical(1, ~1) is false
  equal-always(1, ~1) raises "Roughnum"
  equal-now(1, ~1) raises "Roughnum"

  identical(~1, 1) is false
  equal-always(~1, 1) raises "Roughnum"
  equal-now(~1, 1) raises "Roughnum"
end

check "error (and non-error) on nested fun and meth":
  o1 = {f: f, x: 5}
  o2 = {f: f, x: 5}
  o3 = {f: f, x: 6}

  equal-always(o1, o2) raises f-func-err
  equal-always(o2, o1) raises f-func-err

  equal-now(o1, o2) raises f-func-err
  equal-now(o2, o1) raises f-func-err

  equal-always(o1, o3) is false
  o1 is-not== o3
  equal-always(o3, o1) is false
  o3 is-not== o1

  equal-now(o1, o3) is false
  o1 is-not=~ o3
  equal-now(o3, o1) is false
  o3 is-not=~ o1

  o4 = { subobj: eq-none, f: f }
  o5 = { subobj: eq-all, f: f }

  equal-always(o4, o5) is false
  o4 is-not== o5
  equal-always(o5, o4) raises f-func-err

  equal-now(o4, o5) is false
  o4 is-not=~ o5
  equal-now(o5, o4) raises f-func-err
end


check:
  h = lam(): "no-op" end

  var called = false
  var long-equals = {}
  long-equals := {
    method _equals(_, _, eq) block:
      for each(i from range(0, 10000)):
        i + i
      end
      if called block:
        E.Equal
      else:
        called := true
        eq([list: h, long-equals], [list: h, {}])
      end
    end
  }

  s1 = [list: 1, long-equals, 3, 4]
  s2 = [list: 1, {}, 3, 4]

  equal-always(s1, s2) raises "Functions"
end

check "non-equality result from equals":
  o = { method _equals(_, _, _): true end }
  o == {} raises "EqualityResult"
end

check "https://github.com/brownplt/pyret-lang/issues/896":
  var firsteq = nothing
  var secondeq = nothing
  
  o = {
    method _equals(self, other, eq) block:
      v1 = {}
      v2 = {x:"a"}
      firsteq := eq([list: v1], [list: v2])
      secondeq := eq(v1, v2)
      eq(1, 1)
    end
  }
  o == {} is true
  firsteq satisfies E.is-NotEqual
  secondeq satisfies E.is-NotEqual
end

check "https://github.com/brownplt/pyret-lang/issues/896":
  var firsteq = nothing
  var secondeq = nothing
  
  o = {
    method _equals(self, other, eq) block:
      v1 = {}
      v2 = {x:"a"}
      firsteq := eq(v1, v2)
      secondeq := eq(v1, v2)
      eq(1, 1)
    end
  }
  o == {} is true
  firsteq satisfies E.is-NotEqual
  secondeq satisfies E.is-NotEqual
end

check "https://github.com/brownplt/pyret-lang/issues/895":
  list-to-set([list:
    [list: "top","mid-a"],
    [list: "top","mid-b","low-b-a"],
    [list: "top","mid-b"]]) is-not
  list-to-set([list:
    [list: [list: "top","mid-a"]],
    [list: "top","mid-b"],
    [list: "top","mid-b","low-b-a"]])
end
