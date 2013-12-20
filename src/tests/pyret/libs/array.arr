#lang pyret

check:
  a1 = array([1, 2, 3])

  a1.get(-1) raises "negative"
  a1.get(1 / 2) raises "non-integer"

  non-nums = [true, false, "not-a-num", {}, [], fun(): end, method(self): end]
  for each(n from non-nums): a1.get(n) raises "expected Number";
end

check:
  a1 = build-array(fun(i): i * i end, 6)
  a1.to-list() is [0, 1, 4, 9, 16, 25]
  a2 = for build-array(i from 7):
    (i * i) - i
  end
  a2.to-list() is [0, 0, 2, 6, 12, 20, 30]
end

check:
  fun negate(f): fun(x): not f(x);;

  a1 = array([1, 2, 3])
  a1.length() is 3

  a1 is a1
  a1 satisfies identical(_, a1)

  a2 = array([1, 2, 3])
  a2.length() is 3

  a1 satisfies _ <> a2
  a1.to-list() is a2.to-list()
  array-to-list(a1) is a1.to-list()

  a1 satisfies negate(identical(_, a2))
  a2 satisfies negate(identical(_, a1))

  a3 = array([4, "a", 6])
  a3.length() is 3

  a1.to-list() satisfies _ <> a3.to-list()
  a3.to-list() satisfies _ <> a1.to-list()

end

check:

  a1 = array([1, 2, 3])

  a1 satisfies is-array

  a1.get(0) is 1
  a1.get(1) is 2
  a1.get(2) is 3
  a1.get(3) raises "too large"

  a1.set(1, 5) satisfies identical(_, a1)
  a1.get(1) is 5

  a1.set(1, 3).set(2, 6) satisfies identical(_, a1)
  a1.set(1, 3).set(2, 6) is a1
  a1.get(1) is 3
  a1.get(2) is 6

  a1.length() is 3

end

check:
  a1 = array([1, 2, 3])

  array-get(a1, 0) is 1
  array-get(a1, 1) is 2
  array-get(a1, 2) is 3
  array-get(a1, 3) raises "too large"

  array-set(a1, 1, 5) satisfies identical(_, a1)
  array-get(a1, 1) is 5

  array-set(a1, 1, 3).set(2, 6) satisfies identical(_, a1)
  array-get(a1, 1) is 3
  array-get(a1, 2) is 6

  array-length(a1) is 3
end

check:
  a = array([])
  a.length() is 0
  a.get(0) raises "too large"
  a.get(1) raises "too large"

  a.set(0, "val") raises "too large"
end

check:
  a1 = array-of("init", 5)

  a1 satisfies is-array

  array-length(a1) is 5
  array-get(a1, 5) raises "too large"
  a2 = array(["init","init","init","init","init"])

  a1.to-list() is a2.to-list()
  array-set(a1, 0, "update")
  a1 satisfies _ <> a2
  array-get(a1, 0) is "update"
  for each(i from range(1, 4)): array-get(a1, i) is "init";

end

check:
  torepr(array([1,2,"3"])) is 'array([1, 2, "3"])'
  torepr(array([])) is 'array([])'
  torepr(array-of(3, 3)) is 'array([3, 3, 3])'
end

check:
  fun f(v :: Array<Number>): v;
  f([]) raises "expected Array"
  f(array([])).to-list() is []
  f(array([])) satisfies is-array
end

check:
  data D:
    | single
    | multi(a, b)
  end
  a = array([single, multi(1, "2")])
  torepr(a) is "array([single, multi(1, \"2\")])"
  a.set(0, a.get(1)).to-list() is [multi(1, "2"), multi(1, "2")]
  a.get(0) satisfies identical(_, a.get(1))
  a.length() is 2
end

