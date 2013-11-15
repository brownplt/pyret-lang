#lang pyret

check:
  v1 = vector([1, 2, 3])

  v1.get(-1) raises "negative"
  v1.get(1 / 2) raises "non-integer"

  non-nums = [true, false, "not-a-num", {}, [], fun(): end, method(self): end]
  for each(n from non-nums): v1.get(n) raises "expected Number";
end

check:
  fun negate(f): fun(x): not f(x);;

  v1 = vector([1, 2, 3])
  v1.length() is 3

  v1 is v1
  v1 satisfies identical(_, v1)

  v2 = vector([1, 2, 3])
  v2.length() is 3

  v1 satisfies _ <> v2
  v1.to-list() is v2.to-list()
  vector-to-list(v1) is v1.to-list()

  v1 satisfies negate(identical(_, v2))
  v2 satisfies negate(identical(_, v1))

  v3 = vector([4, "a", 6])
  v3.length() is 3

  v1.to-list() satisfies _ <> v3.to-list()
  v3.to-list() satisfies _ <> v1.to-list()

end

check:

  v1 = vector([1, 2, 3])

  v1 satisfies is-vector

  v1.get(0) is 1
  v1.get(1) is 2
  v1.get(2) is 3
  v1.get(3) raises "too large"

  v1.set(1, 5) satisfies identical(_, v1)
  v1.get(1) is 5

  v1.set(1, 3).set(2, 6) satisfies identical(_, v1)
  v1.set(1, 3).set(2, 6) is v1
  v1.get(1) is 3
  v1.get(2) is 6

  v1.length() is 3

end

check:
  v1 = vector([1, 2, 3])

  vector-get(v1, 0) is 1
  vector-get(v1, 1) is 2
  vector-get(v1, 2) is 3
  vector-get(v1, 3) raises "too large"

  vector-set(v1, 1, 5) satisfies identical(_, v1)
  vector-get(v1, 1) is 5

  vector-set(v1, 1, 3).set(2, 6) satisfies identical(_, v1)
  vector-get(v1, 1) is 3
  vector-get(v1, 2) is 6

  vector-length(v1) is 3
end

check:
  v = vector([])
  v.length() is 0
  v.get(0) raises "too large"
  v.get(1) raises "too large"

  v.set(0, "val") raises "too large"
end

check:
  v1 = const-vector("init", 5)

  v1 satisfies is-vector

  vector-length(v1) is 5
  vector-get(v1, 5) raises "too large"
  v2 = vector(["init","init","init","init","init"])

  v1.to-list() is v2.to-list()
  vector-set(v1, 0, "update")
  v1 satisfies _ <> v2
  vector-get(v1, 0) is "update"
  for each(i from range(1, 4)): vector-get(v1, i) is "init";

end

check:
  torepr(vector([1,2,"3"])) is 'vector([1, 2, "3"])'
  torepr(vector([])) is 'vector([])'
  torepr(const-vector(3, 3)) is 'vector([3, 3, 3])'
end

check:
  fun f(v :: Vector<Number>): v;
  f([]) raises "expected Vector"
  f(vector([])).to-list() is []
  f(vector([])) satisfies is-vector
end

check:
  data D:
    | single
    | multi(a, b)
  end
  v = vector([single, multi(1, "2")])
  torepr(v) is "vector([single, multi(1, \"2\")])"
  v.set(0, v.get(1)).to-list() is [multi(1, "2"), multi(1, "2")]
  v.get(0) satisfies identical(_, v.get(1))
  v.length() is 2
end

