#lang pyret


fun negate(f): lam(x): not(f(x));;

identical = (_ == _)

check:
  a1 = [array: 1, 2, 3]

  a1.get(-1) raises "negative"
  a1.get(1 / 2) raises "integer"

  non-nums = [list: true, false, "not-a-num", {}, [list: ], lam(): 5 end, method(self): 10 end]
  for each(n from non-nums): a1.get(n) raises "Number";
end

check:
  a1 = build-array(lam(i): i * i end, 6)
  a1.to-list() is [list: 0, 1, 4, 9, 16, 25]
  a2 = for build-array(i from 7):
    (i * i) - i
  end
  a2.to-list() is [list: 0, 0, 2, 6, 12, 20, 30]
end

check:
  a1 = [array: 1, 2, 3]
  a1.length() is 3

  a1 is a1
  a1 satisfies identical(_, a1)

  a2 = [array: 1, 2, 3]
  a2.length() is 3

  a1 satisfies _ <> a2
  a1.to-list() is a2.to-list()

  a1 satisfies negate(identical(_, a2))
  a2 satisfies negate(identical(_, a1))

  a3 = [array: 4, "a", 6]
  a3.length() is 3

  a1.to-list() satisfies _ <> a3.to-list()
  a3.to-list() satisfies _ <> a1.to-list()

end

check:

  a1 = [array: 1, 2, 3]

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
  a1 = raw-array-of(0, 3)
  raw-array-set(a1, 0, 1)
  raw-array-set(a1, 1, 2)
  raw-array-set(a1, 2, 3)

  raw-array-get(a1, 0) is 1
  raw-array-get(a1, 1) is 2
  raw-array-get(a1, 2) is 3
  raw-array-get(a1, 3) raises "too large"

  raw-array-set(a1, 1, 5) satisfies identical(_, a1)
  raw-array-get(a1, 1) is 5

  raw-array-set(a1, 1, 3) ^ raw-array-set(_, 2, 6) satisfies identical(_, a1)
  raw-array-get(a1, 1) is 3
  raw-array-get(a1, 2) is 6

  raw-array-length(a1) is 3
end

check:
  a = [array: ]
  a.length() is 0
  a.get(0) raises "too large"
  a.get(1) raises "too large"

  a.set(0, "val") raises "too large"
end

check:
  a1 = raw-array-of("init", 5)

  a1 satisfies is-raw-array

  raw-array-length(a1) is 5
  raw-array-get(a1, 5) raises "too large"
  a2 = [array: "init","init","init","init","init"]

  raw-array-to-list(a1) is a2.to-list()
  raw-array-set(a1, 0, "update")
  a1 satisfies _ <> a2
  raw-array-get(a1, 0) is "update"
  for each(i from range(1, 4)): raw-array-get(a1, i) is "init";

end

check:
  torepr([array: 1,2,"3"]) is '[array: 1, 2, "3"]'
  torepr([array: ]) is '[array: ]'
#  torepr(raw-array-of(3, 3)) is '[array: 3, 3, 3]'
end

check:
  fun f(v :: Array<Number>):
    when not(is-array(v)): raise("not an Array") end
    v
  end
  f([list: ]) raises "Array"
  f([array: ]).to-list() is [list: ]
  f([array: ]) satisfies is-array
end

check:
  a1 = raw-array-of(3, 3)
  a2 = raw-array-of(3, 3)
  a1 is a2
  a2 is a1

  raw-array-set(a1, 0, "f")
  a1 satisfies negate(identical(_, a2))
  a2 satisfies negate(identical(_, a1))
end

data D:
  | single
  | multi(a, b)
end
check:
  a = [array: single, multi(1, "2")]
  torepr(a) is "[array: single, multi(1, \"2\")]"
  a.set(0, a.get(1)).to-list() is [list: multi(1, "2"), multi(1, "2")]
  a.get(0) satisfies identical(_, a.get(1))
  a.length() is 2
end

check:
#  myarr = [array: 1, 2, 3, 4, 5]
#  answer = for arrays.array-fold(acc from 0, elt from myarr, ix from 0):
#    acc + elt
#  end
#  answer is 15

  bigarr = raw-array-of(2, 1000)
  answer2 = for raw-array-fold(acc from 0, elt from bigarr, ix from 0):
    acc + elt
  end
  answer2 is 2 * 1000

  answer3 = for raw-array-fold(acc from 0, elt from bigarr, ix from 0):
    for raw-array-fold(acc2 from acc, elt2 from bigarr, ix2 from 0):
      acc2 + elt2
    end
  end
  answer3 is 2 * 1000 * 1000

  bigarr2 = raw-array-of(3, 1000)
  answer4 = for raw-array-fold(acc from 0, elt from bigarr, ix from 0):
    acc + (elt * for raw-array-fold(acc2 from 0, elt2 from bigarr2, ix2 from 0):
      acc2 + elt2
    end)
  end
  answer4 is  3 * 1000 * 2 * 1000

end

