#lang pyret


fun negate(f): lam(x): not(f(x)) end end

check:
  a1 = [array: 1, 2, 3]

  a1.get-now(-1) raises "negative"
  a1.get-now(1 / 2) raises "integer"

  non-nums = [list: true, false, "not-a-num", {}, [list: ], lam(): 5 end, method(self): 10 end]
  for each(n from non-nums): a1.get-now(n) raises "Number" end
end

check:
  a = array-of("a", 3)
  a is=~ [array: "a", "a", "a"]

  array-set-now(a, 1, "b")
  a is=~ [array: "a", "b", "a"]

  array-get-now(a, 1) is "b"

  array-length(a) is 3
  l = array-to-list-now(a)
  l is [list: "a", "b", "a"]

  # Updating doesn't change the old to-list value
  array-set-now(a, 2, "c")
  l is [list: "a", "b", "a"]
  l2 = array-to-list-now(a)
  l2 is [list: "a", "b", "c"]
end

check:
  a1 = build-array(lam(i): i * i end, 6)
  a1.to-list-now() is [list: 0, 1, 4, 9, 16, 25]
  a2 = for build-array(i from 7):
    (i * i) - i
  end
  a2.to-list-now() is [list: 0, 0, 2, 6, 12, 20, 30]
end

check:
  a1 = [array: 1, 2, 3]
  a1.length() is 3

  a1 is a1
  a1 is<=> a1

  a2 = [array: 1, 2, 3]
  a2.length() is 3

  a1 is-not a2
  a1.to-list-now() is a2.to-list-now()

  a1 is-not<=> a2
  a2 is-not<=> a1

  a3 = [array: 4, "a", 6]
  a3.length() is 3

  a1.to-list-now() is-not a3.to-list-now()
  a3.to-list-now() is-not a1.to-list-now()

end

check:

  a1 = [array: 1, 2, 3]

  a1 satisfies is-array

  a1.get-now(0) is 1
  a1.get-now(1) is 2
  a1.get-now(2) is 3
  a1.get-now(3) raises "too large"

  a1.set-now(1, 5)
  a1 satisfies identical(_, a1)
  a1.get-now(1) is 5

  a1.set-now(1, 3)
  a1.set-now(2, 6)
  a1 satisfies identical(_, a1)
  a1.set-now(1, 3)
  a1.set-now(2, 6)
  a1 is a1
  a1.get-now(1) is 3
  a1.get-now(2) is 6

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

  raw-array-length([array: 1, 2]) raises "RawArray"
end

check:
  a = [array: ]
  a.length() is 0
  a.get-now(0) raises "too large"
  a.get-now(1) raises "too large"

  a.set-now(0, "val") raises "too large"
end

check:
  a1 = raw-array-of("init", 5)

  a1 satisfies is-raw-array

  raw-array-length(a1) is 5
  raw-array-get(a1, 5) raises "too large"
  a2 = [array: "init","init","init","init","init"]

  raw-array-to-list(a1) is a2.to-list-now()
  raw-array-set(a1, 0, "update")
  a1 satisfies _ <> a2
  raw-array-get(a1, 0) is "update"
  for each(i from range(1, 4)): raw-array-get(a1, i) is "init" end

end

check:
  arr = for raw-array-build-opt(i from 100):
    if num-modulo(i, 5) == 0:
      some(i * i)
    else:
      none
    end
  end
  
  arr is=~ [raw-array: 0, 25, 100, 225, 400, 625, 900, 1225, 1600, 2025, 2500, 3025, 3600, 4225, 4900, 5625, 6400, 7225, 8100, 9025]

  fun slowly(n):
    if n <= 0: nothing
    else: slowly(n - 1)
    end
  end
  
  fun slow(i) block:
    slowly(3000)
    if num-modulo(i, 2) == 0:
      some(i)
    else:
      none
    end
  end
  arr2 = raw-array-build-opt(slow, 1000)
  raw-array-length(arr2) is 500
  for each(i from range(0, 500)): raw-array-get(arr2, i) is i * 2 end
end

check:
  torepr([array: 1,2,"3"]) is '[array: 1, 2, "3"]'
  torepr([array: ]) is '[array: ]'
  torepr(raw-array-of(3, 3)) is '[raw-array: 3, 3, 3]'
end

check:
  fun f(v :: Array<Number>) block:
    when not(is-array(v)): raise("not an Array") end
    v
  end
  f([list: ]) raises "Array"
  f([array: ]).to-list-now() is [list: ]
  f([array: ]) satisfies is-array
end

check:
  a1 = raw-array-of(3, 3)
  a2 = raw-array-of(3, 3)
  a1 is=~ a2
  a2 is=~ a1

  raw-array-set(a1, 0, "f")
  a1 is-not<=> a2
  a2 is-not<=> a1
  a1 is-not=~ a2
  a2 is-not=~ a1
end

data D:
  | single
  | multi(a, b)
end
check:
  a = [array: single, multi(1, "2")]
  torepr(a) is "[array: single, multi(1, \"2\")]"
  a.set-now(0, a.get-now(1))
  a.to-list-now() is [list: multi(1, "2"), multi(1, "2")]
  a.get-now(0) satisfies identical(_, a.get-now(1))
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

check:
  [array: 1, 2, 3] is-not== [array: 1, 2, 3]
  [array: 1, 2, 3] is=~ [array: 1, 2, 3]
  [array: 1, 2] is-not=~ [array: 1, 2, 3]

  a1 = [array: 1, 2]
  a2 = [array: 1, 3]
  a1 is-not=~ a2

  a2.set-now(1, 2)
  a1 is=~ a2
end

check:
  fun loop(x):
    if x < 0: 0
    else:
      x + loop(x - 1)
    end
  end
  big-array = raw-array-build(lam(x): x end, 5000)

  filtered = raw-array-filter(lam(x) block:
    loop(1000)
    num-modulo(x, 2) == 0
  end, big-array)

  filtered is=~ raw-array-build(lam(x): x * 2 end, 2500)
end

check:
  raw-array-build(lam(x): x end, 2.5) raises "NumInteger"
  raw-array-build(lam(x): x end, -2) raises "NumNonNegative"
  raw-array-build(lam(x): x end, 9007199254740992) raises "larger than"
  # This constant is one larger than MAX_SAFE_INTEGER in JS
  raw-array-build(lam(x): x end, 4294967296) raises "larger than"

  raw-array-build-opt(lam(x): x end, 2.5) raises "NumInteger"
  raw-array-build-opt(lam(x): x end, -2) raises "NumNonNegative"
  raw-array-build-opt(lam(x): x end, 9007199254740992) raises "larger than"
  raw-array-build-opt(lam(x): x end, 4294967296) raises "larger than"

  raw-array-of("a", 2.5) raises "NumInteger"
  raw-array-of("a", -2) raises "NumNonNegative"
  raw-array-of("a", 9007199254740992) raises "larger than"
  raw-array-of("a", 4294967296) raises "larger than"
end
