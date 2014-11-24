import equality as E


data Box:
  | box(ref v)
end

f = lam(): "no-op" end

check "List where: block":
  [list: 1].push(0) is [list: 0, 1]

  [list: 1, 2, 3, 4, 5, 6].take(3) is [list: 1, 2, 3]

  [list: 1, 2, 3, 4, 5, 6].drop(3) is [list: 4, 5, 6]

  [list: 1, 2, 3].get(0) is 1
  [list: ].get(0) raises ""

  [list: 1, 2, 3].set(0, 5) is [list: 5, 2, 3]
  [list: ].set(0, 5) raises ""
end

check "get where: block":
  lists.get([list: 1, 2, 3], 0) is 1
  lists.get([list: ], 0) raises ""
end

check "set where: block":
  lists.set([list: 1, 2, 3], 0, 5) is [list: 5, 2, 3]
  lists.set([list: 1, 2, 3], 5, 5) raises ""
end

check "range where: block":
  lists.range(0,0) is [list: ]
  lists.range(0,1) is [list: 0]
  lists.range(-5,5) is [list: -5, -4, -3, -2, -1, 0, 1, 2, 3, 4]
end

check "repeat where: block":
  lists.repeat(0, 10) is empty
  lists.repeat(3, -1) is [list: -1, -1, -1]
  lists.repeat(1, "foo") is link("foo", empty)
end

check "reverse where: block":
  lists.reverse([list: ], [list: ]) is [list: ]
  lists.reverse([list: 1, 3], [list: ]) is [list: 3, 1]
end

check "filter where: block":
  filter(lam(e): e > 5;, [list: -1, 1]) is [list: ]
  filter(lam(e): e > 0;, [list: -1, 1]) is [list: 1]
end

check "partition where: block":
  partition(lam(e): e > 0;, [list: -1, 1]) is { is-true: [list: 1], is-false : [list: -1] }
  partition(lam(e): e > 5;, [list: -1, 1]) is { is-true: [list: ], is-false : [list: -1, 1] }
  partition(lam(e): e < 5;, [list: -1, 1]) is { is-true: [list: -1, 1], is-false : [list: ] }
end


check "find where: block":
  find(lam(elt): elt > 1 end, [list: 1, 2, 3]) is some(2)
  find(lam(elt): elt > 4 end, [list: 1, 2, 3]) is none
  find(lam(elt): true end, [list: "find-me", "miss-me"]) is some("find-me")
  find(lam(elt): true end, empty) is none
  find(lam(elt): false end, [list: "miss-me"]) is none
  find(lam(elt): false end, empty) is none
end



check "split-at where: block":
  one-four = link(1, link(2, link(3, link(4, empty))))

  split-at(0, one-four) is { prefix: empty, suffix: one-four }
  split-at(4, one-four) is { prefix: one-four, suffix: empty }
  split-at(2, one-four) is { prefix: link(1, link(2, empty)), suffix: link(3, link(4, empty)) }
  split-at(-1, one-four) raises "Invalid index"
  split-at(5, one-four) raises "Index too large"
end



check "any where: block":
  any(lam(n): n > 1 end, [list: 1, 2, 3]) is true
  any(lam(n): n > 3 end, [list: 1, 2, 3]) is false
  any(lam(x): true  end, empty) is false
  any(lam(x): false end, empty) is false
end


check "all where: block":
  lists.all(lam(n): n > 1 end, [list: 1, 2, 3]) is false
  lists.all(lam(n): n <= 3 end, [list: 1, 2, 3]) is true
  lists.all(lam(x): true  end, empty) is true
  lists.all(lam(x): false end, empty) is true
end



check "all2 where: block":
  lists.all2(lam(n, m): false end, [list: 1, 2, 3], empty) is true
  lists.all2(lam(n, m): true  end, [list: 1, 2, 3], empty) is true
  lists.all2(lam(n, m): n > m end,        [list: 1, 2, 3], [list: 0, 1, 2]) is true
  lists.all2(lam(n, m): (n + m) == 3 end, [list: 1, 2, 3], [list: 2, 1, 0]) is true
  lists.all2(lam(n, m): n < m end,        [list: 1, 2, 3], [list: 0, 1, 2]) is false
  lists.all2(lam(_, _): true  end, empty, empty) is true
  lists.all2(lam(_, _): false end, empty, empty) is true
end


check "map where: block":
  map(lam(_): raise("shipwrecked!");, [list: ]) is [list: ]
  map(lam(_): 2;, [list: 1, 2, 3, 4]) is [list: 2, 2, 2, 2]
  map(lam(x): x + 1;, [list: 1, 2, 3, 4]) is [list: 2, 3, 4, 5]
end



check "map2 where: block":
  map2(lam(_, _): raise("shipwrecked!");, [list: ], [list: ]) is [list: ]
  map2(lam(x, y): x or y;, [list: true, false], [list: false, false]) is [list: true, false]
end


check "map_n where: block":
  map_n(lam(n, e): n;, 0, [list: "captain", "first mate"]) is [list: 0, 1]
end


check "fold where: block":
  fold(lam(acc, cur): acc;, 1, [list: 1, 2, 3, 4]) is 1
  fold(lam(acc, cur): cur;, 1, [list: 1, 2, 3, 4]) is 4
  fold(lam(acc, cur): acc + cur;, 0, [list: 1, 2, 3, 4]) is 10
end


check "fold2 where: block":
  fold2(lam(x, y, z): x - y - z;, 6, [list: 1, 1, 1], [list: 1, 1, 1]) is 0
end


check "fold_n where: block":
  lists.fold_n(lam(n, acc, _): n * acc end, 1, 1, [list: "a", "b", "c", "d"]) is 1 * 2 * 3 * 4
  lists.fold_n(lam(n, acc, cur):
                  tostring(n) + " " + cur + ", " + acc
               end,
               95, "and so forth...", repeat(5, "jugs o' grog in the hold"))
    is "99 jugs o' grog in the hold, 98 jugs o' grog in the hold, "
    + "97 jugs o' grog in the hold, 96 jugs o' grog in the hold, "
    + "95 jugs o' grog in the hold, and so forth..."
  lists.fold_n(lam(n, acc, cur): ((num-modulo(n, 2) == 0) or cur) and acc end,
               0, true, [list: false, true, false])
    is true
end




check "member":

  # This is the old version that just uses == internally
  traditional-member = lam(l, elt): l.member(elt) end

  l = [list: 1, f, 3]

  traditional-member(l, 1) is true
  traditional-member(l, 3) is true
  traditional-member(l, f) raises "function"
  traditional-member(l, 5) is false


  lists.member3(l, 1) is E.Equal
  lists.member3(l, 3) is E.Equal
  lists.member3(l, f) is E.Unknown
  lists.member3(l, 5) satisfies E.is-NotEqual

  lists.member(l, 1) is true
  lists.member(l, 3) is true
  lists.member(l, f) raises "function"
  lists.member(l, 5) is false

  b1 = box(5)
  b2 = box(5)
  l1 = [list: 1, b1]

  lists.member-now(l1, b1) is true
  lists.member-now(l1, b2) is true
  lists.member-now3(l1, b1) is E.Equal
  lists.member-now3(l1, b2) is E.Equal

  lists.member-always(l1, b1) is true
  lists.member-always(l1, b2) is false
  lists.member-always3(l1, b1) is E.Equal
  lists.member-always3(l1, b2) satisfies E.is-NotEqual

  lists.member-identical(l1, b1) is true
  lists.member-identical(l1, b2) is false
  lists.member-identical3(l1, b1) is E.Equal
  lists.member-identical3(l1, b2) satisfies E.is-NotEqual  

  b1!{v: 10}

  lists.member-now(l1, b1) is true
  lists.member-now(l1, b2) is false
  lists.member-now3(l1, b1) is E.Equal
  lists.member-now3(l1, b2) satisfies E.is-NotEqual

end

check "shuffle":
  l = [list: 1, 2, 3, 4]

  num-random-seed(0)
  l-mixed = lists.shuffle(l)
  sets.list-to-set(l-mixed) is sets.list-to-set(l)
  l-mixed.length() is l.length()

  num-random-seed(0)
  l-mixed2 = lists.shuffle(l)
  l-mixed2 is l-mixed
end

