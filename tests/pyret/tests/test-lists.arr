import equality as E


data Box:
  | box(ref v)
end

f = lam(): "no-op" end

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

