data Box:
  | box(ref v)
end

check "s-when":
  var a = 0
  when true:
    a := 1
  end
  a is 1
  when false:
    a := 2
  end
  a is 1
  when true block:
    a := a + 1
    a := a + 1
  end
  a is 3
end

check "s-if":
  if true:
    1
  else if true:
    2
  end is 1

  if false:
    1
  else if true:
    2
  end is 2

  if false:
    1
  else if false:
    2
  end raises "branch"
end

check "s-if-pipe":
  ask:
    | true then: 1
    | true then: 2
  end is 1

  ask:
    | false then: 1
    | true then: 2
  end is 2

  ask:
    | false then: 1
    | false then: 2
  end raises "branch"
end

check "s-if-pipe-else":
  ask:
    | true then: 1
    | otherwise: 2
  end is 1

  ask:
    | false then: 1
    | otherwise: 2
  end is 2
end

check "s-template":
  ... raises 'template'
  lam(): ... end does-not-raise
end

check "s-user-block":
  var a = 0
  block:
    a := 1
    2
  end is 2

  a is 1
end

check "s-paren":
  (1 + 2) is 3
end

check "arith s-op":
  1 + 2 is 3
  3 - 2 is 1
  2 * 3 is 6
  6 / 2 is 3

  6 < 8 is true
  6 < 6 is false
  6 < 4 is false

  6 <= 8 is true
  6 <= 6 is true
  6 <= 4 is false

  6 > 8 is false
  6 > 6 is false
  6 > 4 is true

  6 >= 8 is false
  6 >= 6 is true
  6 >= 4 is true

  6 == 8 is false
  6 == 6 is true
  6 == 4 is false

  6 <> 8 is true
  6 <> 6 is false
  6 <> 4 is true

  box(1) == box(1) is false
  box(1) =~ box(1) is true
  [list: 1] == [list: 1] is true
  [list: 1] <=> [list: 1] is false
  empty <=> empty is true
end

check "s-op and":
  1 and 2 raises "Boolean"
  false and 2 is false
  true and 2 raises "Boolean"

  true and true and true is true
  true and true and false is false
  true and false and true is false

  (false and raise("asd")) and raise("dsa") is false

  true and raise("foo") raises "foo"
  false and raise("foo") does-not-raise
end

check "s-op or":
  1 or 2 raises "Boolean"
  false or 2 raises "Boolean"
  true or 2 is true

  false or false or false is false
  false or false or true is true
  false or true or false is true

  (true or raise("asd")) or raise("dsa") is true

  false or raise("foo") raises "foo"
  true or raise("foo") does-not-raise
end

check "s-op carret":
  1 ^ link(_, empty) is [list: 1]
  8
    ^ (_ - 5)
    ^ some is some(3)
end

check "double desugar":
  a1 = [list: 1]
  a2 = [list: 2]

  a1 satisfies _ <> a2
end

check "s-construct":
  [list: 1, 2, 3, 4, 5, 6] is link(1, link(2, link(3, link(4, link(5, link(6, empty))))))
  [list: 1, 2, 3, 4, 5] is link(1, link(2, link(3, link(4, link(5, empty)))))
  [list: 1, 2, 3, 4] is link(1, link(2, link(3, link(4, empty))))
  [list: 1, 2, 3] is link(1, link(2, link(3, empty)))
  [list: 1, 2] is link(1, link(2, empty))
  [list: 1] is link(1, empty)
  [list: ] is empty
end
