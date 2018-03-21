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
  when true:
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
  end raises "no branch"
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
  end raises "no branch"
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
