check "https://github.com/brownplt/pyret-lang/issues/667":
  var x = 3
  x +
  block:
    x := 5
    x
  end
    is 8
end

check:
  letrec x = y + raise("y should trigger error first"), y = 10: x end
    raises "uninitialized-id"
end

