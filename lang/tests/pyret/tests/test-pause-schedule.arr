import either as E
import equality as EQ
import valueskeleton as VS

fun sum-to(n):
  if n == 0: 0
  else: n + sum-to(n - 1)
  end
where:
  sum-to(0) is 0
  sum-to(1000) is 500500
  sum-to(5000) is 12502500
end

fun mutual-even(n):
  if n == 0: true
  else: mutual-odd(n - 1)
  end
end

fun mutual-odd(n):
  if n == 0: false
  else: mutual-even(n - 1)
  end
where:
  mutual-even(100000) is true
  mutual-odd(100000) is false
  mutual-even(100001) is false
end

data Peano:
  | zero
  | succ(prev :: Peano)
sharing:
  method to-num(self):
    cases(Peano) self:
      | zero => 0
      | succ(p) => 1 + p.to-num()
    end
  end,
  method plus(self, other):
    cases(Peano) self:
      | zero => other
      | succ(p) => succ(p.plus(other))
    end
  end
end

fun make-peano(n):
  if n == 0: zero
  else: succ(make-peano(n - 1))
  end
end

check "method-chain recursion under pause schedules":
  p500 = make-peano(500)
  p500.to-num() is 500
  p500.plus(p500).to-num() is 1000
  make-peano(2000).to-num() is 2000
end

data ModEq:
  | mod-eq(n :: Number, tag)
sharing:
  method _equals(self, other, eq):
    if num-modulo(self.n, 7) == num-modulo(other.n, 7):
      eq(self.tag, other.tag)
    else:
      EQ.NotEqual("mod-7 mismatch", self, other)
    end
  end,
  method _output(self):
    VS.vs-constr("mod-eq", [list: VS.vs-value(num-modulo(self.n, 7)), VS.vs-value(self.tag)])
  end
end

fun nest-mod-eq(n, inner):
  if n == 0: inner
  else: nest-mod-eq(n - 1, mod-eq(n, inner))
  end
end

check "equality callbacks under pause schedules":
  mod-eq(3, "a") is mod-eq(10, "a")
  mod-eq(3, "a") is-not mod-eq(4, "a")
  nest-mod-eq(200, mod-eq(0, "base")) is nest-mod-eq(200, mod-eq(7, "base"))
  nest-mod-eq(200, mod-eq(0, "base")) is-not nest-mod-eq(200, mod-eq(1, "base"))
  equal-always(range(0, 3000), range(0, 3000)) is true
  equal-always([list: range(0, 100), range(0, 100)], [list: range(0, 100), range(0, 100)]) is true
end

check "torepr callbacks under pause schedules":
  torepr(mod-eq(10, "x")) is 'mod-eq(3, "x")'
  torepr(nest-mod-eq(100, mod-eq(0, "deep"))) is torepr(nest-mod-eq(100, mod-eq(7, "deep")))
  (string-length(torepr(range(0, 1000))) > 3000) is true
end

fun deep-raise(n):
  if n == 0: raise("boom-at-depth")
  else: deep-raise(n - 1) + 1
  end
end

check "run-task nesting under pause schedules":
  cases(E.Either) run-task(lam(): sum-to(2000) end):
    | left(v) => v is 2001000
    | right(_) => raise("run-task should succeed")
  end
  cases(E.Either) run-task(lam(): sum-to(2000) + deep-raise(100) end):
    | left(_) => raise("run-task should propagate the failure")
    | right(_) => nothing
  end
end

check "exceptions through deep stacks under pause schedules":
  deep-raise(1000) raises "boom-at-depth"
  run-task(lam(): deep-raise(500) end) satisfies E.is-right
end

check "runtime loops under pause schedules":
  var acc = 0
  each(lam(x): acc := acc + x end, range(0, 2000))
  acc is 1999000
  fold(lam(a, b): a + b end, 0, range(0, 2000)) is 1999000
  map(lam(x): x * 2 end, range(0, 1000)).length() is 1000
  filter(lam(x): num-modulo(x, 2) == 0 end, range(0, 1000)).length() is 500
  string-length(string-repeat("ab", 5000)) is 10000
  raw-array-fold(lam(a, b, _): a + b end, 0, raw-array-of(1, 5000), 0) is 5000
end
