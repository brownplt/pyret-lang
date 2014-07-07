data Natural:
  | zero
  | succ(prev :: Natural)
end

fun add(l :: Natural, r :: Natural):
  cases (Natural) l:
    | zero    => r
    | succ(n) => add(n, succ(r))
  end
end
