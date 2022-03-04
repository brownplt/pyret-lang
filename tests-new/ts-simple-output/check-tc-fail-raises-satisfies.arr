##! type-mismatch
include global
check:
  1 + 2 raises-satisfies lam(x :: Any): to-repr(x) end
end