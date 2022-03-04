##! type-mismatch
include global
check:
  1 + 2 raises-violates lam(x :: Any): to-repr(x) end
end