  p = mk-placeholder()
  p.set(42)
  try:  p.guard(fun(n :: Number): n end) except(e): test-print(e.message) end
  p.set(10)
  