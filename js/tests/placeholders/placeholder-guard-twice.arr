  p = mk-placeholder()
  p.guard(fun(n :: Number): n end)
  p.guard(fun(s :: String): s end)
  try: p.set("not-a-num") except(e): test-print(e.message) end
  try: p.set(5) except(e): test-print(e.message) end
  