  p = mk-placeholder()
  try: p.get() except(e): test-print(e.message) end
  p.set(5)
  p.get()
  