fun f(): try: g() except(e): raise(e + 5) end end
fun g(): raise(5) end
fun h(): try: f() except(e): e end end
h()