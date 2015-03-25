fun f():
  fun g(): nothing end
  g()
end

fun iter(n, thunk):
  if n <= 0: nothing
  else:
     thunk()
     iter(n - 1, thunk)
  end
end

iter(1000, f)  
# test the cost of inner function allocation
