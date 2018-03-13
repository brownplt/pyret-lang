provide *

fun benchmark(f, times) block:
  start = time-now()
  for each(l from range(0, times)):
    f()
  end
  print(tostring(time-now() - start) + "\n")
end
