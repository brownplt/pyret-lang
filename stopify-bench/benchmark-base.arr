provide *

fun benchmark(f, times) block:
  start = time-now()
  for each(l from range(0, times)):
    f()
  end
  print("BEGIN STOPIFY BENCHMARK RESULTS")
  print(tostring(time-now() - start) + ",NA,NA,NA\n")
end
