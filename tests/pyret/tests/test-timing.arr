include timing

fun fib(n):
  if n < 1: 1
  else: fib(n - 1) + fib(n - 2)
  end
end

check:
  time-now satisfies is-function
  time-only satisfies is-function
  time-value satisfies is-function
  

  t1 = time-now()
  t-only = time-only({(): fib(28)})
  t2 = time-now()
  t-only satisfies is-number
  (t-only <= (t2 - t1)) is true

  t3 = time-now()
  {t; val} = time-value({(): fib(28)})
  t4 = time-now()
  t satisfies is-number
  (t <= (t4 - t3)) is true
  val is 832040  
end
