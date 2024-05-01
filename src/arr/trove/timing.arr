provide:
  time-only,
  time-value
end

import global as G

provide from G: time-now end

time-only :: <T> (( -> T) -> Number)
time-value :: <T> ( -> T) -> {Number; T}

fun time-only(f) block:
  start = time-now()
  f()
  stop = time-now()
  stop - start
end

fun time-value(f) block:
  start = time-now()
  ans = f()
  stop = time-now()
  { stop - start ; ans }
end
