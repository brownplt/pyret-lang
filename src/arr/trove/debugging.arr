provide *

import global as _
include lists

fun my-print(s):
  lam(v) block:
    print(s + ": " + tostring(v) + "\n")
    v
  end
end

var now = empty

fun push-time(s):
  lam(v) block:
    now := link({s; time-now()}, now)
    v
  end
end

fun pop-time(v):
  cases (List) now block:
    | empty => raise('bad push-pop')
    | link({s; prev}, r) =>
      now := r
      print(s + ": " + tostring(time-now() - prev) + "\n")
      v
  end
end
