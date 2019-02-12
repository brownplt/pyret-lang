
import lists as L

include from L:
  join-str
end

import file("provider.arr") as P

include from P:
  *,
  type *
end


check:
  join-str([list: 1,2,3], " ") is "1 2 3"
end

check:
  x :: X = 300
  y is 100
  f(3) is 200
end


file-to-string
