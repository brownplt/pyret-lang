import file("alias-x.arr") as A
import file("provide-provide-x.arr") as PPX

include from PPX:
  PX.x
end

include from PPX.PX:
  x
end

include from PPX:
  module PX
end

include from A:
  x
end

include from PPX:
  type PX.N
end
include from PPX.PX:
  type N
end

check:
  A.x is 100
  A.x is x
  x is PX.x

  y :: N = x
  y is x
end

