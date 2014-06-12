import "tests/exporter.arr" as E
import x, f from "tests/exporter.arr"
import string-dict as S

check "Should import only what is exported":
  keys = S.to-dict(E).keys()
  sets.list-to-tree-set(keys).to-list() is [set: "x", "f"].to-list()
end

check "Should import constants": x is 10 end

check "Should only instantiate module once":
  f() is 1
  E.f() is 2
  f() is 3
end


