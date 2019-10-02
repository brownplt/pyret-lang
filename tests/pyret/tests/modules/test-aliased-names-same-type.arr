import file("aliased-names-same-type.arr") as A
import file("aliased-name-re-provided.arr") as A2

include from A:
  type Q,
  d,
  is-d
end

include from A2:
  type Q
end

check:
  some-d :: A2.Q = d
  some-d satisfies is-d
end
