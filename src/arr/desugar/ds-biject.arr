provide *

include string-dict
include file("ds-structs.arr")

bijections = [string-dict:
  "reverse",
  let reverse-term = lam(g):
    cases (Term) g:
      | g-list(lst) => g-list(lst.reverse())
      | else => fail("attempting to call a bijection function reverse on: " + tostring(g))
    end
  end:
    {reverse-term; reverse-term}
  end,
]
