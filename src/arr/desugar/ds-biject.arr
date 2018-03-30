provide *

include string-dict
include file("ds-structs.arr")

bijections = [string-dict:
  "reverse",
  let reverse-term = lam(g):
    cases (Term) g:
      | g-list(lst) => g-list(lst.reverse())
      | else => fail("Reverse bijection: expected a list, but found: " + tostring(g))
    end
  end:
    {reverse-term; reverse-term}
  end,
]
