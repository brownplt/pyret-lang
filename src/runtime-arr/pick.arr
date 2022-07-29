provide *
provide-types *

import primitive-types as _

data Pick<a, b>:
  | pick-none
  | pick-some(elt :: a, rest :: b)
end
