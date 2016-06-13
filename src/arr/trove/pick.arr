provide *
provide-types *

import global as _

data Pick<a, b>:
  | pick-none
  | pick-some(elt :: a, rest :: b)
end
