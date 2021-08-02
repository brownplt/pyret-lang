provide *
provide-types *

import runtime-global as _

data Event:
  | time-tick
  | mouse(x :: Number, y :: Number, kind :: String)
  | keypress(key :: String)
end

