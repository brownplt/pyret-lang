provide *
provide-types *

import global as _

data Event:
  | tick
  | mouse(x :: Number, y :: Number, kind :: String)
  | keypress(key :: String)
end

