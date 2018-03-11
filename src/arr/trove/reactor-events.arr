provide *
provide-types *

import global as _

data Event:
  | time-tick
  | mouse(x :: Number, y :: Number, kind :: String)
  | keypress(key :: String)
  | raw-key(key :: String, key-action :: String, caps :: Boolean, shift :: Boolean, alt :: Boolean, command :: Boolean, control :: Boolean)
end

data RawKeyEventType:
  | key-up
  | key-down
  | key-press
end

