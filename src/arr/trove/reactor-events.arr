provide *
provide-types *

import global as _

type Modifiers = {
  shift :: Boolean,
  alt :: Boolean,
  meta :: Boolean,
  control :: Boolean
}

data Event:
  | time-tick
  | mouse(x :: Number, y :: Number, kind :: String)
  | keypress(key :: String)
  | raw-key(symbol :: String, code :: Number, key :: String, action :: String, modifiers :: Modifiers)
end

data RawKeyEventType:
  | key-up
  | key-down
  | key-press
end

