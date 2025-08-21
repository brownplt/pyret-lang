provide *
provide-types *

import global as _

type Modifiers = {
  shift :: Boolean,
  alt :: Boolean,
  meta :: Boolean,
  control :: Boolean
}

# TODO: add other functions that we need
type Connector<M> = {
  handle-message-return :: (M -> Nothing)
}

data Event:
  | time-tick
  | mouse(x :: Number, y :: Number, kind :: String)
  | keypress(key :: String)
  | raw-key(symbol :: String, code :: Number, key :: String, action :: String, modifiers :: Modifiers)
end

data ConnectedEvent<M>:
  | message(msg :: M)
  | event(e :: Event)
end

data RawKeyEventType:
  | key-up
  | key-down
  | key-press
end

data SendingHandlerResult<S, M>:
  | update(new-state :: S)
  | send(new-state :: S, to-send :: M)
end
