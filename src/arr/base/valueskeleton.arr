#lang pyret

provide *
provide-types *

data ValueSkeleton:
  | value(v :: Any)
  | collection(name :: String, items)
  | constr(name :: String, args)
end
