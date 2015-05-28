#lang pyret

provide *
provide-types *

data ValueSkeleton:
  | vs-value(v :: Any)
  | vs-collection(name :: String, items)
  | vs-constr(name :: String, args)
end
