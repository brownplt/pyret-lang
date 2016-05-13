#lang pyret

provide *
provide-types *

import global as _

data ValueSkeleton:
  | vs-str(s :: String)
  | vs-value(v :: Any)
  | vs-collection(name :: String, items)
  | vs-constr(name :: String, args)
  | vs-seq(items)
end
