#lang pyret

provide *
provide-types *

import global as _

data ValueSkeleton:
  | vs-str(s :: String)
  | vs-value(v :: Any)
  | vs-collection(name :: String, items)
  | vs-constr(name :: String, args)
  | vs-constr-render(name :: String, args, renderers)
  | vs-table(headers :: RawArray, rows #| :: RawArray<RawArray>|#)
  | vs-row(headers :: RawArray, values :: RawArray)
  | vs-seq(items)
  | vs-matrix(rows :: Number, cols :: Number, items :: RawArray)
end
