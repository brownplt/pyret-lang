provide *
provide-types *

import primitive-types as P
include from P: type RawArray end

data ValueSkeleton:
  | vs-literal-str(s :: String)
  | vs-str(s :: String)
  | vs-num(v :: Number)
  | vs-bool(v :: Boolean)
  | vs-nothing
  | vs-tuple(vals :: RawArray<ValueSkeleton>)

  | vs-function(v :: Any)
  | vs-method(v :: Any)

#   | vs-lazy(placeholder :: ValueSkeleton, next-n :: (Number -> { RawArray<ValueSkeleton>; Option<ValueSkeleton%(is-vs-lazy)> }))

  | vs-record(field-names :: RawArray<String>, vals :: RawArray<ValueSkeleton>)

  | vs-collection(name :: String, items :: RawArray<ValueSkeleton>)
  | vs-constr(name :: String, field-names :: RawArray<String>, args :: RawArray<ValueSkeleton>)

  | vs-table(headers :: RawArray<String>, rows :: RawArray<RawArray<ValueSkeleton>>)
  | vs-row(headers :: RawArray<String>, values :: RawArray<ValueSkeleton>)


  | vs-seq(items :: RawArray<ValueSkeleton>)

  | vs-cyclic(label :: String, v :: Any)
  | vs-reactor(v :: Any)
  | vs-other(v :: Any)
end
