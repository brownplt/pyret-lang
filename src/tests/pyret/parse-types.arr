#lang pyret

import ast as A

check:
  ds = A.parse("datatype Foo: | foo with constructor(s): s end end",
                 "simple parse",
                 {["check"]: false}).with-types
  ds.block.stmts.first satisfies A.is-s_datatype
  dt = ds.block.stmts.first
  dt.variants.first satisfies A.is-s_datatype_singleton_variant
  dt.variants.first.constructor satisfies A.is-s_datatype_constructor
end
