#lang pyret

import "pyret-to-js-direct.arr" as P
import namespaces as N
import file as F
import ast as A

moorings = F.input-file("libs/moorings.arr").read-file()

JS-ENV = N.library-env.{
  equiv: true,
  data-equals: true,
  data-to-repr: true,
  prim-read-sexpr: true
}

mast = A.parse-tc(moorings, "moorings.arr", {check : false, env: JS-ENV})

out-moorings = F.output-file("htdocs/moorings.js", false)
free-in-moorings = A.free-ids(A.to-native(mast))
block:
  out-moorings.display("
if (require) {
  var Namespace = require('./namespace.js').Namespace;
}
  ")
  out-moorings.display("LIB = ")
  out-moorings.display(P.program-to-js(mast, free-in-moorings).js-src)
  out-moorings.display("
if(typeof exports !== 'undefined') {
  exports.lib = LIB;
}
")
end

