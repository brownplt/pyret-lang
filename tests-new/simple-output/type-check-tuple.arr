### truefalse

import global as G

foo :: { Boolean; Boolean } = { true; false }

fun printTuple(tuple :: { Boolean; Boolean }):
  result = G.js-to-string(tuple.{0}) + G.js-to-string(tuple.{1})
  G.console-log(result)
end

printTuple( foo )
