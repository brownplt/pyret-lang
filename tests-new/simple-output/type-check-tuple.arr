### [ true, false ]

import global as G

foo :: { Boolean; Boolean } = { true; false }

fun printTuple(tuple :: { Boolean; Boolean }):
  G.console-log( tuple )
end

printTuple( foo )
