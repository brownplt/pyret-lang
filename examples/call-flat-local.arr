#lang pyret

fun get-x(o):
 o.x
end

fun use-get-x():
  a = get-x({x: 133})
  b = get-x({x: a})
  c = get-x({x: b})
  c
end

# Has depth 2 so it's not considered flat (yet)
fun use-use-get-x():
  use-get-x()
end

fun use-get-x-then-non-flat():
  a = get-x({x: 1234})
  # Call a "non-flat" function like use-use-get-x
  use-use-get-x()
end

# This should test the "base" case of compile-flat-apt when the body is none
fun flat-app-as-last-expr():
  get-x({x:4567})
end


fun dont-use-get-x():
  f = get-x
  3
end

use-get-x()
use-get-x-then-non-flat()
flat-app-as-last-expr()