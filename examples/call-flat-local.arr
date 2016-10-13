#lang pyret

fun get-x(o):
 o.x
end

fun use-get-x():
  get-x({x: 3})
end

fun use-use-get-x():
  use-get-x()
end

fun dont-use-get-x():
  f = get-x
  3
end

get-x()
#use-get-x()
