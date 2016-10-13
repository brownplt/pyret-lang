#lang pyret

fun get-x(o):
 o.x
end

fun use-get-x():
 get-x({x: 11})
end
