fun incr(n):
 n + 1
end


o = {x : mk-simple-mutable(1), y : 3, z : mk-simple-mutable( "BLAH")}
o!{x : incr(2), z : incr(incr(3)) + incr(5) + (3 * 2)}

o
