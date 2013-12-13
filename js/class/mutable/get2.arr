#Has Guards

fun r(x):
    "guarded read"
end

fun w(x):
    x
end

o = {x : mk-mutable(1, r, w)}

o!x

