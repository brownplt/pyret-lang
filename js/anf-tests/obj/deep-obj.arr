var f = 0
var g = 0

f  := fun(n):
    if n < 1:
        true
    else:
        goDeep2(n - 1)
    end
end
g := fun(n):
    if n < 1:
        true
    else:
        goDeep(n - 1)
    end
end

o = {x : f(1000), y : 42, z : f(10000), w : "a"}

test-print(o.x)
test-print(o.y)
test-print(o.z)
test-print(o.w)
