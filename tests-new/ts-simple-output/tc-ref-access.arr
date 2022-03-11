### shipshape
include global

data D:
    | foo(ref x :: Number)
end

check:
    o = foo(10)
    o!x is 10
end