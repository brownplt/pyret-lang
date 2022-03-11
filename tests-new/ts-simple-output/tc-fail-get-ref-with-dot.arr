##! a non-ref type
include global

data D:
    | foo(ref x :: Number)
end

foo(10).x
