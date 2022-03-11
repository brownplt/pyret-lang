##! a ref type
include global

data D:
    | foo(x :: Number)
end

check:
    y :: Number = foo(10)!x
    y is 10
end