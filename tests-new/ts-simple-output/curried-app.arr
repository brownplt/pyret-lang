### 110
import global as G

fun foo(x :: Number, y :: Number) -> Number:
    x + y
end

# TODO(alex): no parentheses around type annotation is a parsing error
# bar :: Numbee -> Number = foo(10, _)
bar :: (Number -> Number) = foo(10, _)

# 10 + 10 + 10 + (-30) + 10 + 100 = 110
result = bar(10) + bar(-30) + bar(100)
G.console-log(result)
