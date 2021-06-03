### 10
### 777
### 7
### 10
### 999
### 7
include global
include string
data D:
  | d(x :: Number)
  | e
  | h(y :: Number, x :: String)
end

fun f(val :: D) -> Number:
  cases(D) val:
    | d(m) => m
    | h(n, m) => n + string-length(m)
    | else => 777
  end
end

fun g(val :: D) -> Number:
  cases(D) val:
    | d(m) => m
    | h(n, m) => n + string-length(m)
    | e => 999
  end
end

console-log(f(d(10)))
console-log(f(e))
console-log(f(h(3, "abcd")))

console-log(g(d(10)))
console-log(g(e))
console-log(g(h(3, "abcd")))