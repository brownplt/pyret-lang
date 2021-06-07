### 3
import global as G
import number as N

counter = {
  make: lam(arr :: RawArray<Any>) -> Number: arr.length end
}

c = [counter: "a", "b", "c"]

G.console-log(N.num-to-string(c))
