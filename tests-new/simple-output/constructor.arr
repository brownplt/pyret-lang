### 3
import global as G

counter = {
  make: lam(arr :: RawArray<Any>) -> Number: arr.length end
}

c = [counter: "a", "b", "c"]

G.console-log(G.num-to-str(c))
