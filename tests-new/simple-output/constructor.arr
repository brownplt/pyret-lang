### 3
import global as G
import string as S

counter = {
  make: lam(arr :: RawArray<Any>) -> Number: arr.length end
}

c = [counter: "a", "b", "c"]

G.console-log(S.num-to-string(c))
