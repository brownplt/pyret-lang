### The program didn't define any tests

import global as G
import pick as P
include from P: type Pick end
include from G: raise end

fun pick-value<A, B>(p :: Pick<A, B>) -> A:
  cases(Pick) p:
    | pick-none => raise("pick-value on pick-none")
    | pick-some(v, _) => v
  end
end
