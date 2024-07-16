### The program didn't define any tests

import global as G
import option as O
include from O: type Option end
include from G: raise end

fun option-v<A>(o :: Option<A>) -> A:
  cases(Option) o:
    | none => raise("error")
    | some(v) => v
  end
end
