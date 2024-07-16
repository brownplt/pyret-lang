### The program didn't define any tests

import global as G
import either as E
include from E: type Either end
include from G: raise end

fun either-a<A, B>(e :: Either<A, B>) -> A:
  cases(Either) e:
    | right(_) => raise("error")
    | left(v) => v
  end
end
