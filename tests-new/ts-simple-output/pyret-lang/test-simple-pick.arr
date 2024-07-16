### The program didn't define any tests

import sets as S
include from S: type Set end
import pick as P
include from P: data Pick end

fun pick-sum(s :: Set<Number>) -> Number:
  cases(P.Pick) s.pick():
    | pick-none => 0
    | pick-some(elt, rest) => elt + pick-sum(rest)
  end
end