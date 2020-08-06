### true
import global as G
import option as O

include from O:
  type Option,
  some,
  none,
end

r1 = some(5) == some(4)
r2 = none == none
r3 = some(10) == some(10)

v = some(10)
r4 = cases(Option) v:
  | some(x) => x == 10
  | none => false
end

result = G.not(r1) and r2 and r3 and r4

G.console-log(result)
