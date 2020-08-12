### true
import global as G
import either as E

include from E:
  type Either,
  left,
  right,
end

v = left(100)

result = cases(Either) v:
  | left(l) => l == 100
  | right(_) => false
end

G.console-log(result)
