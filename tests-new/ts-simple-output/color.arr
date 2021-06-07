### true
import global as G
import color as C

include from C:
  color,
  brown,
end

result = brown == color(132, 60, 36, 1)

G.console-log(result)
