### true
import global as G
import image-structs as IS

include from IS:
  color,
  brown,
end

result = brown == color(132, 60, 36, 1)

G.console-log(result)
