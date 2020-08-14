### true
import global as G
import image-structs as IS
import color as C

# TODO(alex): Why does the color import fix importing?

include from IS:
  brown,
  color
end

result = brown == color(132, 60, 36, 1)

G.console-log(result)
