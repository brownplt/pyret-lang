### true
import global as G
import image-structs as IS
import color as C

# TODO(alex): Fix module caching in order to remove
#   `import color as C`
#
# Spying (at src/arr/compiler/compile-structs.arr:186:6-186:64)
#
#  keys: [list: "builtin://global", "builtin://image-structs"]
#  uri: "builtin://color"
#  name: "gold"#
#
#  Key: builtin://color not found

include from IS:
  brown,
  color
end

result = brown == color(132, 60, 36, 1)

G.console-log(result)
