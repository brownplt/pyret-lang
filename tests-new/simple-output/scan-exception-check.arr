### Some tests failed
import global as G

check "FOO":
  "BAR" is block:
    G.raise("OH NO")
    "BAR"
  end
end
