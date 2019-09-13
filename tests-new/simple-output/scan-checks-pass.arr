### All tests pass
import global as G

check "FOO":
  x = 5
  5 is 5
  x is 5
  true is-not false
  "FOO" is "FOO"
  "FOO" is-not "BAR"
end

# TODO(alex): Nested check block at end of check block causes a resolve-scope panic
