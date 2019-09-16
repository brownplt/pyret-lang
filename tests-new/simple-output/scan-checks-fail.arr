### Some tests failed
import global as G

check "FOO":
  x = 5
  5 is 5
  x is 4
  false is-not false
  "FOO" is-not "FOO"
  "FOO" is "BAR"
end
