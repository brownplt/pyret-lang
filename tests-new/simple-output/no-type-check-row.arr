### true

# no-type-check-row.arr
# Row tests.

import global as G
import tables as T
import list as L

my-row = [T.raw-row: {"a"; 1}, {"b"; 2}, {"c"; 3}]

expected-column-names = [L.list: "a", "b", "c"]
actual-column-names = my-row.get-column-names()

column-names-okay =
  T._primitiveEqual(expected-column-names, actual-column-names)

expected-first-value = 1
expected-second-value = 2
expected-third-value = 3

actual-first-value = my-row.get-value("a")
actual-second-value = my-row.get-value("b")
actual-third-value = my-row.get-value("c")

values-okay =
  (expected-first-value == actual-first-value)
  and
  (expected-second-value == actual-second-value)
  and
  (expected-third-value == actual-third-value)

passes-when-true = values-okay and column-names-okay

G.console-log(passes-when-true)