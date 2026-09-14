# var-ann.arr under the type checker: the same stores, statically accepted
# and still dynamically checked.

fun is-small(n :: Number) -> Boolean: n < 100 end

fun deep(n :: Number) -> Boolean:
  if n == 0: true else: deep(n - 1) end
end
fun is-deep-ok(n :: Number) -> Boolean:
  deep(20000)
end

var count :: Number = 0
var label :: String = "start"
var small :: Number%(is-small) = 1
var paused :: Number%(is-deep-ok) = 1
rec limit :: Number = 10

fun bump():
  count := count + 1
end
fun set-small(v :: Number): small := v end
fun set-paused(v :: Number): paused := v end
fun relabel(s :: String) -> String block:
  label := s
  label
end

bump()
bump()
set-small(42)
set-paused(2)

print(tostring(count) + " " + relabel("done") + " " + tostring(small) + " "
  + tostring(paused) + " " + tostring(limit) + "\n")
