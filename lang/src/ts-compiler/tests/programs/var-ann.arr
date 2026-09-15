# Annotated vars: the declaration checks the initial value and every
# assignment checks the box's annotation -- in statement and tail position,
# through a flat and a non-flat refinement, and inside rec.

fun is-small(n): n < 100 end

fun deep(n):
  if n == 0: true else: deep(n - 1) end
end
fun is-deep-ok(n):
  # Non-flat predicate: recursion deep enough to make the check pause
  deep(20000)
end

var count :: Number = 0
var label :: String = "start"
var small :: Number%(is-small) = 1
var paused :: Number%(is-deep-ok) = 1
var free = 0
rec limit :: Number = 10

fun bump():
  count := count + 1
end
fun set-small(v): small := v end
fun set-paused(v): paused := v end
fun relabel(s) block:
  label := s
  label
end

bump()
bump()
set-small(42)
set-paused(2)
free := "anything"

print(tostring(count) + " " + relabel("done") + " " + tostring(small) + " "
  + tostring(paused) + " " + tostring(free) + " " + tostring(limit) + "\n")

check "failed assignments raise and leave the var unchanged":
  (block: count := "no" end) raises "Number"
  count is 2
  (block: small := 500 end) raises "is-small"
  small is 42
  (block: paused := "no" end) raises "Number"
  paused is 2
  (block:
    var bad :: Number = "no"
    bad
  end) raises "Number"
  (block:
    rec worse :: Number = "no"
    worse
  end) raises "Number"
end
