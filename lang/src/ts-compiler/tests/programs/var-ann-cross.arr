# Assignments to another module's annotated vars are checked against the
# annotation attached to the box, including one written with a type alias
# (Score) that is not in scope here, and pause correctly on a non-flat one.
include file("lib/var-ann-lib.arr")

fun bump(): count := count + 1 end
fun set-paused(v): paused := v end

bump()
bump()
label := "done"
score := 99
set-paused(2)
free := "anything"

print(read-all() + "\n")
print(tostring(count) + " " + tostring(score) + "\n")

check "failed assignments raise and leave the var unchanged":
  (block: count := "no" end) raises "Number"
  count is 2
  (block: score := "no" end) raises "Number"
  score is 99
  (block: paused := "no" end) raises "Number"
  paused is 2
end
