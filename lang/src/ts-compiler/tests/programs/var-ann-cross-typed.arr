# var-ann-cross.arr under the type checker: the provided vars keep their
# var-ness and types, so the stores are statically accepted and still
# dynamically checked.
include file("lib/var-ann-lib.arr")

fun bump(): count := count + 1 end
fun set-paused(v :: Number): paused := v end

bump()
bump()
label := "done"
score := 99
set-paused(2)

print(read-all() + "\n")
print(tostring(count) + " " + tostring(score) + "\n")
