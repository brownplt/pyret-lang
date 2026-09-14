# Library for the var-ann-cross programs: annotated vars, one through a type
# alias (Score) that is not in scope in the importer, assigned from there.
provide: count, label, score, paused, free, read-all end

type Score = Number

fun deep(n :: Number) -> Boolean:
  if n == 0: true else: deep(n - 1) end
end
fun is-deep-ok(n :: Number) -> Boolean:
  # Non-flat predicate: recursion deep enough to make the check pause
  deep(20000)
end

var count :: Number = 0
var label :: String = "start"
var score :: Score = 0
var paused :: Number%(is-deep-ok) = 1
var free = 0

fun read-all() -> String:
  tostring(count) + " " + label + " " + tostring(score) + " "
    + tostring(paused) + " " + tostring(free)
end
