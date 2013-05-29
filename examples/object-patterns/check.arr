#lang pyret

provide
  {
    equal: equal,
    tru: tru,
    fals: fals,
    nothin: nothin,
    assert: assert,
  }
end

fun equal(actual, expected, message):
  cond:
    | assert(fun: actual.equals(expected) end, "check.equal: ".append(message))
    => nothing
    | else => print(actual)
  end
end

fun tru(actual, message):
  assert(fun: actual end, "check.tru: ".append(message))
end

fun fals(actual, message):
  assert(fun: actual.not() end, "check.fals: ".append(message))
end

fun nothin(actual, message):
  assert(fun: is-nothing(actual) end, "check.nothin: ".append(message))
end

fun assert(testfun, message):
  result = testfun()
  cond:
    | result => print("Passed: ".append(message)) true
    | else => print("Failed: ".append(message)) false
  end
end
