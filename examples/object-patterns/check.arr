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
  if assert(fun: actual == expected end, "check.equal: ".append(message)):
    nothing
  else:
    print(actual)
  end
end

fun tru(actual, message):
  assert(fun: actual end, "check.tru: ".append(message))
end

fun fals(actual, message):
  assert(fun: not actual end, "check.fals: ".append(message))
end

fun nothin(actual, message):
  assert(fun: is-nothing(actual) end, "check.nothin: ".append(message))
end

fun assert(testfun, message):
  result = testfun()
  if result:
    print("Passed: ".append(message))
    true
  else:
    print("Failed: ".append(message))
    false
  end
end
