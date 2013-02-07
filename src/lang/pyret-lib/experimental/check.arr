#lang pyret

provide
  {
    equal: equal,
    tru: tru,
    nothin: nothin,
    assert: assert,
  }
end

fun equal(actual, expected, message):
  cond:
    | assert(\(actual.equals(expected)), "check.equal: ".append(message))
    => nothing
    | else => print(actual)
  end
end

fun tru(actual, message):
  assert(\(actual), "check.tru: ".append(message))
end

fun nothin(actual, message):
  assert(\(is-nothing(actual)), "check.nothin: ".append(message))
end

fun assert(testfun, message):
  var result: testfun()
  cond:
    | result => print("Passed: ".append(message)) true
    | else => print("Failed: ".append(message)) false
  end
end
