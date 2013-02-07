#lang pyret

provide
  {
    equal: equal,
    assert: assert,
    tru: tru
  }
end

fun equal(actual, expected, message):
  assert(\(actual.equals(expected)), "checkEqual: ".append(message))
end

fun tru(actual, message):
  assert(\(actual), "checkTrue: ".append(message))
end

fun assert(testfun, message):
  var result: testfun()
  cond:
    | result => print("Passed: ".append(message))
    | else => print("Failed: ".append(message))
  end
end
