#lang pyret

provide
  {
    equal: equal,
<<<<<<< HEAD
    assert: assert,
    tru: tru
=======
    assert: assert
>>>>>>> master
  }
end

fun equal(actual, expected, message):
  assert(\(actual.equals(expected)), "checkEqual: ".append(message))
end

<<<<<<< HEAD
fun tru(actual, message):
  assert(\(actual), "checkTrue: ".append(message))
end

=======
>>>>>>> master
fun assert(testfun, message):
  var result: testfun()
  cond:
    | result => print("Passed: ".append(message))
    | else => print("Failed: ".append(message))
  end
end
