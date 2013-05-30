#lang pyret

provide {
  assert-equals: assert-equals,
  check-equals: check-equals,
  check-not-equals: check-not-equals,
  check-exn: check-exn,
  get-results: get-results,
  assert-passing-results: assert-passing-results,
  format-results: format-results
} end

fun assert-true(value):
  assert-equals(value, true)
end

fun assert-equals(value1, value2):
  c = mk-checker("assert-equals", fun: value1 end, value2)
  case:
    | value1.equals(value2) =>
      results := results.push(c.{ passed: true })
    | else =>
      results := results.push(c.{ passed: false, reason: "Values not equal" })
  end
end

var results = []

fun mk-checker(message :: String, thunk, value :: Any):
  base = { expected: value, message: message }
  try:
    base.{ actual: thunk() }
  except(e):
    base.{ exception: e }
  end
end

fun check-equals(message, thunk, value):
  c = mk-checker(message, thunk, value)
  case:
    | builtins.keys(c).member("exception") =>
      results := results.push(c.{ passed: false, reason: "Exception" })
    | c.actual.equals(value) =>
      results := results.push(c.{ passed: true })
    | else =>
      results := results.push(c.{ passed: false, reason: "Values not equal" })
  end
end

fun check-not-equals(message, thunk, value):
  c = mk-checker(message, thunk, value)
  case:
    | builtins.keys(c).member("exception") =>
      results := results.push(c.{ passed: false, reason: "Exception" })
    | c.actual.equals(value) =>
      results := results.push(c.{ passed: false, reason: "Values equal" })
    | else =>
      results := results.push(c.{ passed: true })
  end
end


fun check-exn(message, thunk, exn-pred):
  c = mk-checker(message, thunk, exn-pred)
  case:
    | builtins.keys(c).member("exception") =>
      case:
        | exn-pred(c.exception) =>
          results := results.push(c.{ passed: true })
        | else => 
          results := results.push(c.{
            passed: false,
            reason: "Wrong exception"
          })
      end
    | else =>
      results := results.push(c.{
        passed: false,
        reason: "No exception raised"
      })
  end
end

fun get-results(): results end
fun assert-passing-results(n):
  m = results.filter(fun(r): r.passed end).length()
  when n <> m:
    format-results()
    print("Expected " + n + " passing tests, got " + m)
  end
end

fun format-results():
  passed = results.filter(fun(r): r.passed end)
  failed = results.filter(fun(r): r.passed.not() end)
  passedNum = passed.length()
  failedNum = failed.length()

  print(passedNum.tostring() + " tests passed.")
  print(failedNum.tostring() + " tests failed.")

  results.map(fun(r):
    case:
      | r.passed => nothing
      | else =>
        print("===========================")
        print("Test: " + r.message)
        print("Failed because: " + r.reason)
        print("Expected:")
        print(r.expected)
        case:
          | builtins.keys(r).member("actual") =>
            print("Actual:")
            print(r.actual)
          | builtins.keys(r).member("exception") =>
            print("Exception:")
            print(r.exception)
        end
        print("")
    end
  end)
  nothing
end
