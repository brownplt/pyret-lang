#lang pyret

provide {
  check-equals: check-equals,
  check-exn: check-exn,
  get-results: get-results,
  format-results: format-results
} end

var results: []

fun mk-checker(message :: String, thunk, value :: Any):
  var base: { expected: value, message: message }
  try:
    base.{ actual: thunk() }
  except(e):
    base.{ exception: e }
  end
end

fun check-equals(message, thunk, value):
  var c: mk-checker(message, thunk, value)
  cond:
    | builtins.keys(c).member("exception") =>
      results := results.push(c.{ passed: false, reason: "Exception" })
    | c.actual.equals(value) =>
      results := results.push(c.{ passed: true })
    | else =>
      results := results.push(c.{ passed: false, reason: "Values not equal" })
  end
end

fun check-exn(message, thunk, exn):
  var c: mk-checker(message, thunk, exn)
  cond:
    | builtins.keys(c).member("exception") =>
      cond:
        | c.exception.contains(exn) =>
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

fun format-results():
  var passed: results.filter(\r: (r.passed))
  var failed: results.filter(\r: (r.passed.not()))
  var passedNum: passed.length()
  var failedNum: failed.length()

  print(passedNum.tostring().append(" tests passed."))
  print(failedNum.tostring().append(" tests failed."))

  results.map(\r: (
    print("===========================")
    print("Test: ".append(r.message))
    cond:
      | r.passed => print("Passed")
      | else =>
        print("Failed because: ".append(r.reason))
        print("Expected:")
        print(r.expected)
        cond:
          | builtins.keys(r).member("actual") =>
            print("Actual:")
            print(r.actual)
          | builtins.keys(r).member("exception") =>
            print("Exception:")
            print(r.exception)
        end
    end
    print("")
  ))
end
