#lang pyret

provide {
  check-equals: check-equals,
  check-not-equals: check-not-equals,
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

fun check-not-equals(message, thunk, value):
  var c: mk-checker(message, thunk, value)
  cond:
    | builtins.keys(c).member("exception") =>
      results := results.push(c.{ passed: false, reason: "Exception" })
    | c.actual.equals(value) =>
      results := results.push(c.{ passed: false, reason: "Values equal" })
    | else =>
      results := results.push(c.{ passed: true })
  end
end


fun check-exn(message, thunk, exn-pred):
  var c: mk-checker(message, thunk, exn-pred)
  cond:
    | builtins.keys(c).member("exception") =>
      cond:
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

fun format-results():
  var passed: results.filter(\r: (r.passed))
  var failed: results.filter(\r: (r.passed.not()))
  var passedNum: passed.length()
  var failedNum: failed.length()

  print(passedNum.tostring().append(" tests passed."))
  print(failedNum.tostring().append(" tests failed."))

  results.map(\r: (
    cond:
      | r.passed => nothing
      | else =>
        print("===========================")
        print("Test: ".append(r.message))
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
        print("")
    end
  ))
end
