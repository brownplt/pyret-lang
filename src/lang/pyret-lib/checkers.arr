#lang pyret/library

import "list.rkt" as list
import "error.rkt" as error

provide
  {
    check-equals: check-equals,
    check-pred: check-pred,
    run-checks: run-checks,
    format-check-results: format-check-results,
    clear-results: clear-results,
    get-results: get-results
  }
end

# These are just convenience
Location = error.Location

data Result:
  | success(name :: String)
  | failure(name :: String, reason :: String)
  | err(name :: String, exception :: Any)
end

var current-results = []

fun check-equals(name, val1, val2):
  try:
    case:
      | (val1 == val2) =>
        current-results := current-results.push(success(name))
      | else =>
        current-results :=
          current-results.push(failure(name, "Values not equal: " +
                                       tostring(val1) +
                                       ", " +
                                       tostring(val2)))
    end
  except(e):
    current-results := current-results.push(err(name, e))
  end
end

fun check-pred(name, val1, pred):
  try:
    case:
      | pred(val1) =>
        current-results := current-results.push(success(name))
      | else =>
        current-results :=
          current-results.push(failure(name, "Value didn't satisfy predicate: " +
                                       tostring(val1) +
                                       ", " +
                                       pred._doc))
    end
  except(e):
    current-results := current-results.push(err(name, e))
  end
end

data CheckResult:
  | normal-result(name :: String, location :: Location, results :: list.List)
  | error-result(name :: String, location :: Location, results :: list.List, err :: Any)
end

var all-results :: list.List = []

fun run-checks(checks):
  var old-results = current-results
  these-check-results = 
    for list.map(chk from checks):
      l = chk.location
      loc = error.location(l.file, l.line, l.column)
      current-results := []
      result = try:
        chk.run()
        normal-result(chk.name, loc, current-results)
      except(e):
        error-result(chk.name, loc, current-results, e)
      end
      result
    end

  current-results := old-results
  all-results := all-results.push(these-check-results)
end

fun clear-results(): all-results := [] end
fun get-results(): all-results end

fun format-check-results():
  init = { passed: 0, failed : 0, test-errors: 0, other-errors: 0, total: 0}
  counts = for list.fold(acc from init, results from all-results):
    for list.fold(inner-acc from acc, check-result from results):
      inner-results = check-result.results
      other-errors = [check-result].filter(is-error-result).length()
      for list.each(failure from inner-results.filter(is-failure)):
        print("Test " + failure.name + " failed:")
        print(failure.reason)
        print("")
      end
      when is-error-result(check-result):
        print("Check block " + check-result.name + " " + check-result.location.format() + " ended in an error: ")
        print(check-result.err)
      end
      inner-acc.{
        passed: inner-acc.passed + inner-results.filter(is-success).length(),
        failed: inner-acc.failed + inner-results.filter(is-failure).length(),
        test-errors: inner-acc.test-errors + inner-results.filter(is-err).length(),
        other-errors: inner-acc.other-errors + other-errors,
        total: inner-acc.total + inner-results.length() 
      }
    end
  end
  print("Total: " + counts.total.tostring() +
        ", Passed: " + counts.passed.tostring() +
        ", Failed: " + counts.failed.tostring() +
        ", Errors in tests: " + counts.test-errors.tostring() +
        ", Errors in between tests: " + counts.other-errors.tostring())
end

