#lang pyret/library

import "list.rkt" as list
import "error.rkt" as err
provide
  {
    check-equals: check-equals,
    run-checks: run-checks,
    format-check-results: format-check-results,
    get-results: get-results
  }
end

Location = err.Location
Error = err.Error

data Result:
  | success
  | failure(reason :: String)
  | error(exception :: Error)
end

var current-results = []

fun check-equals(val1, val2):
  try:
    print(val1)
    print(val2)
    case:
      | (val1 == val2) =>
        current-results := current-results.push(success)
      | else =>
        current-results :=
          current-results.push(failure("Values not equal: " +
                                       tostring(val1) +
                                       ", " +
                                       tostring(val2)))
    end
  except(e):
    current-results := current-results.push(error(e))
  end
end

data CheckResult:
  | normal-result(name :: String, location :: Location, results :: list.List)
  | error-result(name :: String, location :: Location, results :: list.List, err :: Error)
end

var all-results :: list.List = []

fun run-checks(checks):
  these-check-results = 
    for list.map(chk from checks):
      l = chk.location
      loc = err.location(l.file, l.line, l.column)
      current-results := []
      try:
        chk.run()
        normal-result(chk.name, loc, current-results)
      except(e):
        error-result(chk.name, loc, current-results, e)
      end
    end

  all-results := all-results.push(these-check-results)
end

fun get-results(): all-results end

fun format-check-results():
  init = { passed: 0, failed : 0, errors: 0, total: 0}
  counts = for list.fold(acc from init, results from all-results):
    for list.fold(inner-acc from acc, check-result from results):
      results = check-result.results
      extra-errors = [check-result].filter(is-error-result).length()
      print(results)
      acc.{
        passed: acc.passed + results.filter(is-success).length(),
        failed: acc.failed + results.filter(is-failure).length(),
        errors: acc.errors + results.filter(is-error).length() + extra-errors,
        total: acc.total + results.length() 
      }
    end
  end
  print(counts)
end

