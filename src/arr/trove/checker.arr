#lang pyret

provide *

Loc = error.Location

data CheckBlockResult:
  | check-block-result(name :: Option<String>, loc :: Loc, test-results :: List<TestResult>)
end

data TestResult:
  | success(loc :: Loc, code :: String)
  | failure(loc :: Loc, code :: String, reason :: String)
end

fun make-check-context(main-module-name :: String, check-all :: Boolean):
  var block-results = []
  fun add-block-result(cbr :: CheckBlockResult):
    block-results := [cbr] + block-results
  end
  var current-results = [] 
  fun add-result(t :: TestResult):
    current-results := [t] + current-results
  end
  fun reset-results(): current-results := [];
  {
    run-checks(self, module-name, checks):
      when check-all or (module-name == main-module-name):
        for each(c from checks):
          reset-results()
          c.run()
          add-block-result(check-block-result(c.name, c.location, current-results))
        end
      end
    end,
    check-is(self, code, left, right, loc):
      if left == right:
        add-result(success(loc, code))
      else:
        add-result(failure(loc, code, "Values not equal: " + torepr(left) + " " + torepr(right)))
      end
    end,
    check-satisfies(self, code, left, pred, loc):
      if pred(left):
        add-result(success(loc, code))
      else:
        add-result(failure(loc, code, "Predicate failed for value " + torepr(left)))
      end
    end,
    check-raises(self, code, thunk, str, loc):
      result = run-task(thunk)
      cases(Either) result:
        | left(v) => add-result(failure(loc, code, "No exception raised, answer was " + torepr(v)))
        | right(v) =>
          err-str = torepr(v)
          if string-contains(err-str, str):
            add-result(success(loc, code))
          else:
            add-result(failure(loc, code, "Wrong exception raised (expected to find " + torepr(str) + "): \n" + err-str))
          end
      end
    end,
    results(self):
      block-results
    end,
    render(self):
      self.block-results^render-check-results()
    end
  }
end

fun results-summary(block-results :: List<CheckBlockResult>):
  init = {
      message: "",
      passed: 0,
      failed: 0,
      total: 0
    }
  complete-summary = for fold(summary from init, br from block-results.reverse()):
    fun format-loc(l):
      l.file + ":" + tostring(l.line) + ":" + tostring(l.column)
    end
    block-summary = for fold(s from init, tr from br.test-results.reverse()):
      cases(TestResult) tr:
        | success(loc, code) => s.{
            message: s.message + "\n  " + loc^format-loc() + ": ok",
            passed: s.passed + 1,
            total: s.total + 1
          }
        | failure(loc, code, reason) =>
          m = s.message + "\n  " + loc^format-loc() + ": failed because: \n    " + reason
          s.{
            message: m,
            failed: s.failed + 1,
            total: s.total + 1
          }
      end
    end
    message = summary.message + "\n\n" + br.loc^format-loc() + ": " + br.name + " (" + tostring(block-summary.passed) + "/" + tostring(block-summary.total) + ") \n"
    rest-of-message =
      if block-summary.failed == 0: ""
      else: block-summary.message
      end
    {
      message: message + rest-of-message,
      passed: summary.passed + block-summary.passed,
      failed: summary.failed + block-summary.failed,
      total: summary.total + block-summary.total
    }
  end
  if complete-summary.total == 0: complete-summary.{message: "The program didn't define any tests."}
  else if complete-summary.failed == 0:
    happy-msg = if complete-summary.passed == 1:
        "Looks shipshape, your test passed, mate!"
      else:
        "Looks shipshape, all " + tostring(complete-summary.passed) + " tests passed, mate!"
      end
    complete-summary.{message: happy-msg}
  else:
    c = complete-summary
    c.{
      message: c.message + "\n\nPassed: " + tostring(c.passed) + "; Failed: " + tostring(c.failed) + "; Total: " + tostring(c.total) + "\n"
    }
  end
end

fun render-check-results(block-results :: List<CheckBlockResult>):
  results-summary(block-results).message
end

