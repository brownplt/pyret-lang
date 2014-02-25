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

fun make-check-context(main-module-name):
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
      when module-name == main-module-name:
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
      l.file + ":" + l.line.tostring() + ":" + l.column.tostring()
    end
    message = summary.message + "\n\n" + br.loc^format-loc() + ": " + br.name + "\n"
    for fold(s from summary.{message: message}, tr from br.test-results.reverse()):
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
  end
  if complete-summary.total == 0: complete-summary.{message: "The program didn't define any tests."}
  else if complete-summary.failed == 0:
    happy-msg = if complete-summary.passed == 1:
        "Looks shipshape, your test passed, mate!"
      else:
        "Looks shipshape, all " + complete-summary.passed.tostring() + " tests passed, mate!"
      end
    complete-summary.{message: happy-msg}
  else:
    c = complete-summary
    c.{
      message: c.message + "\n\nPassed: " + c.passed.tostring() + "; Failed: " + c.failed.tostring() + "; Total: " + c.total.tostring() + "\n"
    }
  end
end

fun render-check-results(block-results :: List<CheckBlockResult>):
  results-summary(block-results).message
end

