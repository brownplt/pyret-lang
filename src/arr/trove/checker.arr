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
    print(t)
    current-results := [t] + current-results
    print(current-results)
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
    end
  }
end

