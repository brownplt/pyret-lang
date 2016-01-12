#lang pyret

provide *
provide-types *
import srcloc as SL
import either as E
import error-display as ED
import render-error-display as RED
type Loc = SL.Srcloc
type Either = E.Either

is-right = E.is-right
is-left = E.is-left


data CheckBlockResult:
  | check-block-result(
      name :: String,
      loc :: Loc,
      test-results :: List<TestResult>,
      maybe-err :: Option<Any>
    )
end

data TestResult:
  | success(loc :: Loc, code :: String)
  | failure-not-equal(loc :: Loc, code :: String, refinement, left, right) with:
    render-fancy-reason(self):
      print("failure-not-equal")
      [ED.error: 
        [ED.para: cases(Option) self.refinement:
            | none    => ED.text("Values not equal")
            | some(_) => ED.text("Values not equal (using custom equality):")
          end],
        [ED.para: ED.embed(self.left)],
        [ED.para: ED.embed(self.right)]]
    end,
    render-reason(self):
      print("failure-not-equal")
      [ED.error: 
        [ED.para: cases(Option) self.refinement:
            | none    => ED.text("Values not equal")
            | some(_) => ED.text("Values not equal (using custom equality):")
          end],
        [ED.para: ED.embed(self.left)],
        [ED.para: ED.embed(self.right)]]
    end
  | failure-not-different(loc :: Loc, code :: String, refinement, left, right) with:
    render-fancy-reason(self):
      print("failure-not-different")
      [ED.error:
        [ED.para: cases(Option) self.refinement:
            | none    => ED.text("Values not different")
            | some(_) => ED.text("Values not different (using custom equality):")
          end],
        [ED.para: ED.embed(self.left)],
        [ED.para: ED.embed(self.right)]]
    end,
    render-reason(self):
      print("failure-not-different")
      [ED.error:
        [ED.para: cases(Option) self.refinement:
            | none    => ED.text("Values not different")
            | some(_) => ED.text("Values not different (using custom equality):")
          end],
        [ED.para: ED.embed(self.left)],
        [ED.para: ED.embed(self.right)]]
    end
  | failure-not-satisfied(loc :: Loc, code :: String, val, pred) with:
    render-fancy-reason(self):
      print("failure-not-satisfied")
      [ED.error:
        [ED.para: ED.text("Predicate failed for value:")],
        [ED.para: ED.embed(self.val)]]
    end,
    render-reason(self):
      print("failure-not-satisfied")
      [ED.error:
        [ED.para: ED.text("Predicate failed for value:")],
        [ED.para: ED.embed(self.val)]]
    end
  | failure-not-dissatisfied(loc :: Loc, code :: String, val, pred) with:
    render-fancy-reason(self):
      print("failure-not-dissatisfied")
      [ED.error:
        [ED.para: ED.text("Predicate succeeded for value (it should have failed):")],
        [ED.para: ED.embed(self.val)]]
    end,
    render-reason(self):
      print("failure-not-dissatisfied")
      [ED.error:
        [ED.para: ED.text("Predicate succeeded for value (it should have failed):")],
        [ED.para: ED.embed(self.val)]]
    end
  | failure-wrong-exn(loc :: Loc, code :: String, exn-expected, actual-exn) with:
    render-fancy-reason(self):
      print("failure-wrong-exn")
      [ED.error:
        [ED.para: ED.text("Got unexpected exception ")],
        [ED.para: ED.embed(self.actual-exn)],
        [ED.para: ED.text("when expecting ")],
        [ED.para: ED.embed(self.exn-expected)]]
    end,
    render-reason(self):
      print("failure-wrong-exn")
      [ED.error:
        [ED.para: ED.text("Got unexpected exception ")],
        [ED.para: ED.embed(self.actual-exn)],
        [ED.para: ED.text("when expecting ")],
        [ED.para: ED.embed(self.exn-expected)]]
    end
  | failure-right-exn(loc :: Loc, code :: String, exn-not-expected, actual-exn) with:
    render-fancy-reason(self):
      print("failure-right-exn")
      [ED.error:
        [ED.para: ED.text("Got exception ")],
        [ED.para: ED.embed(self.actual-exn)],
        [ED.para: ED.text("and expected it not to contain ")],
        [ED.para: ED.embed(self.exn-not-expected)]]
    end,
    render-reason(self):
      print("failure-right-exn")
      [ED.error:
        [ED.para: ED.text("Got exception ")],
        [ED.para: ED.embed(self.actual-exn)],
        [ED.para: ED.text("and expected it not to contain ")],
        [ED.para: ED.embed(self.exn-not-expected)]]
    end
  | failure-exn(loc :: Loc, code :: String, actual-exn) with:
    render-fancy-reason(self):
      print("failure-exn")
      [ED.error:
        [ED.para: ED.text("Got unexpected exception ")],
        [ED.para: ED.embed(self.actual-exn)]]
    end,
    render-reason(self):
      print("failure-exn")
      [ED.error:
        [ED.para: ED.text("Got unexpected exception ")],
        [ED.para: ED.embed(self.actual-exn)]]
    end
  | failure-no-exn(loc :: Loc, code :: String, exn-expected :: Option<String>) with:
    render-fancy-reason(self):
      print("failure-no-exn")
      cases(Option) self.exn-expected:
        | some(exn) => [ED.error: [ED.para: ED.text("No exception raised, expected"), ED.embed(exn)]]
        | none      => [ED.error: [ED.para: ED.text("No exception raised")]]
      end
    end,
    render-reason(self):
      print("failure-no-exn")
      cases(Option) self.exn-expected:
        | some(exn) => [ED.error: [ED.para: ED.text("No exception raised, expected"), ED.embed(exn)]]
        | none      => [ED.error: [ED.para: ED.text("No exception raised")]]
      end
    end
  | failure-raise-not-satisfied(loc :: Loc, code :: String, exn, pred) with:
    render-fancy-reason(self):
      print("failure-raise-not-satisfied")
      [ED.error:
        [ED.para: ED.text("Predicate failed for exception:")],
        [ED.para: ED.embed(self.exn)]]
    end,
    render-reason(self):
      print("failure-raise-not-satisfied")
      [ED.error:
        [ED.para: ED.text("Predicate failed for exception:")],
        [ED.para: ED.embed(self.exn)]]
    end
  | failure-raise-not-dissatisfied(loc :: Loc, code :: String, exn, pred) with:
    render-fancy-reason(self):
      print("failure-raise-not-dissatisfied")
      [ED.error:
        [ED.para: ED.text("Predicate succeeded for exception (it should have failed):")],
        [ED.para: ED.embed(self.exn)]]
    end,
    render-reason(self):
      print("failure-raise-not-dissatisfied")
      [ED.error:
        [ED.para: ED.text("Predicate succeeded for exception (it should have failed):")],
        [ED.para: ED.embed(self.exn)]]
    end
  # This is not so much a test result as an error in a test case:
  # Maybe pull it out in the future?
  | error-not-boolean(loc :: Loc, code :: String, refinement, left, righ, test-result) with:
    render-fancy-reason(self):
      print("error-not-boolean")
      [ED.error:
        [ED.para: ED.text("The custom equality funtion must return a boolean, but instead it returned: ")],
        [ED.para: ED.embed(self.test-result)]]
    end,
    render-reason(self):
      print("error-not-boolean")
      [ED.error:
        [ED.para: ED.text("The custom equality funtion must return a boolean, but instead it returned: ")],
        [ED.para: ED.embed(self.test-result)]]
    end
end

fun make-check-context(main-module-name :: String, check-all :: Boolean):
  var block-results = [list: ]
  fun add-block-result(cbr :: CheckBlockResult):
    block-results := [list: cbr] + block-results
  end
  var current-results = [list: ] 
  fun add-result(t :: TestResult):
    current-results := [list: t] + current-results
  end
  fun left-right-check(loc, code):
    lam(with-vals, left, right):
      run = lam():
        # TODO(joe): Once a bootstrap has happened, these ifs can be changed
        # to just thunk applications.  Need the if-test to accommodate two
        # desugar styles at once.
        lv = if is-function(left): left() else: left end
        rv = if is-function(right): right() else: right end
        with-vals(lv, rv)
      end
      cases(Either) run-task(run):
        | left(v) => v
        | right(e) => add-result(failure-exn(loc, code, exn-unwrap(e)))
      end
    end
  end
  fun check-bool(loc, code, test-result, on-failure):
    if test-result:
      add-result(success(loc, code))
    else:
      add-result(on-failure())
    end
  end
  fun reset-results(): current-results := [list: ] end
  {
    run-checks(self, module-name, checks):
      when check-all or (module-name == main-module-name):
        for each(c from checks):
          results-before = current-results
          reset-results()
          result = run-task(c.run)
          cases(Either) result:
            | left(v) => add-block-result(check-block-result(c.name, c.location, current-results, none))
            | right(err) => add-block-result(check-block-result(c.name, c.location, current-results, some(err)))
          end
          current-results := results-before
        end
      end
    end,
    check-is(self, code, left, right, loc):
      for left-right-check(loc, code)(lv from left, rv from right):
        check-bool(loc, code,
          lv == rv,
          lam(): failure-not-equal(loc, code, none, lv, rv) end)
      end
      nothing
    end,
    check-is-not(self, code, left, right, loc):
      for left-right-check(loc, code)(lv from left, rv from right):
        check-bool(loc, code,
          not(lv == rv),
          lam(): failure-not-different(loc, code, none, lv, rv) end)
      end
      nothing
    end,
    check-is-refinement(self, code, refinement, left, right, loc):
      for left-right-check(loc, code)(lv from left, rv from right):
        test-result = refinement(lv, rv)
        if not(is-boolean(test-result)):
          add-result(error-not-boolean(loc, code, refinement, lv, rv, test-result))
        else:
          check-bool(loc, code, test-result,
            lam(): failure-not-equal(loc, code, some(refinement), lv, rv) end)
        end
      end
      nothing
    end,
    check-is-not-refinement(self, code, refinement, left, right, loc):
      for left-right-check(loc, code)(lv from left, rv from right):
        test-result = refinement(lv, rv)
        if not(is-boolean(test-result)):
          add-result(error-not-boolean(loc, code, refinement, lv, rv, test-result))
        else:
          check-bool(loc, code, not(test-result),
            lam(): failure-not-different(loc, code, some(refinement), lv, rv) end)
        end
      end
      nothing
    end,
    check-satisfies-delayed(self, code, left, pred, loc):
      for left-right-check(loc, code)(lv from left, pv from pred):
        check-bool(loc, code,
          pv(lv),
          lam(): failure-not-satisfied(loc, code, lv, pv) end)
      end
      nothing
    end,
    check-satisfies-not-delayed(self, code, left, pred, loc):
      for left-right-check(loc, code)(lv from left, pv from pred):
        check-bool(loc, code,
          not(pv(lv)),
          lam(): failure-not-dissatisfied(loc, code, lv, pv) end)
      end
      nothing
    end,
    check-satisfies(self, code, left, pred, loc):
      check-bool(loc, code,
        pred(left),
        lam(): failure-not-satisfied(loc, code, left, pred) end)
      nothing
    end,
    check-satisfies-not(self, code, left, pred, loc):
      check-bool(loc, code,
        not(pred(left)),
        lam(): failure-not-dissatisfied(loc, code, left, pred) end)
      nothing
    end,
    check-raises(self, code, thunk, expected, comparator, on-failure, loc):
      result = run-task(thunk)
      cases(Either) result:
        | left(v) => add-result(failure-no-exn(loc, code, some(expected)))
        | right(v) =>
          if comparator(exn-unwrap(v), expected):
            add-result(success(loc, code))
          else:
            add-result(on-failure(exn-unwrap(v)))
          end
      end
      nothing
    end,
    check-raises-str(self, code, thunk, str, loc):
      self.check-raises(code, thunk, str,
        lam(exn, s): string-contains(torepr(exn), s) end,
        lam(exn): failure-wrong-exn(loc, code, str, exn) end,
        loc)
      nothing
    end,
    check-raises-other-str(self, code, thunk, str, loc):
      self.check-raises(code, thunk, str,
        lam(exn, s): not(string-contains(torepr(exn), s)) end,
        lam(exn): failure-right-exn(loc, code, str, exn) end,
        loc)
      nothing
    end,
    check-raises-not(self, code, thunk, loc):
      add-result(
        cases(Either) run-task(thunk):
          | left(v)    => success(loc, code)
          | right(exn) => failure-exn(loc, code, exn-unwrap(exn))
        end)
      nothing
    end,
    check-raises-satisfies(self, code, thunk, pred, loc):
      add-result(
        cases(Either) run-task(thunk):
          | left(v)    => failure-no-exn(loc, code, none)
          | right(exn) =>
            if pred(exn-unwrap(exn)):
              success(loc, code)
            else:
              failure-raise-not-satisfied(loc, code, exn-unwrap(exn), pred)
            end
        end)
      nothing
    end,
    check-raises-violates(self, code, thunk, pred, loc):
      add-result(
        cases(Either) run-task(thunk):
          | left(v)    => failure-no-exn(loc, code, none)
          | right(exn) =>
            if not(pred(exn-unwrap(exn))):
              success(loc, code)
            else:
              failure-raise-not-dissatisfied(loc, code, exn-unwrap(exn), pred)
            end
        end)
      nothing
    end,
    summary(self):
      results-summary(block-results)
    end,
    results(self):
      block-results
    end,
    render(self):
      render-check-results(block-results)
    end
  }
end

fun results-summary(block-results :: List<CheckBlockResult>):
  init = {
      message: "",
      errored: 0,
      passed: 0,
      failed: 0,
      total: 0
    }
  complete-summary = for fold(summary from init, br from block-results.reverse()):
    block-summary = for fold(s from init, tr from br.test-results.reverse()):
      cases(TestResult) tr:
        | success(loc, code) => s.{
            message: s.message + "\n  " + loc.format(false) + ": ok",
            passed: s.passed + 1,
            total: s.total + 1
          }
        | else =>
          m = s.message + "\n  " + tr.loc.format(false) + ": failed because: \n    "
            + RED.display-to-string(tr.render-reason(), torepr, empty)
          s.{
            message: m,
            failed: s.failed + 1,
            total: s.total + 1
          }
      end
    end
    ended-in-error = cases(Option) br.maybe-err:
      | none => ""
      | some(err) => "\n  Block ended in the following error (all tests may not have ran): \n\n  "
          + RED.display-to-string(exn-unwrap(err).render-reason(), torepr, empty) + "\n\n"
    end
    message = summary.message + "\n\n" + br.loc.format(true) + ": " + br.name + " (" + tostring(block-summary.passed) + "/" + tostring(block-summary.total) + ") \n"
    with-error-notification = message + ended-in-error
    rest-of-message =
      if block-summary.failed == 0: ""
      else: block-summary.message
      end
    {
      message: with-error-notification + rest-of-message,
      errored: summary.errored + if is-some(br.maybe-err): 1 else: 0 end,
      passed: summary.passed + block-summary.passed,
      failed: summary.failed + block-summary.failed,
      total: summary.total + block-summary.total
    }
  end
  if (complete-summary.total == 0) and (complete-summary.errored == 0):
    complete-summary.{message: "The program didn't define any tests."}
  else if (complete-summary.failed == 0) and (complete-summary.errored == 0):
    happy-msg = if complete-summary.passed == 1:
        "Looks shipshape, your test passed, mate!"
      else:
        "Looks shipshape, all " + tostring(complete-summary.passed) + " tests passed, mate!"
      end
    complete-summary.{message: happy-msg}
  else:
    c = complete-summary
    c.{
      message: c.message + "\n\nPassed: " + tostring(c.passed) + "; Failed: " + tostring(c.failed) + "; Ended in Error: " + tostring(c.errored) + "; Total: " + tostring(c.total) + "\n"
    }
  end
end

fun render-check-results(block-results :: List<CheckBlockResult>):
  results-summary(block-results).message
end

