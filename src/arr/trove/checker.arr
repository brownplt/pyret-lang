#lang pyret

provide *
provide-types *
import global as _
import srcloc as SL
import error-display as ED
import render-error-display as RED

import ffi as _

import either as either
import lists as lists
import option as option

type Either = either.Either

type List = lists.List
link = lists.link
empty = lists.empty
list = lists.list
is-empty = lists.is-empty
fold = lists.fold
each = lists.each

type Option = option.Option
is-some = option.is-some
some = option.some
none = option.none

type Loc = SL.Srcloc

# Copied from AST to break a module cycle
fun get-op-fun-name(opname):
  ask:
    | opname == "op==" then: "equal-always"
    | opname == "op=~" then: "equal-now"
    | opname == "op<=>" then: "identical"
    | otherwise: raise("Unknown op: " + opname)
  end
end

is-right = either.is-right
is-left = either.is-left

data CheckOperand:
  | on-left
  | on-right
  | on-refinement
end

data CheckBlockResult:
  | check-block-result(
      name :: String,
      loc :: Loc,
      test-results :: List<TestResult>,
      maybe-err :: Option<Any>
    )
end

fun report-value(operand, refinement, value):
  [ED.sequence:
    [ED.para:
      ED.text("The "),
      operand,
      ED.text(" evaluated to:")],
    ED.embed(value)]
end

data TestResult:
  | success(loc :: Loc)
  | failure-not-equal(loc :: Loc, refinement, left, right) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      test-ast = maybe-ast(self.loc).value
      lhs-ast = test-ast.left
      rhs-ast = test-ast.right.value
      ed-lhs = ED.highlight(ED.text("left operand"),  [ED.locs: lhs-ast.l], 0)
      ed-rhs = ED.highlight(ED.text("right operand"), [ED.locs: rhs-ast.l], 2)

      ed-op = cases(Option) test-ast.refinement:
        | none    =>
          ED.h-sequence(test-ast.op.tosource().pretty(80).map(ED.text),"")
        | some(e) =>
          [ED.sequence:
            ED.h-sequence(test-ast.op.tosource().pretty(80).map(ED.text),""),
            ED.text("%("),
            ED.highlight(ED.h-sequence(e.tosource().pretty(80).map(ED.text),""), [list: e.l ], 1),
            ED.text(")")]
      end

      [ED.error:
        [ED.para:
          ED.text("The binary test operator "),
          ED.code(ed-op),
          ED.text(" reported failure for the test ")],
         ED.cmcode(self.loc),
        [ED.para:
          cases(Any) test-ast.op:
            | s-op-is(_) => [ED.sequence:
              ED.text("because it reports success if and only if the predicate "),
              cases(Option) test-ast.refinement:
                | none => ED.code(ED.text("equal-always"))
                | some(e) => ED.highlight(ED.text("predicate"), [list: e.l], 1)
              end,
              ED.text(" is satisfied when the "),
               ed-lhs, ED.text(" and the "), ed-rhs, ED.text(" are applied to it.")]
            | s-op-is-op(_, op) =>
              [ED.sequence:
                ED.text("because it reports success if and only if the predicate "),
                get-op-fun-name(op), ED.text(" is satisfied when the "),
                ed-lhs, ED.text(" and the "), ed-rhs, ED.text(" are applied to it.")]
          end],
          report-value(ed-lhs, self.refinement, self.left),
          report-value(ed-rhs, self.refinement, self.right)]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para: cases(Option) self.refinement:
            | none    => ED.text("Values not equal")
            | some(_) => ED.text("Values not equal (using custom equality):")
          end],
        [ED.para: ED.embed(self.left)],
        [ED.para: ED.embed(self.right)]]
    end
  | failure-not-different(loc :: Loc, refinement, left, right) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      test-ast = maybe-ast(self.loc).value
      lhs-ast = test-ast.left
      rhs-ast = test-ast.right.value
      ed-lhs = ED.highlight(ED.text("left operand"),  [ED.locs: lhs-ast.l], 0)
      ed-rhs = ED.highlight(ED.text("right operand"), [ED.locs: rhs-ast.l], 2)

      ed-op = cases(Option) test-ast.refinement:
        | none    =>
          ED.h-sequence(test-ast.op.tosource().pretty(80).map(ED.text),"")
        | some(e) =>
          [ED.sequence:
            ED.h-sequence(test-ast.op.tosource().pretty(80).map(ED.text),""),
            ED.text("%("),
            ED.highlight(ED.h-sequence(e.tosource().pretty(80).map(ED.text),""), [list: e.l ], 1),
            ED.text(")")]
      end

      [ED.error:
        [ED.para:
          ED.text("The binary test operator "),
          ED.code(ed-op),
          ED.text(" reported failure for the test ")],
         ED.cmcode(self.loc),
        [ED.para:
          cases(Any) test-ast.op:
            | s-op-is-not(_) => [ED.sequence:
              ED.text("because it reports success if and only if the predicate "),
              cases(Option) test-ast.refinement:
                | none => ED.code(ED.text("equal-always"))
                | some(e) => ED.highlight(ED.text("predicate"), [list: e.l], 1)
              end,
              ED.text(" is not satisfied when the "),
               ed-lhs, ED.text(" and the "), ed-rhs, ED.text(" are applied to it.")]
            | s-op-is-not-op(_, op) => [ED.sequence:
              ED.text("because it reports success if and only if the predicate "),
              get-op-fun-name(op), ED.text(" is not satisfied when the "),
              ed-lhs, ED.text(" and the "), ed-rhs, ED.text(" are applied to it.")]
          end],
          report-value(ed-lhs, self.refinement, self.left),
          report-value(ed-rhs, self.refinement, self.right)]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para: cases(Option) self.refinement:
            | none    => ED.text("Values not different")
            | some(_) => ED.text("Values not different (using custom equality):")
          end],
        [ED.para: ED.embed(self.left)],
        [ED.para: ED.embed(self.right)]]
    end
  | failure-not-satisfied(loc :: Loc, val, pred) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      test-ast = maybe-ast(self.loc).value
      lhs-ast = test-ast.left
      rhs-ast = test-ast.right.value
      ed-lhs = ED.highlight(ED.text("left operand"),  [ED.locs: lhs-ast.l], 0)
      ed-rhs = ED.highlight(ED.text("predicate"), [ED.locs: rhs-ast.l], 2)

      [ED.error:
        [ED.para:
          ED.text("The binary test operator "),
          ED.code(ED.text("satisfies")),
          ED.text(" reported failure for the test ")],
         ED.cmcode(self.loc),
        [ED.para:
          ED.text("because it reports success if and only if the "),
          ed-rhs,
          ED.text(" is satisfied when the value of the "),
          ed-lhs,
          ED.text(" is applied to it. The value of the "),
          ed-lhs,
          ED.text(" is:")],
        ED.embed(self.val)]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para: ED.text("Predicate failed for value:")],
        [ED.para: ED.embed(self.val)]]
    end
  | failure-not-dissatisfied(loc :: Loc, val, pred) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      test-ast = maybe-ast(self.loc).value
      lhs-ast = test-ast.left
      rhs-ast = test-ast.right.value
      ed-lhs = ED.highlight(ED.text("left operand"),  [ED.locs: lhs-ast.l], 0)
      ed-rhs = ED.highlight(ED.text("predicate"), [ED.locs: rhs-ast.l], 2)

      [ED.error:
        [ED.para:
          ED.text("The binary test operator "),
          ED.code(ED.text("violates")),
          ED.text(" reported failure for the test ")],
         ED.cmcode(self.loc),
        [ED.para:
          ED.text("because it reports success if and only if the "),
          ed-rhs,
          ED.text(" is not satisfied when the value of the "),
          ed-lhs,
          ED.text(" is applied to it. The value of the "),
          ed-lhs,
          ED.text(" is:")],
        ED.embed(self.val)]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para: ED.text("Predicate succeeded for value (it should have failed):")],
        [ED.para: ED.embed(self.val)]]
    end
  | failure-wrong-exn(loc :: Loc, exn-expected, actual-exn) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      [ED.error:
        [ED.para: ED.text("Got unexpected exception ")],
        [ED.para: ED.embed(self.actual-exn)],
        [ED.para: ED.text("when expecting ")],
        [ED.para: ED.embed(self.exn-expected)]]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para: ED.text("Got unexpected exception ")],
        [ED.para: ED.embed(self.actual-exn)],
        [ED.para: ED.text("when expecting ")],
        [ED.para: ED.embed(self.exn-expected)]]
    end
  | failure-right-exn(loc :: Loc, exn-not-expected, actual-exn) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      [ED.error:
        [ED.para: ED.text("Got exception ")],
        [ED.para: ED.embed(self.actual-exn)],
        [ED.para: ED.text("and expected it not to contain ")],
        [ED.para: ED.embed(self.exn-not-expected)]]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para: ED.text("Got exception ")],
        [ED.para: ED.embed(self.actual-exn)],
        [ED.para: ED.text("and expected it not to contain ")],
        [ED.para: ED.embed(self.exn-not-expected)]]
    end
  | failure-exn(loc :: Loc, actual-exn, exn-place :: CheckOperand) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      test-ast = maybe-ast(self.loc).value
      lhs-ast = test-ast.left
      rhs-ast = test-ast.right.value
      [ED.error:
        [ED.para:
          ED.text("The testing statement")],
         ED.cmcode(self.loc),
        ED.paragraph(
          [list: ED.text("reported failure for the test, because it did not expect the evaluation of the ")] +
          cases(CheckOperand) self.exn-place:
            | on-left =>       [list: ED.highlight(ED.text("left operand"),  [ED.locs: lhs-ast.l], 0)]
            | on-right =>      [list: ED.highlight(ED.text("right operand"), [ED.locs: rhs-ast.l], 0)]
            | on-refinement =>
              cases(Option<Any>) test-ast.refinement: # Ought to be Option<A.Expr>
                | some(v) => [list: ED.highlight(ED.text("refinement"),   [ED.locs: v.l], 0)]
                # this branch shouldn't happen
                | none    => [list:
                                ED.text("predicate"),
                                ED.cmcode(self.loc)]
              end
          end + [list: ED.text(" to raise an exception:")]),
        ED.embed(self.actual-exn)]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para: ED.text("Got unexpected exception ")],
        [ED.para: ED.embed(self.actual-exn)]]
    end
  | failure-no-exn(loc :: Loc, exn-expected :: Option<String>) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      cases(Option) self.exn-expected:
        | some(exn) => [ED.error: [ED.para: ED.text("No exception raised, expected"), ED.embed(exn)]]
        | none      => [ED.error: [ED.para: ED.text("No exception raised")]]
      end
    end,
    method render-reason(self):
      cases(Option) self.exn-expected:
        | some(exn) => [ED.error: [ED.para: ED.text("No exception raised, expected"), ED.embed(exn)]]
        | none      => [ED.error: [ED.para: ED.text("No exception raised")]]
      end
    end
  | failure-raise-not-satisfied(loc :: Loc, exn, pred) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      test-ast = maybe-ast(self.loc).value
      lhs-ast = test-ast.left
      rhs-ast = test-ast.right.value
      ed-lhs = ED.highlight(ED.text("left operand"),  [ED.locs: lhs-ast.l], 0)
      ed-rhs = ED.highlight(ED.text("predicate"), [ED.locs: rhs-ast.l], 2)

      [ED.error:
        [ED.para:
          ED.text("The binary test operator "),
          ED.code(ED.text("raises-satisfies")),
          ED.text(" reported failure for the test ")],
         ED.cmcode(self.loc),
        [ED.para:
          ED.text("because it reports success if and only if the "),
          ed-rhs,
          ED.text(" is satisfied when the value of the exception raised by the "),
          ed-lhs,
          ED.text(" is applied to it. The value of the "),
          ed-lhs,
          ED.text(" is:")],
        ED.embed(self.exn)]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para: ED.text("Predicate failed for exception:")],
        [ED.para: ED.embed(self.exn)]]
    end
  | failure-raise-not-dissatisfied(loc :: Loc, exn, pred) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      test-ast = maybe-ast(self.loc).value
      lhs-ast = test-ast.left
      rhs-ast = test-ast.right.value
      ed-lhs = ED.highlight(ED.text("left operand"),  [ED.locs: lhs-ast.l], 0)
      ed-rhs = ED.highlight(ED.text("predicate"), [ED.locs: rhs-ast.l], 2)

      [ED.error:
        [ED.para:
          ED.text("The binary test operator "),
          ED.code(ED.text("raises-satisfies")),
          ED.text(" reported failure for the test ")],
         ED.cmcode(self.loc),
        [ED.para:
          ED.text("because it reports success if and only if the "),
          ed-rhs,
          ED.text(" is not satisfied when the value of the exception raised by the "),
          ed-lhs,
          ED.text(" is applied to it. The value of the "),
          ed-lhs,
          ED.text(" is:")],
        ED.embed(self.exn)]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para: ED.text("Predicate succeeded for exception (it should have failed):")],
        [ED.para: ED.embed(self.exn)]]
    end
  # This is not so much a test result as an error in a test case:
  # Maybe pull it out in the future?
  | error-not-boolean(loc :: Loc, refinement, left, righ, test-result) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      [ED.error:
        [ED.para: ED.text("The custom equality funtion must return a boolean, but instead it returned: ")],
        [ED.para: ED.embed(self.test-result)]]
    end,
    method render-reason(self):
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
  fun left-right-check(loc):
    lam(with-vals, left, right):
      lv = run-task(if is-function(left): left else: left.v end)
      if is-right(lv):  add-result(failure-exn(loc, lv.v,  on-left))
      else:
        rv = run-task(if is-function(right): right else: right.v end)
        if is-right(rv):  add-result(failure-exn(loc, rv.v,  on-right))
        else:
          res = run-task(lam(): with-vals(lv.v, rv.v) end)
          if is-right(res): add-result(failure-exn(loc, res.v, on-refinement))
          else: res.v
          end
        end
      end
    end
  end
  fun check-bool(loc, test-result, on-failure):
    if test-result:
      add-result(success(loc))
    else:
      add-result(on-failure())
    end
  end
  fun reset-results(): current-results := [list: ] end
  {
    method run-checks(self, module-name, checks):
      when check-all or (module-name == main-module-name):
        for each(c from checks) block:
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
    method check-is(self, left, right, loc) block:
      for left-right-check(loc)(lv from left, rv from right):
        check-bool(loc,
          lv == rv,
          lam(): failure-not-equal(loc, none, lv, rv) end)
      end
      nothing
    end,
    method check-is-not(self, left, right, loc) block:
      for left-right-check(loc)(lv from left, rv from right):
        check-bool(loc,
          not(lv == rv),
          lam(): failure-not-different(loc, none, lv, rv) end)
      end
      nothing
    end,
    method check-is-refinement(self, refinement, left, right, loc) block:
      for left-right-check(loc)(lv from left, rv from right):
        test-result = refinement(lv, rv)
        if not(is-boolean(test-result)):
          add-result(error-not-boolean(loc, refinement, lv, rv, test-result))
        else:
          check-bool(loc, test-result,
            lam(): failure-not-equal(loc, some(refinement), lv, rv) end)
        end
      end
      nothing
    end,
    method check-is-not-refinement(self, refinement, left, right, loc) block:
      for left-right-check(loc)(lv from left, rv from right):
        test-result = refinement(lv, rv)
        if not(is-boolean(test-result)):
          add-result(error-not-boolean(loc, refinement, lv, rv, test-result))
        else:
          check-bool(loc, not(test-result),
            lam(): failure-not-different(loc, some(refinement), lv, rv) end)
        end
      end
      nothing
    end,
    method check-satisfies-delayed(self, left, pred, loc) block:
      for left-right-check(loc)(lv from left, pv from pred):
        check-bool(loc,
          pv(lv),
          lam(): failure-not-satisfied(loc, lv, pv) end)
      end
      nothing
    end,
    method check-satisfies-not-delayed(self, left, pred, loc) block:
      for left-right-check(loc)(lv from left, pv from pred):
        check-bool(loc,
          not(pv(lv)),
          lam(): failure-not-dissatisfied(loc, lv, pv) end)
      end
      nothing
    end,
    method check-satisfies(self, left, pred, loc) block:
      check-bool(loc,
        pred(left),
        lam(): failure-not-satisfied(loc, left, pred) end)
      nothing
    end,
    method check-satisfies-not(self, left, pred, loc) block:
      check-bool(loc,
        not(pred(left)),
        lam(): failure-not-dissatisfied(loc, left, pred) end)
      nothing
    end,
    method check-raises(self, thunk, expected, comparator, on-failure, loc) block:
      result = run-task(thunk)
      cases(Either) result:
        | left(v) => add-result(failure-no-exn(loc, some(expected)))
        | right(v) =>
          if comparator(exn-unwrap(v), expected):
            add-result(success(loc))
          else:
            add-result(on-failure(exn-unwrap(v)))
          end
      end
      nothing
    end,
    method check-raises-str(self, thunk, str, loc) block:
      self.check-raises(thunk, str,
        lam(exn, s): string-contains(torepr(exn), s) end,
        lam(exn): failure-wrong-exn(loc, str, exn) end,
        loc)
      nothing
    end,
    method check-raises-other-str(self, thunk, str, loc) block:
      self.check-raises(thunk, str,
        lam(exn, s): not(string-contains(torepr(exn), s)) end,
        lam(exn): failure-right-exn(loc, str, exn) end,
        loc)
      nothing
    end,
    method check-raises-not(self, thunk, loc) block:
      add-result(
        cases(Either) run-task(thunk):
          | left(v)    => success(loc)
          | right(exn) => failure-exn(loc, exn-unwrap(exn), true)
        end)
      nothing
    end,
    method check-raises-satisfies(self, thunk, pred, loc) block:
      add-result(
        cases(Either) run-task(thunk):
          | left(v)    => failure-no-exn(loc, none)
          | right(exn) =>
            if pred(exn-unwrap(exn)):
              success(loc)
            else:
              failure-raise-not-satisfied(loc, exn-unwrap(exn), pred)
            end
        end)
      nothing
    end,
    method check-raises-violates(self, thunk, pred, loc) block:
      add-result(
        cases(Either) run-task(thunk):
          | left(v)    => failure-no-exn(loc, none)
          | right(exn) =>
            if not(pred(exn-unwrap(exn).value)):
              success(loc)
            else:
              failure-raise-not-dissatisfied(loc, exn-unwrap(exn), pred)
            end
        end)
      nothing
    end,
    method summary(self):
      results-summary(block-results)
    end,
    method results(self):
      block-results
    end,
    method render(self):
      render-check-results(block-results)
    end
  }
end

# NOTE(joe): get-stack lets us hide the stack from Pyret's semantics, and
# require that magical callers provide a get-stack function that produces
# the list of locations to render
fun results-summary(block-results :: List<CheckBlockResult>, get-stack):
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
        | success(loc) => s.{
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
      | some(err) =>
        stack = get-stack(err)
        "\n  Block ended in the following error (all tests may not have ran): \n\n  "
          + RED.display-to-string(exn-unwrap(err).render-reason(), torepr, stack)
          + RED.display-to-string(ED.v-sequence(lists.map(ED.loc, stack)), torepr, empty)
          + "\n\n"
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

fun render-check-results(block-results):
  results-summary(block-results, lam(err): empty end).message
end

fun render-check-results-stack(block-results :: List<CheckBlockResult>, get-stack):
  results-summary(block-results, get-stack)
end
