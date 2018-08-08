#lang pyret

provide *
provide-types *
import global as _
import srcloc as SL
import error as E
import error-display as ED
import render-error-display as RED
import valueskeleton as VS
include equality

import ffi as _

import either as either
include lists
include option

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

data CheckOperand:
  | on-left with:
    method side(self): "left side" end,
    method test-component(self, test-ast): test-ast.left end,
  | on-right with:
    method side(self): "right side" end,
    method test-component(self, test-ast): test-ast.right.value end,
  | on-refinement with:
    method side(self): "refinement" end,
    method test-component(self, test-ast): test-ast.refinement.value end,
  | on-cause with:
    method side(self): "explanation" end,
    method test-component(self, test-ast): test-ast.cause.value end,
sharing:
  method test-op(self, test-ast):
    cases(CheckOperand) self:
      | on-cause => ED.code(ED.text("because"))
      | else =>
        cases(Option) test-ast.refinement:
          | none    =>
            ED.h-sequence(test-ast.op.tosource().pretty(80).map(ED.text),"")
          | some(e) =>
            [ED.sequence:
              ED.h-sequence(test-ast.op.tosource().pretty(80).map(ED.text),""),
              ED.text("%("),
              ED.highlight(ED.h-sequence(e.tosource().pretty(80).map(ED.text),""), [list: e.l ], 1),
              ED.text(")")]
        end
    end
  end,
  method test-preamble(self, test-ast):
    cases(CheckOperand) self:
      | on-cause =>
        [ED.para:
          ED.text("The test was inconsistent: the expected answer and the explanation do not match for the test:")]
      | else =>
        [ED.para:
          ED.text("The test operator "),
          ED.code(self.test-op(test-ast)),
          ED.text(" failed for the test:")]
    end
  end
end

data CheckBlockResult:
  | check-block-result(
      name :: String,
      loc :: Loc,
      keyword-check :: Boolean,
      test-results :: List<TestResult>,
      maybe-err :: Option<Any>
    )
end

fun report-value(operand, value):
  [ED.sequence:
    [ED.para:
      ED.text("The "),
      operand,
      ED.text(" was:")],
    ED.embed(value)]
end

data TestResult:
  | success(loc :: Loc)
  | failure-not-equal(loc :: Loc, refinement, left, left-src, right, right-src) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      if self.loc.is-builtin():
        self.render-reason()
      else if src-available(self.loc):
        cases(Option) maybe-ast(self.loc):
          | some(test-ast) =>
            lhs-ast = self.left-src.test-component(test-ast)
            rhs-ast = self.right-src.test-component(test-ast)
            ed-lhs = ED.highlight(ED.text(self.left-src.side()),  [ED.locs: lhs-ast.l], 3)
            ed-rhs = ED.highlight(ED.text(self.right-src.side()), [ED.locs: rhs-ast.l], 4)
            ed-op = self.left-src.test-op(test-ast)
            [ED.error:
              self.left-src.test-preamble(test-ast),
              ED.cmcode(self.loc),
              [ED.para:
                cases(Any) test-ast.op:
                  | s-op-is-roughly(_) =>
                    [ED.sequence:
                      ED.text("It succeeds only if the "),
                      ed-lhs, ED.text(" and "), ed-rhs, ED.text(" are equal (allowing for rough equality).")]
                  | s-op-is(_) =>
                    cases(Option) test-ast.refinement:
                      | none =>
                        [ED.sequence:
                          ED.text("It succeeds only if the "),
                          ed-lhs, ED.text(" and "), ed-rhs, ED.text(" are equal.")]
                      | some(e) =>
                        [ED.sequence:
                          ED.text("It succeeds only if the "),
                          ED.highlight(ED.text("predicate"), [list: e.l], 1),
                          ED.text(" is satisfied when the "),
                          ed-lhs, ED.text(" and "), ed-rhs,
                          ED.text(" are applied to it.")]
                    end
                  | s-op-is-op(_, op) =>
                    [ED.sequence:
                      ED.text("It succeeds only if the predicate "),
                      ED.code(ED.text(get-op-fun-name(op))),
                      ED.text(" is satisfied when the "),
                      ed-lhs, ED.text(" and "), ed-rhs,
                      ED.text(" are applied to it.")]
                end],
                report-value(ed-lhs, self.left),
                report-value(ed-rhs, self.right)]
          | none      => self.render-reason()
        end
      else:
        self.render-reason()
      end
    end,
    method render-reason(self):
      [ED.error:
        [ED.para: cases(Option) self.refinement:
            | none    => ED.text("Values not equal")
            | some(_) => ED.text("Values not equal (using custom equality):")
          end],
        ED.embed(self.left),
        ED.embed(self.right)]
    end
  | failure-is-incomparable(loc :: Loc, eq-result :: EqualityResult%(is-Unknown), left, left-src, right, right-src) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      if self.loc.is-builtin():
        self.render-reason()
      else if src-available(self.loc):
        cases(Option) maybe-ast(self.loc):
          | some(test-ast) =>
            lhs-ast = self.left-src.test-component(test-ast)
            rhs-ast = self.right-src.test-component(test-ast)
            ed-lhs = ED.highlight(ED.text(self.left-src.side()),  [ED.locs: lhs-ast.l], 3)
            ed-rhs = ED.highlight(ED.text(self.right-src.side()), [ED.locs: rhs-ast.l], 4)
            ed-op = self.left-src.test-op(test-ast)
            within-name = cases(Any) test-ast.op:
              | s-op-is-op(_, op) =>
                ask:
                  | op == "op=~"  then: "within-now"
                  | otherwise: "within"
                end
              | s-op-is-not-op(_, op) =>
                ask:
                  | op == "op=~"  then: "within-now"
                  | otherwise: "within"
                end
              | else => "within"
            end
            use-within = [ED.para:
              ED.text("Use "), ED.code(ED.text("is-roughly")),
              ED.text(" as the testing operator, or consider using the "),
              ED.code(ED.text(within-name)), ED.text(" function to compare them instead.")
            ]
            {msg1; msg2} = ask:
              | is-function(self.eq-result.value1) and is-function(self.eq-result.value2) then:
                {"Attempted to compare two Functions for equality, which is not allowed:";
                  [ED.para: ED.text("Did you mean to call them first?")]}
              | is-function(self.eq-result.value1) or is-function(self.eq-result.value2) then:
                {"Attempted to compare a Function to another value for equality:";
                  [ED.para: ED.text("Did you mean to call the function first?")]}
              | num-is-roughnum(self.eq-result.value1) and num-is-roughnum(self.eq-result.value2) then:
                {"Attempted to compare two Roughnums for equality, which is not allowed:"; use-within}
              | num-is-roughnum(self.eq-result.value1) then:
                {"Attempted to compare a Roughnum to an Exactnum for equality, which is not allowed:"; use-within}
              | otherwise:
                {"Attempted to compare an Exactnum to a Roughnum for equality, which is not allowed:"; use-within}
            end
            [ED.error:
              self.left-src.test-preamble(test-ast),
              ED.cmcode(self.loc),
              [ED.para: ED.text(msg1)],
              report-value(ed-lhs, self.left),
              report-value(ed-rhs, self.right),
              msg2]
          | none      => self.render-reason()
        end
      else:
        self.render-reason()
      end
    end,
    method render-reason(self):
      ask:
        | is-function(self.eq-result.value1) and is-function(self.eq-result.value2) then:
          [ED.error:
            [ED.para:
              ED.text("Attempted to compare two functions using strict equality: did you mean to call them first?")
            ],
            ED.embed(self.left),
            ED.embed(self.right)]
        | is-function(self.eq-result.value1) or is-function(self.eq-result.value2) then:
          [ED.error:
            [ED.para:
              ED.text("Attempted to compare a function to another value using strict equality: did you mean to call the function first?")
            ],
            ED.embed(self.left),
            ED.embed(self.right)]
        | otherwise:
          [ED.error:
            [ED.para:
              ED.text("Attempted to compare roughnums using strict equality: use "), ED.code(ED.text("is-roughly")),
              ED.text(", or consider using the"),
              ED.code(ED.text("within")), ED.text(" function to compare them instead.")
            ],
            ED.embed(self.left),
            ED.embed(self.right)]
      end
    end
  | failure-not-different(loc :: Loc, refinement, left, left-src, right, right-src) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      if self.loc.is-builtin():
        self.render-reason()
      else if src-available(self.loc):
        cases(Option) maybe-ast(self.loc):
          | some(test-ast) =>
            lhs-ast = self.left-src.test-component(test-ast)
            rhs-ast = self.right-src.test-component(test-ast)
            ed-lhs = ED.highlight(ED.text(self.left-src.side()),  [ED.locs: lhs-ast.l], 3)
            ed-rhs = ED.highlight(ED.text(self.right-src.side()), [ED.locs: rhs-ast.l], 4)
            ed-op = self.left-src.test-op(test-ast)
            [ED.error:
              self.left-src.test-preamble(test-ast),
              ED.cmcode(self.loc),
              [ED.para:
                cases(Any) test-ast.op:
                  | s-op-is-not(_) =>
                    cases(Option) test-ast.refinement:
                      | none =>
                        [ED.sequence:
                          ED.text("It succeeds only if the "),
                          ed-lhs, ED.text(" and "), ed-rhs, ED.text(" sides are not equal.")]
                      | some(e) =>
                        [ED.sequence:
                          ED.text("It succeeds only if the "),
                          ED.highlight(ED.text("predicate"), [list: e.l], 1),
                          ED.text(" is not satisfied when the "),
                          ed-lhs, ED.text(" and "), ed-rhs,
                          ED.text(" sides are applied to it.")]
                    end
                  | s-op-is-not-op(_, op) =>
                    [ED.sequence:
                      ED.text("It succeeds only if the predicate "),
                      ED.code(ED.text(get-op-fun-name(op))),
                      ED.text(" is not satisfied when the "),
                      ed-lhs, ED.text(" and "), ed-rhs,
                      ED.text(" sides are applied to it.")]
                end],
                report-value(ed-lhs, self.left),
                report-value(ed-rhs, self.right)]
          | none      => self.render-reason()
        end
      else:
        self.render-reason()
      end
    end,
    method render-reason(self):
      [ED.error:
        [ED.para: cases(Option) self.refinement:
            | none    => ED.text("Values not different")
            | some(_) => ED.text("Values not different (using custom equality):")
          end],
        ED.embed(self.left),
        ED.embed(self.right)]
    end
  | failure-not-satisfied(loc :: Loc, val, val-src, pred) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      if self.loc.is-builtin():
        self.render-reason()
      else if src-available(self.loc):
        cases(Option) maybe-ast(self.loc):
          | some(test-ast) =>
            lhs-ast = self.val-src.test-component(test-ast)
            rhs-ast = test-ast.right.value
            ed-lhs = ED.highlight(ED.text(self.val-src.side()),  [ED.locs: lhs-ast.l], 3)
            ed-rhs = ED.highlight(ED.text("predicate"), [ED.locs: rhs-ast.l], 4)

            [ED.error:
              self.val-src.test-preamble(test-ast),
              ED.cmcode(self.loc),
              [ED.para:
                ED.text("It succeeds only if the "),
                ed-rhs,
                ED.text(" is satisfied when applied to the "),
                ed-lhs,
                ED.text(". The "),
                ed-lhs,
                ED.text(" is:")],
              ED.embed(self.val)]
          | none => self.render-reason()
        end
      else:
        self.render-reason()
      end
    end,
    method render-reason(self):
      [ED.error:
        [ED.para: ED.text("Predicate failed for value:")],
        ED.embed(self.val)]
    end
  | failure-not-dissatisfied(loc :: Loc, val, val-src, pred) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      if self.loc.is-builtin():
        self.render-reason()
      else if src-available(self.loc):
        cases(Option) maybe-ast(self.loc):
          | some(test-ast) =>
            lhs-ast = self.val-src.test-component(test-ast)
            rhs-ast = test-ast.right.value
            ed-lhs = ED.highlight(ED.text(self.val-src.side()),  [ED.locs: lhs-ast.l], 3)
            ed-rhs = ED.highlight(ED.text("predicate"), [ED.locs: rhs-ast.l], 4)
            [ED.error:
              self.val-src.test-preamble(test-ast),
              ED.cmcode(self.loc),
              [ED.para:
                ED.text("It succeeds only if the "),
                ed-rhs,
                ED.text(" is not satisfied when the value of the "),
                ed-lhs,
                ED.text(" is applied to it. The value of the "),
                ed-lhs,
                ED.text(" was:")],
              ED.embed(self.val)]
          | none =>
            self.render-reason()
        end
      else:
        self.render-reason()
      end
    end,
    method render-reason(self):
      [ED.error:
        [ED.para: ED.text("Predicate succeeded for value (it should have failed):")],
        ED.embed(self.val)]
    end
  | failure-wrong-exn(loc :: Loc, exn-expected, actual-exn, actual-src) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      if self.loc.is-builtin():
        self.render-reason()
      else if src-available(self.loc):
        cases(Option) maybe-ast(self.loc):
          | some(test-ast) =>
            [ED.error:
              self.actual-src.test-preamble(test-ast),
              ED.cmcode(self.loc),
              [ED.para: ED.text("Got unexpected exception ")],
              ED.embed(self.actual-exn),
              [ED.para: ED.text("when expecting ")],
              ED.embed(self.exn-expected)]
          | none => self.render-reason()
        end
      else:
        self.render-reason()
      end
    end,
    method access-stack(self, get-stack):
      get-stack(self.actual-exn)
    end,
    method render-reason(self):
      [ED.error:
        [ED.para: ED.text("Got unexpected exception ")],
        ED.embed(self.actual-exn),
        [ED.para: ED.text("when expecting ")],
        ED.embed(self.exn-expected)]
    end,
    method _output(self):
      VS.vs-constr("failure-wrong-exn",
        [list:
          VS.vs-value(self.loc),
          VS.vs-value(self.exn-expected),
          VS.vs-value(exn-unwrap(self.actual-exn))])
    end,
  | failure-right-exn(loc :: Loc, exn-not-expected, actual-exn, actual-src) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      if self.loc.is-builtin():
        self.render-reason()
      else if src-available(self.loc):
        cases(Option) maybe-ast(self.loc):
          | some(test-ast) =>
            [ED.error:
              self.actual-src.test-preamble(test-ast),
              ED.cmcode(self.loc),
              [ED.para: ED.text("Got exception ")],
              ED.embed(self.actual-exn),
              [ED.para: ED.text("and expected it not to contain ")],
              ED.embed(self.exn-not-expected)]
          | none => self.render-reason()
        end
      else:
        self.render-reason()
      end
    end,
    method render-reason(self):
      [ED.error:
        [ED.para: ED.text("Got exception ")],
        ED.embed(self.actual-exn),
        [ED.para: ED.text("and expected it not to contain ")],
        ED.embed(self.exn-not-expected)]
    end,
    method _output(self):
      VS.vs-constr("failure-right-exn",
        [list:
          VS.vs-value(self.loc),
          VS.vs-value(self.exn-not-expected),
          VS.vs-value(exn-unwrap(self.actual-exn))])
    end,
  | failure-exn(loc :: Loc, actual-exn, exn-place :: CheckOperand) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      if self.loc.is-builtin():
        self.render-reason()
      else if src-available(self.loc):
        cases(Option) maybe-ast(self.loc):
          | some(test-ast) =>
            lhs-ast = self.exn-place.test-component(test-ast)
            [ED.error:
              self.exn-place.test-preamble(test-ast),
              ED.cmcode(self.loc),
              ED.paragraph(
                [list: ED.text("It did not expect the evaluation of the ")] +
                cases(CheckOperand) self.exn-place:
                  | on-left =>     [list: ED.highlight(ED.text("left side"),  [ED.locs: lhs-ast.l], -3)]
                  | on-right =>    [list: ED.highlight(ED.text("right side"), [ED.locs: test-ast.right.value.l], -3)]
                  | on-cause =>    [list: ED.highlight(ED.text("explanation"), [ED.locs: test-ast.cause.value.l], -3)]
                  | on-refinement =>
                    cases(Option<Any>) test-ast.refinement: # Ought to be Option<A.Expr>
                      | some(v) => [list: ED.highlight(ED.text("refinement"),   [ED.locs: v.l], -3)]
                      | none    => 
                        [list: ED.highlight(ED.text("predicate"),  [ED.locs: test-ast.right.value.l], -3)]
                    end
                end + [list: ED.text(" to raise an exception:")]),
              ED.embed(self.actual-exn)]
          | none => self.render-reason()
        end
      else:
        self.render-reason()
      end
    end,
    method render-reason(self):
      [ED.error:
        [ED.para: ED.text("Got unexpected exception ")],
        ED.embed(self.actual-exn)]
    end,
    method access-stack(self, get-stack) block:
      # print("The stack is being accessed")
      # print(get-stack(self.actual-exn))
      get-stack(self.actual-exn)
    end,
    method _output(self):
      VS.vs-constr("failure-exn",
        [list:
          VS.vs-value(self.loc),
          VS.vs-value(exn-unwrap(self.actual-exn)),
          VS.vs-value(self.exn-place)])
    end,
  | failure-no-exn(loc :: Loc, exn-expected :: Option<String>, exn-src, wanted :: Boolean) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      if self.loc.is-builtin() block:
        self.render-reason()
      else if src-available(self.loc):
        cases(Option) maybe-ast(self.loc):
          | some(test-ast) =>
            exn-ast = self.exn-src.test-component(test-ast)
            ed-exn = ED.highlight(ED.text(self.exn-src.side()), [ED.locs: exn-ast.l], 3)
            cases(Option) self.exn-expected:
              | some(exn) =>
                [ED.error:
                  self.exn-src.test-preamble(test-ast),
                  ED.cmcode(self.loc),
                  [ED.para:
                    ED.text("No exception raised while evaluating the "), ed-exn,
                    if self.wanted:
                      ED.text(", expected")
                    else:
                      ED.text(", expected any exception other than")
                    end], ED.embed(exn)]
              | none =>
                [ED.error:
                  self.exn-src.test-preamble(test-ast),
                  ED.cmcode(self.loc),
                  [ED.para: ED.text("No exception raised while evaluating the "), ed-exn]]
            end
          | none => self.render-reason()
        end
      else:
        self.render-reason()
      end
    end,
    method render-reason(self):
      cases(Option) self.exn-expected:
        | some(exn) =>
          [ED.error:
            [ED.para: ED.text("No exception raised while evaluating the " + self.exn-src.side() +
                if self.wanted:
                  ", expected"
                else:
                  ", expected any exception other than"
                end)], ED.embed(exn)]
        | none =>
          [ED.error:
            [ED.para: ED.text("No exception raised while evaluating the " + self.exn-src.side())]]
      end
    end
  | failure-raise-not-satisfied(loc :: Loc, exn, exn-src, pred) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      if self.loc.is-builtin():
        self.render-reason()
      else if src-available(self.loc):
        cases(Option) maybe-ast(self.loc):
          | some(test-ast) =>
            lhs-ast = self.exn-src.test-component(test-ast)
            rhs-ast = test-ast.right.value
            ed-lhs = ED.highlight(ED.text(self.exn-src.side()),  [ED.locs: lhs-ast.l], 3)
            ed-rhs = ED.highlight(ED.text("predicate"), [ED.locs: rhs-ast.l], 4)

            [ED.error:
              self.exn-src.test-preamble(test-ast),
              ED.cmcode(self.loc),
              [ED.para:
                ED.text("It succeeds only if the "),
                ed-rhs,
                ED.text(" is satisfied when the value of the exception raised by the "),
                ed-lhs,
                ED.text(" is applied to it. The value of the "),
                ed-lhs,
                ED.text(" was:")],
              ED.embed(self.exn)]
          | none => self.render-reason()
        end
      else:
        self.render-reason()
      end
    end,
    method render-reason(self):
      [ED.error:
        [ED.para: ED.text("Predicate failed for exception:")],
        [ED.para: ED.embed(exn-unwrap(self.exn))]]
    end,
    method _output(self):
      VS.vs-constr("failure-raise-not-satisfied",
        [list:
          VS.vs-value(self.loc),
          VS.vs-value(exn-unwrap(self.exn)),
          VS.vs-value(self.pred)])
    end
  | failure-raise-not-dissatisfied(loc :: Loc, exn, exn-src, pred) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      if self.loc.is-builtin():
        self.render-reason()
      else if src-available(self.loc):
        cases(Option) maybe-ast(self.loc):
          | some(test-ast) =>
            lhs-ast = self.exn-src.test-component(test-ast)
            rhs-ast = test-ast.right.value
            ed-lhs = ED.highlight(ED.text(self.exn-src.side()),  [ED.locs: lhs-ast.l], 3)
            ed-rhs = ED.highlight(ED.text("predicate"), [ED.locs: rhs-ast.l], 4)

            [ED.error:
              [ED.para:
                self.exn-src.test-preamble(test-ast)],
                # ED.text("The test operator "),
                # ED.code(ED.text("raises-violates")),
                # ED.text(" failed for the test:")],
              ED.cmcode(self.loc),
              [ED.para:
                ED.text("It succeeds only if the "),
                ed-rhs,
                ED.text(" is not satisfied when the value of the exception raised by the "),
                ed-lhs,
                ED.text(" is applied to it. The value of the "),
                ed-lhs,
                ED.text(" is:")],
              ED.embed(self.exn)]
          | none => self.render-reason()
        end
      else:
        self.render-reason()
      end
    end,
    method render-reason(self):
      [ED.error:
        [ED.para: ED.text("Predicate succeeded for exception (it should have failed):")],
        [ED.para: ED.embed(exn-unwrap(self.exn))]]
    end,
    method _output(self):
      VS.vs-constr("failure-raise-not-dissatisfied",
        [list:
          VS.vs-value(self.loc),
          VS.vs-value(exn-unwrap(self.exn)),
          VS.vs-value(self.pred)])
    end
  # This is not so much a test result as an error in a test case:
  # Maybe pull it out in the future?
  | error-not-boolean(loc :: Loc, refinement, left, left-src, right, right-src, test-result) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      [ED.error:
        [ED.para: ED.text("The custom equality function must return a boolean, but instead it returned: ")],
        [ED.para: ED.embed(self.test-result)]]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para: ED.text("The custom equality function must return a boolean, but instead it returned: ")],
        [ED.para: ED.embed(self.test-result)]]
    end
  | error-not-pred(loc :: Loc, predicate, arity :: Number) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      if self.loc.is-builtin():
        self.render-reason()
      else if src-available(self.loc):
        cases(Option) maybe-ast(self.loc):
          | some(test-ast) =>
            pred-lhs = ED.highlight(ED.text("test predicate"),  [ED.locs: on-right.test-component(test-ast).l], 2)
            [ED.error:
              [ED.para: ED.text("The "), pred-lhs, ED.text(" must be a " + to-string(self.arity) + "-argument function that returns a boolean:")],
              ED.cmcode(self.loc),
              [ED.para: ED.text("Instead it was: "), ED.embed(self.predicate)]]
          | none =>
            [ED.error:
              [ED.para: ED.text("The test predicate must be a " + to-string(self.arity) + "-argument function that returns a boolean, but instead it was: ")],
              [ED.para: ED.embed(self.predicate)],
              ED.cmcode(self.loc)]
        end
      end
    end,
    method render-reason(self):
      [ED.error:
        [ED.para: ED.text("The test predicate must be a " + to-string(self.arity) + "-argument function that returns a boolean, but instead it was: ")],
        [ED.para: ED.embed(self.predicate)]]
    end
  | error-not-boolean-pred(loc :: Loc, predicate, left, left-src, test-result) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      if self.loc.is-builtin():
        self.render-reason()
      else if src-available(self.loc):
        cases(Option) maybe-ast(self.loc):
          | some(test-ast) =>
            lhs-ast = self.left-src.test-component(test-ast)
            ed-lhs = ED.highlight(ED.text(self.left-src.side()),  [ED.locs: lhs-ast.l], 3)
            pred-lhs = ED.highlight(ED.text("test predicate"),  [ED.locs: on-right.test-component(test-ast).l], 4)
            [ED.error:
              [ED.para: ED.text("The "), pred-lhs, ED.text(" must return a boolean:")],
              ED.cmcode(self.loc),
              [ED.para: ED.text("Instead it returned "), ED.embed(self.test-result),
                ED.text(" when applied to the "), ed-lhs]]
          | none =>
            [ED.error:
              [ED.para: ED.text("The test predicate must return a boolean, but instead it returned: ")],
              [ED.para: ED.embed(self.test-result)],
              [ED.para: ED.text("when applied to the " + self.left-src.side())],
              ED.cmcode(self.loc)]
        end
      end
    end,
    method render-reason(self):
      [ED.error:
        [ED.para: ED.text("The test predicate must return a boolean, but instead it returned: ")],
        [ED.para: ED.embed(self.test-result)]]
    end
sharing:
  # By default, just return an empty stack.  Override this in the cases
  # that actually have an exception to report
  method access-stack(self, stack-getter): empty end
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
      if either.is-right(lv):  add-result(failure-exn(loc, lv.v,  on-left))
      else:
        rv = run-task(if is-function(right): right else: right.v end)
        if either.is-right(rv):  add-result(failure-exn(loc, rv.v,  on-right))
        else:
          res = run-task(lam(): with-vals(lv.v, rv.v) end)
          if either.is-right(res): add-result(failure-exn(loc, res.v, on-refinement))
          else: res.v
          end
        end
      end
    end
  end
  fun left-right-cause-check(loc):
    lam(with-vals, left, right, cause):
      lv = run-task(if is-function(left): left else: left.v end)
      if either.is-right(lv):  add-result(failure-exn(loc, lv.v,  on-left))
      else:
        rv = run-task(if is-function(right): right else: right.v end)
        if either.is-right(rv):  add-result(failure-exn(loc, rv.v,  on-right))
        else:
          cv = run-task(if is-function(cause): cause else: cause.v end)
          if either.is-right(cv):  add-result(failure-exn(loc, cv.v, on-cause))
          else:
            res = run-task(lam(): with-vals(lv.v, rv.v, cv.v) end)
            if either.is-right(res): add-result(failure-exn(loc, res.v, on-refinement))
            else: res.v
            end
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
      when check-all or (module-name == main-module-name) block:
        for each(c from checks) block:
          results-before = current-results
          reset-results()
          result = run-task(c.run)
          cases(either.Either) result:
            | left(v) => add-block-result(check-block-result(c.name, c.location, c.keyword-check, current-results, none))
            | right(err) => add-block-result(check-block-result(c.name, c.location, c.keyword-check, current-results, some(err)))
          end
          current-results := results-before
        end
      end
    end,
    method check-is(self, left, right, loc) block:
      for left-right-check(loc)(lv from left, rv from right):
        eq-lv-rv = equal-always3(lv, rv)
        cases(EqualityResult) eq-lv-rv:
          | Unknown(_, _, _)  => add-result(failure-is-incomparable(loc, eq-lv-rv, lv, on-left, rv, on-right))
          | NotEqual(_, _, _) => add-result(failure-not-equal(loc, none, lv, on-left, rv, on-right))
          | Equal             => add-result(success(loc))
        end
      end
      nothing
    end,
    method check-is-cause(self, left, right, cause, loc) block:
      for left-right-cause-check(loc)(lv from left, rv from right, cv from cause):
        eq-cv-rv = equal-always3(cv, rv)
        cases(EqualityResult) eq-cv-rv:
          | Unknown(_, _, _)  => add-result(failure-is-incomparable(loc, eq-cv-rv, cv, on-cause, rv, on-right))
          | NotEqual(_, _, _) => add-result(failure-not-equal(loc, none, cv, on-cause, rv, on-right))
          | Equal =>
            eq-lv-rv = equal-always3(lv, rv)
            cases(EqualityResult) eq-lv-rv:
              | Unknown(_, _, _)  => add-result(failure-is-incomparable(loc, eq-lv-rv, lv, on-left, rv, on-right))
              | NotEqual(_, _, _) => add-result(failure-not-equal(loc, none, lv, on-left, rv, on-right))
              | Equal             => add-result(success(loc))
            end
        end
      end
      nothing
    end,
    method check-is-roughly(self, left, right, loc) block:
      for left-right-check(loc)(lv from left, rv from right):
        eq-lv-rv = builtins.within-rel3(~0.000001)(lv, rv)
        cases(EqualityResult) eq-lv-rv:
          | Unknown(_, _, _)  => add-result(failure-is-incomparable(loc, eq-lv-rv, lv, on-left, rv, on-right))
          | NotEqual(_, _, _) => add-result(failure-not-equal(loc, none, lv, on-left, rv, on-right))
          | Equal             => add-result(success(loc))
        end
      end
      nothing
    end,
    method check-is-roughly-cause(self, left, right, cause, loc) block:
      for left-right-cause-check(loc)(lv from left, rv from right, cv from cause):
        tol = ~0.000001
        eq-cv-rv = builtins.within-rel3(tol)(cv, rv) # Note: same order as within(tol)(lv, rv) below
        cases(EqualityResult) eq-cv-rv:
          | Unknown(_, _, _)  => add-result(failure-is-incomparable(loc, eq-cv-rv, cv, on-cause, rv, on-right))
          | NotEqual(_, _, _) => add-result(failure-not-equal(loc, none, cv, on-cause, rv, on-right))
          | Equal =>
            eq-lv-rv = builtins.within-rel3(tol)(lv, rv)
            cases(EqualityResult) eq-lv-rv:
              | Unknown(_, _, _)  => add-result(failure-is-incomparable(loc, eq-lv-rv, lv, on-left, rv, on-right))
              | NotEqual(_, _, _) => add-result(failure-not-equal(loc, none, lv, on-left, rv, on-right))
              | Equal             => add-result(success(loc))
            end
        end
      end
      nothing
    end,
    method check-is-not(self, left, right, loc) block:
      for left-right-check(loc)(lv from left, rv from right):
        eq-lv-rv = equal-always3(lv, rv)
        cases(EqualityResult) eq-lv-rv:
          | Unknown(_, _, _)  => add-result(failure-is-incomparable(loc, eq-lv-rv, lv, on-left, rv, on-right))
          | Equal             => add-result(failure-not-different(loc, none, lv, on-left, rv, on-right))
          | NotEqual(_, _, _) => add-result(success(loc))
        end
      end
      nothing
    end,
    method check-is-not-cause(self, left, right, cause, loc) block:
      for left-right-cause-check(loc)(lv from left, rv from right, cv from cause):
        eq-cv-rv = equal-always3(cv, rv) # Note: same order as lv == rv below
        cases(EqualityResult) eq-cv-rv:
          | Unknown(_, _, _)  =>  add-result(failure-is-incomparable(loc, eq-cv-rv, cv, on-cause, rv, on-right))
          | Equal             => add-result(failure-not-different(loc, none, cv, on-cause, rv, on-right))
          | NotEqual(_, _, _) =>
            eq-lv-rv = equal-always3(lv, rv)
            cases(EqualityResult) eq-lv-rv:
              | Unknown(_, _, _)  => add-result(failure-is-incomparable(loc, eq-lv-rv, lv, on-left, rv, on-right))
              | Equal             => add-result(failure-not-different(loc, none, lv, on-left, rv, on-right))
              | NotEqual(_, _, _) => add-result(success(loc))
            end
        end
      end
      nothing
    end,
    method check-is-refinement(self, refinement, left, right, loc) block:
      for left-right-check(loc)(lv from left, rv from right):
        cases(either.Either) run-task(lam(): refinement(lv, rv) end):
          | right(exn) =>
            exn-v = exn-unwrap(exn)
            if E.is-arity-mismatch(exn-v) or E.is-non-function-app(exn-v): add-result(error-not-pred(loc, refinement, 2))
            else: add-result(failure-exn(loc, exn, on-refinement))
            end
          | left(test-result) =>
            ask:
              | is-Unknown(test-result)     then: add-result(failure-is-incomparable(loc, test-result, lv, on-left, rv, on-right))
              | (test-result == false)
                or is-NotEqual(test-result) then: add-result(failure-not-equal(loc, some(refinement), lv, on-left, rv, on-right))
              | not(is-boolean(test-result)
                  or is-Equal(test-result)) then: add-result(error-not-boolean(loc, refinement, lv, on-left, rv, on-right, test-result))
              | otherwise:                        add-result(success(loc))
            end
        end
      end
      nothing
    end,
    method check-is-refinement-cause(self, refinement, left, right, cause, loc) block:
      for left-right-cause-check(loc)(lv from left, rv from right, cv from cause):
        cases(either.Either) run-task(lam(): refinement(cv, rv) end): # Same order as refinement(lv, rv)
          | right(exn) =>
            exn-v = exn-unwrap(exn)
            if E.is-arity-mismatch(exn-v) or E.is-non-function-app(exn-v): add-result(error-not-pred(loc, refinement, 2))
            else: add-result(failure-exn(loc, exn, on-refinement))
            end
          | left(cause-result) =>
            ask:
              | is-Unknown(cause-result)     then: add-result(failure-is-incomparable(loc, cause-result, cv, on-cause, rv, on-right))
              | (cause-result == false)
                or is-NotEqual(cause-result) then: add-result(failure-not-equal(loc, some(refinement), cv, on-cause, rv, on-right))
              | not(is-boolean(cause-result)
                  or is-Equal(cause-result)) then: add-result(error-not-boolean(loc, refinement, cv, on-cause, rv, on-right, cause-result))
              | otherwise:
                cases(either.Either) run-task(lam(): refinement(lv, rv) end):
                  | right(exn) =>
                    exn-v = exn-unwrap(exn)
                    if E.is-arity-mismatch(exn-v) or E.is-non-function-app(exn-v): add-result(error-not-pred(loc, refinement, 2))
                    else: add-result(failure-exn(loc, exn, on-refinement))
                    end
                  | left(test-result) =>
                    ask:
                      | is-Unknown(test-result)     then: add-result(failure-is-incomparable(loc, test-result, lv, on-left, rv, on-right))
                      | (test-result == false)
                        or is-NotEqual(test-result) then: add-result(failure-not-equal(loc, some(refinement), lv, on-left, rv, on-right))
                      | not(is-boolean(test-result)
                          or is-Equal(test-result)) then: add-result(error-not-boolean(loc, refinement, lv, on-left, rv, on-right, test-result))
                      | otherwise:                        add-result(success(loc))
                    end
                end
            end
        end
      end
      nothing
    end,
    method check-is-not-refinement(self, refinement, left, right, loc) block:
      for left-right-check(loc)(lv from left, rv from right):
        cases(either.Either) run-task(lam(): refinement(lv, rv) end):
          | right(exn) =>
            exn-v = exn-unwrap(exn)
            if E.is-arity-mismatch(exn-v) or E.is-non-function-app(exn-v): add-result(error-not-pred(loc, refinement, 2))
            else: add-result(failure-exn(loc, exn, on-refinement))
            end
          | left(test-result) =>
            ask:
              | is-Unknown(test-result)        then: add-result(failure-is-incomparable(loc, test-result, lv, on-left, rv, on-right))
              | (test-result == true)
                or is-Equal(test-result)       then: add-result(failure-not-different(loc, some(refinement), lv, on-left, rv, on-right))
              | not(is-boolean(test-result)
                  or is-NotEqual(test-result)) then: add-result(error-not-boolean(loc, refinement, lv, on-left, rv, on-right, test-result))
              | otherwise:                           add-result(success(loc))
            end
        end
      end
      nothing
    end,
    method check-is-not-refinement-cause(self, refinement, left, right, cause, loc) block:
      for left-right-cause-check(loc)(lv from left, rv from right, cv from cause):
        cases(either.Either) run-task(lam(): refinement(cv, rv) end): # Same order as refinement(lv, rv)
          | right(exn) =>
            exn-v = exn-unwrap(exn)
            if E.is-arity-mismatch(exn-v) or E.is-non-function-app(exn-v): add-result(error-not-pred(loc, refinement, 2))
            else: add-result(failure-exn(loc, exn, on-refinement))
            end
          | left(cause-result) =>
            ask:
              | is-Unknown(cause-result)        then: add-result(failure-is-incomparable(loc, cause-result, cv, on-cause, rv, on-right))
              | (cause-result == true)
                or is-Equal(cause-result)       then: add-result(failure-not-different(loc, some(refinement), cv, on-cause, rv, on-right))
              | not(is-boolean(cause-result)
                  or is-NotEqual(cause-result)) then: add-result(error-not-boolean(loc, refinement, cv, on-cause, rv, on-right, cause-result))
              | otherwise:
                cases(either.Either) run-task(lam(): refinement(lv, rv) end):
                  | right(exn) =>
                    exn-v = exn-unwrap(exn)
                    if E.is-arity-mismatch(exn-v) or E.is-non-function-app(exn-v): add-result(error-not-pred(loc, refinement, 2))
                    else: add-result(failure-exn(loc, exn, on-refinement))
                    end
                  | left(test-result) =>
                    ask:
                      | is-Unknown(test-result)        then: add-result(failure-is-incomparable(loc, test-result, lv, on-left, rv, on-right))
                      | (test-result == true)
                        or is-Equal(test-result)       then: add-result(failure-not-different(loc, some(refinement), lv, on-left, rv, on-right))
                      | not(is-boolean(test-result)
                          or is-NotEqual(test-result)) then: add-result(error-not-boolean(loc, refinement, lv, on-left, rv, on-right, test-result))
                      | otherwise:                           add-result(success(loc))
                    end
                end
            end
        end
      end
      nothing
    end,
    method check-satisfies-delayed(self, left, pred, loc) block:
      for left-right-check(loc)(lv from left, pv from pred):
        cases(either.Either) run-task(lam(): pv(lv) end):
          | right(exn) =>
            exn-v = exn-unwrap(exn)
            if E.is-arity-mismatch(exn-v) or E.is-non-function-app(exn-v): add-result(error-not-pred(loc, pv, 1))
            else: add-result(failure-exn(loc, exn, on-right))
            end
          | left(test-result) =>
            ask:
              | not(is-boolean(test-result)) then: add-result(error-not-boolean-pred(loc, pv, lv, on-left, test-result))
              | not(test-result)             then: add-result(failure-not-satisfied(loc, lv, on-left, pv))
              | otherwise:                         add-result(success(loc))
            end
        end
      end
      nothing
    end,
    method check-satisfies-delayed-cause(self, left, pred, cause, loc) block:
      for left-right-cause-check(loc)(lv from left, pv from pred, cv from cause):
        cases(either.Either) run-task(lam(): pv(cv) end):
          | right(exn) =>
            exn-v = exn-unwrap(exn)
            if E.is-arity-mismatch(exn-v) or E.is-non-function-app(exn-v): add-result(error-not-pred(loc, pv, 1))
            else: add-result(failure-exn(loc, exn, on-right))
            end
          | left(cause-result) =>
            ask:
              | not(is-boolean(cause-result)) then: add-result(error-not-boolean-pred(loc, pv, cv, on-cause, cause-result))
              | not(cause-result)             then: add-result(failure-not-satisfied(loc, cv, on-cause, pv))
              | otherwise:
                cases(either.Either) run-task(lam(): pv(lv) end):
                  | right(exn) =>
                    exn-v = exn-unwrap(exn)
                    if E.is-arity-mismatch(exn-v) or E.is-non-function-app(exn-v): add-result(error-not-pred(loc, pv, 1))
                    else: add-result(failure-exn(loc, exn, on-right))
                    end
                  | left(test-result) =>
                    ask:
                      | not(is-boolean(test-result)) then: add-result(error-not-boolean-pred(loc, pv, lv, on-left, test-result))
                      | not(test-result)             then: add-result(failure-not-satisfied(loc, lv, on-left, pv))
                      | otherwise:                         add-result(success(loc))
                    end
                end
            end
        end
      end
      nothing
    end,
    method check-satisfies-not-delayed(self, left, pred, loc) block:
      for left-right-check(loc)(lv from left, pv from pred):
        cases(either.Either) run-task(lam(): pv(lv) end):
          | right(exn) =>
            exn-v = exn-unwrap(exn)
            if E.is-arity-mismatch(exn-v) or E.is-non-function-app(exn-v): add-result(error-not-pred(loc, pv, 1))
            else: add-result(failure-exn(loc, exn, on-right))
            end
          | left(test-result) =>
            ask:
              | not(is-boolean(test-result)) then: add-result(error-not-boolean-pred(loc, pv, lv, on-left, test-result))
              | test-result                  then: add-result(failure-not-dissatisfied(loc, lv, on-left, pv))
              | otherwise:                         add-result(success(loc))
            end
        end
      end
      nothing
    end,
    method check-satisfies-not-delayed-cause(self, left, pred, cause, loc) block:
      for left-right-cause-check(loc)(lv from left, pv from pred, cv from cause):
        cases(either.Either) run-task(lam(): pv(cv) end):
          | right(exn) =>
            exn-v = exn-unwrap(exn)
            if E.is-arity-mismatch(exn-v) or E.is-non-function-app(exn-v): add-result(error-not-pred(loc, pv, 1))
            else: add-result(failure-exn(loc, exn, on-right))
            end
          | left(cause-result) =>
            ask:
              | not(is-boolean(cause-result)) then: add-result(error-not-boolean-pred(loc, pv, cv, on-cause, cause-result))
              | cause-result                  then: add-result(failure-not-dissatisfied(loc, cv, on-cause, pv))
              | otherwise:
                cases(either.Either) run-task(lam(): pv(lv) end):
                  | right(exn) =>
                    exn-v = exn-unwrap(exn)
                    if E.is-arity-mismatch(exn-v) or E.is-non-function-app(exn-v): add-result(error-not-pred(loc, pv, 1))
                    else: add-result(failure-exn(loc, exn, on-right))
                    end
                  | left(test-result) =>
                    ask:
                      | not(is-boolean(test-result)) then: add-result(error-not-boolean-pred(loc, pv, lv, on-left, test-result))
                      | test-result                  then: add-result(failure-not-dissatisfied(loc, lv, on-left, pv))
                      | otherwise:                         add-result(success(loc))
                    end
                end
            end
        end
      end
      nothing
    end,
    method check-satisfies(self, left, pred, loc) block:
      # XXX Where is this ever called?
      check-bool(loc,
        pred(left),
        lam(): failure-not-satisfied(loc, left, pred) end)
      nothing
    end,
    method check-satisfies-not(self, left, pred, loc) block:
      # XXX Where is this ever called?
      check-bool(loc,
        not(pred(left)),
        lam(): failure-not-dissatisfied(loc, left, pred) end)
      nothing
    end,
    method check-raises-str(self, thunk, str, loc) block:
      result = run-task(thunk)
      cases(either.Either) result:
        | left(_) => add-result(failure-no-exn(loc, some(str), on-left, true))
        | right(lv) =>
          if not(string-contains(torepr(exn-unwrap(lv)), str)):
            add-result(failure-wrong-exn(loc, str, lv, on-left))
          else:
            add-result(success(loc))
          end
      end
      nothing
    end,
    method check-raises-str-cause(self, thunk, str, cause, loc) block:
      cause-result = run-task(cause)
      cases(either.Either) cause-result:
        | left(_) => add-result(failure-no-exn(loc, some(str), on-cause, true))
        | right(cv) =>
          if not(string-contains(torepr(exn-unwrap(cv)), str)):
            add-result(failure-wrong-exn(loc, str, cv, on-cause))
          else:
            result = run-task(thunk)
            cases(either.Either) result:
              | left(_) => add-result(failure-no-exn(loc, some(str), on-left, true))
              | right(lv) =>
                if not(string-contains(torepr(exn-unwrap(lv)), str)):
                  add-result(failure-wrong-exn(loc, str, lv, on-left))
                else:
                  add-result(success(loc))
                end
            end
          end
      end
      nothing
    end,
    method check-raises-other-str(self, thunk, str, loc) block:
      result = run-task(thunk)
      cases(either.Either) result:
        | left(_) => add-result(failure-no-exn(loc, some(str), on-left, false))
        | right(lv) =>
          if string-contains(torepr(exn-unwrap(lv)), str):
            add-result(failure-right-exn(loc, str, lv, on-left))
          else:
            add-result(success(loc))
          end
      end
      nothing
    end,
    method check-raises-other-str-cause(self, thunk, str, cause, loc) block:
      cause-result = run-task(cause)
      cases(either.Either) cause-result:
        | left(_) => add-result(failure-no-exn(loc, some(str), on-cause, false))
        | right(cv) =>
          if string-contains(torepr(exn-unwrap(cv)), str):
            add-result(failure-right-exn(loc, str, cv, on-cause))
          else:
            result = run-task(thunk)
            cases(either.Either) result:
              | left(_) => add-result(failure-no-exn(loc, some(str), on-left, false))
              | right(lv) =>
                if string-contains(torepr(exn-unwrap(lv)), str):
                  add-result(failure-right-exn(loc, str, lv, on-left))
                else:
                  add-result(success(loc))
                end
            end
          end
      end
      nothing
    end,
    method check-raises-not(self, thunk, loc) block:
      cases(either.Either) run-task(thunk):
        | right(exn) => add-result(failure-exn(loc, exn, on-left))
        | left(v)    => add-result(success(loc))
      end
      nothing
    end,
    method check-raises-not-cause(self, thunk, cause, loc) block:
      cases(either.Either) run-task(cause):
        | right(exn) => add-result(failure-exn(loc, exn, on-cause))
        | left(_)    => 
          cases(either.Either) run-task(thunk):
            | right(exn) => add-result(failure-exn(loc, exn, on-left))
            | left(_)    => add-result(success(loc))
          end
      end
      nothing
    end,
    method check-raises-satisfies(self, thunk, pred, loc) block:
      add-result(
        cases(either.Either) run-task(thunk):
          | left(v)    => failure-no-exn(loc, none, on-left, true)
          | right(exn) =>
            exn-val =
              if E.is-user-exception(exn-unwrap(exn)):
                exn-unwrap(exn).value
              else:
                exn-unwrap(exn)
              end
            cases(either.Either) run-task(lam(): pred(exn-val) end):
              | right(shadow exn) =>
                exn-v = exn-unwrap(exn)
                if E.is-arity-mismatch(exn-v) or E.is-non-function-app(exn-v): error-not-pred(loc, pred, 1)
                else: failure-exn(loc, exn, on-right)
                end
              | left(pred-result) =>
                ask:
                  | not(is-boolean(pred-result)) then: error-not-boolean-pred(loc, pred, exn-val, on-left, pred-result)
                  | not(pred-result) then: failure-raise-not-satisfied(loc, exn, on-left, pred)
                  | otherwise: success(loc)
                end
            end
        end)
      nothing
    end,
    method check-raises-satisfies-cause(self, thunk, pred, cause, loc) block:
      add-result(
        cases(either.Either) run-task(cause):
          | left(v)    => failure-no-exn(loc, none, on-cause, true)
          | right(exn) =>
            exn-cause-val =
              if E.is-user-exception(exn-unwrap(exn)):
                exn-unwrap(exn).value
              else:
                exn-unwrap(exn)
              end
            cases(either.Either) run-task(lam(): pred(exn-cause-val) end):
              | right(shadow exn) =>
                exn-v = exn-unwrap(exn)
                if E.is-arity-mismatch(exn-v) or E.is-non-function-app(exn-v): error-not-pred(loc, pred, 1)
                else: failure-exn(loc, exn, on-right)
                end
              | left(pred-cause-result) =>
                ask:
                  | not(is-boolean(pred-cause-result)) then: error-not-boolean-pred(loc, pred, exn-cause-val, on-cause, pred-cause-result)
                  | not(pred-cause-result) then: failure-raise-not-satisfied(loc, exn, on-cause, pred)
                  | otherwise:
                    cases(either.Either) run-task(thunk):
                      | left(v)    => failure-no-exn(loc, none, on-left, true)
                      | right(shadow exn) =>
                        exn-thunk-val = 
                          if E.is-user-exception(exn-unwrap(exn)):
                            exn-unwrap(exn).value
                          else:
                            exn-unwrap(exn)
                          end
                        cases(either.Either) run-task(lam(): pred(exn-thunk-val) end):
                          | right(shadow exn) =>
                            exn-v = exn-unwrap(exn)
                            if E.is-arity-mismatch(exn-v) or E.is-non-function-app(exn-v): error-not-pred(loc, pred, 1)
                            else: failure-exn(loc, exn, on-right)
                            end
                          | left(pred-thunk-result) =>
                            ask:
                              | not(is-boolean(pred-thunk-result)) then: error-not-boolean-pred(loc, pred, exn-thunk-val, on-left, pred-thunk-result)
                              | not(pred-thunk-result) then: failure-raise-not-satisfied(loc, exn, on-left, pred)
                              | otherwise: success(loc)
                            end
                        end
                    end
                end
            end
        end)
      nothing
    end,
    method check-raises-violates(self, thunk, pred, loc) block:
      add-result(
        cases(either.Either) run-task(thunk):
          | left(v)    => failure-no-exn(loc, none, on-left, true)
          | right(exn) =>
            exn-val =
              if E.is-user-exception(exn-unwrap(exn)):
                exn-unwrap(exn).value
              else:
                exn-unwrap(exn)
              end
            cases(either.Either) run-task(lam(): pred(exn-val) end):
              | right(shadow exn) =>
                exn-v = exn-unwrap(exn)
                if E.is-arity-mismatch(exn-v) or E.is-non-function-app(exn-v): error-not-pred(loc, pred, 1)
                else: failure-exn(loc, exn, on-right)
                end
              | left(pred-result) =>
                ask:
                  | not(is-boolean(pred-result)) then: error-not-boolean-pred(loc, pred, exn-val, on-left, pred-result)
                  | pred-result then: failure-raise-not-dissatisfied(loc, exn, on-left, pred)
                  | otherwise: success(loc)
                end
            end
        end)
      nothing
    end,
    method check-raises-violates-cause(self, thunk, pred, cause, loc) block:
      add-result(
        cases(either.Either) run-task(cause):
          | left(v)    => failure-no-exn(loc, none, on-cause, true)
          | right(exn) =>
            exn-cause-val =
              if E.is-user-exception(exn-unwrap(exn)):
                exn-unwrap(exn).value
              else:
                exn-unwrap(exn)
              end
            cases(either.Either) run-task(lam(): pred(exn-cause-val) end):
              | right(shadow exn) =>
                exn-v = exn-unwrap(exn)
                if E.is-arity-mismatch(exn-v) or E.is-non-function-app(exn-v): error-not-pred(loc, pred, 1)
                else: failure-exn(loc, exn, on-right)
                end
              | left(pred-cause-result) =>
                ask:
                  | not(is-boolean(pred-cause-result)) then: error-not-boolean-pred(loc, pred, exn-cause-val, on-cause, pred-cause-result)
                  | pred-cause-result then: failure-raise-not-dissatisfied(loc, exn, on-cause, pred)
                  | otherwise:
                    cases(either.Either) run-task(thunk):
                      | left(v)    => failure-no-exn(loc, none, on-left, true)
                      | right(shadow exn) =>
                        exn-thunk-val =
                          if E.is-user-exception(exn-unwrap(exn)):
                            exn-unwrap(exn).value
                          else:
                            exn-unwrap(exn)
                          end
                        cases(either.Either) run-task(lam(): pred(exn-thunk-val) end):
                          | right(shadow exn) =>
                            exn-v = exn-unwrap(exn)
                            if E.is-arity-mismatch(exn-v) or E.is-non-function-app(exn-v): error-not-pred(loc, pred, 1)
                            else: failure-exn(loc, exn, on-right)
                            end
                          | left(pred-thunk-result) =>
                            ask:
                              | not(is-boolean(pred-thunk-result)) then: error-not-boolean-pred(loc, pred, exn-thunk-val, on-left, pred-thunk-result)
                              | pred-thunk-result then: failure-raise-not-dissatisfied(loc, exn, on-left, pred)
                              | otherwise: success(loc)
                            end
                        end
                    end
                end
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
          stack = tr.access-stack(get-stack)
          m = s.message + "\n  " + tr.loc.format(false) + ": failed because: \n    "
            + RED.display-to-string(tr.render-reason(), torepr, stack)
          s.{
            message: m,
            failed: s.failed + 1,
            total: s.total + 1
          }
      end
    end
    block-type = if br.keyword-check: "Check" else: "Examples" end
    ended-in-error = cases(Option) br.maybe-err:
      | none => ""
      | some(err) =>
        stack = get-stack(err)
        "\n  " + block-type + " block ended in the following error (not all tests may have run): \n\n  "
          + RED.display-to-string(exn-unwrap(err).render-reason(), torepr, stack)
          + RED.display-to-string(ED.v-sequence(map(ED.loc, stack)), torepr, empty)
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
