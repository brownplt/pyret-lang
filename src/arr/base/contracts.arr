#lang pyret 0.5

provide *
provide-types *
import lists as L
import error-display as ED

fun draw-and-highlight(l):
  ED.loc-display(l, "error-highlight", ED.loc(l))
end

data ContractResult:
  | ok with:
    render-reason(self): ED.text("There were no errors") end
  | fail(loc, reason :: FailureReason) with:
    render-fancy-reason(self, loc-to-ast, loc-to-src):
      self.reason.render-fancy-reason(self.loc, false, loc-to-ast, loc-to-src)
    end,
    render-reason(self):
      self.reason.render-reason(self.loc, false)
    end
  | fail-arg(loc, reason :: FailureReason) with:
    render-fancy-reason(self, loc-to-ast, loc-to-src):
      self.reason.render-fancy-reason(self.loc, true, loc-to-ast, loc-to-src)
    end,
    render-reason(self):
      self.reason.render-reason(self.loc, true)
    end
end

# these don't seem to be used in native pyret
data FieldFailure:
  | field-failure(loc, field, reason) with:
    render-reason(self, loc, from-fail-arg):
      [ED.error:
        [ED.para-nospace: ED.text("At "), draw-and-highlight(self.loc),
          ED.text(", field "), ED.code(ED.text(self.field)), ED.text(" failed because")],
        self.reason.render-reason(loc, from-fail-arg)]
    end
  | missing-field(loc, field) with:
    render-reason(self, loc, from-fail-arg):
      [ED.error:
        [ED.para: ED.text("Missing field "), ED.code(ED.text(self.field)),
          ED.text("is required at "), draw-and-highlight(self.loc)]]
    end
end

data FailureReason:
  | ref-init(loc, reason :: FailureReason) with:
    render-fancy-reason(self, loc, from-fail-arg, loc-to-ast, loc-to-src):
      print("ref-init")
      self.render-reason(loc, from-fail-arg)
    end,
    render-reason(self, loc, from-fail-arg):
      ED.maybe-stack-loc(0, true,
        lam(user-loc):
          [ED.error:
            [ED.para: ED.text("Failed while initializing a graph at"), draw-and-highlight(user-loc),
              ED.text("because:")],
            self.reason.render-reason(loc, false)]
        end,
        [ED.error:
          [ED.para: ED.text("Failed while initializing a graph, because:")],
          self.reason.render-reason(loc, false)])
    end
  | type-mismatch(val, name :: String) with:
    render-fancy-reason(self, loc, from-fail-arg, loc-to-ast, loc-to-src):
      ED.maybe-stack-loc(1, true, 
        lam(l):
          [ED.error:
            [ED.para:
              ED.text("The runtime contract checker halted execution because the annotation")],
             ED.code(ED.highlight(ED.text(loc-to-src(loc)), [ED.locs: loc], 0)),
            [ED.para:
              ED.text("was not satisfied by the value")],
             ED.embed(self.val),
            [ED.para:
              ED.text("which was sent from around")],
             ED.code(ED.highlight(ED.text(loc-to-src(l)), [ED.locs: l], 1))]
        end,
        [ED.error:
          [ED.para:
            ED.text("The runtime contract checker halted execution because the annotation")],
           ED.code(ED.highlight(ED.text(loc-to-src(loc)), [ED.locs: loc], 0)),
          [ED.para:
            ED.text("was not satisfied by the value")],
           ED.embed(self.val)])
    end,
    render-reason(self, loc, from-fail-arg):
      message = [ED.para:
        ED.text("Expected to get"), ED.code(ED.text(self.name)),
        ED.text("because of the annotation at"), draw-and-highlight(loc),
        ED.text("but got:")]
      if from-fail-arg:
        ED.maybe-stack-loc(0, true, 
          lam(l):
            [ED.error: message, ED.embed(self.val),
              [ED.para: ED.text("called from around"), draw-and-highlight(l)]]
          end,
          [ED.error: message, ED.embed(self.val)])
      else:
        [ED.error: message, ED.embed(self.val)]
      end
    end
  | predicate-failure(val, pred-name) with:
    render-fancy-reason(self, loc, from-fail-arg, loc-to-ast, loc-to-src):
      ED.maybe-stack-loc(1, true, 
        lam(l):
          [ED.error:
            [ED.para:
              ED.text("The runtime contract checker halted execution because the predicate "),
              ED.code(ED.text(self.pred-name)),
              ED.text(" in the annotation ")],
             ED.code(ED.highlight(ED.text(loc-to-src(loc)), [ED.locs: loc], 0)),
            [ED.para:
              ED.text("was not satisfied by the value")],
             ED.embed(self.val),
            [ED.para:
              ED.text("which was sent from around")],
             ED.code(ED.highlight(ED.text(loc-to-src(l)), [ED.locs: l], 1))]
        end,
        [ED.error:
          [ED.para:
            ED.text("The runtime contract checker halted execution because the predicate "),
            ED.code(ED.text(self.pred-name)),
            ED.text(" in the annotation ")],
           ED.code(ED.highlight(ED.text(loc-to-src(loc)), [ED.locs: loc], 0)),
          [ED.para:
            ED.text("was not satisfied by the value")],
           ED.embed(self.val)])
    end,
    render-reason(self, loc, from-fail-arg):
      message = [ED.para:
        ED.text("The predicate"), ED.code(ED.text(self.pred-name)),
        ED.text("in the annotation at"), draw-and-highlight(loc), ED.text("returned false for this value:")]
      if from-fail-arg:
        ED.maybe-stack-loc(0, true, 
          lam(l):
            [ED.error: message, ED.embed(self.val),
              [ED.para: ED.text("called from around"), draw-and-highlight(l)]]
          end,
          [ED.error: message, ED.embed(self.val)])
      else:
        [ED.error: message, ED.embed(self.val)]
      end
    end
  | record-fields-fail(val, field-failures :: L.List<FieldFailure>) with:
    render-fancy-reason(self, loc, from-fail-arg, loc-to-ast, loc-to-src):
      var n = 0
      reasons =
        self.field-failures.map(lam(failure):
          cases(FieldFailure) failure:
            | missing-field(fl, ff) =>
              n := n + 1
              [ED.error:
                [ED.para:
                  ED.text("The value was excepted to have a field named "),
                  ED.code(ED.text(ff)),
                  ED.text(" because of the annotation ")],
                ED.code(ED.highlight(ED.text(loc-to-src(fl)), [ED.locs: fl], n))]
            | field-failure(_, _, _) => failure.render-reason(loc, from-fail-arg)
          end;) ^ ED.bulleted-sequence
      ED.maybe-stack-loc(1, true, 
        lam(l):
          [ED.error:
            [ED.para:
              ED.text("The runtime contract checker halted execution because the "),
              ED.text("record annotation"),
              ED.text(" was not satisfied by the value")],
             ED.embed(self.val),
            [ED.para:
              ED.text("which was sent from around")],
             ED.code(ED.highlight(ED.text(loc-to-src(l)), [ED.locs: l], 0)),
            [ED.para:
              ED.text("because:")],
            reasons]
        end,
        [ED.error:
          [ED.para:
            ED.text("The runtime contract checker halted execution because the "),
            ED.text("record annotation"),
            ED.text(" was not satisfied by the value")],
           ED.embed(self.val),
          [ED.para:
            ED.text("because:")],
          reasons])
    end,
    render-reason(self, loc, from-fail-arg):
      [ED.error:
        [ED.para:
          ED.text("The record annotation at "),
          ED.loc-display(loc, "error-highlight", ED.text("this annotation")),
          ED.text("failed on this value:")],
        ED.embed(self.val),
        [ED.para: ED.text("Because:")],
        ED.bulleted-sequence(self.field-failures.map(_.render-reason(loc, false)))
      ]
    end
  | dot-ann-not-present(name, field) with:
    render-fancy-reason(self, loc, from-fail-arg, loc-to-ast, loc-to-src):
      [ED.error:
        [ED.para:
          ED.text("The runtime contract checker halted execution because the "),
          ED.highlight(ED.text("dot-annotation"), [ED.locs: loc], 0)],
         # can't highlight individual components since non-top-level
         # expressions can't presently be parsed
         ED.code(
          [ED.sequence:
            ED.text(self.name),
            ED.text("."),
            ED.text(self.field)]),
        [ED.para:
          ED.text("expects that the type named "),
          ED.code(ED.text(self.field)),
          ED.text(" exists in the object named "),
          ED.code(ED.text(self.name)),
          ED.text(", but "),
          ED.code(ED.text(self.field)),
          ED.text(" could not be found.")]]
    end,
    render-reason(self, loc, from-fail-arg):
      [ED.error:
        [ED.para: ED.text("Couldn't find"),
          ED.loc-display(loc, "error-highlight", ED.text("the annotation named " + self.field)),
          ED.text("in the annotations from"), ED.code(ED.text(self.name))]]
    end
end

