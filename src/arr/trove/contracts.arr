#lang pyret 0.5

provide *
provide-types *
import global as _
include lists
include option
import error-display as ED

fun draw-and-highlight(l):
  ED.loc-display(l, "error-highlight", ED.loc(l))
end

data ContractResult:
  | ok with:
    method render-reason(self): ED.text("There were no errors") end
  | fail(loc, reason :: FailureReason) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      self.reason.render-fancy-reason(self.loc, true, maybe-stack-loc, src-available, maybe-ast)
    end,
    method render-reason(self):
      self.reason.render-reason(self.loc, false)
    end
  | fail-arg(loc, reason :: FailureReason) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      self.reason.render-fancy-reason(self.loc, true, maybe-stack-loc, src-available, maybe-ast)
    end,
    method render-reason(self):
      self.reason.render-reason(self.loc, true)
    end
end

# these don't seem to be used in native pyret
data FieldFailure:
  | field-failure(loc, field, reason) with:
    method render-reason(self, loc, from-fail-arg):
      [ED.error:
        [ED.para-nospace: ED.text("At "), draw-and-highlight(self.loc),
          ED.text(", field "), ED.code(ED.text(self.field)), ED.text(" failed because")],
        self.reason.render-reason(loc, from-fail-arg)]
    end
  | ann-failure(loc, ann, reason) with:
    method render-reason(self, loc, from-fail-arg):
      [ED.error:
        [ED.para-nospace: ED.text("The annotation at "), draw-and-highlight(loc),
         ED.text(" failed because")],
        self.reason.render-reason(self.loc, from-fail-arg)]
    end
  | missing-field(loc, field) with:
    method render-reason(self, loc, from-fail-arg):
      [ED.error:
        [ED.para: ED.text("Missing field "), ED.code(ED.text(self.field)),
          ED.text("is required at "), draw-and-highlight(self.loc)]]
    end
end

data FailureReason:
  | bad-bracket-target(loc, obj) with:
    method render-fancy-reason(self, loc, from-fail-arg, maybe-stack-loc, src-available, maybe-ast):
      obj-loc = if src-available(loc):
        cases(Option) maybe-ast(loc):
          | some(ast) =>
            if ast.label() == "s-bracket": ast.obj.l
            else: loc
            end
          | none => loc
        end
      else: loc
      end
      [ED.error:
        [ED.para:
          ED.text("A "), ED.highlight(ED.text("bracket expression"), [ED.locs: self.loc], 0),
          ED.text(" only works with "),
          ED.code(ED.text("Row")), ED.text("s, "),
          ED.code(ED.text("StringDict")), ED.text("s, or values that define a "),
          ED.code(ED.text("get-value")), ED.text(" method.")],
        ED.cmcode(self.loc),
        [ED.para: ED.text("The bracket expression's "),
          ED.highlight(ED.text("target"), [ED.locs: obj-loc], 1), ED.text(" was:")],
        [ED.para: ED.embed(self.obj)]]
    end,
    method render-reason(self, loc, from-fail-arg):
      [ED.error:
        [ED.para:
          ED.text("The bracket expression at "), draw-and-highlight(self.loc),
          ED.text(" only works with "),
          ED.code(ED.text("Row")), ED.text("s, "),
          ED.code(ED.text("StringDict")), ED.text("s, or values that define a "),
          ED.code(ED.text("get-value")), ED.text(" method.  The value was:")],
        [ED.para: ED.embed(self.obj)]]
    end
  | failure-at-arg(loc, index, function-name, args, reason) with:
    method render-fancy-reason(self, loc, from-fail-arg, maybe-stack-loc, src-available, maybe-ast):
      cases(Option) maybe-stack-loc(0, true):
        | some(app-loc) =>
          if src-available(app-loc):
            cases(Option) maybe-ast(app-loc):
              | some(ast) =>
                ast-label = ast.label()
                {nth-arg; call-type} = ask:
                  | ast-label == "s-bracket" then:
                    {if self.index == 0: ast.obj else: ast.key end; "bracket expression"}
                  | otherwise:
                    {ast.args.get(self.index); "function application"}
                end
                [ED.error:
                  [ED.para:
                    ED.text("The "),
                    ED.highlight(ED.text(call-type), [ED.locs: app-loc], -1)],
                   ED.cmcode(app-loc),
                  [ED.para:
                    ED.text("failed because the "),
                    ED.highlight(
                      [ED.sequence: ED.ed-nth(self.index + 1), ED.text(" argument")],
                      [ED.locs: nth-arg.l], 0),
                    ED.text(" evaluated to an unexpected value.")],
                  self.reason.render-fancy-reason(loc, false, maybe-stack-loc, src-available, maybe-ast)]
              | none      =>
                [ED.error:
                  [ED.para:
                    ED.text("The "),
                    ED.highlight(ED.text("function application"), [ED.locs: app-loc], 0)],
                   ED.cmcode(app-loc),
                  [ED.para:
                    ED.text("failed because the "),
                    ED.ed-nth(self.index + 1),
                    ED.text(" argument evaluated to an unexpected value.")],
                  self.reason.render-fancy-reason(loc, false, maybe-stack-loc, src-available, maybe-ast)]
            end
          else:
            [ED.error:
              [ED.para:
                ED.text("The function application at "),
                ED.loc(app-loc),
                ED.text(" failed because the "),
                ED.ed-nth(self.index + 1),
                ED.text(" argument evaluated to an unexpected value.")],
              self.reason.render-fancy-reason(loc, from-fail-arg, maybe-stack-loc, src-available, maybe-ast)]
          end
        | none          =>
          [ED.error:
            [ED.para:
              ED.text("An application of "),
              ED.code(ED.text(self.function-name)),
              ED.text(" failed because the "),
              ED.ed-nth(self.index + 1),
              ED.text(" argument evaluated to an unexpected value.")],
            self.reason.render-fancy-reason(loc, from-fail-arg, maybe-stack-loc, src-available, maybe-ast)]
      end
    end,
    method render-reason(self, loc, from-fail-arg):
      [ED.error:
        [ED.para:
          ED.code(ED.text(self.function-name)),
          ED.text(" : The argument at position " + tostring(self.index + 1)),
          ED.text(" was invalid because: ")],
        self.reason.render-reason(loc, from-fail-arg),
        [ED.para: ED.text("The other arguments were:")],
        [ED.para: ED.h-sequence-sep(map(ED.embed, self.args), " ", " and ")]]
    end
  | ref-init(loc, reason :: FailureReason) with:
    method render-fancy-reason(self, loc, from-fail-arg, maybe-stack-loc, src-available, maybe-ast) block:
      [ED.error:
        [ED.para:
          ED.text("Failed while updating a reference:"),
          ED.cmcode(loc),
          ED.text("because:"),
          self.reason.render-reason(loc, false)]]
    end,
    method render-reason(self, loc, from-fail-arg):
      ED.maybe-stack-loc(0, true,
        lam(user-loc):
          [ED.error:
            [ED.para: ED.text("Failed while updating a reference at"), draw-and-highlight(user-loc),
              ED.text("because:")],
            self.reason.render-reason(loc, false)]
        end,
        [ED.error:
          [ED.para: ED.text("Failed while updating a reference because:")],
          self.reason.render-reason(loc, false)])
    end
  | type-mismatch(val, name :: String) with:
    method render-fancy-reason(self, loc, from-fail-arg, maybe-stack-loc, src-available, maybe-ast):
      [ED.error:
        if loc.is-builtin():
          [ED.para:
            ED.text("An annotation, "),
            ED.code(ED.text(self.name)),
            ED.text(", in "),
            ED.loc(loc)]
        else if src-available(loc):
          [ED.sequence:
            [ED.para:
              ED.text("The "),
              ED.highlight(ED.text(self.name + " annotation"), [ED.locs: loc], 0)],
            ED.cmcode(loc)]
        else:
          [ED.para:
            ED.text("The annotation, "),
            ED.code(ED.text(self.name)),
            ED.text(", at "),
            ED.loc(loc)]
        end,
        [ED.para:
          ED.text("was not satisfied by the value")],
        ED.embed(self.val),
        if from-fail-arg:
          cases(Option) maybe-stack-loc(1, true):
            | some(sender) =>
              if src-available(sender):
                [ED.sequence:
                  [ED.para:
                    ED.text("which was sent from around")],
                   ED.cmcode(sender)]
              else:
                [ED.para:
                  ED.text("which was sent from around "),
                  ED.loc(sender)]
              end
            | none =>
              [ED.sequence:]
          end
        else: [ED.sequence:] end]
    end,
    method render-reason(self, loc, from-fail-arg):
      message = [ED.para:
        ED.text("Expected to get "), ED.code(ED.text(self.name)),
        ED.text(" because of the annotation at "), draw-and-highlight(loc),
        ED.text(" but got:")]
      if from-fail-arg:
        ED.maybe-stack-loc(0, true,
          lam(l):
            [ED.error: message, ED.embed(self.val),
              [ED.para: ED.text("called from around "), draw-and-highlight(l)]]
          end,
          [ED.error: message, ED.embed(self.val)])
      else:
        [ED.error: message, ED.embed(self.val)]
      end
    end
  | predicate-failure(val, name) with:
    method render-fancy-reason(self, loc, from-fail-arg, maybe-stack-loc, src-available, maybe-ast):
      [ED.error:
        if loc.is-builtin():
          [ED.para:
            ED.text("A predicate, "),
            ED.code(ED.text(self.name)),
            ED.text(", in "),
            ED.loc(loc)]
        else if src-available(loc):
          [ED.sequence:
            [ED.para:
              ED.text("The predicate "),
              ED.code(ED.text(self.name)),
              ED.text(" in the "),
              ED.highlight(ED.text("annotation"), [ED.locs: loc], 0)],
            ED.cmcode(loc)]
        else:
          [ED.para:
              ED.text("The predicate, "),
              ED.code(ED.text(self.name)),
              ED.text(", at "),
              ED.loc(loc)]
        end,
        [ED.para:
          ED.text("was not satisfied by the value")],
        ED.embed(self.val),
        if from-fail-arg:
          cases(Option) maybe-stack-loc(1, true):
            | some(sender) =>
              if src-available(sender):
                [ED.sequence:
                  [ED.para:
                    ED.text("which was sent from around")],
                   ED.cmcode(sender)]
              else:
                [ED.para:
                  ED.text("which was sent from around "),
                  ED.loc(sender)]
              end
            | none =>
              [ED.sequence:]
          end
        else: [ED.sequence:] end]
    end,
    method render-reason(self, loc, from-fail-arg):
      message = [ED.para:
        ED.text("The predicate"), ED.code(ED.text(self.name)),
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
  | record-fields-fail(val, field-failures :: List<FieldFailure>) with:
    method render-fancy-reason(self, loc, from-fail-arg, maybe-stack-loc, src-available, maybe-ast):
      [ED.error:
        if loc.is-builtin():
          [ED.para:
            ED.text("A record annotation in "),
            ED.loc(loc)]
        else if src-available(loc):
          [ED.sequence:
            [ED.para:
              ED.text("The "),
              ED.highlight(ED.text("record annotation"), [ED.locs: loc], -1)],
            ED.cmcode(loc)]
        else:
          [ED.para:
            ED.text("The record annotation at "),
            ED.loc(loc)]
        end,
        [ED.para:
          ED.text("was not satisfied by the value")],
        ED.embed(self.val),
        if from-fail-arg:
          cases(Option) maybe-stack-loc(1, true):
            | some(sender) =>
              if src-available(sender):
                [ED.sequence:
                  [ED.para:
                    ED.text("which was sent from around")],
                   ED.cmcode(sender)]
              else:
                [ED.para:
                  ED.text("which was sent from around "),
                  ED.loc(sender)]
              end
            | none =>
              [ED.sequence:]
          end
        else: [ED.sequence:] end,
        [ED.para:
          ED.text("because, "),
          map_n(lam(n, failure):
            cases(FieldFailure) failure block:
              | missing-field(fl, ff) =>
                if src-available(fl):
                  [ED.sequence:
                    ED.text("The value was "),
                    ED.highlight(ED.text("expected"), [ED.locs: fl], n),
                    ED.text(" to have a field named "),
                    ED.code(ED.text(ff))]
                else:
                  [ED.sequence:
                    ED.text("The value was expected to have a field named "),
                    ED.code(ED.text(ff)),
                    ED.text(" because of the annotation at "),
                    ED.loc(fl)]
                end
              | field-failure(_, _, _) => failure.render-reason(loc, from-fail-arg)
            end
          end, 1, self.field-failures) ^ ED.bulleted-sequence]]
    end,
    method render-reason(self, loc, from-fail-arg):
      [ED.error:
        [ED.para:
          ED.text("The record annotation at "),
          ED.loc-display(loc, "error-highlight", ED.code(ED.text("this annotation"))),
          ED.text("failed on this value:")],
        ED.embed(self.val),
        [ED.para: ED.text("Because:")],
        ED.bulleted-sequence(self.field-failures.map(_.render-reason(loc, false)))
      ]
    end
  | tuple-anns-fail(val, anns-failures :: List<FieldFailure>) with:
    method render-fancy-reason(self, loc, from-fail-arg, maybe-stack-loc, src-available, maybe-ast):
      [ED.error:
        if loc.is-builtin():
          [ED.para:
            ED.text("A tuple annotation, "),
            #ED.code(ED.text(self.name)),
            ED.text(", in "),
            ED.loc(loc)]
        else if src-available(loc):
          [ED.sequence:
            [ED.para:
              ED.text("The tuple annotation "),
              #ED.code(ED.text(self.name)),
              ED.text(" in the "),
              ED.highlight(ED.text("annotation"), [ED.locs: loc], 0)],
            ED.cmcode(loc)]
        else:
          [ED.para:
              ED.text("The tuple annotation, "),
              #ED.code(ED.text(self.name)),
              ED.text(", at "),
              ED.loc(loc)]
        end,
        [ED.para:
          ED.text("was not satisfied by the value")],
        ED.embed(self.val),
        if from-fail-arg:
          cases(Option) maybe-stack-loc(1, true):
            | some(sender) =>
              if src-available(sender):
                [ED.sequence:
                  [ED.para:
                    ED.text("which was sent from around")],
                   ED.cmcode(sender)]
              else:
                [ED.para:
                  ED.text("which was sent from around "),
                  ED.loc(sender)]
              end
            | none =>
              [ED.sequence:]
          end
        else: [ED.sequence:] end,
        [ED.para:
          ED.text("because, "),
          map_n(lam(n, failure):
            cases(FieldFailure) failure block:
              | missing-field(fl, ff) =>
                if src-available(fl):
                  [ED.sequence:
                    ED.text("The value was "),
                    ED.highlight(ED.text("expected"), [ED.locs: fl], n),
                    ED.text(" to have a field named "),
                    ED.code(ED.text(ff))]
                else:
                  [ED.sequence:
                    ED.text("The value was expected to have a field named "),
                    ED.code(ED.text(ff)),
                    ED.text(" because of the annotation at "),
                    ED.loc(fl)]
                end
              | field-failure(_, _, _) => failure.render-reason(loc, from-fail-arg)
              | ann-failure(_, _, _) => failure.render-reason(loc, from-fail-arg)
            end
          end, 0, self.anns-failures) ^ ED.bulleted-sequence]]
    end,
    method render-reason(self, loc, from-fail-arg):
      [ED.error:
        [ED.para:
          ED.text("The tuple annotation "),
          ED.loc-display(loc, "error-highlight", ED.text("this annotation")),
          ED.text("failed on this value:")],
        ED.embed(self.val),
        [ED.para: ED.text("Because:")],
        ED.bulleted-sequence(self.anns-failures.map(_.render-reason(loc, false)))
      ]
    end
  | tup-length-mismatch(loc, val, annLength, tupleLength) with:
    method render-fancy-reason(self, loc, from-fail-arg, maybe-stack-loc, src-available, maybe-ast):
      [ED.error:
          if loc.is-builtin():
            [ED.para:
              ED.text("A tuple annotation, "),
              ED.code(ED.text(self.name)),
              ED.text(", in "),
              ED.loc(loc)]
          else if src-available(loc):
            [ED.sequence:
              [ED.para:
                ED.text("The "),
                ED.highlight(ED.text("tuple annotation"), [ED.locs: loc], 0)],
              ED.cmcode(loc)]
          else:
            [ED.para:
                ED.text("The tuple annotation, "),
                ED.code(ED.text(self.name)),
                ED.text(", at "),
                ED.loc(loc)]
          end,
          [ED.para:
            ED.text("which expects a tuple containing exactly "),
            ED.ed-components(self.annLength),
            ED.text(" was not satisfied by the "),
            ED.embed(self.tupleLength),
            ED.text(" component tuple:")],
          ED.embed(self.val),
          if from-fail-arg:
            cases(Option) maybe-stack-loc(1, true):
              | some(sender) =>
                if src-available(sender):
                  [ED.sequence:
                    [ED.para:
                      ED.text("which was sent from around")],
                     ED.cmcode(sender)]
                else:
                  [ED.para:
                    ED.text("which was sent from around "),
                    ED.loc(sender)]
                end
              | none =>
                [ED.sequence:]
            end
          else: [ED.sequence:] end]
    end,
    method render-reason(self, loc, fail-from-arg):
      [ED.error:
        [ED.para:
          ED.text("The tuple annotation at "),
          ED.embed(loc),
          ED.text(" expected the given tuple to be of length "),
          ED.embed(self.annLength)],
        [ED.para:
          ED.text("The given tuple had the incorrect length of "),
          ED.embed(self.tupleLength)]
      ]
    end
  | dot-ann-not-present(name, field) with:
    method render-fancy-reason(self, loc, from-fail-arg, maybe-stack-loc, src-available, maybe-ast):
      if loc.is-builtin():
        [ED.error:
          [ED.para:
            ED.text("The dot-annotation "),
            ED.code(
              [ED.sequence:
                ED.text(self.name),
                ED.text("."),
                ED.text(self.field)]),
            ED.text(" in "),
            ED.loc(loc)],
          [ED.para:
            ED.text("expects that the type named "),
            ED.code(ED.text(self.field)),
            ED.text(" exists in the object named "),
            ED.code(ED.text(self.name)),
            ED.text(", but "),
            ED.code(ED.text(self.field)),
            ED.text(" could not be found.")]]
      else if src-available(loc):
        [ED.error:
          [ED.para:
            ED.text("The "),
            ED.highlight(ED.text("dot-annotation"), [ED.locs: loc], 0)],
          ED.cmcode(loc),
          [ED.para:
            ED.text("expects that the type named "),
            ED.code(ED.text(self.field)),
            ED.text(" exists in the object named "),
            ED.code(ED.text(self.name)),
            ED.text(", but "),
            ED.code(ED.text(self.field)),
            ED.text(" could not be found.")]]
      else:
        [ED.error:
          [ED.para:
            ED.text("The dot-annotation "),
            ED.code(
              [ED.sequence:
                ED.text(self.name),
                ED.text("."),
                ED.text(self.field)]),
            ED.text(" at "),
            ED.loc(loc)],
          [ED.para:
            ED.text("expects that the type named "),
            ED.code(ED.text(self.field)),
            ED.text(" exists in the object named "),
            ED.code(ED.text(self.name)),
            ED.text(", but "),
            ED.code(ED.text(self.field)),
            ED.text(" could not be found.")]]
      end
    end,
    method render-reason(self, loc, from-fail-arg):
      [ED.error:
        [ED.para: ED.text("Couldn't find "),
          ED.loc-display(loc, "error-highlight", ED.text("the annotation named " + self.field)),
          ED.text(" in the annotations from"), ED.code(ED.text(self.name))]]
    end
end
