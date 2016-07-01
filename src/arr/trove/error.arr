provide *
provide-types *
# import arrays as A
# import lists as L

import global as _
import option as O
import error-display as ED

fun draw-and-highlight(l):
  ED.loc-display(l, "error-highlight", ED.loc(l))
end
fun vert-list-values(vals):
  ED.v-sequence(vals.map(lam(val): [ED.para: ED.embed(val)] end))
end

fun ed-intro(name, loc, color):
  [ED.para:
    ED.text("The "),
    ED.highlight(ED.text(name), [ED.locs: loc], color)]
end

fun please-report-bug():
  [ED.para: ED.text("Please report this as a bug.")]
end

data RuntimeError:
  | sample-error(loc, some-info) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      if self.loc.is-builtin():
        # Errors here should end with `please-report-bug()`
        ...
      else if src-available(self.loc):
        # If the location information in the struct isn't enough
        # we can parse the span at the srcloc to tease apart its
        # components. 
        cases(O.Option) maybe-ast(self.loc):
          | some(ast) =>
            # If we can parse
            ...
          | none      =>
            # If we cannot parse
            ...
        end
      else:
        # If the file is not available
        ...
      end
    end,
    method render-reason(self):
      if self.loc.is-builtin():
        # Errors here should end with `please-report-bug()`
        ...
      else:
        # This branch is for when we have an actual srcloc
        ...
      end
    end
  | message-exception(message :: String) with:
    method render-fancy-reason(self, _, _):
      self.render-reason()
    end,
    method render-reason(self):
      [ED.error: [ED.para: ED.text(self.message)]]
    end
  | update-non-obj(loc, obj, objloc) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      if self.loc.is-builtin():
        [ED.error:
          [ED.para:
            ED.text("A reference update expression in "),
            ED.loc(self.loc),
            ED.text(" failed because its left-hand-side evaluated to a non object value:")],
           ED.embed(self.obj),
           please-report-bug()]
      else:
        [ED.error:
          ed-intro("reference update expression", self.loc, -1),
          ED.cmcode(self.loc),
          [ED.para:
            ED.text("failed because the "),
            ED.highlight(ED.text("left hand side"), [ED.locs: self.objloc], 0),
            ED.text(" is expected to evaluate to an object, but its value was: ")],
          ED.embed(self.obj)]
      end
    end,
    method render-reason(self):
      if self.loc.is-builtin():
        [ED.error:
          [ED.para:
            ED.text("A reference update expression in "),
            ED.loc(self.loc),
            ED.text(" failed because its left-hand-side evaluated to a non object value:")],
           ED.embed(self.obj),
           please-report-bug()]
      else:
        [ED.error:
          [ED.para:
            ED.text("A reference update expression in "),
            ED.loc(self.loc),
            ED.text(" failed because its left-hand-side evaluated to a non object value:")],
           ED.embed(self.obj)]
      end
    end
  | update-frozen-ref(loc, obj, objloc, field, fieldloc) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      if self.loc.is-builtin():
        [ED.error:
          [ED.para:
            ED.text("A reference update expression in "),
            ED.loc(self.loc),
            ED.text(" failed because the field "),
            ED.code(ED.text(self.field)),
            ED.text(" is frozen in the object:")],
          ED.embed(self.obj),
          please-report-bug()]
      else:
        [ED.error:
          ed-intro("reference update expression", self.loc, -1),
          ED.cmcode(self.loc),
          [ED.para:
            ED.text("failed because the "),
            ED.highlight(ED.text("field"), [ED.locs: self.fieldloc], 0),
            ED.text(" is frozen in the "),
            ED.highlight(ED.text("object:"), [ED.locs: self.objloc], 1)],
          ED.embed(self.obj)]
      end
    end,
    method render-reason(self):
      if self.loc.is-builtin():
        [ED.error:
          [ED.para:
            ED.text("A reference update expression in "),
            ED.loc(self.loc),
            ED.text(" failed because the field "),
            ED.code(ED.text(self.field)),
            ED.text(" is frozen in the object:")],
          ED.embed(self.obj),
          please-report-bug()]
      else:
        [ED.error:
          [ED.para:
            ED.text("A reference update expression in "),
            ED.loc(self.loc),
            ED.text(" failed because the field "),
            ED.code(ED.text(self.field)),
            ED.text(" is frozen in the object:")],
          ED.embed(self.obj)]
      end
    end
  | update-non-ref(loc, obj, objloc, field, fieldloc) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      if self.loc.is-builtin():
        [ED.error:
          [ED.para:
            ED.text("A reference update expression in "),
            ED.loc(self.loc),
            ED.text(" failed because the field "),
            ED.code(ED.text(self.field)),
            ED.text(" is not a reference in the object:")],
          ED.embed(self.obj),
          please-report-bug()]
      else:
        [ED.error:
          [ED.para:
            ED.text("The reference update expression")],
          ED.cmcode(self.loc),
          [ED.para:
            ED.text("failed because the "),
            ED.highlight(ED.text("field"), [ED.locs: self.fieldloc], 0),
            ED.text(" is not a reference in the "),
            ED.highlight(ED.text("object:"), [ED.locs: self.objloc], 1)],
          ED.embed(self.obj)]
      end
    end,
    method render-reason(self):
      if self.loc.is-builtin():
        [ED.error:
          [ED.para:
            ED.text("A reference update expression in "),
            ED.loc(self.loc),
            ED.text(" failed because the field "),
            ED.code(ED.text(self.field)),
            ED.text(" is not a reference in the object:")],
          ED.embed(self.obj),
          please-report-bug()]
      else:
        [ED.error:
          [ED.para:
            ED.text("A reference update expression in "),
            ED.loc(self.loc),
            ED.text(" failed because the field "),
            ED.code(ED.text(self.field)),
            ED.text(" is not a reference in the object:")],
          ED.embed(self.obj)]
      end
    end
  | update-non-existent-field(loc, obj, objloc, field, fieldloc) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      if self.loc.is-builtin():
        [ED.error:
          [ED.para:
            ED.text("A reference update expression in "),
            ED.loc(self.loc),
            ED.text(" failed because the field "),
            ED.code(ED.text(self.field)),
            ED.text(" does not exist in the object:")],
          ED.embed(self.obj),
          please-report-bug()]
      else:
        [ED.error:
          ed-intro("reference update expression", self.loc, -1),
          ED.cmcode(self.loc),
          [ED.para:
            ED.text("failed because the "),
            ED.highlight(ED.text("field"), [ED.locs: self.fieldloc], 0),
            ED.text(" is does not exist in the "),
            ED.highlight(ED.text("object:"), [ED.locs: self.objloc], 1)],
          ED.embed(self.obj)]
      end
    end,
    method render-reason(self):
      if self.loc.is-builtin():
        [ED.error:
          [ED.para:
            ED.text("A reference update expression in "),
            ED.loc(self.loc),
            ED.text(" failed because the fieldv"),
            ED.code(ED.text(self.field)),
            ED.text(" does not exist in the object:")],
          ED.embed(self.obj),
          please-report-bug()]
      else:
        [ED.error:
          [ED.para:
            ED.text("A reference update expression in "),
            ED.loc(self.loc),
            ED.text(" failed because the fieldv"),
            ED.code(ED.text(self.field)),
            ED.text(" does not exist in the object:")],
          ED.embed(self.obj)]
      end
    end
  | no-cases-matched(loc, val) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      if self.loc.is-builtin():
        [ED.error:
          [ED.para:
            ED.text("A cases expression in "),
            ED.loc(self.loc),
            ED.text(" does not have a branch that matches the value:")],
           ED.embed(self.val),
           please-report-bug()]
      else:
        cases(O.Option) maybe-ast(self.loc):
          | some(ast-cse) =>
            [ED.error:
              [ED.para:
                ED.text("The "),
                ED.highlight(ED.text("cases expression"), [ED.locs: self.loc],0),
                ED.text(" expects there to always be a branch matching the value of the "),
                ED.highlight(ED.text("argument"),[ED.locs: ast-cse.val.l],1),
                ED.text(".")],
              [ED.para:
                ED.text("The value of the "),
                ED.highlight(ED.text("argument"),[ED.locs: ast-cse.val.l],1),
                ED.text(" was:")],
               ED.embed(self.val)]
          | none =>
            [ED.error:
              [ED.para:
                ED.text("The cases expression")],
               ED.cmcode(self.loc),
              [ED.para:
                ED.text(" does not have a branch that matches the value of the argument:")],
               ED.embed(self.val)]
        end
      end
    end,
    method render-reason(self):
      if self.loc.is-builtin():
        [ED.error:
          [ED.para:
            ED.text("A cases expression in "),
            ED.loc(self.loc),
            ED.text(" does not have a branch that matches the value:")],
           ED.embed(self.val),
           please-report-bug()]
      else:
        [ED.error:
          [ED.para:
            ED.text("A cases expression in "),
            ED.loc(self.loc),
            ED.text(" does not have a branch that matches the value:")],
           ED.embed(self.val)]
      end
    end
  | no-branches-matched(loc, expression :: String) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      if self.loc.is-builtin():
        [ED.error:
          [ED.para:
            ED.text("An "),
            ED.text(self.expression),
            ED.text(" expression in "),
            ED.loc(self.loc),
            ED.text(" expected that the condition of at least one branch could be statisfied, but no branch conditions were satisfied, so no branch could be entered.")],
          please-report-bug()]
      else if src-available(self.loc):
        [ED.error:
          ed-intro(self.expression + " expression", self.loc, 0),
          ED.cmcode(self.loc),
          [ED.para:
            ED.text(" expects that the condition of at least one branch be satisfied. No branch conditions were satisfied, so no branch could be entered.")]]
      else:
        [ED.error:
          [ED.para:
            ED.text("The "),
            ED.text(self.expression),
            ED.text(" expression in "),
            ED.loc(self.loc),
            ED.text(" expected that the condition of at least one branch could be statisfied, but no branch conditions were satisfied, so no branch could be entered.")]]
      end
    end,
    method render-reason(self):
      if self.loc.is-builtin():
        [ED.error:
          [ED.para:
            ED.text("An "),
            ED.text(self.expression),
            ED.text(" expression in "),
            ED.loc(self.loc),
            ED.text(" expected that the condition of at least one branch could be statisfied, but no branch conditions were satisfied, so no branch could be entered.")],
          please-report-bug()]
      else:
        [ED.error:
          [ED.para:
            ED.text("The "),
            ED.text(self.expression),
            ED.text(" expression in "),
            ED.loc(self.loc),
            ED.text(" expected that the condition of at least one branch could be statisfied, but no branch conditions were satisfied, so no branch could be entered.")]]
      end
    end
  | internal-error(message, info-args) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      self.render-reason()
    end,
    method render-reason(self):
      [ED.error:
        [ED.para: ED.text("Internal error:"), ED.text(self.message)],
        [ED.para: ED.text("Relevant arguments:")],
        vert-list-values(self.info-args)]
    end
  | template-not-finished(loc) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      if self.loc.is-builtin():
        [ED.error:
          [ED.para: 
            ED.text("The program tried to evaluate an unfinished template expression in "),
            ED.loc(self.loc),
            ED.text(".")],
            please-report-bug()]
      else:
        [ED.error:
          [ED.para: 
            ED.text("The program tried to evaluate an "),
            ED.highlight(ED.text("unfinished template expression"), [ED.locs: self.loc], 0)]]
      end
    end,
    method render-reason(self):
      if self.loc.is-builtin():
        [ED.error:
          [ED.para: 
            ED.text("The program tried to evaluate an unfinished template expression in "),
            ED.loc(self.loc),
            ED.text(".")],
            please-report-bug()]
      else:
        [ED.error:
          [ED.para: 
            ED.text("The program tried to evaluate an unfinished template expression in "),
            ED.loc(self.loc),
            ED.text(".")]]
      end
    end
  | field-not-found(loc, obj, field :: String) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      if self.loc.is-builtin():
        [ED.error:
          [ED.para:
            ED.text("A field lookup expression in "),
            ED.loc(self.loc),
            ED.text(" failed because the object")],
           ED.embed(self.obj),
          [ED.para:
            ED.text("does not have a field named "),
            ED.code(ED.text(self.field))],
           please-report-bug()]
      else if src-available(self.loc):
        cases(O.Option) maybe-ast(self.loc):
          | some(ast) =>
            shadow ast = cases(Any) ast:
              | s-dot(_,_,_) => ast
              | s-app(_,f,_) => f
            end
            [ED.error:
              [ED.para:
                ED.text("The "),
                ED.highlight(ED.text("field lookup expression"), [ED.locs: self.loc], -1)],
               ED.cmcode(self.loc),
              [ED.para:
                ED.text("failed because the "),
                ED.highlight(ED.text("left hand side"),[ED.locs: ast.obj.l], 0),
                ED.text(" evaluated to an object that did not have a "),
                ED.highlight([ED.sequence: ED.text("field named "),
                                           ED.code(ED.text(self.field))],
                             [ED.locs: ast.field-loc()], 1),
                ED.text(":")],
               ED.embed(self.obj)]
          | none =>
            [ED.error:
              [ED.para:
                ED.text("The field lookup expression"),
               ED.cmcode(self.loc),
              [ED.para:
                ED.text("failed because the left hand side evaluated to an object that did not have a field named "),
                ED.code(ED.text(self.field)),
                ED.text(":")],
               ED.embed(self.obj)]]
        end
      else:
        [ED.error:
          [ED.para:
            ED.text("A field lookup expression in "),
            ED.loc(self.loc),
            ED.text(" failed because the object")],
           ED.embed(self.obj),
          [ED.para:
            ED.text("does not have a field named "),
            ED.code(ED.text(self.field))]]
      end
    end,
    method render-reason(self):
      if self.loc.is-builtin():
        [ED.error:
          [ED.para:
            ED.text("A field lookup expression in "),
            ED.loc(self.loc),
            ED.text(" failed because the object")],
           ED.embed(self.obj),
          [ED.para:
            ED.text("does not have a field named "),
            ED.code(ED.text(self.field))],
           please-report-bug()]
      else:
        [ED.error:
          [ED.para:
            ED.text("A field lookup expression in "),
            ED.loc(self.loc),
            ED.text(" failed because the object")],
           ED.embed(self.obj),
          [ED.para:
            ED.text("does not have a field named "),
            ED.code(ED.text(self.field))],
          ED.embed(self.obj)]
      end
    end
  | lookup-constructor-not-object(loc, constr-name :: String, field :: String) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      if self.loc.is-builtin():
        [ED.error:
          [ED.para:
            ED.text("A field lookup expression in "),
            ED.loc(self.loc),
            ED.text(" failed because the field "),
            ED.code(ED.text(self.field)),
            ED.text(" could not be looked up in the constructor "),
            ED.code(ED.text(self.constr-name))],
          please-report-bug()]
      else if src-available(self.loc):
        cases(O.Option) maybe-ast(self.loc):
          | some(ast) =>
            shadow ast = cases(Any) ast:
              | s-dot(_,_,_) => ast
              | s-app(_,f,_) => f
            end
            [ED.error:
              [ED.para:
                ED.text("The "),
                ED.highlight(ED.text("field lookup expression"), [ED.locs: self.loc], -1)],
               ED.cmcode(self.loc),
              [ED.para:
                ED.text("failed because the "),
                ED.highlight(ED.text("left hand side"), [ED.locs: ast.obj.l], 0),
                ED.text(" evaluated to a constructor ("),
                ED.code(ED.text(self.constr-name)),
                ED.text("), which is not an object.")]]
          | none      =>
            [ED.error:
              [ED.para:
                ED.text("The "),
                ED.highlight(ED.text("field lookup expression"), [ED.locs: self.loc], 0)],
               ED.cmcode(self.loc),
              [ED.para:
                ED.text("failed because the left hand side evaluated to a constructor ("),
                ED.code(ED.text(self.constr-name)),
                ED.text("), which is not an object that has a field named "),
                ED.code(ED.text(self.field))]]
        end
      else:
        [ED.error:
          [ED.para:
            ED.text("The field lookup expression in "),
            ED.loc(self.loc),
            ED.text(" failed because the left hand side evaluated to a constructor ("),
            ED.code(ED.text(self.constr-name)),
            ED.text("), which is not an object that has a field named "),
            ED.code(ED.text(self.field))]]
      end
    end,
    method render-reason(self):
      if self.loc.is-builtin():
        [ED.error:
          [ED.para:
            ED.text("A field lookup expression in the builtin location "), 
            ED.loc(self.loc),
            ED.text(" failed because the field "),
            ED.code(ED.text(self.field)),
            ED.text(" could not be looked up in a constructor ("),
            ED.code(ED.text(self.constr-name)),
            ED.text(").")],
          please-report-bug()]
      else:
        [ED.error:
          [ED.para:
            ED.text("The field lookup expression in "),
            ED.loc(self.loc),
            ED.text(" failed because the left hand side evaluated to a constructor ("),
            ED.code(ED.text(self.constr-name)),
            ED.text("), which is not an object that has a field named "),
            ED.code(ED.text(self.field))]]
      end
    end
  | lookup-non-tuple(loc, non-tup, index :: Number) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      if self.loc.is-builtin():
        [ED.error:
          [ED.para:
            ED.text("The tuple lookup expression in "),
            ED.loc(self.loc),
            ED.text(" failed because the left-hand-side did not evaluate to a tuple with a value at position "),
            ED.embed(self.index),
            ED.text(":")],
          ED.embed(self.non-tup),
          please-report-bug()]
      else if src-available(self.loc):
        cases(O.Option) maybe-ast(self.loc):
          | some(ast) =>
            [ED.error:
              ed-intro("tuple lookup expression", self.loc, -1),
              ED.cmcode(self.loc),
              [ED.para:
                ED.text("failed because the "),
                ED.highlight(ED.text("left hand side"), [ED.locs: ast.tup.l], 0),
                ED.text(" did not evaluate to a tuple:")],
              ED.embed(self.non-tup)]
          | none      =>
            [ED.error:
              [ED.para:
                ED.text("The tuple lookup expression in "),
                ED.loc(self.loc),
                ED.text(" failed because the left hand side did not evaluate to a tuple:")],
              ED.embed(self.non-tup)]
        end
      else:
        [ED.error:
          [ED.para:
            ED.text("The field lookup expression in "),
            ED.loc(self.loc),
            ED.text(" failed because the left hand side did not evaluate to a tuple:")],
          ED.embed(self.non-tup)]
      end
    end,
    method render-reason(self):
      if self.loc.is-builtin():
        [ED.error:
          [ED.para:
            ED.text("The field lookup expression in "),
            ED.loc(self.loc),
            ED.text(" failed because the left-hand-side did not evaluate to a tuple with a value at position "),
            ED.embed(self.index),
            ED.text(":")],
          ED.embed(self.non-tup),
          please-report-bug()]
      else:
        [ED.error:
          [ED.para:
            ED.text("The field lookup expression in "),
            ED.loc(self.loc),
            ED.text(" failed because the left hand side did not evaluate to a tuple:")],
          ED.embed(self.non-tup)]
      end
    end
  | lookup-large-index(loc, tup, len, index :: Number) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      if self.loc.is-builtin():
        [ED.error:
          [ED.para:
            ED.text("The tuple lookup expression in "),
            ED.loc(self.loc),
            ED.text(" failed because a value could not be found at the given position, "),
            ED.embed(self.index),
            ED.text(", because the left hand side evaluated to a tuple containing "),
            ED.ed-components(self.len),
            ED.text(":")],
          ED.embed(self.tup),
          please-report-bug()]
      else if src-available(self.loc):
        cases(O.Option) maybe-ast(self.loc):
          | some(ast) =>
            [ED.error:
              ed-intro("tuple lookup expression", self.loc, -1),
              ED.cmcode(self.loc),
              [ED.para:
                ED.text(" failed because a value could not be found at the given "),
                ED.highlight(ED.text("position"), [ED.locs: ast.index-loc], 1),
                ED.text(" because the "),
                ED.highlight(ED.text("left hand side"), [ED.locs: ast.tup.l], 0),
                ED.text(" evaluated to a tuple containing "),
                ED.ed-components(self.len),
                ED.text(":")],
              ED.embed(self.tup)]
          | none      =>
            [ED.error:
              ed-intro("tuple lookup expression", self.loc, 0),
              ED.cmcode(self.loc),
              [ED.para:
                ED.text(" failed because a value could not be found at the given position, "),
                ED.embed(self.index),
                ED.text(", because the left hand side evaluated to a tuple containing "),
                ED.ed-components(self.len),
                ED.text(":")],
              ED.embed(self.tup)]
        end
      else:
        [ED.error:
          [ED.para:
            ED.text("The tuple lookup expression in "),
            ED.loc(self.loc),
            ED.text(" failed because a value could not be found at the given position, "),
            ED.embed(self.index),
            ED.text(", because the left hand side evaluated to a tuple containing "),
            ED.ed-components(self.len),
            ED.text(":")],
          ED.embed(self.tup)]
      end
    end,
    method render-reason(self):
      if self.loc.is-builtin():
        [ED.error:
          [ED.para:
            ED.text("The tuple lookup expression in "),
            ED.loc(self.loc),
            ED.text(" failed because a value could not be found at the given position, "),
            ED.embed(self.index),
            ED.text(", because the left hand side evaluated to a tuple containing "),
            ED.embed(self.els),
            ED.text(" components:")],
          ED.embed(self.tup),
          please-report-bug()]
      else:
        [ED.error:
          [ED.para:
            ED.text("The tuple lookup expression in "),
            ED.loc(self.loc),
            ED.text(" failed because a value could not be found at the given position, "),
            ED.embed(self.index),
            ED.text(", because the left hand side evaluated to a tuple containing "),
            ED.embed(self.els),
            ED.text(" components:")],
          ED.embed(self.tup)]
      end
    end
  | non-tuple-bind(loc, non-tup) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      if self.loc.is-builtin():
        [ED.error:
          [ED.para:
            ED.text("The tuple binding statement in "),
            ED.loc(self.loc),
            ED.text(" failed because the right hand side did not evaluate to a tuple with a value at position "),
            ED.embed(self.index),
            ED.text(":")],
          ED.embed(self.non-tup),
          please-report-bug()]
      else if src-available(self.loc):
        cases(O.Option) maybe-ast(self.loc):
          | some(ast) =>
            [ED.error:
              ed-intro("tuple binding statement", self.loc, -1),
              ED.cmcode(self.loc),
              [ED.para:
                ED.text("failed because the "),
                ED.highlight(ED.text("right hand side"), [ED.locs: ast.tup.l], 0),
                ED.text(" did not evaluate to a tuple:")],
              ED.embed(self.non-tup)]
          | none      =>
            [ED.error:
              [ED.para:
                ED.text("The tuple binding statement in "),
                ED.loc(self.loc),
                ED.text(" failed because the right hand side did not evaluate to a tuple:")],
              ED.embed(self.non-tup)]
        end
      else:
        [ED.error:
          [ED.para:
            ED.text("The tuple binding statement in "),
            ED.loc(self.loc),
            ED.text(" failed because the right hand side did not evaluate to a tuple:")],
          ED.embed(self.non-tup)]
      end
    end,
    method render-reason(self):
      if self.loc.is-builtin():
        [ED.error:
          [ED.para:
            ED.text("The tuple binding statement in "),
            ED.loc(self.loc),
            ED.text(" failed because the right hand side did not evaluate to a tuple:")],
          ED.embed(self.non-tup),
          please-report-bug()]
      else:
        [ED.error:
          [ED.para:
            ED.text("The tuple binding statement in "),
            ED.loc(self.loc),
            ED.text(" failed because the right hand side did not evaluate to a tuple:")],
          ED.embed(self.non-tup)]
      end
    end
  | bad-tuple-bind(loc, tup, length, desiredLength) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      if self.loc.is-builtin():
        [ED.error:
          [ED.para:
            ED.text("The tuple binding in"),
            ED.loc(self.loc),
            ED.text("  failed because "),
            ED.ed-components(self.desiredLength),
            ED.text(" are expected to be bound to values, but the right hand side evaluated to a tuple containing "),
            ED.ed-components(self.els),
            ED.text(":")],
          ED.embed(self.tup),
          please-report-bug()]
      else if src-available(self.loc):
        cases(O.Option) maybe-ast(self.loc):
          | some(ast) =>
            [ED.error:
              ed-intro("tuple binding", self.loc, -1),
              ED.cmcode(self.loc),
              [ED.para:
                ED.text("failed because "),
                ED.highlight(ED.ed-names(self.desiredLength), ast.names.map(_.l), 0),
                ED.text(" are expected to be bound to values, but the "),
                ED.highlight(ED.text("right hand side"), [ED.locs: ast.tup.l], 1),
                ED.text(" evaluated to a tuple containing "),
                ED.ed-components(self.length),
                ED.text(":")],
              ED.embed(self.tup)]
          | none      =>
            [ED.error:
              ed-intro("tuple binding", self.loc, 0),
              ED.cmcode(self.loc),
              [ED.para:
                ED.text("failed because "),
                ED.embed(self.desiredLength),
                ED.ed-components(" are expected to be bound to values, but the right hand side evaluated to a tuple containing "),
                ED.ed-components(self.length),
                ED.text(":")],
              ED.embed(self.tup)]
        end
      else:
        [ED.error:
          [ED.para:
            ED.text("The tuple binding in"),
            ED.loc(self.loc),
            ED.text("  failed because "),
            ED.ed-components(self.desiredLength),
            ED.text(" are expected to be bound to values, but the right hand side evaluated to a tuple containing "),
            ED.ed-components(self.length),
            ED.text(":")],
          ED.embed(self.tup)]
      end
    end,
    method render-reason(self):
      if self.loc.is-builtin():
        [ED.error:
          [ED.para:
            ED.text("The tuple binding in"),
            ED.loc(self.loc),
            ED.text("  failed because "),
            ED.ed-components(self.desiredLength),
            ED.text(" are expected to be bound to values, but the right hand side evaluated to a tuple containing "),
            ED.ed-components(self.length),
            ED.text(":")],
          ED.embed(self.tup),
          please-report-bug()]
      else:
        [ED.error:
          [ED.para:
            ED.text("The tuple binding in"),
            ED.loc(self.loc),
            ED.text("  failed because "),
            ED.ed-components(self.desiredLength),
            ED.text(" are expected to be bound to values, but the right hand side evaluated to a tuple containing "),
            ED.ed-components(self.length),
            ED.text(":")],
          ED.embed(self.tup)]
      end
    end
  | lookup-non-object(loc, non-obj, field :: String) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      if self.loc.is-builtin():
        [ED.error:
          [ED.para:
            ED.text("A field lookup expression in "),
            ED.loc(self.loc),
            ED.text(" failed because its left-hand-side evaluated to a non-object value that did not have a field named "),
            ED.code(ED.text(self.field)),
            ED.text(":")],
           ED.embed(self.non-obj),
           please-report-bug()]
      else if src-available(self.loc):
        cases(O.Option) maybe-ast(self.loc):
          | some(ast) =>
            shadow ast = cases(Any) ast:
              | s-dot(_,_,_) => ast
              | s-app(_,f,_) => f
            end
            [ED.error:
              ed-intro("field lookup expression", self.loc, -1),
              ED.cmcode(self.loc),
              [ED.para:
                ED.text("failed because the "),
                ED.highlight(ED.text("left hand side"),[ED.locs: ast.obj.l], 0),
                ED.text(" evaluated to a non-object value:"),
                ED.text(":")],
               ED.embed(self.non-obj)]
          | none =>
            [ED.error:
              ed-intro("field lookup expression", self.loc, 0),
              ED.cmcode(self.loc),
              [ED.para:
                ED.text("failed because the left hand side evaluated to a non-object value:")],
               ED.embed(self.non-obj)]
        end
      else:
        [ED.error:
          [ED.para:
            ED.text("A field lookup expression in "),
            ED.loc(self.loc),
            ED.text(" failed because its left-hand-side evaluated to a non-object value:")],
           ED.embed(self.non-obj)]
      end
    end,
    method render-reason(self):
      if self.loc.is-builtin():
        [ED.error:
          [ED.para:
            ED.text("A field lookup expression in "),
            ED.loc(self.loc),
            ED.text(" failed because its left-hand-side evaluated to a non-object value that did not have a field named "),
            ED.code(ED.text(self.field)),
            ED.text(":")],
           ED.embed(self.non-obj),
           please-report-bug()]
      else:
        [ED.error:
          [ED.para:
            ED.text("A field lookup expression in "),
            ED.loc(self.loc),
            ED.text(" failed because its left-hand-side evaluated to a non-object value:")],
           ED.embed(self.non-obj)]
      end
    end
  | extend-non-object(loc, non-obj) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      if self.loc.is-builtin():
        [ED.error:
          [ED.para:
            ED.text("An object extension expression in "),
            ED.loc(self.loc),
            ED.text(" failed because its left-hand-side evaluated to a non-object value that did not have a field named "),
            ED.code(ED.text(self.field)),
            ED.text(":")],
           ED.embed(self.non-obj),
           please-report-bug()]
      else if src-available(self.loc):
        cases(O.Option) maybe-ast(self.loc):
          | some(ast) =>
            [ED.error:
               ed-intro("object extension expression", self.loc, -1),
               ED.cmcode(self.loc),
              [ED.para:
                ED.text("failed because the "),
                ED.highlight(ED.text("left hand side"),[ED.locs: ast.supe.l], 0),
                ED.text(" evaluated to a non-object value:"),
                ED.text(":")],
               ED.embed(self.non-obj)]
          | none =>
            [ED.error:
              ed-intro("object extension expression", self.loc, -1),
              ED.cmcode(self.loc),
              [ED.para:
                ED.text("failed because the left hand side evaluated to a non-object value:")],
               ED.embed(self.non-obj)]
        end
      else:
        [ED.error:
          [ED.para:
            ED.text("The object extension expression in "),
            ED.loc(self.loc),
            ED.text(" failed because its left-hand-side evaluated to a non-object value:")],
           ED.embed(self.non-obj)]
      end
    end,
    method render-reason(self):
      if self.loc.is-builtin():
        [ED.error:
          [ED.para:
            ED.text("An object extension expression in "),
            ED.loc(self.loc),
            ED.text(" failed because its left-hand-side evaluated to a non-object value that did not have a field named "),
            ED.code(ED.text(self.field)),
            ED.text(":")],
           ED.embed(self.non-obj),
           please-report-bug()]
      else:
        [ED.error:
          [ED.para:
            ED.text("An object extension expression in "),
            ED.loc(self.loc),
            ED.text(" failed because its left-hand-side evaluated to a non-object value:")],
           ED.embed(self.non-obj)]
      end
    end
  | non-boolean-condition(loc, typ, value) with:
    method render-fancy-reason(self, _, _):
      self.render-reason() # TODO!!
    end,
    method render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Expected"), ED.code(ED.text("true")), ED.text("or"), ED.code(ED.text("false")),
          ED.text("for the test in the"), ED.text(self.typ), ED.text("expression at"),
          draw-and-highlight(self.loc), ED.text(" but got:")],
        ED.embed(self.value)]
    end
  | non-boolean-op(loc, position, typ, value) with:
    method render-fancy-reason(self, _, _):
      self.render-reason() # TODO!!
    end,
    method render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Expected"), ED.code(ED.text("true")), ED.text("or"), ED.code(ED.text("false")),
          ED.text("for the"), ED.text(self.position), ED.text("argument in the"),
          ED.text(self.typ), ED.text("expression at"),
          draw-and-highlight(self.loc), ED.text(" but got:")],
        ED.embed(self.value)]
    end
  | generic-type-mismatch(val, typ :: String) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
    [ED.error:
      cases(O.Option) maybe-stack-loc(0, false):
        | some(loc) =>
          if loc.is-builtin():
            [ED.sequence:
              [ED.para:
                ED.text("An expression in "),
                ED.loc(loc),
                ED.text(" was expected to evaluate to a "),
                ED.embed(self.typ),
                ED.text(". It evaluated to the non-"),
                ED.embed(self.typ),
                ED.text(" value:")],
              ED.embed(self.val),
              please-report-bug()]
          else if src-available(loc):
            [ED.sequence:
              [ED.para:
                ED.text("The expression")],
              ED.cmcode(loc),
              [ED.para:
                ED.text("was expected to evaluate to a "),
                ED.embed(self.typ),
                ED.text(". It evaluated to the non-"),
                ED.embed(self.typ),
                ED.text(" value:")],
              ED.embed(self.val)]
          else:
            [ED.sequence:
              [ED.para:
                ED.text("The expression in "),
                ED.loc(loc),
                ED.text(" was expected to evaluate to a "),
                ED.embed(self.typ),
                ED.text(". It evaluated to the non-"),
                ED.embed(self.typ),
                ED.text(" value:")],
              ED.embed(self.val)]
          end
        | none =>
          [ED.sequence:
            [ED.para:
                ED.text("An expression was expected to evaluate to a "),
                ED.embed(self.typ),
                ED.text(". It evaluated to the non-"),
                ED.embed(self.typ),
                ED.text(" value:")],
              ED.embed(self.val),
              please-report-bug()]
      end]
    end, 
    method render-reason(self):
      [ED.error:
        ED.maybe-stack-loc(0, true,
          lam(loc):
            if loc.is-builtin():
              [ED.sequence:
                [ED.para:
                  ED.text("An expression in "),
                  ED.loc(loc),
                  ED.text(" was expected to evaluate to a "),
                  ED.embed(self.typ),
                  ED.text(". It evaluated to the non-"),
                  ED.embed(self.typ),
                  ED.text(" value:")],
                ED.embed(self.val),
                please-report-bug()]
            else:
              [ED.sequence:
                [ED.para:
                  ED.text("The expression in "),
                  ED.loc(loc),
                  ED.text(" was expected to evaluate to a "),
                  ED.embed(self.typ),
                  ED.text(". It evaluated to the non-"),
                  ED.embed(self.typ),
                  ED.text(" value:")],
                ED.embed(self.val)]
            end
          end,
          [ED.sequence:
            [ED.para:
                ED.text("An expression was expected to evaluate to a "),
                ED.embed(self.typ),
                ED.text(". It evaluated to the non-"),
                ED.embed(self.typ),
                ED.text(" value:")],
              ED.embed(self.val),
              please-report-bug()])]
    end
  | num-string-binop-error(val1, val2, opname, opdesc, methodname) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      [ED.error:
        cases(O.Option) maybe-stack-loc(0, false):
          | some(loc) =>
            if loc.is-builtin():
              [ED.sequence:
                [ED.para:
                  ED.text("A binary "),
                  ED.code(ED.text(self.opdesc)),
                  ED.text(" operator expression in "),
                  ED.loc(loc),
                  ED.text(" failed because the left hand side evaluated to")],
                ED.embed(self.val1),
                [ED.para:
                  ED.text("and the right hand side evaluated to")],
                ED.embed(self.val2),
                please-report-bug(),
                [ED.para:
                  ED.text("The expression expects to be given:"),
                  [ED.bulleted:
                    ED.text("two Numbers, or"),
                    ED.text("two Strings, or"),
                    [ED.sequence: 
                      ED.text("a left hand side that has a method named "), 
                      ED.code(ED.text(self.methodname))]]]]
            else if src-available(loc):
              cases(O.Option) maybe-ast(loc):
                | some(ast) =>
                  left-loc =  ast.left.l
                  right-loc = ast.right.l
                  [ED.sequence:
                    [ED.para:
                      ED.text("The "),
                      ED.highlight(
                        [ED.sequence:
                          ED.text("binary "),
                          ED.code(ED.text(self.opdesc)),
                          ED.text(" operator expression")],
                        [ED.locs: loc], -1)],
                     ED.cmcode(loc),
                     [ED.para:
                        ED.text("failed because the "),
                        ED.highlight(ED.text("left hand side"), [ED.locs: left-loc],0),
                        ED.text(" evaluated to")],
                      ED.embed(self.val1),
                      [ED.para:
                        ED.text("and the "),
                        ED.highlight(ED.text("right hand side"), [ED.locs: right-loc],1),
                        ED.text(" evaluated to")],
                      ED.embed(self.val2),
                    [ED.para:
                      ED.text("The expression expects to be given:"),
                      [ED.bulleted:
                        ED.text("two Numbers, or"),
                        ED.text("two Strings, or"),
                        [ED.sequence: 
                          ED.text("a "),
                          ED.highlight(ED.text("left hand side"), [ED.locs: left-loc], 0),
                          ED.text(" that has a method named "), 
                          ED.code(ED.text(self.methodname))]]]]
                | none      =>
                  [ED.sequence:
                    [ED.para:
                      ED.text("The "),
                      ED.highlight(
                        [ED.sequence:
                          ED.text("binary "),
                          ED.code(ED.text(self.opdesc)),
                          ED.text(" operator expression")],
                        [ED.locs: loc], 0)],
                    ED.cmcode(loc),
                    [ED.para:
                      ED.text("failed because the left hand side evaluated to")],
                    ED.embed(self.val1),
                    [ED.para:
                      ED.text("and the right hand side evaluated to")],
                    ED.embed(self.val2),
                    [ED.para:
                      ED.text("The expression expects to be given:"),
                      [ED.bulleted:
                        ED.text("two Numbers, or"),
                        ED.text("two Strings, or"),
                        [ED.sequence: 
                          ED.text("a left hand side that has a method named "), 
                          ED.code(ED.text(self.methodname))]]],
                    please-report-bug()]
              end
            else:
              [ED.sequence:
                [ED.para:
                  ED.text("The binary "),
                  ED.code(ED.text(self.opdesc)),
                  ED.text(" operator expression in "),
                  ED.loc(loc),
                  ED.text(" failed because the left hand side evaluated to")],
                ED.embed(self.val1),
                [ED.para:
                  ED.text("and the right hand side evaluated to")],
                ED.embed(self.val2),
                [ED.para:
                  ED.text("The expression expects to be given:"),
                  [ED.bulleted:
                    ED.text("two Numbers, or"),
                    ED.text("two Strings, or"),
                    [ED.sequence: 
                      ED.text("a left hand side that has a method named "), 
                      ED.code(ED.text(self.methodname))]]],
                please-report-bug()]
            end
        | none =>
          [ED.sequence:
            [ED.para:
              ED.text("A binary "),
              ED.code(ED.text(self.opdesc)),
              ED.text(" operator expression failed because the left hand side evaluated to")],
            ED.embed(self.val1),
            [ED.para:
              ED.text("and the right hand side evaluated to")],
            ED.embed(self.val2),
            [ED.para:
              ED.text("The expression expects to be given:"),
              [ED.bulleted:
                ED.text("two Numbers, or"),
                ED.text("two Strings, or"),
                [ED.sequence: 
                  ED.text("a left hand side that has a method named "), 
                  ED.code(ED.text(self.methodname))]]],
            please-report-bug()]
      end]
    end,
    method render-reason(self):
      [ED.error: ED.maybe-stack-loc(0, false,
        lam(loc):
          if loc.is-builtin():
            [ED.sequence:
              [ED.para:
                ED.text("A binary "),
                ED.code(ED.text(self.opdesc)),
                ED.text(" operator expression in "),
                ED.loc(loc),
                ED.text(" failed because the left hand side evaluated to")],
              ED.embed(self.val1),
              [ED.para:
                ED.text("and the right hand side evaluated to")],
              ED.embed(self.val2),
              please-report-bug(),
              [ED.para:
                ED.text("The expression expects to be given:"),
                [ED.bulleted:
                  ED.text("two Numbers, or"),
                  ED.text("two Strings, or"),
                  [ED.sequence: 
                    ED.text("a left hand side that has a method named "), 
                    ED.code(ED.text(self.methodname))]]]]
          else:
            [ED.sequence:
              [ED.para:
                ED.text("A binary "),
                ED.code(ED.text(self.opdesc)),
                ED.text(" operator expression in "),
                ED.loc(loc),
                ED.text(" failed because the left hand side evaluated to")],
              ED.embed(self.val1),
              [ED.para:
                ED.text("and the right hand side evaluated to")],
              ED.embed(self.val2),
              [ED.para:
                ED.text("The expression expects to be given:"),
                [ED.bulleted:
                  ED.text("two Numbers, or"),
                  ED.text("two Strings, or"),
                  [ED.sequence: 
                    ED.text("a left hand side that has a method named "), 
                    ED.code(ED.text(self.methodname))]]]]
          end
        end,
        [ED.sequence:
          [ED.para:
            ED.text("A binary "),
            ED.code(ED.text(self.opdesc)),
            ED.text(" operator expression failed because the left hand side evaluated to")],
          ED.embed(self.val1),
          [ED.para:
            ED.text("and the right hand side evaluated to")],
          ED.embed(self.val2),
          [ED.para:
            ED.text("The expression expects to be given:"),
            [ED.bulleted:
              ED.text("two Numbers, or"),
              ED.text("two Strings, or"),
              [ED.sequence: 
                ED.text("a left hand side that has a method named "), 
                ED.code(ED.text(self.methodname))]]],
          please-report-bug()])]
    end
  | numeric-binop-error(val1, val2, opname, opdesc, methodname) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      [ED.error:
        cases(O.Option) maybe-stack-loc(0, false):
          | some(loc) =>
            if loc.is-builtin():
              [ED.sequence:
                [ED.para:
                  ED.text("A binary "),
                  ED.code(ED.text(self.opdesc)),
                  ED.text(" operator expression in "),
                  ED.loc(loc),
                  ED.text(" failed because the left hand side evaluated to")],
                ED.embed(self.val1),
                [ED.para:
                  ED.text("and the right hand side evaluated to")],
                ED.embed(self.val2),
                please-report-bug(),
                [ED.para:
                  ED.text("The expression expects to be given:"),
                  [ED.bulleted:
                    ED.text("two Numbers, or"),
                    [ED.sequence: 
                      ED.text("a left hand side that has a method named "), 
                      ED.code(ED.text(self.methodname))]]]]
            else if src-available(loc):
              cases(O.Option) maybe-ast(loc):
                | some(ast) =>
                  left-loc =  ast.left.l
                  right-loc = ast.right.l
                  [ED.sequence:
                    [ED.para:
                      ED.text("The "),
                      ED.highlight(
                        [ED.sequence:
                          ED.text("binary "),
                          ED.code(ED.text(self.opdesc)),
                          ED.text(" operator expression")],
                        [ED.locs: loc], -1)],
                    ED.cmcode(loc),
                    [ED.para:
                        ED.text("failed because the "),
                        ED.highlight(ED.text("left hand side"), [ED.locs: left-loc],0),
                        ED.text(" evaluated to")],
                      ED.embed(self.val1),
                      [ED.para:
                        ED.text("and the "),
                        ED.highlight(ED.text("right hand side"), [ED.locs: right-loc],1),
                        ED.text(" evaluated to")],
                      ED.embed(self.val2),
                    [ED.para:
                      ED.text("The expression expects to be given:"),
                      [ED.bulleted:
                        ED.text("two Numbers, or"),
                        [ED.sequence: 
                          ED.text("a "),
                          ED.highlight(ED.text("left hand side"), [ED.locs: left-loc], 0),
                          ED.text(" that has a method named "), 
                          ED.code(ED.text(self.methodname))]]]]
                | none      =>
                  [ED.sequence:
                    [ED.para:
                      ED.text("The "),
                      ED.highlight(
                        [ED.sequence:
                          ED.text("binary "),
                          ED.code(ED.text(self.opdesc)),
                          ED.text(" operator expression")],
                        [ED.locs: loc], 0)],
                    ED.cmcode(loc),
                    [ED.para:
                      ED.text("failed because the left hand side evaluated to")],
                    ED.embed(self.val1),
                    [ED.para:
                      ED.text("and the right hand side evaluated to")],
                    ED.embed(self.val2),
                    [ED.para:
                      ED.text("The expression expects to be given:"),
                      [ED.bulleted:
                        ED.text("two Numbers, or"),
                        [ED.sequence: 
                          ED.text("a left hand side that has a method named "), 
                          ED.code(ED.text(self.methodname))]]],
                    please-report-bug()]
              end
            else:
              [ED.sequence:
                [ED.para:
                  ED.text("The binary "),
                  ED.code(ED.text(self.opdesc)),
                  ED.text(" operator expression in "),
                  ED.loc(loc),
                  ED.text(" failed because the left hand side evaluated to")],
                ED.embed(self.val1),
                [ED.para:
                  ED.text("and the right hand side evaluated to")],
                ED.embed(self.val2),
                [ED.para:
                  ED.text("The expression expects to be given:"),
                  [ED.bulleted:
                    ED.text("two Numbers, or"),
                    [ED.sequence: 
                      ED.text("a left hand side that has a method named "), 
                      ED.code(ED.text(self.methodname))]]],
                please-report-bug()]
            end
          | none =>
            [ED.sequence:
              [ED.para:
                ED.text("A binary "),
                ED.code(ED.text(self.opdesc)),
                ED.text(" operator expression failed because the left hand side evaluated to")],
              ED.embed(self.val1),
              [ED.para:
                ED.text("and the right hand side evaluated to")],
              ED.embed(self.val2),
              [ED.para:
                ED.text("The expression expects to be given:"),
                [ED.bulleted:
                  ED.text("two Numbers, or"),
                  [ED.sequence: 
                    ED.text("a left hand side that has a method named "), 
                    ED.code(ED.text(self.methodname))]]],
              please-report-bug()]
      end]
    end,
    method render-reason(self):
      [ED.error: ED.maybe-stack-loc(0, false,
        lam(loc):
          if loc.is-builtin():
            [ED.sequence:
              [ED.para:
                ED.text("A binary "),
                ED.code(ED.text(self.opdesc)),
                ED.text(" operator expression in "),
                ED.loc(loc),
                ED.text(" failed because the left hand side evaluated to")],
              ED.embed(self.val1),
              [ED.para:
                ED.text("and the right hand side evaluated to")],
              ED.embed(self.val2),
              please-report-bug(),
              [ED.para:
                ED.text("The expression expects to be given:"),
                [ED.bulleted:
                  ED.text("two Numbers, or"),
                  [ED.sequence: 
                    ED.text("a left hand side that has a method named "), 
                    ED.code(ED.text(self.methodname))]]]]
          else:
            [ED.sequence:
              [ED.para:
                ED.text("A binary "),
                ED.code(ED.text(self.opdesc)),
                ED.text(" operator expression in "),
                ED.loc(loc),
                ED.text(" failed because the left hand side evaluated to")],
              ED.embed(self.val1),
              [ED.para:
                ED.text("and the right hand side evaluated to")],
              ED.embed(self.val2),
              [ED.para:
                ED.text("The expression expects to be given:"),
                [ED.bulleted:
                  ED.text("two Numbers, or"),
                  [ED.sequence: 
                    ED.text("a left hand side that has a method named "), 
                    ED.code(ED.text(self.methodname))]]]]
          end
        end,
        [ED.sequence:
          [ED.para:
            ED.text("A binary "),
            ED.code(ED.text(self.opdesc)),
            ED.text(" operator expression failed because the left hand side evaluated to")],
          ED.embed(self.val1),
          [ED.para:
            ED.text("and the right hand side evaluated to")],
          ED.embed(self.val2),
          [ED.para:
            ED.text("The expression expects to be given:"),
            [ED.bulleted:
              ED.text("two Numbers, or"),
              [ED.sequence: 
                ED.text("a left hand side that has a method named "), 
                ED.code(ED.text(self.methodname))]]],
          please-report-bug()])]
    end
  | cases-singleton-mismatch(branch-loc, should-be-singleton :: Boolean, cases-loc) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      if self.branch-loc.is-builtin():
        [ED.error:
          [ED.para:
            ED.text("A cases branch in "),
            ED.loc(self.branch-loc),
            if self.should-be-singleton:
              ED.text(" has an argument list but the corresponding variant is a singleton.")
            else:
              ED.text(" doesn't have an argument list in its pattern, but the corresponding variant is not a singleton.")
            end],
          please-report-bug()]
      else if src-available(self.loc):
        cases(O.Option) maybe-ast(self.cases-loc):
          | some(ast) =>
            branch = ast.branches.find(lam(b): b.l.start-line == self.branch-loc.start-line end).value
            [ED.error:
              ed-intro("cases branch", self.branch-loc, -1),
              ED.cmcode(self.branch-loc),
              if self.should-be-singleton:
                [ED.para:
                  ED.text("has an "),
                  ED.highlight(ED.text("argument list"), branch.args.map(_.l), 0),
                  ED.text(" but the corresponding variant is a singleton.")]
              else:
                [ED.para:
                  ED.text("doesn't have an argument list in its "),
                  ED.highlight(ED.text("pattern"), [ED.locs: branch.pat-loc], 0),
                  ED.text(", but the corresponding variant is not a singleton.")]
              end]
          | none      =>
            [ED.error:
              ed-intro("cases branch", self.branch-loc, -1),
              ED.cmcode(self.branch-loc),
              if self.should-be-singleton:
                [ED.para:
                  ED.text("has an argument list but the corresponding variant is a singleton.")]
              else:
                [ED.para:
                  ED.text("doesn't have an argument list in its pattern, but the corresponding variant is not a singleton.")]
              end,
              please-report-bug()]
        end
      else:
        [ED.error:
          [ED.para:
            ED.text("The cases branch in "),
            ED.loc(self.branch-loc),
            if self.should-be-singleton:
              ED.text(" has an argument list but the corresponding variant is a singleton.")
            else:
              ED.text(" doesn't have an argument list in its pattern, but the corresponding variant is not a singleton.")
            end]]
      end
    end,
    method render-reason(self):
      if self.branch-loc.is-builtin():
        [ED.error:
          [ED.para:
            ED.text("A cases branch in "),
            ED.loc(self.branch-loc),
            if self.should-be-singleton:
              ED.text(" has an argument list but the corresponding variant is a singleton.")
            else:
              ED.text(" doesn't have an argument list in its pattern, but the corresponding variant is not a singleton.")
            end],
          please-report-bug()]
      else:
        [ED.error:
          [ED.para:
            ED.text("The cases branch in "),
            ED.loc(self.branch-loc),
            if self.should-be-singleton:
              ED.text(" has an argument list but the corresponding variant is a singleton.")
            else:
              ED.text(" doesn't have an argument list in its pattern, but the corresponding variant is not a singleton.")
            end]]
      end
    end
  | cases-arity-mismatch(branch-loc, num-args, actual-arity, cases-loc) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      if self.branch-loc.is-builtin():
        [ED.error:
          [ED.para:
            ED.text("A cases branch in "),
            ED.loc(self.branch-loc),
            ED.text(" expects that the pattern has has exactly the same number of field bindings as the variant of has fields.")],
          [ED.para:
            ED.text("The cases pattern for has "),
            ED.ed-field-bindings(self.num-args),
            ED.text(".")],
          [ED.para:
            ED.text("The corresponding variant of the datatype has "),
            ED.ed-fields(self.actual-arity)],
          please-report-bug()]
      else if src-available(self.branch-loc):
        cases(O.Option) maybe-ast(self.cases-loc):
          | some(cases-ast) =>
            branch = cases-ast.branches.find(lam(b): b.l.start-line == self.branch-loc.start-line end).value
            [ED.error:
              ed-intro("cases branch", self.branch-loc, -1),
              ED.cmcode(self.branch-loc),
              [ED.para:
                ED.text("expects that the "),
                ED.code(ED.highlight(ED.text(branch.name), [ED.locs: branch.pat-loc],0)),
                ED.text(" pattern has has exactly the same number of "),
                cases(Any) branch:
                  | s-cases-branch(_, _, _, args, _) =>
                      ED.highlight(ED.text("field bindings"),args.map(_.l),1)
                  | s-singleton-cases-branch(_, _, _, _) => ED.text("arguments")
                end,
                ED.text(" as the "),
                ED.code(ED.text(branch.name)),
                ED.text(" variant of "),
                ED.embed(cases-ast.typ.id),
                ED.text(" has fields.")],
              [ED.para:
                ED.text("The cases pattern for "),
                ED.code(ED.highlight(ED.text(branch.name), [ED.locs: branch.pat-loc],0)),
                ED.text(" has "),
                cases(Any) branch:
                  | s-cases-branch(_, _, _, args, _) =>
                      ED.highlight(ED.ed-field-bindings(self.num-args),args.map(_.l),1)
                  | s-singleton-cases-branch(_, _, _, _) => ED.ed-field-bindings(self.num-args)
                end,
                ED.text(".")],
              [ED.para:
                ED.text("The "),
                ED.code(ED.text(branch.name)),
                ED.text(" variant of the "),
                ED.embed(cases-ast.typ.id),
                ED.text(" datatype has "),
                ED.ed-fields(self.actual-arity)]]
          | none      =>
            [ED.error:
              ed-intro("cases branch", self.branch-loc, 0),
              ED.cmcode(self.branch-loc),
              [ED.para:
                ED.text("expects that the pattern has has exactly the same number of field bindings as the variant of has fields.")],
              [ED.para:
                ED.text("The cases pattern for has "),
                ED.ed-field-bindings(self.num-args),
                ED.text(".")],
              [ED.para:
                ED.text("The corresponding variant of the datatype has "),
                ED.ed-fields(self.actual-arity)]]
        end
      else:
        [ED.error:
          [ED.para:
            ED.text("A cases branch in "),
            ED.loc(self.branch-loc),
            ED.text(" expects that the pattern has has exactly the same number of field bindings as the corresponding variant has fields.")],
          [ED.para:
            ED.text("The cases pattern for has "),
            ED.ed-field-bindings(self.num-args),
            ED.text(".")],
          [ED.para:
            ED.text("The corresponding variant of the datatype has "),
            ED.ed-fields(self.actual-arity)]]
      end
    end,
    method render-reason(self):
      if self.branch-loc.is-builtin():
        [ED.error:
          [ED.para:
            ED.text("A cases branch in "),
            ED.loc(self.branch-loc),
            ED.text(" expects that the pattern has has exactly the same number of field bindings as the corresponding variant has fields.")],
          [ED.para:
            ED.text("The cases pattern for has "),
            ED.ed-field-bindings(self.num-args),
            ED.text(".")],
          [ED.para:
            ED.text("The corresponding variant of the datatype has "),
            ED.ed-fields(self.actual-arity)],
          please-report-bug()]
      else:
        [ED.error:
          [ED.para:
            ED.text("A cases branch in "),
            ED.loc(self.branch-loc),
            ED.text(" expects that the pattern has has exactly the same number of field bindings as the corresponding variant has fields.")],
          [ED.para:
            ED.text("The cases pattern for has "),
            ED.ed-field-bindings(self.num-args),
            ED.text(".")],
          [ED.para:
            ED.text("The corresponding variant of the datatype has "),
            ED.ed-fields(self.actual-arity)]]
      end
    end
  | constructor-arity-mismatch(fun-def-loc, constructor-name, fun-def-arity, fun-app-args) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      fun-app-arity = self.fun-app-args.length()
      helper =
        lam(rest):
          [ED.error: 
            cases(O.Option) maybe-stack-loc(0, true):
              | some(fun-app-loc) =>
                if fun-app-loc.is-builtin():
                  [ED.sequence:
                    [ED.para:
                      ED.text("The function application in "),
                      ED.loc(fun-app-loc),
                      ED.text(" expected the applicant to evaluate to a function that accepts exactly the same number of arguments as are given to it.")],
                    [ED.para:
                      ED.text("The applicant had "),
                      ED.ed-args(fun-app-arity),
                      ED.text(" applied to it.")],
                    rest(ED.text("applicant"))]
                else if src-available(fun-app-loc):
                  cases(O.Option) maybe-ast(fun-app-loc):
                    | some(ast) =>
                      applicant = ED.highlight(ED.text("applicant"), [ED.locs: ast._fun.l], 0)
                      [ED.sequence:
                        ed-intro("function application expression", fun-app-loc, -1),
                        ED.cmcode(fun-app-loc),
                        [ED.para:
                          ED.text("expected the "),
                          applicant,
                          ED.text(" to evaluate to a function that accepts exactly the same number of arguments as are given to it.")],
                        [ED.para:
                          ED.text("The "),
                          applicant,
                          ED.text(" had "),
                          ED.highlight(ED.ed-args(fun-app-arity), ast.args.map(_.l),1),
                          ED.text(" applied to it.")],
                        rest(applicant)]
                    | none      =>
                      [ED.sequence:
                        ed-intro("function application expression", fun-app-loc, -1),
                        ED.cmcode(fun-app-loc),
                        [ED.para:
                          ED.text("expected the applicant to evaluate to a function that accepts exactly the same number of arguments as are given to it.")],
                        [ED.para:
                          ED.text("The applicant had "),
                          ED.ed-args(fun-app-arity),
                          ED.text(" applied to it.")],
                        rest(ED.text("applicant"))]
                  end
                else:
                  [ED.sequence:
                    [ED.para:
                      ED.text("The function application in "),
                      ED.loc(fun-app-loc),
                      ED.text(" expected the applicant to evaluate to a function that accepts exactly the same number of arguments as are given to it.")],
                    [ED.para:
                      ED.text("The applicant had "),
                      ED.ed-args(fun-app-arity),
                      ED.text(" applied to it.")],
                      rest(ED.text("applicant"))]
                end
            | none =>
              [ED.sequence:
                [ED.para:
                  ED.text("A function application expects its applicant to evaluate to a function that accepts exactly the same number of arguments as are given to it.")],
                [ED.para:
                  ED.text("The applicant had "),
                  ED.ed-args(fun-app-arity),
                  ED.text(" applied to it.")],
                  rest(ED.text("applicant"))]
            end]
          end
        
        if src-available(self.fun-def-loc):
          cases(O.Option) maybe-ast(self.fun-def-loc):
            | some(ast) =>
              helper(lam(applicant):
                [ED.sequence:
                  [ED.para:
                    ED.text("The "),
                    applicant,
                    ED.text(" evaluated to the "),
                    ED.code(ED.text(self.constructor-name)),
                    ED.text(" constructor, which accepts "),
                    ED.highlight(ED.ed-args(self.fun-def-arity), ast.args.map(_.l), 2),
                    ED.text(":")],
                  ED.cmcode(self.fun-def-loc)]
              end)
            | none      =>
              helper(lam(applicant):
                [ED.sequence:
                  [ED.para:
                    ED.text("The "),
                    applicant,
                    ED.text(" evaluated to the "),
                    ED.code(ED.text(self.constructor-name)),
                    ED.text(" constructor, which accepts "),
                    ED.ed-args(self.fun-def-arity),
                    ED.text(":")],
                  ED.cmcode(self.fun-def-loc)]
              end)
          end
        else:
          helper(lam(applicant):
            [ED.para:
              ED.text("The "),
              applicant,
              ED.text(" evaluated to the "),
              ED.code(ED.text(self.constructor-name)),
              ED.text(" constructor, defined in "),
              ED.loc(self.fun-def-loc),
              ED.text(", which accepts "),
              ED.ed-args(self.fun-def-arity),
              ED.text(".")]
          end)
        end
    end,
    method render-reason(self):
      num-args = self.fun-app-args.length()
      this-str = if num-args == 1: "this " else: "these " end
      arg-str = if num-args == 1: " argument:" else: " arguments:" end
      exp-arg-str = if self.fun-def-arity == 1: " argument" else: " arguments" end
      
      ED.maybe-stack-loc(0, true,
        lam(caller-loc):
          if self.fun-def-loc.is-builtin():
            [ED.error:
              [ED.para: ED.text("Expected to get "), ED.embed(self.fun-def-arity), ED.text(exp-arg-str + " at")],
              draw-and-highlight(caller-loc),
              [ED.para: ED.text(" but got " + this-str), ED.embed(num-args), ED.text(arg-str)],
              vert-list-values(self.fun-app-args)]
          else:
            [ED.error:
              [ED.para: ED.text("Expected to get "), ED.embed(self.fun-def-arity),
                ED.text(exp-arg-str + " when calling the function at ")],
              draw-and-highlight(self.fun-def-loc),
              [ED.para: ED.text("from")],
              draw-and-highlight(caller-loc),
              [ED.para: ED.text(" but got " + this-str), ED.embed(num-args), ED.text(arg-str)],
              vert-list-values(self.fun-app-args)]
          end
        end,
        [ED.error:
          [ED.para: ED.text("Expected to get "), ED.embed(self.fun-def-arity), ED.text(exp-arg-str + " at ")],
          draw-and-highlight(self.fun-def-loc),
          [ED.para: ED.text(" but got " + this-str), ED.embed(num-args), ED.text(arg-str)],
          vert-list-values(self.fun-app-args)])
    end
  | arity-mismatch(fun-def-loc, fun-def-arity, fun-app-args) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast) block:
      fun-app-arity = self.fun-app-args.length()
      helper =
        lam(rest):
          [ED.error: 
            cases(O.Option) maybe-stack-loc(
              if self.fun-def-loc.is-builtin(): 
                0 
              else: 
                1 
              end, true):
              | some(fun-app-loc) =>
                if fun-app-loc.is-builtin():
                  [ED.sequence:
                    [ED.para:
                      ED.text("The function application in "),
                      ED.loc(fun-app-loc),
                      ED.text(" expected the applicant to evaluate to a function that accepts exactly the same number of arguments as are given to it.")],
                    [ED.para:
                      ED.text("The applicant had "),
                      ED.ed-args(fun-app-arity),
                      ED.text(" applied to it.")],
                    rest(ED.text("applicant"))]
                else if src-available(fun-app-loc):
                  cases(O.Option) maybe-ast(fun-app-loc):
                    | some(ast) =>
                      applicant = ED.highlight(ED.text("applicant"), [ED.locs: ast._fun.l], 0)
                      [ED.sequence:
                        ed-intro("function application expression", fun-app-loc, -1),
                        ED.cmcode(fun-app-loc),
                        [ED.para:
                          ED.text("expected the "),
                          applicant,
                          ED.text(" to evaluate to a function that accepts exactly the same number of arguments as are given to it.")],
                        [ED.para:
                          ED.text("The "),
                          applicant,
                          ED.text(" had "),
                          ED.highlight(ED.ed-args(fun-app-arity), ast.args.map(_.l),1),
                          ED.text(" applied to it.")],
                        rest(applicant)]
                    | none      =>
                      [ED.sequence:
                        ed-intro("function application expression", fun-app-loc, -1),
                        ED.cmcode(fun-app-loc),
                        [ED.para:
                          ED.text("expected the applicant to evaluate to a function that accepts exactly the same number of arguments as are given to it.")],
                        [ED.para:
                          ED.text("The applicant had "),
                          ED.ed-args(fun-app-arity),
                          ED.text(" applied to it.")],
                        rest(ED.text("applicant"))]
                  end
                else:
                  [ED.sequence:
                    [ED.para:
                      ED.text("The function application in "),
                      ED.loc(fun-app-loc),
                      ED.text(" expected the applicant to evaluate to a function that accepts exactly the same number of arguments as are given to it.")],
                    [ED.para:
                      ED.text("The applicant had "),
                      ED.ed-args(fun-app-arity),
                      ED.text(" applied to it.")],
                      rest(ED.text("applicant"))]
                end
            | none =>
              [ED.sequence:
                [ED.para:
                  ED.text("A function application expects its applicant to evaluate to a function that accepts exactly the same number of arguments as are given to it.")],
                [ED.para:
                  ED.text("The applicant had "),
                  ED.ed-args(fun-app-arity),
                  ED.text(" applied to it.")],
                  rest(ED.text("applicant"))]
            end]
          end
        
        if src-available(self.fun-def-loc):
          fun is-underscore(arg):
            cases(Any) arg:
              | s-id(_, id) => 
                cases(Any) id:
                  | s-underscore(_) => true
                  | else            => false
                end
              | else                => false
            end
          end
          cases(O.Option) maybe-ast(self.fun-def-loc):
            | some(ast) =>
              {args; fun-def-snippet-loc} = cases(Any) ast:
                | s-op(_,_,_,l,r) =>
                  l-underscore = is-underscore(l)
                  r-underscore = is-underscore(r)
                  {raw-array-to-list(
                    if l-underscore and r-underscore:
                      [raw-array: l.id.l, r.id.l]
                    else if l-underscore:
                      [raw-array: l.id.l]
                    else if r-underscore:
                      [raw-array: r.id.l]
                    else:
                      [raw-array:]
                    end); self.fun-def-loc}
                | s-app(_,_,args) => {args.filter(is-underscore).map(_.l); self.fun-def-loc}
                | s-fun(l, _, _, args, _, _, b, _, _) => {args.map(_.l); l.upto(b.l)}
                | s-dot(_, obj, _)      => {raw-array-to-list([raw-array: obj.id.l]); self.fun-def-loc}
                | s-extend(_, obj, _)   => {raw-array-to-list([raw-array: obj.id.l]); self.fun-def-loc}
                | s-update(_, obj, _)   => {raw-array-to-list([raw-array: obj.id.l]); self.fun-def-loc}
                | s-get-bang(_, obj, _) => {raw-array-to-list([raw-array: obj.id.l]); self.fun-def-loc}
              end
              helper(lam(applicant):
                [ED.sequence:
                  [ED.para:
                    ED.text("The "),
                    applicant,
                    ED.text(" evaluated to a function defined accepting "),
                    ED.highlight(ED.ed-args(self.fun-def-arity), args, 2),
                    ED.text(":")],
                  ED.cmcode(fun-def-snippet-loc)]
              end)
            | none      =>
              helper(lam(applicant):
                [ED.sequence:
                  [ED.para:
                    ED.text("The "),
                    applicant,
                    ED.text(" evaluated to a function defined accepting "),
                    ED.ed-args(self.fun-def-arity),
                    ED.text(":")],
                  ED.cmcode(self.fun-def-loc)]
              end)
          end
        else:
          helper(lam(applicant):
            [ED.para:
              ED.text("The "),
              applicant,
              ED.text(" evaluated to a function defined in "),
              ED.loc(self.fun-def-loc),
              ED.text(" accepting "),
              ED.ed-args(self.fun-def-arity),
              ED.text(".")]
          end)
        end
    end,
    method render-reason(self):
      num-args = self.fun-app-args.length()
      this-str = if num-args == 1: "this " else: "these " end
      arg-str = if num-args == 1: " argument:" else: " arguments:" end
      exp-arg-str = if self.fun-def-arity == 1: " argument" else: " arguments" end
      
      ED.maybe-stack-loc(0, true,
        lam(caller-loc):
          if self.fun-def-loc.is-builtin():
            [ED.error:
              [ED.para: ED.text("Expected to get "), ED.embed(self.fun-def-arity), ED.text(exp-arg-str + " at")],
              draw-and-highlight(caller-loc),
              [ED.para: ED.text(" but got " + this-str), ED.embed(num-args), ED.text(arg-str)],
              vert-list-values(self.fun-app-args)]
          else:
            [ED.error:
              [ED.para: ED.text("Expected to get "), ED.embed(self.fun-def-arity),
                ED.text(exp-arg-str + " when calling the function at ")],
              draw-and-highlight(self.fun-def-loc),
              [ED.para: ED.text("from")],
              draw-and-highlight(caller-loc),
              [ED.para: ED.text(" but got " + this-str), ED.embed(num-args), ED.text(arg-str)],
              vert-list-values(self.fun-app-args)]
          end
        end,
        [ED.error:
          [ED.para: ED.text("Expected to get "), ED.embed(self.fun-def-arity), ED.text(exp-arg-str + " at ")],
          draw-and-highlight(self.fun-def-loc),
          [ED.para: ED.text(" but got " + this-str), ED.embed(num-args), ED.text(arg-str)],
          vert-list-values(self.fun-app-args)])
    end
  | non-function-app(loc, non-fun-val) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      if self.loc.is-builtin():
        [ED.error:
          [ED.para:
            ED.text("A function application expression in "),
            ED.loc(self.loc),
            ED.text(" failed because its applicant evaluated to the non-function value:")],
          ED.embed(self.non-fun-val)]
      else if src-available(self.loc):
        cases(O.Option) maybe-ast(self.loc):
          | some(ast) =>
            [ED.error:
              ed-intro("function application expression", self.loc),
              ED.cmcode(self.loc),
              [ED.para:
                ED.text("because the "),
                ED.highlight(ED.text("applicant"), [ED.locs: ast._fun.l], 0),
                ED.text(" evaluated to the non-function value:")],
                ED.embed(self.non-fun-val)]
          | none      =>
            [ED.error:
              ed-intro("function application expression", self.loc),
              ED.cmcode(self.loc),
              [ED.para:
                ED.text("because the applicant evaluated to the non-function value:")],
                ED.embed(self.non-fun-val)]
        end
      else:
        [ED.error:
          [ED.para:
            ED.text("The function application expression in "),
            ED.loc(self.loc),
            ED.text(" failed because its applicant evaluated to the non-function value:")],
          ED.embed(self.non-fun-val)]
      end
    end,
    method render-reason(self):
      if self.loc.is-builtin():
        [ED.error:
          [ED.para:
            ED.text("A function application expression in "),
            ED.loc(self.loc),
            ED.text(" failed because its applicant evaluated to the non-function value:")],
          ED.embed(self.non-fun-val)]
      else:
        [ED.error:
          [ED.para:
            ED.text("The function application expression in "),
            ED.loc(self.loc),
            ED.text(" failed because its applicant evaluated to the non-function value:")],
          ED.embed(self.non-fun-val)]
      end
    end
  | uninitialized-id(loc, name) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      if self.loc.is-builtin():
        [ED.error:
          [ED.para:
            ED.text("The identifier "), 
            ED.code(ED.text(self.name)),
            ED.text(" is unbound in "),
            ED.loc(self.loc)],
            please-report-bug()]
      else if src-available(self.loc):
        [ED.error:
          [ED.para:
            ED.text("The "),
            ED.highlight(ED.text("identifier"), [ED.locs: self.loc], 0)],
           ED.cmcode(self.loc),
          [ED.para:
            ED.text(" is unbound. Although it has been previously defined, it is being "),
            ED.highlight(ED.text("used"), [ED.locs: self.loc], 0),
            ED.text(" before it has been is initialized to a value.")]]
      else:
        [ED.error:
          [ED.para:
            ED.text("The identifier "), 
            ED.code(ED.text(self.name)),
            ED.text(" in "),
            ED.loc(self.loc),
            ED.text(" is unbound. Although it has been previously defined, it is being used before it has been is initialized to a value.")]]
      end
    end,
    method render-reason(self):
      if self.loc.is-builtin():
        [ED.error:
          [ED.para:
            ED.text("The identifier "), 
            ED.code(ED.text(self.name)),
            ED.text(" is unbound in "),
            ED.loc(self.loc)],
            please-report-bug()]
      else:
        [ED.error:
          [ED.para:
            ED.text("The identifier "), 
            ED.code(ED.text(self.name)),
            ED.text(" in "),
            ED.loc(self.loc),
            ED.text(" is unbound. Although it has been previously defined, it is being used before it has been is initialized to a value.")]]
      end
    end
  | module-load-failure(names) with: # names is List<String>
    method render-fancy-reason(self, loc-to-ast, loc-to-src):
      self.render-reason()
    end,
    method render-reason(self):
      [ED.error:
        [ED.para:
          if self.names.length() == 1: ED.text("The following module failed to load:")
          else:                        ED.text("The following modules failed to load:")
          end],
        ED.h-sequence(self.names.map(ED.text), ", ")]
    end
  | invalid-array-index(method-name :: String, array, index :: Number, reason :: String) with: # array is Array
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      [ED.error:
        ED.maybe-stack-loc(0, true,
          lam(loc):
            if loc.is-builtin():
              [ED.sequence:
                [ED.para: 
                  ED.text("An array interaction, "),
                  ED.code(ED.text(self.method-name)),
                  ED.text(", in "),
                  ED.loc(loc),
                  ED.text(" expects that the index passed to it is an integer within the bounds of the array. ")],
                [ED.para:
                  ED.embed(self.index),
                  ED.text(" is an invalid array index because "),
                  ED.text(self.reason)],
                please-report-bug()]
            else if src-available(loc):
              [ED.sequence:
                ed-intro(self.method-name, loc, 0),
                ED.cmcode(loc),
                [ED.para:
                  ED.text("expects that the index passed to it is an integer within the bounds of the array. "),
                  ED.embed(self.index),
                  ED.text(" is an invalid array index because "),
                  ED.text(self.reason)]]
            else:
              [ED.sequence:
                [ED.para: 
                  ED.text("An array interaction, "),
                  ED.code(ED.text(self.method-name)),
                  ED.text(", in "),
                  ED.loc(loc),
                  ED.text(" expects that the index passed to it is an integer within the bounds of the array. ")],
                [ED.para:
                  ED.embed(self.index),
                  ED.text(" is an invalid array index because "),
                  ED.text(self.reason)]]
            end
          end,
          [ED.sequence:
            [ED.para: 
              ED.text("An array interaction, "),
              ED.code(ED.text(self.method-name)),
              ED.text(" expects that the index passed to it is an integer within the bounds of the array. ")],
            [ED.para:
              ED.embed(self.index),
              ED.text(" is an invalid array index because "),
              ED.text(self.reason)]])]
    end,
    method render-reason(self):
      ED.maybe-stack-loc(0, true,
        lam(loc):
          if loc.is-builtin():
            [ED.error:
              [ED.para: 
                ED.text("An array interaction, "),
                ED.code(ED.text(self.method-name)),
                ED.text(", in "),
                ED.loc(loc),
                ED.text(" expects that the index passed to it is an integer within the bounds of the array. ")],
              [ED.para:
                ED.embed(self.index),
                ED.text(" is an invalid array index because "),
                ED.text(self.reason)],
              please-report-bug()]
          else:
            [ED.error:
              [ED.para: 
                ED.text("An array interaction, "),
                ED.code(ED.text(self.method-name)),
                ED.text(", in "),
                ED.loc(loc),
                ED.text(" expects that the index passed to it is an integer within the bounds of the array. ")],
              [ED.para:
                ED.embed(self.index),
                ED.text(" is an invalid array index because "),
                ED.text(self.reason)]]
          end
        end,
        [ED.error:
          [ED.para: 
            ED.text("An array interaction, "),
            ED.code(ED.text(self.method-name)),
            ED.text(" expects that the index passed to it is an integer within the bounds of the array. ")],
          [ED.para:
            ED.embed(self.index),
            ED.text(" is an invalid array index because "),
            ED.text(self.reason)]])
    end
  | equality-failure(reason :: String, value1, value2) with:
    method render-fancy-reason(self, _, _):
      self.render-reason() # TODO
    end,
    method render-reason(self):
      value1 = self.value1
      value2 = self.value2
      ask:
        | is-number(value1) and is-number(value2) then:
          # one (or both) of them must be a roughnum
          fun within-error(message):
            [ED.error:
              [ED.para: ED.text(message)],
              [ED.para: ED.embed(value1)],
              [ED.para: ED.embed(value2)],
              [ED.para: ED.text("Consider using the "),
              ED.code(ED.text("within")), ED.text(" function to compare them instead.")]]

          end
          if num-is-roughnum(value1) and num-is-roughnum(value2):
            within-error("Attempted to compare two Roughnums for equality, which is not allowed:")
          else if num-is-roughnum(value1):
            within-error("Attempted to compare a Roughnum to an Exactnum for equality, which is not allowed:")
          else if num-is-roughnum(value2):
            within-error("Attempted to compare an Exactnum to a Roughnum for equality, which is not allowed:")
          end
        | otherwise:
          [ED.error:
            [ED.para: ED.text("Attempted to compare two incomparable values: ")],
            [ED.para: ED.embed(self.value1)],
            [ED.para: ED.embed(self.value2)]]
      end
    end

  | user-break with:
    method render-fancy-reason(self, _, _):
      self.render-reason()
    end,
    method render-reason(self):
      [ED.error: ED.text("Program stopped by user")]
    end

  | user-exception(value :: Any) with:
    method render-fancy-reason(self, _, _):
      self.render-reason()
    end,
    method render-reason(self): [ED.error: [ED.para: ED.embed(self.value)]] end
end

data ParseError:
  | parse-error-next-token(loc, next-token :: String) with:
    method render-fancy-reason(self, loc-to-src):
      [ED.error:
        [ED.para: ED.text("Pyret didn't understand your program around ")],
        ED.code(ED.highlight(ED.text(loc-to-src(self.loc)),[ED.locs: self.loc], 0)),
        [ED.para: ED.text(" You may need to add or remove some text to fix your program. "),
          ED.text("Look carefully before the "),ED.highlight(ED.text("highlighted text"),[ED.locs: self.loc],0),
          ED.text(". Is there a missing colon ("), ED.code(ED.text(":")),
          ED.text("), comma ("), ED.code(ED.text(",")),
          ED.text("), string marker ("), ED.code(ED.text("\"")),
          ED.text("), or keyword? Is there something there that shouldn’t be?")]
      ]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para: ED.text("Pyret didn't understand your program around "), draw-and-highlight(self.loc)],
        [ED.para: ED.text("You may need to add or remove some text to fix your program.")],
        [ED.para:
          ED.text("Look carefully before the "),
          ED.styled(ED.text("highlighted text"), 'error-highlight'),
          ED.text(".")],
        [ED.para: ED.text("Is there a missing colon ("), ED.code(ED.text(":")),
          ED.text("), comma ("), ED.code(ED.text(",")),
          ED.text("), string marker ("), ED.code(ED.text("\"")),
          ED.text("), or keyword?")],
        [ED.para: ED.text("Is there something there that shouldn’t be?")]
      ]
    end
  | parse-error-eof(loc) with:
    method render-fancy-reason(self, loc-to-src):
      [ED.error: 
        [ED.para:
          ED.text("Pyret didn't expect your program to "),
          ED.code(ED.highlight(ED.text("end"),[ED.locs: self.loc],0)),
          ED.text(" as soon as it did. You may be missing an \"end\", or closing punctuation like \")\" or \"]\" somewhere in your program.")]]
    end,
    method render-reason(self):
      [ED.error: [ED.para:
          ED.text("Pyret didn't understand the very end of your program."),
          ED.text("You may be missing an \"end\", or closing punctuation like \")\" or \"]\" right at the end.")]]
    end
  | parse-error-unterminated-string(loc) with:
    method render-fancy-reason(self, loc-to-src):
      [ED.error: 
        [ED.para:
          ED.text("Pyret thinks the string ")],
         ED.code(ED.highlight(ED.text(loc-to-src(self.loc)),[ED.locs: self.loc],0)),
        [ED.para:
          ED.text("is unterminated; you may be missing closing punctuation. If you intended to write a multi-line string, use "),
          ED.code(ED.text("```")),
          ED.text(" instead of quotation marks.")]]
    end,
    method render-reason(self):
      [ED.error: [ED.para-nospace:
          ED.text("Pyret thinks your program has an incomplete string literal around "),
          draw-and-highlight(self.loc),
          ED.text("; you may be missing closing punctuation.")]]
    end
  | parse-error-bad-operator(loc) with:
    method render-fancy-reason(self, loc-to-src):
      [ED.error: 
        [ED.para:
          ED.text("The operator "),
          ED.code(ED.highlight(ED.text(loc-to-src(self.loc)),[ED.locs: self.loc],0)),
          ED.text(" must have whitespace separating it from its operands.")]]
    end,
    method render-reason(self):
      [ED.error: [ED.para-nospace:
          ED.text("The operator at "),
          draw-and-highlight(self.loc),
          ED.text(" has no surrounding whitespace.")]]
    end
  | parse-error-bad-number(loc) with:
    method render-fancy-reason(self, loc-to-src):
      [ED.error: 
        [ED.para:
          ED.text("Pyret thinks "),
          ED.code(ED.highlight(ED.text(loc-to-src(self.loc)),[ED.locs: self.loc],0)),
          ED.text(" is probably a number, but number literals in Pyret require at least one digit before the decimal point.")]]
    end,
    method render-reason(self):
      [ED.error: [ED.para-nospace:
          ED.text("Pyret thinks your program probably has a number at "),
          draw-and-highlight(self.loc),
          ED.text("; number literals in Pyret require at least one digit before the decimal point.")]]
    end
  | empty-block(loc) with:
    method _tostring(self, shadow tostring):
      "Empty block at " + self.loc.format(true)
    end
  | bad-block-stmt(loc) with:
    method _tostring(self, shadow tostring):
      "Expected a val binding or an expression, but got something else " + self.loc.format(true)
    end
  | bad-check-block-stmt(loc) with:
    method _tostring(self, shadow tostring):
      "Expected a val binding or an expression, but got something else " + self.loc.format(true)
    end
  | fun-missing-colon(loc) with:
    method _tostring(self, shadow tostring): "fun-missing-colon: " + self.loc.format(true) end
  | fun-missing-end(loc) with:
    method _tostring(self, shadow tostring): "fun-missing-end: " + self.loc.format(true) end
  | args-missing-comma(loc) with:
    method _tostring(self, shadow tostring): "args-missing-comma: " + self.loc.format(true) end
  | app-args-missing-comma(loc) with:
    method _tostring(self, shadow tostring): "app-args-missing-comma: " + self.loc.format(true) end
  | missing-end(loc)
  | missing-comma(loc)
sharing:
  method render-reason(self):
    ED.text(self._tostring(tostring))
  end
end
