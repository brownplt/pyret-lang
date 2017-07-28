provide *
provide-types *
# import arrays as A
# import lists as L

import global as _
import option as O
import srcloc as S
import error-display as ED
import valueskeleton as VS

fun draw-and-highlight(l):
  ED.loc-display(l, "error-highlight", ED.loc(l))
end
fun vert-list-values(vals):
  ED.v-sequence(vals.map(lam(val): [ED.para: ED.embed(val)] end))
end

fun ed-simple-intro(name, loc):
  if loc.is-builtin():
    [ED.para:
      ED.text("Evaluating a " + name + " in "),
      ED.loc(loc),
      ED.text(" errored.")]
  else:
    [ED.para:
      ED.text("Evaluating the " + name + " at "),
      ED.loc(loc),
      ED.text(" errored.")]
  end
end

fun ed-intro(name, loc, color, followed-by-embed):
  [ED.para:
    ED.text("Evaluating this "),
    ED.highlight(ED.text(name), [ED.locs: loc], color),
    ED.text(
      if followed-by-embed:
        " errored:"
      else:
        " errored."
      end)]
end

fun please-report-bug():
  [ED.para: ED.text("Please report this as a bug.")]
end

data RuntimeError:
  | multi-error(errors #|:: List<RuntimeError>|#) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      # The concatenation of renderings is _not_ a valid rendering;
      # this is a ticking time-bomb of vexing highlighting bugs.
      rendered = self.errors.map(_.render-fancy-reason(maybe-stack-loc, src-available, maybe-ast))
      ED.v-sequence(rendered)
    end,
    method render-reason(self):
      rendered = self.errors.map(_.render-reason())
      ED.v-sequence(rendered)
    end
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
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      self.render-reason()
    end,
    method render-reason(self):
      [ED.error: [ED.para: ED.text(self.message)]]
    end,
    method _output(self):
      VS.vs-value(self.message)
    end
  | update-non-obj(loc, obj, objloc) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      if self.loc.is-builtin():
        [ED.error:
          ed-simple-intro("reference update expression", self.loc),
          [ED.para: ED.text("It was given a non-object value:")],
          ED.embed(self.obj)]
      else:
        [ED.error:
          ed-intro("reference update expression", self.loc, -1, true),
          ED.cmcode(self.loc),
          [ED.para:
            ED.text("The "),
            ED.highlight(ED.text("left side"), [ED.locs: self.objloc], 0),
            ED.text(" is expected to evaluate to an object, but its value was: ")],
          ED.embed(self.obj)]
      end
    end,
    method render-reason(self):
      [ED.error:
        ed-simple-intro("reference update expression", self.loc),
        [ED.para: ED.text("It was given a non-object value:")],
        ED.embed(self.obj)]
    end
  | update-frozen-ref(loc, obj, objloc, field, fieldloc) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      if not(self.loc.is-builtin()) and src-available(self.loc):
        [ED.error:
          ed-intro("reference update expression", self.loc, -1, true),
          ED.cmcode(self.loc),
          [ED.para:
            ED.text("The "),
            ED.highlight(ED.text("field"), [ED.locs: self.fieldloc], 0),
            ED.text(" is frozen in the "),
            ED.highlight(ED.text("object:"), [ED.locs: self.objloc], 1)],
          ED.embed(self.obj)]
      else:
        [ED.error:
          ed-simple-intro("reference update expression", self.loc),
          [ED.para:
            ED.text("The field "),
            ED.code(ED.text(self.field)),
            ED.text(" is frozen in the object:")],
          ED.embed(self.obj)]
      end
    end,
    method render-reason(self):
      [ED.error:
        ed-simple-intro("reference update expression", self.loc),
        [ED.para:
          ED.text("The field "),
          ED.code(ED.text(self.field)),
          ED.text(" is frozen in the object:")],
        ED.embed(self.obj)]
    end
  | update-non-ref(loc, obj, objloc, field, fieldloc) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      if not(self.loc.is-builtin()) and src-available(self.loc):
        [ED.error:
          ed-intro("reference update expression", self.loc, -1, true),
          ED.cmcode(self.loc),
          [ED.para:
            ED.text("This "),
            ED.highlight(ED.text("field"), [ED.locs: self.fieldloc], 0),
            ED.text(" is not a mutable reference in the "),
            ED.highlight(ED.text("object:"), [ED.locs: self.objloc], 1)],
          ED.embed(self.obj)]
      else:
        [ED.error:
          ed-simple-intro("reference update expression", self.loc),
          [ED.para:
            ED.text("The field "),
            ED.code(ED.text(self.field)),
            ED.text(" is not a mutable reference in the object:")],
          ED.embed(self.obj)]
      end
    end,
    method render-reason(self):
      [ED.error:
        ed-simple-intro("reference update expression", self.loc),
        [ED.para:
          ED.text("The field "),
          ED.code(ED.text(self.field)),
          ED.text(" is not a mutable reference in the object:")],
        ED.embed(self.obj)]
    end
  | update-non-existent-field(loc, obj, objloc, field, fieldloc) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      if self.loc.is-builtin():        
        [ED.error:
          ed-simple-intro("reference update expression", self.loc),
          [ED.para:
            ED.text("The field "),
            ED.code(ED.text(self.field)),
            ED.text(" does not exist in the object:")],
          ED.embed(self.obj)]
      else:
        [ED.error:
          ed-intro("reference update expression", self.loc, -1, true),
          ED.cmcode(self.loc),
          [ED.para:
            ED.text("The "),
            ED.highlight(ED.text("field"), [ED.locs: self.fieldloc], 0),
            ED.text(" does not exist in the "),
            ED.highlight(ED.text("object:"), [ED.locs: self.objloc], 1)],
          ED.embed(self.obj)]
      end
    end,
    method render-reason(self):
      [ED.error:
        ed-simple-intro("reference update expression", self.loc),
        [ED.para:
          ED.text("The field "),
          ED.code(ED.text(self.field)),
          ED.text(" does not exist in the object:")],
        ED.embed(self.obj)]
    end
  | no-cases-matched(loc, val) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      if self.loc.is-builtin() or not(src-available(self.loc)):
        [ED.error:
          ed-simple-intro("cases expression", self.loc),
          [ED.para: ED.text("No branch matched the value:")],
          ED.embed(self.val)]
      else if src-available(self.loc):
        cases(O.Option) maybe-ast(self.loc):
          | some(ast-cse) =>
            [ED.error:
              ed-intro("cases expression", self.loc, -1, true),
              ED.cmcode(self.loc),
              [ED.para:
                ED.text("None of the "),
                ED.highlight(ED.text("branch patterns"), ast-cse.branches.map(_.pat-loc), 0),
                ED.text(" matched the value of the "),
                ED.highlight(ED.text("argument"),[ED.locs: ast-cse.val.l],1),
                ED.text(":")],
              ED.embed(self.val)]
          | none =>
            [ED.error:
              ed-intro("cases expression", self.loc, 0, true),
              ED.cmcode(self.loc),
              [ED.para: ED.text("No branch matched the value of the argument:")],
              ED.embed(self.val)]
        end
      end
    end,
    method render-reason(self):
      [ED.error:
        ed-simple-intro("cases expression", self.loc),
        [ED.para: ED.text("No branch matched the value:")],
        ED.embed(self.val)]
    end
  | no-branches-matched(loc, expression :: String) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      if not(self.loc.is-builtin()) and src-available(self.loc):
        [ED.error:
          ed-intro(self.expression + " expression", self.loc, 0, true),
          ED.cmcode(self.loc),
          [ED.para:
            ED.text("It expected that the condition of at least one branch be satisfied. No branch conditions were satisfied, so no branch could be entered.")]]
      else:
        [ED.error:
          ed-simple-intro(self.expression + " expression", self.loc),
          [ED.para:
            ED.text("It expected that the condition of at least one branch be satisfied. No branch conditions were satisfied, so no branch could be entered.")]]
      end
    end,
    method render-reason(self):
      [ED.error:
        ed-simple-intro(self.expression + " expression", self.loc),
        [ED.para:
          ED.text("It expected that the condition of at least one branch be satisfied. No branch conditions were satisfied, so no branch could be entered.")]]
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
  | spinnaker-error(funloc, step-num) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      self.render-reason()
    end,
    method render-reason(self):
      [ED.error:
        [ED.para: ED.text("Internal compiler error: No case numbered " + tostring(self.step-num) + " in")],
        ED.loc(self.fun-loc),
        please-report-bug()]
    end
  | template-not-finished(loc) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      if self.loc.is-builtin() or not(src-available(self.loc)):
        [ED.error:
          ed-simple-intro("unfinished template expression", self.loc),
          [ED.para: ED.text("Template expressions cannot be evaluated.")]]
      else:
        [ED.error:
          ed-intro("unfinished template expression", self.loc, 1, true),
          ED.cmcode(self.loc),
          [ED.para: ED.text("Template expressions cannot be evaluated.")]]
      end
    end,
    method render-reason(self):
      [ED.error:
        ed-simple-intro("unfinished template expression", self.loc),
        [ED.para: ED.text("Template expressions cannot be evaluated.")]]
    end
  | field-not-found(loc, obj, field :: String) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      if self.loc.is-builtin() or not(src-available(self.loc)):
        [ED.error:
          ed-simple-intro("field lookup expression", self.loc),
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
              ed-intro("field lookup expression", self.loc, -1, true),
              ED.cmcode(self.loc),
              [ED.para:
                ED.text("The "),
                ED.highlight(ED.text("left side"),[ED.locs: ast.obj.l], 0),
                ED.text(" was an object that did not have a "),
                ED.highlight([ED.sequence: ED.text("field named "),
                    ED.code(ED.text(self.field))],
                  [ED.locs: ast.field-loc()], 1),
                ED.text(":")],
              ED.embed(self.obj)]
          | none =>
            [ED.error:
              ed-intro("field lookup expression", self.loc, -1, true),
              ED.cmcode(self.loc),
              [ED.para:
                ED.text("The left side was an object that did not have a field named "),
                ED.code(ED.text(self.field)),
                ED.text(":")],
              ED.embed(self.obj)]
        end
      else:
        [ED.error:
          ed-simple-intro("field lookup expression", self.loc),
          [ED.para:
            ED.text("The left side was an object that did not have a field named "),
            ED.code(ED.text(self.field)),
            ED.text(":")],
          ED.embed(self.obj)]
      end
    end,
    method render-reason(self):
      [ED.error:
        ed-simple-intro("field lookup expression", self.loc),
        [ED.para:
          ED.text("The left side was an object that did not have a field named "),
          ED.code(ED.text(self.field)),
          ED.text(":")],
        ED.embed(self.obj)]
    end
  | constructor-syntax-non-constructor(expr-loc, constr-loc) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      if self.expr-loc.is-builtin() or not(src-available(self.expr-loc)):
        [ED.error:
          ed-simple-intro("construction expression", self.expr-loc),
          ED.cmcode(self.expr-loc),
          [ED.para:
            ED.text("The left side was not a construction maker.")]]
      else:
        [ED.error:
          ed-intro("construction expression", self.expr-loc, -1, true),
          ED.cmcode(self.expr-loc),
          [ED.para:
            ED.text("The "),
            ED.highlight(ED.text("left side"), [ED.locs: self.constr-loc], 0),
            ED.text(" was not a construction maker.")]]
      end
    end,
    method render-reason(self):
      [ED.error:
        ed-simple-intro("construction expression", self.expr-loc),
        [ED.para:
          ED.text("The left side was not a construction maker.")]]
    end
  | lookup-constructor-not-object(loc, constr-name :: String, field :: String) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      if self.loc.is-builtin() or not(src-available(self.loc)):
        [ED.error:
          ed-simple-intro("field lookup expression", self.loc),
          [ED.para:
            ED.text("The left side was a constructor, not an object.")]]
      else:
        [ED.error:
          ed-intro("field lookup expression", self.loc, 0, true),
          ED.cmcode(self.loc),
          [ED.para: ED.text("The left side was a constructor, not an object.")]]
      end
    end,
    method render-reason(self):
      [ED.error:
        ed-simple-intro("field lookup expression", self.loc),
        [ED.para:
          ED.text("The left side was a constructor, not an object.")]]
    end
  | lookup-non-tuple(loc, non-tup, index :: Number) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      if self.loc.is-builtin():
        [ED.error:
          ed-simple-intro("tuple lookup expression", self.loc),
          [ED.para: ED.text("The left side was not a tuple value:")],
          ED.embed(self.non-tup)]
      else if src-available(self.loc):
        cases(O.Option) maybe-ast(self.loc):
          | some(ast) =>
            [ED.error:
              ed-intro("tuple lookup expression", self.loc, -1, true),
              ED.cmcode(self.loc),
              [ED.para:
                ED.text("The "),
                ED.highlight(ED.text("left side"), [ED.locs: ast.tup.l], 0),
                ED.text(" was not a tuple value:")],
              ED.embed(self.non-tup)]
          | none      =>
            [ED.error:
              ed-intro("tuple lookup expression", self.loc, 0, true),
              ED.cmcode(self.loc),
              [ED.para: ED.text("The left side was not a tuple value:")],
              ED.embed(self.non-tup)]
        end
      else:
        [ED.error:
          ed-simple-intro("tuple lookup expression", self.loc),
          [ED.para: ED.text("The left side was not a tuple value:")],
          ED.embed(self.non-tup)]
      end
    end,
    method render-reason(self):
      [ED.error:
        ed-simple-intro("tuple lookup expression", self.loc),
        [ED.para: ED.text("The left side was not a tuple value:")],
        ED.embed(self.non-tup)]
    end
  | lookup-large-index(loc, tup, len, index :: Number) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      if self.loc.is-builtin():
        [ED.error:
          ed-simple-intro("tuple lookup expression", self.loc),
          [ED.para: 
            ED.text("The left side was a tuple of "),
            ED.ed-components(self.len),
            ED.text(", smaller than the given position ("),
            ED.text(num-to-string(self.index) + "):")],
          ED.embed(self.tup)]
      else if src-available(self.loc):
        cases(O.Option) maybe-ast(self.loc):
          | some(ast) =>
            [ED.error:
              ed-intro("tuple lookup expression", self.loc, -1, true),
              ED.cmcode(self.loc),
              [ED.para:
                ED.text("The "),
                ED.highlight(ED.text("left side"), [ED.locs: ast.tup.l], 0),
                ED.text(" was a tuple of "),
                ED.ed-components(self.len),
                ED.text(", smaller than the given "),
                ED.highlight(ED.text("position"), [ED.locs: ast.index-loc], 1),
                ED.text(" (" + num-to-string(self.index) + "):")],
              ED.embed(self.tup)]
          | none      =>
            [ED.error:
              ed-intro("tuple lookup expression", self.loc, 0, true),
              ED.cmcode(self.loc),
              [ED.para: 
                ED.text("The left side was a tuple of "),
                ED.ed-components(self.len),
                ED.text(", smaller than the given position ("),
                ED.text(num-to-string(self.index) + "):")],
              ED.embed(self.tup)]
        end
      else:
        [ED.error:
          ed-simple-intro("tuple lookup expression", self.loc),
          [ED.para: 
            ED.text(" failed because the left side was a tuple of "),
            ED.ed-components(self.len),
            ED.text(", smaller than the given position ("),
            ED.text(num-to-string(self.index) + "):")],
          ED.embed(self.tup)]
      end
    end,
    method render-reason(self):
      [ED.error:
        ed-simple-intro("tuple lookup expression", self.loc),
        [ED.para: 
          ED.text(" failed because the left side was a tuple of "),
          ED.ed-components(self.len),
          ED.text(", smaller than the given position ("),
          ED.text(num-to-string(self.index) + "):")],
        ED.embed(self.tup)]
    end
  | non-tuple-bind(loc, non-tup) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      if self.loc.is-builtin():
        [ED.error:
          [ED.para:
            ED.text("The tuple binding statement in "),
            ED.loc(self.loc),
            ED.text(" failed because the binding was given a non-tuple value: ")],
          ED.embed(self.non-tup),
          please-report-bug()]
      else if src-available(self.loc):
        cases(O.Option) maybe-ast(self.loc):
          | some(ast) =>
            [ED.error:
              ed-intro("tuple binding statement", self.loc, -1, true),
              ED.cmcode(self.loc),
              [ED.para: ED.text("failed because the binding was given a non-tuple value:")],
              ED.embed(self.non-tup)]
          | none      =>
            [ED.error:
              [ED.para:
                ED.text("The tuple binding statement in "),
                ED.loc(self.loc),
                ED.text(" failed because the binding was given a non-tuple value:")],
              ED.embed(self.non-tup)]
        end
      else:
        [ED.error:
          [ED.para:
            ED.text("The tuple binding statement in "),
            ED.loc(self.loc),
            ED.text(" failed because the binding was given a non-tuple value:")],
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
            ED.text(" are expected to be bound to values, but the binding was given a tuple containing "),
            ED.ed-components(self.els),
            ED.text(":")],
          ED.embed(self.tup),
          please-report-bug()]
      else if src-available(self.loc):
        cases(O.Option) maybe-ast(self.loc):
          | some(ast) =>
            [ED.error:
              ed-intro("tuple binding", self.loc, -1, true),
              ED.cmcode(self.loc),
              [ED.para:
                ED.text("failed because "),
                ED.highlight(ED.ed-names(self.desiredLength), ast.fields.map(_.l), 0),
                ED.text(" are expected to be bound to values, but the binding was given "),
                ED.text(" a tuple containing "),
                ED.ed-components(self.length),
                ED.text(":")],
              ED.embed(self.tup)]
          | none      =>
            [ED.error:
              ed-intro("tuple binding", self.loc, 0, true),
              ED.cmcode(self.loc),
              [ED.para:
                ED.text("failed because "),
                ED.embed(self.desiredLength),
                ED.ed-components(" are expected to be bound to values, but the binding was given a tuple containing "),
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
            ED.text(" are expected to be bound to values, but the binding was given a tuple containing "),
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
            ED.text(" are expected to be bound to values, but the binding was given a tuple containing "),
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
          ed-simple-intro("field lookup", self.loc),
          [ED.para: ED.text("It was given a non-object value:")],
          ED.embed(self.non-obj)]
      else if src-available(self.loc):
        cases(O.Option) maybe-ast(self.loc):
          | some(ast) =>
            shadow ast = cases(Any) ast:
              | s-dot(_,_,_) => ast
              | s-app(_,f,_) => f
            end
            [ED.error:
              ed-intro("field lookup expression", self.loc, -1, true),
              ED.cmcode(self.loc),
              [ED.para:
                ED.text("The "),
                ED.highlight(ED.text("left side"), [ED.locs: ast.obj.l], 0),
                ED.text(" was not an object:")],
              ED.embed(self.non-obj)]
          | none =>
            [ED.error:
              ed-intro("field lookup expression", self.loc, -1, true),
              ED.cmcode(self.loc),
              [ED.para:
                ED.text("The left side was not an object:")],
              ED.embed(self.non-obj)]
        end
      else:
        [ED.error:
          ed-simple-intro("field lookup", self.loc),
          [ED.para: ED.text("The left side was not an object:")],
          ED.embed(self.non-obj)]
      end
    end,
    method render-reason(self):
      [ED.error:
        ed-simple-intro("object lookup", self.loc),
        [ED.para: ED.text("The left side was not an object:")],
        ED.embed(self.non-obj)]
    end
  | extend-non-object(loc, non-obj) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      if self.loc.is-builtin():
        [ED.error:
          ed-simple-intro("object extension expression", self.loc),
          [ED.para: ED.text("It was given a non-object value:")],
          ED.embed(self.non-obj)]
      else if src-available(self.loc):
        cases(O.Option) maybe-ast(self.loc):
          | some(ast) =>
            shadow ast = cases(Any) ast:
              | s-extend(_,_,_) => ast
              | s-app(_,f,_) => f
            end
            [ED.error:
              ed-intro("object extension expression", self.loc, -1, true),
              ED.cmcode(self.loc),
              [ED.para:
                ED.text("The "),
                ED.highlight(ED.text("left side"), [ED.locs: ast.supe.l], 0),
                ED.text(" was not an object:")],
              ED.embed(self.non-obj)]
          | none =>
            [ED.error:
              ed-intro("object extension expression", self.loc, -1, true),
              ED.cmcode(self.loc),
              [ED.para:
                ED.text("The left side was not an object:")],
              ED.embed(self.non-obj)]
        end
      else:
        [ED.error:
          ed-simple-intro("object extension expression", self.loc),
          [ED.para: ED.text("The left side was not an object:")],
          ED.embed(self.non-obj)]
      end
    end,
    method render-reason(self):
      [ED.error:
        ed-simple-intro("object extension expression", self.loc),
        [ED.para: ED.text("The left side was not an object:")],
        ED.embed(self.non-obj)]
    end
  | non-boolean-condition(loc, typ, value) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      self.render-reason() # TODO!!
    end,
    method render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Expected "), ED.code(ED.text("true")), ED.text(" or "), ED.code(ED.text("false")),
          ED.text(" for the test in the "), ED.text(self.typ), ED.text(" expression at "),
          draw-and-highlight(self.loc), ED.text(" but got:")],
        ED.embed(self.value)]
    end
  | non-boolean-op(loc, position, typ, value) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
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
                ed-simple-intro("expression", loc),
                [ED.para:
                  ED.text("It was expected to produce a "),
                  ED.embed(self.typ),
                  ED.text(", but it produced a non-"),
                  ED.embed(self.typ),
                  ED.text(" value:")],
                ED.embed(self.val)]
            else if src-available(loc):
              [ED.sequence:
                ed-intro("expression", loc, 0, true),
                ED.cmcode(loc),
                [ED.para:
                  ED.text("It was expected to produce a "),
                  ED.embed(self.typ),
                  ED.text(", but it produced a non-"),
                  ED.embed(self.typ),
                  ED.text(" value:")],
                ED.embed(self.val)]
            else:
              [ED.sequence:
                ed-simple-intro("expression", loc),
                [ED.para:
                  ED.text("It was expected to produce a "),
                  ED.embed(self.typ),
                  ED.text(", but it produced a non-"),
                  ED.embed(self.typ),
                  ED.text(" value:")],
                ED.embed(self.val)]
            end
          | none =>
            [ED.sequence:
              [ED.para:
                ED.text("Evaluating an expression failed. It was expected to evaluate to a "),
                ED.embed(self.typ),
                ED.text(". It evaluated to the non-"),
                ED.embed(self.typ),
                ED.text(" value:")],
              ED.embed(self.val)]
        end]
    end, 
    method render-reason(self):
      [ED.error:
        ED.maybe-stack-loc(0, true,
          lam(loc):
            [ED.sequence:
              ed-simple-intro("expression", loc),
              [ED.para:
                ED.text("It was expected to evaluate to a "),
                ED.embed(self.typ),
                ED.text(". It evaluated to the non-"),
                ED.embed(self.typ),
                ED.text(" value:")]]
          end,
          [ED.sequence:
            [ED.para:
              ED.text("Evaluating an expression failed. It was expected to evaluate to a "),
              ED.embed(self.typ),
              ED.text(". It evaluated to the non-"),
              ED.embed(self.typ),
              ED.text(" value:")]]),
        ED.embed(self.val)]
    end
  | num-string-binop-error(val1, val2, opname, opdesc, methodname) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      [ED.error:
        cases(O.Option) maybe-stack-loc(0, false):
          | some(loc) =>
            if loc.is-builtin():
              [ED.sequence:
                ed-simple-intro(self.opdesc + " (" + self.opname + ") expression", loc),
                [ED.para:
                  ED.text("The left side was:")],
                ED.embed(self.val1),
                [ED.para:
                  ED.text("The right side was:")],
                ED.embed(self.val2),
                [ED.para:
                  ED.text("The " + self.opname + " operator expects to be given:"),
                  [ED.bulleted:
                    ED.text("two Numbers, or"),
                    ED.text("two Strings")]]]
            else if src-available(loc):
              cases(O.Option) maybe-ast(loc):
                | some(ast) =>
                  left-loc =  ast.left.l
                  right-loc = ast.right.l
                  [ED.sequence:
                    ed-intro(self.opdesc + " (" + self.opname + ") expression", loc, -1, true),
                    ED.cmcode(loc),
                    [ED.para:
                      ED.text("The "),
                      ED.highlight(ED.text("left side"), [ED.locs: left-loc],0),
                      ED.text(" was:")],
                    ED.embed(self.val1),
                    [ED.para:
                      ED.text("The "),
                      ED.highlight(ED.text("right side"), [ED.locs: right-loc],1),
                      ED.text(" was:")],
                    ED.embed(self.val2),
                    [ED.para:
                      ED.text("The " + self.opname + " operator expects to be given:"),
                      [ED.bulleted:
                        ED.text("two Numbers, or"),
                        ED.text("two Strings")]]]
                | none      =>
                  [ED.sequence:
                    ed-intro(self.opdesc + " (" + self.opname + ") expression", loc, 0, true),
                    ED.cmcode(loc),
                    [ED.para: ED.text("The left side was:")],
                    ED.embed(self.val1),
                    [ED.para: ED.text("The right side was:")],
                    ED.embed(self.val2),
                    [ED.para:
                      ED.text("The " + self.opname + " operator expects to be given:"),
                      [ED.bulleted:
                        ED.text("two Numbers, or"),
                        ED.text("two Strings")]]]
              end
            else:
              [ED.sequence:
                ed-simple-intro(self.opdesc + " (" + self.opname + ") expression", loc),
                [ED.para: ED.text("The left side was:")],
                ED.embed(self.val1),
                [ED.para: ED.text("The right side was:")],
                ED.embed(self.val2),
                [ED.para:
                  ED.text("The " + self.opname + " operator expects to be given:"),
                  [ED.bulleted:
                    ED.text("two Numbers, or"),
                    ED.text("two Strings")]]]
            end
          | none =>
            [ED.sequence:
              [ED.para:
                ED.text("A " + self.opdesc + " ("),
                ED.code(self.opname),
                ED.text(") expression errored.")],
              [ED.para:
                ED.text("The left side was:")],
              ED.embed(self.val1),
              [ED.para:
                ED.text("The right side was:")],
              ED.embed(self.val2),
              [ED.para:
                ED.text("The " + self.opname + " operator expects to be given:"),
                [ED.bulleted:
                  ED.text("two Numbers, or"),
                  ED.text("two Strings")]]]
        end]
    end,
    method render-reason(self):
      [ED.error: ED.maybe-stack-loc(0, false,
          lam(loc):
            [ED.sequence:
              ed-simple-intro(self.opdesc + " (" + self.opname + ") expression", loc),
              [ED.para:
                ED.text("The left side was:")],
              ED.embed(self.val1),
              [ED.para:
                ED.text("The right side was:")],
              ED.embed(self.val2),
              [ED.para:
                ED.text("The " + self.opname + " operator expects to be given:"),
                [ED.bulleted:
                  ED.text("two Numbers, or"),
                  ED.text("two Strings")]]]
          end,
          [ED.sequence:
            [ED.para:
              ED.text("A "),
              ED.code(self.opname),
              ED.text(" expression errored.")],
            [ED.para:
              ED.text("The left side was:")],
            ED.embed(self.val1),
            [ED.para:
              ED.text("The right side was:")],
            ED.embed(self.val2),
            [ED.para:
              ED.text("The " + self.opname + " operator expects to be given:"),
              [ED.bulleted:
                ED.text("two Numbers, or"),
                ED.text("two Strings")]]])]
    end
  | numeric-binop-error(val1, val2, opname, opdesc, methodname) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      [ED.error:
        cases(O.Option) maybe-stack-loc(0, false):
          | some(loc) =>
            if loc.is-builtin():
              [ED.sequence:
                ed-simple-intro(self.opdesc + " (" + self.opname + ") expression", loc),
                [ED.para:
                  ED.text("The left side was:")],
                ED.embed(self.val1),
                [ED.para:
                  ED.text("The right side was:")],
                ED.embed(self.val2),
                [ED.para:
                  ED.text("The " + self.opname + " operator expects to be given two Numbers.")]]
            else if src-available(loc):
              cases(O.Option) maybe-ast(loc):
                | some(ast) =>
                  left-loc =  ast.left.l
                  right-loc = ast.right.l
                  [ED.sequence:
                    ed-intro(self.opdesc + " (" + self.opname + ") expression", loc, -1, true),
                    ED.cmcode(loc),
                    [ED.para:
                      ED.text("The "),
                      ED.highlight(ED.text("left side"), [ED.locs: left-loc],0),
                      ED.text(" was:")],
                    ED.embed(self.val1),
                    [ED.para:
                      ED.text("The "),
                      ED.highlight(ED.text("right side"), [ED.locs: right-loc],1),
                      ED.text(" was:")],
                    ED.embed(self.val2),
                    [ED.para:
                      ED.text("The " + self.opname + " operator expects to be given two Numbers.")]]
                | none      =>
                  [ED.sequence:
                    ed-intro(self.opdesc + " (" + self.opname + ") expression", loc, 0, true),
                    ED.cmcode(loc),
                    [ED.para: ED.text("The left side was:")],
                    ED.embed(self.val1),
                    [ED.para: ED.text("The right side was:")],
                    ED.embed(self.val2),
                    [ED.para:
                      ED.text("The " + self.opname + " operator expects to be given two Numbers.")]]
              end
            else:
              [ED.sequence:
                ed-simple-intro(self.opdesc + " (" + self.opname + ") expression", loc),
                [ED.para: ED.text("The left side was:")],
                ED.embed(self.val1),
                [ED.para: ED.text("The right side was:")],
                ED.embed(self.val2),
                [ED.para:
                  ED.text("The " + self.opname + " operator expects to be given two Numbers.")]]
            end
          | none =>
            [ED.sequence:
              [ED.para:
                ED.text("A " + self.opdesc + " ("),
                ED.code(self.opname),
                ED.text(") expression errored.")],
              [ED.para:
                ED.text("The left side was:")],
              ED.embed(self.val1),
              [ED.para:
                ED.text("The right side was:")],
              ED.embed(self.val2),
              [ED.para:
                ED.text("The " + self.opname + " operator expects to be given two Numbers.")]]
        end]
    end,
    method render-reason(self):
      [ED.error: ED.maybe-stack-loc(0, false,
          lam(loc):
            [ED.sequence:
              ed-simple-intro(self.opdesc + " (" + self.opname + ") expression", loc),
              [ED.para:
                ED.text("The left side was:")],
              ED.embed(self.val1),
              [ED.para:
                ED.text("The right side was:")],
              ED.embed(self.val2),
              [ED.para:
                ED.text("The " + self.opname + " operator expects to be given two Numbers.")]]
          end,
          [ED.sequence:
            [ED.para:
              ED.text("A "),
              ED.code(self.opname),
              ED.text(" expression errored.")],
            [ED.para:
              ED.text("The left side was:")],
            ED.embed(self.val1),
            [ED.para:
              ED.text("The right side was:")],
            ED.embed(self.val2),
            [ED.para:
              ED.text("The " + self.opname + " operator expects to be given two Numbers.")]])]
    end
  | cases-singleton-mismatch(branch-loc, should-be-singleton :: Boolean, cases-loc) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      if self.branch-loc.is-builtin():
        [ED.error:
          [ED.para:
            ED.text("A cases branch in "),
            ED.loc(self.branch-loc),
            if self.should-be-singleton:
              ED.text("errored. The branch has an argument list but the corresponding variant is a singleton.")
            else:
              ED.text("The branch doesn't have an argument list in its pattern, but the corresponding variant is not a singleton.")
            end]]
      else if src-available(self.branch-loc):
        cases(O.Option) maybe-ast(self.cases-loc):
          | some(ast) =>
            branch = ast.branches.find(lam(b): b.l.start-line == self.branch-loc.start-line end).value
            [ED.error:
              [ED.para:
                ED.text("Matching the pattern of this "),
                ED.highlight(ED.text("cases branch"), [ED.locs: self.branch-loc], -1),
                ED.text(" errored:")],
              ED.cmcode(self.branch-loc),
              if self.should-be-singleton:
                [ED.para:
                  ED.text("The "),
                  ED.highlight(ED.text("branch"), [ED.locs: self.branch-loc], -1),
                  ED.text(" has an "),
                  ED.highlight(ED.text("argument list"), branch.args.map(_.l), 0),
                  ED.text(" but the corresponding variant is a singleton.")]
              else:
                [ED.para:
                  ED.text("The branch doesn't have an argument list in its "),
                  ED.highlight(ED.text("pattern"), [ED.locs: branch.pat-loc], 0),
                  ED.text(", but the corresponding variant is not a singleton.")]
              end]
          | none      =>
            [ED.error:
              [ED.para:
                ED.text("Matching a patten of this "),
                ED.highlight(ED.text("cases branch"), [ED.locs: self.branch-loc], 0),
                ED.text(" errored.")],
              if self.should-be-singleton:
                [ED.para:
                  ED.text("The branch has an argument list but the corresponding variant is a singleton.")]
              else:
                [ED.para:
                  ED.text("The branch doesn't have an argument list in its pattern, but the corresponding variant is not a singleton.")]
              end]
        end
      else:
        [ED.error:
          [ED.para:
            ED.text("The cases branch at "),
            ED.loc(self.branch-loc),
            if self.should-be-singleton:
              ED.text("errored. The branch has an argument list but the corresponding variant is a singleton.")
            else:
              ED.text("The branch doesn't have an argument list in its pattern, but the corresponding variant is not a singleton.")
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
            ED.text("Matching a cases branch in "),
            ED.loc(self.cases-loc),
            ED.text(" errored because of a problem with the branch pattern in "),
            ED.loc(self.branch-loc),
            ED.text(".")],
          [ED.para:
            ED.text("The cases pattern had "),
            ED.ed-field-bindings(self.num-args),
            ED.text(".")],
          [ED.para:
            ED.text("The corresponding variant of the datatype had "),
            ED.ed-fields(self.actual-arity)],
          [ED.para:
            ED.text("Patterns should have exactly the same number of field bindings as the corresponding variant has fields.")]]
      else if src-available(self.branch-loc):
        cases(O.Option) maybe-ast(self.cases-loc):
          | some(cases-ast) =>
            branch = cases-ast.branches.find(lam(b): b.l.start-line == self.branch-loc.start-line end).value
            [ED.error:
              [ED.para:
                ED.text("Matching the "),
                ED.highlight(ED.text("pattern"), [ED.locs: branch.pat-loc], 0),
                ED.text(" of this "),
                ED.highlight(ED.text("cases branch"), [ED.locs: self.branch-loc], -1),
                ED.text(" errored.")],
              ED.cmcode(branch.pat-loc),
              [ED.para:
                ED.text("The cases pattern had "),
                cases(Any) branch:
                  | s-cases-branch(_, pat-loc, name, args, _) =>
                    args-locs = if self.num-args == 0:
                      name-len = string-length(name)
                      [ED.locs: S.srcloc(pat-loc.source,
                          pat-loc.start-line, pat-loc.start-column + name-len, pat-loc.start-char + name-len,
                          pat-loc.end-line, pat-loc.end-column, pat-loc.end-char)]
                    else:
                      args.map(_.l)
                    end
                    ED.highlight(ED.ed-field-bindings(self.num-args),args-locs, 1)
                  | s-singleton-cases-branch(_, _, _, _) => ED.ed-field-bindings(self.num-args)
                end,
                ED.text(".")],
              [ED.para:
                ED.text("The corresponding variant is declared with "),
                ED.ed-fields(self.actual-arity)],
              [ED.para:
                ED.text("Patterns should have exactly the same number of field bindings as the corresponding variant has fields.")]]
          | none      =>
            [ED.error:
              [ED.para:
                ED.text("Matching the pattern of a "),
                ED.highlight(ED.text("cases branch"), [ED.locs: self.branch-loc], -1),
                ED.text(" errored.")],
              ED.cmcode(self.pat-loc),
              [ED.para:
                ED.text("The cases pattern had "),
                ED.ed-field-bindings(self.num-args),
                ED.text(".")],
              [ED.para:
                ED.text("The corresponding variant is declared with "),
                ED.ed-fields(self.actual-arity)],
              [ED.para:
                ED.text("Patterns should have exactly the same number of field bindings as the corresponding variant has fields.")]]
        end
      else:
        [ED.error:
          [ED.para:
            ED.text("Matching a cases branch in "),
            ED.loc(self.cases-loc),
            ED.text(" errored because of a problem with the branch pattern in "),
            ED.loc(self.branch-loc),
            ED.text(".")],
          [ED.para:
            ED.text("The cases pattern had "),
            ED.ed-field-bindings(self.num-args),
            ED.text(".")],
          [ED.para:
            ED.text("The corresponding variant of the datatype had "),
            ED.ed-fields(self.actual-arity)],
          [ED.para:
            ED.text("Patterns should have exactly the same number of field bindings as the corresponding variant has fields.")]]
      end
    end,
    method render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Matching a cases branch in "),
          ED.loc(self.cases-loc),
          ED.text(" errored because of a problem with the branch pattern in "),
          ED.loc(self.branch-loc),
          ED.text(".")],
        [ED.para:
          ED.text("The cases pattern had "),
          ED.ed-field-bindings(self.num-args),
          ED.text(".")],
        [ED.para:
          ED.text("The corresponding variant of the datatype had "),
          ED.ed-fields(self.actual-arity)],
        [ED.para:
          ED.text("Patterns should have exactly the same number of field bindings as the corresponding variant has fields.")]]
    end
  | constructor-arity-mismatch(fun-def-loc, constructor-name, fun-def-arity, fun-app-args) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      fun-app-arity = self.fun-app-args.length()
      were-was = if fun-app-arity == 1: " was" else: " were" end
      fun helper(rest):
        [ED.error: 
          cases(O.Option) maybe-stack-loc(0, true):
            | some(fun-app-loc) =>
              if fun-app-loc.is-builtin():
                [ED.sequence:
                  [ED.para:
                    ED.text("Evaluating the function application in "),
                    ED.loc(fun-app-loc),
                    ED.text(" errored.  Expected the applicant to evaluate to a function that accepts exactly the same number of arguments as are given to it.")],
                  [ED.para:
                    ED.ed-args(fun-app-arity),
                    ED.text(were-was + " passed to the left side.")],
                  rest(ED.text("left side"))]
              else if src-available(fun-app-loc):
                cases(O.Option) maybe-ast(fun-app-loc):
                  | some(ast) =>
                    applicant = ED.highlight(ED.text("left side"), [ED.locs: ast._fun.l], 0)
                    arg-locs = ast.args.map(_.l)
                    fun-method = "constructor"
                    [ED.sequence:
                      ed-intro(fun-method + " application expression", fun-app-loc, -1, true),
                      ED.cmcode(fun-app-loc),
                      [ED.para:
                        ED.highlight(ED.ed-args(fun-app-arity), arg-locs, 1),
                        ED.text(were-was + " passed to the "),
                        applicant,
                        ED.text(".")],
                      rest(applicant)]
                  | none      =>
                    [ED.sequence:
                      ed-intro("function application expression", fun-app-loc, -1, true),
                      ED.cmcode(fun-app-loc),
                      [ED.para:
                        ED.ed-args(fun-app-arity),
                        ED.text(were-was + " passed to the left side.")],
                      rest(ED.text("applicant"))]
                end
              else:
                [ED.sequence:
                  ed-simple-intro("function application expression", fun-app-loc), 
                  [ED.para:
                    ED.text("The applicant had "),
                    ED.ed-args(fun-app-arity),
                    ED.text(" passed to it.")],
                  rest(ED.text("left side"))]
              end
            | none =>
              [ED.sequence:
                [ED.para:
                  ED.text("A function application expression failed.")],
                [ED.para:
                  ED.text("The applicant had "),
                  ED.ed-args(fun-app-arity),
                  ED.text(" passed to it.")],
                rest(ED.text("left side"))]
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
                {if l-underscore and r-underscore:
                    [ED.locs: l.id.l, r.id.l]
                  else if l-underscore:
                    [ED.locs: l.id.l]
                  else if r-underscore:
                    [ED.locs: r.id.l]
                  else:
                    [ED.locs:]
                  end; self.fun-def-loc}
              | s-app(_,_,args) => {args.map(_.l); self.fun-def-loc}
              | s-fun(l, _, _, args, _, _, b, _, _, _) => {args.map(_.l); l.upto(b.l)}
              | s-lam(l, _, _, args, _, _, b, _, _, _) => {args.map(_.l); l.upto(b.l)}
              | s-method(l, _, _, args, _, _, b, _, _, _) => {args.map(_.l); l.upto(b.l)}
              | s-method-field(l, _, _, args, _, _, b, _, _, _) => {args.map(_.l); l.upto(b.l)}
              | s-dot(_, obj, _)      => {[ED.locs: obj.id.l]; self.fun-def-loc}
              | s-extend(_, obj, _)   => {[ED.locs: obj.id.l]; self.fun-def-loc}
              | s-update(_, obj, _)   => {[ED.locs: obj.id.l]; self.fun-def-loc}
              | s-get-bang(_, obj, _) => {[ED.locs: obj.id.l]; self.fun-def-loc}
            end
            helper(lam(applicant):
                [ED.sequence:
                  [ED.para:
                    ED.text("The "),
                    applicant,
                    ED.text(" was a constructor accepting "),
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
                    ED.text(" was a constructor defined to accept "),
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
              ED.text(" was a constructor defined in "),
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
      
      ED.maybe-stack-loc(0
        #|if self.fun-def-loc.is-builtin():
          0
        else:
          1
        end|#, false,
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
  | arity-mismatch(fun-def-loc, fun-def-arity, fun-app-args, is-method) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast) block:
      fun-app-arity = self.fun-app-args.length()
      were-was = if fun-app-arity == 1: " was" else: " were" end
      function-or-method = if self.is-method: "method" else: "function" end

      fun locs-from-application-ast(ast) block:
        fun adjust(fun-loc, args):
          if fun-app-arity == 0:
            [ED.locs: fun-loc.at-end().upto-end(ast.l)]
          else:
            args
          end
        end
        cases(Any) ast:
          | s-app(l, _fun, args)          => {l; _fun.l; adjust(_fun.l, args.map(_.l))}
          | s-dot(l, _fun, args)          => {l; _fun.l; adjust(_fun.l, args.map(_.l))}
          # TODO: `s-for` is distinct enough that it probably requires specialized wording.
          | s-for(l, _fun, args, _, b, _) => {l; _fun.l; adjust(_fun.l, [ED.locs: b.l] + args.map(_.l))}
          | s-op(l,_,_,l-op,r-op)         => {l; r-op.l; [ED.locs: l-op.l]}
          | else  => block:
            # This _really_ should not happen.
            {ast.l; ast.l; [ED.locs: ast.l]}
            end
        end
      end

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

      fun locs-from-definition-ast(ast):
        # TODO: something clever for definitions with zero parameters
        cases(Any) ast:
          | s-op(_,_,_,l,r) =>
            {[ED.locs: l, r]
              .filter(is-underscore)
              .map(_.id.l);
              self.fun-def-loc}
          | s-app(_, _, args) => {args.filter(is-underscore).map(_.l); self.fun-def-loc}
          | s-fun(l, _, _, args, _, _, b, _, _, _) => {args.map(_.l); l.upto(b.l)}
          | s-lam(l, _, _, args, _, _, b, _, _, _) => {args.map(_.l); l.upto(b.l)}
          | s-method(l, _, _, args, _, _, b, _, _, _) => {args.map(_.l); l.upto(b.l)}
          | s-method-field(l, _, _, args, _, _, b, _, _, _) => {args.map(_.l); l.upto(b.l)}
          | s-dot(_, obj, _)      => {[ED.locs: obj.id.l]; self.fun-def-loc}
          | s-extend(_, obj, _)   => {[ED.locs: obj.id.l]; self.fun-def-loc}
          | s-update(_, obj, _)   => {[ED.locs: obj.id.l]; self.fun-def-loc}
          | s-get-bang(_, obj, _) => {[ED.locs: obj.id.l]; self.fun-def-loc}
        end
      end

      fun and-if(predicate):
        lam(option):
          cases(O.Option) option:
            | none => O.none
            | some(v) =>
              if predicate(v):
                O.some(v)
              else:
                O.none
              end
          end
        end
      end

      fun and-maybe(f):
        lam(option):
          cases(O.Option) option:
            | none => O.none
            | some(v) => f(v)
          end
        end
      end

      application-loc =
        maybe-stack-loc(if self.fun-def-loc.is-builtin(): 0 else: 1 end, false)

      definition-contained =
        application-loc
          .and-then(_.contains(self.fun-def-loc))
          .or-else(false)

      destructured-application =
        (application-loc ^
           and-if(src-available) ^
           and-maybe(maybe-ast))
          .and-then(locs-from-application-ast)

      destructured-definition =
        (O.some(self.fun-def-loc) ^
           and-if(src-available) ^
           and-maybe(maybe-ast))
          .and-then(locs-from-definition-ast)

      fun operator-prose(arguments, operator):
        [ED.para:
          arguments,
          ED.text(were-was + " passed to the "),
          operator,
          ED.text(".")]
      end

      fun definition-prose(operator, defined, parameters, show-definition):
        if show-definition:
          [ED.sequence:
            [ED.para:
              ED.text("The "),
              operator,
              ED.text(" evaluated to a " + function-or-method + " "),
              defined,
              ED.text(" to accept "),
              parameters,
              ED.text(":")],
            ED.cmcode(self.fun-def-loc)]
        else if src-available(self.fun-def-loc):
          [ED.para:
            ED.text("The "),
            operator,
            ED.text(" evaluated to a " + function-or-method + " "),
            defined,
            ED.text(" to accept "),
            parameters,
            ED.text(".")]
        else:
          [ED.para:
            ED.text("The "),
            operator,
            ED.text(" evaluated to a " + function-or-method + " accepting "),
            parameters,
            ED.text(".")]
        end
      end

      fun explanation-prose(application-expression, parameters, arguments):
        [ED.para:
          ED.text("An "),
          application-expression,
          ED.text(" expects the number of "),
          parameters,
          ED.text(" and "),
          arguments,
          ED.text(" to be the same.")]
      end

      arguments = destructured-application
        .and-then(
          lam(v):
            {app-loc; op-loc; args} = v
            ED.highlight(_, args, 3)
          end)
        .or-else({(v): v })

      application-expression = destructured-application
        .and-then(
          lam(v):
            {app-loc; op-loc; args} = v
            ED.highlight(ED.text("application expression"), [ED.locs: app-loc], -1)
          end)
        .or-else(ED.text("application"))

      operator = destructured-application
        .and-then(
          lam(v):
            {app-loc; op-loc; args} = v
            ED.highlight(ED.text("operator"), [ED.locs: op-loc],
              if definition-contained:
                -2
              else:
                2
              end)
          end)
        .or-else(ED.text("operator"))

      parameters = destructured-definition
        .and-then(
          lam(v):
            {params; def-loc} = v
            ED.highlight(_, params, 4)
          end)
        .or-else({(v): v })

      defined = destructured-definition
        .and-then(
          lam(v):
            ED.highlight(ED.text("defined"), [ED.locs: self.fun-def-loc], -5)
          end)
        .or-else(ED.text("defined"))

      [ED.error:
        destructured-application
          .and-then(
            lam(v):
              {app-loc; op-loc; args} = v
              [ED.sequence:
                [ED.para:
                  ED.text("This "),
                  application-expression,
                  ED.text(" errored:")],
                ED.cmcode(app-loc)]
            end)
          .or-else(
            [ED.para:
              ED.text("An application errored.")]),
        operator-prose(arguments(ED.ed-args(fun-app-arity)), operator),
        definition-prose(
          operator,
          defined,
          parameters(ED.ed-params(self.fun-def-arity)),
          not(definition-contained) and src-available(self.fun-def-loc)),
        explanation-prose(application-expression,
          parameters(ED.text("parameters")),
          arguments(ED.text("arguments")))]
    end,
    method render-reason(self):
      num-args = self.fun-app-args.length()
      this-str = if num-args == 1: "this " else: "these " end
      arg-str = if num-args == 1: " argument:" else: " arguments:" end
      exp-arg-str = if self.fun-def-arity == 1: " argument" else: " arguments" end
      
      ED.maybe-stack-loc(
        if self.fun-def-loc.is-builtin():
          0
        else:
          1
        end, false,
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
          ed-simple-intro("function application expression", self.loc),
          [ED.para: ED.text("The left side was not a function value:")],
          ED.embed(self.non-fun-val)]
      else if src-available(self.loc):
        cases(O.Option) maybe-ast(self.loc):
          | some(ast) =>
            [ED.error:
              ed-intro("function application expression", self.loc, -1, true),
              ED.cmcode(self.loc),
              [ED.para:
                ED.text("The "),
                ED.highlight(ED.text("left side"), [ED.locs: 
                    cases(Any) ast:
                      | s-app(_, _fun, _) => _fun.l
                      | s-for(_, _fun, _, _, _, _) => _fun.l
                      | else  => ast.l
                    end], 0),
                ED.text(" was not a function value:")],
              ED.embed(self.non-fun-val)]
          | none      =>
            [ED.error:
              ed-intro("function application expression", self.loc, 0, true),
              ED.cmcode(self.loc),
              [ED.para:
                ED.text("The left side was not a function value:")],
              ED.embed(self.non-fun-val)]
        end
      else:
        [ED.error:
          ed-simple-intro("function application expression", self.loc),
          [ED.para: ED.text("The left side was not a function value:")],
          ED.embed(self.non-fun-val)]
      end
    end,
    method render-reason(self):
      [ED.error:
        ed-simple-intro("function application expression", self.loc),
        [ED.para: ED.text("The left side was not a function value:")],
        ED.embed(self.non-fun-val)]
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
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
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
        cases(O.Option) maybe-stack-loc(0, true):
          | some(loc) =>
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
                ed-intro(self.method-name, loc, 0, true),
                ED.cmcode(loc),
                [ED.para:
                  ED.text("It. expects that the index passed to it is an integer within the bounds of the array. "),
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
                  ED.text("It expects that the index passed to it is an integer within the bounds of the array. ")],
                [ED.para:
                  ED.embed(self.index),
                  ED.text(" is an invalid array index because "),
                  ED.text(self.reason)]]
            end
          | none =>
            [ED.sequence:
              [ED.para: 
                ED.text("An array interaction, "),
                ED.code(ED.text(self.method-name)),
                ED.text("It expects that the index passed to it is an integer within the bounds of the array. ")],
              [ED.para:
                ED.embed(self.index),
                ED.text(" is an invalid array index because "),
                ED.text(self.reason)]]
        end]
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
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
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
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      self.render-reason()
    end,
    method render-reason(self):
      [ED.error: ED.text("Program stopped by user")]
    end
    
  | user-exception(value :: Any) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      self.render-reason()
    end,
    method render-reason(self): [ED.error: [ED.para: ED.embed(self.value)]] end,
    method _output(self):
      VS.vs-value(self.value)
    end
    
  | exit(code :: Number) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      self.render-reason()
    end,
    method render-reason(self):
      [ED.error: ED.text("Exited with code "), ED.embed(self.code)]
    end
    
  | exit-quiet(code :: Number) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      self.render-reason()
    end,
    method render-reason(self):
      ED.text("")
    end
end

data ParseError:
  | parse-error-next-token(loc, next-token :: String) with:
    method render-fancy-reason(self, src-available):
      if src-available(self.loc):
        [ED.error:
          [ED.para: ED.text("Pyret didn't understand your program around ")],
          ED.cmcode(self.loc),
          [ED.para: ED.text(" You may need to add or remove some text to fix your program. "),
            ED.text("Look carefully before the "),ED.highlight(ED.text("highlighted text"),[ED.locs: self.loc],0),
            ED.text(". Is there a missing colon ("), ED.code(ED.text(":")),
            ED.text("), comma ("), ED.code(ED.text(",")),
            ED.text("), string marker ("), ED.code(ED.text("\"")),
            ED.text("), or keyword? Is there something there that shouldn’t be?")]]
      else:
        [ED.error:
          [ED.para: ED.text("Pyret didn't understand your program around "),
            ED.loc(self.loc)],
          [ED.para: ED.text(" You may need to add or remove some text to fix your program. "),
            ED.text("Look carefully before the "),ED.highlight(ED.text("highlighted text"),[ED.locs: self.loc],0),
            ED.text(". Is there a missing colon ("), ED.code(ED.text(":")),
            ED.text("), comma ("), ED.code(ED.text(",")),
            ED.text("), string marker ("), ED.code(ED.text("\"")),
            ED.text("), or keyword? Is there something there that shouldn’t be?")]]
      end
    end,
    method render-reason(self):
      [ED.error:
        [ED.para: ED.text("Pyret didn't understand your program around "), draw-and-highlight(self.loc)],
        [ED.para: ED.text("You may need to add or remove some text to fix your program.")],
        [ED.para: ED.text("Look carefully before the highlighted text.")],
        [ED.para: ED.text("Is there a missing colon ("), ED.code(ED.text(":")),
          ED.text("), comma ("), ED.code(ED.text(",")),
          ED.text("), string marker ("), ED.code(ED.text("\"")),
          ED.text("), or keyword?")],
        [ED.para: ED.text("Is there something there that shouldn’t be?")]
      ]
    end
  | parse-error-eof(loc) with:
    method render-fancy-reason(self, src-available):
      if src-available(self.loc):
        [ED.error: 
          [ED.para:
            ED.text("Pyret didn't expect your program to "),
            ED.highlight(ED.text("end"),[ED.locs: self.loc],-1),
            ED.text(" as soon as it did:")],
           ED.cmcode(self.loc),
          [ED.para:
            ED.text("You may be missing an \"end\", or closing punctuation like \")\" or \"]\" somewhere in your program.")]]
      else:
        [ED.error: 
          [ED.para:
            ED.text("Pyret didn't expect your program to end (at "),
            ED.loc(self.loc),
            ED.text(") as soon as it did. You may be missing an \"end\", or closing punctuation like \")\" or \"]\" somewhere in your program.")]]
      end
    end,
    method render-reason(self):
      [ED.error: [ED.para:
          ED.text("Pyret didn't understand the very end of your program."),
          ED.text("You may be missing an \"end\", or closing punctuation like \")\" or \"]\" right at the end.")]]
    end
  | parse-error-unterminated-string(loc) with:
    method render-fancy-reason(self, src-available):
      if src-available(self.loc):
        [ED.error: 
          [ED.para:
            ED.text("Pyret thinks the string ")],
          ED.cmcode(self.loc),
          [ED.para:
            ED.text("is unterminated; you may be missing closing punctuation. If you intended to write a multi-line string, use "),
            ED.code(ED.text("```")),
            ED.text(" instead of quotation marks.")]]
      else:
        [ED.error: 
          [ED.para:
            ED.text("Pyret thinks the string at "),
            ED.loc(self.loc),
            ED.text("is unterminated; you may be missing closing punctuation. If you intended to write a multi-line string, use "),
            ED.code(ED.text("```")),
            ED.text(" instead of quotation marks.")]]
      end
    end,
    method render-reason(self):
      [ED.error: [ED.para-nospace:
          ED.text("Pyret thinks your program has an incomplete string literal around "),
          draw-and-highlight(self.loc),
          ED.text("; you may be missing closing punctuation.")]]
    end
  | parse-error-bad-operator(loc) with:
    method render-fancy-reason(self, src-available):
      if src-available(self.loc):
        [ED.error: 
          [ED.para:
            ED.text("The "),
            ED.highlight(ED.text("operator"), [ED.locs: self.loc], 0)],
          ED.cmcode(self.loc),
          [ED.para:
            ED.text(" must have whitespace separating it from its operands.")]]
      else:
        [ED.error: 
          [ED.para:
            ED.text("The operator at "),
            ED.loc(self.loc),
            ED.text(" must have whitespace separating it from its operands.")]]
      end
    end,
    method render-reason(self):
      [ED.error: [ED.para-nospace:
          ED.text("The operator at "),
          draw-and-highlight(self.loc),
          ED.text(" has no surrounding whitespace.")]]
    end
  | parse-error-bad-number(loc) with:
    method render-fancy-reason(self, src-available):
      if src-available(self.loc):
        [ED.error: 
          [ED.para:
            ED.text("Pyret thinks ")],
          ED.cmcode(self.loc),
          [ED.para:
            ED.text(" is probably a number, but number literals in Pyret require at least one digit before the decimal point.")]]
      else:
        [ED.error: 
          [ED.para:
            ED.text("Pyret thinks your program has a number at "),
            ED.loc(self.loc),
            ED.text(", but number literals in Pyret require at least one digit before the decimal point.")]]
      end
    end,
    method render-reason(self):
      [ED.error: [ED.para-nospace:
          ED.text("Pyret thinks your program probably has a number at "),
          draw-and-highlight(self.loc),
          ED.text("; number literals in Pyret require at least one digit before the decimal point.")]]
    end
  | parse-error-bad-check-operator(loc) with:
    method render-fancy-reason(self, src-available):
      if src-available(self.loc):
        [ED.error: 
          [ED.para:
            ED.text("The "),
            ED.highlight(ED.text("testing operator"), [ED.locs: self.loc], 0)],
          ED.cmcode(self.loc),
          [ED.para:
            ED.text(" must be used inside a "),
            ED.code(ED.text("check")), ED.text(" or "), ED.code(ED.text("where")), ED.text(" block.")],
          [ED.para:
            ED.text("Did you mean to use one of the comparison operators instead?")]]
      else:
        [ED.error: 
          [ED.para-nospace:
            ED.text("The testing operator at "),
            ED.loc(self.loc),
            ED.text(" must be used inside a "),
            ED.code(ED.text("check")), ED.text(" or "), ED.code(ED.text("where")), ED.text(" block.")],
          [ED.para:
            ED.text("Did you mean to use one of the comparison operators instead?")]]
      end
    end,
    method render-reason(self):
      [ED.error: 
        [ED.para-nospace:
          ED.text("The testing operator at "),
          draw-and-highlight(self.loc),
          ED.text(" must be used inside a"),
          ED.code(ED.text("check")), ED.text(" or "), ED.code(ED.text("where")), ED.text(" block.")],
        [ED.para:
          ED.text("Did you mean to use one of the comparison operators instead?")]]
    end
  | empty-block(loc) with:
    method render-reason(self):
      ED.text("Empty block at " + self.loc.format(true))
    end
  | bad-block-stmt(loc) with:
    method render-reason(self):
      ED.text("Expected a val binding or an expression, but got something else " + self.loc.format(true))
    end
  | bad-check-block-stmt(loc) with:
    method render-reason(self):
      ED.text("Expected a val binding or an expression, but got something else " + self.loc.format(true))
    end
  | fun-missing-colon(loc) with:
    method render-reason(self): ED.text("fun-missing-colon: " + self.loc.format(true)) end
  | fun-missing-end(loc) with:
    method render-reason(self): ED.text("fun-missing-end: " + self.loc.format(true)) end
  | args-missing-comma(loc) with:
    method render-reason(self): ED.text("args-missing-comma: " + self.loc.format(true)) end
  | app-args-missing-comma(loc) with:
    method render-reason(self): ED.text("app-args-missing-comma: " + self.loc.format(true)) end
  | missing-end(loc) with:
    method render-reason(self): ED.text("missing-end: " + self.loc.format(true)) end
  | missing-comma(loc) with:
    method render-reason(self): ED.text("missing-comma: " + self.loc.format(true)) end
end
