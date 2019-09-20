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
fun horz-list-values(vals):
  [ED.para: ED.h-sequence(vals.map(lam(val): ED.embed(val) end), ",") ]
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

fun and-if(predicate, option):
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

fun and-maybe(f, option):
  cases(O.Option) option:
    | none => O.none
    | some(v) => f(v)
  end
end

fun destructure-method-application(l, src-available, maybe-ast):
  l ^ and-if(src-available, _)
    ^ and-maybe(maybe-ast, _)
    ^ and-maybe({(ast):
        cases(Any) ast:
          | s-app(_, _fun, args)  =>
            O.some({cases(Any) _fun:
                      | s-dot(_, t, m) => O.some(t.l)
                      | else           => O.none
                    end; args.map(_.l)})
          | s-dot(_, _fun, args)  => O.some({O.some(_fun.l); args.map(_.l)})
          | s-op(_,_,_,l-op,r-op) => O.some({O.some(r-op.l); [ED.locs: l-op.l]})
          | else                  => O.none
        end}, _)
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
            ED.highlight(ED.text("field"),
              [ED.locs:
                let l = self.fieldloc,
                    n = string-length(self.field):
                    S.srcloc(l.source,
                      l.start-line, l.start-column, l.start-char,
                      l.start-line, l.start-column + n, l.start-char + n)
                end], 0),
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
            ED.highlight(ED.text("field"),
              [ED.locs:
                let l = self.fieldloc,
                    n = string-length(self.field):
                    S.srcloc(l.source,
                      l.start-line, l.start-column, l.start-char,
                      l.start-line, l.start-column + n, l.start-char + n)
                end], 0),
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
            ED.highlight(ED.text("field"),
              [ED.locs:
                let l = self.fieldloc,
                    n = string-length(self.field):
                    S.srcloc(l.source,
                      l.start-line, l.start-column, l.start-char,
                      l.start-line, l.start-column + n, l.start-char + n)
                end], 0),
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
            ED.text("The left side was not a defined convenience constructor.")]]
      else:
        [ED.error:
          ed-intro("construction expression", self.expr-loc, -1, true),
          ED.cmcode(self.expr-loc),
          [ED.para:
            ED.text("The "),
            ED.highlight(ED.text("left side"), [ED.locs: self.constr-loc], 0),
            ED.text(" was not a defined convenience constructor.")]]
      end
    end,
    method render-reason(self):
      [ED.error:
        ed-simple-intro("construction expression", self.expr-loc),
        [ED.para:
          ED.text("The left side was not a defined convenience constructor.")]]
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
                ED.ed-components(self.desiredLength),
                ED.text(" are expected to be bound to values, but the binding was given a tuple containing "),
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
                ED.code(ED.text(self.opname)),
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
              ED.code(ED.text(self.opname)),
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
                ED.code(ED.text(self.opname)),
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
              ED.code(ED.text(self.opname)),
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
  | cases-singleton-mismatch(branch-loc, should-be-singleton :: Boolean, cases-loc, constructor-loc) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      fun locs-from-cases-ast(ast) block:
        cases(Any) ast:
          | s-cases-branch(_, pat-loc, name, args, _) =>
            {pat-loc; args.map(_.l)}
          | s-singleton-cases-branch(_, pat-loc, name, _) =>
            {pat-loc; nothing}
        end
      end

      fun maybe-first(l):
        cases(Any) l:
          | link(first, rest) => O.some(first)
          | empty => O.none
        end
      end

      fun locs-from-constructor-ast(ast):
        # TODO: something clever for definitions with zero parameters
        cases(Any) ast:
          | s-variant(l, constr-loc, _, members, _) =>
            {members.map(_.l); constr-loc}
          | s-singleton-variant(l, name, with-members) =>
            # We really need a `constr-loc` equivalent for `s-singleton-variant`.
            {nothing;
              maybe-first(with-members)
                .and-then(_.l)
                .and-then(l.upto)
                .or-else(l)}
        end
      end

      destructured-pattern =
        (O.some(self.branch-loc) ^
           and-if(src-available, _) ^
           and-maybe(maybe-ast, _))
          .and-then(locs-from-cases-ast)

      destructured-definition =
        (O.some(self.constructor-loc) ^
           and-if(src-available, _) ^
           and-maybe(maybe-ast, _))
          .and-then(locs-from-constructor-ast)

      constructor-loc =
        destructured-definition
          .and-then({(v) block:
            v.{1}})
          .or-else(self.constructor-loc)

      fun pattern-prose(pattern, bindings):
        [ED.para:
          ED.text("The "),
          pattern,
          ED.text(" has "),
          bindings,
          ED.text(".")]
      end

      fun observation-prose(pattern, bindings, variant, members):
        if self.should-be-singleton:
          if src-available(constructor-loc):
            [ED.sequence:
              [ED.para:
                ED.text("The "),
                pattern,
                ED.text(" has a "),
                bindings,
                ED.text(", but refers to a "),
                variant,
                ED.text(" that is a singleton:")],
              ED.cmcode(constructor-loc)]
          else:
            [ED.para:
              ED.text("The "),
              pattern,
              ED.text(" has a "),
              bindings,
              ED.text(", but refers to a "),
              variant,
              ED.text(" that is a singleton.")]
          end
        else:
          if src-available(constructor-loc):
            [ED.sequence:
              [ED.para:
                ED.text("The "),
                pattern,
                ED.text(" has no binding list, but refers to a "),
                variant,
                ED.text(" that has a "),
                members,
                ED.text(":")],
              ED.cmcode(constructor-loc)]
          else:
            [ED.para:
              ED.text("The "),
              pattern,
              ED.text(" has no binding list, but refers to a "),
              variant,
              ED.text(" that has a "),
              members,
              ED.text(".")]
          end
        end
      end

      fun explanation-prose(pattern, variant):
        [ED.para:
          ED.text("A "),
          pattern,
          ED.text(" must match the "),
          variant,
          ED.text(" that it refers to.")]
      end

      bindings = destructured-pattern
        .and-then(
          lam(v):
            {pat-loc; binds} = v
            if is-nothing(binds):
              {(w): w }
            else:
              ED.highlight(_, binds, 3)
            end
          end)
        .or-else({(v): v })

      pattern = destructured-pattern
        .and-then(
          lam(v):
            {pat-loc; binds} = v
            ED.highlight(_, [ED.locs: pat-loc],
              if self.should-be-singleton:
                -1
              else:
                1
              end)
          end)
        .or-else({(v): v })

      fields = destructured-definition
        .and-then(
          lam(v):
            {params; def-loc} = v
            if is-nothing(params):
              {(w): w }
            else:
              ED.highlight(_, params, 4)
            end
          end)
        .or-else({(v): v })

      variant = destructured-definition
        .and-then(
          lam(v):
            {params; def-loc} = v
            ED.highlight(ED.text("variant"), [ED.locs: def-loc],
              if self.should-be-singleton:
                5
              else:
                -5
              end)
          end)
        .or-else(ED.text("variant"))

      [ED.error:
        destructured-pattern
          .and-then(
            lam(v):
              {pat-loc; binds} = v
              [ED.sequence:
                [ED.para:
                  ED.text("Matching this "),
                  pattern(ED.text("cases branch pattern")),
                  ED.text(" errored:")],
                ED.cmcode(pat-loc)]
            end)
          .or-else(
            [ED.para:
              ED.text("Matching a cases branch pattern errored.")]),
        observation-prose(
          pattern(ED.text("branch")),
          bindings(ED.text("binding list")),
          variant,
          fields(ED.text("fields list"))),
        explanation-prose(
          pattern(ED.text("cases branch pattern")),
          variant)]
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
  | cases-arity-mismatch(branch-loc, num-args, actual-arity, cases-loc, constructor-loc) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      were-was = if self.num-args == 1: " was" else: " were" end

      fun locs-from-cases-ast(ast) block:
        fun adjust(fun-loc, args):
          if self.num-args == 0:
            [ED.locs: fun-loc.at-end().upto-end(ast.l)]
          else:
            args
          end
        end
        cases(Any) ast:
          | s-cases-branch(_, pat-loc, name, args, _) =>
            {pat-loc; args.map(_.l)}
        end
      end

      fun locs-from-constructor-ast(ast):
        # TODO: something clever for definitions with zero parameters
        cases(Any) ast:
          | s-variant(l, constr-loc, _, members, _) =>
            {members.map(_.l); constr-loc}
        end
      end

      destructured-pattern =
        (O.some(self.branch-loc) ^
           and-if(src-available, _) ^
           and-maybe(maybe-ast, _))
          .and-then(locs-from-cases-ast)

      destructured-definition =
        (O.some(self.constructor-loc) ^
           and-if(src-available, _) ^
           and-maybe(maybe-ast, _))
          .and-then(locs-from-constructor-ast)

      fun pattern-prose(pattern, bindings):
        [ED.para:
          ED.text("The "),
          pattern,
          ED.text(" has "),
          bindings,
          ED.text(".")]
      end

      fun observation-prose(pattern, bindings, variant, members):
        if src-available(self.constructor-loc):
          [ED.sequence:
            [ED.para:
              ED.text("The "),
              pattern,
              ED.text(" has "),
              bindings,
              ED.text(", but refers to a "),
              variant,
              ED.text(" that has "),
              members,
              ED.text(":")],
            ED.cmcode(self.constructor-loc)]
        else:
          [ED.para:
            ED.text("The "),
            pattern,
            ED.text(" refers to a "),
            variant,
            ED.text(" that has "),
            members,
            ED.text(".")]
        end
      end

      fun explanation-prose(pattern, parameters, bindings):
        [ED.para:
          ED.text("A "),
          pattern,
          ED.text(" expects the number of "),
          parameters,
          ED.text(" and "),
          bindings,
          ED.text(" to be the same.")]
      end

      bindings = destructured-pattern
        .and-then(
          lam(v):
            {pat-loc; binds} = v
            ED.highlight(_, binds, 3)
          end)
        .or-else({(v): v })

      pattern = destructured-pattern
        .and-then(
          lam(v):
            {pat-loc; binds} = v
            ED.highlight(_, [ED.locs: pat-loc], -1)
          end)
        .or-else({(v): v })

      fields = destructured-definition
        .and-then(
          lam(v):
            {params; def-loc} = v
            ED.highlight(_, params, 4)
          end)
        .or-else({(v): v })

      variant = destructured-definition
        .and-then(
          lam(v):
            ED.highlight(ED.text("variant"), [ED.locs: self.constructor-loc], -5)
          end)
        .or-else(ED.text("variant"))

      [ED.error:
        destructured-pattern
          .and-then(
            lam(v):
              {pat-loc; binds} = v
              [ED.sequence:
                [ED.para:
                  ED.text("Matching this "),
                  pattern(ED.text("cases branch pattern")),
                  ED.text(" errored:")],
                ED.cmcode(pat-loc)]
            end)
          .or-else(
            [ED.para:
              ED.text("Matching a cases branch pattern errored.")]),
        observation-prose(
          pattern(ED.text("branch")),
          bindings(ED.ed-bindings(self.num-args)),
          variant,
          fields(ED.ed-fields(self.actual-arity))),
        explanation-prose(
          pattern(ED.text("cases branch pattern")),
          fields(ED.text("fields")),
          bindings(ED.text("bindings")))]
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

      fun locs-from-definition-ast(ast):
        # TODO: something clever for definitions with zero parameters
        cases(Any) ast:
          | s-variant(l, constr-loc, _, members, _) =>
            {members.map(_.l); constr-loc}
        end
      end

      application-loc =
        maybe-stack-loc(0, false)

      definition-contained =
        application-loc
          .and-then(_.contains(self.fun-def-loc))
          .or-else(false)

      destructured-application =
        (application-loc ^
           and-if(src-available, _) ^
           and-maybe(maybe-ast, _))
          .and-then(locs-from-application-ast)

      destructured-definition =
        (O.some(self.fun-def-loc) ^
           and-if(src-available, _) ^
           and-maybe(maybe-ast, _))
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
              ED.text(" evaluated to a constructor "),
              defined,
              ED.text(" to accept "),
              parameters,
              ED.text(":")],
            ED.cmcode(self.fun-def-loc)]
        else if src-available(self.fun-def-loc):
          [ED.para:
            ED.text("The "),
            operator,
            ED.text(" evaluated to a constructor "),
            defined,
            ED.text(" to accept "),
            parameters,
            ED.text(".")]
        else:
          [ED.para:
            ED.text("The "),
            operator,
            ED.text(" evaluated to a constructor accepting "),
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

      application-loc =
        maybe-stack-loc(if self.fun-def-loc.is-builtin(): 0 else: 1 end, false)

      definition-contained =
        application-loc
          .and-then(_.contains(self.fun-def-loc))
          .or-else(false)

      destructured-application =
        (application-loc ^
           and-if(src-available, _) ^
           and-maybe(maybe-ast, _))
          .and-then(locs-from-application-ast)

      destructured-definition =
        (O.some(self.fun-def-loc) ^
           and-if(src-available, _) ^
           and-maybe(maybe-ast, _))
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

  | row-length-mismatch(colnames, provided-vals) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      cases(O.Option) maybe-stack-loc(0, true):
        | some(l) =>
          [ED.error:
            ed-intro("row construction", l, -1, true),
            [ED.para: ED.text("The row could not be constructed because the number of expected columns didn't match the number of provided values.")],
            [ED.para: ED.text("Expected columns:")],
            ED.embed(self.colnames),
            [ED.para: ED.text("Provided values:")],
            horz-list-values(raw-array-to-list(self.provided-vals))]
        | none => self.render-reason()
      end
    end,
    method render-reason(self):
      [ED.error:
        [ED.para: ED.text("The row could not be constructed because the number of columns didn't match the number of provided values.")],
        [ED.para: ED.text("Expected columns:")],
        ED.embed(self.colnames),
        [ED.para: ED.text("Provided values:")],
        vert-list-values(raw-array-to-list(self.provided-vals))]
    end

  | col-length-mismatch(colname, expected, actual, value) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      application-loc = maybe-stack-loc(0, true)

      intro-prose =
        application-loc
          .and-then({(l):
            if src-available(l):
              [ED.sequence:
                [ED.para:
                  ED.text("This "),
                  ED.highlight(
                    [ED.sequence: ED.code(ED.text("add-column")),
                                  ED.text(" operation")],
                    [ED.locs: l], -1),
                  ED.text(" errored:")],
                ED.cmcode(l)]
            else:
              [ED.para:
                ED.text("An "),
                ED.code(ED.text("add-column")),
                ED.text(" operation in "),
                ED.loc(l),
                ED.text(" errored.")]
            end})
          .or-else(
            [ED.para:
              ED.text("An "),
              ED.code(ED.text("add-column")),
              ED.text(" operation errored.")])

      info-prose =
        destructure-method-application(application-loc, src-available, maybe-ast)
          .and-then({({maybe-table; args}):
              tbl = maybe-table
                  .and-then({(tl): ED.highlight(ED.text("table"), [ED.locs: tl], 0)})
                  .or-else(ED.text("table"))
              [ED.para:
                ED.text("The "),
                tbl,
                ED.text(" had "),
                ED.ed-rows(self.expected),
                ED.text(", but "),
                ED.ed-values(self.actual),
                ED.text(" were "),
                ED.highlight(ED.text("provided"), [ED.locs: args.get(1)], 1),
                ED.text(":")]})
          .or-else(
            [ED.para:
              ED.text("The table had "),
              ED.ed-rows(self.expected),
              ED.text(", but "),
              ED.ed-values(self.actual),
              ED.text(" were provided:")])

      [ED.error:
        intro-prose,
        info-prose,
        ED.embed(self.value),
        [ED.para:
          ED.text("The "),
          ED.code(ED.text("add-column")),
          ED.text(" operation expects the number of provided values to match the number of rows in the table.")]]
    end,
    method render-reason(self):
      [ED.error:
        ED.maybe-stack-loc(0, true,
          {(l):
            [ED.para:
              ED.text("An "),
              ED.code(ED.text("add-column")),
              ED.text(" operation in "),
              ED.loc(l),
              ED.text(" errored.")]},
          [ED.para:
            ED.text("An "),
            ED.code(ED.text("add-column")),
            ED.text(" operation errored.")]),
        [ED.para:
          ED.text("The table had "),
          ED.ed-rows(self.expected),
          ED.text(", but "),
          ED.ed-values(self.actual),
          ED.text(" were provided:")],
        ED.embed(self.value),
        [ED.para:
          ED.text("The "),
          ED.code(ED.text("add-column")),
          ED.text(" operation expects the number of provided values to match the number of rows in the table.")]]
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
        ED.h-sequence-sep(self.names.map(ED.text), ", ", ", and ")]
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
                  ED.text(self.reason), ED.text(".")],
                please-report-bug()]
            else if src-available(loc):
              [ED.sequence:
                ed-intro(self.method-name, loc, 0, true),
                ED.cmcode(loc),
                [ED.para:
                  ED.text("It expects that the index passed to it is an integer within the bounds of the array. "),
                  ED.embed(self.index),
                  ED.text(" is an invalid array index because "),
                  ED.text(self.reason), ED.text(".")]]
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
                  ED.text(self.reason), ED.text(".")]]
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
                ED.text(self.reason), ED.text(".")]]
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
                ED.text(self.reason), ED.text(".")],
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
                ED.text(self.reason), ED.text(".")]]
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
            ED.text(self.reason), ED.text(".")]])
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
              [ED.para: ED.text("The left side was:")],
              [ED.para: ED.embed(value1)],
              [ED.para: ED.text("The right side was:")],
              [ED.para: ED.embed(value2)],
              [ED.para: ED.text("Consider using the "),
                ED.code(ED.text("within")), ED.text(" function to compare them instead.")]]
            
          end
          if self.reason == "RoughnumZeroTolerances":
            [ED.error:
              [ED.para: ED.text("Pyret cannot be certain that Roughnums are the same to exactly-zero tolerance.")],
              [ED.para: ED.text("The left side was:")],
              [ED.para: ED.embed(value1)],
              [ED.para: ED.text("The right side was:")],
              [ED.para: ED.embed(value2)],
              [ED.para: ED.text("Use a larger tolerance, or "), ED.embed(~0), ED.text(" instead.")]]
          else if num-is-roughnum(value1) and num-is-roughnum(value2):
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
  | column-not-found(operation-loc, column-name, column-loc, columns) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      fun destructure-ast(ast):
        cases(Any) ast:
          | s-table-extend(l, column-binds, extensions) =>
            {"extend";  column-binds.table.l}
          | s-table-update(l, column-binds, updates) =>
            {"update";  column-binds.table.l}
          | s-table-select(l, columns, table) =>
            {"select";  table.l}
          | s-table-order(l, table, ordering) =>
            {"order"  ; table.l}
          | s-table-filter(l, column-binds, predicate) =>
            {"sieve"  ; column-binds.table.l}
          | s-table-extract(l, column, table) =>
            {"extract"; table.l}
          | else => {""; self.operation-loc}
        end
      end

      destructured-pattern =
        (O.some(self.operation-loc) ^
           and-if(src-available, _) ^
           and-maybe(maybe-ast, _))
          .and-then(destructure-ast)

      table-text =
        destructured-pattern
          .and-then({(v): ED.highlight(_, [ED.locs: v.{1}], 0)})
          .or-else({(v): v})

      column-text =
        if src-available(self.column-loc):
          ED.highlight(_, [ED.locs: self.column-loc], 1)
        else:
          {(v):v}
        end

      [ED.error:
        destructured-pattern
          .and-then({(v):
            [ED.sequence:
              ed-intro("table " + v.{0} + " expression", self.operation-loc, -1, true),
              ED.cmcode(self.operation-loc)]})
          .or-else(ed-simple-intro("table operation", self.operation-loc)),
        [ED.para:
          ED.text("The "),
          table-text(ED.text("table")),
          ED.text(" did not have a column named "),
          column-text(ED.code(ED.text(self.column-name))),
          ED.text("; it only had columns named:")],
         ED.bulleted-sequence(self.columns.map(ED.text).map(ED.code))]
    end,
    method render-reason(self):
      [ED.error:
        ed-simple-intro("table operation", self.operation-loc),
        [ED.para:
          ED.text("The table did not have a column named "),
          ED.code(ED.text(self.column-name)),
          ED.text("; it only had columns named:")],
        ED.bulleted-sequence(self.columns.map(ED.text).map(ED.code))]
    end
  | duplicate-column(operation-loc, column-name, column-loc) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      fun destructure-ast(ast):
        cases(Any) ast:
          | s-table-extend(l, column-binds, extensions) =>
            {"extend";  column-binds.table.l}
          | s-table-update(l, column-binds, updates) =>
            {"update";  column-binds.table.l}
          | s-table-select(l, columns, table) =>
            {"select";  table.l}
          | s-table-order(l, table, ordering) =>
            {"order"  ; table.l}
          | s-table-filter(l, column-binds, predicate) =>
            {"sieve"  ; column-binds.table.l}
          | s-table-extract(l, column, table) =>
            {"extract"; table.l}
          | else => {"operation"; self.operation-loc}
        end
      end

      destructured-pattern =
        (O.some(self.operation-loc) ^
           and-if(src-available, _) ^
           and-maybe(maybe-ast, _))
          .and-then(destructure-ast)

      table-text =
        destructured-pattern
          .and-then({(v): ED.highlight(_, [ED.locs: v.{1}], 0)})
          .or-else({(v): v})

      column-text =
        if src-available(self.column-loc):
          ED.highlight(_, [ED.locs: self.column-loc], 1)
        else:
          {(v):v}
        end

      [ED.error:
        destructured-pattern
          .and-then({(v):
            [ED.sequence:
              ed-intro("table " + v.{0} + " expression", self.operation-loc, -1, true),
              ED.cmcode(self.operation-loc)]})
          .or-else(ed-simple-intro("table operation", self.operation-loc)),
        [ED.para:
          ED.text("The "),
          table-text(ED.text("table")),
          ED.text(" already has a column named "),
          column-text(ED.code(ED.text(self.column-name))),
          ED.text(".")]]
    end,
    method render-reason(self):
      [ED.error:
        ed-simple-intro("table operation", self.operation-loc),
        [ED.para:
          ED.text("The table already has a column named "),
          ED.code(ED.text(self.column-name)),
          ED.text(".")]]
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
  | units-on-unsupported-ann(loc, unit-str) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      if self.loc.is-builtin():
        [ED.error:
          ed-intro("unit annotation", self.loc, -1, true),
          ED.cmcode(self.loc),
          [ED.para: ED.text("It annotated a type which does not support units.")],
          please-report-bug()]
      else if src-available(self.loc):
        cases(O.Option) maybe-ast(self.loc):
          | some(ast) =>
            [ED.error:
              ed-intro("unit annotation", self.loc, -1, true),
              ED.cmcode(self.loc),
              [ED.para:
                ED.highlight(ED.text("The unit " + self.unit-str), [ED.locs: ast.l], 0),
                ED.text(" annotated a "),
                ED.highlight(ED.text("base type"), [ED.locs: ast.ann.l], 1),
                ED.text(" which does not support units. Consider removing the unit annotation or changing the base type.")]]
          | none =>
            [ED.error:
              ed-intro("unit annotation", self.loc, -1, true),
              ED.cmcode(self.loc),
              [ED.para:
                ED.text("It annotated a type which does not support units. "),
                ED.text("Consider removing the unit annotation or changing the base type.")]]
        end
      else:
        self.render-reason()
      end
    end,
    method render-reason(self) block:
      base-err = [ED.para:
        ed-simple-intro("unit annotation", self.loc),
        ED.text("The annotation is annotated with the unit "),
        ED.code(ED.text(self.unit-str)),
        ED.text(" but does not support unit annotations.")]
      if self.loc.is-builtin():
        [ED.error: base-err, please-report-bug()]
      else:
        [ED.error: base-err]
      end
    end
  | incompatible-units(reason, u, v) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast) block:
      base-err-msg = [ED.para:
        ED.text("The units "),
        ED.code(ED.text(self.u)),
        ED.text(" and "),
        ED.code(ED.text(self.v)),
        ED.text(" are not compatible")]
      [ED.error:
        cases(O.Option) maybe-stack-loc(0, true) block:
          | some(loc) =>
            default-err-msg = [ED.sequence:
              ed-intro(self.reason, loc, -1, true),
              ED.cmcode(loc),
              base-err-msg]
            if loc.is-builtin():
              [ED.sequence:
                ed-intro(self.reason, loc, -1, true),
                base-err-msg,
                please-report-bug()]
            else if src-available(loc):
              cases(O.Option) maybe-ast(loc):
                | some(ast) =>
                  cases(Any) ast:
                    | s-op(_l, op-l, opname, l, r) =>
                      left-loc = l.l
                      right-loc = r.l
                      [ED.sequence:
                        ed-intro(self.reason, loc, -1, true),
                        ED.cmcode(loc),
                        [ED.para:
                          ED.text("The "),
                          ED.highlight(ED.text("left side"), [ED.locs: left-loc], 0),
                          ED.text(" had the unit:")],
                        ED.code(ED.text(self.u)),
                        [ED.para:
                          ED.text("The "),
                          ED.highlight(ED.text("right side"), [ED.locs: right-loc], 1),
                          ED.text(" had the unit: ")],
                        ED.code(ED.text(self.v)),
                        [ED.para: ED.text("These units are not compatible")]]
                    | else => default-err-msg
                  end
                | none => default-err-msg
              end
            else:
              base-err-msg
            end
          | none =>
            [ED.sequence:
              [ED.para:
                ED.text("A "),
                ED.code(ED.text(self.reason)),
                ED.text(" errored.")],
              base-err-msg]
        end]
    end,
    method render-reason(self) block:
      base-err-msg = [ED.para:
        ED.text("The units "),
        ED.code(ED.text(self.u)),
        ED.text(" and "),
        ED.code(ED.text(self.v)),
        ED.text(" are not compatible")]
      [ED.error: ED.maybe-stack-loc(0, false,
        lam(loc):
          [ED.sequence:
            ed-simple-intro(self.reason, loc),
            base-err-msg]
        end,
        [ED.sequence:
          [ED.para:
            ED.text("A "),
            ED.code(ED.text(self.reason)),
            ED.text(" errored.")],
          base-err-msg])]
    end
  | invalid-unit-state(opname, n, desc) with:
    method render-fancy-reason(self, maybe-stack-loc, src-available, maybe-ast):
      base-err-msg = [ED.sequence:
        [ED.para:
          ED.text("The " + self.opname + " function failed.")],
        [ED.para:
          ED.code(ED.text(tostring(self.n))),
          ED.text(" is an invalid argument because: "),
          ED.text(self.desc)]]
      [ED.error:
        cases(O.Option) maybe-stack-loc(0, false):
          | some(loc) =>
            if loc.is-builtin():
              [ED.sequence:
                ed-intro(self.opname + " function", loc, -1, true),
                [ED.para:
                  ED.code(ED.text(tostring(self.n))),
                  ED.text(" is an invalid argument because: "),
                  ED.text(self.desc)],
                please-report-bug()]
            else if src-available(loc):
              [ED.sequence:
                ed-intro(self.opname + " function", loc, -1, true),
                ED.cmcode(loc),
                [ED.para:
                  ED.code(ED.text(tostring(self.n))),
                  ED.text(" is an invalid argument because: "),
                  ED.text(self.desc)]]
            else:
              base-err-msg
            end
          | none => base-err-msg
        end]
    end,
    method render-reason(self):
      [ED.error: ED.maybe-stack-loc(0, false,
        lam(loc):
          [ED.sequence:
            ed-simple-intro(self.opname + " function", loc),
            [ED.para:
              ED.code(ED.text(tostring(self.n))),
              ED.text(" is an invalid argument because: "),
              ED.text(self.desc)]]
        end,
        [ED.sequence:
          [ED.para: ED.text("The " + self.opname + " function failed.")],
          [ED.para:
            ED.code(ED.text(tostring(self.n))),
            ED.text(" is an invalid argument because: "),
            ED.text(self.desc)]])]
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
  | parse-error-colon-colon(loc) with:
    method render-fancy-reason(self, src-available):
      if src-available(self.loc):
        [ED.error:
          [ED.para: ED.text("Pyret didn't understand your program around ")],
          ED.cmcode(self.loc),
          [ED.para: ED.text(" If you were trying to write a type annotation (with "),
            ED.highlight(ED.code(ED.text("::")),[ED.locs: self.loc],0),
            ED.text("), remember that annotations only apply directly to names.  "),
            ED.text("If you were not trying to write an annotation, perhaps use a single colon instead.")]]
      else:
        [ED.error:
          [ED.para: ED.text("Pyret didn't understand your program around "),
            ED.loc(self.loc)],
          [ED.para: ED.text(" If you were trying to write a type annotation (with "), ED.code(ED.text("::")),
            ED.text("), remember that annotations only apply directly to names.  "),
            ED.text("If you were not trying to write an annotation, perhaps use a single colon instead.")]]
      end
    end,
    method render-reason(self):
      [ED.error:
        [ED.para: ED.text("Pyret didn't understand your program around ")],
        draw-and-highlight(self.loc),
        [ED.para: ED.text(" If you were trying to write a type annotation (with "), ED.code(ED.text("::")),
          ED.text("), remember that annotations only apply directly to names.  "),
          ED.text("If you were not trying to write an annotation, perhaps use a single colon instead.")]]
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
