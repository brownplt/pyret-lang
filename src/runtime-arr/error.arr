provide: *, data * end

import raw-array as A
include from A: raw-array-map, raw-array-length end
import string as STR
include from STR: string-length end
import global as G
include from G: not end
import number as N
include from N: num-to-string, is-number, is-roughnum end
import option as O
import srcloc as S
import error-display as ED
import valueskeleton as VS

fun draw-and-highlight(l):
  ED.loc-display(l, "error-highlight", ED.loc(l))
end
fun vert-list-values(vals :: RawArray<Any>):
  ED.v-sequence(raw-array-map(lam(val): [ED.para: ED.embed(val)] end, vals))
end
fun horz-list-values(vals :: RawArray<Any>):
  [ED.para: ED.h-sequence(raw-array-map(lam(val): ED.embed(val) end, vals), ",")]
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

fun and-if<A>(predicate :: (A -> Boolean), option :: O.Option<A>) -> O.Option<A>:
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

fun and-maybe<A, B>(f :: (A -> O.Option<B>), option :: O.Option<A>) -> O.Option<B>:
  cases(O.Option) option:
    | none => O.none
    | some(v) => f(v)
  end
end

data RuntimeError:
  | multi-error(errors :: RawArray<RuntimeError>) with:
    method render-reason(self):
      rendered = raw-array-map(_.render-reason(), self.errors)
      ED.v-sequence(rendered)
    end
  | sample-error(loc :: S.Srcloc, some-info :: Any) with:
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
    method render-reason(self):
      [ED.error: [ED.para: ED.text(self.message)]]
    end,
    method _output(self):
      VS.vs-literal-str(self.message)
    end
  | update-non-obj(loc :: S.Srcloc, obj :: Any, objloc :: S.Srcloc) with:
    method render-reason(self):
      [ED.error:
        ed-simple-intro("reference update expression", self.loc),
        [ED.para: ED.text("It was given a non-object value:")],
        ED.embed(self.obj)]
    end
  | update-non-ref(loc :: S.Srcloc, obj :: Any, objloc :: S.Srcloc, field :: String, fieldloc :: S.Srcloc) with:
    method render-reason(self):
      [ED.error:
        ed-simple-intro("reference update expression", self.loc),
        [ED.para:
          ED.text("The field "),
          ED.code(ED.text(self.field)),
          ED.text(" is not a mutable reference in the object:")],
        ED.embed(self.obj)]
    end
  | update-non-existent-field(loc :: S.Srcloc, obj :: Any, objloc :: S.Srcloc, field :: String, fieldloc :: S.Srcloc) with:
    method render-reason(self):
      [ED.error:
        ed-simple-intro("reference update expression", self.loc),
        [ED.para:
          ED.text("The field "),
          ED.code(ED.text(self.field)),
          ED.text(" does not exist in the object:")],
        ED.embed(self.obj)]
    end
  | no-cases-matched(loc :: S.Srcloc, val :: Any) with:
    method render-reason(self):
      [ED.error:
        ed-simple-intro("cases expression", self.loc),
        [ED.para: ED.text("No branch matched the value:")],
        ED.embed(self.val)]
    end
  | no-branches-matched(loc :: S.Srcloc, expression :: String) with:
    method render-reason(self):
      [ED.error:
        ed-simple-intro(self.expression + " expression", self.loc),
        [ED.para:
          ED.text("It expected that the condition of at least one branch be satisfied. No branch conditions were satisfied, so no branch could be entered.")]]
    end
  | internal-error(message :: String, info-args :: RawArray<Any>) with:
    method render-reason(self):
      [ED.error:
        [ED.para: ED.text("Internal error:"), ED.text(self.message)],
        [ED.para: ED.text("Relevant arguments:")],
        vert-list-values(self.info-args)]
    end
  | spinnaker-error(funloc :: S.Srcloc, step-num :: Number) with:
    method render-reason(self):
      [ED.error:
        [ED.para: ED.text("Internal compiler error: No case numbered " + num-to-string(self.step-num) + " in")],
        ED.loc(self.funloc),
        please-report-bug()]
    end
  | template-not-finished(loc :: S.Srcloc) with:
    method render-reason(self):
      [ED.error:
        ed-simple-intro("unfinished template expression", self.loc),
        [ED.para: ED.text("Template expressions cannot be evaluated.")]]
    end
  | field-not-found(loc :: S.Srcloc, obj :: Any, field :: String) with:
    method render-reason(self):
      [ED.error:
        ed-simple-intro("field lookup expression", self.loc),
        [ED.para:
          ED.text("The left side was an object that did not have a field named "),
          ED.code(ED.text(self.field)),
          ED.text(":")],
        ED.embed(self.obj)]
    end
  | constructor-syntax-non-constructor(expr-loc :: S.Srcloc, constr-loc :: S.Srcloc) with:
    method render-reason(self):
      [ED.error:
        ed-simple-intro("construction expression", self.expr-loc),
        [ED.para:
          ED.text("The left side was not a defined convenience constructor.")]]
    end
  | lookup-constructor-not-object(loc :: S.Srcloc, constr-name :: String, field :: String) with:
    method render-reason(self):
      [ED.error:
        ed-simple-intro("field lookup expression", self.loc),
        [ED.para:
          ED.text("The left side was a constructor, not an object.")]]
    end
  | lookup-non-tuple(loc :: S.Srcloc, non-tup :: Any, index :: Number) with:
    method render-reason(self):
      [ED.error:
        ed-simple-intro("tuple lookup expression", self.loc),
        [ED.para: ED.text("The left side was not a tuple value:")],
        ED.embed(self.non-tup)]
    end
  | lookup-large-index(loc :: S.Srcloc, tup :: Any, len :: Number, index :: Number) with:
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
  | non-tuple-bind(loc :: S.Srcloc, non-tup :: Any) with:
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
  | bad-tuple-bind(loc :: S.Srcloc, tup :: Any, length :: Number, desiredLength :: Number) with:
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
  | lookup-non-object(loc :: S.Srcloc, non-obj :: Any, field :: String) with:
    method render-reason(self):
      [ED.error:
        ed-simple-intro("object lookup", self.loc),
        [ED.para: ED.text("The left side was not an object:")],
        ED.embed(self.non-obj)]
    end
  | extend-non-object(loc :: S.Srcloc, non-obj :: Any) with:
    method render-reason(self):
      [ED.error:
        ed-simple-intro("object extension expression", self.loc),
        [ED.para: ED.text("The left side was not an object:")],
        ED.embed(self.non-obj)]
    end
  | non-boolean-condition(loc :: S.Srcloc, typ :: String, value :: Any) with:
    method render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Expected "), ED.code(ED.text("true")), ED.text(" or "), ED.code(ED.text("false")),
          ED.text(" for the test in the "), ED.text(self.typ), ED.text(" expression at "),
          draw-and-highlight(self.loc), ED.text(" but got:")],
        ED.embed(self.value)]
    end
  | non-boolean-op(loc :: S.Srcloc, position :: String, typ :: String, value :: Any) with:
    method render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Expected"), ED.code(ED.text("true")), ED.text("or"), ED.code(ED.text("false")),
          ED.text("for the"), ED.text(self.position), ED.text("argument in the"),
          ED.text(self.typ), ED.text("expression at"),
          draw-and-highlight(self.loc), ED.text(" but got:")],
        ED.embed(self.value)]
    end
  | generic-type-mismatch(val :: Any, typ :: String) with:
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
  | num-string-binop-error(val1 :: Any, val2 :: Any, opname :: String, opdesc :: String, methodname :: String) with:
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
  | numeric-binop-error(val1 :: Any, val2 :: Any, opname :: String, opdesc :: String, methodname :: String) with:
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
  | cases-singleton-mismatch(branch-loc :: S.Srcloc, should-be-singleton :: Boolean, cases-loc :: S.Srcloc, constructor-loc :: S.Srcloc) with:
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
  | cases-arity-mismatch(branch-loc :: S.Srcloc, num-args :: Number, actual-arity :: Number, cases-loc :: S.Srcloc, constructor-loc :: S.Srcloc) with:
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
  | constructor-arity-mismatch(fun-def-loc :: S.Srcloc, constructor-name :: String, fun-def-arity :: Number, fun-app-args :: RawArray<Any>) with:
    method render-reason(self):
      num-args = raw-array-length(self.fun-app-args)
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

  | arity-mismatch(fun-def-loc :: S.Srcloc, fun-def-arity :: Number, fun-app-args :: RawArray<Any>, is-method :: Boolean) with:
    method render-reason(self):
      num-args = raw-array-length(self.fun-app-args)
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

  | row-length-mismatch(colnames :: RawArray<String>, provided-vals :: RawArray<Any>) with:
    method render-reason(self):
      [ED.error:
        [ED.para: ED.text("The row could not be constructed because the number of columns didn't match the number of provided values.")],
        [ED.para: ED.text("Expected columns:")],
        ED.embed(self.colnames),
        [ED.para: ED.text("Provided values:")],
        vert-list-values(self.provided-vals)]
    end

  | col-length-mismatch(colname :: String, expected :: Number, actual :: Number, value :: Any) with:
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

  | non-function-app(loc :: S.Srcloc, non-fun-val :: Any) with:
    method render-reason(self):
      [ED.error:
        ed-simple-intro("function application expression", self.loc),
        [ED.para: ED.text("The left side was not a function value:")],
        ED.embed(self.non-fun-val)]
    end
  | uninitialized-id(loc :: S.Srcloc, name :: String) with:
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
  | module-load-failure(names :: RawArray<String>) with:
    method render-reason(self):
      [ED.error:
        [ED.para:
          if raw-array-length(self.names) == 1: ED.text("The following module failed to load:")
          else:                        ED.text("The following modules failed to load:")
          end],
        ED.h-sequence-sep(raw-array-map(ED.text, self.names), ", ", ", and ")]
    end
  | invalid-array-index(method-name :: String, array :: RawArray<Any>, index :: Number, reason :: String) with:
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
  | equality-failure(reason :: String, value1 :: Any, value2 :: Any) with:
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
          else if is-roughnum(value1) and is-roughnum(value2):
            within-error("Attempted to compare two Roughnums for equality, which is not allowed:")
          else if is-roughnum(value1):
            within-error("Attempted to compare a Roughnum to an Exactnum for equality, which is not allowed:")
          else if is-roughnum(value2):
            within-error("Attempted to compare an Exactnum to a Roughnum for equality, which is not allowed:")
          end
        | otherwise:
          [ED.error:
            [ED.para: ED.text("Attempted to compare two incomparable values: ")],
            [ED.para: ED.embed(self.value1)],
            [ED.para: ED.embed(self.value2)]]
      end
    end
  | column-not-found(operation-loc :: S.Srcloc, column-name :: String, column-loc :: S.Srcloc, columns :: RawArray<String>) with:
    method render-reason(self):
      [ED.error:
        ed-simple-intro("table operation", self.operation-loc),
        [ED.para:
          ED.text("The table did not have a column named "),
          ED.code(ED.text(self.column-name)),
          ED.text("; it only had columns named:")],
        ED.bulleted-sequence(raw-array-map(ED.text, self.columns) ^ raw-array-map(ED.code, _))]
    end
  | duplicate-column(operation-loc :: S.Srcloc, column-name :: String, column-loc :: S.Srcloc) with:
    method render-reason(self):
      [ED.error:
        ed-simple-intro("table operation", self.operation-loc),
        [ED.para:
          ED.text("The table already has a column named "),
          ED.code(ED.text(self.column-name)),
          ED.text(".")]]
    end

  | user-break with:
    method render-reason(self):
      [ED.error: ED.text("Program stopped by user")]
    end
    
  | user-exception(value :: Any) with:
    method render-reason(self): [ED.error: [ED.para: ED.embed(self.value)]] end,
    
  | exit(code :: Number) with:
    method render-reason(self):
      [ED.error: ED.text("Exited with code "), ED.embed(self.code)]
    end
    
  | exit-quiet(code :: Number) with:
    method render-reason(self):
      ED.text("")
    end
sharing:
  method render-reason(self):
    G.raise("Unimplemented render-reason")
  end
end