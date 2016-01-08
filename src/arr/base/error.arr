provide *
provide-types *
# import arrays as A
# import lists as L

import error-display as ED

fun draw-and-highlight(l):
  ED.loc-display(l, "error-highlight", ED.loc(l))
end
fun vert-list-values(vals):
  ED.v-sequence(vals.map(lam(val): [ED.para: ED.embed(val)] end))
end

data RuntimeError:
  | message-exception(message :: String) with:
    render-reason(self):
      [ED.error: [ED.para: ED.text(self.message)]]
    end
  | no-cases-matched(loc, val) with:
    render-fancy-reason(self, loc-to-ast, loc-to-src, make-pallet):
      pallet = make-pallet(2)
      ast-cse = loc-to-ast(self.loc).block.stmts.first
      txt-val = loc-to-src(ast-cse.val.l)
      branches-loc = ast-cse.branches-loc()
      val-loc = ast-cse.val.l
      [ED.error:
        [ED.para:
          ED.text("The "),
          ED.highlight(ED.text("cases expression"), [ED.locs: self.loc], pallet.get(0)),
          ED.text(" expects there to be a branch matching the value of the expression switched on. But no branches matched the value of "),
          ED.highlight(ED.text(txt-val), [ED.locs: ast-cse.val.l], pallet.get(1)),
          ED.text(":")],
        [ED.para: ED.embed(self.val)]]
    end,
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("No branches matched in the cases expression at"),
          draw-and-highlight(self.loc), ED.text("for value")],
        ED.embed(self.val)]
    end
  | no-branches-matched(loc, expression :: String) with:
    render-fancy-reason(self, loc-to-ast, loc-to-src, make-pallet):
      [ED.error:
        [ED.para:
          ED.text("The "),
          ED.text(self.expression),
          ED.text(" expression")],
        [ED.para:
          ED.code(
            ED.highlight(ED.text(loc-to-src(self.loc)), [ED.locs: self.loc], make-pallet(1).get(0)))],
        [ED.para:
          ED.text("expects that the condition of at least one branch be satisfied, but no branches matched.")]]
    end,
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("No branches matched in the"), ED.code(ED.text(self.expression)),
          ED.text("expression at"), draw-and-highlight(self.loc)]]
    end
  | internal-error(message, info-args) with:
    render-reason(self):
      [ED.error:
        [ED.para: ED.text("Internal error:"), ED.text(self.message)],
        [ED.para: ED.text("Relevant arguments:")],
        vert-list-values(self.info-args)]
    end
    
  | field-not-found(loc, obj, field :: String) with:
    render-fancy-reason(self, loc-to-ast, loc-to-src, make-pallet):
      pallet = make-pallet(2)
      ast-dot = loc-to-ast(self.loc).block.stmts.first
      txt-obj = loc-to-src(ast-dot.obj.l)
      obj-loc = ast-dot.obj.l
      fld-loc = ast-dot.field-loc()
      [ED.error:
        [ED.para:
          ED.text("The dot expression "),
          ED.code([ED.sequence:
            ED.highlight(ED.text(txt-obj), [ED.locs: obj-loc], pallet.get(0)),
            ED.text("."),
            ED.highlight(ED.text(self.field), [ED.locs: fld-loc], pallet.get(1))]),
          ED.text(" expects the value of the "),
          ED.highlight(ED.code(ED.text("object")),[ED.locs: obj-loc],pallet.get(0)),
          ED.text(" to have a "),
          ED.highlight(ED.text("field"), [ED.locs: fld-loc], pallet.get(1)),
          ED.text(" named "),
          ED.highlight(ED.code(ED.text(self.field)),[ED.locs: fld-loc], pallet.get(1)),
          ED.text(", but "),
          ED.highlight(ED.code(ED.text(self.field)), [ED.locs: fld-loc],pallet.get(1)),
          ED.text(" does not appear in the value of the "),
          ED.highlight(ED.code(ED.text("object")), [ED.locs: obj-loc],  pallet.get(0)),
          ED.text(":")],
        [ED.para: ED.embed(self.obj)]]
    end,
    
    render-reason(self, loc-to-ast):
      [ED.error:
        [ED.para:
          ED.text("Field"), ED.code(ED.text(self.field)), ED.text("not found in the lookup expression at"),
          draw-and-highlight(self.loc)],
        [ED.para: ED.text("The object was:")],
        ED.embed(self.obj)]
    end
  | lookup-non-object(loc, non-obj, field :: String) with:
    render-fancy-reason(self, loc-to-ast, loc-to-src, make-pallet):
      pallet = make-pallet(2)
      ast-dot = loc-to-ast(self.loc).block.stmts.first
      txt-obj = loc-to-src(ast-dot.obj.l)
      obj-loc = ast-dot.obj.l
      fld-loc = ast-dot.field-loc()
      [ED.error:
        [ED.para:
          ED.text("The dot expression "),
          ED.code([ED.sequence:
            ED.highlight(ED.text(txt-obj), [ED.locs: obj-loc], pallet.get(0)),
            ED.text("."),
            ED.highlight(ED.text(self.field), [ED.locs: fld-loc], pallet.get(1))]),
          ED.text(" expects the "),
          ED.highlight(ED.text("left hand side"),[ED.locs: obj-loc], pallet.get(0)),
          ED.text(" to evaluate to an object, but the value of "),
          ED.highlight(ED.text(txt-obj), [ED.locs: obj-loc], pallet.get(0)),
          ED.text(" was not an object, it was:")],
        [ED.para: ED.embed(self.non-obj)]]
    end,
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Tried to look up field"), ED.code(ED.text(self.field)),
          ED.text("on a non-object in the lookup expression at"),
          draw-and-highlight(self.loc)],
        [ED.para: ED.text("The non-object was:")],
        ED.embed(self.non-obj)]
    end
  | extend-non-object(loc, non-obj) with:
    render-fancy-reason(self, loc-to-ast, loc-to-src, make-pallet):
      pallet = make-pallet(2)
      ast-ext = loc-to-ast(self.loc).block.stmts.first
      txt-obj = loc-to-src(ast-ext.supe.l)
      obj-loc = ast-ext.supe.l
      fld-loc = ast-ext.field-loc()
      txt-fld = loc-to-src(fld-loc)
      [ED.error:
        [ED.para:
          ED.text("The object extension expression "),
          ED.code([ED.sequence:
            ED.highlight(ED.text(txt-obj), [ED.locs: obj-loc], pallet.get(0)),
            ED.text("."),
            ED.highlight(ED.text(txt-fld), [ED.locs: fld-loc], pallet.get(1))]),
          ED.text(" expects the "),
          ED.highlight(ED.text("left hand side"), [ED.locs: obj-loc], pallet.get(0)),
          ED.text(" to evaluate to an object, but the value of "),
          ED.highlight(ED.text(txt-obj), [ED.locs: obj-loc], pallet.get(0)),
          ED.text(" was not an object, it was:")],
        [ED.para: ED.embed(self.non-obj)]]
    end,
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Tried to extend a non-object in the expression at"),
          draw-and-highlight(self.loc)],
        [ED.para: ED.text("The non-object was:")],
        ED.embed(self.non-obj)]
    end
  | non-boolean-condition(loc, typ, value) with:
    # TODO : is this error even used?
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Expected"), ED.code(ED.text("true")), ED.text("or"), ED.code(ED.text("false")),
          ED.text("for the test in the"), ED.text(self.typ), ED.text("expression at"),
          draw-and-highlight(self.loc), ED.text(" but got:")],
        ED.embed(self.value)]
    end
  | non-boolean-op(loc, position, typ, value) with:
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Expected"), ED.code(ED.text("true")), ED.text("or"), ED.code(ED.text("false")),
          ED.text("for the"), ED.text(self.position), ED.text("argument in the"),
          ED.text(self.typ), ED.text("expression at"),
          draw-and-highlight(self.loc), ED.text(" but got:")],
        ED.embed(self.value)]
    end
  | generic-type-mismatch(val, typ :: String) with:
    render-fancy-reason(self, loc-to-ast, loc-to-src, make-pallet):
      ED.maybe-stack-loc(0, true,
        lam(loc):
          src = loc-to-src(loc)
          [ED.error:
            [ED.para:
              ED.text("The expression")],
            [ED.para:
              ED.code(ED.highlight(ED.text(src), [ED.locs: loc], make-pallet(1).get(0)))],
            [ED.para:
              ED.text("was expected to evaluate to a "),
              ED.embed(self.typ),
              ED.text(" but it evaluated to the non-"),
              ED.embed(self.typ),
              ED.text(" value:")],
            [ED.para:
              ED.embed(self.val)]]
        end,
        [ED.error:
          [ED.para-nospace: ED.text("Expected "), ED.embed(self.typ), ED.text(", but got "), ED.embed(self.val)]])
    end,
      
    render-reason(self):
      ED.maybe-stack-loc(0, true,
        lam(loc):
          [ED.error:
            [ED.para:
              ED.text("Expected to get a"), ED.embed(self.typ), ED.text("as an argument, but got this instead:")],
            ED.embed(self.val),
            [ED.para: ED.text("at"), draw-and-highlight(loc)]]
        end,
        [ED.error:
          [ED.para-nospace: ED.text("Expected "), ED.embed(self.typ), ED.text(", but got "), ED.embed(self.val)]])
    end
  | outside-numeric-range(val, low, high) with:
    render-reason(self):
      [ED.error:
        [ED.para-nospace: ED.text("expected a number between "), ED.embed(self.low),
          ED.text(" and "), ED.embed(self.high),
          ED.text(", but got"), ED.embed(self.val)]]
    end
  | num-string-binop-error(val1, val2, opname, opdesc, methodname) with:
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Invalid use of"), ED.code(ED.text(self.opname)), ED.text("for these values:")],
        [ED.para: ED.embed(self.val1)],
        [ED.para: ED.embed(self.val2)],
        ED.text(self.opdesc + " requires:"),
        [ED.bulleted:
          ED.text("Two numbers,"),
          ED.text("Two strings, or"),
          [ED.para:
            ED.text("A left-hand operand that has a"), ED.code(ED.text(self.methodname)), ED.text("method")]]]
    end
  | numeric-binop-error(val1, val2, opname, methodname) with:
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Invalid use of"), ED.code(ED.text(self.opname)), ED.text("for these values:")],
        [ED.para: ED.embed(self.val1)],
        [ED.para: ED.embed(self.val2)],
        ED.text("Either:"),
        [ED.bulleted:
          ED.text("Both arguments must be numbers, or"),
          [ED.para:
            ED.text("The left operand must have a"), ED.code(ED.text(self.methodname)), ED.text("method")]]]
    end
  | cases-arity-mismatch(branch-loc, num-args, actual-arity, cases-loc) with:
    render-fancy-reason(self, loc-to-ast, loc-to-src, make-pallet):
      fun ed-args(n):
        [ED.sequence:
          ED.embed(n),
          ED.text(if n == 1: " argument"
                  else:      " arguments";)]
      end
      pallet = make-pallet(4)
      ast-cases = loc-to-ast(self.cases-loc).block.stmts.first
      src-branch = loc-to-src(self.branch-loc)
      ast-branch = ast-cases.branches.find(lam(b): b.l.start-line == self.branch-loc.start-line;).value
      [ED.error:
        [ED.para:
          ED.text("The cases branch pattern")],
        [ED.para:
          ED.code([ED.sequence:
            ED.highlight(ED.text(ast-branch.name), [ED.locs: ast-branch.pat-loc], pallet.get(2)),
            ED.text("("),
            ED.h-sequence(
              ast-branch.args.map(
                lam(arg):ED.highlight(ED.text(loc-to-src(arg.l)), [ED.locs: arg.l], pallet.get(3));),
              ","),
            ED.text(")")])],
        [ED.para:
          ED.text("expects that the pattern for the "),
          ED.code(ED.highlight(ED.text(ast-branch.name), [ED.locs: ast-branch.pat-loc], pallet.get(2))),
          ED.text(" branch has exactly the same number of arguments as the"),
          ED.code(ED.text(ast-branch.name)),
          ED.text(" variant of "),
          ED.embed(ast-cases.typ.id),
          ED.text(" accepts.")],
        [ED.para:
          ED.text("The cases pattern for "),
          ED.code(ED.highlight(ED.text(ast-branch.name), [ED.locs: ast-branch.pat-loc], pallet.get(2))),
          ED.text(" accepts "),
          ED.highlight(ed-args(self.num-args), ast-branch.args.map(_.l), pallet.get(3)),
          ED.text(" but the "),
          ED.code(ED.text(ast-branch.name)),
          ED.text(" variant of the "),
          ED.embed(ast-cases.typ.id),
          ED.text(" datatype accepts "),
          ed-args(self.num-args)]]
    end,
    
    render-reason(self):
      [ED.error:
        if self.num-args < self.actual-arity:
          [ED.para:
            ED.text("The cases branch at"), draw-and-highlight(self.branch-loc),
            ED.text("expects only"), ED.embed(self.num-args),
            if self.num-args == 1: ED.text("argument,") else: ED.text("arguments,") end,
            ED.text("but the actual value has"), ED.embed(self.actual-arity),
            if self.actual-arity == 1: ED.text("field") else: ED.text("fields") end]
        else:
          [ED.para:
            ED.text("The cases branch at"), draw-and-highlight(self.branch-loc),
            ED.text("expects"), ED.embed(self.num-args),
            if self.num-args == 1: ED.text("argument,") else: ED.text("arguments,") end,
            ED.text("but the actual value has only"), ED.embed(self.actual-arity),
            if self.actual-arity == 1: ED.text("field") else: ED.text("fields") end]
        end]
    end
  | cases-singleton-mismatch(branch-loc, should-be-singleton :: Boolean, casesLoc) with:
    render-reason(self):
      if self.should-be-singleton:
        [ED.error:
          [ED.para:
            ED.text("The cases branch at"), draw-and-highlight(self.branch-loc),
            ED.text("has an argument list, but the variant is a singleton.")]]
      else:
        [ED.error:
          [ED.para:
            ED.text("The cases branch at"), draw-and-highlight(self.branch-loc),
            ED.text("doesn't have an argument list, but the variant is not a singleton.")]]
      end
    end
  | arity-mismatch(fun-def-loc, fun-def-arity, fun-app-args) with:
    render-fancy-reason(self, loc-to-ast, loc-to-src, make-pallet):
      pallet = make-pallet(4)
      fun ed-args(n):
        [ED.sequence:
          ED.embed(n),
          ED.text(if n == 1: " argument"
                  else:      " arguments";)]
      end
      fun ed-were(n):
        ED.text(if n == 1: "was" else: "were";)
      end
      fun-app-arity = self.fun-app-args.length()
      ED.maybe-stack-loc(1, true,
        lam(fun-app-loc):
          fun-app-ast     = loc-to-ast(fun-app-loc).block.stmts.first
          fun-app-fun-loc = fun-app-ast._fun.l
          fun-app-fun-src = loc-to-src(fun-app-fun-loc)
          fun-app-arg-loc = fun-app-ast.args-loc()
          fun-app-arg-src = loc-to-src(fun-app-arg-loc)
          
          if self.fun-def-loc.is-builtin():
            [ED.error: ED.text("Batman")]
          else:
            fun-def-ast = loc-to-ast(self.fun-def-loc).block.stmts.first
            [ED.error:
              [ED.para:
                ED.text("The function application expression ")],
              [ED.para:
                ED.code([ED.sequence:
                  ED.highlight(ED.text(fun-app-fun-src), [ED.locs: fun-app-fun-loc], pallet.get(0)),
                  ED.text("("),
                  ED.h-sequence(
                    fun-app-ast.args.map(
                      lam(arg):ED.highlight(ED.text(loc-to-src(arg.l)), [ED.locs: arg.l], pallet.get(1));),
                    ","),
                  ED.text(")")])],
              [ED.para:
                ED.text("expects the "),
                ED.highlight(ED.text("applicant"), [ED.locs: fun-app-fun-loc], pallet.get(0)),
                ED.text(" to evaluate to a function that accepts exactly the same number of arguments as are given to it.")],
              [ED.para:
                ED.text("The "),
                ED.highlight(ED.text("applicant"), [ED.locs: fun-app-fun-loc], pallet.get(0)),
                ED.text(" evaluated to a "),
                ED.highlight(ED.text("function"), [ED.locs: self.fun-def-loc], pallet.get(2)),
                ED.text(" accepting exactly "),
                ED.highlight(ed-args(self.fun-def-arity), fun-def-ast.args.map(_.l), pallet.get(3)),
                ED.text(",")],
              [ED.para:
                ED.code([ED.sequence:
                  ED.highlight(ED.text(fun-def-ast.name), [ED.locs: self.fun-def-loc], pallet.get(2)),
                  ED.text("("),
                  ED.h-sequence(fun-def-ast.args.map(
                    lam(arg):
                      ED.highlight(ED.text("⬜"), [ED.locs: arg.l], pallet.get(3));),","),
                  ED.text(")")])],
              [ED.para:
                ED.text("but the "),
                ED.highlight(ED.text("applicant"), [ED.locs: fun-app-fun-loc], pallet.get(0)),
                ED.text(" had "),
                ED.highlight(ed-args(fun-app-arity), fun-app-ast.args.map(_.l), pallet.get(1)),
                ED.text(" applied to it:")],
              vert-list-values(self.fun-app-args)]
          end
        end,
        [ED.error: ED.text("Batman")])
    end,
    render-reason(self):
      num-args = self.fun-app-args.length()
      this-str = if num-args == 1: "this" else: "these" end
      arg-str = if num-args == 1: "argument:" else: "arguments:" end
      exp-arg-str = if self.fun-def-arity == 1: "argument" else: "arguments" end
      
      ED.maybe-stack-loc(0, true,
        lam(caller-loc):
          if self.fun-def-loc.is-builtin():
            [ED.error:
              [ED.para: ED.text("Expected to get"), ED.embed(self.fun-def-arity), ED.text(exp-arg-str + " at")],
              draw-and-highlight(caller-loc),
              [ED.para: ED.text("but got " + this-str), ED.embed(num-args), ED.text(arg-str)],
              vert-list-values(self.fun-app-args)]
          else:
            [ED.error:
              [ED.para: ED.text("Expected to get"), ED.embed(self.fun-def-arity),
                ED.text(exp-arg-str + " when calling the function at")],
              draw-and-highlight(self.fun-def-loc),
              [ED.para: ED.text("from")],
              draw-and-highlight(caller-loc),
              [ED.para: ED.text("but got " + this-str), ED.embed(num-args), ED.text(arg-str)],
              vert-list-values(self.fun-app-args)]
          end
        end,
        [ED.error:
          [ED.para: ED.text("Expected to get"), ED.embed(self.fun-def-arity), ED.text(exp-arg-str + " at")],
          draw-and-highlight(self.fun-def-loc),
          [ED.para: ED.text("but got " + this-str), ED.embed(num-args), ED.text(arg-str)],
          vert-list-values(self.fun-app-args)])
    end
  | non-function-app(loc, non-fun-val) with:
    render-fancy-reason(self, loc-to-ast, loc-to-src, make-pallet):
      fun ed-args(n):
        [ED.sequence:
          ED.embed(n),
          ED.text(if n == 1: " argument"
                  else:      " arguments";)]
      end
      pallet = make-pallet(2)
      app-ast = loc-to-ast(self.loc).block.stmts.first
      fun-loc = app-ast._fun.l
      fun-src = loc-to-src(fun-loc)
      arg-loc = app-ast.args-loc()
      arg-src = loc-to-src(arg-loc)
      num-args = app-ast.args.length()
      [ED.error:
        [ED.para:
          ED.text("The function application expression ")],
        [ED.para:
          ED.code([ED.sequence:
            ED.highlight(ED.text(fun-src), [ED.locs: fun-loc], pallet.get(0)),
            ED.text("("),
            ED.h-sequence(
              app-ast.args.map(
                lam(arg):ED.highlight(ED.text(loc-to-src(arg.l)), [ED.locs: arg.l], pallet.get(1));),
              ","),
            ED.text(")")])],
        [ED.para:
          ED.text("expects the applicant"),
          ED.highlight(ED.code(ED.text(fun-src)), [ED.locs: fun-loc], pallet.get(0)),
          ED.text(" to evaluate to a function accepting "),
          ED.highlight(ed-args(num-args), app-ast.args.map(_.l), pallet.get(1)),
          ED.text(", but the expression "),
          ED.highlight(ED.code(ED.text(fun-src)), [ED.locs: fun-loc], pallet.get(0)),
          ED.text(" evaluated to the non-function value:")],
        [ED.para: ED.embed(self.non-fun-val)]]
    end,
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Expected a function in the application expression at"),
          draw-and-highlight(self.loc), ED.text(" but got:")],
        ED.embed(self.non-fun-val)]
    end
  | bad-app(loc, fun-name :: String, message :: String, arg-position :: Number, arg-val) with:
    render-reason(self):
      [ED.error: ED.text(tostring(self))]
    end
  | uninitialized-id(loc, name :: String) with:
    render-reason(self):
      [ED.error:
        [ED.para: ED.text("The name"), ED.code(ED.text(self.name)), ED.text("was used at"),
          draw-and-highlight(self.loc), ED.text("before it was defined.")]]
    end
  | module-load-failure(names) with: # names is List<String>
    render-reason(self):
      [ED.error:
        [ED.para:
          if self.names.length() == 1: ED.text("The following module failed to load:")
          else:                        ED.text("The following modules failed to load:")
          end],
        ED.h-sequence(self.names.map(ED.text), ", ")]
    end
  | invalid-array-index(method-name :: String, array, index :: Number, reason :: String) with: # array is Array
    render-reason(self):
      ED.maybe-stack-loc(0, true,
        lam(loc):
          [ED.error:
            [ED.para: ED.text("Invalid array index"), ED.code(ED.embed(self.index)),
              ED.text("around the function call at"), draw-and-highlight(loc),
              ED.text("because:"), ED.text(self.reason)]]
        end,
        [ED.error:
          [ED.para: ED.text("Invalid array index"), ED.code(ED.embed(self.index)),
            ED.text("because:"), ED.text(self.reason)]])
    end
  | equality-failure(reason :: String, value1, value2) with:
    render-reason(self):
      [ED.error:
        [ED.para: ED.text("Attempted to compare the following two incomparable values:")],
        ED.embed(self.value1),
        ED.embed(self.value2),
        [ED.para: ED.text(self.reason)]]
    end

  | user-break with:
    render-reason(self):
      [ED.error: ED.text("Program stopped by user")]
    end

  | user-exception(value :: Any) with:
    render-reason(self): ED.embed(self.value) end
end

data ParseError:
  | parse-error-next-token(loc, next-token :: String) with:
    render-reason(self):
      missing =
        [ED.error:
          [ED.para: ED.text("The program is missing something")],
          [ED.para-nospace:
            ED.text("Look carefully before the "), ED.styled(ED.text("highlighted text"), 'error-highlight'),
            ED.text(".  Is something missing just before it?"),
            ED.text("  Common missing items are colons ("), ED.code(ED.text(":")),
            ED.text("), commas ("), ED.code(ED.text(",")), ED.text("), string markers ("),
            ED.code(ED.text("\"")), ED.text("), and keywords.")],
          [ED.para: ED.styled(ED.text("Usually, inserting the missing item will fix this error."), "hint")]]
      extra =
        [ED.error:
          [ED.para: ED.text("The program contains something extra")],
          [ED.para-nospace:
            ED.text("Look carefully before the "), ED.styled(ED.text("highlighted text"), 'error-highlight'),
            ED.text(".  Does it contains something extra?"),
            ED.text("  A common source of errors is typing too much text or in the wrong order.")],
          [ED.para:
            ED.styled(ED.text("Usually, removing the extra item will fix this error."), "hint"),
            ED.text("However, you may have meant to keep this text, so think before you delete!")]]
      [ED.error:
        [ED.para: ED.text("Pyret didn't understand your program around"), draw-and-highlight(self.loc)],
        [ED.opt:
          [ED.para: ED.text("Typical reasons for getting this error are")],
          [ED.bulleted: missing, extra]]]
    end
  | parse-error-eof(loc) with:
    render-reason(self):
      [ED.error: [ED.para:
          ED.text("Pyret didn't understand the very end of your program."),
          ED.text("You may be missing an \"end\", or closing punctuation like \")\" or \"]\" right at the end.")]]
    end
  | parse-error-unterminated-string(loc) with:
    render-reason(self):
      [ED.error: [ED.para-nospace:
          ED.text("Pyret thinks your program has an incomplete string literal around "),
          draw-and-highlight(self.loc),
          ED.text("; you may be missing closing punctuation.")]]
    end
  | parse-error-bad-operator(loc) with:
    render-reason(self):
      [ED.error: [ED.para-nospace:
          ED.text("The operator at "),
          draw-and-highlight(self.loc),
          ED.text(" has no surrounding whitespace.")]]
    end
  | parse-error-bad-number(loc) with:
    render-reason(self):
      [ED.error: [ED.para-nospace:
          ED.text("Pyret thinks your program probably has a number at "),
          draw-and-highlight(self.loc),
          ED.text("; number literals in Pyret require at least one digit before the decimal point.")]]
    end
  | empty-block(loc) with:
    _tostring(self, shadow tostring):
      "Empty block at " + self.loc.format(true)
    end
  | bad-block-stmt(loc) with:
    _tostring(self, shadow tostring):
      "Expected a val binding or an expression, but got something else " + self.loc.format(true)
    end
  | bad-check-block-stmt(loc) with:
    _tostring(self, shadow tostring):
      "Expected a val binding or an expression, but got something else " + self.loc.format(true)
    end
  | fun-missing-colon(loc) with:
    _tostring(self, shadow tostring): "fun-missing-colon: " + self.loc.format(true) end
  | fun-missing-end(loc) with:
    _tostring(self, shadow tostring): "fun-missing-end: " + self.loc.format(true) end
  | args-missing-comma(loc) with:
    _tostring(self, shadow tostring): "args-missing-comma: " + self.loc.format(true) end
  | app-args-missing-comma(loc) with:
    _tostring(self, shadow tostring): "app-args-missing-comma: " + self.loc.format(true) end
  | missing-end(loc)
  | missing-comma(loc)
sharing:
  render-reason(self):
    ED.text(self._tostring(tostring))
  end
end
