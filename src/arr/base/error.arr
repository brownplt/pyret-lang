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
    render-fancy-reason(self, loc-to-ast, loc-to-src, make-palette):
      palette = make-palette(2)
      ast-cse = loc-to-ast(self.loc).block.stmts.first
      txt-val = loc-to-src(ast-cse.val.l)
      branches-loc = ast-cse.branches-loc()
      val-loc = ast-cse.val.l
      [ED.error:
        [ED.para:
          ED.text("The "),
          ED.highlight(ED.text("cases expression"), [ED.locs: self.loc], palette.get(0)),
          ED.text(" expects there to always be a branch matching the value of the "),
          ED.highlight(ED.text("argument"),[ED.locs: ast-cse.val.l], palette.get(1)),
          ED.text(".")],
        [ED.para:
          ED.text("The value of the "),
          ED.highlight(ED.text("argument"),[ED.locs: ast-cse.val.l], palette.get(1)),
          ED.text(" was "),
          ED.highlight(ED.text(txt-val), [ED.locs: ast-cse.val.l], palette.get(1)),
          ED.text(":")],
         ED.embed(self.val)]
    end,
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("No branches matched in the cases expression at"),
          draw-and-highlight(self.loc), ED.text("for value")],
        ED.embed(self.val)]
    end
  | no-branches-matched(loc, expression :: String) with:
    render-fancy-reason(self, loc-to-ast, loc-to-src, make-palette):
      [ED.error:
        [ED.para:
          ED.text("The "),
          ED.text(self.expression),
          ED.text(" expression")],
        [ED.para:
          ED.code(
            ED.highlight(ED.text(loc-to-src(self.loc)), [ED.locs: self.loc], make-palette(1).get(0)))],
        [ED.para:
          ED.text("expects that the condition of at least one branch be satisfied. No branches were satisfied.")]]
    end,
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("No branches matched in the"), ED.code(ED.text(self.expression)),
          ED.text("expression at"), draw-and-highlight(self.loc)]]
    end
  | internal-error(message, info-args) with:
    render-fancy-reason(self, loc-to-ast, loc-to-src, make-palette):
      self.render-reason()
    end,
    render-reason(self):
      [ED.error:
        [ED.para: ED.text("Internal error:"), ED.text(self.message)],
        [ED.para: ED.text("Relevant arguments:")],
        vert-list-values(self.info-args)]
    end
  | field-not-found(loc, obj, field :: String) with:
    render-fancy-reason(self, loc-to-ast, loc-to-src, make-palette):
      palette = make-palette(2)
      ast-dot = loc-to-ast(self.loc).block.stmts.first
      txt-obj = loc-to-src(ast-dot.obj.l)
      obj-loc = ast-dot.obj.l
      fld-loc = ast-dot.field-loc()
      obj-col = palette.get(0)
      fld-col = palette.get(1)
      [ED.error:
        [ED.para:
          ED.text("The field lookup expression ")],
         ED.code([ED.sequence:
          ED.highlight(ED.text(txt-obj), [ED.locs: obj-loc], obj-col),
          ED.text("."),
          ED.highlight(ED.text(self.field), [ED.locs: fld-loc], fld-col)]),
        [ED.para:
          ED.text(" expects the value of the "),
          ED.highlight(ED.text("object"),[ED.locs: obj-loc], obj-col),
          ED.text(" to have a "),
          ED.highlight(
            [ED.sequence:
              ED.text("field named "),
              ED.code(ED.text(self.field))],
            [ED.locs: fld-loc], fld-col),
          ED.text(".")],
        [ED.para:
          ED.text("The value of the "),
          ED.highlight(ED.text("object"),[ED.locs: obj-loc], obj-col),
          ED.text(" is:")],
         ED.embed(self.obj)]
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
    render-fancy-reason(self, loc-to-ast, loc-to-src, make-palette):
      palette = make-palette(2)
      ast-dot = loc-to-ast(self.loc).block.stmts.first
      txt-obj = loc-to-src(ast-dot.obj.l)
      obj-loc = ast-dot.obj.l
      fld-loc = ast-dot.field-loc()
      obj-col = palette.get(0)
      fld-col = palette.get(1)
      [ED.error:
        [ED.para:
          ED.text("The field lookup expression ")],
         ED.code([ED.sequence:
          ED.highlight(ED.text(txt-obj), [ED.locs: obj-loc], obj-col),
          ED.text("."),
          ED.highlight(ED.text(self.field), [ED.locs: fld-loc], fld-col)]),
        [ED.para:
          ED.text(" expects the "),
          ED.highlight(ED.text("left hand side"), [ED.locs: obj-loc], obj-col),
          ED.text(" to evaluate to an object.")],
        [ED.para:
          ED.text("The "),
          ED.highlight(ED.text("left hand side"), [ED.locs: obj-loc], obj-col),
          ED.text(" evaluated to a non-object value:")],
         ED.embed(self.non-obj)]
    end,
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The field lookup expression at "),
          ED.loc(self.loc),
          ED.text(" expects the left hand side to evaluate to an object.")],
        [ED.para:
          ED.text("The left hand side"),
          ED.text(" evaluated to a non-object value:")],
         ED.embed(self.non-obj)]
    end
  | extend-non-object(loc, non-obj) with:
    render-fancy-reason(self, loc-to-ast, loc-to-src, make-palette):
      palette = make-palette(2)
      ast-ext = loc-to-ast(self.loc).block.stmts.first
      txt-obj = loc-to-src(ast-ext.supe.l)
      obj-loc = ast-ext.supe.l
      fld-loc = ast-ext.field-loc()
      txt-fld = loc-to-src(fld-loc)
      obj-col = palette.get(0)
      fld-col = palette.get(1)
      [ED.error:
        [ED.para:
          ED.text("The object extension expression ")],
         ED.code([ED.sequence:
          ED.highlight(ED.text(txt-obj), [ED.locs: obj-loc], obj-col),
          ED.text("."),
          ED.highlight(ED.text(self.field), [ED.locs: fld-loc], fld-col)]),
        [ED.para:
          ED.text(" expects the "),
          ED.highlight(ED.text("left hand side"), [ED.locs: obj-loc], obj-col),
          ED.text(" to evaluate to an object.")],
        [ED.para:
          ED.text("The "),
          ED.highlight(ED.text("left hand side"), [ED.locs: obj-loc], obj-col),
          ED.text(" evaluated to a non-object value:")],
         ED.embed(self.non-obj)]
    end,
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The object extension expression at "),
          ED.loc(self.loc),
          ED.text(" expects the left hand side to evaluate to an object.")],
        [ED.para:
          ED.text("The left hand side"),
          ED.text(" evaluated to a non-object value:")],
         ED.embed(self.non-obj)]
    end
  | non-boolean-condition(loc, typ, value) with:
    # TODO : is this error even used?
    render-fancy-reason(self, loc-to-ast, loc-to-src, make-palette):
      self.render-reason()
    end,
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Expected"), ED.code(ED.text("true")), ED.text("or"), ED.code(ED.text("false")),
          ED.text("for the test in the"), ED.text(self.typ), ED.text("expression at"),
          draw-and-highlight(self.loc), ED.text(" but got:")],
        ED.embed(self.value)]
    end
  | non-boolean-op(loc, position, typ, value) with:
    render-fancy-reason(self, loc-to-ast, loc-to-src, make-palette):
      self.render-reason()
    end,
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
    render-fancy-reason(self, loc-to-ast, loc-to-src, make-palette):
      ED.maybe-stack-loc(0, true,
        lam(loc):
          src = loc-to-src(loc)
          [ED.error:
            [ED.para:
              ED.text("The expression")],
            [ED.para:
              ED.code(ED.highlight(ED.text(src), [ED.locs: loc], make-palette(1).get(0)))],
            [ED.para:
              ED.text("was expected to evaluate to a "),
              ED.embed(self.typ),
              ED.text(". It evaluated to the non-"),
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
  | num-string-binop-error(val1, val2, opname, opdesc, methodname) with:
    render-fancy-reason(self, loc-to-ast, loc-to-src, make-palette):
      # TODO: breaks on multi-operator expression chains
      palette = make-palette(3)
      ED.maybe-stack-loc(0, true,
        lam(binop-loc):
          binop-ast = loc-to-ast(binop-loc).block.stmts.first
          [ED.error:
            [ED.para:
              ED.text("The binary "),
              ED.highlight(ED.text(self.opdesc),[ED.locs: binop-ast.op-l], palette.get(1)),
              ED.text(" operator expression ")],
            [ED.para:
              ED.code([ED.sequence:
                ED.highlight(ED.text(loc-to-src(binop-ast.left.l)),  [ED.locs: binop-ast.left.l], palette.get(0)),
                ED.text(" "),
                ED.highlight(ED.text(self.opname),                   [ED.locs: binop-ast.op-l], palette.get(1)),
                ED.text(" "),
                ED.highlight(ED.text(loc-to-src(binop-ast.right.l)), [ED.locs: binop-ast.right.l], palette.get(2))])],
            [ED.para:
              ED.text("expects to be given:"),
              [ED.bulleted:
                ED.text("two Numbers,"),
                ED.text("two Strings, or"),
                [ED.sequence: 
                  ED.text("a "),
                  ED.highlight(ED.text("left operand"), [ED.locs: binop-ast.left.l], palette.get(0)),
                  ED.text(" that has a method named "), 
                  ED.code(ED.text(self.methodname))]]],
            [ED.para:
              ED.text("However, the expression's "),
              ED.highlight(ED.text("left operand"), [ED.locs: binop-ast.left.l], palette.get(0)),
              ED.text(" evaluated to")],
            ED.embed(self.val1),
            [ED.para:
              ED.text("and its "),
              ED.highlight(ED.text("right operand"), [ED.locs: binop-ast.right.l], palette.get(2)),
              ED.text(" evaluated to")],
            ED.embed(self.val2)]
        end, 
        self.render-reason())
    end,
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
    render-fancy-reason(self, loc-to-ast, loc-to-src, make-palette):
      # TODO: breaks on multi-operator expression chains
      palette = make-palette(3)
      ED.maybe-stack-loc(0, true,
        lam(binop-loc):
          binop-ast = loc-to-ast(binop-loc).block.stmts.first
          [ED.error:
            [ED.para:
              ED.text("The binary "),
              ED.highlight(ED.text(self.opname),[ED.locs: binop-ast.op-l], palette.get(1)),
              ED.text(" operator expression ")],
            [ED.para:
              ED.code([ED.sequence:
                ED.highlight(ED.text(loc-to-src(binop-ast.left.l)),  [ED.locs: binop-ast.left.l], palette.get(0)),
                ED.text(" "),
                ED.highlight(ED.text(self.opname),                   [ED.locs: binop-ast.op-l], palette.get(1)),
                ED.text(" "),
                ED.highlight(ED.text(loc-to-src(binop-ast.right.l)), [ED.locs: binop-ast.right.l], palette.get(2))])],
            [ED.para:
              ED.text("expects to be given:"),
              [ED.bulleted:
                ED.text("two Numbers, or"),
                [ED.sequence: 
                  ED.text("a "),
                  ED.highlight(ED.text("left operand"), [ED.locs: binop-ast.left.l], palette.get(0)),
                  ED.text(" that has a method named "), 
                  ED.code(ED.text(self.methodname))]]],
            [ED.para:
              ED.text("However, the expression's "),
              ED.highlight(ED.text("left operand"), [ED.locs: binop-ast.left.l], palette.get(0)),
              ED.text(" evaluated to")],
            ED.embed(self.val1),
            [ED.para:
              ED.text("and its "),
              ED.highlight(ED.text("right operand"), [ED.locs: binop-ast.right.l], palette.get(2)),
              ED.text(" evaluated to")],
            ED.embed(self.val2)]
        end, 
        self.render-reason())
    end,
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
    render-fancy-reason(self, loc-to-ast, loc-to-src, make-palette):
      palette = make-palette(4)
      ast-cases = loc-to-ast(self.cases-loc).block.stmts.first
      src-branch = loc-to-src(self.branch-loc)
      ast-branch = ast-cases.branches.find(lam(b): b.l.start-line == self.branch-loc.start-line;).value
      [ED.error:
        [ED.para:
          ED.text("The cases branch pattern")],
        [ED.para:
          ED.code([ED.sequence:
            ED.highlight(ED.text(ast-branch.name), [ED.locs: ast-branch.pat-loc], palette.get(2)),
            cases(Any) ast-branch:
              | s-cases-branch(loc, pat-loc, name, args, _) =>
                [ED.sequence:
                  ED.text("("),
                  ED.h-sequence(
                    ast-branch.args.map(
                      lam(arg):ED.highlight(ED.text(loc-to-src(arg.l)), [ED.locs: arg.l], palette.get(3));),
                    ","),
                  ED.text(")")]
              | s-singleton-cases-branch(loc, pat-loc, name, _) => ED.text("")
            end])],
        [ED.para:
          ED.text("expects that the pattern for the "),
          ED.code(ED.highlight(ED.text(ast-branch.name), [ED.locs: ast-branch.pat-loc], palette.get(2))),
          ED.text(" branch has exactly the same number of "),
          ED.highlight(ED.text("arguments"), ast-branch.args.map(_.l), palette.get(3)),
          ED.text(" as the "),
          ED.code(ED.text(ast-branch.name)),
          ED.text(" variant of "),
          ED.embed(ast-cases.typ.id),
          ED.text(" accepts.")],
        [ED.para:
          ED.text("The cases pattern for "),
          ED.code(ED.highlight(ED.text(ast-branch.name), [ED.locs: ast-branch.pat-loc], palette.get(2))),
          ED.text(" accepts "),
          cases(Any) ast-branch:
            | s-cases-branch(_, _, _, args, _) =>
                ED.highlight(ED.ed-args(self.num-args),args.map(_.l), palette.get(3))
            | s-singleton-cases-branch(_, _, _, _) => ED.ed-args(self.num-args)
          end,
          ED.text(". The "),
          ED.code(ED.text(ast-branch.name)),
          ED.text(" variant of the "),
          ED.embed(ast-cases.typ.id),
          ED.text(" datatype accepts "),
          ED.ed-args(self.actual-arity)]]
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
  | arity-mismatch(fun-def-loc, fun-def-arity, fun-app-args) with:
    render-fancy-reason(self, loc-to-ast, loc-to-src, make-palette):
      palette = make-palette(4)
      fun-app-arity = self.fun-app-args.length()
      arity-helper = lam(n, els):
        ED.maybe-stack-loc(
          n,
          true,
          lam(fun-app-loc):
            fun-app-ast     = loc-to-ast(fun-app-loc).block.stmts.first
            fun-app-fun-loc = fun-app-ast._fun.l
            fun-app-fun-src = loc-to-src(fun-app-fun-loc)
            fun-app-arg-loc = fun-app-ast.args-loc()
            fun-app-arg-src = loc-to-src(fun-app-arg-loc)
            [ED.error:
              [ED.para:
                ED.text("The function application expression ")],
              [ED.para:
                ED.code([ED.sequence:
                  ED.highlight(ED.text(fun-app-fun-src), [ED.locs: fun-app-fun-loc], palette.get(0)),
                  ED.text("("),
                  ED.h-sequence(
                    fun-app-ast.args.map(
                      lam(arg):ED.highlight(ED.text(loc-to-src(arg.l)), [ED.locs: arg.l], palette.get(1));),
                    ","),
                  ED.text(")")])],
              [ED.para:
                ED.text("expects the "),
                ED.highlight(ED.text("applicant"), [ED.locs: fun-app-fun-loc], palette.get(0)),
                ED.text(" to evaluate to a function that accepts exactly the same number of arguments as are given to it.")],
              [ED.para:
                ED.text("The "),
                ED.highlight(ED.text("applicant"), [ED.locs: fun-app-fun-loc], palette.get(0)),
                ED.text(" evaluated to a "),
                ED.highlight(ED.text("function"), [ED.locs: self.fun-def-loc], palette.get(2)),
                ED.text(" accepting exactly "),
                if self.fun-def-loc.is-builtin():
                  ED.ed-args(self.fun-def-arity)
                else:
                  cases(Any) loc-to-ast(self.fun-def-loc).block.stmts.first:
                    | s-fun(_,_,_,args,_,_,_,_) =>
                      ED.highlight(ED.ed-args(self.fun-def-arity), args.map(_.l), palette.get(3))
                    | else => 
                      ED.ed-args(self.fun-def-arity)
                  end
                end,
                ED.text(".")],
              [ED.para:
                ED.text("The "),
                ED.highlight(ED.text("applicant"), [ED.locs: fun-app-fun-loc], palette.get(0)),
                ED.text(" had "),
                ED.highlight(ED.ed-args(fun-app-arity), fun-app-ast.args.map(_.l), palette.get(1)),
                ED.text(" applied to it.")]]
          end, els);
        arity-helper(1, arity-helper(0, self.render-reason()))
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
    render-fancy-reason(self, loc-to-ast, loc-to-src, make-palette):
      palette = make-palette(2)
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
            ED.highlight(ED.text(fun-src), [ED.locs: fun-loc], palette.get(0)),
            ED.text("("),
            ED.h-sequence(
              app-ast.args.map(
                lam(arg):ED.highlight(ED.text(loc-to-src(arg.l)), [ED.locs: arg.l], palette.get(1));),
              ","),
            ED.text(")")])],
        [ED.para:
          ED.text("expects the "),
          ED.highlight(ED.text("applicant"), [ED.locs: fun-loc], palette.get(0)),
          ED.text(" to evaluate to a function value accepting "),
          ED.highlight(ED.ed-args(num-args), app-ast.args.map(_.l), palette.get(1)),
          ED.text(".")],
        [ED.para:
          ED.text("The expression "),
          ED.highlight(ED.code(ED.text(fun-src)), [ED.locs: fun-loc], palette.get(0)),
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
    render-fancy-reason(self, loc-to-ast, loc-to-src, make-palette):
      self.render-reason()
    end,
    render-reason(self):
      [ED.error: ED.text(tostring(self))]
    end
  | uninitialized-id(loc, name) with:
    render-fancy-reason(self, loc-to-ast, loc-to-src, make-palette):
      palette = make-palette(1)
      [ED.error:
        [ED.para:
          ED.text("The identifier "), 
          ED.code(ED.highlight(ED.text(self.name), [ED.locs: self.loc], palette.get(0))),
          ED.text(" is unbound. Although it has been previously defined, it is being "),
          ED.highlight(ED.text("used"), [ED.locs: self.loc], palette.get(0)),
          ED.text(" before it has been is initialized to a value.")]]
    end,
    render-reason(self):
      [ED.error:
        [ED.para: ED.text("The name"), ED.code(ED.text(self.name)), ED.text("was used at"),
          draw-and-highlight(self.loc), ED.text("before it was defined.")]]
    end
  | module-load-failure(names) with: # names is List<String>
    render-fancy-reason(self, loc-to-ast, loc-to-src, make-palette):
      self.render-reason()
    end,
    render-reason(self):
      [ED.error:
        [ED.para:
          if self.names.length() == 1: ED.text("The following module failed to load:")
          else:                        ED.text("The following modules failed to load:")
          end],
        ED.h-sequence(self.names.map(ED.text), ", ")]
    end
  | invalid-array-index(method-name :: String, array, index :: Number, reason :: String) with: # array is Array
    render-fancy-reason(self, loc-to-ast, loc-to-src, make-palette):
      palette = make-palette(1)
      ED.maybe-stack-loc(0, true,
        lam(loc):
          ast-dot = loc-to-ast(loc).block.stmts.first
          dot-ast = ast-dot._fun
          [ED.error:
            [ED.para: 
              ED.text("The array interaction "),
              ED.code(ED.highlight(ED.text(self.method-name), [ED.locs: loc], palette.get(0)))],
            [ED.para:
              ED.code(ED.highlight(ED.text(loc-to-src(loc)), [ED.locs: loc], palette.get(0)))],
            [ED.para:
              ED.text("expects that the index passed to it is an integer within the bounds of the array. "),
              ED.embed(self.index),
              ED.text(" "),
              ED.text(self.reason)]]
        end,
        [ED.error:
          [ED.para: ED.text("Invalid array index"), ED.code(ED.embed(self.index)),
            ED.text("because:"), ED.text(self.reason)]])
    end,
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
    render-fancy-reason(self, loc-to-ast, loc-to-src, make-palette):
      self.render-reason()
    end,
    # TODO
    render-reason(self):
      [ED.error:
        [ED.para: ED.text("Attempted to compare the following two incomparable values:")],
        ED.embed(self.value1),
        ED.text(" "),
        ED.embed(self.value2),
        [ED.para: ED.text(self.reason)]]
    end

  | user-break with:
    render-fancy-reason(self, loc-to-ast, loc-to-src, make-palette):
      self.render-reason()
    end,
    render-reason(self):
      [ED.error: ED.text("Program stopped by user")]
    end

  | user-exception(value :: Any) with:
    render-fancy-reason(self, loc-to-ast, loc-to-src, make-palette):
      self.render-reason()
    end,
    render-reason(self): ED.embed(self.value) end
end

data ParseError:
  | parse-error-next-token(loc, next-token :: String) with:
    render-fancy-reason(self, loc-to-src, make-palette):
      color = make-palette(1).get(0)
      missing =
        [ED.error:
          [ED.para: ED.text("The program is missing something")],
          [ED.para-nospace:
            ED.text("Look carefully before the "), 
            ED.highlight(ED.text("highlighted text"),[ED.locs: self.loc], color),
            ED.text(".  Is something missing just before it?"),
            ED.text("  Common missing items are colons ("), ED.code(ED.text(":")),
            ED.text("), commas ("), ED.code(ED.text(",")), ED.text("), string markers ("),
            ED.code(ED.text("\"")), ED.text("), and keywords.")],
          [ED.para: ED.styled(ED.text("Usually, inserting the missing item will fix this error."), "hint")]]
      extra =
        [ED.error:
          [ED.para: ED.text("The program contains something extra")],
          [ED.para-nospace:
            ED.text("Look carefully before the "), 
            ED.highlight(ED.text("highlighted text"),[ED.locs: self.loc], color),
            ED.text(".  Does it contains something extra?"),
            ED.text("  A common source of errors is typing too much text or in the wrong order.")],
          [ED.para:
            ED.styled(ED.text("Usually, removing the extra item will fix this error."), "hint"),
            ED.text(" However, you may have meant to keep this text, so think before you delete!")]]
      [ED.error:
        [ED.para: ED.text("Pyret didn't understand your program around ")],
         ED.code(ED.highlight(ED.text(loc-to-src(self.loc)),[ED.locs: self.loc], color)),
        [ED.opt:
          [ED.para: ED.text("Typical reasons for getting this error are")],
          [ED.bulleted: missing, extra]]]
    end,
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
    render-fancy-reason(self, loc-to-src, make-palette):
      [ED.error: 
        [ED.para:
          ED.text("Pyret didn't expect your program to "),
          ED.code(ED.highlight(ED.text("end"),[ED.locs: self.loc], make-palette(1).get(0))),
          ED.text(" as soon as it did. You may be missing an \"end\", or closing punctuation like \")\" or \"]\" somewhere in your program.")]]
    end,
    render-reason(self):
      [ED.error: [ED.para:
          ED.text("Pyret didn't understand the very end of your program."),
          ED.text("You may be missing an \"end\", or closing punctuation like \")\" or \"]\" right at the end.")]]
    end
  | parse-error-unterminated-string(loc) with:
    render-fancy-reason(self, loc-to-src, make-palette):
      [ED.error: 
        [ED.para:
          ED.text("Pyret thinks the string ")],
         ED.code(ED.highlight(ED.text(loc-to-src(self.loc)),[ED.locs: self.loc], make-palette(1).get(0))),
        [ED.para:
          ED.text("is unterminated; you may be missing closing punctuation. If you intended to write a multi-line string, use "),
          ED.code(ED.text("```")),
          ED.text(" instead of quotation marks.")]]
    end,
    render-reason(self):
      [ED.error: [ED.para-nospace:
          ED.text("Pyret thinks your program has an incomplete string literal around "),
          draw-and-highlight(self.loc),
          ED.text("; you may be missing closing punctuation.")]]
    end
  | parse-error-bad-operator(loc) with:
    render-fancy-reason(self, loc-to-src, make-palette):
      [ED.error: 
        [ED.para:
          ED.text("The operator "),
          ED.code(ED.highlight(ED.text(loc-to-src(self.loc)),[ED.locs: self.loc], make-palette(1).get(0))),
          ED.text(" must have whitespace separating it from its operands.")]]
    end,
    render-reason(self):
      [ED.error: [ED.para-nospace:
          ED.text("The operator at "),
          draw-and-highlight(self.loc),
          ED.text(" has no surrounding whitespace.")]]
    end
  | parse-error-bad-number(loc) with:
    render-fancy-reason(self, loc-to-src, make-palette):
      [ED.error: 
        [ED.para:
          ED.text("Pyret thinks "),
          ED.code(ED.highlight(ED.text(loc-to-src(self.loc)),[ED.locs: self.loc], make-palette(1).get(0))),
          ED.text(" is probably a number, but number literals in Pyret require at least one digit before the decimal point.")]]
    end,
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
