#lang pyret

provide *
import pprint as PP
import srcloc as S

Loc = S.Srcloc
loc = S.srcloc

dummy-loc = loc("dummy location", -1, -1, -1, -1, -1, -1)

INDENT = 2

break-one = PP.sbreak(1)
str-any = PP.str("Any")
str-arrow = PP.str("->")
str-arrowspace = PP.str("-> ")
str-as = PP.str("as")
str-blank = PP.str("")
str-let = PP.str("let")
str-letrec = PP.str("letrec")
str-block = PP.str("block:")
str-brackets = PP.str("[]")
str-cases = PP.str("cases")
str-caret = PP.str("^")
str-checkcolon = PP.str("check:")
str-examplescolon = PP.str("examples:")
str-colon = PP.str(":")
str-coloncolon = PP.str("::")
str-colonspace = PP.str(": ")
str-comment = PP.str("# ")
str-constructor = PP.str("with constructor")
str-data = PP.str("data ")
str-data-expr = PP.str("data-expr ")
str-datatype = PP.str("datatype ")
str-deriving = PP.str("deriving ")
str-doc = PP.str("doc: ")
str-elsebranch = PP.str("| else =>")
str-elsecolon = PP.str("else:")
str-otherwisecolon = PP.str("otherwise:")
str-elsespace = PP.str("else ")
str-end = PP.str("end")
str-except = PP.str("except")
str-for = PP.str("for ")
str-from = PP.str("from")
str-fun = PP.str("fun")
str-graph = PP.str("graph:")
str-if = PP.str("if ")
str-askcolon = PP.str("ask:")
str-import = PP.str("import")
str-method = PP.str("method")
str-mutable = PP.str("mutable")
str-period = PP.str(".")
str-bang = PP.str("!")
str-pipespace = PP.str("| ")
str-provide = PP.str("provide")
str-provide-star = PP.str("provide *")
str-sharing = PP.str("sharing:")
str-space = PP.str(" ")
str-spacecolonequal = PP.str(" :=")
str-spaceequal = PP.str(" =")
str-thencolon = PP.str("then:")
str-thickarrow = PP.str("=>")
str-try = PP.str("try:")
str-use-loc = PP.str("UseLoc")
str-var = PP.str("var ")
str-val = PP.str("val ")
str-when = PP.str("when")
str-where = PP.str("where:")
str-with = PP.str("with:")

data Name:
  | s-underscore(l :: Loc) with:
    to-compiled-source(self): raise("Cannot compile underscores") end,
    to-compiled(self): raise("Cannot compile underscores") end,
    tosource(self): PP.str("_") end,
    tostring(self): "_" end,
    toname(self): raise("Cannot get name for underscore") end,
    key(self): "underscore#" end

  | s-name(l :: Loc, s :: String) with:
    to-compiled-source(self): raise("Cannot compile local name " + self.s) end,
    to-compiled(self): raise("Cannot compile local name " + self.s) end,
    tosource(self): PP.str(self.s) end,
    tostring(self): self.s end,
    toname(self): self.s end,
    key(self): "name#" + self.s end

  | s-global(s :: String) with:
    to-compiled-source(self): PP.str(self.to-compiled()) end,
    to-compiled(self): self.s end,
    tosource(self): PP.str(self.s) end,
    tostring(self): self.s end,
    toname(self): self.s end,
    key(self): "global#" + self.s end

  | s-atom(base :: String, serial :: Number) with:
    to-compiled-source(self): PP.str(self.to-compiled()) end,
    to-compiled(self): self.base + tostring(self.serial) end,
    tosource(self): PP.str(self.to-compiled()) end,
    tostring(self): self.to-compiled() end,
    toname(self): self.base end,
    key(self): "atom#" + self.base + tostring(self.serial) end
sharing:
  _lessthan(self, other): self.key() < other.key() end,
  _lessequal(self, other): self.key() <= other.key() end,
  _greaterthan(self, other): self.key() > other.key() end,
  _greaterequal(self, other): self.key() >= other.key() end,
  visit(self, visitor):
    self._match(visitor, fun(): raise("No visitor field for " + self.label()) end)
  end
end

fun MakeName(start):
  var count = start
  fun atom(base):
    when not(is-string(base)):
      raise("Got a non-string in make-atom: " + torepr(base))
    end
    count := 1 + count
    s-atom(base, count)
  end
  {
    s-underscore: s-underscore,
    s-name: s-name,
    s-global: s-global,
    make-atom: atom,
    is-s-underscore: is-s-underscore,
    is-s-name: is-s-name,
    is-s-global: is-s-global,
    is-s-atom: is-s-atom,
    Name: Name
  }

end

global-names = MakeName(0)

fun funlam-tosource(funtype, name, params, args :: List<Bind>,
    ann :: Ann, doc :: String, body :: Expr, _check :: Option<Expr>) -> PP.PPrintDoc:
  typarams =
    if is-nothing(params): PP.mt-doc
    else: PP.surround-separate(INDENT, 0, PP.mt-doc, PP.langle, PP.commabreak, PP.rangle,
        params.map(PP.str))
    end
  arg-list = PP.nest(INDENT,
    PP.surround-separate(INDENT, 0, PP.lparen + PP.rparen, PP.lparen, PP.commabreak, PP.rparen,
      args.map(fun(a): a.tosource() end)))
  ftype = funtype + typarams
  fname =
    if is-nothing(name): ftype
    else if PP.is-mt-doc(ftype): PP.str(name)
    else: ftype + PP.str(" " + name)
    end
  fann =
    if is-a-blank(ann) or is-nothing(ann): PP.mt-doc
    else: break-one + str-arrowspace + ann.tosource()
    end
  header = PP.group(fname + arg-list + fann + str-colon)
  checker = cases(Option) _check:
    | none => PP.mt-doc
    | some(chk) => chk.tosource()
  end
  footer =
    if PP.is-mt-doc(checker): str-end
    else: PP.surround(INDENT, 1, str-where, checker, str-end)
    end
  docstr =
    if is-nothing(doc) or (doc == ""): PP.mt-doc
    else: str-doc + PP.dquote(PP.str(doc)) + PP.hardline
    end
  PP.surround(INDENT, 1, header, docstr + body.tosource(), footer)
end

data Program:
  | s-program(l :: Loc, _provide :: Provide, imports :: List<Import>, block :: Expr) with:
    label(self): "s-program" end,
    tosource(self):
      PP.group(
        PP.flow-map(PP.hardline, _.tosource(), self.imports)
          + PP.hardline
          + self.block.tosource()
        )
    end
sharing:
  visit(self, visitor):
    self._match(visitor, fun(): raise("No visitor field for " + self.label()) end)
  end
end

data Import:
  | s-import(l :: Loc, file :: ImportType, name :: Name) with:
    label(self): "s-import" end,
    tosource(self):
      PP.flow([str-import, self.file.tosource(), str-as, self.name.tosource()])
    end
  | s-import-fields(l :: Loc, fields :: List<Name>, file :: ImportType) with:
    label(self): "s-import-fields" end,
    tosource(self):
      PP.flow([str-import,
          PP.flow-map(PP.commabreak, _.tosource(), self.fields),
          str-from, self.file.tosource()])
    end
sharing:
  visit(self, visitor):
    self._match(visitor, fun(): raise("No visitor field for " + self.label()) end)
  end
end

data Provide:
  | s-provide(l :: Loc, block :: Expr) with:
    label(self): "s-provide" end,
    tosource(self):
      PP.soft-surround(INDENT, 1, str-provide,
        self.block.tosource(), str-end)
    end
  | s-provide-all(l :: Loc) with:
    label(self): "s-provide-all" end,
    tosource(self): str-provide-star end
  | s-provide-none(l :: Loc) with:
    label(self): "s-provide-none" end,
    tosource(self): PP.mt-doc end
sharing:
  visit(self, visitor):
    self._match(visitor, fun(): raise("No visitor field for " + self.label()) end)
  end
end

data ImportType:
  | s-file-import(l :: Loc, file :: String) with:
    label(self): "s-file-import" end,
    tosource(self): PP.str(torepr(self.file)) end
  | s-const-import(l :: Loc, mod :: String) with:
    label(self): "s-const-import" end,
    tosource(self): PP.str(self.mod) end
sharing:
  visit(self, visitor):
    self._match(visitor, fun(): raise("No visitor field for " + self.label()) end)
  end
end

data Hint:
  | h-use-loc(l :: Loc) with:
    tosource(self): str-use-loc + PP.parens(PP.str(tostring(self.l))) end
sharing:
  visit(self, visitor):
    self._match(visitor, fun(): raise("No visitor field for " + self.label()) end)
  end
end

data LetBind:
  | s-let-bind(l :: Loc, b :: Bind, value :: Expr) with:
    tosource(self):
      PP.group(PP.nest(INDENT, self.b.tosource() + str-spaceequal + break-one + self.value.tosource()))
    end
  | s-var-bind(l :: Loc, b :: Bind, value :: Expr) with:
    tosource(self):
      PP.group(PP.nest(INDENT, PP.str("var ") + self.b.tosource() + str-spaceequal + break-one + self.value.tosource()))
    end
sharing:
  visit(self, visitor):
    self._match(visitor, fun(): raise("No visitor field for " + self.label()) end)
  end
end

data LetrecBind:
  | s-letrec-bind(l :: Loc, b :: Bind, value :: Expr) with:
    tosource(self):
      PP.group(PP.nest(INDENT, self.b.tosource() + str-spaceequal + break-one + self.value.tosource()))
    end
sharing:
  visit(self, visitor):
    self._match(visitor, fun(): raise("No visitor field for " + self.label()) end)
  end
end

data Expr:
  | s-let-expr(l :: Loc, binds :: List<LetBind>, body :: Expr) with:
    label(self): "s-let" end,
    tosource(self):
      header = PP.surround-separate(2 * INDENT, 1, str-let, str-let + PP.str(" "), PP.commabreak, PP.mt-doc,
          self.binds.map(_.tosource()))
          + str-colon
      PP.surround(INDENT, 1, header, self.body.tosource(), str-end)
    end
  | s-letrec(l :: Loc, binds :: List<LetrecBind>, body :: Expr) with:
    label(self): "s-letrec" end,
    tosource(self):
      header = PP.surround-separate(2 * INDENT, 1, str-letrec, str-letrec + PP.str(" "), PP.commabreak, PP.mt-doc,
          self.binds.map(_.tosource()))
          + str-colon
      PP.surround(INDENT, 1, header, self.body.tosource(), str-end)
    end
  | s-hint-exp(l :: Loc, hints :: List<Hint>, exp :: Expr) with:
    label(self): "s-hint-exp" end,
    tosource(self):
      PP.flow-map(PP.hardline, fun(h): str-comment + h.tosource() end, self.hints) + PP.hardline
        + self.e.tosource()
    end
  | s-instantiate(l :: Loc, expr :: Expr, params :: List<Ann>) with:
    label(self): "s-instantiate" end,
    tosource(self):
      PP.group(self.expr.tosource() +
        PP.surround-separate(INDENT, 0, PP.mt-doc, PP.langle, PP.commabreak, PP.rangle,
            self.params.map(_.tosource)))
    end
  | s-block(l :: Loc, stmts :: List<Expr>) with:
    label(self): "s-block" end,
    tosource(self):
      PP.flow-map(PP.hardline, _.tosource(), self.stmts)
    end
  | s-user-block(l :: Loc, body :: Expr) with:
    label(self): "s-user-block" end,
    tosource(self):
      PP.surround(INDENT, 1, str-block, self.body.tosource(), str-end)
    end
  | s-fun(
      l :: Loc,
      name :: String,
      params :: List<String>, # Type parameters
      args :: List<Bind>, # Value parameters
      ann :: Ann, # return type
      doc :: String,
      body :: Expr,
      _check :: Option<Expr>
    ) with:
      label(self): "s-fun" end,
    tosource(self):
      funlam-tosource(str-fun,
        self.name, self.params, self.args, self.ann, self.doc, self.body, self._check)
    end
  | s-var(l :: Loc, name :: Bind, value :: Expr) with:
    label(self): "s-var" end,
    tosource(self):
      str-var
        + PP.group(PP.nest(INDENT, self.name.tosource()
            + str-spaceequal + break-one + self.value.tosource()))
    end
  | s-let(l :: Loc, name :: Bind, value :: Expr, keyword-val :: Bool) with:
    label(self): "s-let" end,
    tosource(self):
      PP.group(PP.nest(INDENT,
          if self.keyword-val: str-val else: PP.mt-doc end
            + self.name.tosource() + str-spaceequal + break-one + self.value.tosource()))
    end
  | s-graph(l :: Loc, bindings :: List<is-s-let>) with:
    label(self): "s-graph" end,
    tosource(self):
      PP.soft-surround(0, 1, # NOTE: Not indented
        str-graph,
        PP.flow-map(PP.hardline, _.tosource(), self.bindings),
        str-end)
    end
  | s-contract(l :: Loc, name :: Name, ann :: Ann) with:
    label(self): "s-contract" end,
    tosource(self):
      PP.infix(INDENT, 1, str-coloncolon, self.name.tosource(), self.ann.tosource())
    end
  | s-when(l :: Loc, test :: Expr, block :: Expr) with:
    label(self): "s-when" end,
    tosource(self):
      PP.soft-surround(INDENT, 1,
        str-when + PP.parens(self.test.tosource()) + str-colon,
        self.block.tosource(),
        str-end)
    end
  | s-assign(l :: Loc, id :: Name, value :: Expr) with:
    label(self): "s-assign" end,
    tosource(self):
      PP.group(PP.nest(INDENT, self.id.tosource() + str-spacecolonequal + break-one + self.value.tosource()))
    end
  | s-if-pipe(l :: Loc, branches :: List<IfPipeBranch>) with:
    label(self): "s-if-pipe" end,
    tosource(self):
      PP.surround-separate(INDENT, 1, str-askcolon + str-space + str-end,
        PP.group(str-askcolon), break-one, str-end,
        self.branches.map(fun(b): PP.group(b.tosource()) end))
    end
  | s-if-pipe-else(l :: Loc, branches :: List<IfPipeBranch>, _else :: Expr) with:
    label(self): "s-if-pipe-else" end,
    tosource(self):
      body = PP.separate(break-one, self.branches.map(fun(b): PP.group(b.tosource()) end))
        + break-one + PP.group(str-pipespace + str-otherwisecolon + break-one + self._else.tosource())
      PP.surround(INDENT, 1, PP.group(str-askcolon), body, str-end)
    end
  | s-if(l :: Loc, branches :: List<IfBranch>) with:
    label(self): "s-if" end,
    tosource(self):
      branches = PP.separate(break-one + str-elsespace,
        self.branches.map(fun(b): b.tosource() end))
      PP.group(branches + break-one + str-end)
    end
  | s-if-else(l :: Loc, branches :: List<IfBranch>, _else :: Expr) with:
    label(self): "s-if-else" end,
    tosource(self):
      branches = PP.separate(break-one + str-elsespace,
        self.branches.map(fun(b): b.tosource() end))
      _else = str-elsecolon + PP.nest(INDENT, break-one + self._else.tosource())
      PP.group(branches + break-one + _else + break-one + str-end)
    end
  | s-cases(l :: Loc, typ :: Ann, val :: Expr, branches :: List<CasesBranch>) with:
    label(self): "s-cases" end,
    tosource(self):
      header = str-cases + PP.parens(self.type.tosource()) + break-one
        + self.val.tosource() + str-colon
      PP.surround-separate(INDENT, 1, header + str-space + str-end,
        PP.group(header), break-one, str-end,
        self.branches.map(fun(b): PP.group(b.tosource()) end))
    end
  | s-cases-else(l :: Loc, typ :: Ann, val :: Expr, branches :: List<CasesBranch>, _else :: Expr) with:
    label(self): "s-cases-else" end,
    tosource(self):
      header = str-cases + PP.parens(self.type.tosource()) + break-one
        + self.val.tosource() + str-colon
      body = PP.separate(break-one, self.branches.map(fun(b): PP.group(b.tosource()) end))
        + break-one + PP.group(str-elsebranch + break-one + self._else.tosource())
      PP.surround(INDENT, 1, PP.group(header), body, str-end)
    end
  | s-try(l :: Loc, body :: Expr, id :: Bind, _except :: Expr) with:
    label(self): "s-try" end,
    tosource(self):
      _try = str-try + break-one
        + PP.nest(INDENT, self.body.tosource()) + break-one
      _except = str-except + PP.parens(self.id.tosource()) + str-colon + break-one
        + PP.nest(INDENT, self._except.tosource()) + break-one
      PP.group(_try + _except + str-end)
    end
  | s-op(l :: Loc, op :: String, left :: Expr, right :: Expr) with:
    # This should be left-associated, always.
    label(self): "s-op" end,
    tosource(self):
      fun collect-same-operands(exp):
        if is-s-op(exp) and (exp.op == self.op):
          collect-same-operands(exp.left) + collect-same-operands(exp.right)
        else:
          [exp]
        end
      end
      operands = collect-same-operands(self.left) + collect-same-operands(self.right)
      cases(List) operands:
        | empty => PP.mt-doc
        | link(first, rest) =>
          cases(List) rest:
            | empty => first.tosource()
            | link(second, rest2) =>
              op = break-one + PP.str(string-substring(self.op, 2, string-length(self.op))) + break-one
              nested = for list.fold(acc from second.tosource(), operand from rest2):
                acc + PP.group(op + operand.tosource())
              end
              PP.group(first.tosource() + op + PP.nest(INDENT, nested))
          end
      end
    end
  | s-check-test(l :: Loc, op :: String, left :: Expr, right :: Expr) with:
    tosource(self): PP.infix(INDENT, 1, PP.str(string-substring(self.op, 2, string-length(self.op))), self.left.tosource(), self.right.tosource()) end
  | s-paren(l :: Loc, expr :: Expr) with:
    label(self): "s-paren" end,
    tosource(self): PP.parens(self.expr.tosource()) end
  | s-lam(
      l :: Loc,
      params :: List<String>, # Type parameters
      args :: List<Bind>, # Value parameters
      ann :: Ann, # return type
      doc :: String,
      body :: Expr,
      _check :: Option<Expr>
    ) with:
    label(self): "s-lam" end,
    tosource(self):
      funlam-tosource(str-fun,
        nothing, self.params, self.args, self.ann, self.doc, self.body, self._check)
    end
  | s-method(
      l :: Loc,
      args :: List<Bind>, # Value parameters
      ann :: Ann, # return type
      doc :: String,
      body :: Expr,
      _check :: Option<Expr>
    ) with:
    label(self): "s-method" end,
    tosource(self):
      funlam-tosource(str-method,
        nothing, nothing, self.args, self.ann, self.doc, self.body, self._check)
    end
  | s-extend(l :: Loc, supe :: Expr, fields :: List<Member>) with:
    label(self): "s-extend" end,
    tosource(self):
      PP.group(self.supe.tosource() + str-period
          + PP.surround-separate(INDENT, 1, PP.lbrace + PP.rbrace,
          PP.lbrace, PP.commabreak, PP.rbrace, self.fields.map(_.tosource())))
    end
  | s-update(l :: Loc, supe :: Expr, fields :: List<Member>) with:
    label(self): "s-update" end,
  | s-obj(l :: Loc, fields :: List<Member>) with:
    label(self): "s-obj" end,
    tosource(self):
      PP.surround-separate(INDENT, 1, PP.lbrace + PP.rbrace,
        PP.lbrace, PP.commabreak, PP.rbrace, self.fields.map(_.tosource()))
    end
  | s-list(l :: Loc, values :: List<Expr>) with:
    label(self): "s-list" end,
    tosource(self):
      PP.surround-separate(INDENT, 0, str-brackets, PP.lbrack, PP.commabreak, PP.rbrack,
        self.values.map(_.tosource()))
    end
  | s-construct(l :: Loc, modifier :: ConstructModifier, constructor :: Expr, values :: List<Expr>) with:
    label(self): "s-construct" end,
    tosource(self):
      PP.surround(INDENT, 0, PP.lbrack,
        PP.group(PP.separate(PP.sbreak(1), [self.modifier.tosource(), self.constructor.tosource()]))
          + str-colonspace
          + PP.separate(PP.commabreak, self.values.map(_.tosource())),
        PP.rbrack)
    end
  | s-app(l :: Loc, _fun :: Expr, args :: List<Expr>) with:
    label(self): "s-app" end,
    tosource(self):
      PP.group(self._fun.tosource()
          + PP.parens(PP.nest(INDENT,
            PP.separate(PP.commabreak, self.args.map(_.tosource())))))
    end
  | s-prim-app(l :: Loc, _fun :: String, args :: List<Expr>) with:
    label(self): "s-prim-app" end,
    tosource(self):
      PP.group(PP.str(self._fun)
          + PP.parens(PP.nest(INDENT,
            PP.separate(PP.commabreak, self.args.map(_.tosource())))))
    end
  | s-prim-val(l :: Loc, name :: String) with:
    label(self): "s-prim-val" end,
    tosource(self): PP.str(self.name) end
  | s-id(l :: Loc, id :: Name) with:
    label(self): "s-id" end,
    tosource(self): self.id.tosource() end
  | s-id-var(l :: Loc, id :: Name) with:
    label(self): "s-id-var" end,
    tosource(self): PP.str("!") + self.id.tosource() end
  | s-id-letrec(l :: Loc, id :: Name, safe :: Boolean) with:
    label(self): "s-id-letrec" end,
    tosource(self): PP.str("~") + self.id.tosource() end
  | s-undefined(l :: Loc) with:
    label(self): "s-undefined" end,
    tosource(self): PP.str("undefined") end
  | s-num(l :: Loc, n :: Number) with:
    label(self): "s-num" end,
    tosource(self): PP.number(self.n) end
  | s-frac(l :: Loc, num :: Number, den :: Number) with:
    label(self): "s-frac" end,
    tosource(self): PP.number(self.num) + PP.str("/") + PP.number(self.den) end
  | s-bool(l :: Loc, b :: Bool) with:
    label(self): "s-bool" end,
    tosource(self): PP.str(tostring(self.b)) end
  | s-str(l :: Loc, s :: String) with:
    label(self): "s-str" end,
    tosource(self): PP.str(torepr(self.s)) end
  | s-dot(l :: Loc, obj :: Expr, field :: String) with:
    label(self): "s-dot" end,
    tosource(self): PP.infix-break(INDENT, 0, str-period, self.obj.tosource(), PP.str(self.field)) end
  | s-get-bang(l :: Loc, obj :: Expr, field :: String) with:
    label(self): "s-get-bang" end,
    tosource(self): PP.infix-break(INDENT, 0, str-bang, self.obj.tosource(), PP.str(self.field)) end
  | s-bracket(l :: Loc, obj :: Expr, field :: Expr) with:
    label(self): "s-bracket" end,
    tosource(self): PP.infix-break(INDENT, 0, str-period, self.obj.tosource(),
        PP.surround(INDENT, 0, PP.lbrack, self.field.tosource(), PP.rbrack))
    end
  | s-colon(l :: Loc, obj :: Expr, field :: String) with:
    label(self): "s-colon" end,
    tosource(self): PP.infix(INDENT, 0, str-colon, self.obj.tosource(), PP.str(self.field)) end
  | s-colon-bracket(l :: Loc, obj :: Expr, field :: Expr) with:
    label(self): "s-colon-bracket" end,
    tosource(self): PP.infix(INDENT, 0, str-colon, self.obj.tosource(),
        PP.surround(INDENT, 0, PP.lbrack, self.field.tosource(), PP.rbrack))
    end
  | s-data(
      l :: Loc,
      name :: String,
      params :: List<String>, # type params
      mixins :: List<Expr>,
      variants :: List<Variant>,
      shared-members :: List<Member>,
      _check :: Option<Expr>
      ) with:
    label(self): "s-data" end,
    tosource(self):
      fun optional-section(lbl, section):
        if PP.is-mt-doc(section): PP.mt-doc
        else: break-one + PP.group(PP.nest(INDENT, lbl + break-one + section))
        end
      end
      tys = PP.surround-separate(2 * INDENT, 0, PP.mt-doc, PP.langle, PP.commabreak, PP.rangle,
        self.params.map(PP.str))
      header = str-data + PP.str(self.name) + tys + str-colon
      _deriving =
        PP.surround-separate(INDENT, 0, PP.mt-doc, break-one + str-deriving, PP.commabreak, PP.mt-doc, self.mixins.map(fun(m): m.tosource() end))
      variants = PP.separate(break-one + str-pipespace,
        str-blank ^ list.link(_, self.variants.map(fun(v): PP.nest(INDENT, v.tosource()) end)))
      shared = optional-section(str-sharing,
        PP.separate(PP.commabreak, self.shared-members.map(fun(s): s.tosource() end)))
      _check = cases(Option) self._check:
        | none => PP.mt-doc
        | some(chk) => optional-section(str-where, chk.tosource())
      end
      footer = break-one + str-end
      header + _deriving + PP.group(PP.nest(INDENT, variants) + shared + _check + footer)
    end
  | s-data-expr(
      l :: Loc,
      name :: String,
      params :: List<String>, # type params
      mixins :: List<Expr>,
      variants :: List<Variant>,
      shared-members :: List<Member>,
      _check :: Option<Expr>
    ) with:
      label(self): "s-data-expr" end,
    tosource(self):
      fun optional-section(lbl, section):
        if PP.is-mt-doc(section): PP.mt-doc
        else: break-one + PP.group(PP.nest(INDENT, lbl + break-one + section))
        end
      end
      tys = PP.surround-separate(2 * INDENT, 0, PP.mt-doc, PP.langle, PP.commabreak, PP.rangle,
        self.params.map(PP.str))
      header = str-data-expr + PP.str(self.name) + tys + str-colon
      _deriving =
        PP.surround-separate(INDENT, 0, PP.mt-doc, break-one + str-deriving, PP.commabreak, PP.mt-doc, self.mixins.map(fun(m): m.tosource() end))
      variants = PP.separate(break-one + str-pipespace,
        str-blank ^ list.link(_, self.variants.map(fun(v): PP.nest(INDENT, v.tosource()) end)))
      shared = optional-section(str-sharing,
        PP.separate(PP.commabreak, self.shared-members.map(fun(s): s.tosource() end)))
      _check = cases(Option) self._check:
        | none => PP.mt-doc
        | some(chk) => optional-section(str-where, chk.tosource())
      end
      footer = break-one + str-end
      header + _deriving + PP.group(PP.nest(INDENT, variants) + shared + _check + footer)
    end
  | s-for(
      l :: Loc,
      iterator :: Expr,
      bindings :: List<ForBind>,
      ann :: Ann,
      body :: Expr
    ) with:
      label(self): "s-for" end,
    tosource(self):
      header = PP.group(str-for
          + self.iterator.tosource()
          + PP.surround-separate(2 * INDENT, 0, PP.lparen + PP.rparen, PP.lparen, PP.commabreak, PP.rparen,
          self.bindings.map(fun(b): b.tosource() end))
          + PP.group(PP.nest(2 * INDENT,
            break-one + str-arrow + break-one + self.ann.tosource() + str-colon)))
      PP.surround(INDENT, 1, header, self.body.tosource(), str-end)
    end
  | s-check(
      l :: Loc,
      name :: Option<String>,
      body :: Expr,
      keyword-check :: Bool
    ) with:
      label(self): "s-check" end,
    tosource(self):
      cases(Option) self.name:
        | none => PP.surround(INDENT, 1,
            if self.keyword-check: str-checkcolon else: str-examplescolon end,
            self.body.tosource(), str-end)
        | some(name) => PP.surround(INDENT, 1,
            if self.keyword-check: PP.str("check ") else: PP.str("examples ") end
              + PP.dquote(name) + str-colon,
            self.body.tosource(), str-end)
      end
    end
sharing:
  visit(self, visitor):
    self._match(visitor, fun(): raise("No visitor field for " + self.label()) end)
  end
end

data ConstructModifier:
  | s-construct-normal with:
    label(self): "s-construct-normal" end,
    tosource(self): PP.mt-doc end
  | s-construct-lazy with:
    label(self): "s-construct-lazy" end,
    tosource(self): PP.str("lazy") end
sharing:
  visit(self, visitor):
    self._match(visitor, fun(): raise("No visitor field for " + self.label()) end)
  end
end
    

data Bind:
  | s-bind(l :: Loc, shadows :: Bool, id :: Name, ann :: Ann) with:
    tosource(self):
      if is-a-blank(self.ann):
        if self.shadows: PP.str("shadow ") + self.id.tosource()
        else: self.id.tosource()
        end
      else:
        if self.shadows:
          PP.infix(INDENT, 1, str-coloncolon, PP.str("shadow ") + self.id.tosource(), self.ann.tosource())
        else: PP.infix(INDENT, 1, str-coloncolon, self.id.tosource(), self.ann.tosource())
        end
      end
    end,
    label(self): "s_bind" end
sharing:
  visit(self, visitor):
    self._match(visitor, fun(): raise("No visitor field for " + self.label()) end)
  end
end

data Member:
  | s-data-field(l :: Loc, name :: Expr, value :: Expr) with:
    label(self): "s-data-field" end,
    tosource(self): PP.nest(INDENT, self.name.tosource() + str-colonspace + self.value.tosource()) end,
  | s-mutable-field(l :: Loc, name :: Expr, ann :: Ann, value :: Expr) with:
    label(self): "s-mutable-field" end,
    tosource(self): PP.nest(INDENT, str-mutable + self.name.tosource() + str-coloncolon + self.ann.tosource() + str-colonspace + self.value.tosource()) end,
  | s-once-field(l :: Loc, name :: Expr, ann :: Ann, value :: Expr) with:
    label(self): "s-once-field" end
  | s-method-field(
      l :: Loc,
      name :: Expr,
      args :: List<Bind>, # Value parameters
      ann :: Ann, # return type
      doc :: String,
      body :: Expr,
      _check :: Option<Expr>
    ) with:
      label(self): "s-method-field" end,
    tosource(self):
      name-part = cases(Expr) self.name:
        | s-str(l, s) => PP.str(s)
        | else => self.name.tosource()
      end
      funlam-tosource(name-part,
        nothing, nothing, self.args, self.ann, self.doc, self.body, self._check)
    end
sharing:
  visit(self, visitor):
    self._match(visitor, fun(): raise("No visitor field for " + self.label()) end)
  end
end

data ForBind:
  | s-for-bind(l :: Loc, bind :: Bind, value :: Expr) with:
    label(self): "s-for-bind" end,
    tosource(self):
      PP.group(self.bind.tosource() + break-one + str-from + break-one + self.value.tosource())
    end
sharing:
  visit(self, visitor):
    self._match(visitor, fun(): raise("No visitor field for " + self.label()) end)
  end
end

data VariantMemberType:
  | s-normal with:
    label(self): "s-normal" end,
    tosource(self): PP.mt-doc end
  | s-cyclic with:
    label(self): "s-cyclic" end,
    tosource(self): PP.str("cyclic ") end    
  | s-mutable with:
    label(self): "s-mutable" end,
    tosource(self): PP.str("mutable ") end
sharing:
  visit(self, visitor):
    self._match(visitor, fun(): raise("No visitor field for " + self.label()) end)
  end
end

data VariantMember:
  | s-variant-member(l :: Loc, member-type :: VariantMemberType, bind :: Bind) with:
    label(self): "s-variant-member" end,
    tosource(self):
      self.member-type.tosource() + self.bind.tosource()
    end
sharing:
  visit(self, visitor):
    self._match(visitor, fun(): raise("No visitor field for " + self.label()) end)
  end
end

data Variant:
  | s-variant(
      l :: Loc,
      constr-loc :: Loc,
      name :: String,
      members :: List<VariantMember>,
      with-members :: List<Member>
    ) with:
    label(self): "s-variant" end,
    tosource(self):
      header-nowith =
        PP.str(self.name)
        + PP.surround-separate(INDENT, 0, PP.mt-doc, PP.lparen, PP.commabreak, PP.rparen,
        self.members.map(fun(b): b.tosource() end))
      header = PP.group(header-nowith + break-one + str-with)
      withs = self.with-members.map(fun(m): m.tosource() end)
      if list.is-empty(withs): header-nowith
      else: header + PP.group(PP.nest(INDENT, break-one + PP.separate(PP.commabreak, withs)))
      end
    end
  | s-singleton-variant(
      l :: Loc,
      name :: String,
      with-members :: List<Member>
    ) with:
    label(self): "s-singleton-variant" end,
    tosource(self):
      header-nowith = PP.str(self.name)
      header = PP.group(header-nowith + break-one + str-with)
      withs = self.with-members.map(fun(m): m.tosource() end)
      if list.is-empty(withs): header-nowith
      else: header + PP.group(PP.nest(INDENT, break-one + PP.separate(PP.commabreak, withs)))
      end
    end
sharing:
  visit(self, visitor):
    self._match(visitor, fun(): raise("No visitor field for " + self.label()) end)
  end
end

data DatatypeVariant:
  | s-datatype-variant(
      l :: Loc,
      name :: String,
      members :: List<VariantMember>,
      constructor :: Constructor
    ) with:
    label(self): "s-datatype-variant" end,
    tosource(self):
      PP.str("FIXME 10/24/2013: dbp doesn't understand this pp stuff")
    end
  | s-datatype-singleton-variant(
      l :: Loc,
      name :: String,
      constructor :: Constructor
    ) with:
    label(self): "s-datatype-singleton-variant" end,
    tosource(self):
      PP.str("FIXME 10/24/2013: dbp doesn't understand this pp stuff")
    end
sharing:
  visit(self, visitor):
    self._match(visitor, fun(): raise("No visitor field for " + self.label()) end)
  end
end

data Constructor:
  | s-datatype-constructor(
      l :: Loc,
      self :: String,
      body :: Expr
      ) with:
    label(self): "s-datatype-constructor" end,
    tosource(self):
      PP.str("FIXME 10/24/2013: dbp doesn't understand this pp stuff")
    end
sharing:
  visit(self, visitor):
    self._match(visitor, fun(): raise("No visitor field for " + self.label()) end)
  end
end

data IfBranch:
  | s-if-branch(l :: Loc, test :: Expr, body :: Expr) with:
    label(self): "s-if-branch" end,
    tosource(self):
      str-if
        + PP.nest(2 * INDENT, self.test.tosource() + str-colon)
        + PP.nest(INDENT, break-one + self.body.tosource())
    end
sharing:
  visit(self, visitor):
    self._match(visitor, fun(): raise("No visitor field for " + self.label()) end)
  end
end

data IfPipeBranch:
  | s-if-pipe-branch(l :: Loc, test :: Expr, body :: Expr) with:
    label(self): "s-if-pipe-branch" end,
    tosource(self):
      str-pipespace
        + PP.nest(2 * INDENT, self.test.tosource() + break-one + str-thencolon)
        + PP.nest(INDENT, break-one + self.body.tosource())
    end
sharing:
  visit(self, visitor):
    self._match(visitor, fun(): raise("No visitor field for " + self.label()) end)
  end
end

data CasesBranch:
  | s-cases-branch(l :: Loc, name :: String, args :: List<Bind>, body :: Expr) with:
    label(self): "s-cases-branch" end,
    tosource(self):
      PP.nest(INDENT,
        PP.group(PP.str("| " + self.name)
            + PP.surround-separate(INDENT, 0, PP.mt-doc, PP.lparen, PP.commabreak, PP.rparen,
            self.args.map(fun(a): a.tosource() end)) + break-one + str-thickarrow) + break-one +
        self.body.tosource())
    end
sharing:
  visit(self, visitor):
    self._match(visitor, fun(): raise("No visitor field for " + self.label()) end)
  end
end

data Ann:
  | a-blank with:
    label(self): "a-blank" end,
    tosource(self): str-any end
  | a-any with:
    label(self): "a-any" end,
    tosource(self): str-any end
  | a-name(l :: Loc, id :: Name) with:
    label(self): "a-name" end,
    tosource(self): self.id.tosource() end
  | a-arrow(l :: Loc, args :: List<Ann>, ret :: Ann, use-parens :: Bool) with:
    label(self): "a-arrow" end,
    tosource(self):
      ann = PP.separate(str-space,
        [PP.separate(PP.commabreak, self.args.map(_.tosource()))] + [str-arrow, self.ret.tosource()])
      if (self.use-parens): PP.surround(INDENT, 0, PP.lparen, ann, PP.rparen)
      else: ann
      end
    end
  | a-method(l :: Loc, args :: List<Ann>, ret :: Ann) with:
    label(self): "a-method" end,
    tosource(self): PP.str("NYI: A-method") end
  | a-record(l :: Loc, fields :: List<AField>) with:
    label(self): "a-record" end,
    tosource(self):
      PP.surround-separate(INDENT, 1, PP.lbrace + PP.rbrace, PP.lbrace, PP.commabreak, PP.rbrace,
        self.fields.map(_.tosource()))
    end
  | a-app(l :: Loc, ann :: Ann, args :: List<Ann>) with:
    label(self): "a-app" end,
    tosource(self):
      PP.group(self.ann.tosource()
          + PP.group(PP.langle + PP.nest(INDENT,
            PP.separate(PP.commabreak, self.args.map(_.tosource()))) + PP.rangle))
    end
  | a-pred(l :: Loc, ann :: Ann, exp :: Expr) with:
    label(self): "a-pred" end,
    tosource(self): self.ann.tosource() + PP.parens(self.exp.tosource()) end
  | a-dot(l :: Loc, obj :: Name, field :: String) with:
    label(self): "a-dot" end,
    tosource(self): self.obj.tosource() + PP.str("." + self.field) end
sharing:
  visit(self, visitor):
    self._match(visitor, fun(): raise("No visitor field for " + self.label()) end)
  end
end

data AField:
  | a-field(l :: Loc, name :: String, ann :: Ann) with:
    label(self): "a-field" end,
    tosource(self):
      if is-a-blank(self.ann): PP.str(self.name)
      else: PP.infix(INDENT, 1, str-coloncolon, PP.str(self.name), self.ann.tosource())
      end
    end
sharing:
  visit(self, visitor):
    self._match(visitor, fun(): raise("No visitor field for " + self.label()) end)
  end
end

fun make-checker-name(name): "is-" + name;

fun flatten(list-of-lists :: List):
  for fold(biglist from [], piece from list-of-lists):
    biglist + piece
  end
end

fun binding-ids(stmt) -> List<Name>:
  fun variant-ids(variant):
    cases(Variant) variant:
      | s-variant(_, l2, name, _, _) => [s-name(l2, name), s-name(l2, make-checker-name(name))]
      | s-singleton-variant(l, name, _) => [s-name(l, name), s-name(l, make-checker-name(name))]
    end
  end
  cases(Expr) stmt:
    | s-let(_, b, _, _) => [b.id]
    | s-var(_, b, _) => [b.id]
    | s-fun(l, name, _, _, _, _, _, _) => [s-name(l, name)]
    | s-graph(_, bindings) => flatten(bindings.map(binding-ids))
    | s-data(l, name, _, _, variants, _, _) =>
      s-name(l, name) ^ link(_, flatten(variants.map(variant-ids)))
    | else => []
  end
end

fun block-ids(b :: is-s-block) -> List<Name>:
  cases(Expr) b:
    | s-block(_, stmts) => flatten(stmts.map(binding-ids))
    | else => raise("Non-block given to block-ids")
  end
end

fun toplevel-ids(program :: Program) -> List<Name>:
  cases(Program) program:
    | s-program(_, _, _, b) => block-ids(b)
    | else => raise("Non-program given to toplevel-ids")
  end
end

data Pair:
  | pair(l, r)
end

fun length-andmap(pred, l1, l2):
  (l1.length() == l2.length()) and list.all2(pred, l1, l2)
end


# Equivalence modulo srclocs
fun equiv-ast-prog(ast1 :: Program, ast2 :: Program):
  cases(Program) ast1:
    | s-program(_, provide1, imports1, body1) =>
      cases(Program) ast2:
        | s-program(_, provide2, imports2, body2) =>
          equiv-ast-provide(provide1, provide2) and
          length-andmap(equiv-ast-import, imports1, imports2) and
            equiv-ast(body1, body2)
        | else => false
      end
  end
end

fun equiv-name(n1 :: Name, n2 :: Name):
  cases(Name) n1:
    | s-underscore(_) =>
      cases(Name) n2:
        | s-underscore(_) => true
        | else => false
      end
    | s-name(_, s1) =>
      cases(Name) n2:
        | s-name(_, s2) => s1 == s2
        | else => false
      end
    | else => n1 == n2
  end
end

fun equiv-ast-member(m1 :: Member, m2 :: Member):
  cases(Member) m1:
    | s-data-field(_, n1, v1) =>
      cases(Member) m2:
        | s-data-field(_, n2, v2) => equiv-ast(n1, n2) and equiv-ast(v1, v2)
        | else => false
      end
    | s-method-field(_, n1, args1, ann1, doc1, body1, check1) =>
      cases(Member) m2:
        | s-method-field(_, n2, args2, ann2, doc2, body2, check2) =>
          (n1 == n2) and
            length-andmap(equiv-ast-bind, args1, args2) and
            equiv-ast-ann(ann1, ann2) and
            (doc1 == doc2) and
            equiv-ast(body1, body2) and
            equiv-opt(check1, check2)
        | else => false
      end
    | else => false
  end
end

fun equiv-ast-bind(b1 :: Bind, b2 :: Bind):
  cases(Bind) b1:
    | s-bind(_, shadow1, id1, ann1) =>
      cases(Bind) b2:
        | s-bind(_, shadow2, id2, ann2) =>
          (shadow1 == shadow2) and
            equiv-name(id1, id2) and
            equiv-ast-ann(ann1, ann2)
      end
  end
end

fun equiv-ast-for-binding(b1 :: ForBind, b2 :: ForBind):
  cases(ForBind) b1:
    | s-for-bind(_, bind1, value1) =>
      cases(ForBind) b2:
        | s-for-bind(_, bind2, value2) =>
            equiv-ast-bind(bind1, bind2) and equiv-ast(value1, value2)      
        | else => false
      end
  end
end

fun equiv-ast-if-branch(b1 :: IfBranch, b2 :: IfBranch):
  cases(IfBranch) b1:
    | s-if-branch(_, expr1, body1) =>
      cases(IfBranch) b2:
        | s-if-branch(_, expr2, body2) =>
          equiv-ast(expr1, expr2) and equiv-ast(body1, body2)
        | else => false
      end
  end
end

fun equiv-ast-if-pipe-branch(b1 :: IfPipeBranch, b2 :: IfPipeBranch):
  cases(IfPipeBranch) b1:
    | s-if-pipe-branch(_, expr1, body1) =>
      cases(IfPipeBranch) b2:
        | s-if-pipe-branch(_, expr2, body2) =>
          equiv-ast(expr1, expr2) and equiv-ast(body1, body2)
        | else => false
      end
  end
end

fun equiv-ast-cases-branch(cb1 :: CasesBranch, cb2 :: CasesBranch):
  cases(CasesBranch) cb1:
    | s-cases-branch(_, n1, ar1, body1) =>
      cases(CasesBranch) cb2:
        | s-cases-branch(_, n2, ar2, body2) =>
          (n1 == n2) and
            equiv-ast(ar1, ar2) and
            equiv-ast(body1, body2)
        | else => false
      end
  end
end

fun equiv-ast-variant-member(m1 :: VariantMember, m2 :: VariantMember):
  cases(VariantMember) m1:
    | s-variant-member(_, t1, b1) =>
      cases(VariantMember) m2:
        | s-variant-member(_, t2, b2) =>
          (t1 == t2) and equiv-ast-bind(b1, b2)
        | else => false
      end
  end
end

fun equiv-ast-variant(v1 :: Variant, v2 :: Variant):
  cases(Variant) v1:
    | s-variant(_, _, n1, b1, wm1) =>
      cases(Variant) v2:
        | s-variant(_, _, n2, b2, wm2) =>
          (n1 == n2) and
            length-andmap(equiv-ast-bind, b1, b2) and
            length-andmap(equiv-ast-member, wm1, wm2)
        | else => false
      end
    | else => raise("nyi variant")
  end
  #(match (cons v1 v2)
  #  [(cons
  #    (s-singleton-variant _ name1 with-members1)
  #    (s-singleton-variant _ name2 with-members2))
  #   (and
  #    (symbol=? name1 name2)
  #    (length-andmap equiv-ast-member with-members1 with-members2))]
end

fun equiv-ast-datatype-variant(v1 :: DatatypeVariant, v2 :: DatatypeVariant):
  raise("nyi datatype-variant")
  #  [(cons
  #    (s-datatype-variant _ name1 binds1 constructor1)
  #    (s-datatype-variant _ name2 binds2 constructor2))
  #   (and
  #    (symbol=? name1 name2)
  #    (length-andmap equiv-ast-variant-member binds1 binds2)
  #    (equiv-ast-constructor constructor1 constructor2))]
  # [(cons
  #   (s-datatype-singleton-variant _ name1 constructor1)
  #   (s-datatype-singleton-variant _ name2 constructor2))
  #  (and
  #    (symbol=? name1 name2)
  #    (equiv-ast-constructor constructor1 constructor2))]
  #  [_ #f]))
end

fun equiv-ast-constructor(c1 :: Constructor, c2 :: Constructor):
  raise("nyi constructor")
  #(match (cons c1 c2)
  #  [(cons (s-datatype-constructor _ self1 body1)
  #         (s-datatype-constructor _ self2 body2))
  #   (and
  #    (symbol=? self1 self2)
  #    (equiv-ast body1 body2))]))
end

fun equiv-ast-ann(a1, a2):
  cases(Ann) a1:
    | a-any => is-a-any(a2)
    | a-blank => is-a-blank(a2)
    | else =>
      raise("nyi equiv-ast-ann")
  end
  #  [(cons (a-name _ id1) (a-name _ id2)) (equal? id1 id2)]
  #  [(cons (a-pred _ a1 pred1) (a-pred _ a2 pred2))
  #   (and
  #    (equiv-ast-ann a1 a2)
  #    (equiv-ast pred1 pred2))]
  #  [(cons (a-arrow _ args1 ret1 _) (a-arrow _ args2 ret2 _))
  #   (and
  #    (length-andmap equiv-ast-ann args1 args2)
  #    (equiv-ast-ann ret1 ret2))]
  #  [(cons (a-method _ args1 ret1) (a-method _ args2 ret2))
  #   (and
  #    (length-andmap equiv-ast-ann args1 args2)
  #    (equiv-ast-ann ret1 ret2))]
#
#      [(cons (a-field _ name1 ann1) (a-field _ name2 ann2))
#       (and
#        (equal? name1 name2)
#        (equiv-ast-ann ann1 ann2))]
#
#      [(cons (a-record _ fields1) (a-record _ fields2))
#       (length-andmap equiv-ast-ann fields1 fields2)]
#      [(cons (a-app _ ann1 parameters1) (a-app _ ann2 parameters2))
#       (and
#        (equiv-ast-ann ann1 ann2)
#        (length-andmap equiv-ast-ann parameters1 parameters2))]
#      [(cons (a-dot _ obj1 field1) (a-dot _ obj2 field2))
#       (and
#        (equiv-ast-ann obj1 obj2)
#        (equal? field1 field2))]
#      ;; How to catch NYI things?  I wish for some sort of tag-match predicate on pairs
#      [_ #f]))
end

fun equiv-ast-fun(
      name1, params1, args1, ann1, doc1, body1, check1,
      name2, params2, args2, ann2, doc2, body2, check2):
   length-andmap(_ == _, params1, params2) and
     length-andmap(equiv-ast-bind, args1, args2) and
     equiv-ast-ann(ann1, ann2) and
     (doc1 == doc2) and
     equiv-ast(body1, body2) and
     equiv-opt(check1, check2) and
     (name1 == name2)
end

fun equiv-ast-provide(p1 :: Provide, p2 :: Provide):
  cases(Provide) p1:
    | s-provide(_, b1) =>
      cases(Provide) p2:
        | s-provide(_, b2) => equiv-ast(b1, b2)
        | else => false
      end
    | s-provide-all(_) => is-s-provide-all(p2)
    | s-provide-none(_) => is-s-provide-none(p2)
  end
end

fun equiv-import-type(f1, f2):
  cases(ImportType) f1:
    | s-file-import(_, n1) =>
      cases(ImportType) f2:
        | s-file-import(_, n2) => n1 == n2
        | else => false
      end
    | s-const-import(_, n1) =>
      cases(ImportType) f2:
        | s-const-import(_, n2) => n1 == n2
        | else => false
      end
  end
end

fun equiv-ast-import(i1 :: Import, i2 :: Import):
  cases(Import) i1:
    | s-import(_, f1, n1) =>
      cases(Import) i2:
        | s-import(_, f2, n2) => equiv-import-type(f1, f2) and equiv-name(n1, n2)
        | else => false
      end
    | s-import-fields(_, fields1, f1) =>
      cases(Import) i2:
        | s-import-fields(_, fields2, f2) =>
          length-andmap(equiv-name, fields1, fields2) and equiv-name(f1, f2)
        | else => false
      end
  end
end

fun equiv-ast-let-bind(lb1 :: LetBind, lb2 :: LetBind):
  cases(LetBind) lb1:
    | s-let-bind(_, bind1, value1) => 
      cases(LetBind) lb2:
        | s-let-bind(_, bind2, value2) =>
          equiv-ast-bind(bind1, bind2) and
            equiv-ast(value1, value2)
        | else => false
      end
    | s-var-bind(_, bind1, value1) => 
      cases(LetBind) lb2:
        | s-var-bind(_, bind2, value2) => 
          equiv-ast-bind(bind1, bind2) and
            equiv-ast(value1, value2)
        | else => false
      end
  end
end

fun equiv-ast-letrec-bind(lb1 :: LetrecBind, lb2 :: LetrecBind):
  cases(LetrecBind) lb1:
    | s-letrec-bind(_, bind1, value1) => 
      cases(LetrecBind) lb2:
        | s-letrec-bind(_, bind2, value2) =>
          equiv-ast-bind(bind1, bind2) and
            equiv-ast(value1, value2)
        | else => false
      end
  end
end

fun equiv-opt(opt1 :: Option<Expr>, opt2 :: Option<Expr>):
  cases(Option) opt1:
    | none => is-none(opt2)
    | some(v1) =>
      cases(Option) opt2:
        | none => false
        | some(v2) => equiv-ast(v1, v2)
      end
  end
end

fun equiv-ast(ast1 :: Expr, ast2 :: Expr):
  cases (Expr) ast1:
    | s-block(_, stmts1) =>
      cases(Expr) ast2:
        | s-block(_, stmts2) => length-andmap(equiv-ast, stmts1, stmts2)
        | else => false
      end
    | s-fun(_, n1, p1, ar1, an1, d1, b1, c1) =>
      cases(Expr) ast2:
        | s-fun(_, n2, p2, ar2, an2, d2, b2, c2) =>
          equiv-ast-fun(
              n1, p1, ar1, an1, d1, b1, c1,
              n2, p2, ar2, an2, d2, b2, c2
            )
        | else => false
      end
    | s-lam(_, p1, ar1, an1, d1, b1, c1) =>
      cases(Expr) ast2:
        | s-lam(_, p2, ar2, an2, d2, b2, c2) =>
          equiv-ast-fun(
              "anon", p2, ar1, an1, d1, b1, c1,
              "anon", p2, ar2, an2, d2, b2, c2
            )
        | else => false
      end
    | s-method(_, ar1, an1, d1, b1, c1) =>
      cases(Expr) ast2:
        | s-method(_, ar2, an2, d2, b2, c2) =>
          equiv-ast-fun(
              "meth", [], ar1, an1, d1, b1, c1,
              "meth", [], ar2, an2, d2, b2, c2
            )
        | else => false
      end
    | s_check(_, name1, body1, check1) =>
      cases(Expr) ast2:
        | s_check(_, name2, body2, check2) => (name1 == name2) and (check1 == check2) and equiv-ast(body1, body2)
        | else => false
      end
    | s-var(_, bind1, value1) =>
      cases(Expr) ast2:
        | s-var(_, bind2, value2) =>
          equiv-ast-bind(bind1, bind2) and equiv-ast(value1, value2)
        | else => false
      end
    | s-let(_, bind1, value1, keyword-val1) =>
      cases(Expr) ast2:
        | s-let(_, bind2, value2, keyword-val2) =>
          (keyword-val1 == keyword-val2) and equiv-ast-bind(bind1, bind2) and equiv-ast(value1, value2)
        | else => false
      end
    | s-graph(_, bindings1) =>
      cases(Expr) ast2:
        | s-graph(_, bindings2) => length-andmap(equiv-ast, bindings1, bindings2)
        | else => false
      end
    | s-contract(_, name1, ann1) =>
      cases(Expr) ast2:
        | s-contract(_, name2, ann2) => equiv-name(name1, name2) and equiv-ast-ann(ann1, ann2)
        | else => false
      end          
    | s-list(_, values1) =>
      cases(Expr) ast2:
        | s-list(_, values2) => length-andmap(equiv-ast, values1, values2)
        | else => false
      end
    | s-construct(_, mod1, constr1, values1) =>
      cases(Expr) ast2:
        | s-construct(_, mod2, constr2, values2) =>
          (mod1 == mod2) and equiv-ast(constr1, constr2) and length-andmap(equiv-ast, values1, values2)
        | else => false
      end
    | s-op(_, op1, left1, right1) =>
      cases(Expr) ast2:
        | s-op(_, op2, left2, right2) =>
            (op1 == op2) and
              equiv-ast(left1, left2) and
              equiv-ast(right1, right2)
        | else => false
      end
    | s-check-test(_, op1, left1, right1) =>
      cases(Expr) ast2:
        | s-check-test(_, op2, left2, right2) =>
            (op1 == op2) and
              equiv-ast(left1, left2) and
              equiv-ast(right1, right2)
        | else => false
      end
    | s-user-block(_, block1) =>
      cases(Expr) ast2:
        | s-user-block(_, block2) => equiv-ast(block1, block2)
        | else => false
      end
    | s-when(_, test1, block1) =>
      cases(Expr) ast2:
        | s-when(_, test2, block2) =>
          equiv-ast(test1, test2) and equiv-ast(block1, block2)
        | else => false
      end
    | s-if(_, branches1) =>
      cases(Expr) ast2:
        | s-if(_, branches2) =>
          length-andmap(equiv-ast-if-branch, branches1, branches2)
        | else => false
      end
    | s-if-pipe(_, branches1) =>
      cases(Expr) ast2:
        | s-if-pipe(_, branches2) =>
          length-andmap(equiv-ast-if-pipe-branch, branches1, branches2)
        | else => false
      end
    | s-if-else(_, branches1, _else1) => 
      cases(Expr) ast2:
        | s-if-else(_, branches2, _else2) =>
          length-andmap(equiv-ast-if-branch, branches1, branches2) and
            equiv-ast(_else1, _else2)
        | else => false
      end
    | s-if-pipe-else(_, branches1, _else1) => 
      cases(Expr) ast2:
        | s-if-pipe-else(_, branches2, _else2) =>
          length-andmap(equiv-ast-if-pipe-branch, branches1, branches2) and
            equiv-ast(_else1, _else2)
        | else => false
      end
    | s-try(_, body1, id1, except1) =>
      cases(Expr) ast2:
        | s-try(_, body2, id2, except2) =>
          equiv-ast(body1, body2) and
            equiv-ast-bind(id1, id2) and
            equiv-ast(except1, except2)
        | else => false
      end
    | s-cases(_, type1, val1, branches1) => 
      cases(Expr) ast2:
        | s-cases(_, type2, val2, branches2) =>
          equiv-ast-ann(type1, type2) and
            equiv-ast(val1, val2) and
            length-andmap(equiv-ast-cases-branch, branches1, branches2)
        | else => false
      end
    | s-cases-else(_, type1, val1, branches1, _else1) =>
      cases(Expr) ast2:
        | s-cases-else(_, type2, val2, branches2, _else2) =>
          equiv-ast-ann(type1, type2) and
            equiv-ast(val1, val2) and
            length-andmap(equiv-ast-cases-branch, branches1, branches2) and
            equiv-ast(_else1, _else2)
        | else => false
      end
    | s-paren(_, expr1) =>
      cases(Expr) ast2:
        | s-paren(_, expr2) => equiv-ast(expr1, expr2)
        | else => false
      end
    | s-extend(_, super1, fields1) => 
      cases(Expr) ast2:
        | s-extend(_, super2, fields2) => 
          equiv-ast(super1, super2) and length-andmap(equiv-ast-member, fields1, fields2)
        | else => false
      end
    | s-update(_, super1, fields1) => 
      cases(Expr) ast2:
        | s-update(_, super2, fields2) => 
          equiv-ast(super1, super2) and length-andmap(equiv-ast-member, fields1, fields2)
        | else => false
      end
    | s-obj(_, fields1) => 
      cases(Expr) ast2:
        | s-obj(_, fields2) => length-andmap(equiv-ast-member, fields1, fields2)
        | else => false
      end
    | s-app(_, fun1, args1) =>
      cases(Expr) ast2:
        | s-app(_, fun2, args2) =>
          equiv-ast(fun1, fun2) and length-andmap(equiv-ast, args1, args2)
        | else => false
      end
    | s-prim-app(_, fun1, args1) =>
      cases(Expr) ast2:
        | s-prim-app(_, fun2, args2) =>
          (fun1 == fun2) and length-andmap(equiv-ast, args1, args2)
        | else => false
      end
    | s-prim-val(_, name1) =>
      cases(Expr) ast2:
        | s-prim-val(_, name2) => name1 == name2
        | else => false
      end
    | s-assign(_, id1, value1) =>
      cases(Expr) ast2:
        | s-assign(_, id2, value2) =>
          equiv-name(id1, id2) and
            equiv-ast(value1, value2)
        | else => false
      end
    | s-colon(_, obj1, field1) =>
      cases(Expr) ast2:
        | s-colon(_, obj2, field2) =>
          equiv-ast(obj1, obj2) and (field1 == field2)
        | else => false
      end
    | s-dot(_, obj1, field1) =>
      cases(Expr) ast2:
        | s-dot(_, obj2, field2) =>
          equiv-ast(obj1, obj2) and (field1 == field2)
        | else => false
      end
    | s-get-bang(_, obj1, field1) =>
      cases(Expr) ast2:
        | s-get-bang(_, obj2, field2) =>
          equiv-ast(obj1, obj2) and (field1 == field2)
        | else => false
      end
    | s-bracket(_, obj1, field1) =>
      cases(Expr) ast2:
        | s-bracket(_, obj2, field2) =>
          equiv-ast(obj1, obj2) and equiv-ast(field1, field2)
        | else => false
      end
    | s-colon-bracket(_, obj1, field1) =>
      cases(Expr) ast2:
        | s-colon-bracket(_, obj2, field2) =>
          equiv-ast(obj1, obj2) and equiv-ast(field1, field2)
        | else => false
      end
    | s-for(_, iter1, binds1, ann1, body1) =>
      cases(Expr) ast2:
        | s-for(_, iter2, binds2, ann2, body2) =>
          equiv-ast(iter1, iter2) and
            length-andmap(equiv-ast-for-binding, binds1, binds2) and
            equiv-ast-ann(ann1, ann2) and
            equiv-ast(body1, body2)
        | else => false
      end
    | s-data(_, n1, p1, m1, v1, sm1, c1) =>
      cases(Expr) ast2:
        | s-data(_, n2, p2, m2, v2, sm2, c2) =>
          (n1 == n2) and
            length-andmap(_ == _, p1, p2) and
            length-andmap(equiv-ast, m1, m2) and
            length-andmap(equiv-ast-variant, v1, v2) and
            length-andmap(equiv-ast-member, sm1, sm2) and
            equiv-opt(c1, c2)
        | else => false
      end
    | s-data-expr(_, n1, p1, m1, v1, sm1, c1) =>
      cases(Expr) ast2:
        | s-data-expr(_, n2, p2, m2, v2, sm2, c2) =>
          (n1 == n2) and
            length-andmap(_ == _, p1, p2) and
            length-andmap(equiv-ast, m1, m2) and
            length-andmap(equiv-ast-variant, v1, v2) and
            length-andmap(equiv-ast-member, sm1, sm2) and
            equiv-opt(c1, c2)
        | else => false
      end
    | s-num(_, n1) =>
      cases(Expr) ast2:
        | s-num(_, n2) => n1 == n2
        | else => false
      end
    | s-frac(_, n1, d1) =>
      cases(Expr) ast2:
        | s-frac(_, n2, d2) => (n1 == n2) and (d1 == d2)
        | else => false
      end
    | s-str(_, s1) =>
      cases(Expr) ast2:
        | s-str(_, s2) => s1 == s2
        | else => false
      end
    | s-bool(_, b1) => 
      cases(Expr) ast2:
        | s-bool(_, b2) => b1 == b2
        | else => false
      end
    | s-undefined(_) =>
      is-s-undefined(ast2)
    | s-id(_, id1) =>
      cases(Expr) ast2:
        | s-id(_, id2) => equiv-name(id1, id2)
        | else => false
      end
    | s-id-var(_, id1) =>
      cases(Expr) ast2:
        | s-id-var(_, id2) => equiv-name(id1, id2)
        | else => false
      end
    | s-id-letrec(_, id1, safe1) =>
      cases(Expr) ast2:
        | s-id-letrec(_, id2, safe2) =>
          equiv-name(id1, id2) and (safe1 == safe2)
        | else => false
      end
    | s-let-expr(_, let-binds1, body1) =>
      cases(Expr) ast2:
        | s-let-expr(_, let-binds2, body2) =>
          length-andmap(equiv-ast-let-bind, let-binds1, let-binds2) and
            equiv-ast(body1, body2)
        | else => false
      end
    | s-letrec(_, let-binds1, body1) =>
      cases(Expr) ast2:
        | s-letrec(_, let-binds2, body2) =>
          length-andmap(equiv-ast-letrec-bind, let-binds1, let-binds2) and
            equiv-ast(body1, body2)
        | else => false
      end
  end
end

default-map-visitor = {
  option(self, opt):
    cases(Option) opt:
      | none => none
      | some(v) => some(v.visit(self))
    end
  end,

  s-underscore(self, l):
    s-underscore(l)
  end,

  s-name(self, l, s):
    s-name(l, s)
  end,

  s-global(self, s):
    s-global(s)
  end,

  s-atom(self, base, serial):
    s-atom(base, serial)
  end,
  
  s-program(self, l, _provide, imports, body):
    s-program(l, _provide.visit(self), imports.map(_.visit(self)), body.visit(self))
  end,

  s-import(self, l, import-type, name):
    s-import(l, import-type, name.visit(self))
  end,
  s-import-fields(self, l, fields, import-type):
    s-import-fields(l, fields.map(_.visit(self)), import-type)
  end,
  s-provide(self, l, expr):
    s-provide(l, expr.visit(self))
  end,
  s-provide-all(self, l):
    s-provide-all(l)
  end,
  s-provide-none(self, l):
    s-provide-none(l)
  end,

  s-bind(self, l, shadows, name, ann):
    s-bind(l, shadows, name.visit(self), ann.visit(self))
  end,

  s-var-bind(self, l, bind, expr):
    s-var-bind(l, bind.visit(self), expr.visit(self))
  end,
  s-let-bind(self, l, bind, expr):
    s-let-bind(l, bind.visit(self), expr.visit(self))
  end,

  s-let-expr(self, l, binds, body):
    s-let-expr(l, binds.map(_.visit(self)), body.visit(self))
  end,

  s-letrec-bind(self, l, bind, expr):
    s-letrec-bind(l, bind.visit(self), expr.visit(self))
  end,

  s-letrec(self, l, binds, body):
    s-letrec(l, binds.map(_.visit(self)), body.visit(self))
  end,

  s-hint-exp(self, l :: Loc, hints :: List<Hint>, exp :: Expr):
    s-hint-exp(l, hints, exp.visit(self))
  end,

  s-instantiate(self, l :: Loc, expr :: Expr, params :: List<Ann>):
    s-instantiate(l, expr.visit(self), params.map(_.visit(self)))
  end,

  s-block(self, l, stmts):
    s-block(l, stmts.map(_.visit(self)))
  end,

  s-user-block(self, l :: Loc, body :: Expr):
    s-user-block(l, body.visit(self))
  end,

  s-fun(self, l, name, params, args, ann, doc, body, _check):
    s-fun(l, name, params, args.map(_.visit(self)), ann.visit(self), doc, body.visit(self), self.option(_check))
  end,

  s-var(self, l :: Loc, name :: Bind, value :: Expr):
    s-var(l, name.visit(self), value.visit(self))
  end,

  s-let(self, l :: Loc, name :: Bind, value :: Expr, keyword-val :: Bool):
    s-let(l, name.visit(self), value.visit(self), keyword-val) 
  end,

  s-graph(self, l :: Loc, bindings :: List<is-s-let>):
    s-graph(l, bindings.map(_.visit(self)))
  end,

  s-when(self, l :: Loc, test :: Expr, block :: Expr):
    s-when(l, test.visit(self), block.visit(self))
  end,

  s-contract(self, l, name, ann):
    s-contract(l, name.visit(self), ann.visit(self))
  end,

  s-assign(self, l :: Loc, id :: Name, value :: Expr):
    s-assign(l, id.visit(self), value.visit(self))
  end,

  s-if-branch(self, l :: Loc, test :: Expr, body :: Expr):
    s-if-branch(l, test.visit(self), body.visit(self))
  end,

  s-if-pipe-branch(self, l :: Loc, test :: Expr, body :: Expr):
    s-if-pipe-branch(l, test.visit(self), body.visit(self))
  end,

  s-if(self, l :: Loc, branches :: List<IfBranch>):
    s-if(l, branches.map(_.visit(self)))
  end,
  s-if-else(self, l :: Loc, branches :: List<IfBranch>, _else :: Expr):
    s-if-else(l, branches.map(_.visit(self)), _else.visit(self))
  end,
  
  s-if-pipe(self, l :: Loc, branches :: List<IfPipeBranch>):
    s-if-pipe(l, branches.map(_.visit(self)))
  end,
  s-if-pipe-else(self, l :: Loc, branches :: List<IfPipeBranch>, _else :: Expr):
    s-if-pipe-else(l, branches.map(_.visit(self)), _else.visit(self))
  end,

  s-cases-branch(self, l :: Loc, name :: String, args :: List<Bind>, body :: Expr):
    s-cases-branch(l, name, args.map(_.visit(self)), body.visit(self))
  end,

  s-cases(self, l :: Loc, typ :: Ann, val :: Expr, branches :: List<CasesBranch>):
    s-cases(l, typ.visit(self), val.visit(self), branches.map(_.visit(self)))
  end,
  s-cases-else(self, l :: Loc, typ :: Ann, val :: Expr, branches :: List<CasesBranch>, _else :: Expr):
    s-cases-else(l, typ.visit(self), val.visit(self), branches.map(_.visit(self)), _else.visit(self))
  end,

  s-try(self, l :: Loc, body :: Expr, id :: Bind, _except :: Expr):
    s-try(l, body.visit(self), id.visit(self), _except.visit(self))
  end,

  s-op(self, l :: Loc, op :: String, left :: Expr, right :: Expr):
    s-op(l, op, left.visit(self), right.visit(self))
  end,

  s-check-test(self, l :: Loc, op :: String, left :: Expr, right :: Expr):
    s-check-test(l, op, left.visit(self), right.visit(self))
  end,

  s-paren(self, l :: Loc, expr :: Expr):
    s-paren(l, expr.visit(self))
  end,

  s-lam(
      self,
      l :: Loc,
      params :: List<String>,
      args :: List<Bind>,
      ann :: Ann,
      doc :: String,
      body :: Expr,
      _check :: Option<Expr>
    ):
    s-lam(l, params, args.map(_.visit(self)), ann.visit(self), doc, body.visit(self), self.option(_check))
  end,
  s-method(
      self,
      l :: Loc,
      args :: List<Bind>, # Value parameters
      ann :: Ann, # return type
      doc :: String,
      body :: Expr,
      _check :: Option<Expr>
    ):
    s-method(l, args.map(_.visit(self)), ann.visit(self), doc, body.visit(self), self.option(_check))
  end,
  s-extend(self, l :: Loc, supe :: Expr, fields :: List<Member>):
    s-extend(l, supe.visit(self), fields.map(_.visit(self)))
  end,
  s-update(self, l :: Loc, supe :: Expr, fields :: List<Member>):
    s-update(l, supe.visit(self), fields.map(_.visit(self)))
  end,
  s-obj(self, l :: Loc, fields :: List<Member>):
    s-obj(l, fields.map(_.visit(self)))
  end,
  s-list(self, l :: Loc, values :: List<Expr>):
    s-list(l, values.map(_.visit(self)))
  end,
  s-construct(self, l :: Loc, mod :: ArrayModifier, constructor :: Expr, values :: List<Expr>):
    s-construct(l, mod, constructor.visit(self), values.map(_.visit(self)))
  end,
  s-app(self, l :: Loc, _fun :: Expr, args :: List<Expr>):
    s-app(l, _fun.visit(self), args.map(_.visit(self)))
  end,
  s-prim-app(self, l :: Loc, _fun :: String, args :: List<Expr>):
    s-prim-app(l, _fun, args.map(_.visit(self)))
  end,
  s-prim-val(self, l :: Loc, name :: String):
    s-prim-val(l, name)
  end,
  s-id(self, l :: Loc, id :: Name):
    s-id(l, id.visit(self))
  end,
  s-id-var(self, l :: Loc, id :: Name):
    s-id-var(l, id.visit(self))
  end,
  s-id-letrec(self, l :: Loc, id :: Name, safe :: Boolean):
    s-id-letrec(l, id.visit(self), safe)
  end,
  s-undefined(self, l :: Loc):
    s-undefined(self)
  end,
  s-num(self, l :: Loc, n :: Number):
    s-num(l, n)
  end,
  s-frac(self, l :: Loc, num :: Number, den :: Number):
    s-frac(l, num, den)
  end,
  s-bool(self, l :: Loc, b :: Bool):
    s-bool(l, b)
  end,
  s-str(self, l :: Loc, s :: String):
    s-str(l, s)
  end,
  s-dot(self, l :: Loc, obj :: Expr, field :: String):
    s-dot(l, obj.visit(self), field)
  end,
  s-get-bang(self, l :: Loc, obj :: Expr, field :: String):
    s-get-bang(l, obj.visit(self), field)
  end,
  s-bracket(self, l :: Loc, obj :: Expr, field :: Expr):
    s-bracket(l, obj.visit(self), field.visit(self))
  end,
  s-colon(self, l :: Loc, obj :: Expr, field :: String):
    s-colon(l, obj.visit(self), field)
  end,
  s-colon-bracket(self, l :: Loc, obj :: Expr, field :: Expr):
    s-colon-bracket(l, obj.visit(self), field.visit(self))
  end,
  s-data(
      self,
      l :: Loc,
      name :: String,
      params :: List<String>, # type params
      mixins :: List<Expr>,
      variants :: List<Variant>,
      shared-members :: List<Member>,
      _check :: Option<Expr>
    ):
    s-data(
        l,
        name,
        params,
        mixins.map(_.visit(self)),
        variants.map(_.visit(self)),
        shared-members.map(_.visit(self)),
        self.option(_check)
      )
  end,
  s-data-expr(
      self,
      l :: Loc,
      name :: String,
      params :: List<String>, # type params
      mixins :: List<Expr>,
      variants :: List<Variant>,
      shared-members :: List<Member>,
      _check :: Option<Expr>
    ):
    s-data-expr(
        l,
        name,
        params,
        mixins.map(_.visit(self)),
        variants.map(_.visit(self)),
        shared-members.map(_.visit(self)),
        self.option(_check)
      )
  end,
  s-for(
      self,
      l :: Loc,
      iterator :: Expr,
      bindings :: List<ForBind>,
      ann :: Ann,
      body :: Expr
    ):
    s-for(l, iterator.visit(self), bindings.map(_.visit(self)), ann.visit(self), body.visit(self))
  end,
  s-check(self, l :: Loc, name :: Option<String>, body :: Expr, keyword-check :: Bool):
    s-check(l, name, body.visit(self), keyword-check)
  end,

  s-data-field(self, l :: Loc, name :: Expr, value :: Expr):
    s-data-field(l, name.visit(self), value.visit(self))
  end,
  s-mutable-field(self, l :: Loc, name :: Expr, ann :: Ann, value :: Expr):
    s-mutable-field(l, name.visit(self), ann.visit(self), value.visit(self))
  end,
  s-once-field(self, l :: Loc, name :: Expr, ann :: Ann, value :: Expr):
    s-once-field(l, name.visit(self), ann.visit(self), value.visit(self))
  end,
  s-method-field(
      self,
      l :: Loc,
      name :: Expr,
      args :: List<Bind>, # Value parameters
      ann :: Ann, # return type
      doc :: String,
      body :: Expr,
      _check :: Option<Expr>
    ):
    s-method-field(
        l,
        name.visit(self),
        args.map(_.visit(self)),
        ann.visit(self),
        doc,
        body.visit(self),
        self.option(_check)
      )
  end,

  s-for-bind(self, l :: Loc, bind :: Bind, value :: Expr):
    s-for-bind(l, bind.visit(self), value.visit(self))   
  end,
  s-variant-member(self, l :: Loc, member-type :: VariantMemberType, bind :: Bind):
    s-variant-member(l, member-type, bind.visit(self))
  end,
  s-variant(
      self,
      l :: Loc,
      constr-loc :: Loc,
      name :: String,
      members :: List<VariantMember>,
      with-members :: List<Member>
    ):
    s-variant(l, constr-loc, name, members.map(_.visit(self)), with-members.map(_.visit(self)))
  end,
  s-singleton-variant(
      self,
      l :: Loc,
      name :: String,
      with-members :: List<Member>
    ):
    s-singleton-variant(l, name, with-members.map(_.visit(self)))
  end,
  s-datatype-variant(
      self,
      l :: Loc,
      name :: String,
      members :: List<VariantMember>,
      constructor :: Constructor
    ):
    s-datatype-variant(l, name, members.map(_.visit(self)), constructor.visit(self))
  end,
  s-datatype-singleton-variant(
      self,
      l :: Loc,
      name :: String,
      constructor :: Constructor
    ):
    s-datatype-singleton-variant(l, name, constructor.visit(self))
  end,
  s-datatype-constructor(
      self,
      l :: Loc,
      self-arg :: String,
      body :: Expr
      ):
    s-datatype-constructor(l, self-arg, body.visit(self))
  end,

  a-blank(self): a-blank end,
  a-any(self): a-any end,
  a-name(self, l, id): a-name(l, id.visit(self)) end,
  a-arrow(self, l, args, ret, use-parens):
    a-arrow(l, args.map(_.visit(self)), ret.visit(self), use-parens)
  end,
  a-method(self, l, args, ret):
    a-method(l, args.map(_.visit(self)), ret.visit(self))
  end,
  a-record(self, l, fields):
    a-record(l, fields.map(_.visit(self)))
  end,
  a-app(self, l, ann, args):
    a-app(l, ann.visit(self), args.map(_.visit(self)))
  end,
  a-pred(self, l, ann, exp):
    a-pred(l, ann.visit(self), exp.visit(self))
  end,
  a-dot(self, l, obj, field):
    a-dot(l, obj.visit(self), field)
  end,
  a-field(self, l, name, ann):
    a-field(l, name, ann.visit(self))
  end
}


default-iter-visitor = {
  option(self, opt):
    cases(Option) opt:
      | none => true
      | some(v) => v.visit(self)
    end
  end,

  s-underscore(self, l):
    true
  end,
  s-name(self, l, s):
    true
  end,
  s-global(self, s):
    true
  end,
  s-atom(self, base, serial):
    true
  end,
  
  s-program(self, l, _provide, imports, body):
    _provide.visit(self) and list.all(_.visit(self), imports) and body.visit(self)
  end,
  
  s-import(self, l, import-type, name):
    name.visit(self)
  end,
  s-import-fields(self, l, fields, import-type):
    list.all(_.visit(self), fields)
  end,
  s-provide(self, l, expr):
    expr.visit(self)
  end,
  s-provide-all(self, l):
    true
  end,
  s-provide-none(self, l):
    true
  end,
  
  s-bind(self, l, shadows, name, ann):
    name.visit(self) and ann.visit(self)
  end,
  
  s-var-bind(self, l, bind, expr):
    bind.visit(self) and expr.visit(self)
  end,
  s-let-bind(self, l, bind, expr):
    bind.visit(self) and expr.visit(self)
  end,
  
  s-let-expr(self, l, binds, body):
    list.all(_.visit(self), binds) and body.visit(self)
  end,
  
  s-letrec-bind(self, l, bind, expr):
    bind.visit(self) and expr.visit(self)
  end,
  
  s-letrec(self, l, binds, body):
    list.all(_.visit(self), binds) and body.visit(self)
  end,
  
  s-hint-exp(self, l :: Loc, hints :: List<Hint>, exp :: Expr):
    exp.visit(self)
  end,
  
  s-instantiate(self, l :: Loc, expr :: Expr, params :: List<Ann>):
    expr.visit(self) and list.all(_.visit(self), params)
  end,
  
  s-block(self, l, stmts):
    list.all(_.visit(self), stmts)
  end,
  
  s-user-block(self, l :: Loc, body :: Expr):
    body.visit(self)
  end,
  
  s-fun(self, l, name, params, args, ann, doc, body, _check):
    list.all(_.visit(self), args) and ann.visit(self) and body.visit(self) and self.option(_check)
  end,
  
  s-var(self, l :: Loc, name :: Bind, value :: Expr):
    name.visit(self) and value.visit(self)
  end,
  
  s-let(self, l :: Loc, name :: Bind, value :: Expr, keyword-val :: Bool):
    name.visit(self) and value.visit(self)
  end,
  
  s-graph(self, l :: Loc, bindings :: List<is-s-let>):
    list.all(_.visit(self), bindings)
  end,
  
  s-when(self, l :: Loc, test :: Expr, block :: Expr):
    test.visit(self) and block.visit(self)
  end,

  s-contract(self, l :: Loc, name :: Name, ann :: Ann):
    name.visit(self) and ann.visit(self)
  end,
  
  s-assign(self, l :: Loc, id :: Name, value :: Expr):
    id.visit(self) and value.visit(self)
  end,
  
  s-if-branch(self, l :: Loc, test :: Expr, body :: Expr):
    test.visit(self) and body.visit(self)
  end,
  
  s-if-pipe-branch(self, l :: Loc, test :: Expr, body :: Expr):
    test.visit(self) and body.visit(self)
  end,
  
  s-if(self, l :: Loc, branches :: List<IfBranch>):
    list.all(_.visit(self), branches)
  end,
  s-if-else(self, l :: Loc, branches :: List<IfBranch>, _else :: Expr):
    list.all(_.visit(self), branches) and _else.visit(self)
  end,
  
  s-if-pipe(self, l :: Loc, branches :: List<IfPipeBranch>):
    list.all(_.visit(self), branches)
  end,
  s-if-pipe-else(self, l :: Loc, branches :: List<IfPipeBranch>, _else :: Expr):
    list.all(_.visit(self), branches) and _else.visit(self)
  end,
  
  s-cases-branch(self, l :: Loc, name :: String, args :: List<Bind>, body :: Expr):
    list.all(_.visit(self), args) and body.visit(self)
  end,
  
  s-cases(self, l :: Loc, typ :: Ann, val :: Expr, branches :: List<CasesBranch>):
    typ.visit(self) and val.visit(self) and list.all(_.visit(self), branches)
  end,
  s-cases-else(self, l :: Loc, typ :: Ann, val :: Expr, branches :: List<CasesBranch>, _else :: Expr):
    typ.visit(self) and val.visit(self) and list.all(_.visit(self), branches) and _else.visit(self)
  end,
  
  s-try(self, l :: Loc, body :: Expr, id :: Bind, _except :: Expr):
    body.visit(self) and id.visit(self) and _except.visit(self)
  end,
  
  s-op(self, l :: Loc, op :: String, left :: Expr, right :: Expr):
    left.visit(self) and right.visit(self)
  end,
  
  s-check-test(self, l :: Loc, op :: String, left :: Expr, right :: Expr):
    left.visit(self) and right.visit(self)
  end,
  
  s-paren(self, l :: Loc, expr :: Expr):
    expr.visit(self)
  end,
  
  s-lam(
      self,
      l :: Loc,
      params :: List<String>,
      args :: List<Bind>,
      ann :: Ann,
      doc :: String,
      body :: Expr,
      _check :: Option<Expr>
      ):
    list.all(_.visit(self), args) and ann.visit(self) and body.visit(self) and self.option(_check)
  end,
  s-method(
      self,
      l :: Loc,
      args :: List<Bind>, # Value parameters
      ann :: Ann, # return type
      doc :: String,
      body :: Expr,
      _check :: Option<Expr>
      ):
    list.all(_.visit(self), args) and ann.visit(self) and body.visit(self) and self.option(_check)
  end,
  s-extend(self, l :: Loc, supe :: Expr, fields :: List<Member>):
    supe.visit(self) and list.all(_.visit(self), fields)
  end,
  s-update(self, l :: Loc, supe :: Expr, fields :: List<Member>):
    supe.visit(self) and list.all(_.visit(self), fields)
  end,
  s-obj(self, l :: Loc, fields :: List<Member>):
    list.all(_.visit(self), fields)
  end,
  s-list(self, l :: Loc, values :: List<Expr>):
    list.all(_.visit(self), values)
  end,
  s-construct(self, l :: Loc, mod :: ArrayModifier, constructor :: Expr, values :: List<Expr>):
    constructor.visit(self) and list.all(_.visit(self), values)
  end,
  s-app(self, l :: Loc, _fun :: Expr, args :: List<Expr>):
    _fun.visit(self) and list.all(_.visit(self), args)
  end,
  s-prim-app(self, l :: Loc, _fun :: String, args :: List<Expr>):
    list.all(_.visit(self), args)
  end,
  s-prim-val(self, l :: Loc, name :: String):
    true
  end,
  s-id(self, l :: Loc, id :: Name):
    id.visit(self)
  end,
  s-id-var(self, l :: Loc, id :: Name):
    id.visit(self)
  end,
  s-id-letrec(self, l :: Loc, id :: Name, safe :: Boolean):
    id.visit(self)
  end,
  s-undefined(self, l :: Loc):
    true
  end,
  s-num(self, l :: Loc, n :: Number):
    true
  end,
  s-frac(self, l :: Loc, num :: Number, den :: Number):
    true
  end,
  s-bool(self, l :: Loc, b :: Bool):
    true
  end,
  s-str(self, l :: Loc, s :: String):
    true
  end,
  s-dot(self, l :: Loc, obj :: Expr, field :: String):
    obj.visit(self)
  end,
  s-get-bang(self, l :: Loc, obj :: Expr, field :: String):
    obj.visit(self)
  end,
  s-bracket(self, l :: Loc, obj :: Expr, field :: Expr):
    obj.visit(self) and field.visit(self)
  end,
  s-colon(self, l :: Loc, obj :: Expr, field :: String):
    obj.visit(self)
  end,
  s-colon-bracket(self, l :: Loc, obj :: Expr, field :: Expr):
    obj.visit(self) and field.visit(self)
  end,
  s-data(
      self,
      l :: Loc,
      name :: String,
      params :: List<String>, # type params
      mixins :: List<Expr>,
      variants :: List<Variant>,
      shared-members :: List<Member>,
      _check :: Option<Expr>
      ):
    list.all(_.visit(self), mixins) 
    and list.all(_.visit(self), variants)
    and list.all(_.visit(self), shared-members)
    and self.option(_check)
  end,
  s-data-expr(
      self,
      l :: Loc,
      name :: String,
      params :: List<String>, # type params
      mixins :: List<Expr>,
      variants :: List<Variant>,
      shared-members :: List<Member>,
      _check :: Option<Expr>
      ):
    list.all(_.visit(self), mixins)
    and list.all(_.visit(self), variants)
    and list.all(_.visit(self), shared-members)
    and self.option(_check)
  end,
  s-for(
      self,
      l :: Loc,
      iterator :: Expr,
      bindings :: List<ForBind>,
      ann :: Ann,
      body :: Expr
      ):
    iterator.visit(self) and list.all(_.visit(self), bindings) and ann.visit(self) and body.visit(self)
  end,
  s-check(self, l :: Loc, name :: String, body :: Expr, keyword-check :: Bool):
    body.visit(self)
  end,
  
  s-data-field(self, l :: Loc, name :: Expr, value :: Expr):
    name.visit(self) and value.visit(self)
  end,
  s-mutable-field(self, l :: Loc, name :: Expr, ann :: Ann, value :: Expr):
    name.visit(self) and ann.visit(self) and value.visit(self)
  end,
  s-once-field(self, l :: Loc, name :: Expr, ann :: Ann, value :: Expr):
    name.visit(self) and ann.visit(self) and value.visit(self)
  end,
  s-method-field(
      self,
      l :: Loc,
      name :: Expr,
      args :: List<Bind>, # Value parameters
      ann :: Ann, # return type
      doc :: String,
      body :: Expr,
      _check :: Option<Expr>
      ):
    name.visit(self)
    and list.all(_.visit(self), args)
    and ann.visit(self)
    and body.visit(self)
    and self.option(_check)
  end,
  
  s-for-bind(self, l :: Loc, bind :: Bind, value :: Expr):
    bind.visit(self) and value.visit(self)
  end,
  s-variant-member(self, l :: Loc, member-type :: VariantMemberType, bind :: Bind):
    bind.visit(self)
  end,
  s-variant(
      self,
      l :: Loc,
      constr-loc :: Loc,
      name :: String,
      members :: List<VariantMember>,
      with-members :: List<Member>
      ):
    list.all(_.visit(self), members) and list.all(_.visit(self), with-members)
  end,
  s-singleton-variant(
      self,
      l :: Loc,
      name :: String,
      with-members :: List<Member>
      ):
    list.all(_.visit(self), with-members)
  end,
  s-datatype-variant(
      self,
      l :: Loc,
      name :: String,
      members :: List<VariantMember>,
      constructor :: Constructor
      ):
    list.all(_.visit(self), members) and constructor.visit(self)
  end,
  s-datatype-singleton-variant(
      self,
      l :: Loc,
      name :: String,
      constructor :: Constructor
      ):
    constructor.visit(self)
  end,
  s-datatype-constructor(
      self,
      l :: Loc,
      self-arg :: String,
      body :: Expr
      ):
    body.visit(self)
  end,
  a-blank(self):
    true
  end,
  a-any(self):
    true
  end,
  a-name(self, l, id):
    true
  end,
  a-arrow(self, l, args, ret, _):
    list.all(_.visit(self), args) and ret.visit(self)
  end,
  a-method(self, l, args, ret):
    list.all(_.visit(self), args) and ret.visit(self)
  end,
  a-record(self, l, fields):
    list.all(_.visit(self), fields)
  end,
  a-app(self, l, ann, args):
    ann.visit(self) and list.all(_.visit(self), args)
  end,
  a-pred(self, l, ann, exp):
    ann.visit(self) and exp.visit(self)
  end,
  a-dot(self, l, obj, field):
    obj.visit(self)
  end,
  a-field(self, l, name, ann):
    ann.visit(self)
  end
}

dummy-loc-visitor = {
  option(self, opt):
    cases(Option) opt:
      | none => none
      | some(v) => some(v.visit(self))
    end
  end,

  s-underscore(self, l):
    s-underscore(dummy-loc)
  end,
  s-name(self, l, s):
    s-name(dummy-loc, s)
  end,
  s-global(self, s):
    s-global(s)
  end,
  s-atom(self, base, serial):
    s-atom(base, serial)
  end,
  
  s-program(self, l, _provide, imports, body):
    s-program(dummy-loc, _provide.visit(self), imports.map(_.visit(self)), body.visit(self))
  end,

  s-import(self, l, import-type, name):
    s-import(dummy-loc, import-type, name.visit(self))
  end,
  s-import-fields(self, l, fields, import-type):
    s-import-fields(dummy-loc, fields.map(_.visit(self)), import-type)
  end,
  s-provide(self, l, expr):
    s-provide(dummy-loc, expr.visit(self))
  end,
  s-provide-all(self, l):
    s-provide-all(l)
  end,
  s-provide-none(self, l):
    s-provide-none(l)
  end,

  s-bind(self, l, shadows, name, ann):
    s-bind(dummy-loc, shadows, name.visit(self), ann.visit(self))
  end,

  s-var-bind(self, l, bind, expr):
    s-var-bind(dummy-loc, bind.visit(self), expr.visit(self))
  end,
  s-let-bind(self, l, bind, expr):
    s-let-bind(dummy-loc, bind.visit(self), expr.visit(self))
  end,

  s-let-expr(self, l, binds, body):
    s-let-expr(dummy-loc, binds.map(_.visit(self)), body.visit(self))
  end,

  s-letrec-bind(self, l, bind, expr):
    s-letrec-bind(dummy-loc, bind.visit(self), expr.visit(self))
  end,

  s-letrec(self, l, binds, body):
    s-letrec(dummy-loc, binds.map(_.visit(self)), body.visit(self))
  end,

  s-hint-exp(self, l :: Loc, hints :: List<Hint>, exp :: Expr):
    s-hint-exp(dummy-loc, hints, exp.visit(self))
  end,

  s-instantiate(self, l :: Loc, expr :: Expr, params :: List<Ann>):
    s-instantiate(dummy-loc, expr.visit(self), params.map(_.visit(self)))
  end,

  s-block(self, l, stmts):
    s-block(dummy-loc, stmts.map(_.visit(self)))
  end,

  s-user-block(self, l :: Loc, body :: Expr):
    s-user-block(dummy-loc, body.visit(self))
  end,

  s-fun(self, l, name, params, args, ann, doc, body, _check):
    s-fun(dummy-loc, name, params, args.map(_.visit(self)), ann.visit(self), doc, body.visit(self), self.option(_check))
  end,

  s-var(self, l :: Loc, name :: Bind, value :: Expr):
    s-var(dummy-loc, name.visit(self), value.visit(self))
  end,

  s-let(self, l :: Loc, name :: Bind, value :: Expr, keyword-val :: Bool):
    s-let(dummy-loc, name.visit(self), value.visit(self), keyword-val) 
  end,

  s-graph(self, l :: Loc, bindings :: List<is-s-let>):
    s-graph(dummy-loc, bindings.map(_.visit(self)))
  end,

  s-when(self, l :: Loc, test :: Expr, block :: Expr):
    s-when(dummy-loc, test.visit(self), block.visit(self))
  end,

  s-contract(self, l, name, ann):
    s-contract(dummy-loc, name.visit(self), ann.visit(self))
  end,

  s-assign(self, l :: Loc, id :: String, value :: Expr):
    s-assign(dummy-loc, id, value.visit(self))
  end,

  s-if-branch(self, l :: Loc, test :: Expr, body :: Expr):
    s-if-branch(dummy-loc, test.visit(self), body.visit(self))
  end,

  s-if-pipe-branch(self, l :: Loc, test :: Expr, body :: Expr):
    s-if-pipe-branch(dummy-loc, test.visit(self), body.visit(self))
  end,

  s-if(self, l :: Loc, branches :: List<IfBranch>):
    s-if(dummy-loc, branches.map(_.visit(self)))
  end,
  s-if-else(self, l :: Loc, branches :: List<IfBranch>, _else :: Expr):
    s-if-else(dummy-loc, branches.map(_.visit(self)), _else.visit(self))
  end,
  
  s-if-pipe(self, l :: Loc, branches :: List<IfPipeBranch>):
    s-if-pipe(dummy-loc, branches.map(_.visit(self)))
  end,
  s-if-pipe-else(self, l :: Loc, branches :: List<IfPipeBranch>, _else :: Expr):
    s-if-pipe-else(dummy-loc, branches.map(_.visit(self)), _else.visit(self))
  end,

  s-cases-branch(self, l :: Loc, name :: String, args :: List<Bind>, body :: Expr):
    s-cases-branch(dummy-loc, name, args.map(_.visit(self)), body.visit(self))
  end,

  s-cases(self, l :: Loc, typ :: Ann, val :: Expr, branches :: List<CasesBranch>):
    s-cases(dummy-loc, typ, val.visit(self), branches.map(_.visit(self)))
  end,
  s-cases-else(self, l :: Loc, typ :: Ann, val :: Expr, branches :: List<CasesBranch>, _else :: Expr):
    s-cases-else(dummy-loc, typ, val.visit(self), branches.map(_.visit(self)), _else.visit(self))
  end,

  s-try(self, l :: Loc, body :: Expr, id :: Bind, _except :: Expr):
    s-try(dummy-loc, body.visit(self), id.visit(self), _except.visit(self))
  end,

  s-op(self, l :: Loc, op :: String, left :: Expr, right :: Expr):
    s-op(dummy-loc, op, left.visit(self), right.visit(self))
  end,

  s-check-test(self, l :: Loc, op :: String, left :: Expr, right :: Expr):
    s-check-test(dummy-loc, op, left.visit(self), right.visit(self))
  end,

  s-paren(self, l :: Loc, expr :: Expr):
    s-paren(dummy-loc, expr.visit(self))
  end,

  s-lam(
      self,
      l :: Loc,
      params :: List<String>,
      args :: List<Bind>,
      ann :: Ann,
      doc :: String,
      body :: Expr,
      _check :: Option<Expr>
    ):
    s-lam(dummy-loc, params, args.map(_.visit(self)), ann.visit(self), doc, body.visit(self), self.option(_check))
  end,
  s-method(
      self,
      l :: Loc,
      args :: List<Bind>, # Value parameters
      ann :: Ann, # return type
      doc :: String,
      body :: Expr,
      _check :: Option<Expr>
    ):
    s-method(dummy-loc, args.map(_.visit(self)), ann.visit(self), doc, body.visit(self), self.option(_check))
  end,
  s-extend(self, l :: Loc, supe :: Expr, fields :: List<Member>):
    s-extend(dummy-loc, supe.visit(self), fields.map(_.visit(self)))
  end,
  s-update(self, l :: Loc, supe :: Expr, fields :: List<Member>):
    s-update(dummy-loc, supe.visit(self), fields.map(_.visit(self)))
  end,
  s-obj(self, l :: Loc, fields :: List<Member>):
    s-obj(dummy-loc, fields.map(_.visit(self)))
  end,
  s-list(self, l :: Loc, values :: List<Expr>):
    s-list(dummy-loc, values.map(_.visit(self)))
  end,
  s-construct(self, l :: Loc, mod :: ArrayModifier, constructor :: Expr, values :: List<Expr>):
    s-construct(dummy-loc, mod, constructor.visit(self), values.map(_.visit(self)))
  end,
  s-app(self, l :: Loc, _fun :: Expr, args :: List<Expr>):
    s-app(dummy-loc, _fun.visit(self), args.map(_.visit(self)))
  end,
  s-prim-app(self, l :: Loc, _fun :: String, args :: List<Expr>):
    s-prim-app(dummy-loc, _fun, args.map(_.visit(self)))
  end,
  s-prim-val(self, l :: Loc, name :: String):
    s-prim-val(dummy-loc, name)
  end,
  s-id(self, l :: Loc, id :: Name):
    s-id(dummy-loc, id.visit(self))
  end,
  s-id-var(self, l :: Loc, id :: Name):
    s-id-var(dummy-loc, id.visit(self))
  end,
  s-id-letrec(self, l :: Loc, id :: Name, safe :: Boolean):
    s-id-letrec(dummy-loc, id.visit(self), safe)
  end,
  s-undefined(self, l :: Loc):
    s-undefined(self)
  end,
  s-num(self, l :: Loc, n :: Number):
    s-num(dummy-loc, n)
  end,
  s-frac(self, l :: Loc, num :: Number, den :: Number):
    s-frac(dummy-loc, num, den)
  end,
  s-bool(self, l :: Loc, b :: Bool):
    s-bool(dummy-loc, b)
  end,
  s-str(self, l :: Loc, s :: String):
    s-str(dummy-loc, s)
  end,
  s-dot(self, l :: Loc, obj :: Expr, field :: String):
    s-dot(dummy-loc, obj.visit(self), field)
  end,
  s-get-bang(self, l :: Loc, obj :: Expr, field :: String):
    s-get-bang(dummy-loc, obj.visit(self), field)
  end,
  s-bracket(self, l :: Loc, obj :: Expr, field :: Expr):
    s-bracket(dummy-loc, obj.visit(self), field.visit(self))
  end,
  s-colon(self, l :: Loc, obj :: Expr, field :: String):
    s-colon(dummy-loc, obj.visit(self), field)
  end,
  s-colon-bracket(self, l :: Loc, obj :: Expr, field :: Expr):
    s-colon-bracket(dummy-loc, obj.visit(self), field.visit(self))
  end,
  s-data(
      self,
      l :: Loc,
      name :: String,
      params :: List<String>, # type params
      mixins :: List<Expr>,
      variants :: List<Variant>,
      shared-members :: List<Member>,
      _check :: Option<Expr>
    ):
    s-data(
        dummy-loc,
        name,
        params,
        mixins.map(_.visit(self)),
        variants.map(_.visit(self)),
        shared-members.map(_.visit(self)),
        self.option(_check)
      )
  end,
  s-data-expr(
      self,
      l :: Loc,
      name :: String,
      params :: List<String>, # type params
      mixins :: List<Expr>,
      variants :: List<Variant>,
      shared-members :: List<Member>,
      _check :: Option<Expr>
    ):
    s-data-expr(
        dummy-loc,
        name,
        params,
        mixins.map(_.visit(self)),
        variants.map(_.visit(self)),
        shared-members.map(_.visit(self)),
        self.option(_check)
      )
  end,
  s-for(
      self,
      l :: Loc,
      iterator :: Expr,
      bindings :: List<ForBind>,
      ann :: Ann,
      body :: Expr
    ):
    s-for(dummy-loc, iterator.visit(self), bindings.map(_.visit(self)), ann, body.visit(self))
  end,
  s-check(self, l :: Loc, name :: Option<String>, body :: Expr, keyword-check :: Bool):
    s-check(dummy-loc, name, body.visit(self), keyword-check)
  end,

  s-data-field(self, l :: Loc, name :: Expr, value :: Expr):
    s-data-field(dummy-loc, name, value.visit(self))
  end,
  s-mutable-field(self, l :: Loc, name :: Expr, ann :: Ann, value :: Expr):
    s-mutable-field(dummy-loc, name, ann, value.visit(self))
  end,
  s-once-field(self, l :: Loc, name :: Expr, ann :: Ann, value :: Expr):
    s-once-field(dummy-loc, name, ann, value.visit(self))
  end,
  s-method-field(
      self,
      l :: Loc,
      name :: Expr,
      args :: List<Bind>, # Value parameters
      ann :: Ann, # return type
      doc :: String,
      body :: Expr,
      _check :: Option<Expr>
    ):
    s-method-field(
        dummy-loc,
        name,
        args.map(_.visit(self)),
        ann.visit(self),
        doc,
        body.visit(self),
        self.option(_check)
      )
  end,

  s-for-bind(self, l :: Loc, bind :: Bind, value :: Expr):
    s-for-bind(dummy-loc, bind.visit(self), value.visit(self))   
  end,
  s-variant-member(self, l :: Loc, member-type :: VariantMemberType, bind :: Bind):
    s-variant-member(dummy-loc, member-type, bind.visit(self))
  end,
  s-variant(
      self,
      l :: Loc,
      constr-loc :: Loc,
      name :: String,
      members :: List<VariantMember>,
      with-members :: List<Member>
    ):
    s-variant(dummy-loc, dummy-loc, name, members.map(_.visit(self)), with-members.map(_.visit(self)))
  end,
  s-singleton-variant(
      self,
      l :: Loc,
      name :: String,
      with-members :: List<Member>
    ):
    s-singleton-variant(dummy-loc, name, with-members.map(_.visit(self)))
  end,
  s-datatype-variant(
      self,
      l :: Loc,
      name :: String,
      members :: List<VariantMember>,
      constructor :: Constructor
    ):
    s-datatype-variant(dummy-loc, name, members.map(_.visit(self)), constructor.visit(self))
  end,
  s-datatype-singleton-variant(
      self,
      l :: Loc,
      name :: String,
      constructor :: Constructor
    ):
    s-datatype-singleton-variant(dummy-loc, name, constructor.visit(self))
  end,
  s-datatype-constructor(
      self,
      l :: Loc,
      self-arg :: String,
      body :: Expr
      ):
    s-datatype-constructor(dummy-loc, self-arg, body.visit(self))
  end,

  a-blank(self): a-blank end,
  a-any(self): a-any end,
  a-name(self, l, id): a-name(dummy-loc, id.visit(self)) end,
  a-arrow(self, l, args, ret, use-parens):
    a-arrow(dummy-loc, args.map(_.visit(self)), ret.visit(self), use-parens)
  end,
  a-method(self, l, args, ret):
    a-method(dummy-loc, args.map(_.visit(self)), ret.visit(self))
  end,
  a-record(self, l, fields):
    a-record(dummy-loc, fields.map(_.visit(self)))
  end,
  a-app(self, l, ann, args):
    a-app(dummy-loc, ann.visit(self), args.map(_.visit(self)))
  end,
  a-pred(self, l, ann, exp):
    a-pred(dummy-loc, ann.visit(self), exp.visit(self))
  end,
  a-dot(self, l, obj, field):
    a-dot(dummy-loc, obj, field)
  end,
  a-field(self, l, name, ann):
    a-field(dummy-loc, name.visit(self), ann.visit(self))
  end
}

fun build-loc(l):
  cases(S.Srcloc) l:
    | srcloc(source, start-line, start-column, start-char, end-line, end-column, end-char) =>
      s-obj(l, [
          s-data-field(l, s-str(l, "source"), s-str(l, source)),
          s-data-field(l, s-str(l, "start-line"), s-num(l, start-line)),
          s-data-field(l, s-str(l, "start-column"), s-num(l, start-column)),
          s-data-field(l, s-str(l, "start-char"), s-num(l, start-char)),
          s-data-field(l, s-str(l, "end-line"), s-num(l, end-line)),
          s-data-field(l, s-str(l, "end-column"), s-num(l, end-column)),
          s-data-field(l, s-str(l, "end-char"), s-num(l, end-char))
        ])
  end
end
