#lang pyret

provide *
import pprint as PP
import srcloc as S

Loc = S.Srcloc
loc = S.srcloc

dummy-loc = S.builtin("dummy location")

INDENT = 2

break-one = PP.sbreak(1)
str-any = PP.str("Any")
str-arrow = PP.str("->")
str-arrowspace = PP.str("-> ")
str-as = PP.str("as")
str-blank = PP.str("")
str-let = PP.str("let")
str-type-let = PP.str("type-let")
str-letrec = PP.str("letrec")
str-block = PP.str("block:")
str-brackets = PP.str("[list: ]")
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
str-lam = PP.str("lam")
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
str-provide-types = PP.str("provide-types")
str-provide-star = PP.str("provide *")
str-provide-types-star = PP.str("provide-types *")
str-sharing = PP.str("sharing:")
str-space = PP.str(" ")
str-spacecolonequal = PP.str(" :=")
str-spaceequal = PP.str(" =")
str-thencolon = PP.str("then:")
str-thickarrow = PP.str("=>")
str-try = PP.str("try:")
str-use-loc = PP.str("UseLoc")
str-var = PP.str("var ")
str-newtype = PP.str("type ")
str-type = PP.str("type ")
str-bless = PP.str("bless ")
str-confirm = PP.str("confirm ")
str-val = PP.str("val ")
str-when = PP.str("when")
str-where = PP.str("where:")
str-with = PP.str("with:")

data Name:
  | s-underscore(l :: S.Location) with:
    to-compiled-source(self): raise("Cannot compile underscores") end,
    to-compiled(self): raise("Cannot compile underscores") end,
    tosource(self): PP.str("_") end,
    tostring(self): "_" end,
    toname(self): raise("Cannot get name for underscore") end,
    key(self): "underscore#" end

  | s-name(l :: S.Location, s :: String) with:
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
    self._match(visitor, lam(): raise("No visitor field for " + tostring(self)) end)
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
      args.map(lam(a): a.tosource() end)))
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
  | s-program(l :: S.Location, _provide :: Provide, provided-types :: ProvideTypes, imports :: List<Import>, block :: Expr) with:
    label(self): "s-program" end,
    tosource(self):
      PP.group(
        PP.vert(
          [list:
            self._provide.tosource(),
            self.provided-types.tosource()]
            + self.imports.map(_.tosource)
            + [list: self.block.tosource()]
          ))
    end
sharing:
  visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end
end

data Import:
  | s-import(l :: S.Location, file :: ImportType, name :: Name) with:
    label(self): "s-import" end,
    tosource(self):
      PP.flow([list: str-import, self.file.tosource(), str-as, self.name.tosource()])
    end
  | s-import-types(l :: S.Location, file :: ImportType, name :: Name, types :: Name) with:
    label(self): "s-import-types" end,
    tosource(self):
      PP.flow([list: str-import, self.file.tosource(), str-as, self.name.tosource(), PP.comma, self.types.tosource()])
    end
  | s-import-fields(l :: S.Location, fields :: List<Name>, file :: ImportType) with:
    label(self): "s-import-fields" end,
    tosource(self):
      PP.flow([list: str-import,
          PP.flow-map(PP.commabreak, _.tosource(), self.fields),
          str-from, self.file.tosource()])
    end
sharing:
  visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end
end

data Provide:
  | s-provide(l :: S.Location, block :: Expr) with:
    label(self): "s-provide" end,
    tosource(self):
      PP.soft-surround(INDENT, 1, str-provide,
        self.block.tosource(), str-end)
    end
  | s-provide-all(l :: S.Location) with:
    label(self): "s-provide-all" end,
    tosource(self): str-provide-star end
  | s-provide-none(l :: S.Location) with:
    label(self): "s-provide-none" end,
    tosource(self): PP.mt-doc end
sharing:
  visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end
end

data ProvideTypes:
  | s-provide-types(l :: S.Location, ann :: List<AField>) with:
    label(self): "a-provide-type" end,
    tosource(self):
      PP.surround-separate(INDENT, 1, str-provide-types + break-one + PP.lbrace + PP.rbrace,
        str-provide-types + break-one + PP.lbrace, PP.commabreak, PP.rbrace,
        self.ann.map(_.tosource()))
    end
  | s-provide-types-all(l :: S.Location) with:
    label(self): "s-provide-types-all" end,
    tosource(self): str-provide-types-star end
  | s-provide-types-none(l :: S.Location) with:
    label(self): "s-provide-types-none" end,
    tosource(self): PP.mt-doc end
sharing:
  visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end
end
  
  
data ImportType:
  | s-file-import(l :: S.Location, file :: String) with:
    label(self): "s-file-import" end,
    tosource(self): PP.str(torepr(self.file)) end
  | s-const-import(l :: S.Location, mod :: String) with:
    label(self): "s-const-import" end,
    tosource(self): PP.str(self.mod) end
sharing:
  visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end
end

data Hint:
  | h-use-loc(l :: S.Location) with:
    tosource(self): str-use-loc + PP.parens(PP.str(tostring(self.l))) end
sharing:
  visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end
end

data LetBind:
  | s-let-bind(l :: S.Location, b :: Bind, value :: Expr) with:
    tosource(self):
      PP.group(PP.nest(INDENT, self.b.tosource() + str-spaceequal + break-one + self.value.tosource()))
    end
  | s-var-bind(l :: S.Location, b :: Bind, value :: Expr) with:
    tosource(self):
      PP.group(PP.nest(INDENT, PP.str("var ") + self.b.tosource() + str-spaceequal + break-one + self.value.tosource()))
    end
sharing:
  visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end
end

data LetrecBind:
  | s-letrec-bind(l :: S.Location, b :: Bind, value :: Expr) with:
    tosource(self):
      PP.group(PP.nest(INDENT, self.b.tosource() + str-spaceequal + break-one + self.value.tosource()))
    end
sharing:
  visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end
end

data TypeLetBind:
  | s-type-bind(l :: S.Location, name :: Name, ann :: Ann) with:
    label(self): "s-type-bind" end,
    tosource(self):
      PP.group(PP.nest(INDENT, self.name.tosource() + str-spaceequal + break-one + self.ann.tosource()))
    end
  | s-newtype-bind(l :: S.Location, name :: Name, namet :: Name) with:
    label(self): "s-newtype-bind" end,
    tosource(self):
      PP.group(PP.nest(INDENT, str-newtype + self.name.tosource()
          + break-one + str-as
          + break-one + self.namet.tosource()))
    end
sharing:
  visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end
end


data Expr:
  | s-module(l :: S.Location, answer :: Expr, provides :: Expr, types :: List<AField>, checks :: Expr) with:
    label(self): "s-module" end,
    tosource(self):
      PP.str("Module") + PP.parens(PP.flow-map(PP.commabreak, lam(x): x end, [list:
            PP.infix(INDENT, 1, str-colon, PP.str("Answer"), self.answer.tosource()),
            PP.infix(INDENT, 1, str-colon, PP.str("Provides"), self.provides.tosource()),
            PP.infix(INDENT, 1, str-colon,PP.str("Types"), 
              PP.brackets(PP.flow-map(PP.commabreak, _.tosource(), self.types))),
            PP.infix(INDENT, 1, str-colon, PP.str("checks"), self.checks.tosource())]))
    end
  | s-type-let-expr(l :: S.Location, binds :: List<TypeLetBind>, body :: Expr) with:
    label(self): "s-type-let" end,
    tosource(self):
      header = PP.surround-separate(2 * INDENT, 1, str-type-let, str-type-let + PP.str(" "), PP.commabreak, PP.mt-doc,
          self.binds.map(_.tosource()))
          + str-colon
      PP.surround(INDENT, 1, header, self.body.tosource(), str-end)
    end
  | s-let-expr(l :: S.Location, binds :: List<LetBind>, body :: Expr) with:
    label(self): "s-let" end,
    tosource(self):
      header = PP.surround-separate(2 * INDENT, 1, str-let, str-let + PP.str(" "), PP.commabreak, PP.mt-doc,
          self.binds.map(_.tosource()))
          + str-colon
      PP.surround(INDENT, 1, header, self.body.tosource(), str-end)
    end
  | s-letrec(l :: S.Location, binds :: List<LetrecBind>, body :: Expr) with:
    label(self): "s-letrec" end,
    tosource(self):
      header = PP.surround-separate(2 * INDENT, 1, str-letrec, str-letrec + PP.str(" "), PP.commabreak, PP.mt-doc,
          self.binds.map(_.tosource()))
          + str-colon
      PP.surround(INDENT, 1, header, self.body.tosource(), str-end)
    end
  | s-hint-exp(l :: S.Location, hints :: List<Hint>, exp :: Expr) with:
    label(self): "s-hint-exp" end,
    tosource(self):
      PP.flow-map(PP.hardline, lam(h): str-comment + h.tosource() end, self.hints) + PP.hardline
        + self.e.tosource()
    end
  | s-instantiate(l :: S.Location, expr :: Expr, params :: List<Ann>) with:
    label(self): "s-instantiate" end,
    tosource(self):
      PP.group(self.expr.tosource() +
        PP.surround-separate(INDENT, 0, PP.mt-doc, PP.langle, PP.commabreak, PP.rangle,
            self.params.map(_.tosource)))
    end
  | s-block(l :: S.Location, stmts :: List<Expr>) with:
    label(self): "s-block" end,
    tosource(self):
      PP.flow-map(PP.hardline, _.tosource(), self.stmts)
    end
  | s-user-block(l :: S.Location, body :: Expr) with:
    label(self): "s-user-block" end,
    tosource(self):
      PP.surround(INDENT, 1, str-block, self.body.tosource(), str-end)
    end
  | s-fun(
      l :: S.Location,
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
  | s-type(l :: S.Location, name :: Name, ann :: Ann) with:
    label(self): "s-type" end,
    tosource(self):
      PP.group(PP.nest(INDENT,
          str-type + self.name.tosource() + str-spaceequal + break-one + self.ann.tosource()))
    end
  | s-newtype(l :: S.Location, name :: Name, namet :: Name) with:
    label(self): "s-newtype" end,
    tosource(self):
      PP.group(PP.nest(INDENT, str-newtype + self.name.tosource()
          + break-one + str-as
          + break-one + self.namet.tosource()))
    end
  | s-var(l :: S.Location, name :: Bind, value :: Expr) with:
    label(self): "s-var" end,
    tosource(self):
      str-var
        + PP.group(PP.nest(INDENT, self.name.tosource()
            + str-spaceequal + break-one + self.value.tosource()))
    end
  | s-let(l :: S.Location, name :: Bind, value :: Expr, keyword-val :: Boolean) with:
    label(self): "s-let" end,
    tosource(self):
      PP.group(PP.nest(INDENT,
          if self.keyword-val: str-val else: PP.mt-doc end
            + self.name.tosource() + str-spaceequal + break-one + self.value.tosource()))
    end
  | s-graph(l :: S.Location, bindings :: List<Expr%(is-s-let)>) with:
    label(self): "s-graph" end,
    tosource(self):
      PP.soft-surround(0, 1, # NOTE: Not indented
        str-graph,
        PP.flow-map(PP.hardline, _.tosource(), self.bindings),
        str-end)
    end
  | s-contract(l :: S.Location, name :: Name, ann :: Ann) with:
    label(self): "s-contract" end,
    tosource(self):
      PP.infix(INDENT, 1, str-coloncolon, self.name.tosource(), self.ann.tosource())
    end
  | s-when(l :: S.Location, test :: Expr, block :: Expr) with:
    label(self): "s-when" end,
    tosource(self):
      PP.soft-surround(INDENT, 1,
        str-when + PP.parens(self.test.tosource()) + str-colon,
        self.block.tosource(),
        str-end)
    end
  | s-assign(l :: S.Location, id :: Name, value :: Expr) with:
    label(self): "s-assign" end,
    tosource(self):
      PP.group(PP.nest(INDENT, self.id.tosource() + str-spacecolonequal + break-one + self.value.tosource()))
    end
  | s-if-pipe(l :: S.Location, branches :: List<IfPipeBranch>) with:
    label(self): "s-if-pipe" end,
    tosource(self):
      PP.surround-separate(INDENT, 1, str-askcolon + str-space + str-end,
        PP.group(str-askcolon), break-one, str-end,
        self.branches.map(lam(b): PP.group(b.tosource()) end))
    end
  | s-if-pipe-else(l :: S.Location, branches :: List<IfPipeBranch>, _else :: Expr) with:
    label(self): "s-if-pipe-else" end,
    tosource(self):
      body = PP.separate(break-one, self.branches.map(lam(b): PP.group(b.tosource()) end))
        + break-one + PP.group(str-pipespace + str-otherwisecolon + break-one + self._else.tosource())
      PP.surround(INDENT, 1, PP.group(str-askcolon), body, str-end)
    end
  | s-if(l :: S.Location, branches :: List<IfBranch>) with:
    label(self): "s-if" end,
    tosource(self):
      branches = PP.separate(break-one + str-elsespace,
        self.branches.map(lam(b): b.tosource() end))
      PP.group(branches + break-one + str-end)
    end
  | s-if-else(l :: S.Location, branches :: List<IfBranch>, _else :: Expr) with:
    label(self): "s-if-else" end,
    tosource(self):
      branches = PP.separate(break-one + str-elsespace,
        self.branches.map(lam(b): b.tosource() end))
      _else = str-elsecolon + PP.nest(INDENT, break-one + self._else.tosource())
      PP.group(branches + break-one + _else + break-one + str-end)
    end
  | s-cases(l :: S.Location, typ :: Ann, val :: Expr, branches :: List<CasesBranch>) with:
    label(self): "s-cases" end,
    tosource(self):
      header = str-cases + PP.parens(self.typ.tosource()) + break-one
        + self.val.tosource() + str-colon
      PP.surround-separate(INDENT, 1, header + str-space + str-end,
        PP.group(header), break-one, str-end,
        self.branches.map(lam(b): PP.group(b.tosource()) end))
    end
  | s-cases-else(l :: S.Location, typ :: Ann, val :: Expr, branches :: List<CasesBranch>, _else :: Expr) with:
    label(self): "s-cases-else" end,
    tosource(self):
      header = str-cases + PP.parens(self.typ.tosource()) + break-one
        + self.val.tosource() + str-colon
      body = PP.separate(break-one, self.branches.map(lam(b): PP.group(b.tosource()) end))
        + break-one + PP.group(str-elsebranch + break-one + self._else.tosource())
      PP.surround(INDENT, 1, PP.group(header), body, str-end)
    end
  | s-try(l :: S.Location, body :: Expr, id :: Bind, _except :: Expr) with:
    label(self): "s-try" end,
    tosource(self):
      _try = str-try + break-one
        + PP.nest(INDENT, self.body.tosource()) + break-one
      _except = str-except + PP.parens(self.id.tosource()) + str-colon + break-one
        + PP.nest(INDENT, self._except.tosource()) + break-one
      PP.group(_try + _except + str-end)
    end
  | s-op(l :: S.Location, op :: String, left :: Expr, right :: Expr) with:
    # This should be left-associated, always.
    label(self): "s-op" end,
    tosource(self):
      fun collect-same-operands(exp):
        if is-s-op(exp) and (exp.op == self.op):
          collect-same-operands(exp.left) + collect-same-operands(exp.right)
        else:
          [list: exp]
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
              nested = for lists.fold(acc from second.tosource(), operand from rest2):
                acc + PP.group(op + operand.tosource())
              end
              PP.group(first.tosource() + op + PP.nest(INDENT, nested))
          end
      end
    end
  | s-check-test(l :: S.Location, op :: String, left :: Expr, right :: Expr) with:
    tosource(self): PP.infix(INDENT, 1, PP.str(string-substring(self.op, 2, string-length(self.op))), self.left.tosource(), self.right.tosource()) end
  | s-paren(l :: S.Location, expr :: Expr) with:
    label(self): "s-paren" end,
    tosource(self): PP.parens(self.expr.tosource()) end
  | s-lam(
      l :: S.Location,
      params :: List<String>, # Type parameters
      args :: List<Bind>, # Value parameters
      ann :: Ann, # return type
      doc :: String,
      body :: Expr,
      _check :: Option<Expr>
    ) with:
    label(self): "s-lam" end,
    tosource(self):
      funlam-tosource(str-lam,
        nothing, self.params, self.args, self.ann, self.doc, self.body, self._check)
    end
  | s-method(
      l :: S.Location,
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
  | s-extend(l :: S.Location, supe :: Expr, fields :: List<Member>) with:
    label(self): "s-extend" end,
    tosource(self):
      PP.group(self.supe.tosource() + str-period
          + PP.surround-separate(INDENT, 1, PP.lbrace + PP.rbrace,
          PP.lbrace, PP.commabreak, PP.rbrace, self.fields.map(_.tosource())))
    end
  | s-update(l :: S.Location, supe :: Expr, fields :: List<Member>) with:
    label(self): "s-update" end,
  | s-obj(l :: S.Location, fields :: List<Member>) with:
    label(self): "s-obj" end,
    tosource(self):
      PP.surround-separate(INDENT, 1, PP.lbrace + PP.rbrace,
        PP.lbrace, PP.commabreak, PP.rbrace, self.fields.map(_.tosource()))
    end
  | s-array(l :: S.Location, values :: List<Expr>) with:
    label(self): "s-array" end,
    tosource(self):
      PP.surround-separate(INDENT, 0, PP.str("[raw-array: ]"), PP.str("[raw-array: "), PP.commabreak, PP.rbrack,
        self.values.map(_.tosource()))
    end
  | s-construct(l :: S.Location, modifier :: ConstructModifier, constructor :: Expr, values :: List<Expr>) with:
    label(self): "s-construct" end,
    tosource(self):
      prefix = PP.lbrack
        + PP.group(PP.separate(PP.sbreak(1), [list: self.modifier.tosource(), self.constructor.tosource()]))
        + str-colonspace
      if is-empty(self.values): prefix + PP.rbrack
      else:
        PP.surround(INDENT, 0, prefix, PP.separate(PP.commabreak, self.values.map(_.tosource())), PP.rbrack)
      end
    end
  | s-confirm(l :: S.Location, expr :: Expr, typ :: Name) with:
    label(self): "s-confirm" end,
    tosource(self):
      PP.flow([list: str-confirm, self.expr.tosource(), str-as, self.typ.tosource()])
    end
  | s-bless(l :: S.Location, expr :: Expr, typ :: Name) with:
    label(self): "s-bless" end,
    tosource(self):
      PP.flow([list: str-bless, self.expr.tosource(), str-as, self.typ.tosource()])
    end
  | s-app(l :: S.Location, _fun :: Expr, args :: List<Expr>) with:
    label(self): "s-app" end,
    tosource(self):
      PP.group(self._fun.tosource()
          + PP.parens(PP.nest(INDENT,
            PP.separate(PP.commabreak, self.args.map(_.tosource())))))
    end
  | s-prim-app(l :: S.Location, _fun :: String, args :: List<Expr>) with:
    label(self): "s-prim-app" end,
    tosource(self):
      PP.group(PP.str(self._fun)
          + PP.parens(PP.nest(INDENT,
            PP.separate(PP.commabreak, self.args.map(_.tosource())))))
    end
  | s-prim-val(l :: S.Location, name :: String) with:
    label(self): "s-prim-val" end,
    tosource(self): PP.str(self.name) end
  | s-id(l :: S.Location, id :: Name) with:
    label(self): "s-id" end,
    tosource(self): self.id.tosource() end
  | s-id-var(l :: S.Location, id :: Name) with:
    label(self): "s-id-var" end,
    tosource(self): PP.str("!") + self.id.tosource() end
  | s-id-letrec(l :: S.Location, id :: Name, safe :: Boolean) with:
    label(self): "s-id-letrec" end,
    tosource(self): PP.str("~") + self.id.tosource() end
  | s-undefined(l :: S.Location) with:
    label(self): "s-undefined" end,
    tosource(self): PP.str("undefined") end
  | s-srcloc(l :: S.Location, loc :: S.Location) with:
    label(self): "s-srcloc" end,
    tosource(self): PP.str(torepr(self.loc)) end
  | s-num(l :: S.Location, n :: Number) with:
    label(self): "s-num" end,
    tosource(self): PP.number(self.n) end
  | s-frac(l :: S.Location, num :: Number, den :: Number) with:
    label(self): "s-frac" end,
    tosource(self): PP.number(self.num) + PP.str("/") + PP.number(self.den) end
  | s-bool(l :: S.Location, b :: Boolean) with:
    label(self): "s-bool" end,
    tosource(self): PP.str(tostring(self.b)) end
  | s-str(l :: S.Location, s :: String) with:
    label(self): "s-str" end,
    tosource(self): PP.str(torepr(self.s)) end
  | s-dot(l :: S.Location, obj :: Expr, field :: String) with:
    label(self): "s-dot" end,
    tosource(self): PP.infix-break(INDENT, 0, str-period, self.obj.tosource(), PP.str(self.field)) end
  | s-get-bang(l :: S.Location, obj :: Expr, field :: String) with:
    label(self): "s-get-bang" end,
    tosource(self): PP.infix-break(INDENT, 0, str-bang, self.obj.tosource(), PP.str(self.field)) end
  | s-bracket(l :: S.Location, obj :: Expr, field :: Expr) with:
    label(self): "s-bracket" end,
    tosource(self): PP.infix-break(INDENT, 0, str-period, self.obj.tosource(),
        PP.surround(INDENT, 0, PP.lbrack, self.field.tosource(), PP.rbrack))
    end
  | s-data(
      l :: S.Location,
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
        PP.surround-separate(INDENT, 0, PP.mt-doc, break-one + str-deriving, PP.commabreak, PP.mt-doc, self.mixins.map(lam(m): m.tosource() end))
      variants = PP.separate(break-one + str-pipespace,
        str-blank ^ lists.link(_, self.variants.map(lam(v): PP.nest(INDENT, v.tosource()) end)))
      shared = optional-section(str-sharing,
        PP.separate(PP.commabreak, self.shared-members.map(lam(s): s.tosource() end)))
      _check = cases(Option) self._check:
        | none => PP.mt-doc
        | some(chk) => optional-section(str-where, chk.tosource())
      end
      footer = break-one + str-end
      header + _deriving + PP.group(PP.nest(INDENT, variants) + shared + _check + footer)
    end
  | s-data-expr(
      l :: S.Location,
      name :: String,
      namet :: Name,
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
        PP.surround-separate(INDENT, 0, PP.mt-doc, break-one + str-deriving, PP.commabreak, PP.mt-doc, self.mixins.map(lam(m): m.tosource() end))
      variants = PP.separate(break-one + str-pipespace,
        str-blank ^ lists.link(_, self.variants.map(lam(v): PP.nest(INDENT, v.tosource()) end)))
      shared = optional-section(str-sharing,
        PP.separate(PP.commabreak, self.shared-members.map(lam(s): s.tosource() end)))
      _check = cases(Option) self._check:
        | none => PP.mt-doc
        | some(chk) => optional-section(str-where, chk.tosource())
      end
      footer = break-one + str-end
      header + _deriving + PP.group(PP.nest(INDENT, variants) + shared + _check + footer)
    end
  | s-for(
      l :: S.Location,
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
          self.bindings.map(lam(b): b.tosource() end))
          + PP.group(PP.nest(2 * INDENT,
            break-one + str-arrow + break-one + self.ann.tosource() + str-colon)))
      PP.surround(INDENT, 1, header, self.body.tosource(), str-end)
    end
  | s-check(
      l :: S.Location,
      name :: Option<String>,
      body :: Expr,
      keyword-check :: Boolean
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
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
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
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end
end
    

data Bind:
  | s-bind(l :: S.Location, shadows :: Boolean, id :: Name, ann :: Ann) with:
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
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end
end

data Member:
  | s-data-field(l :: S.Location, name :: Expr, value :: Expr) with:
    label(self): "s-data-field" end,
    tosource(self): PP.nest(INDENT, self.name.tosource() + str-colonspace + self.value.tosource()) end,
  | s-mutable-field(l :: S.Location, name :: Expr, ann :: Ann, value :: Expr) with:
    label(self): "s-mutable-field" end,
    tosource(self): PP.nest(INDENT, str-mutable + self.name.tosource() + str-coloncolon + self.ann.tosource() + str-colonspace + self.value.tosource()) end,
  | s-once-field(l :: S.Location, name :: Expr, ann :: Ann, value :: Expr) with:
    label(self): "s-once-field" end
  | s-method-field(
      l :: S.Location,
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
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end
end

data ForBind:
  | s-for-bind(l :: S.Location, bind :: Bind, value :: Expr) with:
    label(self): "s-for-bind" end,
    tosource(self):
      PP.group(self.bind.tosource() + break-one + str-from + break-one + self.value.tosource())
    end
sharing:
  visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
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
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end
end

data VariantMember:
  | s-variant-member(l :: S.Location, member-type :: VariantMemberType, bind :: Bind) with:
    label(self): "s-variant-member" end,
    tosource(self):
      self.member-type.tosource() + self.bind.tosource()
    end
sharing:
  visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end
end

data Variant:
  | s-variant(
      l :: S.Location,
      constr-loc :: S.Location,
      name :: String,
      members :: List<VariantMember>,
      with-members :: List<Member>
    ) with:
    label(self): "s-variant" end,
    tosource(self):
      header-nowith =
        PP.str(self.name)
        + PP.surround-separate(INDENT, 0, PP.mt-doc, PP.lparen, PP.commabreak, PP.rparen,
        self.members.map(lam(b): b.tosource() end))
      header = PP.group(header-nowith + break-one + str-with)
      withs = self.with-members.map(lam(m): m.tosource() end)
      if lists.is-empty(withs): header-nowith
      else: header + PP.group(PP.nest(INDENT, break-one + PP.separate(PP.commabreak, withs)))
      end
    end
  | s-singleton-variant(
      l :: S.Location,
      name :: String,
      with-members :: List<Member>
    ) with:
    label(self): "s-singleton-variant" end,
    tosource(self):
      header-nowith = PP.str(self.name)
      header = PP.group(header-nowith + break-one + str-with)
      withs = self.with-members.map(lam(m): m.tosource() end)
      if lists.is-empty(withs): header-nowith
      else: header + PP.group(PP.nest(INDENT, break-one + PP.separate(PP.commabreak, withs)))
      end
    end
sharing:
  visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end
end

data DatatypeVariant:
  | s-datatype-variant(
      l :: S.Location,
      name :: String,
      members :: List<VariantMember>,
      constructor :: Constructor
    ) with:
    label(self): "s-datatype-variant" end,
    tosource(self):
      PP.str("FIXME 10/24/2013: dbp doesn't understand this pp stuff")
    end
  | s-datatype-singleton-variant(
      l :: S.Location,
      name :: String,
      constructor :: Constructor
    ) with:
    label(self): "s-datatype-singleton-variant" end,
    tosource(self):
      PP.str("FIXME 10/24/2013: dbp doesn't understand this pp stuff")
    end
sharing:
  visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end
end

data Constructor:
  | s-datatype-constructor(
      l :: S.Location,
      self :: String,
      body :: Expr
      ) with:
    label(self): "s-datatype-constructor" end,
    tosource(self):
      PP.str("FIXME 10/24/2013: dbp doesn't understand this pp stuff")
    end
sharing:
  visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end
end

data IfBranch:
  | s-if-branch(l :: S.Location, test :: Expr, body :: Expr) with:
    label(self): "s-if-branch" end,
    tosource(self):
      str-if
        + PP.nest(2 * INDENT, self.test.tosource() + str-colon)
        + PP.nest(INDENT, break-one + self.body.tosource())
    end
sharing:
  visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end
end

data IfPipeBranch:
  | s-if-pipe-branch(l :: S.Location, test :: Expr, body :: Expr) with:
    label(self): "s-if-pipe-branch" end,
    tosource(self):
      str-pipespace
        + PP.nest(2 * INDENT, self.test.tosource() + break-one + str-thencolon)
        + PP.nest(INDENT, break-one + self.body.tosource())
    end
sharing:
  visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end
end

data CasesBranch:
  | s-cases-branch(l :: S.Location, name :: String, args :: List<Bind>, body :: Expr) with:
    label(self): "s-cases-branch" end,
    tosource(self):
      PP.nest(INDENT,
        PP.group(PP.str("| " + self.name)
            + PP.surround-separate(INDENT, 0, PP.mt-doc, PP.lparen, PP.commabreak, PP.rparen,
            self.args.map(lam(a): a.tosource() end)) + break-one + str-thickarrow) + break-one +
        self.body.tosource())
    end
sharing:
  visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end
end

data Ann:
  | a-blank with:
    label(self): "a-blank" end,
    tosource(self): str-any end
  | a-any with:
    label(self): "a-any" end,
    tosource(self): str-any end
  | a-name(l :: S.Location, id :: Name) with:
    label(self): "a-name" end,
    tosource(self): self.id.tosource() end
  | a-arrow(l :: S.Location, args :: List<Ann>, ret :: Ann, use-parens :: Boolean) with:
    label(self): "a-arrow" end,
    tosource(self):
      ann = PP.separate(str-space,
        [list: PP.separate(PP.commabreak, self.args.map(_.tosource()))] + [list: str-arrow, self.ret.tosource()])
      if (self.use-parens): PP.surround(INDENT, 0, PP.lparen, ann, PP.rparen)
      else: ann
      end
    end
  | a-method(l :: S.Location, args :: List<Ann>, ret :: Ann) with:
    label(self): "a-method" end,
    tosource(self): PP.str("NYI: A-method") end
  | a-record(l :: S.Location, fields :: List<AField>) with:
    label(self): "a-record" end,
    tosource(self):
      PP.surround-separate(INDENT, 1, PP.lbrace + PP.rbrace, PP.lbrace, PP.commabreak, PP.rbrace,
        self.fields.map(_.tosource()))
    end
  | a-app(l :: S.Location, ann :: Ann, args :: List<Ann>) with:
    label(self): "a-app" end,
    tosource(self):
      PP.group(self.ann.tosource()
          + PP.group(PP.langle + PP.nest(INDENT,
            PP.separate(PP.commabreak, self.args.map(_.tosource()))) + PP.rangle))
    end
  | a-pred(l :: S.Location, ann :: Ann, exp :: Expr) with:
    label(self): "a-pred" end,
    tosource(self): self.ann.tosource() + PP.parens(self.exp.tosource()) end
  | a-dot(l :: S.Location, obj :: Name, field :: String) with:
    label(self): "a-dot" end,
    tosource(self): self.obj.tosource() + PP.str("." + self.field) end
sharing:
  visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end
end

data AField:
  | a-field(l :: S.Location, name :: String, ann :: Ann) with:
    label(self): "a-field" end,
    tosource(self):
      if is-a-blank(self.ann): PP.str(self.name)
      else: PP.infix(INDENT, 1, str-coloncolon, PP.str(self.name), self.ann.tosource())
      end
    end
sharing:
  visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end
end

fun make-checker-name(name): "is-" + name;

fun flatten(list-of-lists :: List):
  for fold(biglist from [list: ], piece from list-of-lists):
    biglist + piece
  end
end

fun binding-type-ids(stmt) -> List<Name>:
  cases(Expr) stmt:
    | s-newtype(l, name, _) => [list: {type: "normal", name: name}]
    | s-type(l, name, _) => [list: {type: "normal", name: name}]
    | s-data(l, name, _, _, _, _, _) => [list: {type: "data", name: s-name(l, name)}]
    | else => empty
  end
end

fun block-type-ids(b :: Expr%(is-s-block)) -> List<Name>:
  cases(Expr) b:
    | s-block(_, stmts) => flatten(stmts.map(binding-type-ids))
    | else => raise("Non-block given to block-ids")
  end
end

fun binding-ids(stmt) -> List<Name>:
  fun variant-ids(variant):
    cases(Variant) variant:
      | s-variant(_, l2, name, _, _) => [list: s-name(l2, name), s-name(l2, make-checker-name(name))]
      | s-singleton-variant(l, name, _) => [list: s-name(l, name), s-name(l, make-checker-name(name))]
    end
  end
  cases(Expr) stmt:
    | s-let(_, b, _, _) => [list: b.id]
    | s-var(_, b, _) => [list: b.id]
    | s-fun(l, name, _, _, _, _, _, _) => [list: s-name(l, name)]
    | s-graph(_, bindings) => flatten(bindings.map(binding-ids))
    | s-data(l, name, _, _, variants, _, _) =>
      s-name(l, name) ^ link(_, flatten(variants.map(variant-ids)))
    | else => [list: ]
  end
end

fun block-ids(b :: Expr%(is-s-block)) -> List<Name>:
  cases(Expr) b:
    | s-block(_, stmts) => flatten(stmts.map(binding-ids))
    | else => raise("Non-block given to block-ids")
  end
end

fun toplevel-ids(program :: Program) -> List<Name>:
  cases(Program) program:
    | s-program(_, _, _, _, b) => block-ids(b)
    | else => raise("Non-program given to toplevel-ids")
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

  s-module(self, l, answer, provides, types, checks):
    s-module(l, answer.visit(self), provides.visit(self), lists.map(_.visit(self), types), checks.visit(self))
  end,
  
  s-program(self, l, _provide, provided-types, imports, body):
    s-program(l, _provide.visit(self), provided-types.visit(self), imports.map(_.visit(self)), body.visit(self))
  end,

  s-import(self, l, import-type, name):
    s-import(l, import-type.visit(self), name.visit(self))
  end,
  s-file-import(self, l, file):
    s-file-import(l, file)
  end,
  s-const-import(self, l, mod):
    s-const-import(l, mod)
  end,
  s-import-types(self, l, import-type, name, types):
    s-import-types(l, import-type, name.visit(self), types.visit(self))
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
  s-provide-types(self, l, anns):
    s-provide-types(l, anns.map(_.visit(self)))
  end,
  s-provide-types-all(self, l):
    s-provide-types-all(l)
  end,
  s-provide-types-none(self, l):
    s-provide-types-none(l)
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

  s-type-bind(self, l, name, ann):
    s-type-bind(l, name.visit(self), ann.visit(self))
  end,

  s-newtype-bind(self, l, name, namet):
    s-newtype-bind(l, name.visit(self), namet.visit(self))
  end,

  s-type-let-expr(self, l, binds, body):
    s-type-let-expr(l, binds.map(_.visit(self)), body.visit(self))
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

  s-hint-exp(self, l :: S.Location, hints :: List<Hint>, exp :: Expr):
    s-hint-exp(l, hints, exp.visit(self))
  end,

  s-instantiate(self, l :: S.Location, expr :: Expr, params :: List<Ann>):
    s-instantiate(l, expr.visit(self), params.map(_.visit(self)))
  end,

  s-block(self, l, stmts):
    s-block(l, stmts.map(_.visit(self)))
  end,

  s-user-block(self, l :: S.Location, body :: Expr):
    s-user-block(l, body.visit(self))
  end,

  s-fun(self, l, name, params, args, ann, doc, body, _check):
    s-fun(l, name, params, args.map(_.visit(self)), ann.visit(self), doc, body.visit(self), self.option(_check))
  end,

  s-type(self, l :: S.Location, name :: Name, ann :: Ann):
    s-type(l, name.visit(self), ann.visit(self))
  end,

  s-newtype(self, l :: S.Location, name :: Name, namet :: Name):
    s-newtype(l, name.visit(self), namet.visit(self))
  end,

  s-var(self, l :: S.Location, name :: Bind, value :: Expr):
    s-var(l, name.visit(self), value.visit(self))
  end,

  s-let(self, l :: S.Location, name :: Bind, value :: Expr, keyword-val :: Boolean):
    s-let(l, name.visit(self), value.visit(self), keyword-val) 
  end,

  s-graph(self, l :: S.Location, bindings :: List<Expr%(is-s-let)>):
    s-graph(l, bindings.map(_.visit(self)))
  end,

  s-when(self, l :: S.Location, test :: Expr, block :: Expr):
    s-when(l, test.visit(self), block.visit(self))
  end,

  s-contract(self, l, name, ann):
    s-contract(l, name.visit(self), ann.visit(self))
  end,

  s-assign(self, l :: S.Location, id :: Name, value :: Expr):
    s-assign(l, id.visit(self), value.visit(self))
  end,

  s-if-branch(self, l :: S.Location, test :: Expr, body :: Expr):
    s-if-branch(l, test.visit(self), body.visit(self))
  end,

  s-if-pipe-branch(self, l :: S.Location, test :: Expr, body :: Expr):
    s-if-pipe-branch(l, test.visit(self), body.visit(self))
  end,

  s-if(self, l :: S.Location, branches :: List<IfBranch>):
    s-if(l, branches.map(_.visit(self)))
  end,
  s-if-else(self, l :: S.Location, branches :: List<IfBranch>, _else :: Expr):
    s-if-else(l, branches.map(_.visit(self)), _else.visit(self))
  end,
  
  s-if-pipe(self, l :: S.Location, branches :: List<IfPipeBranch>):
    s-if-pipe(l, branches.map(_.visit(self)))
  end,
  s-if-pipe-else(self, l :: S.Location, branches :: List<IfPipeBranch>, _else :: Expr):
    s-if-pipe-else(l, branches.map(_.visit(self)), _else.visit(self))
  end,

  s-cases-branch(self, l :: S.Location, name :: String, args :: List<Bind>, body :: Expr):
    s-cases-branch(l, name, args.map(_.visit(self)), body.visit(self))
  end,

  s-cases(self, l :: S.Location, typ :: Ann, val :: Expr, branches :: List<CasesBranch>):
    s-cases(l, typ.visit(self), val.visit(self), branches.map(_.visit(self)))
  end,
  s-cases-else(self, l :: S.Location, typ :: Ann, val :: Expr, branches :: List<CasesBranch>, _else :: Expr):
    s-cases-else(l, typ.visit(self), val.visit(self), branches.map(_.visit(self)), _else.visit(self))
  end,

  s-try(self, l :: S.Location, body :: Expr, id :: Bind, _except :: Expr):
    s-try(l, body.visit(self), id.visit(self), _except.visit(self))
  end,

  s-op(self, l :: S.Location, op :: String, left :: Expr, right :: Expr):
    s-op(l, op, left.visit(self), right.visit(self))
  end,

  s-check-test(self, l :: S.Location, op :: String, left :: Expr, right :: Expr):
    s-check-test(l, op, left.visit(self), right.visit(self))
  end,

  s-paren(self, l :: S.Location, expr :: Expr):
    s-paren(l, expr.visit(self))
  end,

  s-lam(
      self,
      l :: S.Location,
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
      l :: S.Location,
      args :: List<Bind>, # Value parameters
      ann :: Ann, # return type
      doc :: String,
      body :: Expr,
      _check :: Option<Expr>
    ):
    s-method(l, args.map(_.visit(self)), ann.visit(self), doc, body.visit(self), self.option(_check))
  end,
  s-extend(self, l :: S.Location, supe :: Expr, fields :: List<Member>):
    s-extend(l, supe.visit(self), fields.map(_.visit(self)))
  end,
  s-update(self, l :: S.Location, supe :: Expr, fields :: List<Member>):
    s-update(l, supe.visit(self), fields.map(_.visit(self)))
  end,
  s-obj(self, l :: S.Location, fields :: List<Member>):
    s-obj(l, fields.map(_.visit(self)))
  end,
  s-array(self, l :: S.Location, values :: Array<Expr>):
    s-array(l, values.map(_.visit(self)))
  end,
  s-bless(self, l :: S.Location, expr :: Expr, typ :: Name):
    s-bless(l, expr.visit(self), typ.visit(self))
  end,
  s-confirm(self, l :: S.Location, expr :: Expr, typ :: Name):
    s-confirm(l, expr.visit(self), typ.visit(self))
  end,
  s-construct(self, l :: S.Location, mod :: ConstructModifier, constructor :: Expr, values :: List<Expr>):
    s-construct(l, mod, constructor.visit(self), values.map(_.visit(self)))
  end,
  s-app(self, l :: S.Location, _fun :: Expr, args :: List<Expr>):
    s-app(l, _fun.visit(self), args.map(_.visit(self)))
  end,
  s-prim-app(self, l :: S.Location, _fun :: String, args :: List<Expr>):
    s-prim-app(l, _fun, args.map(_.visit(self)))
  end,
  s-prim-val(self, l :: S.Location, name :: String):
    s-prim-val(l, name)
  end,
  s-id(self, l :: S.Location, id :: Name):
    s-id(l, id.visit(self))
  end,
  s-id-var(self, l :: S.Location, id :: Name):
    s-id-var(l, id.visit(self))
  end,
  s-id-letrec(self, l :: S.Location, id :: Name, safe :: Boolean):
    s-id-letrec(l, id.visit(self), safe)
  end,
  s-undefined(self, l :: S.Location):
    s-undefined(self)
  end,
  s-srcloc(self, l, shadow loc):
    s-srcloc(l, loc)
  end,
  s-num(self, l :: S.Location, n :: Number):
    s-num(l, n)
  end,
  s-frac(self, l :: S.Location, num :: Number, den :: Number):
    s-frac(l, num, den)
  end,
  s-bool(self, l :: S.Location, b :: Boolean):
    s-bool(l, b)
  end,
  s-str(self, l :: S.Location, s :: String):
    s-str(l, s)
  end,
  s-dot(self, l :: S.Location, obj :: Expr, field :: String):
    s-dot(l, obj.visit(self), field)
  end,
  s-get-bang(self, l :: S.Location, obj :: Expr, field :: String):
    s-get-bang(l, obj.visit(self), field)
  end,
  s-bracket(self, l :: S.Location, obj :: Expr, field :: Expr):
    s-bracket(l, obj.visit(self), field.visit(self))
  end,
  s-data(
      self,
      l :: S.Location,
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
      l :: S.Location,
      name :: String,
      namet :: Name,
      params :: List<String>, # type params
      mixins :: List<Expr>,
      variants :: List<Variant>,
      shared-members :: List<Member>,
      _check :: Option<Expr>
    ):
    s-data-expr(
        l,
        name,
        namet.visit(self),
        params,
        mixins.map(_.visit(self)),
        variants.map(_.visit(self)),
        shared-members.map(_.visit(self)),
        self.option(_check)
      )
  end,
  s-for(
      self,
      l :: S.Location,
      iterator :: Expr,
      bindings :: List<ForBind>,
      ann :: Ann,
      body :: Expr
    ):
    s-for(l, iterator.visit(self), bindings.map(_.visit(self)), ann.visit(self), body.visit(self))
  end,
  s-check(self, l :: S.Location, name :: Option<String>, body :: Expr, keyword-check :: Boolean):
    s-check(l, name, body.visit(self), keyword-check)
  end,

  s-data-field(self, l :: S.Location, name :: Expr, value :: Expr):
    s-data-field(l, name.visit(self), value.visit(self))
  end,
  s-mutable-field(self, l :: S.Location, name :: Expr, ann :: Ann, value :: Expr):
    s-mutable-field(l, name.visit(self), ann.visit(self), value.visit(self))
  end,
  s-once-field(self, l :: S.Location, name :: Expr, ann :: Ann, value :: Expr):
    s-once-field(l, name.visit(self), ann.visit(self), value.visit(self))
  end,
  s-method-field(
      self,
      l :: S.Location,
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

  s-for-bind(self, l :: S.Location, bind :: Bind, value :: Expr):
    s-for-bind(l, bind.visit(self), value.visit(self))   
  end,
  s-variant-member(self, l :: S.Location, member-type :: VariantMemberType, bind :: Bind):
    s-variant-member(l, member-type, bind.visit(self))
  end,
  s-variant(
      self,
      l :: S.Location,
      constr-loc :: S.Location,
      name :: String,
      members :: List<VariantMember>,
      with-members :: List<Member>
    ):
    s-variant(l, constr-loc, name, members.map(_.visit(self)), with-members.map(_.visit(self)))
  end,
  s-singleton-variant(
      self,
      l :: S.Location,
      name :: String,
      with-members :: List<Member>
    ):
    s-singleton-variant(l, name, with-members.map(_.visit(self)))
  end,
  s-datatype-variant(
      self,
      l :: S.Location,
      name :: String,
      members :: List<VariantMember>,
      constructor :: Constructor
    ):
    s-datatype-variant(l, name, members.map(_.visit(self)), constructor.visit(self))
  end,
  s-datatype-singleton-variant(
      self,
      l :: S.Location,
      name :: String,
      constructor :: Constructor
    ):
    s-datatype-singleton-variant(l, name, constructor.visit(self))
  end,
  s-datatype-constructor(
      self,
      l :: S.Location,
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
  
  s-module(self, l, answer, provides, types, checks):
    answer.visit(self) and provides.visit(self) and lists.all(_.visit(self), types) and checks.visit(self)
  end,
  
  s-program(self, l, _provide, provided-types, imports, body):
    _provide.visit(self)
    and lists.all(_.visit(self), provided-types)
    and lists.all(_.visit(self), imports)
    and body.visit(self)
  end,
  
  s-import(self, l, import-type, name):
    import-type.visit(self) and name.visit(self)
  end,
  s-file-import(self, l, file):
    true
  end,
  s-const-import(self, l, mod):
    true
  end,
  s-import-types(self, l, import-type, name, types):
    name.visit(self) and types.visit(self)
  end,
  s-import-fields(self, l, fields, import-type):
    lists.all(_.visit(self), fields)
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
  s-provide-types(self, l, anns):
    lists.all(_.visit(self), anns)
  end,
  s-provide-types-all(self, l):
    true
  end,
  s-provide-types-none(self, l):
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
  
  s-type-bind(self, l, name, ann):
    name.visit(self) and ann.visit(self)
  end,

  s-newtype-bind(self, l, name, namet):
    name.visit(self) and namet.visit(self)
  end,

  s-type-let-expr(self, l, binds, body):
    lists.all(_.visit(self), binds) and body.visit(self)
  end,

  s-let-expr(self, l, binds, body):
    lists.all(_.visit(self), binds) and body.visit(self)
  end,
  
  s-letrec-bind(self, l, bind, expr):
    bind.visit(self) and expr.visit(self)
  end,
  
  s-letrec(self, l, binds, body):
    lists.all(_.visit(self), binds) and body.visit(self)
  end,
  
  s-hint-exp(self, l :: S.Location, hints :: List<Hint>, exp :: Expr):
    exp.visit(self)
  end,
  
  s-instantiate(self, l :: S.Location, expr :: Expr, params :: List<Ann>):
    expr.visit(self) and lists.all(_.visit(self), params)
  end,
  
  s-block(self, l, stmts):
    lists.all(_.visit(self), stmts)
  end,
  
  s-user-block(self, l :: S.Location, body :: Expr):
    body.visit(self)
  end,
  
  s-fun(self, l, name, params, args, ann, doc, body, _check):
    lists.all(_.visit(self), args) and ann.visit(self) and body.visit(self) and self.option(_check)
  end,

  s-type(self, l :: S.Location, name :: Name, ann :: Ann):
    name.visit(self) and ann.visit(self)
  end,
  
  s-newtype(self, l :: S.Location, name :: Name, namet :: Name):
    name.visit(self) and namet.visit(self)
  end,

  s-var(self, l :: S.Location, name :: Bind, value :: Expr):
    name.visit(self) and value.visit(self)
  end,
  
  s-let(self, l :: S.Location, name :: Bind, value :: Expr, keyword-val :: Boolean):
    name.visit(self) and value.visit(self)
  end,
  
  s-graph(self, l :: S.Location, bindings :: List<Expr%(is-s-let)>):
    lists.all(_.visit(self), bindings)
  end,
  
  s-when(self, l :: S.Location, test :: Expr, block :: Expr):
    test.visit(self) and block.visit(self)
  end,

  s-contract(self, l :: S.Location, name :: Name, ann :: Ann):
    name.visit(self) and ann.visit(self)
  end,
  
  s-assign(self, l :: S.Location, id :: Name, value :: Expr):
    id.visit(self) and value.visit(self)
  end,
  
  s-if-branch(self, l :: S.Location, test :: Expr, body :: Expr):
    test.visit(self) and body.visit(self)
  end,
  
  s-if-pipe-branch(self, l :: S.Location, test :: Expr, body :: Expr):
    test.visit(self) and body.visit(self)
  end,
  
  s-if(self, l :: S.Location, branches :: List<IfBranch>):
    lists.all(_.visit(self), branches)
  end,
  s-if-else(self, l :: S.Location, branches :: List<IfBranch>, _else :: Expr):
    lists.all(_.visit(self), branches) and _else.visit(self)
  end,
  
  s-if-pipe(self, l :: S.Location, branches :: List<IfPipeBranch>):
    lists.all(_.visit(self), branches)
  end,
  s-if-pipe-else(self, l :: S.Location, branches :: List<IfPipeBranch>, _else :: Expr):
    lists.all(_.visit(self), branches) and _else.visit(self)
  end,
  
  s-cases-branch(self, l :: S.Location, name :: String, args :: List<Bind>, body :: Expr):
    lists.all(_.visit(self), args) and body.visit(self)
  end,
  
  s-cases(self, l :: S.Location, typ :: Ann, val :: Expr, branches :: List<CasesBranch>):
    typ.visit(self) and val.visit(self) and lists.all(_.visit(self), branches)
  end,
  s-cases-else(self, l :: S.Location, typ :: Ann, val :: Expr, branches :: List<CasesBranch>, _else :: Expr):
    typ.visit(self) and val.visit(self) and lists.all(_.visit(self), branches) and _else.visit(self)
  end,
  
  s-try(self, l :: S.Location, body :: Expr, id :: Bind, _except :: Expr):
    body.visit(self) and id.visit(self) and _except.visit(self)
  end,
  
  s-op(self, l :: S.Location, op :: String, left :: Expr, right :: Expr):
    left.visit(self) and right.visit(self)
  end,
  
  s-check-test(self, l :: S.Location, op :: String, left :: Expr, right :: Expr):
    left.visit(self) and right.visit(self)
  end,
  
  s-paren(self, l :: S.Location, expr :: Expr):
    expr.visit(self)
  end,
  
  s-lam(
      self,
      l :: S.Location,
      params :: List<String>,
      args :: List<Bind>,
      ann :: Ann,
      doc :: String,
      body :: Expr,
      _check :: Option<Expr>
      ):
    lists.all(_.visit(self), args) and ann.visit(self) and body.visit(self) and self.option(_check)
  end,
  s-method(
      self,
      l :: S.Location,
      args :: List<Bind>, # Value parameters
      ann :: Ann, # return type
      doc :: String,
      body :: Expr,
      _check :: Option<Expr>
      ):
    lists.all(_.visit(self), args) and ann.visit(self) and body.visit(self) and self.option(_check)
  end,
  s-extend(self, l :: S.Location, supe :: Expr, fields :: List<Member>):
    supe.visit(self) and lists.all(_.visit(self), fields)
  end,
  s-update(self, l :: S.Location, supe :: Expr, fields :: List<Member>):
    supe.visit(self) and lists.all(_.visit(self), fields)
  end,
  s-obj(self, l :: S.Location, fields :: List<Member>):
    lists.all(_.visit(self), fields)
  end,
  s-array(self, l :: S.Location, values :: List<Expr>):
    lists.all(_.visit(self), values)
  end,
  s-bless(self, l :: S.Location, expr :: Expr, typ :: Name):
    expr.visit(self) and typ.visit(self)
  end,
  s-confirm(self, l :: S.Location, expr :: Expr, typ :: Name):
    expr.visit(self) and typ.visit(self)
  end,
  s-construct(self, l :: S.Location, mod :: ConstructModifier, constructor :: Expr, values :: List<Expr>):
    constructor.visit(self) and lists.all(_.visit(self), values)
  end,
  s-app(self, l :: S.Location, _fun :: Expr, args :: List<Expr>):
    _fun.visit(self) and lists.all(_.visit(self), args)
  end,
  s-prim-app(self, l :: S.Location, _fun :: String, args :: List<Expr>):
    lists.all(_.visit(self), args)
  end,
  s-prim-val(self, l :: S.Location, name :: String):
    true
  end,
  s-id(self, l :: S.Location, id :: Name):
    id.visit(self)
  end,
  s-id-var(self, l :: S.Location, id :: Name):
    id.visit(self)
  end,
  s-id-letrec(self, l :: S.Location, id :: Name, safe :: Boolean):
    id.visit(self)
  end,
  s-undefined(self, l :: S.Location):
    true
  end,
  s-srcloc(self, l, shadow loc):
    true
  end,
  s-num(self, l :: S.Location, n :: Number):
    true
  end,
  s-frac(self, l :: S.Location, num :: Number, den :: Number):
    true
  end,
  s-bool(self, l :: S.Location, b :: Boolean):
    true
  end,
  s-str(self, l :: S.Location, s :: String):
    true
  end,
  s-dot(self, l :: S.Location, obj :: Expr, field :: String):
    obj.visit(self)
  end,
  s-get-bang(self, l :: S.Location, obj :: Expr, field :: String):
    obj.visit(self)
  end,
  s-bracket(self, l :: S.Location, obj :: Expr, field :: Expr):
    obj.visit(self) and field.visit(self)
  end,
  s-data(
      self,
      l :: S.Location,
      name :: String,
      params :: List<String>, # type params
      mixins :: List<Expr>,
      variants :: List<Variant>,
      shared-members :: List<Member>,
      _check :: Option<Expr>
      ):
    lists.all(_.visit(self), mixins) 
    and lists.all(_.visit(self), variants)
    and lists.all(_.visit(self), shared-members)
    and self.option(_check)
  end,
  s-data-expr(
      self,
      l :: S.Location,
      name :: String,
      namet :: Name,
      params :: List<String>, # type params
      mixins :: List<Expr>,
      variants :: List<Variant>,
      shared-members :: List<Member>,
      _check :: Option<Expr>
      ):
    namet.visit(self)
    and lists.all(_.visit(self), mixins)
    and lists.all(_.visit(self), variants)
    and lists.all(_.visit(self), shared-members)
    and self.option(_check)
  end,
  s-for(
      self,
      l :: S.Location,
      iterator :: Expr,
      bindings :: List<ForBind>,
      ann :: Ann,
      body :: Expr
      ):
    iterator.visit(self) and lists.all(_.visit(self), bindings) and ann.visit(self) and body.visit(self)
  end,
  s-check(self, l :: S.Location, name :: String, body :: Expr, keyword-check :: Boolean):
    body.visit(self)
  end,
  
  s-data-field(self, l :: S.Location, name :: Expr, value :: Expr):
    name.visit(self) and value.visit(self)
  end,
  s-mutable-field(self, l :: S.Location, name :: Expr, ann :: Ann, value :: Expr):
    name.visit(self) and ann.visit(self) and value.visit(self)
  end,
  s-once-field(self, l :: S.Location, name :: Expr, ann :: Ann, value :: Expr):
    name.visit(self) and ann.visit(self) and value.visit(self)
  end,
  s-method-field(
      self,
      l :: S.Location,
      name :: Expr,
      args :: List<Bind>, # Value parameters
      ann :: Ann, # return type
      doc :: String,
      body :: Expr,
      _check :: Option<Expr>
      ):
    name.visit(self)
    and lists.all(_.visit(self), args)
    and ann.visit(self)
    and body.visit(self)
    and self.option(_check)
  end,
  
  s-for-bind(self, l :: S.Location, bind :: Bind, value :: Expr):
    bind.visit(self) and value.visit(self)
  end,
  s-variant-member(self, l :: S.Location, member-type :: VariantMemberType, bind :: Bind):
    bind.visit(self)
  end,
  s-variant(
      self,
      l :: S.Location,
      constr-loc :: S.Location,
      name :: String,
      members :: List<VariantMember>,
      with-members :: List<Member>
      ):
    lists.all(_.visit(self), members) and lists.all(_.visit(self), with-members)
  end,
  s-singleton-variant(
      self,
      l :: S.Location,
      name :: String,
      with-members :: List<Member>
      ):
    lists.all(_.visit(self), with-members)
  end,
  s-datatype-variant(
      self,
      l :: S.Location,
      name :: String,
      members :: List<VariantMember>,
      constructor :: Constructor
      ):
    lists.all(_.visit(self), members) and constructor.visit(self)
  end,
  s-datatype-singleton-variant(
      self,
      l :: S.Location,
      name :: String,
      constructor :: Constructor
      ):
    constructor.visit(self)
  end,
  s-datatype-constructor(
      self,
      l :: S.Location,
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
    lists.all(_.visit(self), args) and ret.visit(self)
  end,
  a-method(self, l, args, ret):
    lists.all(_.visit(self), args) and ret.visit(self)
  end,
  a-record(self, l, fields):
    lists.all(_.visit(self), fields)
  end,
  a-app(self, l, ann, args):
    ann.visit(self) and lists.all(_.visit(self), args)
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
  
  s-module(self, l, answer, provides, types, checks):
    s-module(dummy-loc,
      answer.visit(self), provides.visit(self), lists.map(_.visit(self), types), checks.visit(self))
  end,
  
  s-program(self, l, _provide, provided-types, imports, body):
    s-program(dummy-loc, _provide.visit(self), provided-types.visit(self), imports.map(_.visit(self)), body.visit(self))
  end,

  s-file-import(self, l :: S.Location, file :: String):
    s-file-import(dummy-loc, file)
  end,
  s-const-import(self, l :: S.Location, mod :: String):
    s-const-import(dummy-loc, mod)
  end,
  s-import(self, l, import-type, name):
    s-import(dummy-loc, import-type.visit(self), name.visit(self))
  end,
  s-import-types(self, l, import-type, name, types):
    s-import-types(dummy-loc, import-type.visit(self), name.visit(self), types.visit(self))
  end,
  s-import-fields(self, l, fields, import-type):
    s-import-fields(dummy-loc, fields.map(_.visit(self)), import-type.visit(self))
  end,
  s-provide(self, l, expr):
    s-provide(dummy-loc, expr.visit(self))
  end,
  s-provide-all(self, l):
    s-provide-all(dummy-loc)
  end,
  s-provide-none(self, l):
    s-provide-none(dummy-loc)
  end,
  s-provide-types(self, l, anns):
    s-provide(dummy-loc, anns.map(_.visit(self)))
  end,
  s-provide-types-all(self, l):
    s-provide-types-all(dummy-loc)
  end,
  s-provide-types-none(self, l):
    s-provide-types-none(dummy-loc)
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

  s-type-bind(self, l, name, ann):
    s-type-bind(dummy-loc, name, ann)
  end,

  s-newtype-bind(self, l, name, namet):
    s-newtype-bind(l, name.visit(self), namet.visit(self))
  end,

  s-type-let-expr(self, l, binds, body):
    s-type-let-expr(dummy-loc, binds.map(_.visit(self)), body.visit(self))
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

  s-hint-exp(self, l :: S.Location, hints :: List<Hint>, exp :: Expr):
    s-hint-exp(dummy-loc, hints, exp.visit(self))
  end,

  s-instantiate(self, l :: S.Location, expr :: Expr, params :: List<Ann>):
    s-instantiate(dummy-loc, expr.visit(self), params.map(_.visit(self)))
  end,

  s-block(self, l, stmts):
    s-block(dummy-loc, stmts.map(_.visit(self)))
  end,

  s-user-block(self, l :: S.Location, body :: Expr):
    s-user-block(dummy-loc, body.visit(self))
  end,

  s-fun(self, l, name, params, args, ann, doc, body, _check):
    s-fun(dummy-loc, name, params, args.map(_.visit(self)), ann.visit(self), doc, body.visit(self), self.option(_check))
  end,

  s-type(self, l :: S.Location, name :: Name, ann :: Ann):
    s-type(dummy-loc, name.visit(self), ann.visit(self))
  end,

  s-newtype(self, l :: S.Location, name :: Name, namet :: Name):
    s-newtype(dummy-loc, name.visit(self), namet.visit(self))
  end,

  s-var(self, l :: S.Location, name :: Bind, value :: Expr):
    s-var(dummy-loc, name.visit(self), value.visit(self))
  end,

  s-let(self, l :: S.Location, name :: Bind, value :: Expr, keyword-val :: Boolean):
    s-let(dummy-loc, name.visit(self), value.visit(self), keyword-val) 
  end,

  s-graph(self, l :: S.Location, bindings :: List<Expr%(is-s-let)>):
    s-graph(dummy-loc, bindings.map(_.visit(self)))
  end,

  s-when(self, l :: S.Location, test :: Expr, block :: Expr):
    s-when(dummy-loc, test.visit(self), block.visit(self))
  end,

  s-contract(self, l, name, ann):
    s-contract(dummy-loc, name.visit(self), ann.visit(self))
  end,

  s-assign(self, l :: S.Location, id :: Name, value :: Expr):
    s-assign(dummy-loc, id.visit(self), value.visit(self))
  end,

  s-if-branch(self, l :: S.Location, test :: Expr, body :: Expr):
    s-if-branch(dummy-loc, test.visit(self), body.visit(self))
  end,

  s-if-pipe-branch(self, l :: S.Location, test :: Expr, body :: Expr):
    s-if-pipe-branch(dummy-loc, test.visit(self), body.visit(self))
  end,

  s-if(self, l :: S.Location, branches :: List<IfBranch>):
    s-if(dummy-loc, branches.map(_.visit(self)))
  end,
  s-if-else(self, l :: S.Location, branches :: List<IfBranch>, _else :: Expr):
    s-if-else(dummy-loc, branches.map(_.visit(self)), _else.visit(self))
  end,
  
  s-if-pipe(self, l :: S.Location, branches :: List<IfPipeBranch>):
    s-if-pipe(dummy-loc, branches.map(_.visit(self)))
  end,
  s-if-pipe-else(self, l :: S.Location, branches :: List<IfPipeBranch>, _else :: Expr):
    s-if-pipe-else(dummy-loc, branches.map(_.visit(self)), _else.visit(self))
  end,

  s-cases-branch(self, l :: S.Location, name :: String, args :: List<Bind>, body :: Expr):
    s-cases-branch(dummy-loc, name, args.map(_.visit(self)), body.visit(self))
  end,

  s-cases(self, l :: S.Location, typ :: Ann, val :: Expr, branches :: List<CasesBranch>):
    s-cases(dummy-loc, typ.visit(self), val.visit(self), branches.map(_.visit(self)))
  end,
  s-cases-else(self, l :: S.Location, typ :: Ann, val :: Expr, branches :: List<CasesBranch>, _else :: Expr):
    s-cases-else(dummy-loc, typ.visit(self), val.visit(self), branches.map(_.visit(self)), _else.visit(self))
  end,

  s-try(self, l :: S.Location, body :: Expr, id :: Bind, _except :: Expr):
    s-try(dummy-loc, body.visit(self), id.visit(self), _except.visit(self))
  end,

  s-op(self, l :: S.Location, op :: String, left :: Expr, right :: Expr):
    s-op(dummy-loc, op, left.visit(self), right.visit(self))
  end,

  s-check-test(self, l :: S.Location, op :: String, left :: Expr, right :: Expr):
    s-check-test(dummy-loc, op, left.visit(self), right.visit(self))
  end,

  s-paren(self, l :: S.Location, expr :: Expr):
    s-paren(dummy-loc, expr.visit(self))
  end,

  s-lam(
      self,
      l :: S.Location,
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
      l :: S.Location,
      args :: List<Bind>, # Value parameters
      ann :: Ann, # return type
      doc :: String,
      body :: Expr,
      _check :: Option<Expr>
    ):
    s-method(dummy-loc, args.map(_.visit(self)), ann.visit(self), doc, body.visit(self), self.option(_check))
  end,
  s-extend(self, l :: S.Location, supe :: Expr, fields :: List<Member>):
    s-extend(dummy-loc, supe.visit(self), fields.map(_.visit(self)))
  end,
  s-update(self, l :: S.Location, supe :: Expr, fields :: List<Member>):
    s-update(dummy-loc, supe.visit(self), fields.map(_.visit(self)))
  end,
  s-obj(self, l :: S.Location, fields :: List<Member>):
    s-obj(dummy-loc, fields.map(_.visit(self)))
  end,
  s-array(self, l :: S.Location, values :: List<Expr>):
    s-array(dummy-loc, values.map(_.visit(self)))
  end,
  s-bless(self, l :: S.Location, expr :: Expr, typ :: Name):
    s-bless(dummy-loc, expr.visit(self), typ.visit(self))
  end,
  s-confirm(self, l :: S.Location, expr :: Expr, typ :: Name):
    s-confirm(dummy-loc, expr.visit(self), typ.visit(self))
  end,
  s-construct(self, l :: S.Location, mod :: ConstructModifier, constructor :: Expr, values :: List<Expr>):
    s-construct(dummy-loc, mod, constructor.visit(self), values.map(_.visit(self)))
  end,
  s-app(self, l :: S.Location, _fun :: Expr, args :: List<Expr>):
    s-app(dummy-loc, _fun.visit(self), args.map(_.visit(self)))
  end,
  s-prim-app(self, l :: S.Location, _fun :: String, args :: List<Expr>):
    s-prim-app(dummy-loc, _fun, args.map(_.visit(self)))
  end,
  s-prim-val(self, l :: S.Location, name :: String):
    s-prim-val(dummy-loc, name)
  end,
  s-id(self, l :: S.Location, id :: Name):
    s-id(dummy-loc, id.visit(self))
  end,
  s-id-var(self, l :: S.Location, id :: Name):
    s-id-var(dummy-loc, id.visit(self))
  end,
  s-id-letrec(self, l :: S.Location, id :: Name, safe :: Boolean):
    s-id-letrec(dummy-loc, id.visit(self), safe)
  end,
  s-undefined(self, l :: S.Location):
    s-undefined(self)
  end,
  s-srcloc(self, l, shadow loc):
    s-srcloc(dummy-loc, loc)
  end,
  s-num(self, l :: S.Location, n :: Number):
    s-num(dummy-loc, n)
  end,
  s-frac(self, l :: S.Location, num :: Number, den :: Number):
    s-frac(dummy-loc, num, den)
  end,
  s-bool(self, l :: S.Location, b :: Boolean):
    s-bool(dummy-loc, b)
  end,
  s-str(self, l :: S.Location, s :: String):
    s-str(dummy-loc, s)
  end,
  s-dot(self, l :: S.Location, obj :: Expr, field :: String):
    s-dot(dummy-loc, obj.visit(self), field)
  end,
  s-get-bang(self, l :: S.Location, obj :: Expr, field :: String):
    s-get-bang(dummy-loc, obj.visit(self), field)
  end,
  s-bracket(self, l :: S.Location, obj :: Expr, field :: Expr):
    s-bracket(dummy-loc, obj.visit(self), field.visit(self))
  end,
  s-data(
      self,
      l :: S.Location,
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
      l :: S.Location,
      name :: String,
      namet :: String,
      params :: List<String>, # type params
      mixins :: List<Expr>,
      variants :: List<Variant>,
      shared-members :: List<Member>,
      _check :: Option<Expr>
    ):
    s-data-expr(
        dummy-loc,
        name,
        namet.visit(self),
        params,
        mixins.map(_.visit(self)),
        variants.map(_.visit(self)),
        shared-members.map(_.visit(self)),
        self.option(_check)
      )
  end,
  s-for(
      self,
      l :: S.Location,
      iterator :: Expr,
      bindings :: List<ForBind>,
      ann :: Ann,
      body :: Expr
    ):
    s-for(dummy-loc, iterator.visit(self), bindings.map(_.visit(self)), ann.visit(self), body.visit(self))
  end,
  s-check(self, l :: S.Location, name :: Option<String>, body :: Expr, keyword-check :: Boolean):
    s-check(dummy-loc, name, body.visit(self), keyword-check)
  end,

  s-data-field(self, l :: S.Location, name :: Expr, value :: Expr):
    s-data-field(dummy-loc, name.visit(self), value.visit(self))
  end,
  s-mutable-field(self, l :: S.Location, name :: Expr, ann :: Ann, value :: Expr):
    s-mutable-field(dummy-loc, name.visit(self), ann.visit(self), value.visit(self))
  end,
  s-once-field(self, l :: S.Location, name :: Expr, ann :: Ann, value :: Expr):
    s-once-field(dummy-loc, name.visit(self), ann.visit(self), value.visit(self))
  end,
  s-method-field(
      self,
      l :: S.Location,
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

  s-for-bind(self, l :: S.Location, bind :: Bind, value :: Expr):
    s-for-bind(dummy-loc, bind.visit(self), value.visit(self))   
  end,
  s-variant-member(self, l :: S.Location, member-type :: VariantMemberType, bind :: Bind):
    s-variant-member(dummy-loc, member-type, bind.visit(self))
  end,
  s-variant(
      self,
      l :: S.Location,
      constr-loc :: S.Location,
      name :: String,
      members :: List<VariantMember>,
      with-members :: List<Member>
    ):
    s-variant(dummy-loc, dummy-loc, name, members.map(_.visit(self)), with-members.map(_.visit(self)))
  end,
  s-singleton-variant(
      self,
      l :: S.Location,
      name :: String,
      with-members :: List<Member>
    ):
    s-singleton-variant(dummy-loc, name, with-members.map(_.visit(self)))
  end,
  s-datatype-variant(
      self,
      l :: S.Location,
      name :: String,
      members :: List<VariantMember>,
      constructor :: Constructor
    ):
    s-datatype-variant(dummy-loc, name, members.map(_.visit(self)), constructor.visit(self))
  end,
  s-datatype-singleton-variant(
      self,
      l :: S.Location,
      name :: String,
      constructor :: Constructor
    ):
    s-datatype-singleton-variant(dummy-loc, name, constructor.visit(self))
  end,
  s-datatype-constructor(
      self,
      l :: S.Location,
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
