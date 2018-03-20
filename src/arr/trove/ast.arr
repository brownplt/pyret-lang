#|
This source file is programmatically read to generate AST visitors. Make sure
to maintain the following:

1. All AST nodes are annotated with type annotation
2. All "built-in" annotations are either String, Boolean, Number, and Loc
3. All atomic annotations are datatype defined in this file which contains
   method visit
4. Complex annotations are either List<...> or Option<...> where ... is either
   a built-in annotation or atomic annotation
|#

provide *
provide-types *
import pprint as PP
import srcloc as S
import contracts as C
import valueskeleton as VS
import lists as lists
import option as option
import global as _
import base as _

type List = lists.List
link = lists.link
empty = lists.empty
list = lists.list
is-empty = lists.is-empty
is-link = lists.is-link
fold = lists.fold

type Option = option.Option
some = option.some
none = option.none
is-none = option.is-none

type Loc = S.Srcloc

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
str-deriving = PP.str("deriving ")
str-doc = PP.str("doc: ")
str-elsebranch = PP.str("| else =>")
str-elsecolon = PP.str("else:")
str-otherwisecolon = PP.str("otherwise:")
str-elsespace = PP.str("else ")
str-end = PP.str("end")
str-except = PP.str("except")
str-for = PP.str("for ")
str-do = PP.str("do ")
str-from = PP.str("from")
str-fun = PP.str("fun")
str-lam = PP.str("lam")
str-if = PP.str("if ")
str-of = PP.str("of ")
str-ask = PP.str("ask")
str-import = PP.str("import")
str-include = PP.str("include")
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
str-use-loc = PP.str("UseLoc")
str-var = PP.str("var ")
str-rec = PP.str("rec ")
str-newtype = PP.str("type ")
str-type = PP.str("type ")
str-val = PP.str("val ")
str-when = PP.str("when")
str-where = PP.str("where:")
str-with = PP.str("with:")
str-is = PP.str("is")
str-is-not = PP.str("is-not")
str-satisfies = PP.str("satisfies")
str-satisfies-not = PP.str("violates")
str-raises = PP.str("raises")
str-raises-other = PP.str("raises-other-than")
str-raises-not = PP.str("does-not-raise")
str-raises-satisfies = PP.str("raises-satisfies")
str-raises-violates = PP.str("raises-violates")
str-percent = PP.str("%")
str-tablecolon = PP.str("table:")
str-rowcolon = PP.str("row:")
str-extend = PP.str("extend")
str-transform = PP.str("transform")
str-using = PP.str("using")
str-select = PP.str("select")
str-sieve = PP.str("sieve")
str-order = PP.str("order")
str-extract = PP.str("extract")
str-load-table = PP.str("load-table:")
str-src = PP.str("source:")
str-sanitize = PP.str("sanitize")

data Name:
  | s-underscore(l :: Loc) with:
    method to-compiled-source(self): raise("Cannot compile underscores") end,
    method to-compiled(self): raise("Cannot compile underscores") end,
    method tosource(self): PP.str("_") end,
    method tosourcestring(self): "_" end,
    method toname(self): "_" end,
    method key(self): "underscore#" end

  | s-name(l :: Loc, s :: String) with:
    method to-compiled-source(self): PP.str(self.to-compiled()) end,
    method to-compiled(self): self.s end,
    method tosource(self): PP.str(self.s) end,
    method tosourcestring(self): self.s end,
    method toname(self): self.s end,
    method key(self): "name#" + self.s end

  | s-global(s :: String) with:
    method to-compiled-source(self): PP.str(self.to-compiled()) end,
    method to-compiled(self): self.s end,
    method tosource(self): PP.str(self.s) end,
    method tosourcestring(self): self.s end,
    method toname(self): self.s end,
    method key(self): "global#" + self.s end

  | s-type-global(s :: String) with:
    method to-compiled-source(self): PP.str(self.to-compiled()) end,
    method to-compiled(self): "$type$" + self.s end,
    method tosource(self): PP.str(self.s) end,
    method tosourcestring(self): "$type$" + self.s end,
    method toname(self): self.s end,
    method key(self): "tglobal#" + self.s end
    
  | s-atom(base :: String, serial :: Number) with:
    method to-compiled-source(self): PP.str(self.to-compiled()) end,
    method to-compiled(self): self.base + tostring(self.serial) end,
    method tosource(self): PP.str(self.toname()) end,
    method tosourcestring(self): self.to-compiled() end,
    method toname(self): self.base end,
    method key(self): "atom#" + self.base + "#" + tostring(self.serial) end
sharing:
  method _lessthan(self, other): self.key() < other.key() end,
  method _lessequal(self, other): self.key() <= other.key() end,
  method _greaterthan(self, other): self.key() > other.key() end,
  method _greaterequal(self, other): self.key() >= other.key() end,
  method _equals(self, other, eq): eq(self.key(), other.key()) end,
  method _output(self): VS.vs-str(self.tosourcestring()) end,
  method visit(self, visitor):
    self._match(visitor, lam(val): raise("No visitor field for " + tostring(self)) end)
  end
end

fun MakeName(start):
  var count = start
  fun atom(base :: String) block:
    count := 1 + count
    s-atom(base, count)
  end
  {
    reset: lam(): count := start end,
    s-underscore: s-underscore,
    s-name: s-name,
    s-global: s-global,
    s-type-global: s-type-global,
    make-atom: atom,
    is-s-underscore: is-s-underscore,
    is-s-name: is-s-name,
    is-s-global: is-s-global,
    is-s-atom: is-s-atom,
  }
end

global-names = MakeName(0)

data AppInfo:
  | app-info-c(is-recursive :: Boolean, is-tail :: Boolean)
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(val): raise("No visitor field for " + tostring(self)) end)
  end
end

fun funlam-tosource(funtype, name, params, args :: List<Bind>,
    ann :: Ann, doc :: String, body :: Expr, _check :: Option<Expr>, blocky :: Boolean) -> PP.PPrintDoc:
  typarams =
    if is-nothing(params): PP.mt-doc
    else: PP.surround-separate(INDENT, 0, PP.mt-doc, PP.langle, PP.commabreak, PP.rangle,
        params.map(_.tosource()))
    end
  arg-list = PP.nest(INDENT,
    PP.surround-separate(INDENT, 0, PP.lparen + PP.rparen, PP.lparen, PP.commabreak, PP.rparen,
      args.map(lam(a): a.tosource() end)))
  fname =
    if PP.is-mt-doc(name): funtype + typarams
    else: PP.group(funtype + break-one + name + typarams)
    end
  fann =
    if is-a-blank(ann) or is-nothing(ann): PP.mt-doc
    else: break-one + str-arrowspace + ann.tosource()
    end
  fblockycolon =
    if blocky: break-one + str-block
    else: str-colon
    end
  header = PP.group(fname + arg-list + fann + fblockycolon)
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

fun blocky-colon(blocky):
  if blocky: break-one + str-block else: str-colon end
end

data Program:
  | s-program(l :: Loc, _provide :: Provide, provided-types :: ProvideTypes, imports :: List<Import>, block :: Expr) with:
    method label(self): "s-program" end,
    method tosource(self):
      PP.group(
        PP.vert(
          [list:
            self._provide.tosource(),
            self.provided-types.tosource()]
            + self.imports.map(_.tosource())
            + [list: self.block.tosource()]
          ))
    end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(val): raise("No visitor field for " + self.label()) end)
  end
end

data Import:
  | s-include(l :: Loc, mod :: ImportType) with:
    method label(self): "s-include" end,
    method tosource(self):
      PP.flow([list: str-include, self.mod.tosource()])
    end
  | s-import(l :: Loc, file :: ImportType, name :: Name) with:
    method label(self): "s-import" end,
    method tosource(self):
      PP.flow([list: str-import, self.file.tosource(), str-as, self.name.tosource()])
    end
  | s-import-fields(l :: Loc, fields :: List<Name>, file :: ImportType) with:
    method label(self): "s-import-fields" end,
    method tosource(self):
      PP.flow([list: str-import,
          PP.flow-map(PP.commabreak, _.tosource(), self.fields),
          str-from, self.file.tosource()])
    end
  | s-import-complete(
      l :: Loc,
      values :: List<Name>,
      types :: List<Name>,
      import-type :: ImportType,
      vals-name :: Name,
      types-name :: Name) with:
    method label(self): "s-import-complete" end,
    method tosource(self):
      PP.flow([list: str-import,
          PP.flow-map(PP.commabreak, _.tosource(), self.values + self.types),
          str-from,
          self.import-type.tosource(),
          str-as,
          self.vals-name.tosource(),
          self.types-name.tosource()])
    end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(val): raise("No visitor field for " + self.label()) end)
  end
end

data ProvidedValue:
  # INVARIANT(joe): all a-names in Ann are defined in the lists of
  # ProvidedAlias or ProvidedDatatype
  | p-value(l :: Loc, v :: Name, ann :: Ann) with:
    method label(self):
      "p-value"
    end,
    method tosource(self):
      PP.infix(INDENT, 1, str-coloncolon, PP.str(self.v.toname()), self.ann.tosource())
    end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(val): raise("No visitor field for " + tostring(self)) end)
  end
end

data ProvidedAlias:
  | p-alias(l :: Loc, in-name :: Name, out-name :: Name, mod :: Option<ImportType>) with:
    method label(self):
      "p-alias"
    end,
    method tosource(self):
      PP.infix(INDENT, 1, str-as, PP.str(self.in-name.toname()), PP.str(self.out-name.toname()))
    end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(val): raise("No visitor field for " + tostring(self)) end)
  end
end

data ProvidedDatatype:
  | p-data(l :: Loc, d :: Name, mod :: Option<ImportType>) with:
    method label(self):
      "p-data"
    end,
    method tosource(self):
      PP.str(self.d.toname())
    end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(val): raise("No visitor field for " + tostring(self)) end)
  end
end

data Provide:
  | s-provide(l :: Loc, block :: Expr) with:
    method label(self): "s-provide" end,
    method tosource(self):
      PP.soft-surround(INDENT, 1, str-provide,
        self.block.tosource(), str-end)
    end
  | s-provide-complete(
      l :: Loc,
      values :: List<ProvidedValue>,
      aliases :: List<ProvidedAlias>,
      data-definitions :: List<ProvidedDatatype>
    ) with:
    method label(self): "s-provide" end,
    method tosource(self):
      PP.str("provide-complete") + PP.parens(PP.flow-map(PP.commabreak, lam(x): x end, [list:
            PP.infix(INDENT, 1, str-colon,PP.str("Values"),
              PP.brackets(PP.flow-map(PP.commabreak, _.tosource(), self.values))),
            PP.infix(INDENT, 1, str-colon,PP.str("Aliases"),
              PP.brackets(PP.flow-map(PP.commabreak, _.tosource(), self.aliases))),
            PP.infix(INDENT, 1, str-colon,PP.str("Data"),
              PP.brackets(PP.flow-map(PP.commabreak, _.tosource(), self.data-definitions)))]))
    end
  | s-provide-all(l :: Loc) with:
    method label(self): "s-provide-all" end,
    method tosource(self): str-provide-star end
  | s-provide-none(l :: Loc) with:
    method label(self): "s-provide-none" end,
    method tosource(self): PP.mt-doc end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(val): raise("No visitor field for " + self.label()) end)
  end
end

data ProvideTypes:
  | s-provide-types(l :: Loc, ann :: List<AField>) with:
    method label(self): "a-provide-type" end,
    method tosource(self):
      PP.surround-separate(INDENT, 1, str-provide-types + break-one + PP.lbrace + PP.rbrace,
        str-provide-types + break-one + PP.lbrace, PP.commabreak, PP.rbrace,
        self.ann.map(_.tosource()))
    end
  | s-provide-types-all(l :: Loc) with:
    method label(self): "s-provide-types-all" end,
    method tosource(self): str-provide-types-star end
  | s-provide-types-none(l :: Loc) with:
    method label(self): "s-provide-types-none" end,
    method tosource(self): PP.mt-doc end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(val): raise("No visitor field for " + self.label()) end)
  end
end


data ImportType:
  | s-const-import(l :: Loc, mod :: String) with:
    method label(self): "s-const-import" end,
    method tosource(self): PP.str(self.mod) end
  | s-special-import(l :: Loc, kind :: String, args :: List<String>) with:
    method label(self): "s-special-import" end,
    method tosource(self):
      PP.group(PP.str(self.kind)
          + PP.parens(PP.nest(INDENT,
            PP.separate(PP.commabreak, self.args.map(torepr).map(PP.str)))))
    end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(val): raise("No visitor field for " + self.label()) end)
  end
end

data Hint:
  | h-use-loc(l :: Loc) with:
    method tosource(self): str-use-loc + PP.parens(PP.str(tostring(self.l))) end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(val): raise("No visitor field for " + self.label()) end)
  end
end

data LetBind:
  | s-let-bind(l :: Loc, b :: Bind, value :: Expr) with:
    method tosource(self):
      PP.group(PP.nest(INDENT, self.b.tosource() + str-spaceequal + break-one + self.value.tosource()))
    end
  | s-var-bind(l :: Loc, b :: Bind, value :: Expr) with:
    method tosource(self):
      PP.group(PP.nest(INDENT, PP.str("var ") + self.b.tosource() + str-spaceequal + break-one + self.value.tosource()))
    end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(val): raise("No visitor field for " + self.label()) end)
  end
end

data LetrecBind:
  | s-letrec-bind(l :: Loc, b :: Bind, value :: Expr) with:
    method tosource(self):
      PP.group(PP.nest(INDENT, self.b.tosource() + str-spaceequal + break-one + self.value.tosource()))
    end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(val): raise("No visitor field for " + self.label()) end)
  end
end

data TypeLetBind:
  | s-type-bind(l :: Loc, name :: Name, params :: List<Name>, ann :: Ann) with:
    method label(self): "s-type-bind" end,
    method tosource(self):
      params = PP.surround-separate(2 * INDENT, 0, PP.mt-doc, PP.langle, PP.commabreak, PP.rangle,
        self.params.map(_.tosource()))
      PP.group(PP.nest(INDENT, self.name.tosource() + params + str-spaceequal + break-one + self.ann.tosource()))
    end
  | s-newtype-bind(l :: Loc, name :: Name, namet :: Name) with:
    method label(self): "s-newtype-bind" end,
    method tosource(self):
      PP.group(PP.nest(INDENT, str-newtype + self.name.tosource()
          + break-one + str-as
          + break-one + self.namet.tosource()))
    end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(val): raise("No visitor field for " + self.label()) end)
  end
end

data DefinedValue:
  | s-defined-value(name :: String, value :: Expr) with:
    method label(self): "s-defined-value" end,
    method tosource(self):
      PP.infix(INDENT, 1, str-colon, PP.str(self.name), self.value.tosource())
    end
  | s-defined-var(name :: String, id :: Name) with:
    method label(self): "s-defined-var" end,
    method tosource(self):
      PP.infix(INDENT, 1, str-colon, PP.str(self.name), PP.str(self.id.toname()))
    end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(val): raise("No visitor field for " + self.label()) end)
  end
end
data DefinedType:
  | s-defined-type(name :: String, typ :: Ann) with:
    method label(self): "s-defined-type" end,
    method tosource(self):
      PP.infix(INDENT, 1, str-coloncolon, PP.str(self.name), self.typ.tosource())
    end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(val): raise("No visitor field for " + self.label()) end)
  end
end

fun is-binder(expr):
  is-s-let(expr) or is-s-fun(expr) or is-s-var(expr) or is-s-rec(expr)
end

data Expr:
  | s-module(
      l :: Loc,
      answer :: Expr,
      defined-values :: List<DefinedValue>,
      defined-types :: List<DefinedType>,
      provided-values :: Expr,
      provided-types :: List<AField>,
      checks :: Expr) with:
    method label(self): "s-module" end,
    method tosource(self):
      PP.str("Module") + PP.parens(PP.flow-map(PP.commabreak, lam(x): x end, [list:
            PP.infix(INDENT, 1, str-colon, PP.str("Answer"), self.answer.tosource()),
            PP.infix(INDENT, 1, str-colon,PP.str("DefinedValues"),
              PP.brackets(PP.flow-map(PP.commabreak, _.tosource(), self.defined-values))),
            PP.infix(INDENT, 1, str-colon,PP.str("DefinedTypes"),
              PP.brackets(PP.flow-map(PP.commabreak, _.tosource(), self.defined-types))),
            PP.infix(INDENT, 1, str-colon, PP.str("Provides"), self.provided-values.tosource()),
            PP.infix(INDENT, 1, str-colon,PP.str("Types"),
              PP.brackets(PP.flow-map(PP.commabreak, _.tosource(), self.provided-types))),
            PP.infix(INDENT, 1, str-colon, PP.str("checks"), self.checks.tosource())]))
    end
  | s-template(l :: Loc) with:
    method label(self): "s-template" end,
    method tosource(self): PP.str("...") end
  | s-type-let-expr(l :: Loc, binds :: List<TypeLetBind>, body :: Expr, blocky :: Boolean) with:
    method label(self): "s-type-let" end,
    method tosource(self):
      header = PP.surround-separate(2 * INDENT, 1, str-type-let, str-type-let + PP.str(" "), PP.commabreak, PP.mt-doc,
          self.binds.map(_.tosource()))
        + blocky-colon(self.blocky)
      PP.surround(INDENT, 1, header, self.body.tosource(), str-end)
    end
  | s-let-expr(l :: Loc, binds :: List<LetBind>, body :: Expr, blocky :: Boolean) with:
    method label(self): "s-let" end,
    method tosource(self):
      header = PP.surround-separate(2 * INDENT, 1, str-let, str-let + PP.str(" "), PP.commabreak, PP.mt-doc,
          self.binds.map(_.tosource()))
          + blocky-colon(self.blocky)
      PP.surround(INDENT, 1, header, self.body.tosource(), str-end)
    end
  | s-letrec(l :: Loc, binds :: List<LetrecBind>, body :: Expr, blocky :: Boolean) with:
    method label(self): "s-letrec" end,
    method tosource(self):
      header = PP.surround-separate(2 * INDENT, 1, str-letrec, str-letrec + PP.str(" "), PP.commabreak, PP.mt-doc,
          self.binds.map(_.tosource()))
          + blocky-colon(self.blocky)
      PP.surround(INDENT, 1, header, self.body.tosource(), str-end)
    end
  | s-hint-exp(l :: Loc, hints :: List<Hint>, exp :: Expr) with:
    method label(self): "s-hint-exp" end,
    method tosource(self):
      PP.flow-map(PP.hardline, lam(h): str-comment + h.tosource() end, self.hints) + PP.hardline
        + self.e.tosource()
    end
  | s-instantiate(l :: Loc, expr :: Expr, params :: List<Ann>) with:
    method label(self): "s-instantiate" end,
    method tosource(self):
      PP.group(self.expr.tosource() +
        PP.surround-separate(INDENT, 0, PP.mt-doc, PP.langle, PP.commabreak, PP.rangle,
            self.params.map(_.tosource())))
    end
  | s-block(l :: Loc, stmts :: List<Expr>) with:
    method label(self): "s-block" end,
    method tosource(self):
      PP.flow-map(PP.hardline, _.tosource(), self.stmts)
    end
  | s-user-block(l :: Loc, body :: Expr) with:
    method label(self): "s-user-block" end,
    method tosource(self):
      PP.surround(INDENT, 1, str-block, self.body.tosource(), str-end)
    end
  | s-fun(
      l :: Loc,
      name :: String,
      params :: List<Name>, # Type parameters
      args :: List<Bind>, # Value parameters
      ann :: Ann, # return type
      doc :: String,
      body :: Expr,
      _check-loc :: Option<Loc>,
      _check :: Option<Expr>,
      blocky :: Boolean
    ) with:
      method label(self): "s-fun" end,
    method tosource(self):
      funlam-tosource(str-fun,
        PP.str(self.name), self.params, self.args, self.ann, self.doc, self.body, self._check, self.blocky)
    end
  | s-type(l :: Loc, name :: Name, params :: List<Name>, ann :: Ann) with:
    method label(self): "s-type" end,
    method tosource(self):
      params = PP.surround-separate(2 * INDENT, 0, PP.mt-doc, PP.langle, PP.commabreak, PP.rangle,
        self.params.map(_.tosource()))
      PP.group(PP.nest(INDENT,
          str-type + self.name.tosource() + params + str-spaceequal + break-one + self.ann.tosource()))
    end
  | s-newtype(l :: Loc, name :: Name, namet :: Name) with:
    method label(self): "s-newtype" end,
    method tosource(self):
      PP.group(PP.nest(INDENT, str-newtype + self.name.tosource()
          + break-one + str-as
          + break-one + self.namet.tosource()))
    end
  | s-var(l :: Loc, name :: Bind, value :: Expr) with:
    method label(self): "s-var" end,
    method tosource(self):
      str-var
        + PP.group(PP.nest(INDENT, self.name.tosource()
            + str-spaceequal + break-one + self.value.tosource()))
    end
  | s-rec(l :: Loc, name :: Bind, value :: Expr) with:
    method label(self): "s-rec" end,
    method tosource(self):
      str-rec
        + PP.group(PP.nest(INDENT, self.name.tosource()
            + str-spaceequal + break-one + self.value.tosource()))
    end
  | s-let(l :: Loc, name :: Bind, value :: Expr, keyword-val :: Boolean) with:
    method label(self): "s-let" end,
    method tosource(self):
      PP.group(PP.nest(INDENT,
          if self.keyword-val: str-val else: PP.mt-doc end
            + self.name.tosource() + str-spaceequal + break-one + self.value.tosource()))
    end
  | s-ref(l :: Loc, ann :: Option<Ann>) with:
    method label(self): "s-ref" end,
    method tosource(self):
      cases(Option) self.ann:
        | none => PP.str("bare-ref")
        | some(ann) =>
          PP.group(PP.str("ref ") + ann.tosource())
      end
    end
  | s-contract(l :: Loc, name :: Name, ann :: Ann) with:
    method label(self): "s-contract" end,
    method tosource(self):
      PP.infix(INDENT, 1, str-coloncolon, self.name.tosource(), self.ann.tosource())
    end
  | s-when(l :: Loc, test :: Expr, block :: Expr, blocky :: Boolean) with:
    method label(self): "s-when" end,
    method tosource(self):
      PP.soft-surround(INDENT, 1,
        str-when + PP.parens(self.test.tosource()) + blocky-colon(self.blocky),
        self.block.tosource(),
        str-end)
    end
  | s-assign(l :: Loc, id :: Name, value :: Expr) with:
    method label(self): "s-assign" end,
    method tosource(self):
      PP.group(PP.nest(INDENT, self.id.tosource() + str-spacecolonequal + break-one + self.value.tosource()))
    end
  | s-if-pipe(l :: Loc, branches :: List<IfPipeBranch>, blocky :: Boolean) with:
    method label(self): "s-if-pipe" end,
    method tosource(self):
      PP.surround-separate(INDENT, 1, str-ask + blocky-colon(self.blocky) + str-space + str-end,
        PP.group(str-ask + blocky-colon(self.blocky)), break-one, str-end,
        self.branches.map(lam(b): PP.group(b.tosource()) end))
    end
  | s-if-pipe-else(l :: Loc, branches :: List<IfPipeBranch>, _else :: Expr, blocky :: Boolean) with:
    method label(self): "s-if-pipe-else" end,
    method tosource(self):
      body = PP.separate(break-one, self.branches.map(lam(b): PP.group(b.tosource()) end))
        + break-one + PP.group(str-pipespace + str-otherwisecolon + break-one + self._else.tosource())
      PP.surround(INDENT, 1, PP.group(str-ask + blocky-colon(self.blocky)), body, str-end)
    end
  | s-if(l :: Loc, branches :: List<IfBranch>, blocky :: Boolean) with:
    method label(self): "s-if" end,
    method tosource(self):
      first-branch =
        if self.blocky: self.branches.first.tosource-blocky()
        else: self.branches.first.tosource()
        end
      first-sep =
        if is-link(self.branches.rest): break-one + str-elsespace
        else: PP.mt-doc
        end
      branches = PP.separate(break-one + str-elsespace,
        self.branches.rest.map(lam(b): b.tosource() end))
      PP.group(first-branch + first-sep + branches + break-one + str-end)
    end
  | s-if-else(l :: Loc, branches :: List<IfBranch>, _else :: Expr, blocky :: Boolean) with:
    method label(self): "s-if-else" end,
    method tosource(self):
      first-branch =
        if self.blocky: self.branches.first.tosource-blocky()
        else: self.branches.first.tosource()
        end
      first-sep =
        if is-link(self.branches.rest): break-one + str-elsespace
        else: PP.mt-doc
        end
      branches = PP.separate(break-one + str-elsespace,
        self.branches.rest.map(lam(b): b.tosource() end))
      _else = str-elsecolon + PP.nest(INDENT, break-one + self._else.tosource())
      PP.group(first-branch + first-sep + branches + break-one + _else + break-one + str-end)
    end
  | s-cases(l :: Loc, typ :: Ann, val :: Expr, branches :: List<CasesBranch>, blocky :: Boolean) with:
    method label(self): "s-cases" end,
    method branches-loc(self):
      first-loc = self.branches.first.l
      last-loc = self.branches.last().l
      S.srcloc(
        self.l.source,
        first-loc.start-line,
        first-loc.start-column,
        first-loc.start-char,
        last-loc.end-line,
        last-loc.end-column,
        last-loc.end-char)
    end,
    method tosource(self):
      header = str-cases + PP.parens(self.typ.tosource()) + break-one
        + self.val.tosource() + blocky-colon(self.blocky)
      PP.surround-separate(INDENT, 1, header + str-space + str-end,
        PP.group(header), break-one, str-end,
        self.branches.map(lam(b): PP.group(b.tosource()) end))
    end
  | s-cases-else(l :: Loc, typ :: Ann, val :: Expr, branches :: List<CasesBranch>, _else :: Expr, blocky :: Boolean) with:
    method label(self): "s-cases-else" end,
    method tosource(self):
      header = str-cases + PP.parens(self.typ.tosource()) + break-one
        + self.val.tosource() + blocky-colon(self.blocky)
      body = PP.separate(break-one, self.branches.map(lam(b): PP.group(b.tosource()) end))
        + break-one + PP.group(str-elsebranch + break-one + self._else.tosource())
      PP.surround(INDENT, 1, PP.group(header), body, str-end)
    end
  | s-op(l :: Loc, op-l :: Loc, op :: String, left :: Expr, right :: Expr) with:
    # This should be left-associated, always.
    method label(self): "s-op" end,
    method tosource(self):
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
  | s-check-test(l :: Loc, op :: CheckOp, refinement :: Option<Expr>, left :: Expr, right :: Option<Expr>) with:
    # Only 's-op-is' and 's-op-is-not' can have a refinement. (Checked in wf)
    # Only 's-op-raises-not' can lack a RHS. (Guaranteed by parsing; maintain this invariant!)
    method label(self): "s-check-test" end,
    method tosource(self):
      fun option-tosource(opt):
        cases(Option) opt:
          | none     => PP.mt-doc
          | some(ast) => ast.tosource()
        end
      end
      cases(Option) self.refinement:
        | none =>
          PP.infix(INDENT, 1, self.op.tosource(), self.left.tosource(), option-tosource(self.right))
        | some(refinement) =>
          PP.infix(INDENT, 1,
            PP.infix(INDENT, 0, str-percent, self.op.tosource(), PP.parens(refinement.tosource())),
            self.left.tosource(), option-tosource(self.right))
      end
    end
  | s-check-expr(l :: Loc, expr :: Expr, ann :: Ann) with:
    method label(self): "s-check-expr" end,
    method tosource(self):
      PP.infix(INDENT, 1, str-coloncolon, self.expr.tosource(), self.ann.tosource())
    end
  | s-paren(l :: Loc, expr :: Expr) with:
    method label(self): "s-paren" end,
    method tosource(self): PP.parens(self.expr.tosource()) end
  | s-lam(
      l :: Loc,
      name :: String, # Declared function name, or "" if anonymous lambda
      params :: List<Name>, # Type parameters
      args :: List<Bind>, # Value parameters
      ann :: Ann, # return type
      doc :: String,
      body :: Expr,
      _check-loc :: Option<Loc>,
      _check :: Option<Expr>,
      blocky :: Boolean
    ) with:
    method label(self): "s-lam" end,
    method tosource(self):
      funlam-tosource(str-lam,
        PP.mt-doc, self.params, self.args, self.ann, self.doc, self.body, self._check, self.blocky)
    end
  | s-method(
      l :: Loc,
      name :: String, # Declared method name, or "" if anonymous method
      params :: List<Name>, # Type parameters
      args :: List<Bind>, # Value parameters
      ann :: Ann, # return type
      doc :: String,
      body :: Expr,
      _check-loc :: Option<Loc>,
      _check :: Option<Expr>,
      blocky :: Boolean
    ) with:
    method label(self): "s-method" end,
    method tosource(self):
      funlam-tosource(str-method,
        PP.mt-doc, self.params, self.args, self.ann, self.doc, self.body, self._check, self.blocky)
    end
  | s-extend(l :: Loc, supe :: Expr, fields :: List<Member>) with:
    method label(self): "s-extend" end,
    method tosource(self):
      PP.group(self.supe.tosource() + str-period
          + PP.surround-separate(INDENT, 1, PP.lbrace + PP.rbrace,
          PP.lbrace, PP.commabreak, PP.rbrace, self.fields.map(_.tosource())))
    end,
    method field-loc(self):
      S.srcloc(
      self.l.source,
      self.supe.l.end-line,
      self.supe.l.end-column + 1,
      self.supe.l.end-char + 1,
      self.l.end-line,
      self.l.end-column,
      self.l.end-char)
    end,
  | s-update(l :: Loc, supe :: Expr, fields :: List<Member>) with:
    method label(self): "s-update" end,
    method tosource(self):
      PP.group(self.supe.tosource() + str-bang
          + PP.surround-separate(INDENT, 1, PP.lbrace + PP.rbrace,
          PP.lbrace, PP.commabreak, PP.rbrace, self.fields.map(_.tosource())))
    end
  | s-tuple(l :: Loc, fields :: List<Expr>) with:
    method label(self): "s-tuple" end,
    method tosource(self):
      PP.surround-separate(INDENT, 1, PP.str("Empty tuple shoudn't happen"), 
        PP.lbrace, PP.semibreak, PP.rbrace, self.fields.map(_.tosource()))
    end
  | s-tuple-get(l :: Loc, tup :: Expr, index :: Number, index-loc :: Loc) with:
    method label(self): "s-tuple-get" end,
    method tosource(self): self.tup.tosource() + PP.str(".") + PP.lbrace + PP.number(self.index) + PP.rbrace
    end 
  | s-obj(l :: Loc, fields :: List<Member>) with:
    method label(self): "s-obj" end,
    method tosource(self):
      PP.surround-separate(INDENT, 1, PP.lbrace + PP.rbrace,
        PP.lbrace, PP.commabreak, PP.rbrace, self.fields.map(_.tosource()))
    end
  | s-array(l :: Loc, values :: List<Expr>) with:
    method label(self): "s-array" end,
    method tosource(self):
      PP.surround-separate(INDENT, 0, PP.str("[raw-array: ]"), PP.str("[raw-array: "), PP.commabreak, PP.rbrack,
        self.values.map(_.tosource()))
    end
  | s-construct(l :: Loc, modifier :: ConstructModifier, constructor :: Expr, values :: List<Expr>) with:
    method label(self): "s-construct" end,
    method tosource(self):
      prefix = PP.lbrack
        + PP.group(PP.separate(PP.sbreak(1), [list: self.modifier.tosource(), self.constructor.tosource()]))
        + str-colonspace
      if is-empty(self.values): prefix + PP.rbrack
      else:
        PP.surround(INDENT, 0, prefix, PP.separate(PP.commabreak, self.values.map(_.tosource())), PP.rbrack)
      end
    end
  | s-app(l :: Loc, _fun :: Expr, args :: List<Expr>) with:
    method label(self): "s-app" end,
    method args-loc(self):
      if is-empty(self.args):
        S.srcloc(
          self.l.source,
          self._fun.l.end-line,
          self._fun.l.end-column,
          self._fun.l.end-char,
          self.l.end-line,
          self.l.end-column,
          self.l.end-char)
      else:
        first = self.args.first.l
        last = self.args.last().l
        S.srcloc(
          self.l.source,
          first.start-line,
          first.start-column,
          first.start-char,
          last.end-line,
          last.end-column,
          last.end-char)
      end
    end,
    method tosource(self):
      PP.group(self._fun.tosource()
          + PP.parens(PP.nest(INDENT,
            PP.separate(PP.commabreak, self.args.map(_.tosource())))))
    end
  | s-app-enriched(l :: Loc, _fun :: Expr, args :: List<Expr>, app-info :: AppInfo) with:
    # this is used only in the step before transforming the program to ANF
    method label(self): "s-app" end,
    method tosource(self):
      PP.group(self._fun.tosource()
          + PP.parens(PP.nest(INDENT,
            PP.separate(PP.commabreak, self.args.map(_.tosource())))))
    end
  | s-prim-app(l :: Loc, _fun :: String, args :: List<Expr>) with:
    method label(self): "s-prim-app" end,
    method tosource(self):
      PP.group(PP.str(self._fun)
          + PP.parens(PP.nest(INDENT,
            PP.separate(PP.commabreak, self.args.map(_.tosource())))))
    end
  | s-prim-val(l :: Loc, name :: String) with:
    method label(self): "s-prim-val" end,
    method tosource(self): PP.str(self.name) end
  | s-id(l :: Loc, id :: Name) with:
    method label(self): "s-id" end,
    method tosource(self): self.id.tosource() end
  | s-id-var(l :: Loc, id :: Name) with:
    method label(self): "s-id-var" end,
    method tosource(self): PP.str("!") + self.id.tosource() end
  | s-id-letrec(l :: Loc, id :: Name, safe :: Boolean) with:
    method label(self): "s-id-letrec" end,
    method tosource(self): PP.str("~") + self.id.tosource() end
  | s-undefined(l :: Loc) with:
    method label(self): "s-undefined" end,
    method tosource(self): PP.str("undefined") end
  | s-srcloc(l :: Loc, loc :: Loc) with:
    method label(self): "s-srcloc" end,
    method tosource(self): PP.str(torepr(self.loc)) end
  | s-num(l :: Loc, n :: Number) with:
    method label(self): "s-num" end,
    method tosource(self): PP.number(self.n) end
  | s-frac(l :: Loc, num :: NumInteger, den :: NumInteger) with:
    method label(self): "s-frac" end,
    method tosource(self): PP.number(self.num) + PP.str("/") + PP.number(self.den) end
  | s-rfrac(l :: Loc, num :: NumInteger, den :: NumInteger) with:
    method label(self): "s-rfrac" end,
    method tosource(self): PP.str("~") + PP.number(self.num) + PP.str("/") + PP.number(self.den) end
  | s-bool(l :: Loc, b :: Boolean) with:
    method label(self): "s-bool" end,
    method tosource(self): PP.str(tostring(self.b)) end
  | s-str(l :: Loc, s :: String) with:
    method label(self): "s-str" end,
    method tosource(self): PP.str(torepr(self.s)) end
  | s-dot(l :: Loc, obj :: Expr, field :: String) with:
    method label(self): "s-dot" end,
    method tosource(self): PP.infix-break(INDENT, 0, str-period, self.obj.tosource(), PP.str(self.field)) end,
    method field-loc(self):
      S.srcloc(
      self.obj.l.source,
      self.l.end-line,
      self.l.end-column - string-length(self.field),
      self.l.end-char - string-length(self.field),
      self.l.end-line,
      self.l.end-column,
      self.l.end-char)

    end
  | s-get-bang(l :: Loc, obj :: Expr, field :: String) with:
    method label(self): "s-get-bang" end,
    method tosource(self): PP.infix-break(INDENT, 0, str-bang, self.obj.tosource(), PP.str(self.field)) end
  | s-bracket(l :: Loc, obj :: Expr, key :: Expr) with:
    method label(self): "s-bracket" end,
    method tosource(self): PP.infix-break(INDENT, 0, str-period, self.obj.tosource(),
        PP.surround(INDENT, 0, PP.lbrack, self.key.tosource(), PP.rbrack))
    end
  | s-data(
      l :: Loc,
      name :: String,
      params :: List<Name>, # type params
      mixins :: List<Expr>,
      variants :: List<Variant>,
      shared-members :: List<Member>,
      _check-loc :: Option<Loc>,
      _check :: Option<Expr>
      ) with:
    method label(self): "s-data" end,
    method tosource(self):
      fun optional-section(lbl, section):
        if PP.is-mt-doc(section): PP.mt-doc
        else: break-one + PP.group(PP.nest(INDENT, lbl + break-one + section))
        end
      end
      tys = PP.surround-separate(2 * INDENT, 0, PP.mt-doc, PP.langle, PP.commabreak, PP.rangle,
        self.params.map(_.tosource()))
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
      l :: Loc,
      name :: String,
      namet :: Name,
      params :: List<Name>, # type params
      mixins :: List<Expr>,
      variants :: List<Variant>,
      shared-members :: List<Member>,
      _check-loc :: Option<Loc>,
      _check :: Option<Expr>
    ) with:
    method label(self): "s-data-expr" end,
    method tosource(self):
      fun optional-section(lbl, section):
        if PP.is-mt-doc(section): PP.mt-doc
        else: break-one + PP.group(PP.nest(INDENT, lbl + break-one + section))
        end
      end
      tys = PP.surround-separate(2 * INDENT, 0, PP.mt-doc, PP.langle, PP.commabreak, PP.rangle,
        self.params.map(_.tosource()))
      header = str-data-expr + PP.str(self.name) + PP.comma + self.namet.tosource() + tys + str-colon
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
      l :: Loc,
      iterator :: Expr,
      bindings :: List<ForBind>,
      ann :: Ann,
      body :: Expr,
      blocky :: Boolean
    ) with:
    method label(self): "s-for" end,
    method tosource(self):
      header = PP.group(str-for
          + self.iterator.tosource()
          + PP.surround-separate(2 * INDENT, 0, PP.lparen + PP.rparen, PP.lparen, PP.commabreak, PP.rparen,
          self.bindings.map(lam(b): b.tosource() end))
          + PP.group(PP.nest(2 * INDENT,
            break-one + str-arrow + break-one + self.ann.tosource() + blocky-colon(self.blocky))))
      PP.surround(INDENT, 1, header, self.body.tosource(), str-end)
    end
  | s-check(
      l :: Loc,
      name :: Option<String>,
      body :: Expr,
      keyword-check :: Boolean
    ) with:
    method label(self): "s-check" end,
    method tosource(self):
      cases(Option) self.name:
        | none => PP.surround(INDENT, 1,
            if self.keyword-check: str-checkcolon else: str-examplescolon end,
            self.body.tosource(), str-end)
        | some(name) => PP.surround(INDENT, 1,
            if self.keyword-check: PP.str("check ") else: PP.str("examples ") end
              + PP.dquote(PP.str(name)) + str-colon,
            self.body.tosource(), str-end)
      end
    end
  | s-reactor(l :: Loc, fields :: List<Member>) with:
    method label(self): "s-reactor" end,
    method tosource(self):
      PP.surround-separate(INDENT, 1, PP.str("reactor: end"),
        PP.str("reactor:"), PP.commabreak, PP.str("end"), self.fields.map(_.tosource()))
    end
  | s-table-extend(l :: Loc,
      column-binds :: ColumnBinds,
      extensions :: List<TableExtendField>) with:
    method label(self): "s-table-extend" end,
    method tosource(self):
      maybe-using =
        cases(List) self.column-binds.binds:
          | empty => empty
          | link(_, _) => link(str-using,
              [list: PP.flow-map(PP.commabreak, _.tosource(),
                  self.column-binds.binds) + str-colon])
        end
      tbl-src =
        cases(List) maybe-using:
          | empty => self.column-binds.table.tosource() + str-colon
          | link(_,_) => self.column-binds.table.tosource()
        end
      header = PP.flow([list: str-extend, tbl-src] + maybe-using)
      PP.surround(INDENT, 1,
        header,
        PP.flow-map(PP.hardline, _.tosource(), self.extensions),
        str-end)
    end
    # s-table-update not yet implemented
  | s-table-update(l :: Loc,
      column-binds :: ColumnBinds,
      updates :: List<Member>)
  | s-table-select(l :: Loc,
      columns :: List<Name>,
      table   :: Expr) with:
    method label(self): "s-table-select" end,
    method tosource(self):
      PP.flow([list: str-select,
          PP.flow-map(PP.commabreak, _.tosource(), self.columns),
          str-from,
          self.table.tosource(),
          str-end])
    end
  | s-table-order(l :: Loc,
      table   :: Expr,
      ordering :: List<ColumnSort>) with:
    method label(self): "s-table-order" end,
    method tosource(self):
      PP.surround(INDENT, 1,
        PP.flow([list: str-order, self.table.tosource() + str-colon]),
        PP.flow-map(PP.commabreak, _.tosource(), self.ordering),
        str-end)
    end
  | s-table-filter(l :: Loc,
      column-binds :: ColumnBinds,
      predicate :: Expr) with:
    method label(self): "s-table-filter" end,
    method tosource(self):
      maybe-using =
        cases(List) self.column-binds.binds:
          | empty => empty
          | link(_, _) => link(str-using,
              [list: PP.flow-map(PP.commabreak, _.tosource(),
                  self.column-binds.binds) + str-colon])
        end
      tbl-src =
        cases(List) maybe-using:
          | empty => self.column-binds.table.tosource() + str-colon
          | link(_,_) => self.column-binds.table.tosource()
        end
      header = PP.flow([list: str-sieve, tbl-src] + maybe-using)
      PP.surround(INDENT, 1, header,
        self.predicate.tosource(),
        str-end)
    end
  | s-table-extract(l :: Loc,
      column :: Name,
      table   :: Expr) with:
    method label(self): "s-table-extract" end,
    method tosource(self):
      PP.flow([list: str-extract, self.column.tosource(),
          str-from, self.table.tosource(), str-end])
    end
  | s-table(
      l :: Loc,
      headers :: List<FieldName>,
      rows :: List<TableRow>)
    with:
    method label(self): "s-table" end,
    method tosource(self):
      PP.surround(INDENT, 1,
        PP.flow([list: str-tablecolon,
            PP.flow-map(PP.commabreak, _.tosource(), self.headers)]),
        PP.flow-map(PP.hardline, _.tosource(), self.rows),
        str-end)
    end
  | s-load-table(l :: Loc, headers :: List<FieldName>, spec :: List<LoadTableSpec>)
    with:
    method label(self): "s-load-table" end,
    method tosource(self):
      PP.surround(INDENT, 1,
        PP.flow([list: str-load-table,
            PP.flow-map(PP.commabreak, _.tosource(), self.headers)]),
        PP.flow-map(PP.hardline, _.tosource(), self.spec),
        str-end)
    end
  | s-spy-block(l :: Loc, message :: Option<Expr>, contents :: List<SpyField>) with:
    method label(self): "s-spy-block" end,
    method tosource(self):
      cases(Option<Expr>) self.message:
        | none =>
          PP.surround-separate(INDENT, 1, PP.str("spy: end"),
            PP.str("spy:"), PP.commabreak, str-end, self.contents.map(_.tosource()))
        | some(msg) =>
          msg-source = msg.tosource()
          PP.surround-separate(INDENT, 1, PP.str("spy ") + msg-source + PP.str(": end"),
            PP.str("spy ") + msg-source + str-colon, PP.commabreak, str-end, self.contents.map(_.tosource()))
      end
    end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end
end

data TableRow:
  | s-table-row(
      l :: Loc,
      elems :: List<Expr>)
    with:
    method label(self): "s-table-row" end,
    method tosource(self):
      PP.flow([list: str-rowcolon,
          PP.flow-map(PP.commabreak, _.tosource(), self.elems)])
    end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(val): raise("No visitor field for " + self.label()) end)
  end
end

data SpyField:
  | s-spy-name(l :: Loc, name :: Expr%(is-s-id)) with:
    method label(self): "s-spy-name" end,
    method tosource(self): self.name.tosource() end
  | s-spy-expr(l :: Loc, name :: String, value :: Expr) with:
    method label(self): "s-spy-expr" end,
    method tosource(self): 
      PP.nest(INDENT, PP.str(self.name) + str-colonspace + self.value.tosource())
    end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(val): raise("No visitor field for " + self.label()) end)
  end
end

data ConstructModifier:
  | s-construct-normal with:
    method label(self): "s-construct-normal" end,
    method tosource(self): PP.mt-doc end
  | s-construct-lazy with:
    method label(self): "s-construct-lazy" end,
    method tosource(self): PP.str("lazy") end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(val): raise("No visitor field for " + self.label()) end)
  end
end


data Bind:
  | s-bind(l :: Loc, shadows :: Boolean, id :: Name, ann :: Ann) with:
    method tosource(self):
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
    method label(self): "s-bind" end
  | s-tuple-bind(l :: Loc, fields :: List<Bind>, as-name :: Option<Bind>) with:
    method label(self): "s-tuple-bind" end,
    method tosource(self):
      main-pat = PP.surround-separate(INDENT, 1, PP.lbrace + PP.rbrace, PP.lbrace, PP.semibreak, PP.rbrace,
        self.fields.map(_.tosource()))
      cases(Option) self.as-name:
        | none => main-pat
        | some(n) => PP.infix(INDENT, 1, str-as, main-pat, n.tosource())
      end
    end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(val): raise("No visitor field for " + self.label()) end)
  end
end

data Member:
  | s-data-field(l :: Loc, name :: String, value :: Expr) with:
    method label(self): "s-data-field" end,
    method tosource(self):
      name-part = PP.str(self.name)
      PP.nest(INDENT, name-part + str-colonspace + self.value.tosource())
    end,
  | s-mutable-field(l :: Loc, name :: String, ann :: Ann, value :: Expr) with:
    method label(self): "s-mutable-field" end,
    method tosource(self):
      name-part = PP.str(self.name)
      PP.nest(INDENT, str-mutable + name-part + str-coloncolon + self.ann.tosource() + str-colonspace + self.value.tosource())
    end,
  | s-method-field(
      l :: Loc,
      name :: String,
      params :: List<Name>, # Type parameters
      args :: List<Bind>, # Value parameters
      ann :: Ann, # return type
      doc :: String,
      body :: Expr,
      _check-loc :: Option<Loc>,
      _check :: Option<Expr>,
      blocky :: Boolean
    ) with:
      method label(self): "s-method-field" end,
    method tosource(self):
      funlam-tosource(str-method,
        PP.str(self.name), self.params, self.args, self.ann, self.doc, self.body, self._check, self.blocky)
    end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(val): raise("No visitor field for " + self.label()) end)
  end
end

data FieldName:
  | s-field-name(l :: Loc, name :: String, ann :: Ann) with:
    method label(self): "s-field-name" end,
    method tosource(self):
      if is-a-blank(self.ann): PP.str(self.name)
      else: PP.infix(INDENT, 1, str-coloncolon, PP.str(self.name), self.ann.tosource())
      end
    end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end
end

data ForBind:
  | s-for-bind(l :: Loc, bind :: Bind, value :: Expr) with:
    method label(self): "s-for-bind" end,
    method tosource(self):
      PP.group(self.bind.tosource() + break-one + str-from + break-one + self.value.tosource())
    end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(val): raise("No visitor field for " + self.label()) end)
  end
end

data ColumnBinds:
  | s-column-binds(l :: Loc, binds :: List<Bind>, table :: Expr)
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end
end

data ColumnSortOrder:
  | ASCENDING with:
    method tosource(self):
      PP.str("ascending")
    end
  | DESCENDING with:
    method tosource(self):
      PP.str("descending")
    end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + torepr(self)) end)
  end
end

data ColumnSort:
  | s-column-sort(
      l         :: Loc,
      column    :: Name,
      direction :: ColumnSortOrder) with:
    method label(self): "s-column-sort" end,
    method tosource(self):
      PP.flow([list: self.column.tosource(), self.direction.tosource()])
    end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + torepr(self)) end)
  end
end

data TableExtendField:
  | s-table-extend-field(l :: Loc, name :: String, value :: Expr, ann :: Ann) with:
    method label(self): "s-table-extend-field" end,
    method tosource(self):
      name-part = PP.str(self.name)
      maybe-ann =
        if is-a-blank(self.ann):
          PP.mt-doc
        else:
          str-coloncolon + self.ann.tosource()
        end
      PP.nest(INDENT, name-part + maybe-ann + str-colonspace + self.value.tosource())
    end
  | s-table-extend-reducer(l :: Loc, name :: String, reducer :: Expr, col :: Name, ann :: Ann) with:
    method label(self): "s-table-extend-reducer" end,
    method tosource(self):
      name-part = PP.str(self.name)
      maybe-ann =
        if is-a-blank(self.ann):
          PP.mt-doc
        else:
          str-coloncolon + self.ann.tosource()
        end
      col-part = self.col.tosource()
      PP.nest(INDENT, name-part + maybe-ann + str-colonspace + self.reducer.tosource() + PP.str(" ") + str-of + col-part)
    end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + torepr(self)) end)
  end
end

data LoadTableSpec:
  | s-sanitize(l :: Loc, name :: Name, sanitizer :: Expr) with:
    method label(self): "s-sanitize" end,
    method tosource(self):
      name-part = self.name.tosource()
      PP.flow([list: str-sanitize, name-part, str-using, self.sanitizer.tosource()])
    end
  | s-table-src(l :: Loc, src :: Expr) with:
    method label(self): "s-table-src" end,
    method tosource(self):
      PP.flow([list: str-src, self.src.tosource()])
    end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + torepr(self)) end)
  end
end

data VariantMemberType:
  | s-normal with:
    method label(self): "s-normal" end,
    method tosource(self): PP.mt-doc end
  | s-mutable with:
    method label(self): "s-mutable" end,
    method tosource(self): PP.str("ref ") end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(val): raise("No visitor field for " + self.label()) end)
  end
end

data VariantMember:
  | s-variant-member(l :: Loc, member-type :: VariantMemberType, bind :: Bind) with:
    method label(self): "s-variant-member" end,
    method tosource(self):
      self.member-type.tosource() + self.bind.tosource()
    end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(val): raise("No visitor field for " + self.label()) end)
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
    method label(self): "s-variant" end,
    method tosource(self):
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
      l :: Loc,
      name :: String,
      with-members :: List<Member>
    ) with:
    method label(self): "s-singleton-variant" end,
    method tosource(self):
      header-nowith = PP.str(self.name)
      header = PP.group(header-nowith + break-one + str-with)
      withs = self.with-members.map(lam(m): m.tosource() end)
      if lists.is-empty(withs): header-nowith
      else: header + PP.group(PP.nest(INDENT, break-one + PP.separate(PP.commabreak, withs)))
      end
    end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(val): raise("No visitor field for " + self.label()) end)
  end
end

data IfBranch:
  | s-if-branch(l :: Loc, test :: Expr, body :: Expr) with:
    method label(self): "s-if-branch" end,
    method tosource(self):
      str-if
        + PP.nest(2 * INDENT, self.test.tosource() + str-colon)
        + PP.nest(INDENT, break-one + self.body.tosource())
    end,
    method tosource-blocky(self):
      str-if
        + PP.nest(2 * INDENT, self.test.tosource() + break-one + str-block)
        + PP.nest(INDENT, break-one + self.body.tosource())
    end

sharing:
  method visit(self, visitor):
    self._match(visitor, lam(val): raise("No visitor field for " + self.label()) end)
  end
end

data IfPipeBranch:
  | s-if-pipe-branch(l :: Loc, test :: Expr, body :: Expr) with:
    method label(self): "s-if-pipe-branch" end,
    method tosource(self):
      str-pipespace
        + PP.nest(2 * INDENT, self.test.tosource() + break-one + str-thencolon)
        + PP.nest(INDENT, break-one + self.body.tosource())
    end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(val): raise("No visitor field for " + self.label()) end)
  end
end

data CasesBindType:
  | s-cases-bind-ref with:
    method label(self): "s-cases-bind-ref" end,
    method tosource(self): PP.str("ref") end
  | s-cases-bind-normal with:
    method label(self): "s-cases-bind-normal" end,
    method tosource(self): PP.str("") end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(val): raise("No visitor field for " + tostring(self)) end)
  end
end

data CasesBind:
  | s-cases-bind(l :: Loc, field-type :: CasesBindType, bind :: Bind) with:
    method label(self): "s-cases-bind" end,
    method tosource(self):
      self.field-type.tosource() + PP.str(" ") + self.bind.tosource()
    end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(val): raise("No visitor field for " + self.label()) end)
  end
end

data CasesBranch:
  | s-cases-branch(l :: Loc, pat-loc :: Loc, name :: String, args :: List<CasesBind>, body :: Expr) with:
    method label(self): "s-cases-branch" end,
    method tosource(self):
      PP.nest(INDENT,
        PP.group(PP.str("| " + self.name)
            + PP.surround-separate(INDENT, 0, PP.str("()"), PP.lparen, PP.commabreak, PP.rparen,
            self.args.map(lam(a): a.tosource() end)) + break-one + str-thickarrow) + break-one +
        self.body.tosource())
    end
  | s-singleton-cases-branch(l :: Loc, pat-loc :: Loc, name :: String, body :: Expr) with:
    method label(self): "s-singleton-cases-branch" end,
    method tosource(self):
      PP.nest(INDENT,
        PP.group(PP.str("| " + self.name) + break-one + str-thickarrow) + break-one + self.body.tosource())
    end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(val): raise("No visitor field for " + self.label()) end)
  end
end

fun ann-loc(ann):
  if is-a-blank(ann): dummy-loc
  else: ann.l
  end
end

fun get-op-fun-name(opname):
  ask:
    | opname == "op==" then: "equal-always"
    | opname == "op=~" then: "equal-now"
    | opname == "op<=>" then: "identical"
    | otherwise: raise("Unknown op: " + opname)
  end
end

data CheckOp:
  | s-op-is(l :: Loc) with:
    method label(self): "s-op-is" end,
    method tosource(self): str-is end
  | s-op-is-roughly(l :: Loc) with:
    method label(self): "s-op-is-roughly" end,
    method tosource(self): PP.str("is-roughly") end
  | s-op-is-op(l :: Loc, op :: String) with:
    method label(self): "s-op-is-op" end,
    method tosource(self): str-is + PP.str(string-substring(self.op, 2, string-length(self.op))) end
  | s-op-is-not(l :: Loc) with:
    method label(self): "s-op-is-not" end,
    method tosource(self): str-is-not end
  | s-op-is-not-op(l :: Loc, op :: String) with:
    method label(self): "s-op-is-not-op" end,
    method tosource(self): str-is-not + PP.str(string-substring(self.op, 2, string-length(self.op))) end
  | s-op-satisfies(l :: Loc) with:
    method label(self): "s-op-satisfies" end,
    method tosource(self): str-satisfies end
  | s-op-satisfies-not(l :: Loc) with:
    method label(self): "s-op-satisfies-not" end,
    method tosource(self): str-satisfies-not end
  | s-op-raises(l :: Loc) with:
    method label(self): "s-op-raises" end,
    method tosource(self): str-raises end
  | s-op-raises-other(l :: Loc) with:
    method label(self): "s-op-raises-other" end,
    method tosource(self): str-raises-other end
  | s-op-raises-not(l :: Loc) with:
    method label(self): "s-op-raises-not" end,
    method tosource(self): str-raises-not end
  | s-op-raises-satisfies(l :: Loc) with:
    method label(self): "s-op-raises-satisfies" end,
    method tosource(self): str-raises-satisfies end
  | s-op-raises-violates(l :: Loc) with:
    method label(self): "s-op-raises-violates" end,
    method tosource(self): str-raises-violates end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(val): raise("No visitor field for " + self.label()) end)
  end
end

data Ann:
  | a-blank with:
    method label(self): "a-blank" end,
    method tosource(self): str-any end,
  | a-any(l :: Loc) with:
    method label(self): "a-any" end,
    method tosource(self): str-any end,
  | a-name(l :: Loc, id :: Name) with:
    method label(self): "a-name" end,
    method tosource(self): self.id.tosource() end,
  | a-type-var(l :: Loc, id :: Name) with:
    method label(self): "a-type-var" end,
    method tosource(self): self.id.tosource() end,
  | a-arrow(l :: Loc, args :: List<Ann>, ret :: Ann, use-parens :: Boolean) with:
    method label(self): "a-arrow" end,
    method tosource(self):
      ann = PP.separate(str-space,
        [list:
          PP.separate(PP.commabreak, self.args.map(_.tosource())),
          str-arrow, self.ret.tosource()])
      if self.use-parens:
        PP.surround(INDENT, 0, PP.lparen, ann, PP.rparen)
      else:
        ann
      end
    end,
  | a-arrow-argnames(l :: Loc, args :: List<AField>, ret :: Ann, use-parens :: Boolean) with:
    method label(self): "a-arrow-argnames" end,
    method tosource(self):
      ann = PP.separate(str-space,
        [list:
          PP.surround(INDENT, 0, PP.lparen,
            PP.separate(PP.commabreak, self.args.map(_.tosource())),
            PP.rparen),
          str-arrow, self.ret.tosource()])
      if self.use-parens:
        PP.surround(INDENT, 0, PP.lparen, ann, PP.rparen)
      else:
        ann
      end
    end,
  | a-method(l :: Loc, args :: List<Ann>, ret :: Ann) with:
    method label(self): "a-method" end,
    method tosource(self): PP.str("NYI: A-method") end,
  | a-record(l :: Loc, fields :: List<AField>) with:
    method label(self): "a-record" end,
    method tosource(self):
      PP.surround-separate(INDENT, 1, PP.lbrace + PP.rbrace, PP.lbrace, PP.commabreak, PP.rbrace,
        self.fields.map(_.tosource()))
    end,
  | a-tuple(l :: Loc, fields :: List<AField>) with:
    method label(self): "a-tuple" end,
    method tosource(self):
      PP.surround-separate(INDENT, 1, PP.lbrace + PP.rbrace, PP.lbrace, PP.semibreak, PP.rbrace,
        self.fields.map(_.tosource()))
    end,
  | a-app(l :: Loc, ann :: Ann, args :: List<Ann>) with:
    method label(self): "a-app" end,
    method tosource(self):
      PP.group(self.ann.tosource()
          + PP.group(PP.langle + PP.nest(INDENT,
            PP.separate(PP.commabreak, self.args.map(_.tosource()))) + PP.rangle))
    end,
  | a-pred(l :: Loc, ann :: Ann, exp :: Expr) with:
    method label(self): "a-pred" end,
    method tosource(self): self.ann.tosource() + str-percent + PP.parens(self.exp.tosource()) end,
  | a-dot(l :: Loc, obj :: Name, field :: String) with:
    method label(self): "a-dot" end,
    method tosource(self): self.obj.tosource() + PP.str("." + self.field) end,
  | a-checked(checked :: Ann, residual :: Ann) with:
    method label(self): "a-checked" end,
    method tosource(self): self.residual.tosource() end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(val): raise("No visitor field for " + self.label()) end)
  end
end

data AField:
  | a-field(l :: Loc, name :: String, ann :: Ann) with:
    method label(self): "a-field" end,
    method tosource(self):
      if is-a-blank(self.ann): PP.str(self.name)
      else: PP.infix(INDENT, 1, str-coloncolon, PP.str(self.name), self.ann.tosource())
      end
    end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(val): raise("No visitor field for " + self.label()) end)
  end
end

fun make-checker-name(name): "is-" + name end

fun flatten(list-of-lists :: List):
  for fold(biglist from [list: ], piece from list-of-lists):
    biglist + piece
  end
end

fun binding-type-ids(stmt) -> List<Name>:
  cases(Expr) stmt:
    | s-newtype(l, name, _) => [list: {bind-type: "normal", name: name}]
    | s-type(l, name, params,  _) => [list: {bind-type: "normal", name: name}]
    | s-data(l, name, _, _, _, _, _, _) => [list: {bind-type: "data", name: s-name(l, name)}]
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
  fun bind-ids(b):
    cases(Bind) b:
      | s-bind(_,_,id,_) => [list: id]
      | s-tuple-bind(_, fields, as-name) =>
        extra = cases(Option) as-name:
          | none => empty
          | some(n) => [list: n.id]
        end
        tup-ids = for lists.foldr(acc from extra, f from fields):
          bind-ids(f) + acc
        end
        tup-ids
    end
  end
  cases(Expr) stmt:
    | s-let(_, b, _, _) => bind-ids(b)
    | s-var(_, b, _) => bind-ids(b)
    | s-rec(_, b, _) => bind-ids(b)
    | s-fun(l, name, _, _, _, _, _, _, _, _) => [list: s-name(l, name)]
    | s-data(l, name, _, _, variants, _, _, _) =>
      s-name(l, make-checker-name(name)) ^ link(_, flatten(variants.map(variant-ids)))
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
