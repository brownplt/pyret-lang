#lang pyret

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

  | s-module-global(s :: String) with:
    method to-compiled-source(self): PP.str(self.to-compiled()) end,
    method to-compiled(self): "$module$" + self.s end,
    method tosource(self): PP.str(self.s) end,
    method tosourcestring(self): "$module$" + self.s end,
    method toname(self): self.s end,
    method key(self): "mglobal#" + self.s end

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
    s-module-global: s-module-global,
    s-type-global: s-type-global,
    make-atom: atom,
    is-s-underscore: is-s-underscore,
    is-s-name: is-s-name,
    is-s-global: is-s-global,
    is-s-module-global: is-s-module-global,
    is-s-atom: is-s-atom,
  }
end

global-names = MakeName(0)

data AppInfo:
  | app-info-c(is-recursive :: Boolean, is-tail :: Boolean)
end
data PrimAppInfo:
  | prim-app-info-c(needs-step :: Boolean)
end

fun funlam-tosource(funtype, name, params, args :: List<Bind>,
    ann :: Ann, doc :: String, body :: Expr, _check :: Option<Expr>, blocky :: Boolean) -> PP.PPrintDoc:
  typarams =
    if is-empty(params): PP.mt-doc
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
    else: str-doc + PP.str(torepr(doc)) + PP.hardline
    end
  PP.surround(INDENT, 1, header, docstr + body.tosource(), footer)
end

fun blocky-colon(blocky):
  if blocky: break-one + str-block else: str-colon end
end

data Program:
  | s-program(l :: Loc, _provide :: Provide, provided-types :: ProvideTypes, provides :: List<ProvideBlock>, imports :: List<Import>, block :: Expr) with:
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
  | s-include-from(l :: Loc, mod :: List<Name>, specs :: List<IncludeSpec>) with:
    method label(self): "s-include" end,
    method tosource(self):
      PP.flow([list: str-include, str-from, PP.separate(str-period, self.mod.map(_.tosource())), str-colon,
        PP.separate(PP.commabreak, self.specs.map(_.tosource()))])
    end
  | s-import(l :: Loc, file :: ImportType, name :: Name) with:
    method label(self): "s-import" end,
    method tosource(self):
      PP.flow([list: str-import, self.file.tosource(), str-as, self.name.tosource()])
    end
  | s-import-types(l :: Loc, file :: ImportType, name :: Name, types :: Name) with:
    method label(self): "s-import-types" end,
    method tosource(self):
      PP.flow([list: str-import, self.file.tosource(), str-as, self.name.tosource(), PP.comma, self.types.tosource()])
    end
  | s-import-fields(l :: Loc, fields :: List<Name>, file :: ImportType) with:
    method label(self): "s-import-fields" end,
    method tosource(self):
      PP.flow([list: str-import,
          PP.flow-map(PP.commabreak, _.tosource(), self.fields),
          str-from, self.file.tosource()])
    end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(val): raise("No visitor field for " + self.label()) end)
  end
end

data IncludeSpec:
  | s-include-name(l :: Loc, name-spec :: NameSpec) with:
    method label(self): "s-include-name" end,
    method tosource(self): self.name-spec.tosource() end
  | s-include-data(l :: Loc, name-spec :: NameSpec, hidden :: List<Name>) with:
    method label(self): "s-include-data" end,
    method tosource(self): PP.flow([list: self.name-spec.tosource(), PP.str("hiding"), PP.separate(PP.str(","), self.hidden.map(_.tosource()))]) end
  | s-include-type(l :: Loc, name-spec :: NameSpec) with:
    method label(self): "s-include-type" end,
    method tosource(self): self.name-spec.tosource() end
  | s-include-module(l :: Loc, name-spec :: NameSpec) with:
    method label(self): "s-include-module" end,
    method tosource(self): self.name-spec.tosource() end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(val): raise("No visitor field for " + self.label()) end)
  end
end



data ProvidedModule:
  | p-module(l :: Loc, name :: String, v :: Name, uri :: String) with:
    method label(self):
      "p-module"
    end,
    method tosource(self):
      PP.infix(INDENT, 1, str-coloncolon, PP.str(self.v.toname()), self.ann.tosource())
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
end

data ProvidedAlias:
  | p-alias(l :: Loc, in-name :: Name, out-name :: Name, mod :: Option<ImportType>) with:
    method label(self):
      "p-alias"
    end,
    method tosource(self):
      PP.infix(INDENT, 1, str-as, PP.str(self.in-name.toname()), PP.str(self.out-name.toname()))
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
end

data Provide:
  | s-provide(l :: Loc, block :: Expr) with:
    method label(self): "s-provide" end,
    method tosource(self):
      PP.soft-surround(INDENT, 1, str-provide,
        self.block.tosource(), str-end)
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

data ProvideBlock:
  | s-provide-block(l :: Loc, specs :: List<ProvideSpec>) with:
    method label(self): "s-provide-block" end,
    method tosource(self): PP.str("provide-block") end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(val): raise("No visitor field for " + self.label()) end)
  end
end

data ProvideSpec:
  | s-provide-name(l :: Loc, name-spec :: NameSpec) with:
    method label(self): "s-provide-name" end,
    method tosource(self): self.name-spec.tosource() end
  | s-provide-data(l :: Loc, name-spec :: NameSpec, hidden :: List<Name>) with:
    method label(self): "s-provide-data" end,
    method tosource(self): PP.flow([list: self.name-spec.tosource(), PP.str("hiding"), PP.separate(PP.str(","), self.hidden.map(_.tosource()))]) end
  | s-provide-type(l :: Loc, name-spec :: NameSpec) with:
    method label(self): "s-provide-type" end,
    method tosource(self): self.name-spec.tosource() end
  | s-provide-module(l :: Loc, name-spec :: NameSpec) with:
    method label(self): "s-provide-module" end,
    method tosource(self): self.name-spec.tosource() end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(val): raise("No visitor field for " + self.label()) end)
  end
end

data NameSpec:
  | s-star(l :: Loc, hidden :: List<Name>) with:
    method label(self): "s-star" end,
    method tosource(self): PP.flow([list: PP.str("*"), PP.separate(PP.str(","), self.hidden.map(_.tosource()))]) end
  | s-module-ref(l :: Loc, path :: List<Name>, as-name :: Option<Name>) with:
    method label(self): "s-module-ref" end,
    method tosource(self):
      cases(Option) self.as-name:
        | none => 
          PP.flow([list: PP.separate(PP.str(","), self.path.map(_.tosource()))])
        | some(name) =>
          PP.flow([list: PP.separate(PP.str(","), self.path.map(_.tosource()), PP.str("as"), name.tosource())])
      end
    end
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

data DefinedModule:
  | s-defined-module(name :: String, value :: Name, uri :: String) with:
    method label(self): "s-defined-module" end,
    method tosource(self):
      PP.infix(INDENT, 1, str-colon, PP.str(self.name), PP.str(self.uri))
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
      defined-modules :: List<DefinedModule>,
      defined-values :: List<DefinedValue>,
      defined-types :: List<DefinedType>,
      checks :: Expr) with:
    method label(self): "s-module" end,
    method tosource(self):
      PP.str("Module") + PP.parens(PP.flow-map(PP.commabreak, lam(x): x end, [list:
            PP.infix(INDENT, 1, str-colon, PP.str("Answer"), self.answer.tosource()),
            PP.infix(INDENT, 1, str-colon,PP.str("DefinedValues"),
              PP.brackets(PP.flow-map(PP.commabreak, _.tosource(), self.defined-values))),
            PP.infix(INDENT, 1, str-colon,PP.str("DefinedTypes"),
              PP.brackets(PP.flow-map(PP.commabreak, _.tosource(), self.defined-types))),
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
  | s-contract(l :: Loc, name :: Name, params :: List<Name>, ann :: Ann) with:
    method label(self): "s-contract" end,
    method tosource(self):
      typarams =
        if is-empty(self.params): PP.mt-doc
        else: PP.surround-separate(INDENT, 0, PP.mt-doc, PP.langle, PP.commabreak, PP.rangle,
            self.params.map(_.tosource()))
        end
      PP.infix(INDENT, 1, str-coloncolon, self.name.tosource(), typarams + self.ann.tosource())
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
        + break-one + PP.group(str-elsebranch + PP.nest(INDENT, break-one + self._else.tosource()))
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
  | s-prim-app(l :: Loc, _fun :: String, args :: List<Expr>, app-info :: PrimAppInfo) with:
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
  # A fully-resolved reference to a module
  | s-id-modref(l :: Loc, id :: Name, uri :: String, name :: String) with:
    method label(self): "s-id-modref" end,
    method tosource(self): self.id.tosource() + PP.str("@") + PP.parens(PP.str(self.uri)) + PP.str("." + self.name) end
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
    method tosource(self): PP.infix-break(INDENT, 0, PP.mt-doc, self.obj.tosource(),
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
      blocky
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
              + PP.str(torepr(name)) + str-colon,
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
    self._match(visitor, lam(val): raise("No visitor field for " + self.label()) end)
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
  | s-spy-expr(l :: Loc, name :: String, value :: Expr, implicit-label :: Boolean) with:
    # implicit-label is true for the shorthand form (`spy: x end`), and false for
    # the longer form (`spy: some-name: x end`)
    method label(self): "s-spy-expr" end,
    method tosource(self):
      if self.implicit-label: self.value.tosource()
      else: PP.nest(INDENT, PP.str(self.name) + str-colonspace + self.value.tosource())
      end
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
      params :: List<Name>,
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
    method tosource(self): PP.mt-doc end
end

data CasesBind:
  | s-cases-bind(l :: Loc, field-type :: CasesBindType, bind :: Bind) with:
    method label(self): "s-cases-bind" end,
    method tosource(self):
      ft = self.field-type.tosource()
      if PP.is-mt-doc(ft): self.bind.tosource()
      else: ft + PP.str(" ") + self.bind.tosource()
      end
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
        PP.nest(INDENT, self.body.tosource()))
    end
  | s-singleton-cases-branch(l :: Loc, pat-loc :: Loc, name :: String, body :: Expr) with:
    method label(self): "s-singleton-cases-branch" end,
    method tosource(self):
      PP.nest(INDENT,
        PP.group(PP.str("| " + self.name) + break-one + str-thickarrow) + break-one
          + PP.nest(INDENT, self.body.tosource()))
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
    | s-program(_, _, _, _, _, b) => block-ids(b)
    | else => raise("Non-program given to toplevel-ids")
  end
end
    
default-map-visitor = {
  method option(self, opt):
    cases(Option) opt:
      | none => none
      | some(v) => some(v.visit(self))
    end
  end,

  method s-underscore(self, l):
    s-underscore(l)
  end,

  method s-name(self, l, s):
    s-name(l, s)
  end,

  method s-type-global(self, s):
    s-type-global(s)
  end,

  method s-module-global(self, s):
    s-module-global(s)
  end,

  method s-global(self, s):
    s-global(s)
  end,

  method s-atom(self, base, serial):
    s-atom(base, serial)
  end,

  method s-star(self, l, hidden):
    s-star(l, hidden.map(_.visit(self)))
  end,
  method s-module-ref(self, l, path, as-name):
    s-module-ref(l, path.map(_.visit(self)), self.option(as-name))
  end,

  method s-defined-module(self, name, val, uri):
    s-defined-module(name, val.visit(self), uri)
  end,
  method s-defined-value(self, name, val):
    s-defined-value(name, val.visit(self))
  end,
  method s-defined-var(self, name, id):
    s-defined-var(name, id.visit(self))
  end,
  method s-defined-type(self, name, typ):
    s-defined-type(name, typ.visit(self))
  end,

  method s-module(self, l, answer, dm, dv, dt, checks):
    s-module(l, answer.visit(self), dm.map(_.visit(self)), dv.map(_.visit(self)), dt.map(_.visit(self)), checks.visit(self))
  end,

  method s-program(self, l, _provide, provided-types, provides, imports, body):
    s-program(l, _provide.visit(self), provided-types.visit(self), provides.map(_.visit(self)), imports.map(_.visit(self)), body.visit(self))
  end,

  method s-include-from(self, l, mod, specs):
    s-include-from(l, mod.map(_.visit(self)), specs.map(_.visit(self)))
  end,
  method s-include-name(self, l, name-spec):
    s-include-name(l, name-spec.visit(self))
  end,
  method s-include-data(self, l, name-spec, hidden):
    s-include-data(l, name-spec.visit(self), hidden.map(_.visit(self)))
  end,
  method s-include-type(self, l, name-spec):
    s-include-type(l, name-spec.visit(self))
  end,
  method s-include-module(self, l, name-spec):
    s-include-module(l, name-spec.visit(self))
  end,

  method s-include(self, l, import-type):
    s-include(l, import-type.visit(self))
  end,
  method s-import(self, l, import-type, name):
    s-import(l, import-type.visit(self), name.visit(self))
  end,
  method s-const-import(self, l, mod):
    s-const-import(l, mod)
  end,
  method s-special-import(self, l, kind, args):
    s-special-import(l, kind, args)
  end,
  method s-import-types(self, l, import-type, name, types):
    s-import-types(l, import-type, name.visit(self), types.visit(self))
  end,
  method s-import-fields(self, l, fields, import-type):
    s-import-fields(l, fields.map(_.visit(self)), import-type)
  end,
  method s-provide(self, l, expr):
    s-provide(l, expr.visit(self))
  end,
  method s-provide-all(self, l):
    s-provide-all(l)
  end,
  method s-provide-none(self, l):
    s-provide-none(l)
  end,
  method s-provide-types(self, l, anns):
    s-provide-types(l, anns.map(_.visit(self)))
  end,
  method s-provide-types-all(self, l):
    s-provide-types-all(l)
  end,
  method s-provide-types-none(self, l):
    s-provide-types-none(l)
  end,
  method s-provide-block(self, l, specs):
    s-provide-block(l, specs.map(_.visit(self)))
  end,
  method s-provide-name(self, l, name-spec):
    s-provide-name(l, name-spec.visit(self))
  end,
  method s-provide-data(self, l, name-spec, hidden):
    s-provide-data(l, name-spec.visit(self), hidden.map(_.visit(self)))
  end,
  method s-provide-type(self, l, name-spec):
    s-provide-type(l, name-spec.visit(self))
  end,
  method s-provide-module(self, l, name-spec):
    s-provide-module(l, name-spec.visit(self))
  end,


  method s-bind(self, l, shadows, name, ann):
    s-bind(l, shadows, name.visit(self), ann.visit(self))
  end,

  method s-tuple-bind(self, l, fields, as-name):
    s-tuple-bind(l, fields.map(_.visit(self)), self.option(as-name))
  end,

  method s-var-bind(self, l, bind, expr):
    s-var-bind(l, bind.visit(self), expr.visit(self))
  end,
  method s-let-bind(self, l, bind, expr):
    s-let-bind(l, bind.visit(self), expr.visit(self))
  end,

  method s-type-bind(self, l, name, params, ann):
    s-type-bind(l, name.visit(self), params.map(_.visit(self)), ann.visit(self))
  end,

  method s-newtype-bind(self, l, name, namet):
    s-newtype-bind(l, name.visit(self), namet.visit(self))
  end,

  method s-type-let-expr(self, l, binds, body, blocky):
    s-type-let-expr(l, binds.map(_.visit(self)), body.visit(self), blocky)
  end,

  method s-template(self, l):
    s-template(l)
  end,

  method s-let-expr(self, l, binds, body, blocky):
    s-let-expr(l, binds.map(_.visit(self)), body.visit(self), blocky)
  end,

  method s-letrec-bind(self, l, bind, expr):
    s-letrec-bind(l, bind.visit(self), expr.visit(self))
  end,

  method s-letrec(self, l, binds, body, blocky):
    s-letrec(l, binds.map(_.visit(self)), body.visit(self), blocky)
  end,

  method s-hint-exp(self, l :: Loc, hints :: List<Hint>, exp :: Expr):
    s-hint-exp(l, hints, exp.visit(self))
  end,

  method s-instantiate(self, l :: Loc, expr :: Expr, params :: List<Ann>):
    s-instantiate(l, expr.visit(self), params.map(_.visit(self)))
  end,

  method s-block(self, l, stmts):
    s-block(l, stmts.map(_.visit(self)))
  end,

  method s-user-block(self, l :: Loc, body :: Expr):
    s-user-block(l, body.visit(self))
  end,

  method s-fun(self, l, name, params, args, ann, doc, body, _check-loc, _check, blocky):
    s-fun(l, name, params, args.map(_.visit(self)), ann.visit(self), doc, body.visit(self), _check-loc, self.option(_check), blocky)
  end,

  method s-type(self, l :: Loc, name :: Name, params :: List<Name>, ann :: Ann):
    s-type(l, name.visit(self), params.map(_.visit(self)), ann.visit(self))
  end,

  method s-newtype(self, l :: Loc, name :: Name, namet :: Name):
    s-newtype(l, name.visit(self), namet.visit(self))
  end,

  method s-var(self, l :: Loc, name :: Bind, value :: Expr):
    s-var(l, name.visit(self), value.visit(self))
  end,

  method s-rec(self, l :: Loc, name :: Bind, value :: Expr):
    s-rec(l, name.visit(self), value.visit(self))
  end,

  method s-let(self, l :: Loc, name :: Bind, value :: Expr, keyword-val :: Boolean):
    s-let(l, name.visit(self), value.visit(self), keyword-val)
  end,

  method s-ref(self, l :: Loc, ann :: Option<Ann>):
    s-ref(l, self.option(ann))
  end,

  method s-when(self, l :: Loc, test :: Expr, block :: Expr, blocky :: Boolean):
    s-when(l, test.visit(self), block.visit(self), blocky)
  end,

  method s-contract(self, l, name, params, ann):
    s-contract(l, name.visit(self), params.map(_.visit(self)), ann.visit(self))
  end,

  method s-assign(self, l :: Loc, id :: Name, value :: Expr):
    s-assign(l, id.visit(self), value.visit(self))
  end,

  method s-if-branch(self, l :: Loc, test :: Expr, body :: Expr):
    s-if-branch(l, test.visit(self), body.visit(self))
  end,

  method s-if-pipe-branch(self, l :: Loc, test :: Expr, body :: Expr):
    s-if-pipe-branch(l, test.visit(self), body.visit(self))
  end,

  method s-if(self, l :: Loc, branches :: List<IfBranch>, blocky :: Boolean):
    s-if(l, branches.map(_.visit(self)), blocky)
  end,
  method s-if-else(self, l :: Loc, branches :: List<IfBranch>, _else :: Expr, blocky :: Boolean):
    s-if-else(l, branches.map(_.visit(self)), _else.visit(self), blocky)
  end,

  method s-if-pipe(self, l :: Loc, branches :: List<IfPipeBranch>, blocky :: Boolean):
    s-if-pipe(l, branches.map(_.visit(self)), blocky)
  end,
  method s-if-pipe-else(self, l :: Loc, branches :: List<IfPipeBranch>, _else :: Expr, blocky :: Boolean):
    s-if-pipe-else(l, branches.map(_.visit(self)), _else.visit(self), blocky)
  end,

  method s-cases-bind(self, l :: Loc, typ :: CasesBindType, bind :: Bind):
    s-cases-bind(l, typ, bind.visit(self))
  end,
  method s-cases-branch(self, l :: Loc, pat-loc :: Loc, name :: String, args :: List<CasesBind>, body :: Expr):
    s-cases-branch(l, pat-loc, name, args.map(_.visit(self)), body.visit(self))
  end,

  method s-singleton-cases-branch(self, l :: Loc, pat-loc :: Loc, name :: String, body :: Expr):
    s-singleton-cases-branch(l, pat-loc, name, body.visit(self))
  end,

  method s-cases(self, l :: Loc, typ :: Ann, val :: Expr, branches :: List<CasesBranch>, blocky :: Boolean):
    s-cases(l, typ.visit(self), val.visit(self), branches.map(_.visit(self)), blocky)
  end,
  method s-cases-else(self, l :: Loc, typ :: Ann, val :: Expr, branches :: List<CasesBranch>, _else :: Expr, blocky :: Boolean):
    s-cases-else(l, typ.visit(self), val.visit(self), branches.map(_.visit(self)), _else.visit(self), blocky)
  end,

  method s-op(self, l :: Loc, op-l :: Loc, op :: String, left :: Expr, right :: Expr):
    s-op(l, op-l, op, left.visit(self), right.visit(self))
  end,

  method s-check-test(self, l :: Loc, op :: CheckOp, refinement :: Option<Expr>, left :: Expr, right :: Option<Expr>):
    s-check-test(l, op, self.option(refinement), left.visit(self), self.option(right))
  end,

  method s-check-expr(self, l :: Loc, expr :: Expr, ann :: Ann):
    s-check-expr(l, expr.visit(self), ann.visit(self))
  end,

  method s-paren(self, l :: Loc, expr :: Expr):
    s-paren(l, expr.visit(self))
  end,

  method s-lam(
      self,
      l :: Loc,
      name :: String,
      params :: List<Name>,
      args :: List<Bind>,
      ann :: Ann,
      doc :: String,
      body :: Expr,
      _check-loc :: Option<Loc>,
      _check :: Option<Expr>,
      blocky :: Boolean
    ):
    s-lam(l, name, params.map(_.visit(self)), args.map(_.visit(self)), ann.visit(self), doc, body.visit(self), _check-loc, self.option(_check), blocky)
  end,
  method s-method(
      self,
      l :: Loc,
      name :: String,
      params :: List<Name>,
      args :: List<Bind>, # Value parameters
      ann :: Ann, # return type
      doc :: String,
      body :: Expr,
      _check-loc :: Option<Loc>,
      _check :: Option<Expr>,
      blocky :: Boolean
    ):
    s-method(l, name, params.map(_.visit(self)), args.map(_.visit(self)), ann.visit(self), doc, body.visit(self), _check-loc, self.option(_check), blocky)
  end,
  method s-extend(self, l :: Loc, supe :: Expr, fields :: List<Member>):
    s-extend(l, supe.visit(self), fields.map(_.visit(self)))
  end,
  method s-update(self, l :: Loc, supe :: Expr, fields :: List<Member>):
    s-update(l, supe.visit(self), fields.map(_.visit(self)))
  end,
  method s-tuple(self, l :: Loc, fields :: List<Expr>):
    s-tuple(l, fields.map(_.visit(self)))
  end,
  method s-tuple-get(self, l :: Loc, tup :: Expr, index :: Number, index-loc :: Loc):
    s-tuple-get(l, tup.visit(self), index, index-loc)
  end,
  method s-obj(self, l :: Loc, fields :: List<Member>):
    s-obj(l, fields.map(_.visit(self)))
  end,
  method s-array(self, l :: Loc, values :: List<Expr>):
    s-array(l, values.map(_.visit(self)))
  end,
  method s-construct(self, l :: Loc, mod :: ConstructModifier, constructor :: Expr, values :: List<Expr>):
    s-construct(l, mod, constructor.visit(self), values.map(_.visit(self)))
  end,
  method s-reactor(self, l :: Loc, fields :: List<Member>):
    s-reactor(l, fields.map(_.visit(self)))
  end,
  method s-table(self, l :: Loc, headers :: List<FieldName>, rows :: List<TableRow>):
    s-table(l, headers.map(_.visit(self)), rows.map(_.visit(self)))
  end,
  method s-table-row(self, l :: Loc, elems :: List<Expr>):
    s-table-row(l, elems.map(_.visit(self)))
  end,
  method s-load-table(self, l, headers :: List<FieldName>, spec :: List<LoadTableSpec>):
    s-load-table(l, headers.map(_.visit(self)), spec.map(_.visit(self)))
  end,
  method s-field-name(self, l :: Loc, name :: String, ann :: Ann):
    s-field-name(l, name, ann.visit(self))
  end,
  method s-app(self, l :: Loc, _fun :: Expr, args :: List<Expr>):
    s-app(l, _fun.visit(self), args.map(_.visit(self)))
  end,
  method s-app-enriched(self, l :: Loc, _fun :: Expr, args :: List<Expr>, app-info :: AppInfo):
    s-app-enriched(l, _fun.visit(self), args.map(_.visit(self)), app-info)
  end,
  method s-prim-app(self, l :: Loc, _fun :: String, args :: List<Expr>, app-info :: PrimAppInfo):
    s-prim-app(l, _fun, args.map(_.visit(self)), app-info)
  end,
  method s-prim-val(self, l :: Loc, name :: String):
    s-prim-val(l, name)
  end,
  method s-id(self, l :: Loc, id :: Name):
    s-id(l, id.visit(self))
  end,
  method s-id-var(self, l :: Loc, id :: Name):
    s-id-var(l, id.visit(self))
  end,
  method s-id-letrec(self, l :: Loc, id :: Name, safe :: Boolean):
    s-id-letrec(l, id.visit(self), safe)
  end,
  method s-id-modref(self, l :: Loc, id :: Name, uri :: String, name :: String):
    s-id-modref(l, id.visit(self), uri, name)
  end,
  method s-undefined(self, l :: Loc):
    s-undefined(self)
  end,
  method s-srcloc(self, l, shadow loc):
    s-srcloc(l, loc)
  end,
  method s-num(self, l :: Loc, n :: Number):
    s-num(l, n)
  end,
  method s-frac(self, l :: Loc, num :: NumInteger, den :: NumInteger):
    s-frac(l, num, den)
  end,
  method s-rfrac(self, l :: Loc, num :: NumInteger, den :: NumInteger):
    s-rfrac(l, num, den)
  end,
  method s-bool(self, l :: Loc, b :: Boolean):
    s-bool(l, b)
  end,
  method s-str(self, l :: Loc, s :: String):
    s-str(l, s)
  end,
  method s-dot(self, l :: Loc, obj :: Expr, field :: String):
    s-dot(l, obj.visit(self), field)
  end,
  method s-get-bang(self, l :: Loc, obj :: Expr, field :: String):
    s-get-bang(l, obj.visit(self), field)
  end,
  method s-bracket(self, l :: Loc, obj :: Expr, key :: Expr):
    s-bracket(l, obj.visit(self), key.visit(self))
  end,
  method s-data(
      self,
      l :: Loc,
      name :: String,
      params :: List<Name>, # type params
      mixins :: List<Expr>,
      variants :: List<Variant>,
      shared-members :: List<Member>,
      _check-loc :: Option<Loc>,
      _check :: Option<Expr>
    ):
    s-data(
        l,
        name,
        params.map(_.visit(self)),
        mixins.map(_.visit(self)),
        variants.map(_.visit(self)),
        shared-members.map(_.visit(self)),
        _check-loc,
        self.option(_check)
      )
  end,
  method s-data-expr(
      self,
      l :: Loc,
      name :: String,
      namet :: Name,
      params :: List<Name>, # type params
      mixins :: List<Expr>,
      variants :: List<Variant>,
      shared-members :: List<Member>,
      _check-loc :: Option<Loc>,
      _check :: Option<Expr>
    ):
    s-data-expr(
        l,
        name,
        namet.visit(self),
        params.map(_.visit(self)),
        mixins.map(_.visit(self)),
        variants.map(_.visit(self)),
        shared-members.map(_.visit(self)),
        _check-loc,
        self.option(_check)
      )
  end,
  method s-for(
      self,
      l :: Loc,
      iterator :: Expr,
      bindings :: List<ForBind>,
      ann :: Ann,
      body :: Expr,
      blocky :: Boolean
    ):
    s-for(l, iterator.visit(self), bindings.map(_.visit(self)), ann.visit(self), body.visit(self), blocky)
  end,
  method s-check(self, l :: Loc, name :: Option<String>, body :: Expr, keyword-check :: Boolean):
    s-check(l, name, body.visit(self), keyword-check)
  end,

  method s-data-field(self, l :: Loc, name :: String, value :: Expr):
    s-data-field(l, name, value.visit(self))
  end,
  method s-mutable-field(self, l :: Loc, name :: String, ann :: Ann, value :: Expr):
    s-mutable-field(l, name, ann.visit(self), value.visit(self))
  end,
  method s-method-field(
      self,
      l :: Loc,
      name :: String,
      params :: List<Name>,
      args :: List<Bind>, # Value parameters
      ann :: Ann, # return type
      doc :: String,
      body :: Expr,
      _check-loc :: Option<Loc>,
      _check :: Option<Expr>,
      blocky :: Boolean
    ):
    s-method-field(
      l,
      name,
      params.map(_.visit(self)),
      args.map(_.visit(self)),
      ann.visit(self),
      doc,
      body.visit(self),
      _check-loc,
      self.option(_check),
      blocky
      )
  end,

  method s-for-bind(self, l :: Loc, bind :: Bind, value :: Expr):
    s-for-bind(l, bind.visit(self), value.visit(self))
  end,
  method s-column-binds(self, l :: Loc, binds :: List<Bind>, table :: Expr):
    s-column-binds(l, binds.map(_.visit(self)), table.visit(self))
  end,
  method s-variant-member(self, l :: Loc, member-type :: VariantMemberType, bind :: Bind):
    s-variant-member(l, member-type, bind.visit(self))
  end,
  method s-variant(
      self,
      l :: Loc,
      constr-loc :: Loc,
      name :: String,
      members :: List<VariantMember>,
      with-members :: List<Member>
    ):
    s-variant(l, constr-loc, name, members.map(_.visit(self)), with-members.map(_.visit(self)))
  end,
  method s-singleton-variant(
      self,
      l :: Loc,
      name :: String,
      with-members :: List<Member>
    ):
    s-singleton-variant(l, name, with-members.map(_.visit(self)))
  end,
  method s-column-sort(self, l, column :: Name, direction :: ColumnSortOrder):
    s-column-sort(l, column.visit(self), direction)
  end,
  method s-table-extend(self, l, column-binds :: ColumnBinds, extensions :: List<Member>):
    s-table-extend(l, column-binds.visit(self), extensions.map(_.visit(self)))
  end,
  method s-table-update(self, l, column-binds :: ColumnBinds, updates :: List<Member>):
    s-table-update(l, column-binds.visit(self), updates.map(_.visit(self)))
  end,
  method s-table-filter(self, l, column-binds :: ColumnBinds, predicate :: Expr):
    s-table-filter(l, column-binds.visit(self), predicate.visit(self))
  end,
  method s-table-select(self, l, columns :: List<Name>, table :: Expr):
    s-table-select(l, columns.map(_.visit(self)), table.visit(self))
  end,
  method s-table-order(self, l, table :: Expr, ordering :: List<ColumnSort>):
    s-table-order(l, table.visit(self), ordering.map(_.visit(self)))
  end,
  method s-table-extract(self, l, column :: Name, table :: Expr):
    s-table-extract(l, column.visit(self), table.visit(self))
  end,
  method s-table-extend-field(self, l, name :: String, value :: Expr, ann :: Ann):
    s-table-extend-field(l, name, value.visit(self), ann.visit(self))
  end,
  method s-table-extend-reducer(self, l, name :: String, reducer :: Expr, col :: Name, ann :: Ann):
    s-table-extend-reducer(l, name, reducer.visit(self),
      col.visit(self), ann.visit(self))
  end,
  method s-sanitize(self, l, name :: Name, sanitizer :: Expr):
    s-sanitize(l, name.visit(self), sanitizer.visit(self))
  end,
  method s-table-src(self, l, src :: Expr):
    s-table-src(l, src.visit(self))
  end,

  method s-spy-block(self, l :: Loc, message :: Option<Expr>, contents :: List<SpyField>):
    s-spy-block(l, self.option(message), contents.map(_.visit(self)))
  end,
  method s-spy-expr(self, l :: Loc, name :: String, value :: Expr, implicit-label :: Boolean):
    s-spy-expr(l, name, value.visit(self), implicit-label)
  end,

  method a-blank(self): a-blank end,
  method a-any(self, l): a-any(l) end,
  method a-name(self, l, id): a-name(l, id.visit(self)) end,
  method a-type-var(self, l, id): a-type-var(l, id.visit(self)) end,
  method a-arrow(self, l, args, ret, use-parens):
    a-arrow(l, args.map(_.visit(self)), ret.visit(self), use-parens)
  end,
  method a-arrow-argnames(self, l, args, ret, use-parens):
    a-arrow-argnames(l, args.map(_.visit(self)), ret.visit(self), use-parens)
  end,
  method a-method(self, l, args, ret):
    a-method(l, args.map(_.visit(self)), ret.visit(self))
  end,
  method a-record(self, l, fields):
    a-record(l, fields.map(_.visit(self)))
  end,
  method a-tuple(self, l, fields):
    a-tuple(l, fields.map(_.visit(self)))
  end,
  method a-app(self, l, ann, args):
    a-app(l, ann.visit(self), args.map(_.visit(self)))
  end,
  method a-pred(self, l, ann, exp):
    a-pred(l, ann.visit(self), exp.visit(self))
  end,
  method a-dot(self, l, obj, field):
    a-dot(l, obj.visit(self), field)
  end,
  method a-field(self, l, name, ann):
    a-field(l, name, ann.visit(self))
  end
}


default-iter-visitor = {
  method option(self, opt):
    cases(Option) opt:
      | none => true
      | some(v) => v.visit(self)
    end
  end,

  method s-underscore(self, l):
    true
  end,
  method s-name(self, l, s):
    true
  end,
  method s-global(self, s):
    true
  end,
  method s-type-global(self, s):
    true
  end,
  method s-module-global(self, s):
    true
  end,
  method s-atom(self, base, serial):
    true
  end,

  method s-star(self, l, hidden):
    hidden.all(_.visit(self))
  end,
  method s-module-ref(self, l, path, as-name):
    path.all(_.visit(self)) and self.option(as-name)
  end,

  method s-defined-module(self, name, val, uri):
    val.visit(self)
  end,
  method s-defined-value(self, name, val):
    val.visit(self)
  end,
  method s-defined-var(self, name, id):
    id.visit(self)
  end,
  method s-defined-type(self, name, typ):
    typ.visit(self)
  end,

  method s-module(self, l, answer, dm, dv, dt, checks):
    answer.visit(self) and lists.all(_.visit(self), dm) and lists.all(_.visit(self), dv) and lists.all(_.visit(self), dt) and checks.visit(self)
  end,

  method s-program(self, l, _provide, provided-types, provides, imports, body):
    _provide.visit(self)
    and provided-types.visit(self)
    and lists.all(_.visit(self), provides)
    and lists.all(_.visit(self), imports)
    and body.visit(self)
  end,

  method s-import(self, l, import-type, name):
    import-type.visit(self) and name.visit(self)
  end,
  method s-include(self, l, import-type):
    import-type.visit(self)
  end,

  method s-include-from(self, l, mod, specs):
    mod.all(_.visit(self)) and specs.all(_.visit(self))
  end,
  method s-include-name(self, l, name-spec):
    name-spec.visit(self)
  end,
  method s-include-data(self, l, name-spec, hidden):
    name-spec.visit(self) and hidden.all(_.visit(self))
  end,
  method s-include-type(self, l, name-spec):
    name-spec.visit(self)
  end,
  method s-include-module(self, l, name-spec):
    name-spec.visit(self)
  end,

  method s-const-import(self, l, mod):
    true
  end,
  method s-special-import(self, l, kind, args):
    true
  end,
  method s-import-types(self, l, import-type, name, types):
    name.visit(self) and types.visit(self)
  end,
  method s-import-fields(self, l, fields, import-type):
    lists.all(_.visit(self), fields)
  end,
  method s-provide(self, l, expr):
    expr.visit(self)
  end,
  method s-provide-all(self, l):
    true
  end,
  method s-provide-none(self, l):
    true
  end,
  method s-provide-types(self, l, anns):
    lists.all(_.visit(self), anns)
  end,
  method s-provide-types-all(self, l):
    true
  end,
  method s-provide-types-none(self, l):
    true
  end,
  method s-provide-block(self, l, specs):
    specs.all(_.visit(self))
  end,

  method s-provide-name(self, l, name-spec):
    name-spec.visit(self)
  end,
  method s-provide-data(self, l, name-spec, hidden):
    name-spec.visit(self) and hidden.all(_.visit(self))
  end,
  method s-provide-type(self, l, name-spec):
    name-spec.visit(self)
  end,
  method s-provide-module(self, l, name-spec):
    name-spec.visit(self)
  end,

  method s-template(self, l):
    true
  end,

  method s-bind(self, l, shadows, name, ann):
    name.visit(self) and ann.visit(self)
  end,

  method s-tuple-bind(self, l, fields, as-name):
    lists.all(_.visit(self), fields) and self.option(as-name)
  end,

  method s-var-bind(self, l, bind, expr):
    bind.visit(self) and expr.visit(self)
  end,
  method s-let-bind(self, l, bind, expr):
    bind.visit(self) and expr.visit(self)
  end,

  method s-type-bind(self, l, name, params, ann):
    name.visit(self) and ann.visit(self) and lists.all(_.visit(self), params)
  end,

  method s-newtype-bind(self, l, name, namet):
    name.visit(self) and namet.visit(self)
  end,

  method s-type-let-expr(self, l, binds, body, blocky):
    lists.all(_.visit(self), binds) and body.visit(self)
  end,

  method s-let-expr(self, l, binds, body, blocky):
    lists.all(_.visit(self), binds) and body.visit(self)
  end,

  method s-letrec-bind(self, l, bind, expr):
    bind.visit(self) and expr.visit(self)
  end,

  method s-letrec(self, l, binds, body, blocky):
    lists.all(_.visit(self), binds) and body.visit(self)
  end,

  method s-hint-exp(self, l :: Loc, hints :: List<Hint>, exp :: Expr):
    exp.visit(self)
  end,

  method s-instantiate(self, l :: Loc, expr :: Expr, params :: List<Ann>):
    expr.visit(self) and lists.all(_.visit(self), params)
  end,

  method s-block(self, l, stmts):
    lists.all(_.visit(self), stmts)
  end,

  method s-user-block(self, l :: Loc, body :: Expr):
    body.visit(self)
  end,

  method s-fun(self, l, name, params, args, ann, doc, body, _check-loc, _check, blocky):
    lists.all(_.visit(self), params)
    and lists.all(_.visit(self), args) and ann.visit(self) and body.visit(self) and self.option(_check)
  end,

  method s-type(self, l :: Loc, name :: Name, params :: List<Name>, ann :: Ann):
    name.visit(self) and ann.visit(self) and lists.all(_.visit(self), params)
  end,

  method s-newtype(self, l :: Loc, name :: Name, namet :: Name):
    name.visit(self) and namet.visit(self)
  end,

  method s-var(self, l :: Loc, name :: Bind, value :: Expr):
    name.visit(self) and value.visit(self)
  end,

  method s-rec(self, l :: Loc, name :: Bind, value :: Expr):
    name.visit(self) and value.visit(self)
  end,

  method s-let(self, l :: Loc, name :: Bind, value :: Expr, keyword-val :: Boolean):
    name.visit(self) and value.visit(self)
  end,

  method s-ref(self, l :: Loc, ann :: Option<Ann>):
    self.option(ann)
  end,

  method s-when(self, l :: Loc, test :: Expr, block :: Expr, blocky :: Boolean):
    test.visit(self) and block.visit(self)
  end,

  method s-contract(self, l :: Loc, name :: Name, params :: List<Name>, ann :: Ann):
    name.visit(self) and lists.all(_.visit(self), params) and ann.visit(self)
  end,

  method s-assign(self, l :: Loc, id :: Name, value :: Expr):
    id.visit(self) and value.visit(self)
  end,

  method s-if-branch(self, l :: Loc, test :: Expr, body :: Expr):
    test.visit(self) and body.visit(self)
  end,

  method s-if-pipe-branch(self, l :: Loc, test :: Expr, body :: Expr):
    test.visit(self) and body.visit(self)
  end,

  method s-if(self, l :: Loc, branches :: List<IfBranch>, blocky :: Boolean):
    lists.all(_.visit(self), branches)
  end,
  method s-if-else(self, l :: Loc, branches :: List<IfBranch>, _else :: Expr, blocky :: Boolean):
    lists.all(_.visit(self), branches) and _else.visit(self)
  end,

  method s-if-pipe(self, l :: Loc, branches :: List<IfPipeBranch>, blocky :: Boolean):
    lists.all(_.visit(self), branches)
  end,
  method s-if-pipe-else(self, l :: Loc, branches :: List<IfPipeBranch>, _else :: Expr, blocky :: Boolean):
    lists.all(_.visit(self), branches) and _else.visit(self)
  end,

  method s-cases-bind(self, l :: Loc, typ :: CasesBindType, bind :: Bind):
    bind.visit(self)
  end,
  method s-cases-branch(self, l :: Loc, pat-loc :: Loc, name :: String, args :: List<CasesBind>, body :: Expr):
    lists.all(_.visit(self), args) and body.visit(self)
  end,

  method s-singleton-cases-branch(self, l :: Loc, pat-loc :: Loc, name :: String, body :: Expr):
    body.visit(self)
  end,

  method s-cases(self, l :: Loc, typ :: Ann, val :: Expr, branches :: List<CasesBranch>, blocky :: Boolean):
    typ.visit(self) and val.visit(self) and lists.all(_.visit(self), branches)
  end,
  method s-cases-else(self, l :: Loc, typ :: Ann, val :: Expr, branches :: List<CasesBranch>, _else :: Expr, blocky :: Boolean):
    typ.visit(self) and val.visit(self) and lists.all(_.visit(self), branches) and _else.visit(self)
  end,

  method s-op(self, l :: Loc, op-l :: Loc, op :: String, left :: Expr, right :: Expr):
    left.visit(self) and right.visit(self)
  end,

  method s-check-test(self, l :: Loc, op :: CheckOp, refinement :: Option<Expr>, left :: Expr, right :: Option<Expr>):
    self.option(refinement) and left.visit(self) and self.option(right)
  end,

  method s-check-expr(self, l :: Loc, expr :: Expr, ann :: Ann):
    expr.visit(self) and ann.visit(self)
  end,

  method s-paren(self, l :: Loc, expr :: Expr):
    expr.visit(self)
  end,

  method s-lam(
      self,
      l :: Loc,
      name :: String,
      params :: List<Name>,
      args :: List<Bind>,
      ann :: Ann,
      doc :: String,
      body :: Expr,
      _check-loc :: Option<Loc>,
      _check :: Option<Expr>,
      blocky :: Boolean
      ):
    lists.all(_.visit(self), params)
    and lists.all(_.visit(self), args) and ann.visit(self) and body.visit(self) and self.option(_check)
  end,
  method s-method(
      self,
      l :: Loc,
      name :: String,
      params :: List<Name>,
      args :: List<Bind>, # Value parameters
      ann :: Ann, # return type
      doc :: String,
      body :: Expr,
      _check-loc :: Option<Loc>,
      _check :: Option<Expr>,
      blocky :: Boolean
      ):
    lists.all(_.visit(self), params) and lists.all(_.visit(self), args) and ann.visit(self) and body.visit(self) and self.option(_check)
  end,
  method s-extend(self, l :: Loc, supe :: Expr, fields :: List<Member>):
    supe.visit(self) and lists.all(_.visit(self), fields)
  end,
  method s-update(self, l :: Loc, supe :: Expr, fields :: List<Member>):
    supe.visit(self) and lists.all(_.visit(self), fields)
  end,
  method s-tuple(self, l :: Loc, fields :: List<Expr>):
    lists.all(_.visit(self), fields)
  end,
  method s-tuple-get(self, l :: Loc, tup :: Expr, index :: Number, index-loc :: Loc):
    tup.visit(self)
  end,
  method s-obj(self, l :: Loc, fields :: List<Member>):
    lists.all(_.visit(self), fields)
  end,
  method s-array(self, l :: Loc, values :: List<Expr>):
    lists.all(_.visit(self), values)
  end,
  method s-construct(self, l :: Loc, mod :: ConstructModifier, constructor :: Expr, values :: List<Expr>):
    constructor.visit(self) and lists.all(_.visit(self), values)
  end,
  method s-reactor(self, l :: Loc, fields :: List<Member>):
    lists.all(_.visit(self), fields)
  end,
  method s-table(self, l :: Loc, headers :: List<FieldName>, rows :: List<TableRow>):
    lists.all(_.visit(self), headers) and lists.all(_.visit(self), rows)
  end,
  method s-table-row(self, l :: Loc, elems :: List<Expr>):
    lists.all(_.visit(self), elems)
  end,
  method s-load-table(self, l :: Loc, headers :: List<FieldName>, spec :: List<LoadTableSpec>):
    lists.all(_.visit(self), headers) and lists.all(_.visit(self), spec)
  end,
  method s-field-name(self, l :: Loc, name :: String, ann :: Ann):
    true
  end,
  method s-app(self, l :: Loc, _fun :: Expr, args :: List<Expr>):
    _fun.visit(self) and lists.all(_.visit(self), args)
  end,
  method s-prim-app(self, l :: Loc, _fun :: String, args :: List<Expr>, _):
    lists.all(_.visit(self), args)
  end,
  method s-prim-val(self, l :: Loc, name :: String):
    true
  end,
  method s-id(self, l :: Loc, id :: Name):
    id.visit(self)
  end,
  method s-id-var(self, l :: Loc, id :: Name):
    id.visit(self)
  end,
  method s-id-letrec(self, l :: Loc, id :: Name, safe :: Boolean):
    id.visit(self)
  end,
  method s-id-modref(self, l :: Loc, id :: Name, uri :: String, name :: String):
    id.visit(self)
  end,
  method s-undefined(self, l :: Loc):
    true
  end,
  method s-srcloc(self, l, shadow loc):
    true
  end,
  method s-num(self, l :: Loc, n :: Number):
    true
  end,
  method s-frac(self, l :: Loc, num :: NumInteger, den :: NumInteger):
    true
  end,
  method s-rfrac(self, l :: Loc, num :: NumInteger, den :: NumInteger):
    true
  end,
  method s-bool(self, l :: Loc, b :: Boolean):
    true
  end,
  method s-str(self, l :: Loc, s :: String):
    true
  end,
  method s-dot(self, l :: Loc, obj :: Expr, field :: String):
    obj.visit(self)
  end,
  method s-get-bang(self, l :: Loc, obj :: Expr, field :: String):
    obj.visit(self)
  end,
  method s-bracket(self, l :: Loc, obj :: Expr, key :: Expr):
    obj.visit(self) and key.visit(self)
  end,
  method s-data(
      self,
      l :: Loc,
      name :: String,
      params :: List<Name>, # type params
      mixins :: List<Expr>,
      variants :: List<Variant>,
      shared-members :: List<Member>,
      _check-loc :: Option<Loc>,
      _check :: Option<Expr>
      ):
    lists.all(_.visit(self), params)
    and lists.all(_.visit(self), mixins)
    and lists.all(_.visit(self), variants)
    and lists.all(_.visit(self), shared-members)
    and self.option(_check)
  end,
  method s-data-expr(
      self,
      l :: Loc,
      name :: String,
      namet :: Name,
      params :: List<Name>, # type params
      mixins :: List<Expr>,
      variants :: List<Variant>,
      shared-members :: List<Member>,
      _check-loc :: Option<Loc>,
      _check :: Option<Expr>
      ):
    namet.visit(self)
    and lists.all(_.visit(self), params)
    and lists.all(_.visit(self), mixins)
    and lists.all(_.visit(self), variants)
    and lists.all(_.visit(self), shared-members)
    and self.option(_check)
  end,
  method s-for(
      self,
      l :: Loc,
      iterator :: Expr,
      bindings :: List<ForBind>,
      ann :: Ann,
      body :: Expr,
      blocky :: Boolean
      ):
    iterator.visit(self) and lists.all(_.visit(self), bindings) and ann.visit(self) and body.visit(self)
  end,
  method s-check(self, l :: Loc, name :: Option<String>, body :: Expr, keyword-check :: Boolean):
    body.visit(self)
  end,

  method s-data-field(self, l :: Loc, name :: String, value :: Expr):
    value.visit(self)
  end,
  method s-mutable-field(self, l :: Loc, name :: String, ann :: Ann, value :: Expr):
    ann.visit(self) and value.visit(self)
  end,
  method s-method-field(
      self,
      l :: Loc,
      name :: String,
      params :: List<Name>,
      args :: List<Bind>, # Value parameters
      ann :: Ann, # return type
      doc :: String,
      body :: Expr,
      _check-loc :: Option<Loc>,
      _check :: Option<Expr>,
      blocky :: Boolean
      ):
    lists.all(_.visit(self), args)
    and lists.all(_.visit(self), args)
    and ann.visit(self)
    and body.visit(self)
    and self.option(_check)
  end,

  method s-for-bind(self, l :: Loc, bind :: Bind, value :: Expr):
    bind.visit(self) and value.visit(self)
  end,
  method s-column-binds(self, l :: Loc, binds :: List<Bind>, table :: Expr):
    binds.all(_.visit(self)) and table.visit(self)
  end,
  method s-variant-member(self, l :: Loc, member-type :: VariantMemberType, bind :: Bind):
    bind.visit(self)
  end,
  method s-variant(
      self,
      l :: Loc,
      constr-loc :: Loc,
      name :: String,
      members :: List<VariantMember>,
      with-members :: List<Member>
      ):
    lists.all(_.visit(self), members) and lists.all(_.visit(self), with-members)
  end,
  method s-singleton-variant(
      self,
      l :: Loc,
      name :: String,
      with-members :: List<Member>
      ):
    lists.all(_.visit(self), with-members)
  end,
  method s-column-sort(self, l, column :: Name, direction :: ColumnSortOrder):
    column.visit(self)
  end,
  method s-table-extend(self, l, column-binds :: ColumnBinds, extensions :: List<Member>):
    column-binds.visit(self) and extensions.all(_.visit(self))
  end,
  method s-table-update(self, l, column-binds :: ColumnBinds, updates :: List<Member>):
    column-binds.visit(self) and updates.all(_.visit(self))
  end,
  method s-table-filter(self, l, column-binds :: ColumnBinds, predicate :: Expr):
    column-binds.visit(self) and predicate.visit(self)
  end,
  method s-table-select(self, l, columns :: List<Name>, table :: Expr):
    columns.all(_.visit(self)) and table.visit(self)
  end,
  method s-table-order(self, l, table :: Expr, ordering :: List<ColumnSort>):
    table.visit(self) and ordering.all(_.visit(self))
  end,
  method s-table-extract(self, l, column :: Name, table :: Expr):
    column.visit(self) and table.visit(self)
  end,
  method s-table-extend-field(self, l, name :: String, value :: Expr, ann :: Ann):
    value.visit(self) and ann.visit(self)
  end,
  method s-table-extend-reducer(self, l, name :: String, reducer :: Expr, col :: Name, ann :: Ann):
    reducer.visit(self) and col.visit(self) and ann.visit(self)
  end,
  method s-sanitize(self, l, name, sanitizer):
    name.visit(self) and sanitizer.visit(self)
  end,
  method s-table-src(self, l, src):
    src.visit(self)
  end,

  method s-spy-block(self, l :: Loc, message :: Option<Expr>, contents :: List<SpyField>):
    self.option(message) and lists.all(_.visit(self), contents)
  end,
  method s-spy-expr(self, l :: Loc, name :: String, value :: Expr, implicit-label :: Boolean):
    value.visit(self)
  end,

  method a-blank(self):
    true
  end,
  method a-any(self, l):
    true
  end,
  method a-name(self, l, id):
    true
  end,
  method a-type-var(self, l, id):
    true
  end,
  method a-arrow(self, l, args, ret, _):
    lists.all(_.visit(self), args) and ret.visit(self)
  end,
  method a-arrow-argnames(self, l, args, ret, _):
    lists.all(_.visit(self), args) and ret.visit(self)
  end,
  method a-method(self, l, args, ret):
    lists.all(_.visit(self), args) and ret.visit(self)
  end,
  method a-record(self, l, fields):
    lists.all(_.visit(self), fields)
  end,
  method a-tuple(self, l, fields):
    lists.all(_.visit(self), fields)
  end,
  method a-app(self, l, ann, args):
    ann.visit(self) and lists.all(_.visit(self), args)
  end,
  method a-pred(self, l, ann, exp):
    ann.visit(self) and exp.visit(self)
  end,
  method a-dot(self, l, obj, field):
    obj.visit(self)
  end,
  method a-field(self, l, name, ann):
    ann.visit(self)
  end
}

dummy-loc-visitor = {
  method option(self, opt):
    cases(Option) opt:
      | none => none
      | some(v) => some(v.visit(self))
    end
  end,

  method s-underscore(self, l):
    s-underscore(dummy-loc)
  end,
  method s-name(self, l, s):
    s-name(dummy-loc, s)
  end,
  method s-global(self, s):
    s-global(s)
  end,
  method s-type-global(self, s):
    s-type-global(s)
  end,
  method s-module-global(self, s):
    s-module-global(s)
  end,
  method s-atom(self, base, serial):
    s-atom(base, serial)
  end,

  method s-star(self, _, hidden):
    s-star(dummy-loc, hidden.map(_.visit(self)))
  end,
  method s-module-ref(self, _, path, as-name):
    s-module-ref(dummy-loc, path.map(_.visit(self)), self.option(as-name))
  end,

  method s-defined-module(self, name, val, uri):
    s-defined-module(name, val.visit(self), uri)
  end,
  method s-defined-value(self, name, val):
    s-defined-value(name, val.visit(self))
  end,
  method s-defined-var(self, name, id):
    s-defined-var(name, id.visit(self))
  end,
  method s-defined-type(self, name, typ):
    s-defined-type(name, typ.visit(self))
  end,

  method s-module(self, l, answer, dm, dv, dt, checks):
    s-module(dummy-loc,
      answer.visit(self), dm.map(_.visit(self)), dv.map(_.visit(self)), dt.map(_.visit(self)), checks.visit(self))
  end,

  method s-program(self, l, _provide, provided-types, provides, imports, body):
    s-program(dummy-loc, _provide.visit(self), provided-types.visit(self), provides.map(_.visit(self)), imports.map(_.visit(self)), body.visit(self))
  end,

  method s-const-import(self, l :: Loc, mod :: String):
    s-const-import(dummy-loc, mod)
  end,
  method s-special-import(self, l, kind, args):
    s-special-import(dummy-loc, kind, args)
  end,
  method s-import(self, l, import-type, name):
    s-import(dummy-loc, import-type.visit(self), name.visit(self))
  end,
  method s-include-from(self, l, mod, specs):
    s-include-from(dummy-loc, mod.map(_.visit(self)), specs.map(_.visit(self)))
  end,
  method s-include-name(self, l, name-spec):
    s-include-name(dummy-loc, name-spec.visit(self))
  end,
  method s-include-data(self, l, name-spec, hidden):
    s-include-data(dummy-loc, name-spec.visit(self), hidden.map(_.visit(self)))
  end,
  method s-include-type(self, l, name-spec):
    s-include-type(dummy-loc, name-spec.visit(self))
  end,
  method s-include-module(self, l, name-spec):
    s-include-module(dummy-loc, name-spec.visit(self))
  end,

  method s-include(self, l, import-type):
    s-include(dummy-loc, import-type.visit(self))
  end,
  method s-import-types(self, l, import-type, name, types):
    s-import-types(dummy-loc, import-type.visit(self), name.visit(self), types.visit(self))
  end,
  method s-import-fields(self, l, fields, import-type):
    s-import-fields(dummy-loc, fields.map(_.visit(self)), import-type.visit(self))
  end,
  method s-provide(self, l, expr):
    s-provide(dummy-loc, expr.visit(self))
  end,
  method s-provide-all(self, l):
    s-provide-all(dummy-loc)
  end,
  method s-provide-none(self, l):
    s-provide-none(dummy-loc)
  end,
  method s-provide-types(self, l, anns):
    s-provide-types(dummy-loc, anns.map(_.visit(self)))
  end,
  method s-provide-types-all(self, l):
    s-provide-types-all(dummy-loc)
  end,
  method s-provide-types-none(self, l):
    s-provide-types-none(dummy-loc)
  end,

  method s-bind(self, l, shadows, name, ann):
    s-bind(dummy-loc, shadows, name.visit(self), ann.visit(self))
  end,

  method s-tuple-bind(self, l, fields, as-name):
    s-tuple-bind(dummy-loc, fields.map(_.visit(self)), self.option(as-name))
  end,

  method s-var-bind(self, l, bind, expr):
    s-var-bind(dummy-loc, bind.visit(self), expr.visit(self))
  end,
  method s-let-bind(self, l, bind, expr):
    s-let-bind(dummy-loc, bind.visit(self), expr.visit(self))
  end,

  method s-type-bind(self, l, name, params, ann):
    s-type-bind(dummy-loc, name, params, ann)
  end,

  method s-newtype-bind(self, l, name, namet):
    s-newtype-bind(l, name.visit(self), namet.visit(self))
  end,

  method s-template(self, l):
    s-template(dummy-loc)
  end,

  method s-type-let-expr(self, l, binds, body, blocky):
    s-type-let-expr(dummy-loc, binds.map(_.visit(self)), body.visit(self), blocky)
  end,

  method s-let-expr(self, l, binds, body, blocky):
    s-let-expr(dummy-loc, binds.map(_.visit(self)), body.visit(self), blocky)
  end,

  method s-letrec-bind(self, l, bind, expr):
    s-letrec-bind(dummy-loc, bind.visit(self), expr.visit(self))
  end,

  method s-letrec(self, l, binds, body, blocky):
    s-letrec(dummy-loc, binds.map(_.visit(self)), body.visit(self), blocky)
  end,

  method s-hint-exp(self, l :: Loc, hints :: List<Hint>, exp :: Expr):
    s-hint-exp(dummy-loc, hints, exp.visit(self))
  end,

  method s-instantiate(self, l :: Loc, expr :: Expr, params :: List<Ann>):
    s-instantiate(dummy-loc, expr.visit(self), params.map(_.visit(self)))
  end,

  method s-block(self, l, stmts):
    s-block(dummy-loc, stmts.map(_.visit(self)))
  end,

  method s-user-block(self, l :: Loc, body :: Expr):
    s-user-block(dummy-loc, body.visit(self))
  end,

  method s-fun(self, l, name, params, args, ann, doc, body, _check-loc, _check, blocky):
    s-fun(dummy-loc, name, params.map(_.visit(self)), args.map(_.visit(self)), ann.visit(self), doc, body.visit(self), if is-none(_check-loc): none else: some(dummy-loc) end, self.option(_check), blocky)
  end,

  method s-type(self, l :: Loc, name :: Name, params :: List<Name>, ann :: Ann):
    s-type(dummy-loc, name.visit(self), params.map(_.visit(self)), ann.visit(self))
  end,

  method s-newtype(self, l :: Loc, name :: Name, namet :: Name):
    s-newtype(dummy-loc, name.visit(self), namet.visit(self))
  end,

  method s-var(self, l :: Loc, name :: Bind, value :: Expr):
    s-var(dummy-loc, name.visit(self), value.visit(self))
  end,

  method s-rec(self, l :: Loc, name :: Bind, value :: Expr):
    s-rec(dummy-loc, name.visit(self), value.visit(self))
  end,

  method s-let(self, l :: Loc, name :: Bind, value :: Expr, keyword-val :: Boolean):
    s-let(dummy-loc, name.visit(self), value.visit(self), keyword-val)
  end,

  method s-ref(self, l :: Loc, ann :: Option<Ann>):
    s-ref(self, dummy-loc, self.option(ann))
  end,

  method s-when(self, l :: Loc, test :: Expr, block :: Expr, blocky :: Boolean):
    s-when(dummy-loc, test.visit(self), block.visit(self), blocky)
  end,

  method s-contract(self, l, name, params, ann):
    s-contract(dummy-loc, name.visit(self), params.map(_.visit(self)), ann.visit(self))
  end,

  method s-assign(self, l :: Loc, id :: Name, value :: Expr):
    s-assign(dummy-loc, id.visit(self), value.visit(self))
  end,

  method s-if-branch(self, l :: Loc, test :: Expr, body :: Expr):
    s-if-branch(dummy-loc, test.visit(self), body.visit(self))
  end,

  method s-if-pipe-branch(self, l :: Loc, test :: Expr, body :: Expr):
    s-if-pipe-branch(dummy-loc, test.visit(self), body.visit(self))
  end,

  method s-if(self, l :: Loc, branches :: List<IfBranch>, blocky :: Boolean):
    s-if(dummy-loc, branches.map(_.visit(self)), blocky)
  end,
  method s-if-else(self, l :: Loc, branches :: List<IfBranch>, _else :: Expr, blocky :: Boolean):
    s-if-else(dummy-loc, branches.map(_.visit(self)), _else.visit(self), blocky)
  end,

  method s-if-pipe(self, l :: Loc, branches :: List<IfPipeBranch>, blocky :: Boolean):
    s-if-pipe(dummy-loc, branches.map(_.visit(self)), blocky)
  end,
  method s-if-pipe-else(self, l :: Loc, branches :: List<IfPipeBranch>, _else :: Expr, blocky :: Boolean):
    s-if-pipe-else(dummy-loc, branches.map(_.visit(self)), _else.visit(self), blocky)
  end,

  method s-cases-bind(self, l :: Loc, typ :: CasesBindType, bind :: Bind):
    s-cases-bind(dummy-loc, l, typ, bind.visit(self))
  end,
  method s-cases-branch(self, l :: Loc, pat-loc :: Loc, name :: String, args :: List<CasesBind>, body :: Expr):
    s-cases-branch(dummy-loc, dummy-loc, name, args.map(_.visit(self)), body.visit(self))
  end,

  method s-singleton-cases-branch(self, l :: Loc, pat-loc :: Loc, name :: String, body :: Expr):
    s-singleton-cases-branch(dummy-loc, dummy-loc, name, body.visit(self))
  end,

  method s-cases(self, l :: Loc, typ :: Ann, val :: Expr, branches :: List<CasesBranch>, blocky :: Boolean):
    s-cases(dummy-loc, typ.visit(self), val.visit(self), branches.map(_.visit(self)), blocky)
  end,
  method s-cases-else(self, l :: Loc, typ :: Ann, val :: Expr, branches :: List<CasesBranch>, _else :: Expr, blocky :: Boolean):
    s-cases-else(dummy-loc, typ.visit(self), val.visit(self), branches.map(_.visit(self)), _else.visit(self), blocky)
  end,

  method s-op(self, l :: Loc, op-l :: Loc, op :: String, left :: Expr, right :: Expr):
    s-op(dummy-loc, dummy-loc, op, left.visit(self), right.visit(self))
  end,

  method s-check-test(self, l :: Loc, op :: CheckOp, refinement :: Option<Expr>, left :: Expr, right :: Option<Expr>):
    s-check-test(dummy-loc, op, self.option(refinement), left.visit(self), self.option(right))
  end,

  method s-paren(self, l :: Loc, expr :: Expr):
    s-paren(dummy-loc, expr.visit(self))
  end,

  method s-lam(
      self,
      l :: Loc,
      name :: String,
      params :: List<Name>,
      args :: List<Bind>,
      ann :: Ann,
      doc :: String,
      body :: Expr,
      _check-loc :: Option<Loc>,
      _check :: Option<Expr>,
      blocky :: Boolean
    ):
    s-lam(dummy-loc, "", params.map(_.visit(self)), args.map(_.visit(self)), ann.visit(self), doc, body.visit(self), if is-none(_check): none else: some(dummy-loc) end, self.option(_check), blocky)
  end,
  method s-method(
      self,
      l :: Loc,
      name :: String,
      params :: List<Name>,
      args :: List<Bind>, # Value parameters
      ann :: Ann, # return type
      doc :: String,
      body :: Expr,
      _check-loc :: Option<Loc>,
      _check :: Option<Expr>,
      blocky :: Boolean
    ):
    s-method(dummy-loc, "", params.map(_.visit(self)), args.map(_.visit(self)), ann.visit(self), doc, body.visit(self), if is-none(_check-loc): none else: some(dummy-loc) end, self.option(_check), blocky)
  end,
  method s-extend(self, l :: Loc, supe :: Expr, fields :: List<Member>):
    s-extend(dummy-loc, supe.visit(self), fields.map(_.visit(self)))
  end,
  method s-update(self, l :: Loc, supe :: Expr, fields :: List<Member>):
    s-update(dummy-loc, supe.visit(self), fields.map(_.visit(self)))
  end,
  method s-obj(self, l :: Loc, fields :: List<Member>):
    s-obj(dummy-loc, fields.map(_.visit(self)))
  end,
  method s-array(self, l :: Loc, values :: List<Expr>):
    s-array(dummy-loc, values.map(_.visit(self)))
  end,
  method s-construct(self, l :: Loc, mod :: ConstructModifier, constructor :: Expr, values :: List<Expr>):
    s-construct(dummy-loc, mod, constructor.visit(self), values.map(_.visit(self)))
  end,
  method s-reactor(self, l :: Loc, fields):
    s-reactor(dummy-loc, fields.map(_.visit(self)))
  end,
  method s-table(self, l :: Loc, headers :: List<FieldName>, rows :: List<TableRow>):
    s-table(dummy-loc, headers.map(_.visit(self)), rows.map(_.visit(self)))
  end,
  method s-table-row(self, l :: Loc, elems :: List<Expr>):
    s-table-row(dummy-loc, elems.map(_.visit(self)))
  end,
  method s-field-name(self, l :: Loc, name :: String, ann :: Ann):
    s-field-name(dummy-loc, name, ann.visit(self))
  end,
  method s-load-table(self, l, headers, spec :: List<LoadTableSpec>):
    s-load-table(dummy-loc, headers.map(_.visit(self)), spec.map(_.visit(self)))
  end,
  method s-app(self, l :: Loc, _fun :: Expr, args :: List<Expr>):
    s-app(dummy-loc, _fun.visit(self), args.map(_.visit(self)))
  end,
  method s-prim-app(self, l :: Loc, _fun :: String, args :: List<Expr>, app-info :: PrimAppInfo):
    s-prim-app(dummy-loc, _fun, args.map(_.visit(self)), app-info)
  end,
  method s-prim-val(self, l :: Loc, name :: String):
    s-prim-val(dummy-loc, name)
  end,
  method s-id(self, l :: Loc, id :: Name):
    s-id(dummy-loc, id.visit(self))
  end,
  method s-id-var(self, l :: Loc, id :: Name):
    s-id-var(dummy-loc, id.visit(self))
  end,
  method s-id-letrec(self, l :: Loc, id :: Name, safe :: Boolean):
    s-id-letrec(dummy-loc, id.visit(self), safe)
  end,
  method s-id-modref(self, l :: Loc, id :: Name, uri :: String, name :: String):
    s-id-modref(dummy-loc, id.visit(self), uri, name)
  end,
  method s-undefined(self, l :: Loc):
    s-undefined(self)
  end,
  method s-srcloc(self, l, shadow loc):
    s-srcloc(dummy-loc, loc)
  end,
  method s-num(self, l :: Loc, n :: Number):
    s-num(dummy-loc, n)
  end,
  method s-frac(self, l :: Loc, num :: NumInteger, den :: NumInteger):
    s-frac(dummy-loc, num, den)
  end,
  method s-rfrac(self, l :: Loc, num :: NumInteger, den :: NumInteger):
    s-rfrac(dummy-loc, num, den)
  end,
  method s-bool(self, l :: Loc, b :: Boolean):
    s-bool(dummy-loc, b)
  end,
  method s-str(self, l :: Loc, s :: String):
    s-str(dummy-loc, s)
  end,
  method s-dot(self, l :: Loc, obj :: Expr, field :: String):
    s-dot(dummy-loc, obj.visit(self), field)
  end,
  method s-get-bang(self, l :: Loc, obj :: Expr, field :: String):
    s-get-bang(dummy-loc, obj.visit(self), field)
  end,
  method s-bracket(self, l :: Loc, obj :: Expr, key :: Expr):
    s-bracket(dummy-loc, obj.visit(self), key.visit(self))
  end,
  method s-data(
      self,
      l :: Loc,
      name :: String,
      params :: List<Name>, # type params
      mixins :: List<Expr>,
      variants :: List<Variant>,
      shared-members :: List<Member>,
      _check-loc :: Option<Loc>,
      _check :: Option<Expr>
    ):
    s-data(
        dummy-loc,
        name,
        params.map(_.visit(self)),
        mixins.map(_.visit(self)),
        variants.map(_.visit(self)),
        shared-members.map(_.visit(self)),
        if is-none(_check-loc): none else: some(dummy-loc) end,
        self.option(_check)
      )
  end,
  method s-data-expr(
      self,
      l :: Loc,
      name :: String,
      namet :: String,
      params :: List<Name>, # type params
      mixins :: List<Expr>,
      variants :: List<Variant>,
      shared-members :: List<Member>,
      _check-loc :: Option<Loc>,
      _check :: Option<Expr>
    ):
    s-data-expr(
        dummy-loc,
        name,
        namet.visit(self),
        params.map(_.visit(self)),
        mixins.map(_.visit(self)),
        variants.map(_.visit(self)),
        shared-members.map(_.visit(self)),
        if is-none(_check-loc): none else: some(dummy-loc) end,
        self.option(_check)
      )
  end,
  method s-for(
      self,
      l :: Loc,
      iterator :: Expr,
      bindings :: List<ForBind>,
      ann :: Ann,
      body :: Expr,
      blocky :: Boolean
    ):
    s-for(dummy-loc, iterator.visit(self), bindings.map(_.visit(self)), ann.visit(self), body.visit(self), blocky)
  end,
  method s-check(self, l :: Loc, name :: Option<String>, body :: Expr, keyword-check :: Boolean):
    s-check(dummy-loc, name, body.visit(self), keyword-check)
  end,

  method s-data-field(self, l :: Loc, name :: String, value :: Expr):
    s-data-field(dummy-loc, name, value.visit(self))
  end,
  method s-mutable-field(self, l :: Loc, name :: String, ann :: Ann, value :: Expr):
    s-mutable-field(dummy-loc, name, ann.visit(self), value.visit(self))
  end,
  method s-method-field(
      self,
      l :: Loc,
      name :: String,
      params :: List<Name>,
      args :: List<Bind>, # Value parameters
      ann :: Ann, # return type
      doc :: String,
      body :: Expr,
      _check-loc :: Option<Loc>,
      _check :: Option<Expr>,
      blocky :: Boolean
    ):
    s-method-field(
      dummy-loc,
      name,
      params.map(_.visit(self)),
      args.map(_.visit(self)),
      ann.visit(self),
      doc,
      body.visit(self),
      if is-none(_check-loc): none else: some(dummy-loc) end,
      self.option(_check),
      blocky
      )
  end,

  method s-for-bind(self, l :: Loc, bind :: Bind, value :: Expr):
    s-for-bind(dummy-loc, bind.visit(self), value.visit(self))
  end,
  method s-column-binds(self, l :: Loc, binds :: List<Bind>, table :: Expr):
    s-column-binds(dummy-loc, binds.map(_.visit(self)), table.visit(self))
  end,
  method s-variant-member(self, l :: Loc, member-type :: VariantMemberType, bind :: Bind):
    s-variant-member(dummy-loc, member-type, bind.visit(self))
  end,
  method s-variant(
      self,
      l :: Loc,
      constr-loc :: Loc,
      name :: String,
      members :: List<VariantMember>,
      with-members :: List<Member>
    ):
    s-variant(dummy-loc, dummy-loc, name, members.map(_.visit(self)), with-members.map(_.visit(self)))
  end,
  method s-singleton-variant(
      self,
      l :: Loc,
      name :: String,
      with-members :: List<Member>
    ):
    s-singleton-variant(dummy-loc, name, with-members.map(_.visit(self)))
  end,
  method s-column-sort(self, l, column :: Name, direction :: ColumnSortOrder):
    s-column-sort(dummy-loc, column.visit(self), direction)
  end,
  method s-table-extend(self, l, column-binds :: ColumnBinds, extensions :: List<Member>):
    s-table-extend(dummy-loc, column-binds.visit(self), extensions.map(_.visit(self)))
  end,
  method s-table-update(self, l, column-binds :: ColumnBinds, updates :: List<Member>):
    s-table-update(dummy-loc, column-binds.visit(self), updates.map(_.visit(self)))
  end,
  method s-table-filter(self, l, column-binds :: ColumnBinds, predicate :: Expr):
    s-table-filter(dummy-loc, column-binds.visit(self), predicate.visit(self))
  end,
  method s-table-select(self, l, columns :: List<Name>, table :: Expr):
    s-table-select(dummy-loc, columns.map(_.visit(self)), table.visit(self))
  end,
  method s-table-order(self, l, table :: Expr, ordering :: List<ColumnSort>):
    s-table-order(dummy-loc, table.visit(self), ordering.map(_.visit(self)))
  end,
  method s-table-extract(self, l, column :: Name, table :: Expr):
    s-table-extract(dummy-loc, column.visit(self), table.visit(self))
  end,
  method s-table-extend-field(self, l, name :: String, value :: Expr, ann :: Ann):
    s-table-extend-field(dummy-loc, name.visit(self), value.visit(self), ann.visit(self))
  end,
  method s-table-extend-reducer(self, l, name :: String, reducer :: Expr, col :: Name, ann :: Ann):
    s-table-extend-reducer(dummy-loc, name.visit(self), reducer.visit(self),
      col.visit(self), ann.visit(self))
  end,
  method s-sanitize(self, l, name :: Name, sanitizer :: Expr):
    s-sanitize(dummy-loc, name.visit(self), sanitizer.visit(self))
  end,
  method s-table-src(self, l, src :: Expr):
    s-table-src(dummy-loc, src.visit(self))
  end,

  method s-spy-block(self, l :: Loc, message :: Option<Expr>, contents :: List<SpyField>):
    s-spy-block(dummy-loc, self.option(message), contents.map(_.visit(self)))
  end,
  method s-spy-expr(self, l :: Loc, name :: String, value :: Expr, implicit-label :: Boolean):
    s-spy-expr(dummy-loc, name, value.visit(self), implicit-label)
  end,

  method a-blank(self): a-blank end,
  method a-any(self, l): a-any(l) end,
  method a-name(self, l, id): a-name(dummy-loc, id.visit(self)) end,
  method a-type-var(self, l, id): a-type-var(dummy-loc, id.visit(self)) end,
  method a-arrow(self, l, args, ret, use-parens):
    a-arrow(dummy-loc, args.map(_.visit(self)), ret.visit(self), use-parens)
  end,
  method a-arrow-argnames(self, l, args, ret, use-parens):
    a-arrow-argnames(dummy-loc, args.map(_.visit(self)), ret.visit(self), use-parens)
  end,
  method a-method(self, l, args, ret):
    a-method(dummy-loc, args.map(_.visit(self)), ret.visit(self))
  end,
  method a-record(self, l, fields):
    a-record(dummy-loc, fields.map(_.visit(self)))
  end,
  method a-tuple(self, l, fields):
    a-tuple(dummy-loc, fields.map(_.visit(self)))
  end,
  method a-app(self, l, ann, args):
    a-app(dummy-loc, ann.visit(self), args.map(_.visit(self)))
  end,
  method a-pred(self, l, ann, exp):
    a-pred(dummy-loc, ann.visit(self), exp.visit(self))
  end,
  method a-dot(self, l, obj, field):
    a-dot(dummy-loc, obj, field)
  end,
  method a-field(self, l, name, ann):
    a-field(dummy-loc, name, ann.visit(self))
  end
}
