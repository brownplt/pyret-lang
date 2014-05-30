#lang pyret

provide *
import ast as A
import pprint as PP
import srcloc as SL

INDENT = 2

break-one = PP.sbreak(1)
str-method = PP.str(" method")
str-letrec = PP.str("letrec ")
str-period = PP.str(".")
str-bang = PP.str("!")
str-brackets = PP.str("[]")
str-colon = PP.str(":")
str-coloncolon = PP.str("::")
str-colonspace = PP.str(":")
str-end = PP.str("end")
str-type-let = PP.str("type-let ")
str-let = PP.str("let ")
str-var = PP.str("var ")
str-if = PP.str("if ")
str-elsecolon = PP.str("else:")
str-try = PP.str("try:")
str-except = PP.str("except")
str-spacecolonequal = PP.str(" :=")
str-spaceequal = PP.str(" =")
str-import = PP.str("import")
str-provide = PP.str("provide")
str-as = PP.str("as")
str-newtype = PP.str("newtype ")

dummy-loc = SL.builtin("dummy-location")

Loc = SL.Srcloc

data AProg:
  | a-program(l :: SL.Location, imports :: List<AImport>, body :: AExpr) with:
    label(self): "a-program" end,
    tosource(self):
      PP.group(
        PP.flow-map(PP.hardline, lam(i): i.tosource() end, self.imports)
          + PP.hardline
          + self.body.tosource()
        )
    end
sharing:
  visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end
end

data AImportType:
  | a-import-builtin(l :: SL.Location, lib :: String) with:
    tosource(self): PP.str(self.file) end
  | a-import-file(l :: SL.Location, file :: String) with:
    tosource(self): PP.dquote(PP.str(self.file)) end
end

data AImport:
  | a-import(l :: SL.Location, import-type :: AImportType, name :: A.Name) with:
    label(self): "a-import" end,
    tosource(self):
      PP.flow([list: str-import, self.import-type.tosource(), str-as, self.name.tosource()])
    end
  | a-import-types(l :: SL.Location, import-type :: AImportType, name :: A.Name, types :: A.Name) with:
    label(self): "a-import-types" end,
    tosource(self):
      PP.flow([list: str-import, self.import-type.tosource(), str-as, self.name.tosource(), PP.commabreak, self.types.tosource()])
    end
sharing:
  visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end
end

data ATypeBind:
  | a-type-bind(l :: SL.Location, name :: A.Name, ann :: A.Ann) with:
    label(self): "a-type-bind" end,
    tosource(self): PP.infix(INDENT, 1, str-coloncolon, self.name.tosource(), self.ann.tosource()) end
  | a-newtype-bind(l :: SL.Location, name :: A.Name, namet :: A.Name) with:
    label(self): "a-newtype-bind" end,
    tosource(self):
      PP.group(str-newtype + self.name.tosource() + break-one + str-as + break-one + self.namet.tosource())
    end
sharing:
  visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end
end

data AExpr:
  | a-type-let(l :: SL.Location, bind :: ATypeBind, body :: AExpr) with:
    label(self): "a-type-let" end,
    tosource(self):
      PP.soft-surround(INDENT, 1,
        str-type-let +
        PP.group(PP.nest(INDENT,
            self.bind.tosource())) + str-colon,
        self.body.tosource(),
        str-end)
    end
  | a-let(l :: SL.Location, bind :: ABind, e :: ALettable, body :: AExpr) with:
    label(self): "a-let" end,
    tosource(self):
      PP.soft-surround(INDENT, 1,
        str-let +
        PP.group(PP.nest(INDENT,
            self.bind.tosource() + str-spaceequal + break-one + self.e.tosource())) + str-colon,
        self.body.tosource(),
        str-end)
    end
  | a-var(l :: SL.Location, bind :: ABind, e :: ALettable, body :: AExpr) with:
    label(self): "a-var" end,
    tosource(self):
      PP.soft-surround(INDENT, 1,
        str-var +
        PP.group(PP.nest(INDENT,
            self.bind.tosource() + str-spaceequal + break-one + self.e.tosource())) + str-colon,
        self.body.tosource(),
        str-end)
    end
  | a-seq(l :: SL.Location, e1 :: ALettable, e2 :: AExpr) with:
    label(self): "a-seq" end,
    tosource(self):
      self.e1.tosource() + PP.hardline + self.e2.tosource()
    end
  | a-tail-app(l :: SL.Location, f :: AVal, args :: List<AVal>) with:
    label(self): "a-tail-app" end,
    tosource(self):
      PP.group(self.f.tosource()
          + PP.parens(PP.nest(INDENT,
            PP.separate(PP.commabreak, self.args.map(lam(f): f.tosource() end)))))
    end
  | a-split-app(l :: SL.Location, is-var :: Boolean, f :: AVal, args :: List<AVal>, helper :: A.Name, helper-args :: List<AVal>) with:
    label(self): "a-split-app" end,
    tosource(self):
      PP.group(
        PP.group(PP.nest(INDENT,
            PP.str("split ")
              + PP.group(self.helper-args.first.tosource() + PP.str(" <== ") + self.f.tosource()
                + PP.parens(PP.nest(INDENT,
                PP.separate(PP.commabreak, self.args.map(lam(f): f.tosource() end))))))) +
        break-one +
        PP.group(PP.nest(INDENT, PP.str("and then") + break-one
              + self.helper.tosource()
              + PP.parens(PP.nest(INDENT,
                PP.separate(PP.commabreak, self.helper-args.map(lam(f): f.tosource() end)))))))
    end
  | a-if(l :: SL.Location, c :: AVal, t :: AExpr, e :: AExpr) with:
    label(self): "a-if" end,
    tosource(self):
      PP.group(
        str-if + PP.nest(2 * INDENT, self.c.tosource() + str-colon)
          + PP.nest(INDENT, break-one + self.t.tosource())
          + break-one + str-elsecolon
          + PP.nest(INDENT, break-one + self.e.tosource())
          + break-one + str-end)
    end
  | a-lettable(e :: ALettable) with:
    label(self): "a-lettable" end,
    tosource(self):
      self.e.tosource()
    end
sharing:
  visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end
end

data ABind:
  | a-bind(l :: SL.Location, id :: A.Name, ann :: A.Ann) with:
    label(self): "a-bind" end,
    tosource(self):
      if A.is-a-blank(self.ann): self.id.tosource()
      else: PP.infix(INDENT, 1, str-coloncolon, self.id.tosource(), self.ann.tosource())
      end
    end
sharing:
  visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end
end

data AVariant:
  | a-variant(
      l :: SL.Location,
      constr-loc :: SL.Location,
      name :: String,
      members :: List<AVariantMember>,
      with-members :: List<AField>
    ) with:
    label(self): "a-variant" end,
    tosource(self): PP.str("a-variant") end
  | a-singleton-variant(
      l :: SL.Location,
      name :: String,
      with-members :: List<AField>
    ) with:
    label(self): "a-variant" end,
    tosource(self): PP.str("a-variant") end
sharing:
  visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end
end

data AMemberType:
  | a-normal with:
    label(self): "a-normal" end,
    tosource(self): PP.str("") end
  | a-cyclic with:
    label(self): "a-cyclic" end,
    tosource(self): PP.str("cyclic ") end
  | a-mutable with:
    label(self): "a-mutable" end,
    tosource(self): PP.str("mutable ") end
end

data AVariantMember:
  | a-variant-member(
      l :: SL.Location,
      member-type :: AMemberType,
      bind :: ABind
    ) with:
    label(self): "a-variant-member" end,
    tosource(self):
      self.member_type.tosource() + self.bind.tosource()
    end
sharing:
  visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end
end


data ALettable:
  | a-module(l :: SL.Location, answer :: AVal, provides :: AVal, types, checks :: AVal) with:
    label(self): "a-module" end,
    tosource(self):
      PP.str("Module") + PP.parens(PP.flow-map(PP.commabreak, lam(x): x end, [list:
            PP.infix(INDENT, 1, str-colon, PP.str("Answer"), self.answer.tosource()),
            PP.infix(INDENT, 1, str-colon, PP.str("Provides"), self.provides.tosource()),
            PP.infix(INDENT, 1, str-colon, PP.str("Types"), 
              PP.brackets(PP.flow-map(PP.commabreak, _.tosource(), self.types))),
            PP.infix(INDENT, 1, str-colon, PP.str("checks"), self.checks.tosource())]))
    end    
  | a-data-expr(l :: SL.Location, name :: String, variants :: List<AVariant>, shared :: List<AField>) with:
    label(self): "a-data-expr" end,
    tosource(self):
      PP.str("data-expr")
    end
  | a-assign(l :: SL.Location, id :: A.Name, value :: AVal) with:
    label(self): "a-assign" end,
    tosource(self):
      PP.group(PP.nest(INDENT, self.id.tosource() + str-spacecolonequal + break-one + self.value.tosource()))
    end
  | a-app(l :: SL.Location, _fun :: AVal, args :: List<AVal>) with:
    label(self): "a-app" end,
    tosource(self):
      PP.group(self._fun.tosource()
          + PP.parens(PP.nest(INDENT,
            PP.separate(PP.commabreak, self.args.map(lam(f): f.tosource() end)))))
    end
  | a-prim-app(l :: SL.Location, f :: String, args :: List<AVal>) with:
    label(self): "a-prim-app" end,
    tosource(self):
      PP.group(PP.str(self.f) +
          PP.parens(PP.nest(INDENT,
            PP.separate(PP.commabreak, self.args.map(lam(f): f.tosource() end)))))
    end
  | a-obj(l :: SL.Location, fields :: List<AField>) with:
    label(self): "a-obj" end,
    tosource(self):
      PP.surround-separate(INDENT, 1, PP.lbrace + PP.rbrace,
        PP.lbrace, PP.commabreak, PP.rbrace, self.fields.map(lam(f): f.tosource() end))
    end
  | a-update(l :: SL.Location, supe :: AVal, fields :: List<AField>) with:
    label(self): "a-update" end,
    tosource(self):
      PP.str("update")
    end
  | a-extend(l :: SL.Location, supe :: AVal, fields :: List<AField>) with:
    label(self): "a-extend" end,
    tosource(self):
      PP.str("extend")
    end
  | a-dot(l :: SL.Location, obj :: AVal, field :: String) with:
    label(self): "a-dot" end,
    tosource(self): PP.infix(INDENT, 0, str-period, self.obj.tosource(), PP.str(self.field)) end
  | a-colon(l :: SL.Location, obj :: AVal, field :: String) with:
    label(self): "a-colon" end,
    tosource(self): PP.infix(INDENT, 0, str-colon, self.obj.tosource(), PP.str(self.field)) end
  | a-get-bang(l :: SL.Location, obj :: AVal, field :: String) with:
    label(self): "a-get-bang" end,
    tosource(self): PP.infix(INDENT, 0, str-bang, self.obj.tosource(), PP.str(self.field)) end
  | a-lam(l :: SL.Location, args :: List<ABind>, ret :: A.Ann, body :: AExpr) with:
    label(self): "a-lam" end,
    tosource(self): fun-method-pretty(PP.str("lam"), self.args, self.body) end
  | a-method(l :: SL.Location, args :: List<ABind>, ret :: A.Ann, body :: AExpr) with:
    label(self): "a-method" end,
    tosource(self): fun-method-pretty(PP.str("method"), self.args, self.body) end
  | a-val(v :: AVal) with:
    label(self): "a-val" end,
    tosource(self): self.v.tosource() end
sharing:
  visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end
end

fun fun-method-pretty(typ, args, body):
  arg-list = PP.nest(INDENT,
    PP.surround-separate(INDENT, 0, PP.lparen + PP.rparen, PP.lparen, PP.commabreak, PP.rparen,
      args.map(lam(a): a.tosource() end)))
  header = PP.group(typ + arg-list + str-colon)
  PP.surround(INDENT, 1, header, body.tosource(), str-end)
end

data AField:
  | a-field(l :: SL.Location, name :: String, value :: AVal) with:
    label(self): "a-field" end,
    tosource(self): PP.nest(INDENT, PP.str(self.name) + str-colonspace + self.value.tosource()) end,
sharing:
  visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end
end

data AVal:
  | a-srcloc(l :: SL.Location, loc :: SL.Location) with:
    label(self): "a-srcloc" end,
    tosource(self): PP.str(torepr(self.loc)) end
  | a-num(l :: SL.Location, n :: Number) with:
    label(self): "a-num" end,
    tosource(self): PP.number(self.n) end
  | a-str(l :: SL.Location, s :: String) with:
    label(self): "a-str" end,
    tosource(self): PP.squote(PP.str(self.s)) end
  | a-bool(l :: SL.Location, b :: Boolean) with:
    label(self): "a-bool" end,
    tosource(self): PP.str(tostring(self.b)) end
  | a-array(l :: SL.Location, values :: List<AVal>) with:
    label(self): "a-array" end,
    tosource(self):
      PP.surround-separate(INDENT, 0, PP.str("[raw-array: ]"), PP.str("[raw-array: "), PP.commabreak, PP.rbrack,
        self.values.map(_.tosource()))
    end
  # used for letrec
  | a-undefined(l :: SL.Location) with:
    label(self): "a-undefined" end,
    tosource(self): PP.str("UNDEFINED") end
  | a-id(l :: SL.Location, id :: A.Name) with:
    label(self): "a-id" end,
    tosource(self): PP.str(self.id.tostring()) end
  | a-id-var(l :: SL.Location, id :: A.Name) with:
    label(self): "a-id-var" end,
    tosource(self): PP.str("!" + self.id.tostring()) end
  | a-id-letrec(l :: SL.Location, id :: A.Name, safe :: Boolean) with:
    label(self): "a-id-letrec" end,
    tosource(self): PP.str("~" + self.id.tostring()) end
sharing:
  visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end
end

fun strip-loc-prog(p :: AProg):
  cases(AProg) p:
    | a-program(_, imports, body) =>
      a-program(dummy-loc, imports.map(strip-loc-import), body ^ strip-loc-expr)
  end
end

fun strip-loc-import(i :: AImport):
  cases(AImport) i:
    | a-import(_, import-type, name) =>
      a-import(dummy-loc, strip-loc-import-type(import-type), name.visit(A.dummy-loc-visitor))
    | a-import-types(_, import-type, name, types) =>
      a-import-types(dummy-loc, strip-loc-import-type(import-type),
        name.visit(A.dummy-loc-visitor), types.visit(A.dummy-loc-visitor))
  end
end

fun strip-loc-import-type(i :: AImportType):
  cases(AImportType) i:
    | a-import-builtin(_, name, id) => a-import-builtin(dummy-loc, name, id)
    | a-import-file(_, file, id) => a-import-builtin(dummy-loc, file, id)
  end
end

fun strip-loc-expr(expr :: AExpr):
  cases(AExpr) expr:
    | a-type-let(_, bind, body) =>
      a-type-let(dummy-loc, bind, body)
    | a-let(_, bind, val, body) =>
      a-let(dummy-loc, strip-loc-bind(bind), strip-loc-lettable(val), strip-loc-expr(body))
    | a-var(_, bind, val, body) =>
      a-var(dummy-loc, strip-loc-bind(bind), strip-loc-lettable(val), strip-loc-expr(body))
    | a-seq(_, e1, e2) =>
      a-seq(dummy-loc, strip-loc-lettable(e1), strip-loc-expr(e2))
    | a-if(_, c, t, e) =>
      a-if(dummy-loc, strip-loc-val(c), strip-loc-expr(t), strip-loc-expr(e))
    | a-split-app(_, is-var, f, args, helper, helper-args) =>
      a-split-app(
          dummy-loc,
          is-var,
          strip-loc-val(f),
          args.map(strip-loc-val),
          helper,
          helper-args.map(strip-loc-val)
        )
    | a-lettable(e) =>
      a-lettable(strip-loc-lettable(e))
  end
end

fun strip-loc-bind(bind :: ABind):
  cases(ABind) bind:
    | a-bind(_, id, ann) => a-bind(dummy-loc, id, ann.visit(A.dummy-loc-visitor))
  end
end

fun strip-loc-lettable(lettable :: ALettable):
  cases(ALettable) lettable:
    | a-module(_, answer, provides, types, checks) =>
      a-module(dummy-loc, strip-loc-val(answer), strip-loc-val(provides),
        types.map(_.visit(A.dummy-loc-visitor)), strip-loc-val(checks))
    | a-assign(_, id, value) => a-assign(dummy-loc, id, strip-loc-val(value))
    | a-app(_, f, args) =>
      a-app(dummy-loc, strip-loc-val(f), args.map(strip-loc-val))
    | a-prim-app(_, f, args) =>
      a-prim-app(dummy-loc, f, args.map(strip-loc-val))
    | a-obj(_, fields) => a-obj(dummy-loc, fields.map(strip-loc-field))
    | a-update(_, supe, fields) =>
      a-update(_, strip-loc-val(supe), fields.map(strip-loc-field))
    | a-extend(_, supe, fields) =>
      a-extend(_, strip-loc-val(supe), fields.map(strip-loc-field))
    | a-dot(_, obj, field) =>
      a-dot(dummy-loc, strip-loc-val(obj), field)
    | a-colon(_, obj, field) =>
      a-colon(dummy-loc, strip-loc-val(obj), field)
    | a-get-bang(_, obj, field) =>
      a-get-bang(dummy-loc, strip-loc-val(obj), field)
    | a-lam(_, args, ret, body) =>
      a-lam(dummy-loc, args, ret, strip-loc-expr(body))
    | a-method(_, args, ret, body) =>
      a-method(dummy-loc, args, ret, strip-loc-expr(body))
    | a-val(v) =>
      a-val(strip-loc-val(v))
  end
end

fun strip-loc-field(field :: AField):
  cases(AField) field:
    | a-field(_, name, value) => a-field(dummy-loc, name, strip-loc-val(value))
  end
end

fun strip-loc-val(val :: AVal):
  cases(AVal) val:
    | a-srcloc(_, l) => a-srcloc(dummy-loc, l)
    | a-num(_, n) => a-num(dummy-loc, n)
    | a-str(_, s) => a-str(dummy-loc, s)
    | a-bool(_, b) => a-bool(dummy-loc, b)
    | a-array(_, vs) => a-array(dummy-loc, vs)
    | a-undefined(_) => a-undefined(dummy-loc)
    | a-id(_, id) => a-id(dummy-loc, id)
    | a-id-var(_, id) => a-id-var(dummy-loc, id)
    | a-id-letrec(_, id, safe) => a-id-letrec(dummy-loc, id, safe)
  end
end

default-map-visitor = {
  a-module(self, l :: SL.Location, answer :: AVal, provides :: AVal, types :: List<A.AField>, checks :: AVal):
    a-module(l, answer.visit(self), provides.visit(self), types, checks.visit(self))
  end,
  a-program(self, l :: SL.Location, imports :: List<AImport>, body :: AExpr):
    a-program(l, imports.map(_.visit(self)), body.visit(self))
  end,
  a-import(self, l :: SL.Location, import-type :: AImportType, name :: A.Name):
    a-import(l, import-type.visit(self), name.visit(self))
  end,
  a-import-types(self, l :: SL.Location, import-type :: AImportType, name :: A.NAme, types :: A.Name):
    a-import-types(l, import-type.visit(self), name.visit(self), types.visit(self))
  end,
  a-import-file(self, l :: SL.Location, file :: String, name :: A.Name):
    a-import-file(l, file, name)
  end,
  a-import-builtin(self, l :: SL.Location, lib :: String, name :: A.Name):
    a-import-builtin(l, lib, name)
  end,
  a-type-bind(self, l, name, ann):
    a-type-bind(l, name, ann)
  end,
  a-newtype-bind(self, l, name, nameb):
    a-newtype-bind(l, name, nameb)
  end,
  a-type-let(self, l, bind, body):
    a-type-let(l, bind.visit(self), body.visit(self))
  end,
  a-let(self, l :: SL.Location, bind :: ABind, e :: ALettable, body :: AExpr):
    a-let(l, bind.visit(self), e.visit(self), body.visit(self))
  end,
  a-var(self, l :: SL.Location, bind :: ABind, e :: ALettable, body :: AExpr):
    a-var(l, bind.visit(self), e.visit(self), body.visit(self))
  end,
  a-seq(self, l :: SL.Location, e1 :: ALettable, e2 :: AExpr):
    a-seq(l, e1.visit(self), e2.visit(self))
  end,
  a-data-expr(self, l :: SL.Location, name :: String, variants :: List<AVariant>, shared :: List<AField>):
    a-data-expr(l, name, variants.map(_.visit(self)), shared.map(_.visit(self)))
  end,
  a-variant(self, l :: SL.Location, constr-loc :: SL.Location, name :: String, members :: List<AVariantMember>, with-members :: List<AField>):
    a-variant(l, constr-loc, name, members.map(_.visit(self)), with-members.map(_.visit(self)))
  end,
  a-singleton-variant(self, l :: SL.Location, name :: String, with-members :: List<AField>):
    a-singleton-variant(l, name, with-members.map(_.visit(self)))
  end,
  a-variant-member(self, l :: SL.Location, member-type :: AMemberType, bind :: ABind):
    a-variant-member(l, member-type, bind.visit(self))
  end,
  a-tail-app(self, l :: SL.Location, _fun :: AVal, args :: List<AVal>):
    a-tail-app(l, _fun.visit(self), args.map(_.visit(self)))
  end,
  a-split-app(self, l :: SL.Location, is-var :: Boolean, f :: AVal, args :: List<AVal>, helper :: String, helper-args :: List<AVal>):
    a-split-app(l, is-var, f.visit(self), args.map(_.visit(self)), helper, helper-args.map(_.visit(self)))
  end,
  a-if(self, l :: SL.Location, c :: AVal, t :: AExpr, e :: AExpr):
    a-if(l, c.visit(self), t.visit(self), e.visit(self))
  end,
  a-lettable(self, e :: ALettable):
    a-lettable(e.visit(self))
  end,
  a-assign(self, l :: SL.Location, id :: A.Name, value :: AVal):
    a-assign(l, id, value.visit(self))
  end,
  a-app(self, l :: SL.Location, _fun :: AVal, args :: List<AVal>):
    a-app(l, _fun.visit(self), args.map(_.visit(self)))
  end,
  a-prim-app(self, l :: SL.Location, f :: String, args :: List<AVal>):
    a-prim-app(l, f, args.map(_.visit(self)))
  end,
  a-obj(self, l :: SL.Location, fields :: List<AField>):
    a-obj(l, fields.map(_.visit(self)))
  end,
  a-update(self, l :: SL.Location, supe :: AVal, fields :: List<AField>):
    a-update(l, supe.visit(self), fields.map(_.visit(self)))
  end,
  a-extend(self, l :: SL.Location, supe :: AVal, fields :: List<AField>):
    a-extend(l, supe.visit(self), fields.map(_.visit(self)))
  end,
  a-dot(self, l :: SL.Location, obj :: AVal, field :: String):
    a-dot(l, obj.visit(self), field)
  end,
  a-colon(self, l :: SL.Location, obj :: AVal, field :: String):
    a-colon(l, obj.visit(self), field)
  end,
  a-get-bang(self, l :: SL.Location, obj :: AVal, field :: String):
    a-get-bang(l, obj.visit(self), field)
  end,
  a-lam(self, l :: SL.Location, args :: List<ABind>, ret :: A.Ann, body :: AExpr):
    a-lam(l, args.map(_.visit(self)), ret, body.visit(self))
  end,
  a-method(self, l :: SL.Location, args :: List<ABind>, ret :: A.Ann, body :: AExpr):
    a-method(l, args.map(_.visit(self)), ret, body.visit(self))
  end,
  a-val(self, v :: AVal):
    a-val(v.visit(self))
  end,
  a-bind(self, l :: SL.Location, id :: A.Name, ann :: A.Ann):
    a-bind(l, id, ann)
  end,
  a-field(self, l :: SL.Location, name :: String, value :: AVal):
    a-field(l, name, value.visit(self))
  end,
  a-srcloc(self, l, loc):
    a-srcloc(l, loc)
  end,
  a-num(self, l :: SL.Location, n :: Number):
    a-num(l, n)
  end,
  a-array(self, l :: SL.Location, vals :: List<AVal>):
    a-array(l, vals.map(_.visit(self)))
  end,
  a-str(self, l :: SL.Location, s :: String):
    a-str(l, s)
  end,
  a-bool(self, l :: SL.Location, b :: Boolean):
    a-bool(l, b)
  end,
  a-undefined(self, l :: SL.Location):
    a-undefined(l)
  end,
  a-id(self, l :: SL.Location, id :: A.Name):
    a-id(l, id)
  end,
  a-id-var(self, l :: SL.Location, id :: A.Name):
    a-id-var(l, id)
  end,
  a-id-letrec(self, l :: SL.Location, id :: A.Name, safe :: Boolean):
    a-id-letrec(l, id, safe)
  end
}

fun freevars-list-acc(anns :: List<A.Ann>, seen-so-far):
  for fold(acc from seen-so-far, a from anns):
    freevars-ann-acc(a, acc)
  end
end

fun freevars-ann-acc(ann :: A.Ann, seen-so-far :: Set<A.Name>) -> Set<A.Name>:
  lst-a = freevars-list-acc(_, seen-so-far)
  cases(A.Ann) ann:
    | a-blank => seen-so-far
    | a-any => seen-so-far
    | a-name(l, name) => seen-so-far.add(name)
    | a-dot(l, left, right) => seen-so-far.add(left)
    | a-arrow(l, args, ret, _) => lst-a(link(ret, args))
    | a-method(l, args, ret) => lst-a(link(ret, args))
    | a-record(l, fields) => lst-a(fields.map(_.ann))
    | a-app(l, a, args) => lst-a(link(a, args))
    | a-pred(l, a, pred) =>
      name = cases(A.Expr) pred:
        | s-id(_, n) => n
        | s-id-letrec(_, n, _) => n
      end
      freevars-ann-acc(a, seen-so-far.add(name))
  end
end

fun freevars-e-acc(expr :: AExpr, seen-so-far :: Set<A.Name>) -> Set<A.Name>:
  cases(AExpr) expr:
    | a-type-let(_, b, body) =>
      body-ids = freevars-e-acc(body, seen-so-far)
      cases(ATypeBind) b:
        | a-type-bind(_, name, ann) =>
          freevars-ann-acc(ann, body-ids.remove(name))
        | a-newtype-bind(_, name, nameb) =>
          body-ids.remove(name).remove(nameb)
      end
    | a-let(_, b, e, body) =>
      from-body = freevars-e-acc(body, seen-so-far)
      freevars-ann-acc(b.ann, freevars-l-acc(e, from-body.remove(b.id)))
    | a-var(_, b, e, body) =>
      from-body = freevars-e-acc(body, seen-so-far)
      freevars-ann-acc(b.ann, freevars-l-acc(e, from-body.remove(b.id)))
    | a-seq(_, e1, e2) =>
      from-e2 = freevars-e-acc(e2, seen-so-far)
      freevars-l-acc(e1, from-e2)
    | a-tail-app(_, f, args) =>
      from-f = freevars-v-acc(f, seen-so-far)
      for fold(acc from from-f, arg from args):
        freevars-v-acc(arg, acc)
      end
    | a-split-app(_, _, f, args, name, helper-args) =>
      from-f = freevars-v-acc(f, seen-so-far)
      with-args = for fold(acc from from-f, arg from args):
        freevars-v-acc(arg, acc)
      end
      with-helper-args = for fold(acc from with-args, arg from helper-args):
        freevars-v-acc(arg, acc)
      end
      with-helper-args.remove(name)
    | a-lettable(e) => freevars-l-acc(e, seen-so-far)
    | a-if(_, c, t, a) =>
      freevars-e-acc(a, freevars-e-acc(t, freevars-v-acc(c, seen-so-far)))
  end
end

fun freevars-e(expr :: AExpr) -> Set<A.Name>:
  freevars-e-acc(expr, sets.empty-tree-set)
where:
  d = dummy-loc
  freevars-e(
      a-let(d, a-bind(d, "x", A.a-blank), a-val(a-num(d, 4)),
        a-lettable(a-val(a-id(d, "y"))))).to-list() is [list: "y"]
end

fun freevars-variant-acc(v :: AVariant, seen-so-far :: Set<A.Name>) -> Set<A.Name>:
  from-members = cases(AVariant) v:
    | a-variant(_, _, _, members, _) =>
      for fold(acc from seen-so-far, m from members):
        freevars-ann-acc(m.bind.ann, acc)
      end
    | a-singleton-variant(_, _, _) => seen-so-far
  end
  for fold(acc from from-members, m from v.with-members):
    freevars-v-acc(m.value, acc)
  end
end

fun freevars-l-acc(e :: ALettable, seen-so-far :: Set<A.Name>) -> Set<A.Name>:
  cases(ALettable) e:
    | a-module(_, ans, provs, types, checks) =>
      freevars-v-acc(ans,
        freevars-v-acc(provs,
          freevars-list-acc(types.map(_.ann),
            freevars-v-acc(checks, seen-so-far))))
    | a-assign(_, id, v) => freevars-v-acc(v, seen-so-far.add(id))
    | a-app(_, f, args) =>
      from-f = freevars-v-acc(f, seen-so-far)
      for fold(acc from from-f, arg from args):
        freevars-v-acc(arg, acc)
      end
    | a-prim-app(_, _, args) =>
      for fold(acc from seen-so-far, arg from args):
        freevars-v-acc(arg, acc)
      end
    | a-lam(_, args, ret, body) =>
      from-body = freevars-e-acc(body, seen-so-far)
      without-args = from-body.difference(sets.list-to-tree-set(args.map(_.id)))
      from-args = for fold(acc from without-args, a from args):
        freevars-ann-acc(a.ann, acc)
      end
      freevars-ann-acc(ret, from-args)
    | a-method(_, args, ret, body) =>
      from-body = freevars-e-acc(body, seen-so-far)
      without-args = from-body.difference(sets.list-to-tree-set(args.map(_.id)))
      from-args = for fold(acc from without-args, a from args):
        freevars-ann-acc(a.ann, acc)
      end
      freevars-ann-acc(ret, from-args)
    | a-obj(_, fields) =>
      for fold(acc from seen-so-far, f from fields):
        freevars-v-acc(f.value, acc)
      end
    | a-update(_, supe, fields) =>
      from-supe = freevars-v-acc(supe, seen-so-far)
      for fold(acc from from-supe, f from fields):
        freevars-v-acc(f.value, acc)
      end
    | a-data-expr(_, _, variants, shared) =>
      from-variants = for fold(acc from seen-so-far, v from variants):
        freevars-variant-acc(v, acc)
      end
      for fold(acc from from-variants, s from shared):
        freevars-v-acc(s.value, acc)
      end
    | a-extend(_, supe, fields) =>
      from-supe = freevars-v-acc(supe, seen-so-far)
      for fold(acc from from-supe, f from fields):
        freevars-v-acc(f.value, acc)
      end
    | a-dot(_, obj, _) => freevars-v-acc(obj, seen-so-far)
    | a-colon(_, obj, _) => freevars-v-acc(obj, seen-so-far)
    | a-get-bang(_, obj, _) => freevars-v-acc(obj, seen-so-far)
    | a-val(v) => freevars-v-acc(v, seen-so-far)
    | else => raise("Non-lettable in freevars-l " + torepr(e))
  end
end

fun freevars-l(e :: ALettable) -> Set<A.Name>:
  freevars-l-acc(e, sets.empty-tree-set)
end

fun freevars-v-acc(v :: AVal, seen-so-far :: Set<A.Name>) -> Set<A.Name>:
  cases(AVal) v:
    | a-array(_, vs) =>
      for fold(acc from seen-so-far, shadow v from vs):
        freevars-v-acc(v, acc)
      end
    | a-id(_, id) => seen-so-far.add(id)
    | a-id-var(_, id) => seen-so-far.add(id)
    | a-id-letrec(_, id, _) => seen-so-far.add(id)
    | a-srcloc(_, _) => seen-so-far
    | a-num(_, _) => seen-so-far
    | a-str(_, _) => seen-so-far
    | a-bool(_, _) => seen-so-far
    | a-undefined(_) => seen-so-far
    | else => raise("Unknown AVal in freevars-v " + torepr(v))
  end
end

fun freevars-v(v :: AVal) -> Set<A.Name>:
  freevars-v-acc(v, sets.empty-tree-set)
end

fun <a> unions(ss :: List<Set<a>>) -> Set<a>:
  for fold(unioned from sets.empty-tree-set, s from ss):
    unioned.union(s)
  end
end

