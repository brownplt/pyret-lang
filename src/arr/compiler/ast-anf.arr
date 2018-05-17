#lang pyret

provide *
provide-types *
import ast as A
import pprint as PP
import srcloc as SL
import string-dict as SD

type NameDict = SD.MutableStringDict
type FrozenNameDict = SD.StringDict

empty-dict = SD.make-mutable-string-dict


type Loc = SL.Srcloc

INDENT = 2

break-one = PP.sbreak(1)
str-method = PP.str(" method")
str-letrec = PP.str("letrec ")
str-period = PP.str(".")
str-bang = PP.str("!")
str-brackets = PP.str("[]")
str-cases = PP.str("cases")
str-colon = PP.str(":")
str-coloncolon = PP.str("::")
str-colonspace = PP.str(":")
str-end = PP.str("end")
str-type-let = PP.str("type-let ")
str-let = PP.str("let ")
str-var = PP.str("var ")
str-if = PP.str("if ")
str-elsecolon = PP.str("else:")
str-elsebranch = PP.str("| else =>")
str-thickarrow = PP.str("=>")
str-spacecolonequal = PP.str(" :=")
str-spaceequal = PP.str(" =")
str-import = PP.str("import")
str-provide = PP.str("provide")
str-as = PP.str("as")
str-from = PP.str("from")
str-newtype = PP.str("newtype ")

dummy-loc = SL.builtin("dummy-location")
is-s-provide-complete = A.is-s-provide-complete
data AProg:
  | a-program(l :: Loc, provides :: A.Provide%(is-s-provide-complete), imports :: List<AImport>, body :: AExpr) with:
    method label(self): "a-program" end,
    method tosource(self):
      PP.group(
        PP.flow-map(PP.hardline, lam(i): i.tosource() end, self.imports)
          + PP.hardline
          + self.body.tosource()
        )
    end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end
end

data AImportType:
  | a-import-builtin(l :: Loc, lib :: String) with:
    method tosource(self): PP.str(self.lib) end
  | a-import-special(l :: Loc, kind :: String, args :: List<String>) with:
    method tosource(self):
      PP.group(PP.str(self.kind)
          + PP.parens(PP.nest(INDENT,
            PP.separate(PP.commabreak, self.args.map(PP.str)))))
    end
end

data AImport:
  | a-import-complete(
      l :: Loc,
      values :: List<A.Name>,
      types :: List<A.Name>,
      import-type :: AImportType,
      vals-name :: A.Name,
      types-name :: A.Name) with:
    method label(self): "a-import-complete" end,
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
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end
end

data ATypeBind:
  | a-type-bind(l :: Loc, name :: A.Name, ann :: A.Ann) with:
    method label(self): "a-type-bind" end,
    method tosource(self): PP.infix(INDENT, 1, str-coloncolon, self.name.tosource(), self.ann.tosource()) end
  | a-newtype-bind(l :: Loc, name :: A.Name, namet :: A.Name) with:
    method label(self): "a-newtype-bind" end,
    method tosource(self):
      PP.group(str-newtype + self.name.tosource() + break-one + str-as + break-one + self.namet.tosource())
    end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end
end

data AExpr:
  | a-type-let(l :: Loc, bind :: ATypeBind, body :: AExpr) with:
    method label(self): "a-type-let" end,
    method tosource(self):
      PP.soft-surround(INDENT, 1,
        str-type-let +
        PP.group(PP.nest(INDENT,
            self.bind.tosource())) + str-colon,
        self.body.tosource(),
        str-end)
    end
  | a-let(l :: Loc, bind :: ABind, e :: ALettable, body :: AExpr) with:
    method label(self): "a-let" end,
    method tosource(self):
      PP.soft-surround(INDENT, 1,
        str-let +
        PP.group(PP.nest(INDENT,
            self.bind.tosource() + str-spaceequal + break-one + self.e.tosource())) + str-colon,
        self.body.tosource(),
        str-end)
    end
  | a-arr-let(l :: Loc, bind :: ABind, idx :: Number, e :: ALettable, body :: AExpr) with:
    method label(self): "a-arr-let" end,
    method tosource(self):
      PP.soft-surround(INDENT, 1,
        str-let +
        PP.group(
          PP.nest(
            INDENT,
            PP.group(self.bind.tosource() + PP.brackets(PP.number(self.idx))) +
            str-spaceequal +
            break-one +
            self.e.tosource())) +
        str-colon,
        self.body.tosource(),
        str-end)
    end
  | a-var(l :: Loc, bind :: ABind, e :: ALettable, body :: AExpr) with:
    method label(self): "a-var" end,
    method tosource(self):
      PP.soft-surround(INDENT, 1,
        str-var +
        PP.group(PP.nest(INDENT,
            self.bind.tosource() + str-spaceequal + break-one + self.e.tosource())) + str-colon,
        self.body.tosource(),
        str-end)
    end
  | a-seq(l :: Loc, e1 :: ALettable, e2 :: AExpr) with:
    method label(self): "a-seq" end,
    method tosource(self):
      self.e1.tosource() + PP.hardline + self.e2.tosource()
    end
  | a-lettable(l :: Loc, e :: ALettable) with:
    method label(self): "a-lettable" end,
    method tosource(self):
      self.e.tosource()
    end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(elt): raise("No visitor field for " + self.label()) end)
  end
end

data ABind:
  | a-bind(l :: Loc, id :: A.Name, ann :: A.Ann) with:
    method label(self): "a-bind" end,
    method tosource(self):
      if A.is-a-blank(self.ann): self.id.to-compiled-source()
      else: PP.infix(INDENT, 1, str-coloncolon, self.id.to-compiled-source(), self.ann.tosource())
      end
    end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end
end

data AVariant:
  | a-variant(
      l :: Loc,
      constr-loc :: Loc,
      name :: String,
      members :: List<AVariantMember>,
      with-members :: List<AField>
    ) with:
    method label(self): "a-variant" end,
    method tosource(self): PP.str("a-variant") end
  | a-singleton-variant(
      l :: Loc,
      name :: String,
      with-members :: List<AField>
    ) with:
    method label(self): "a-variant" end,
    method tosource(self): PP.str("a-variant") end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end
end

data AMemberType:
  | a-normal with:
    method label(self): "a-normal" end,
    method tosource(self): PP.str("") end
  | a-mutable with:
    method label(self): "a-mutable" end,
    method tosource(self): PP.str("mutable ") end
end

data AVariantMember:
  | a-variant-member(
      l :: Loc,
      member-type :: AMemberType,
      bind :: ABind
    ) with:
    method label(self): "a-variant-member" end,
    method tosource(self):
      self.member_type.tosource() + self.bind.tosource()
    end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end
end

data ACasesBind:
  | a-cases-bind(l :: Loc, field-type :: A.CasesBindType, bind :: ABind) with:
    method label(self): "s-cases-bind" end,
    method tosource(self):
      self.field-type.tosource() + PP.str(" ") + self.bind.tosource()
    end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end
end


data ACasesBranch:
  | a-cases-branch(l :: Loc, pat-loc :: Loc, name :: String, args :: List<ACasesBind>, body :: AExpr) with:
    method label(self): "a-cases-branch" end,
    method tosource(self):
      PP.nest(INDENT,
        PP.group(PP.str("| " + self.name)
            + PP.surround-separate(INDENT, 0, PP.str("()"), PP.lparen, PP.commabreak, PP.rparen,
            self.args.map(lam(a): a.tosource() end)) + break-one + str-thickarrow) + break-one +
        self.body.tosource())
    end
  | a-singleton-cases-branch(l :: Loc, pat-loc :: Loc, name :: String, body :: AExpr) with:
    method label(self): "a-singleton-cases-branch" end,
    method tosource(self):
      PP.nest(INDENT,
        PP.group(PP.str("| " + self.name) + break-one + str-thickarrow) + break-one + self.body.tosource())
    end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end
end

data ADefinedValue:
  | a-defined-value(name :: String, value :: AVal) with:
    method label(self): "a-defined-value" end,
    method tosource(self):
      PP.infix(INDENT, 1, str-colon, PP.str(self.name), self.value.tosource())
    end
  | a-defined-var(name :: String, id :: A.Name) with:
    method label(self): "a-defined-var" end,
    method tosource(self):
      PP.infix(INDENT, 1, str-colon, PP.str(self.name), self.id.toname())
    end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end
end
data ADefinedType:
  | a-defined-type(name :: String, typ :: A.Ann) with:
    method label(self): "a-defined-type" end,
    method tosource(self):
      PP.infix(INDENT, 1, str-coloncolon, PP.str(self.name), self.typ.tosource())
    end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end
end

data ALettable:
  | a-module(
      l :: Loc,
      answer :: AVal,
      defined-values :: List<ADefinedValue>,
      defined-types :: List<ADefinedType>,
      provided-values :: AVal,
      provided-types,
      checks :: AVal) with:
    method label(self): "a-module" end,
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
  | a-id-var(l :: Loc, id :: A.Name) with:
    method label(self): "a-id-var" end,
    method tosource(self): PP.str("!" + tostring(self.id)) end
  | a-id-letrec(l :: Loc, id :: A.Name, safe :: Boolean) with:
    method label(self): "a-id-letrec" end,
    method tosource(self): PP.str("~!" + tostring(self.id)) end
  | a-cases(l :: Loc, typ :: A.Ann, val :: AVal, branches :: List<ACasesBranch>, _else :: AExpr) with:
    method label(self): "a-cases" end,
    method tosource(self):
      header = str-cases + PP.parens(self.typ.tosource()) + break-one
        + self.val.tosource() + str-colon
      body = PP.separate(break-one, self.branches.map(lam(b): PP.group(b.tosource()) end))
        + break-one + PP.group(str-elsebranch + break-one + self._else.tosource())
      PP.surround(INDENT, 1, PP.group(header), body, str-end)
    end
  | a-if(l :: Loc, c :: AVal, t :: AExpr, e :: AExpr) with:
    method label(self): "a-if" end,
    method tosource(self):
      PP.group(
        str-if + PP.nest(2 * INDENT, self.c.tosource() + str-colon)
          + PP.nest(INDENT, break-one + self.t.tosource())
          + break-one + str-elsecolon
          + PP.nest(INDENT, break-one + self.e.tosource())
          + break-one + str-end)
    end
  | a-data-expr(l :: Loc, name :: String, namet :: A.Name, variants :: List<AVariant>, shared :: List<AField>) with:
    method label(self): "a-data-expr" end,
    method tosource(self):
      PP.str("data-expr")
    end
  | a-assign(l :: Loc, id :: A.Name, value :: AVal) with:
    method label(self): "a-assign" end,
    method tosource(self):
      PP.group(PP.nest(INDENT, self.id.to-compiled-source() + str-spacecolonequal + break-one + self.value.tosource()))
    end
  | a-app(l :: Loc, _fun :: AVal, args :: List<AVal>, app-info :: A.AppInfo) with:
    method label(self): "a-app" end,
    method tosource(self):
      PP.group(self._fun.tosource()
          + PP.parens(PP.nest(INDENT,
            PP.separate(PP.commabreak, self.args.map(lam(f): f.tosource() end)))))
    end
  | a-method-app(l :: Loc, obj :: AVal, meth :: String, args :: List<AVal>) with:
    method label(self): "a-app" end,
    method tosource(self):
      PP.group(PP.infix(INDENT, 0, str-period, self.obj.tosource(), PP.str(self.meth))
          + PP.parens(PP.nest(INDENT,
            PP.separate(PP.commabreak, self.args.map(lam(f): f.tosource() end)))))
    end
  | a-prim-app(l :: Loc, f :: String, args :: List<AVal>) with:
    method label(self): "a-prim-app" end,
    method tosource(self):
      PP.group(PP.str(self.f) +
          PP.parens(PP.nest(INDENT,
            PP.separate(PP.commabreak, self.args.map(lam(f): f.tosource() end)))))
    end
  | a-ref(l :: Loc, ann :: Option<A.Ann>) with:
    method label(self): "a-ref" end,
    method tosource(self):
      cases(Option) self.ann:
        | none => PP.str("bare-ref")
        | some(ann) =>
          PP.group(PP.str("ref ") + ann.tosource())
      end
    end
  | a-tuple(l :: Loc, fields :: List<AVal>%(is-link)) with:
    method label(self): "a-tuple" end,
    method tosource(self):
     PP.surround-separate(INDENT, 1, PP.str("Empty tuple shoudn't happen"), 
        PP.lbrace, PP.semibreak, PP.rbrace, self.fields.map(_.tosource()))
    end
  | a-tuple-get(l :: Loc, tup :: AVal, index :: Number) with:
   method label(self): "s-tuple-get" end,
    method tosource(self): self.tup.tosource() + PP.str(".") + PP.lbrace + PP.number(self.index) + PP.rbrace
    end 
  | a-obj(l :: Loc, fields :: List<AField>) with:
    method label(self): "a-obj" end,
    method tosource(self):
      PP.surround-separate(INDENT, 1, PP.lbrace + PP.rbrace,
        PP.lbrace, PP.commabreak, PP.rbrace, self.fields.map(lam(f): f.tosource() end))
    end
  | a-update(l :: Loc, supe :: AVal, fields :: List<AField>) with:
    method label(self): "a-update" end,
    method tosource(self):
      PP.str("update")
    end
  | a-extend(l :: Loc, supe :: AVal, fields :: List<AField>) with:
    method label(self): "a-extend" end,
    method tosource(self):
      PP.str("extend")
    end
  | a-dot(l :: Loc, obj :: AVal, field :: String) with:
    method label(self): "a-dot" end,
    method tosource(self): PP.infix(INDENT, 0, str-period, self.obj.tosource(), PP.str(self.field)) end
  | a-colon(l :: Loc, obj :: AVal, field :: String) with:
    method label(self): "a-colon" end,
    method tosource(self): PP.infix(INDENT, 0, str-colon, self.obj.tosource(), PP.str(self.field)) end
  | a-get-bang(l :: Loc, obj :: AVal, field :: String) with:
    method label(self): "a-get-bang" end,
    method tosource(self): PP.infix(INDENT, 0, str-bang, self.obj.tosource(), PP.str(self.field)) end
  | a-lam(l :: Loc, name :: String, args :: List<ABind>, ret :: A.Ann, body :: AExpr) with:
    method label(self): "a-lam" end,
    method tosource(self): fun-method-pretty(PP.str("lam"), self.args, self.body) end
  | a-method(l :: Loc, name :: String, args :: List<ABind>, ret :: A.Ann, body :: AExpr) with:
    method label(self): "a-method" end,
    method tosource(self): fun-method-pretty(PP.str("method"), self.args, self.body) end
  | a-val(l :: Loc, v :: AVal) with:
    method label(self): "a-val" end,
    method tosource(self): self.v.tosource() end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(_): raise("No visitor field for " + self.label()) end)
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
  | a-field(l :: Loc, name :: String, value :: AVal) with:
    method label(self): "a-field" end,
    method tosource(self): PP.nest(INDENT, PP.str(self.name) + str-colonspace + self.value.tosource()) end,
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end
end

data AVal:
  | a-srcloc(l :: Loc, loc :: Loc) with:
    method label(self): "a-srcloc" end,
    method tosource(self): PP.str(torepr(self.loc)) end
  | a-num(l :: Loc, n :: Number) with:
    method label(self): "a-num" end,
    method tosource(self): PP.number(self.n) end
  | a-str(l :: Loc, s :: String) with:
    method label(self): "a-str" end,
    method tosource(self): PP.str(torepr(self.s)) end
  | a-bool(l :: Loc, b :: Boolean) with:
    method label(self): "a-bool" end,
    method tosource(self): PP.str(tostring(self.b)) end
  # used for letrec
  | a-undefined(l :: Loc) with:
    method label(self): "a-undefined" end,
    method tosource(self): PP.str("UNDEFINED") end
  | a-id(l :: Loc, id :: A.Name) with:
    method label(self): "a-id" end,
    method tosource(self): self.id.to-compiled-source() end
  | a-id-safe-letrec(l :: Loc, id :: A.Name) with:
    method label(self): "a-id-safe-letrec" end,
    method tosource(self): PP.str("~" + tostring(self.id)) end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end
end

fun strip-loc-prog(p :: AProg):
  cases(AProg) p:
    | a-program(_, prov, imports, body) =>
      a-program(dummy-loc, prov, imports.map(strip-loc-import), body ^ strip-loc-expr)
  end
end

fun strip-loc-import(i :: AImport):
  cases(AImport) i:
    | a-import-complete(_, vns, tns, imp, vn, tn) =>
      a-import-complete(dummy-loc, vns, tns, imp, vn, tn)
  end
end

fun strip-loc-import-type(i :: AImportType):
  cases(AImportType) i:
    | a-import-builtin(_, name, id) => a-import-builtin(dummy-loc, name, id)
  end
end

fun strip-loc-expr(expr :: AExpr):
  cases(AExpr) expr:
    | a-type-let(_, bind, body) =>
      a-type-let(dummy-loc, bind, body)
    | a-let(_, bind, val, body) =>
      a-let(dummy-loc, strip-loc-bind(bind), strip-loc-lettable(val), strip-loc-expr(body))
    | a-arr-let(_, bind, idx, e, body) =>
      a-arr-let(dummy-loc, bind, idx, e, body)
    | a-var(_, bind, val, body) =>
      a-var(dummy-loc, strip-loc-bind(bind), strip-loc-lettable(val), strip-loc-expr(body))
    | a-seq(_, e1, e2) =>
      a-seq(dummy-loc, strip-loc-lettable(e1), strip-loc-expr(e2))
    | a-lettable(_, e) =>
      a-lettable(dummy-loc, strip-loc-lettable(e))
  end
end

fun strip-loc-bind(bind :: ABind):
  cases(ABind) bind:
    | a-bind(_, id, ann) => a-bind(dummy-loc, id, ann.visit(A.dummy-loc-visitor))
  end
end

fun strip-loc-lettable(lettable :: ALettable):
  cases(ALettable) lettable:
    | a-module(_, answer, dv, dt, provides, types, checks) =>
      a-module(dummy-loc, strip-loc-val(answer), dv, dt, strip-loc-val(provides),
        types.map(_.visit(A.dummy-loc-visitor)), strip-loc-val(checks))
    | a-if(_, c, t, e) =>
      a-if(dummy-loc, strip-loc-val(c), strip-loc-expr(t), strip-loc-expr(e))
    | a-assign(_, id, value) => a-assign(dummy-loc, id, strip-loc-val(value))
    | a-app(_, f, args, app-info) =>
      a-app(dummy-loc, strip-loc-val(f), args.map(strip-loc-val), app-info)
    | a-method-app(_, obj, meth, args) =>
      a-method-app(dummy-loc, strip-loc-val(obj), meth, args.map(strip-loc-val))
    | a-prim-app(_, f, args) =>
      a-prim-app(dummy-loc, f, args.map(strip-loc-val))
    | a-ref(_, ann) => a-ref(dummy-loc, A.dummy-loc-visitor.option(ann))
    | a-tuple(_, fields) => a-tuple(dummy-loc, fields.map(strip-loc-val))
    | a-tuple-get(_, tup, index) => a-tuple-get(dummy-loc, strip-loc-val(tup), index)
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
    | a-lam(_, name, args, ret, body) =>
      a-lam(dummy-loc, name, args, ret, strip-loc-expr(body))
    | a-method(_, name, args, ret, body) =>
      a-method(dummy-loc, name, args, ret, strip-loc-expr(body))
    | a-id-var(_, id) => a-id-var(dummy-loc, id)
    | a-id-letrec(_, id, safe) => a-id-letrec(dummy-loc, id, safe)
    | a-val(_, v) =>
      a-val(dummy-loc, strip-loc-val(v))
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
    | a-undefined(_) => a-undefined(dummy-loc)
    | a-id(_, id) => a-id(dummy-loc, id)
    | a-id-safe-letrec(_, id) => a-id-safe-letrec(dummy-loc, id)
  end
end

default-map-visitor = {
  method a-module(self, l :: Loc, answer :: AVal, dv, dt, provides :: AVal, types :: List<A.AField>, checks :: AVal):
    a-module(l, answer.visit(self), dv, dt, provides.visit(self), types, checks.visit(self))
  end,
  method a-program(self, l :: Loc, p, imports :: List<AImport>, body :: AExpr):
    a-program(l, p, imports.map(_.visit(self)), body.visit(self))
  end,
  method a-import-builtin(self, l :: Loc, lib :: String, name :: A.Name):
    a-import-builtin(l, lib, name)
  end,
  method a-type-bind(self, l, name, ann):
    a-type-bind(l, name, ann)
  end,
  method a-newtype-bind(self, l, name, nameb):
    a-newtype-bind(l, name, nameb)
  end,
  method a-type-let(self, l, bind, body):
    a-type-let(l, bind.visit(self), body.visit(self))
  end,
  method a-let(self, l :: Loc, bind :: ABind, e :: ALettable, body :: AExpr):
    a-let(l, bind.visit(self), e.visit(self), body.visit(self))
  end,
  method a-arr-let(self, l :: Loc, bind :: ABind, idx :: Number, e :: ALettable, body :: AExpr):
    a-arr-let(l, bind.visit(self), idx, e.visit(self), body.visit(self))
  end,
  method a-var(self, l :: Loc, bind :: ABind, e :: ALettable, body :: AExpr):
    a-var(l, bind.visit(self), e.visit(self), body.visit(self))
  end,
  method a-seq(self, l :: Loc, e1 :: ALettable, e2 :: AExpr):
    a-seq(l, e1.visit(self), e2.visit(self))
  end,
  method a-cases(self, l :: Loc, typ :: A.Ann, val :: AVal, branches :: List<ACasesBranch>, _else :: AExpr):
    # NOTE: Not visiting the annotation yet
    a-cases(l, typ, val.visit(self), branches.map(_.visit(self)), _else.visit(self))
  end,
  method a-cases-bind(self, l, typ, bind):
    a-cases-bind(l, typ, bind.visit(self))
  end,
  method a-cases-branch(self, l :: Loc, pat-loc :: Loc, name :: String, args :: List<ACasesBind>, body :: AExpr):
    a-cases-branch(l, pat-loc, name, args.map(_.visit(self)), body.visit(self))
  end,
  method a-singleton-cases-branch(self, l :: Loc, pat-loc :: Loc, name :: String, body :: AExpr):
    a-singleton-cases-branch(l, pat-loc, name, body.visit(self))
  end,
  method a-data-expr(self, l :: Loc, name :: String, namet :: A.Name, variants :: List<AVariant>, shared :: List<AField>):
    a-data-expr(l, name, namet, variants.map(_.visit(self)), shared.map(_.visit(self)))
  end,
  method a-variant(self, l :: Loc, constr-loc :: Loc, name :: String, members :: List<AVariantMember>, with-members :: List<AField>):
    a-variant(l, constr-loc, name, members.map(_.visit(self)), with-members.map(_.visit(self)))
  end,
  method a-singleton-variant(self, l :: Loc, name :: String, with-members :: List<AField>):
    a-singleton-variant(l, name, with-members.map(_.visit(self)))
  end,
  method a-variant-member(self, l :: Loc, member-type :: AMemberType, bind :: ABind):
    a-variant-member(l, member-type, bind.visit(self))
  end,
  method a-if(self, l :: Loc, c :: AVal, t :: AExpr, e :: AExpr):
    a-if(l, c.visit(self), t.visit(self), e.visit(self))
  end,
  method a-lettable(self, l, e :: ALettable):
    a-lettable(l, e.visit(self))
  end,
  method a-assign(self, l :: Loc, id :: A.Name, value :: AVal):
    a-assign(l, id, value.visit(self))
  end,
  method a-app(self, l :: Loc, _fun :: AVal, args :: List<AVal>, app-info :: A.AppInfo):
    a-app(l, _fun.visit(self), args.map(_.visit(self)), app-info)
  end,
  method a-method-app(self, l :: Loc, obj :: AVal, meth :: String, args :: List<AVal>):
    a-method-app(l, obj.visit(self), meth, args.map(_.visit(self)))
  end,
  method a-prim-app(self, l :: Loc, f :: String, args :: List<AVal>):
    a-prim-app(l, f, args.map(_.visit(self)))
  end,
  method a-ref(self, l :: Loc, ann :: Option<A.Ann>):
    a-ref(l, ann)
  end,
  method a-tuple(self, l :: Loc, fields :: List<AVal>):
    a-tuple(l, fields.map(_.visit(self)))
  end,
  method a-tuple-get(self, l :: Loc, tup :: AVal, index :: Number):
    a-tuple-get(l, tup.visit(self), index)
  end,
  method a-obj(self, l :: Loc, fields :: List<AField>):
    a-obj(l, fields.map(_.visit(self)))
  end,
  method a-update(self, l :: Loc, supe :: AVal, fields :: List<AField>):
    a-update(l, supe.visit(self), fields.map(_.visit(self)))
  end,
  method a-extend(self, l :: Loc, supe :: AVal, fields :: List<AField>):
    a-extend(l, supe.visit(self), fields.map(_.visit(self)))
  end,
  method a-dot(self, l :: Loc, obj :: AVal, field :: String):
    a-dot(l, obj.visit(self), field)
  end,
  method a-colon(self, l :: Loc, obj :: AVal, field :: String):
    a-colon(l, obj.visit(self), field)
  end,
  method a-get-bang(self, l :: Loc, obj :: AVal, field :: String):
    a-get-bang(l, obj.visit(self), field)
  end,
  method a-lam(self, l :: Loc, name :: String, args :: List<ABind>, ret :: A.Ann, body :: AExpr):
    a-lam(l, name, args.map(_.visit(self)), ret, body.visit(self))
  end,
  method a-method(self, l :: Loc, name :: String, args :: List<ABind>, ret :: A.Ann, body :: AExpr):
    a-method(l, name, args.map(_.visit(self)), ret, body.visit(self))
  end,
  method a-val(self, l, v :: AVal):
    a-val(l, v.visit(self))
  end,
  method a-bind(self, l :: Loc, id :: A.Name, ann :: A.Ann):
    a-bind(l, id, ann)
  end,
  method a-field(self, l :: Loc, name :: String, value :: AVal):
    a-field(l, name, value.visit(self))
  end,
  method a-srcloc(self, l, loc):
    a-srcloc(l, loc)
  end,
  method a-num(self, l :: Loc, n :: Number):
    a-num(l, n)
  end,
  method a-str(self, l :: Loc, s :: String):
    a-str(l, s)
  end,
  method a-bool(self, l :: Loc, b :: Boolean):
    a-bool(l, b)
  end,
  method a-undefined(self, l :: Loc):
    a-undefined(l)
  end,
  method a-id(self, l :: Loc, id :: A.Name):
    a-id(l, id)
  end,
  method a-id-var(self, l :: Loc, id :: A.Name):
    a-id-var(l, id)
  end,
  method a-id-letrec(self, l :: Loc, id :: A.Name, safe :: Boolean):
    a-id-letrec(l, id, safe)
  end,
  method a-id-safe-letrec(self, l :: Loc, id :: A.Name):
    a-id-safe-letrec(l, id)
  end
}

fun freevars-list-acc(anns :: List<A.Ann>, seen-so-far):
  for fold(acc from seen-so-far, a from anns):
    freevars-ann-acc(a, acc)
  end
end

fun freevars-fields-acc(fields :: List<A.AField>, seen-so-far):
  for fold(acc from seen-so-far, f from fields):
    freevars-ann-acc(f.ann, acc)
  end
end

fun freevars-ann-acc(ann :: A.Ann, seen-so-far :: NameDict<A.Name>) -> NameDict<A.Name>:
  cases(A.Ann) ann block:
    | a-blank => seen-so-far
    | a-any(l) => seen-so-far
    | a-name(l, name) =>
      seen-so-far.set-now(name.key(), name)
      seen-so-far
    | a-type-var(l, name) => seen-so-far
    | a-dot(l, left, right) =>
      seen-so-far.set-now(left.key(), left)
      seen-so-far
    | a-arrow(l, args, ret, _) => freevars-list-acc(args, freevars-ann-acc(ret, seen-so-far))
    | a-arrow-argnames(l, args, ret, _) => freevars-fields-acc(args, freevars-ann-acc(ret, seen-so-far))
    | a-method(l, args, ret) => freevars-list-acc(args, freevars-ann-acc(ret, seen-so-far))
    | a-record(l, fields) => freevars-fields-acc(fields, seen-so-far)
    | a-tuple(l, fields) => freevars-list-acc(fields, seen-so-far)
    | a-app(l, a, args) => freevars-list-acc(args, freevars-ann-acc(a, seen-so-far))
    | a-method-app(l, a, _, args) => freevars-list-acc(args, freevars-ann-acc(a, seen-so-far))
    | a-pred(l, a, pred) =>
      name = cases(A.Expr) pred:
        | s-id(_, n) => n
        | s-id-letrec(_, n, _) => n
      end
      seen-so-far.set-now(name.key(), name)
      freevars-ann-acc(a, seen-so-far)
  end
end

fun freevars-e-acc(expr :: AExpr, seen-so-far :: NameDict<A.Name>) -> NameDict<A.Name>:
  cases(AExpr) expr block:
    | a-type-let(_, b, body) =>
      body-ids = freevars-e-acc(body, seen-so-far)
      cases(ATypeBind) b block:
        | a-type-bind(_, name, ann) =>
          body-ids.remove-now(name.key())
          freevars-ann-acc(ann, body-ids)
        | a-newtype-bind(_, name, nameb) =>
          body-ids.remove-now(name.key())
          body-ids.remove-now(nameb.key())
          body-ids
      end
    | a-let(_, b, e, body) =>
      from-body = freevars-e-acc(body, seen-so-far)
      from-body.remove-now(b.id.key())
      freevars-ann-acc(b.ann, freevars-l-acc(e, from-body))
    | a-arr-let(_, b, _, e, body) =>
      from-body = freevars-e-acc(body, seen-so-far)
      from-body.remove-now(b.id.key())
      freevars-ann-acc(b.ann, freevars-l-acc(e, from-body))
    | a-var(_, b, e, body) =>
      from-body = freevars-e-acc(body, seen-so-far)
      from-body.remove-now(b.id.key())
      from-body
      freevars-ann-acc(b.ann, freevars-l-acc(e, from-body))
    | a-seq(_, e1, e2) =>
      from-e2 = freevars-e-acc(e2, seen-so-far)
      freevars-l-acc(e1, from-e2)
    | a-lettable(_, e) => freevars-l-acc(e, seen-so-far)
  end
end

fun freevars-e(expr :: AExpr) -> FrozenNameDict<A.Name>:
  freevars-e-acc(expr, empty-dict()).freeze()
where:
  d = dummy-loc
  n = A.global-names.make-atom
  x = n("x")
  y = n("y")
  freevars-e(
      a-let(d, a-bind(d, x, A.a-blank), a-val(d, a-num(d, 4)),
        a-lettable(d, a-val(d, a-id(d, y))))).keys-list() is [list: y.key()]
end

fun freevars-variant-acc(v :: AVariant, seen-so-far :: NameDict<A.Name>) -> NameDict<A.Name>:
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

fun freevars-branches-acc(branches :: List<ACasesBranch>, seen-so-far :: NameDict<A.Name>) -> NameDict<A.Name>:
  for fold(acc from seen-so-far, b from branches):
    cases(ACasesBranch) b block:
      | a-cases-branch(_, _, _, args, body) =>
        from-body = freevars-e-acc(body, acc)
        shadow args = args.map(_.bind)
        without-args = from-body
        for each(arg from args):
          without-args.remove-now(arg.id.key())
        end
        for fold(inner-acc from without-args, arg from args):
          freevars-ann-acc(arg.ann, inner-acc)
        end
      | a-singleton-cases-branch(_, _, _, body) =>
        freevars-e-acc(body, acc)
    end
  end
end
fun freevars-l-acc(e :: ALettable, seen-so-far :: NameDict<A.Name>) -> NameDict<A.Name>:
  cases(ALettable) e block:
    | a-module(_, ans, dv, dt, provs, types, checks) =>
      freevars-v-acc(ans,
        freevars-v-acc(provs,
          freevars-list-acc(types.map(_.ann),
            freevars-v-acc(checks, seen-so-far))))
    | a-cases(_, typ, val, branches, _else) =>
      freevars-ann-acc(typ,
        freevars-v-acc(val,
          freevars-branches-acc(branches,
            freevars-e-acc(_else, seen-so-far))))
    | a-if(_, c, t, a) =>
      freevars-e-acc(a, freevars-e-acc(t, freevars-v-acc(c, seen-so-far)))
    | a-array(_, vs) =>
      for fold(acc from seen-so-far, shadow v from vs):
        freevars-v-acc(v, acc)
      end
    | a-assign(_, id, v) =>
      seen-so-far.set-now(id.key(), id)
      freevars-v-acc(v, seen-so-far)
    | a-app(_, f, args, _) =>
      from-f = freevars-v-acc(f, seen-so-far)
      for fold(acc from from-f, arg from args):
        freevars-v-acc(arg, acc)
      end
    | a-method-app(_, obj, _, args) =>
      from-obj = freevars-v-acc(obj, seen-so-far)
      for fold(acc from from-obj, arg from args):
        freevars-v-acc(arg, acc)
      end
    | a-prim-app(_, _, args) =>
      for fold(acc from seen-so-far, arg from args):
        freevars-v-acc(arg, acc)
      end
    | a-lam(_, _, args, ret, body) =>
      from-body = freevars-e-acc(body, seen-so-far)
      without-args = from-body
      for each(arg from args):
        without-args.remove-now(arg.id.key())
      end
      from-args = for fold(acc from without-args, a from args):
        freevars-ann-acc(a.ann, acc)
      end
      freevars-ann-acc(ret, from-args)
    | a-method(_, _, args, ret, body) =>
      from-body = freevars-e-acc(body, seen-so-far)
      without-args = from-body
      for each(arg from args):
        without-args.remove-now(arg.id.key())
      end
      from-args = for fold(acc from without-args, a from args):
        freevars-ann-acc(a.ann, acc)
      end
      freevars-ann-acc(ret, from-args)
    | a-ref(_, maybe-ann) =>
      cases(Option) maybe-ann:
        | none => seen-so-far
        | some(a) => freevars-ann-acc(a, seen-so-far)
      end
    | a-tuple(_, fields) =>
      for fold(acc from seen-so-far, f from fields):
        freevars-v-acc(f, acc)
      end
    | a-tuple-get(_, tup, index) =>
       freevars-v-acc(tup, seen-so-far)
    | a-obj(_, fields) =>
      for fold(acc from seen-so-far, f from fields):
        freevars-v-acc(f.value, acc)
      end
    | a-update(_, supe, fields) =>
      from-supe = freevars-v-acc(supe, seen-so-far)
      for fold(acc from from-supe, f from fields):
        freevars-v-acc(f.value, acc)
      end
    | a-data-expr(_, _, namet, variants, shared) =>
      from-variants = for fold(acc from seen-so-far, v from variants):
        freevars-variant-acc(v, acc)
      end
      from-shared = for fold(acc from from-variants, s from shared):
        freevars-v-acc(s.value, acc)
      end
      from-shared.set-now(namet.key(), namet)
      from-shared
    | a-extend(_, supe, fields) =>
      from-supe = freevars-v-acc(supe, seen-so-far)
      for fold(acc from from-supe, f from fields):
        freevars-v-acc(f.value, acc)
      end
    | a-dot(_, obj, _) => freevars-v-acc(obj, seen-so-far)
    | a-colon(_, obj, _) => freevars-v-acc(obj, seen-so-far)
    | a-get-bang(_, obj, _) => freevars-v-acc(obj, seen-so-far)
    | a-id-var(_, id) => 
      seen-so-far.set-now(id.key(), id)
      seen-so-far
    | a-id-letrec(_, id, _) => 
      seen-so-far.set-now(id.key(), id)
      seen-so-far
    | a-val(_, v) => freevars-v-acc(v, seen-so-far)
    | else => raise("Non-lettable in freevars-l " + torepr(e))
  end
end

fun freevars-l(e :: ALettable) -> FrozenNameDict<A.Name>:
  freevars-l-acc(e, empty-dict()).freeze()
end

fun freevars-v-acc(v :: AVal, seen-so-far :: NameDict<A.Name>) -> NameDict<A.Name>:
  cases(AVal) v block:
    | a-id(_, id) =>
      seen-so-far.set-now(id.key(), id)
      seen-so-far
    | a-id-var(_, id) =>
      seen-so-far.set-now(id.key(), id)
      seen-so-far
    | a-id-letrec(_, id, _) =>
      seen-so-far.set-now(id.key(), id)
      seen-so-far
    | a-id-safe-letrec(_, id) =>
      seen-so-far.set-now(id.key(), id)
      seen-so-far
    | a-srcloc(_, _) => seen-so-far
    | a-num(_, _) => seen-so-far
    | a-str(_, _) => seen-so-far
    | a-bool(_, _) => seen-so-far
    | a-undefined(_) => seen-so-far
    | else => raise("Unknown AVal in freevars-v " + torepr(v))
  end
end

fun freevars-v(v :: AVal) -> FrozenNameDict<A.Name>:
  freevars-v-acc(v, empty-dict()).freeze()
end

fun freevars-prog(p :: AProg) -> FrozenNameDict<A.Name>:
  cases(AProg) p block:
    | a-program(l, _, imports, body) =>
      body-vars = freevars-e-acc(body, empty-dict())
      for each(i from imports):
        for each(n from i.values + i.types):
          body-vars.remove-now(n.key())
        end
      end
      body-vars.freeze()
  end
end

