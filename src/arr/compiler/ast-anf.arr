#lang pyret

provide *
provide-types *
import ast as A
import pprint as PP
import srcloc as SL
import string-dict as SD

type StringDict = SD.StringDict


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

data AProg:
  | a-program(l :: Loc, imports :: List<AImport>, body :: AExpr) with:
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
  | a-import-builtin(l :: Loc, lib :: String) with:
    tosource(self): PP.str(self.lib) end
  | a-import-file(l :: Loc, file :: String) with:
    tosource(self): PP.dquote(PP.str(self.file)) end
  | a-import-special(l :: Loc, kind :: String, args :: List<String>) with:
    tosource(self): 
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
    label(self): "a-import-complete" end,
    tosource(self):
      PP.flow([list: str-import,
          PP.flow-map(PP.commabreak, _.tosource(), self.values + self.types),
          str-from,
          self.import-type.tosource(),
          str-as,
          self.vals-name.tosource(),
          self.types-name.tosource()])
    end
sharing:
  visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end
end

data ATypeBind:
  | a-type-bind(l :: Loc, name :: A.Name, ann :: A.Ann) with:
    label(self): "a-type-bind" end,
    tosource(self): PP.infix(INDENT, 1, str-coloncolon, self.name.tosource(), self.ann.tosource()) end
  | a-newtype-bind(l :: Loc, name :: A.Name, namet :: A.Name) with:
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
  | a-type-let(l :: Loc, bind :: ATypeBind, body :: AExpr) with:
    label(self): "a-type-let" end,
    tosource(self):
      PP.soft-surround(INDENT, 1,
        str-type-let +
        PP.group(PP.nest(INDENT,
            self.bind.tosource())) + str-colon,
        self.body.tosource(),
        str-end)
    end
  | a-let(l :: Loc, bind :: ABind, e :: ALettable, body :: AExpr) with:
    label(self): "a-let" end,
    tosource(self):
      PP.soft-surround(INDENT, 1,
        str-let +
        PP.group(PP.nest(INDENT,
            self.bind.tosource() + str-spaceequal + break-one + self.e.tosource())) + str-colon,
        self.body.tosource(),
        str-end)
    end
  | a-var(l :: Loc, bind :: ABind, e :: ALettable, body :: AExpr) with:
    label(self): "a-var" end,
    tosource(self):
      PP.soft-surround(INDENT, 1,
        str-var +
        PP.group(PP.nest(INDENT,
            self.bind.tosource() + str-spaceequal + break-one + self.e.tosource())) + str-colon,
        self.body.tosource(),
        str-end)
    end
  | a-seq(l :: Loc, e1 :: ALettable, e2 :: AExpr) with:
    label(self): "a-seq" end,
    tosource(self):
      self.e1.tosource() + PP.hardline + self.e2.tosource()
    end
  | a-lettable(l :: Loc, e :: ALettable) with:
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
  | a-bind(l :: Loc, id :: A.Name, ann :: A.Ann) with:
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
      l :: Loc,
      constr-loc :: Loc,
      name :: String,
      members :: List<AVariantMember>,
      with-members :: List<AField>
    ) with:
    label(self): "a-variant" end,
    tosource(self): PP.str("a-variant") end
  | a-singleton-variant(
      l :: Loc,
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
  | a-mutable with:
    label(self): "a-mutable" end,
    tosource(self): PP.str("mutable ") end
end

data AVariantMember:
  | a-variant-member(
      l :: Loc,
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

data ACasesBind:
  | a-cases-bind(l :: Loc, field-type :: A.CasesBindType, bind :: ABind) with:
    label(self): "s-cases-bind" end,
    tosource(self):
      self.field-type.tosource() + PP.str(" ") + self.bind.tosource()
    end
sharing:
  visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end
end


data ACasesBranch:
  | a-cases-branch(l :: Loc, pat-loc :: Loc, name :: String, args :: List<ABind>, body :: AExpr) with:
    label(self): "a-cases-branch" end,
    tosource(self):
      PP.nest(INDENT,
        PP.group(PP.str("| " + self.name)
            + PP.surround-separate(INDENT, 0, PP.str("()"), PP.lparen, PP.commabreak, PP.rparen,
            self.args.map(lam(a): a.tosource() end)) + break-one + str-thickarrow) + break-one +
        self.body.tosource())
    end
  | a-singleton-cases-branch(l :: Loc, pat-loc :: Loc, name :: String, body :: AExpr) with:
    label(self): "a-singleton-cases-branch" end,
    tosource(self):
      PP.nest(INDENT,
        PP.group(PP.str("| " + self.name) + break-one + str-thickarrow) + break-one + self.body.tosource())
    end
sharing:
  visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end
end

data ALettable:
  | a-module(l :: Loc, answer :: AVal, provides :: AVal, types, checks :: AVal) with:
    label(self): "a-module" end,
    tosource(self):
      PP.str("Module") + PP.parens(PP.flow-map(PP.commabreak, lam(x): x end, [list:
            PP.infix(INDENT, 1, str-colon, PP.str("Answer"), self.answer.tosource()),
            PP.infix(INDENT, 1, str-colon, PP.str("Provides"), self.provides.tosource()),
            PP.infix(INDENT, 1, str-colon, PP.str("Types"), 
              PP.brackets(PP.flow-map(PP.commabreak, _.tosource(), self.types))),
            PP.infix(INDENT, 1, str-colon, PP.str("checks"), self.checks.tosource())]))
    end
  | a-cases(l :: Loc, typ :: A.Ann, val :: AVal, branches :: List<ACasesBranch>, _else :: AExpr) with:
    label(self): "a-cases" end,
    tosource(self):
      header = str-cases + PP.parens(self.typ.tosource()) + break-one
        + self.val.tosource() + str-colon
      body = PP.separate(break-one, self.branches.map(lam(b): PP.group(b.tosource()) end))
        + break-one + PP.group(str-elsebranch + break-one + self._else.tosource())
      PP.surround(INDENT, 1, PP.group(header), body, str-end)
    end
  | a-if(l :: Loc, c :: AVal, t :: AExpr, e :: AExpr) with:
    label(self): "a-if" end,
    tosource(self):
      PP.group(
        str-if + PP.nest(2 * INDENT, self.c.tosource() + str-colon)
          + PP.nest(INDENT, break-one + self.t.tosource())
          + break-one + str-elsecolon
          + PP.nest(INDENT, break-one + self.e.tosource())
          + break-one + str-end)
    end
  | a-data-expr(l :: Loc, name :: String, namet :: A.Name, variants :: List<AVariant>, shared :: List<AField>) with:
    label(self): "a-data-expr" end,
    tosource(self):
      PP.str("data-expr")
    end
  | a-assign(l :: Loc, id :: A.Name, value :: AVal) with:
    label(self): "a-assign" end,
    tosource(self):
      PP.group(PP.nest(INDENT, self.id.tosource() + str-spacecolonequal + break-one + self.value.tosource()))
    end
  | a-app(l :: Loc, _fun :: AVal, args :: List<AVal>) with:
    label(self): "a-app" end,
    tosource(self):
      PP.group(self._fun.tosource()
          + PP.parens(PP.nest(INDENT,
            PP.separate(PP.commabreak, self.args.map(lam(f): f.tosource() end)))))
    end
  | a-method-app(l :: Loc, obj :: AVal, meth :: String, args :: List<AVal>) with:
    label(self): "a-app" end,
    tosource(self):
      PP.group(self.obj.tosource() + PP.str("METHOD")
          + PP.parens(PP.nest(INDENT,
            PP.separate(PP.commabreak, self.args.map(lam(f): f.tosource() end)))))
    end
  | a-prim-app(l :: Loc, f :: String, args :: List<AVal>) with:
    label(self): "a-prim-app" end,
    tosource(self):
      PP.group(PP.str(self.f) +
          PP.parens(PP.nest(INDENT,
            PP.separate(PP.commabreak, self.args.map(lam(f): f.tosource() end)))))
    end
  | a-array(l :: Loc, values :: List<AVal>) with:
    label(self): "a-array" end,
    tosource(self):
      PP.surround-separate(INDENT, 0, PP.str("[raw-array: ]"), PP.str("[raw-array: "), PP.commabreak, PP.rbrack,
        self.values.map(_.tosource()))
    end
  | a-ref(l :: Loc, ann :: Option<A.Ann>) with:
    label(self): "a-ref" end,
    tosource(self):
      cases(Option) self.ann:
        | none => PP.str("bare-ref")
        | some(ann) =>
          PP.group(PP.str("ref ") + ann.tosource())
      end
    end
  | a-obj(l :: Loc, fields :: List<AField>) with:
    label(self): "a-obj" end,
    tosource(self):
      PP.surround-separate(INDENT, 1, PP.lbrace + PP.rbrace,
        PP.lbrace, PP.commabreak, PP.rbrace, self.fields.map(lam(f): f.tosource() end))
    end
  | a-update(l :: Loc, supe :: AVal, fields :: List<AField>) with:
    label(self): "a-update" end,
    tosource(self):
      PP.str("update")
    end
  | a-extend(l :: Loc, supe :: AVal, fields :: List<AField>) with:
    label(self): "a-extend" end,
    tosource(self):
      PP.str("extend")
    end
  | a-dot(l :: Loc, obj :: AVal, field :: String) with:
    label(self): "a-dot" end,
    tosource(self): PP.infix(INDENT, 0, str-period, self.obj.tosource(), PP.str(self.field)) end
  | a-colon(l :: Loc, obj :: AVal, field :: String) with:
    label(self): "a-colon" end,
    tosource(self): PP.infix(INDENT, 0, str-colon, self.obj.tosource(), PP.str(self.field)) end
  | a-get-bang(l :: Loc, obj :: AVal, field :: String) with:
    label(self): "a-get-bang" end,
    tosource(self): PP.infix(INDENT, 0, str-bang, self.obj.tosource(), PP.str(self.field)) end
  | a-lam(l :: Loc, args :: List<ABind>, ret :: A.Ann, body :: AExpr) with:
    label(self): "a-lam" end,
    tosource(self): fun-method-pretty(PP.str("lam"), self.args, self.body) end
  | a-method(l :: Loc, args :: List<ABind>, ret :: A.Ann, body :: AExpr) with:
    label(self): "a-method" end,
    tosource(self): fun-method-pretty(PP.str("method"), self.args, self.body) end
  | a-val(l :: Loc, v :: AVal) with:
    label(self): "a-val" end,
    tosource(self): self.v.tosource() end
sharing:
  visit(self, visitor):
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
    label(self): "a-field" end,
    tosource(self): PP.nest(INDENT, PP.str(self.name) + str-colonspace + self.value.tosource()) end,
sharing:
  visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end
end

data AVal:
  | a-srcloc(l :: Loc, loc :: Loc) with:
    label(self): "a-srcloc" end,
    tosource(self): PP.str(torepr(self.loc)) end
  | a-num(l :: Loc, n :: Number) with:
    label(self): "a-num" end,
    tosource(self): PP.number(self.n) end
  | a-str(l :: Loc, s :: String) with:
    label(self): "a-str" end,
    tosource(self): PP.str(torepr(self.s)) end
  | a-bool(l :: Loc, b :: Boolean) with:
    label(self): "a-bool" end,
    tosource(self): PP.str(tostring(self.b)) end
  # used for letrec
  | a-undefined(l :: Loc) with:
    label(self): "a-undefined" end,
    tosource(self): PP.str("UNDEFINED") end
  | a-id(l :: Loc, id :: A.Name) with:
    label(self): "a-id" end,
    tosource(self): PP.str(tostring(self.id)) end
  | a-id-var(l :: Loc, id :: A.Name) with:
    label(self): "a-id-var" end,
    tosource(self): PP.str("!" + tostring(self.id)) end
  | a-id-letrec(l :: Loc, id :: A.Name, safe :: Boolean) with:
    label(self): "a-id-letrec" end,
    tosource(self): PP.str("~" + tostring(self.id)) end
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
    | a-import-complete(_, vns, tns, imp, vn, tn) =>
      a-import-complete(dummy-loc, vns, tns, imp, vn, tn)
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
    | a-module(_, answer, provides, types, checks) =>
      a-module(dummy-loc, strip-loc-val(answer), strip-loc-val(provides),
        types.map(_.visit(A.dummy-loc-visitor)), strip-loc-val(checks))
    | a-if(_, c, t, e) =>
      a-if(dummy-loc, strip-loc-val(c), strip-loc-expr(t), strip-loc-expr(e))
    | a-assign(_, id, value) => a-assign(dummy-loc, id, strip-loc-val(value))
    | a-app(_, f, args) =>
      a-app(dummy-loc, strip-loc-val(f), args.map(strip-loc-val))
    | a-method-app(_, obj, meth, args) =>
      a-method-app(dummy-loc, strip-loc-val(obj), meth, args.map(strip-loc-val))
    | a-prim-app(_, f, args) =>
      a-prim-app(dummy-loc, f, args.map(strip-loc-val))
    | a-array(_, vs) => a-array(dummy-loc, vs.map(strip-loc-val))
    | a-ref(_, ann) => a-ref(dummy-loc, A.dummy-loc-visitor.option(ann))
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
    | a-id-var(_, id) => a-id-var(dummy-loc, id)
    | a-id-letrec(_, id, safe) => a-id-letrec(dummy-loc, id, safe)
  end
end

default-map-visitor = {
  a-module(self, l :: Loc, answer :: AVal, provides :: AVal, types :: List<A.AField>, checks :: AVal):
    a-module(l, answer.visit(self), provides.visit(self), types, checks.visit(self))
  end,
  a-program(self, l :: Loc, imports :: List<AImport>, body :: AExpr):
    a-program(l, imports.map(_.visit(self)), body.visit(self))
  end,
  a-import-file(self, l :: Loc, file :: String, name :: A.Name):
    a-import-file(l, file, name)
  end,
  a-import-builtin(self, l :: Loc, lib :: String, name :: A.Name):
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
  a-let(self, l :: Loc, bind :: ABind, e :: ALettable, body :: AExpr):
    a-let(l, bind.visit(self), e.visit(self), body.visit(self))
  end,
  a-var(self, l :: Loc, bind :: ABind, e :: ALettable, body :: AExpr):
    a-var(l, bind.visit(self), e.visit(self), body.visit(self))
  end,
  a-seq(self, l :: Loc, e1 :: ALettable, e2 :: AExpr):
    a-seq(l, e1.visit(self), e2.visit(self))
  end,
  a-cases(self, l :: Loc, typ :: A.Ann, val :: AVal, branches :: List<ACasesBranch>, _else :: AExpr):
    # NOTE: Not visiting the annotation yet
    a-cases(l, typ, val.visit(self), branches.map(_.visit(self)), _else.visit(self))
  end,
  a-cases-bind(self, l, typ, bind):
    a-cases-bind(l, typ, bind.visit(self))
  end,
  a-cases-branch(self, l :: Loc, pat-loc :: Loc, name :: String, args :: List<ABind>, body :: AExpr):
    a-cases-branch(l, pat-loc, name, args.map(_.visit(self)), body.visit(self))
  end,
  a-singleton-cases-branch(self, l :: Loc, pat-loc :: Loc, name :: String, body :: AExpr):
    a-singleton-cases-branch(l, pat-loc, name, body.visit(self))
  end,
  a-data-expr(self, l :: Loc, name :: String, namet :: A.Name, variants :: List<AVariant>, shared :: List<AField>):
    a-data-expr(l, name, namet, variants.map(_.visit(self)), shared.map(_.visit(self)))
  end,
  a-variant(self, l :: Loc, constr-loc :: Loc, name :: String, members :: List<AVariantMember>, with-members :: List<AField>):
    a-variant(l, constr-loc, name, members.map(_.visit(self)), with-members.map(_.visit(self)))
  end,
  a-singleton-variant(self, l :: Loc, name :: String, with-members :: List<AField>):
    a-singleton-variant(l, name, with-members.map(_.visit(self)))
  end,
  a-variant-member(self, l :: Loc, member-type :: AMemberType, bind :: ABind):
    a-variant-member(l, member-type, bind.visit(self))
  end,
  a-if(self, l :: Loc, c :: AVal, t :: AExpr, e :: AExpr):
    a-if(l, c.visit(self), t.visit(self), e.visit(self))
  end,
  a-lettable(self, l, e :: ALettable):
    a-lettable(l, e.visit(self))
  end,
  a-assign(self, l :: Loc, id :: A.Name, value :: AVal):
    a-assign(l, id, value.visit(self))
  end,
  a-app(self, l :: Loc, _fun :: AVal, args :: List<AVal>):
    a-app(l, _fun.visit(self), args.map(_.visit(self)))
  end,
  a-method-app(self, l :: Loc, obj :: AVal, meth :: String, args :: List<AVal>):
    a-method-app(l, obj.visit(self), meth, args.map(_.visit(self)))
  end,
  a-prim-app(self, l :: Loc, f :: String, args :: List<AVal>):
    a-prim-app(l, f, args.map(_.visit(self)))
  end,
  a-ref(self, l :: Loc, ann :: Option<A.Ann>):
    a-ref(l, ann)
  end,
  a-obj(self, l :: Loc, fields :: List<AField>):
    a-obj(l, fields.map(_.visit(self)))
  end,
  a-update(self, l :: Loc, supe :: AVal, fields :: List<AField>):
    a-update(l, supe.visit(self), fields.map(_.visit(self)))
  end,
  a-extend(self, l :: Loc, supe :: AVal, fields :: List<AField>):
    a-extend(l, supe.visit(self), fields.map(_.visit(self)))
  end,
  a-dot(self, l :: Loc, obj :: AVal, field :: String):
    a-dot(l, obj.visit(self), field)
  end,
  a-colon(self, l :: Loc, obj :: AVal, field :: String):
    a-colon(l, obj.visit(self), field)
  end,
  a-get-bang(self, l :: Loc, obj :: AVal, field :: String):
    a-get-bang(l, obj.visit(self), field)
  end,
  a-lam(self, l :: Loc, args :: List<ABind>, ret :: A.Ann, body :: AExpr):
    a-lam(l, args.map(_.visit(self)), ret, body.visit(self))
  end,
  a-method(self, l :: Loc, args :: List<ABind>, ret :: A.Ann, body :: AExpr):
    a-method(l, args.map(_.visit(self)), ret, body.visit(self))
  end,
  a-val(self, l, v :: AVal):
    a-val(l, v.visit(self))
  end,
  a-bind(self, l :: Loc, id :: A.Name, ann :: A.Ann):
    a-bind(l, id, ann)
  end,
  a-field(self, l :: Loc, name :: String, value :: AVal):
    a-field(l, name, value.visit(self))
  end,
  a-srcloc(self, l, loc):
    a-srcloc(l, loc)
  end,
  a-num(self, l :: Loc, n :: Number):
    a-num(l, n)
  end,
  a-array(self, l :: Loc, vals :: List<AVal>):
    a-array(l, vals.map(_.visit(self)))
  end,
  a-str(self, l :: Loc, s :: String):
    a-str(l, s)
  end,
  a-bool(self, l :: Loc, b :: Boolean):
    a-bool(l, b)
  end,
  a-undefined(self, l :: Loc):
    a-undefined(l)
  end,
  a-id(self, l :: Loc, id :: A.Name):
    a-id(l, id)
  end,
  a-id-var(self, l :: Loc, id :: A.Name):
    a-id-var(l, id)
  end,
  a-id-letrec(self, l :: Loc, id :: A.Name, safe :: Boolean):
    a-id-letrec(l, id, safe)
  end
}

fun freevars-list-acc(anns :: List<A.Ann>, seen-so-far):
  for fold(acc from seen-so-far, a from anns):
    freevars-ann-acc(a, acc)
  end
end

rec get-ann = _.ann

fun freevars-ann-acc(ann :: A.Ann, seen-so-far :: StringDict<A.Name>) -> StringDict<A.Name>:
  lst-a = freevars-list-acc(_, seen-so-far)
  cases(A.Ann) ann:
    | a-blank => seen-so-far
    | a-any => seen-so-far
    | a-name(l, name) => seen-so-far.set(name.key(), name)
    | a-type-var(l, name) => seen-so-far
    | a-dot(l, left, right) => seen-so-far.set(left.key(), left)
    | a-arrow(l, args, ret, _) => lst-a(link(ret, args))
    | a-method(l, args, ret) => lst-a(link(ret, args))
    | a-record(l, fields) => lst-a(fields.map(get-ann))
    | a-app(l, a, args) => lst-a(link(a, args))
    | a-method-app(l, a, _, args) => lst-a(link(a, args))
    | a-pred(l, a, pred) =>
      name = cases(A.Expr) pred:
        | s-id(_, n) => n
        | s-id-letrec(_, n, _) => n
      end
      freevars-ann-acc(a, seen-so-far.set(name.key(), name))
  end
end

fun freevars-e-acc(expr :: AExpr, seen-so-far :: StringDict<A.Name>) -> StringDict<A.Name>:
  cases(AExpr) expr:
    | a-type-let(_, b, body) =>
      body-ids = freevars-e-acc(body, seen-so-far)
      cases(ATypeBind) b:
        | a-type-bind(_, name, ann) =>
          freevars-ann-acc(ann, body-ids.remove(name.key()))
        | a-newtype-bind(_, name, nameb) =>
          body-ids.remove(name.key()).remove(nameb.key())
      end
    | a-let(_, b, e, body) =>
      from-body = freevars-e-acc(body, seen-so-far)
      freevars-ann-acc(b.ann, freevars-l-acc(e, from-body.remove(b.id.key())))
    | a-var(_, b, e, body) =>
      from-body = freevars-e-acc(body, seen-so-far)
      freevars-ann-acc(b.ann, freevars-l-acc(e, from-body.remove(b.id.key())))
    | a-seq(_, e1, e2) =>
      from-e2 = freevars-e-acc(e2, seen-so-far)
      freevars-l-acc(e1, from-e2)
    | a-lettable(_, e) => freevars-l-acc(e, seen-so-far)
  end
end

fun freevars-e(expr :: AExpr) -> StringDict<A.Name>:
  freevars-e-acc(expr, empty-dict)
where:
  d = dummy-loc
  n = A.global-names.make-atom
  x = n("x")
  y = n("y")
  freevars-e(
      a-let(d, a-bind(d, x, A.a-blank), a-val(d, a-num(d, 4)),
        a-lettable(d, a-val(d, a-id(d, y))))).keys().to-list() is [list: y.key()]
end

fun freevars-variant-acc(v :: AVariant, seen-so-far :: StringDict<A.Name>) -> StringDict<A.Name>:
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

rec get-id = _.id

fun freevars-branches-acc(branches :: List<ACasesBranch>, seen-so-far :: StringDict<A.Name>) -> StringDict<A.Name>:
  for fold(acc from seen-so-far, b from branches):
    cases(ACasesBranch) b:
      | a-cases-branch(_, _, _, args, body) =>
        from-body = freevars-e-acc(body, acc)
        shadow args = args.map(_.bind)
        without-args = for fold(without from from-body, arg from args.map(get-id)):
          without.remove(arg.key())
        end
        for fold(inner-acc from without-args, arg from args):
          freevars-ann-acc(arg.ann, inner-acc)
        end
      | a-singleton-cases-branch(_, _, _, body) =>
        freevars-e-acc(body, acc)
    end
  end
end
fun freevars-l-acc(e :: ALettable, seen-so-far :: StringDict<A.Name>) -> StringDict<A.Name>:
  cases(ALettable) e:
    | a-module(_, ans, provs, types, checks) =>
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
    | a-assign(_, id, v) => freevars-v-acc(v, seen-so-far.set(id.key(), id))
    | a-app(_, f, args) =>
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
    | a-lam(_, args, ret, body) =>
      from-body = freevars-e-acc(body, seen-so-far)
      without-args = for fold(without from from-body, arg from args.map(get-id)):
          without.remove(arg.key())
        end
      from-args = for fold(acc from without-args, a from args):
        freevars-ann-acc(a.ann, acc)
      end
      freevars-ann-acc(ret, from-args)
    | a-method(_, args, ret, body) =>
      from-body = freevars-e-acc(body, seen-so-far)
      without-args = for fold(without from from-body, arg from args.map(get-id)):
          without.remove(arg.key())
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
      from-shared.set(namet.key(), namet)
    | a-extend(_, supe, fields) =>
      from-supe = freevars-v-acc(supe, seen-so-far)
      for fold(acc from from-supe, f from fields):
        freevars-v-acc(f.value, acc)
      end
    | a-dot(_, obj, _) => freevars-v-acc(obj, seen-so-far)
    | a-colon(_, obj, _) => freevars-v-acc(obj, seen-so-far)
    | a-get-bang(_, obj, _) => freevars-v-acc(obj, seen-so-far)
    | a-val(_, v) => freevars-v-acc(v, seen-so-far)
    | else => raise("Non-lettable in freevars-l " + torepr(e))
  end
end

fun freevars-l(e :: ALettable) -> StringDict<A.Name>:
  freevars-l-acc(e, empty-dict)
end

fun freevars-v-acc(v :: AVal, seen-so-far :: StringDict<A.Name>) -> StringDict<A.Name>:
  cases(AVal) v:
    | a-id(_, id) => seen-so-far.set(id.key(), id)
    | a-id-var(_, id) => seen-so-far.set(id.key(), id)
    | a-id-letrec(_, id, _) => seen-so-far.set(id.key(), id)
    | a-srcloc(_, _) => seen-so-far
    | a-num(_, _) => seen-so-far
    | a-str(_, _) => seen-so-far
    | a-bool(_, _) => seen-so-far
    | a-undefined(_) => seen-so-far
    | else => raise("Unknown AVal in freevars-v " + torepr(v))
  end
end

fun freevars-v(v :: AVal) -> StringDict<A.Name>:
  freevars-v-acc(v, empty-dict)
end

rec empty-dict = [SD.string-dict:]

