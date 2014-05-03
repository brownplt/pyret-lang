#lang pyret

provide *
import ast as A
import pprint as PP
import srcloc as SL

INDENT = 2

break-one = PP.break(1)
str-method = PP.str(" method")
str-letrec = PP.str("letrec ")
str-period = PP.str(".")
str-bang = PP.str("!")
str-colon = PP.str(":")
str-coloncolon = PP.str("::")
str-colonspace = PP.str(":")
str-end = PP.str("end")
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

dummy-loc = SL.builtin("dummy-location")

Loc = SL.Srcloc

data AProg:
  | a-program(l :: Loc, imports :: List<AHeader>, body :: AExpr) with:
    label(self): "a-program" end,
    tosource(self):
      PP.group(
        PP.flow-map(PP.hardline, fun(i): i.tosource() end, self.imports)
          + PP.hardline
          + self.body.tosource()
        )
    end
sharing:
  visit(self, visitor):
    self._match(visitor, fun(): raise("No visitor field for " + self.label()) end)
  end
end

data AImport:
  | a-import-file(l :: Loc, file :: String, name :: Name) with:
    label(self): "a-import-file" end,
    tosource(self):
      PP.flow([str-import, PP.dquote(PP.str(self.file)), str-as, self.name.tosource()])
    end
  | a-import-builtin(l :: Loc, lib :: String, name :: Name) with:
    label(self): "a-import-builtin" end,
    tosource(self):
      PP.flow([str-import, PP.str(self.lib), str-as, self.name.tosource()])
    end
sharing:
  visit(self, visitor):
    self._match(visitor, fun(): raise("No visitor field for " + self.label()) end)
  end
end

data AExpr:
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
  | a-tail-app(l :: Loc, f :: AVal, args :: List<AVal>) with:
    label(self): "a-tail-app" end,
    tosource(self):
      PP.group(self.f.tosource()
          + PP.parens(PP.nest(INDENT,
            PP.separate(PP.commabreak, self.args.map(fun(f): f.tosource() end)))))
    end
  | a-split-app(l :: Loc, is-var :: Boolean, f :: AVal, args :: List<AVal>, helper :: Name, helper-args :: List<AVal>) with:
    label(self): "a-split-app" end,
    tosource(self):
      PP.group(
        PP.group(PP.nest(INDENT,
            PP.str("split ")
              + PP.group(self.helper-args.first.tosource() + PP.str(" <== ") + self.f.tosource()
                + PP.parens(PP.nest(INDENT,
                PP.separate(PP.commabreak, self.args.map(fun(f): f.tosource() end))))))) +
        break-one +
        PP.group(PP.nest(INDENT, PP.str("and then") + break-one
              + self.helper.tosource()
              + PP.parens(PP.nest(INDENT,
                PP.separate(PP.commabreak, self.helper-args.map(fun(f): f.tosource() end)))))))
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
  | a-lettable(e :: ALettable) with:
    label(self): "a-lettable" end,
    tosource(self):
      self.e.tosource()
    end
sharing:
  visit(self, visitor):
    self._match(visitor, fun(): raise("No visitor field for " + self.label()) end)
  end
end

data ABind:
  | a-bind(l :: Loc, id :: Name, ann :: A.Ann) with:
    label(self): "a-bind" end,
    tosource(self):
      if A.is-a-blank(self.ann): self.id.tosource()
      else: PP.infix(INDENT, 1, str-coloncolon, self.id.tosource(), self.ann.tosource())
      end
    end
sharing:
  visit(self, visitor):
    self._match(visitor, fun(): raise("No visitor field for " + self.label()) end)
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
    self._match(visitor, fun(): raise("No visitor field for " + self.label()) end)
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
    self._match(visitor, fun(): raise("No visitor field for " + self.label()) end)
  end
end


data ALettable:
  | a-data-expr(l :: Loc, name :: String, variants :: List<AVariant>, shared :: List<AField>) with:
    label(self): "a-data-expr" end,
    tosource(self):
      PP.str("data-expr")
    end
  | a-assign(l :: Loc, id :: Name, value :: AVal) with:
    label(self): "a-assign" end,
    tosource(self):
      PP.group(PP.nest(INDENT, self.id.tosource() + str-spacecolonequal + break-one + self.value.tosource()))
    end
  | a-app(l :: Loc, _fun :: AVal, args :: List<AVal>) with:
    label(self): "a-app" end,
    tosource(self):
      PP.group(self._fun.tosource()
          + PP.parens(PP.nest(INDENT,
            PP.separate(PP.commabreak, self.args.map(fun(f): f.tosource() end)))))
    end
  | a-prim-app(l :: Loc, f :: String, args :: List<AVal>) with:
    label(self): "a-prim-app" end,
    tosource(self):
      PP.group(PP.str(self.f) +
          PP.parens(PP.nest(INDENT,
            PP.separate(PP.commabreak, self.args.map(fun(f): f.tosource() end)))))
    end
  | a-obj(l :: Loc, fields :: List<AField>) with:
    label(self): "a-obj" end,
    tosource(self):
      PP.surround-separate(INDENT, 1, PP.lbrace + PP.rbrace,
        PP.lbrace, PP.commabreak, PP.rbrace, self.fields.map(fun(f): f.tosource() end))
    end
  | a-update(l :: Loc, super :: AVal, fields :: List<AField>) with:
    label(self): "a-update" end,
    tosource(self):
      PP.str("update")
    end
  | a-extend(l :: Loc, super :: AVal, fields :: List<AField>) with:
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
  | a-lam(l :: Loc, args :: List<ABind>, body :: AExpr) with:
    label(self): "a-lam" end,
    tosource(self): fun-method-pretty(PP.str("lam"), self.args, self.body) end
  | a-method(l :: Loc, args :: List<ABind>, body :: AExpr) with:
    label(self): "a-method" end,
    tosource(self): fun-method-pretty(PP.str("method"), self.args, self.body) end
  | a-val(v :: AVal) with:
    label(self): "a-val" end,
    tosource(self): self.v.tosource() end
sharing:
  visit(self, visitor):
    self._match(visitor, fun(): raise("No visitor field for " + self.label()) end)
  end
end

fun fun-method-pretty(typ, args, body):
  arg-list = PP.nest(INDENT,
    PP.surround-separate(INDENT, 0, PP.lparen + PP.rparen, PP.lparen, PP.commabreak, PP.rparen,
      args.map(fun(a): a.tosource() end)))
  header = PP.group(typ + arg-list + str-colon)
  PP.surround(INDENT, 1, header, body.tosource(), str-end)
end

data AField:
  | a-field(l :: Loc, name :: String, value :: AVal) with:
    label(self): "a-field" end,
    tosource(self): PP.nest(INDENT, PP.str(self.name) + str-colonspace + self.value.tosource()) end,
sharing:
  visit(self, visitor):
    self._match(visitor, fun(): raise("No visitor field for " + self.label()) end)
  end
end

data AVal:
  | a-num(l :: Loc, n :: Number) with:
    label(self): "a-num" end,
    tosource(self): PP.number(self.n) end
  | a-str(l :: Loc, s :: String) with:
    label(self): "a-str" end,
    tosource(self): PP.squote(PP.str(self.s)) end
  | a-bool(l :: Loc, b :: Bool) with:
    label(self): "a-bool" end,
    tosource(self): PP.str(tostring(self.b)) end
  # used for letrec
  | a-undefined(l :: Loc) with:
    label(self): "a-undefined" end,
    tosource(self): PP.str("UNDEFINED") end
  | a-id(l :: Loc, id :: Name) with:
    label(self): "a-id" end,
    tosource(self): PP.str(self.id.tostring()) end
  | a-id-var(l :: Loc, id :: Name) with:
    label(self): "a-id-var" end,
    tosource(self): PP.str("!" + self.id.tostring()) end
  | a-id-letrec(l :: Loc, id :: Name) with:
    label(self): "a-id-letrec" end,
    tosource(self): PP.str("~" + self.id.tostring()) end
sharing:
  visit(self, visitor):
    self._match(visitor, fun(): raise("No visitor field for " + self.label()) end)
  end
end

fun strip-loc-prog(p :: AProg):
  cases(AProg) p:
    | a-program(_, imports, body) =>
      a-program(dummy-loc, imports.map(strip-loc-header), body^strip-loc-expr())
  end
end

fun strip-loc-header(h :: AHeader):
  cases(AHeader) h:
    | a-import-builtin(_, name, id) => a-import-builtin(dummy-loc, name, id)
    | a-import-file(_, file, id) => a-import-builtin(dummy-loc, file, id)
  end
end

fun strip-loc-expr(expr :: AExpr):
  cases(AExpr) expr:
    | a-let(_, bind, val, body) =>
      a-let(dummy-loc, bind^strip-loc-bind(), val^strip-loc-lettable(), body^strip-loc-expr())
    | a-var(_, bind, val, body) =>
      a-var(dummy-loc, bind^strip-loc-bind(), val^strip-loc-lettable(), body^strip-loc-expr())
    | a-seq(_, e1, e2) =>
      a-seq(dummy-loc, e1^strip-loc-lettable(), e2^strip-loc-expr())
    | a-if(_, c, t, e) =>
      a-if(dummy-loc, c^strip-loc-val(), t^strip-loc-expr(), e^strip-loc-expr())
    | a-split-app(_, is-var, f, args, helper, helper-args) =>
      a-split-app(
          dummy-loc,
          is-var,
          f^strip-loc-val(),
          args.map(strip-loc-val),
          helper,
          helper-args.map(strip-loc-val)
        )
    | a-lettable(e) =>
      a-lettable(e^strip-loc-lettable())
  end
end

fun strip-loc-bind(bind :: ABind):
  cases(ABind) bind:
    | a-bind(_, id, ann) => a-bind(dummy-loc, id, ann)
  end
end

fun strip-loc-lettable(lettable :: ALettable):
  cases(ALettable) lettable:
    | a-assign(_, id, value) => a-assign(dummy-loc, id, value^strip-loc-val())
    | a-app(_, f, args) =>
      a-app(dummy-loc, f^strip-loc-val(), args.map(strip-loc-val))
    | a-prim-app(_, f, args) =>
      a-prim-app(dummy-loc, f, args.map(strip-loc-val))
    | a-obj(_, fields) => a-obj(dummy-loc, fields.map(strip-loc-field))
    | a-update(_, super, fields) =>
      a-update(_, super^strip-loc-val(), fields.map(strip-loc-field))
    | a-extend(_, super, fields) =>
      a-extend(_, super^strip-loc-val(), fields.map(strip-loc-field))
    | a-dot(_, obj, field) =>
      a-dot(dummy-loc, obj^strip-loc-val(), field)
    | a-colon(_, obj, field) =>
      a-colon(dummy-loc, obj^strip-loc-val(), field)
    | a-get-bang(_, obj, field) =>
      a-get-bang(dummy-loc, obj^strip-loc-val(), field)
    | a-lam(_, args, body) =>
      a-lam(dummy-loc, args, body^strip-loc-expr())
    | a-method(_, args, body) =>
      a-method(dummy-loc, args, body^strip-loc-expr())
    | a-val(v) =>
      a-val(v^strip-loc-val())
  end
end

fun strip-loc-field(field :: AField):
  cases(AField) field:
    | a-field(_, name, value) => a-field(dummy-loc, name, value^strip-loc-val())
  end
end

fun strip-loc-val(val :: AVal):
  cases(AVal) val:
    | a-num(_, n) => a-num(dummy-loc, n)
    | a-str(_, s) => a-str(dummy-loc, s)
    | a-bool(_, b) => a-bool(dummy-loc, b)
    | a-undefined(_) => a-undefined(dummy-loc)
    | a-id(_, id) => a-id(dummy-loc, id)
    | a-id-var(_, id) => a-id-var(dummy-loc, id)
    | a-id-letrec(_, id) => a-id-letrec(dummy-loc, id)
  end
end

default-map-visitor = {
  a-program(self, l :: Loc, imports :: List<AHeader>, body :: AExpr):
    a-program(l, imports.map(_.visit(self)), body.visit(self))
  end,
  a-import-file(self, l :: Loc, file :: String, name :: Name):
    a-import-file(l, file, name)
  end,
  a-import-builtin(self, l :: Loc, lib :: String, name :: Name):
    a-import-builtin(l, lib, name)
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
  a-data-expr(self, l :: Loc, name :: String, variants :: List<AVariant>, shared :: List<AField>):
    a-data-expr(l, name, variants.map(_.visit(self)), shared.map(_.visit(self)))
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
  a-tail-app(self, l :: Loc, _fun :: AVal, args :: List<AVal>):
    a-tail-app(l, _fun.visit(self), args.map(_.visit(self)))
  end,
  a-split-app(self, l :: Loc, is-var :: Boolean, f :: AVal, args :: List<AVal>, helper :: String, helper-args :: List<AVal>):
    a-split-app(l, is-var, f.visit(self), args.map(_.visit(self)), helper, helper-args.map(_.visit(self)))
  end,
  a-if(self, l :: Loc, c :: AVal, t :: AExpr, e :: AExpr):
    a-if(l, c.visit(self), t.visit(self), e.visit(self))
  end,
  a-lettable(self, e :: ALettable):
    a-lettable(e.visit(self))
  end,
  a-assign(self, l :: Loc, id :: Name, value :: AVal):
    a-assign(l, id, value.visit(self))
  end,
  a-app(self, l :: Loc, _fun :: AVal, args :: List<AVal>):
    a-app(l, _fun.visit(self), args.map(_.visit(self)))
  end,
  a-prim-app(self, l :: Loc, f :: String, args :: List<AVal>):
    a-prim-app(l, f, args.map(_.visit(self)))
  end,
  a-obj(self, l :: Loc, fields :: List<AField>):
    a-obj(l, fields.map(_.visit(self)))
  end,
  a-update(self, l :: Loc, super :: AVal, fields :: List<AField>):
    a-update(l, super.visit(self), fields.map(_.visit(self)))
  end,
  a-extend(self, l :: Loc, super :: AVal, fields :: List<AField>):
    a-extend(l, super.visit(self), fields.map(_.visit(self)))
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
  a-lam(self, l :: Loc, args :: List<ABind>, body :: AExpr):
    a-lam(l, args.map(_.visit(self)), body.visit(self))
  end,
  a-method(self, l :: Loc, args :: List<ABind>, body :: AExpr):
    a-method(l, args.map(_.visit(self)), body.visit(self))
  end,
  a-val(self, v :: AVal):
    a-val(v.visit(self))
  end,
  a-bind(self, l :: Loc, id :: Name, ann :: A.Ann):
    a-bind(l, id, ann)
  end,
  a-field(self, l :: Loc, name :: String, value :: AVal):
    a-field(l, name, value.visit(self))
  end,
  a-num(self, l :: Loc, n :: Number):
    a-num(l, n)
  end,
  a-str(self, l :: Loc, s :: String):
    a-str(l, s)
  end,
  a-bool(self, l :: Loc, b :: Bool):
    a-bool(l, b)
  end,
  a-undefined(self, l :: Loc):
    a-undefined(l)
  end,
  a-id(self, l :: Loc, id :: Name):
    a-id(l, id)
  end,
  a-id-var(self, l :: Loc, id :: Name):
    a-id-var(l, id)
  end,
  a-id-letrec(self, l :: Loc, id :: Name):
    a-id-letrec(l, id)
  end
}

fun freevars-e-acc(expr :: AExpr, seen-so-far :: Set<Name>) -> Set<Name>:
  cases(AExpr) expr:
    | a-let(_, b, e, body) =>
      from-body = freevars-e-acc(body, seen-so-far)
      freevars-l-acc(e, from-body.remove(b.id))
    | a-var(_, b, e, body) =>
      from-body = freevars-e-acc(body, seen-so-far)
      freevars-l-acc(e, from-body.remove(b.id))
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

fun freevars-e(expr :: AExpr) -> Set<Name>:
  freevars-e-acc(expr, set([]))
where:
  d = dummy-loc
  freevars-e(
      a-let(d, a-bind(d, "x", A.a-blank), a-val(a-num(d, 4)),
        a-lettable(a-val(a-id(d, "y"))))).to-list() is ["y"]
end

fun freevars-variant-acc(v :: AVariant, seen-so-far :: Set<Name>) -> Set<Name>:
  for fold(acc from seen-so-far, member from v.with-members):
    freevars-v-acc(member, acc)
  end
end

fun freevars-l-acc(e :: ALettable, seen-so-far :: Set<Name>) -> Set<Name>:
  cases(ALettable) e:
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
    | a-lam(_, args, body) =>
      from-body = freevars-e-acc(body, seen-so-far)
      from-body.difference(set(args.map(_.id)))
    | a-method(_, args, body) =>
      from-body = freevars-e-acc(body, seen-so-far)
      from-body.difference(set(args.map(_.id)))
    | a-obj(_, fields) =>
      for fold(acc from seen-so-far, f from fields):
        freevars-v-acc(f.value, acc)
      end
    | a-update(_, super, fields) =>
      from-super = freevars-v-acc(super, seen-so-far)
      for fold(acc from from-super, f from fields):
        freevars-v-acc(f.value, acc)
      end
    | a-data-expr(_, _, variants, shared) =>
      from-variants = for fold(acc from seen-so-far, v from variants):
        freevars-variant-acc(v, acc)
      end
      for fold(acc from from-variants, s from shared):
        freevars-v-acc(s.value, acc)
      end
    | a-extend(_, super, fields) =>
      from-super = freevars-v-acc(super, seen-so-far)
      for fold(acc from from-super, f from fields):
        freevars-v-acc(f.value, acc)
      end
    | a-dot(_, obj, _) => freevars-v-acc(obj, seen-so-far)
    | a-colon(_, obj, _) => freevars-v-acc(obj, seen-so-far)
    | a-get-bang(_, obj, _) => freevars-v-acc(obj, seen-so-far)
    | a-val(v) => freevars-v-acc(v, seen-so-far)
    | else => raise("Non-lettable in freevars-l " + torepr(e))
  end
end

fun freevars-l(e :: ALettable) -> Set<Name>:
  freevars-l-acc(e, set([]))
end

fun freevars-v-acc(v :: AVal, seen-so-far :: Set<Name>) -> Set<Name>:
  cases(AVal) v:
    | a-id(_, id) => seen-so-far.add(id)
    | a-id-var(_, id) => seen-so-far.add(id)
    | a-id-letrec(_, id) => seen-so-far.add(id)
    | else => seen-so-far
  end
end

fun freevars-v(v :: AVal) -> Set<Name>:
  freevars-v-acc(v, set([]))
end

fun <a> unions(ss :: List<Set<a>>) -> Set<a>:
  for fold(unioned from set([]), s from ss):
    unioned.union(s)
  end
end

