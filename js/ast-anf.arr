#lang pyret

provide *
import ast as A
import pprint as PP

INDENT = 2

break-one = PP.break(1)
str-method = PP.str(" method")
str-letrec = PP.str("letrec ")
str-period = PP.str(".")
str-bang = PP.str("!")
str-colon = PP.str(":")
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

dummy-loc = error.location("dummy-location", -1, -1)

Loc = error.Location

data AProg:
  | a-program(l :: Loc, imports :: List<AImport>, body :: AExpr) with:
    tosource(self):
      PP.group(
        PP.flow_map(PP.hardline, fun(i): i.tosource() end, self.imports)
          + PP.hardline
          + self.body.tosource()
        )
    end
end

data AImport:
  | a-import-file(l :: Loc, file :: String, name :: String)
  | a-import-builtin(l :: Loc, lib :: String, name :: String)
  | a-provide(l :: Loc, val :: AExpr)
end

data AExpr:
  | a-let(l :: Loc, bind :: ABind, e :: ALettable, body :: AExpr) with:
    tosource(self):
      PP.soft-surround(INDENT, 1,
        str-let + break-one + self.bind.tosource() + str-spaceequal + self.e.tosource() + str-colon,
        self.body.tosource(),
        str-end)
    end
  | a-var(l :: Loc, bind :: ABind, e :: ALettable, body :: AExpr) with:
    tosource(self):
      PP.soft-surround(INDENT, 1,
        str-var + break-one + self.bind.tosource() + str-spaceequal + self.e.tosource() + str-colon,
        self.body.tosource(),
        str-end)
    end
  | a-try(l :: Loc, body :: AExpr, b :: ABind, _except :: AExpr) with:
    tosource(self):
      _try = str-try + break-one
        + PP.nest(INDENT, self.body.tosource()) + break-one
      _except = str-except + PP.parens(self.b.tosource()) + str-colon + break-one
        + PP.nest(INDENT, self._except.tosource()) + break-one
      PP.group(_try + _except + str-end)
    end
  | a-split-app(l :: Loc, is-var :: Boolean, f :: AVal, args :: List<AVal>, helper :: String, helper-args :: List<AVal>) with:
    tosource(self):
      PP.group(self.f.tosource()
          + PP.parens(PP.nest(INDENT,
            PP.separate(PP.commabreak, self.args.map(fun(f): f.tosource() end))))) +
        PP.str("and then... ") +
      PP.group(PP.str(self.helper) +
          PP.parens(PP.nest(INDENT,
            PP.separate(PP.commabreak, self.helper-args.map(fun(f): f.tosource() end)))))
    end
  | a-lettable(e :: ALettable) with:
    tosource(self):
      self.e.tosource()
    end
end

data ABind:
  | a-bind(l :: Loc, id :: String, ann :: A.Ann) with:
    tosource(self): PP.str(self.id) end
end

data ALettable:
  | a-assign(l :: Loc, id :: String, value :: AVal) with:
    tosource(self):
      PP.nest(INDENT, PP.str(self.id) + str-spacecolonequal + break-one + self.value.tosource())
    end
  | a-app(l :: Loc, _fun :: AVal, args :: List<AVal>) with:
    tosource(self):
      PP.group(self._fun.tosource()
          + PP.parens(PP.nest(INDENT,
            PP.separate(PP.commabreak, self.args.map(fun(f): f.tosource() end)))))
    end
  | a-help-app(l :: Loc, f :: String, args :: List<AVal>) with:
    tosource(self):
      PP.group(PP.str(self.f) +
          PP.parens(PP.nest(INDENT,
            PP.separate(PP.commabreak, self.args.map(fun(f): f.tosource() end)))))
    end
  | a-obj(l :: Loc, fields :: List<AField>) with:
    tosource(self):
      PP.surround-separate(INDENT, 1, PP.lbrace + PP.rbrace,
        PP.lbrace, PP.commabreak, PP.rbrace, self.fields.map(fun(f): f.tosource() end))
    end
  | a-update(l :: Loc, super :: AVal, fields :: List<AField>) with:
    tosource(self):
      PP.str("update")
    end
  | a-extend(l :: Loc, super :: AVal, fields :: List<AField>) with:
    tosource(self):
      PP.str("extend")
    end
  | a-dot(l :: Loc, obj :: AVal, field :: String) with:
    tosource(self): PP.infix(INDENT, 0, str-period, self.obj.tosource(), PP.str(self.field)) end
  | a-colon(l :: Loc, obj :: AVal, field :: String) with:
    tosource(self): PP.infix(INDENT, 0, str-colon, self.obj.tosource(), PP.str(self.field)) end
  | a-get-bang(l :: Loc, obj :: AVal, field :: String) with:
    tosource(self): PP.infix(INDENT, 0, str-bang, self.obj.tosource(), PP.str(self.field)) end
  | a-lam(l :: Loc, args :: List<ABind>, body :: AExpr) with:
    tosource(self): fun-method-pretty(PP.str("lam"), self.args, self.body) end
  | a-method(l :: Loc, args :: List<ABind>, body :: AExpr) with:
    tosource(self): fun-method-pretty(PP.str("method"), self.args, self.body) end
  | a-if(l :: Loc, c :: AVal, t :: AExpr, e :: AExpr) with:
    tosource(self):
      str-if + break-one + self.c.tosource() + str-colon +
          PP.nest(INDENT, break-one + self.t.tosource()) +
        str-elsecolon
          PP.nest(INDENT, break-one + self.e.tosource())
    end
  | a-val(v :: AVal) with:
    tosource(self): self.v.tosource() end
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
    tosource(self): PP.nest(INDENT, PP.str(self.name) + str-colonspace + self.value.tosource()) end,
end

data AVal:
  | a-num(l :: Loc, n :: Number) with:
    tosource(self): PP.number(self.n) end
  | a-str(l :: Loc, s :: String) with:
    tosource(self): PP.squote(PP.str(self.s)) end
  | a-bool(l :: Loc, b :: Bool) with:
    tosource(self): PP.str(self.b.tostring()) end
  # used for letrec
  | a-undefined(l :: Loc) with:
    tosource(self): PP.str("UNDEFINED") end
  | a-id(l :: Loc, id :: String) with:
    tosource(self): PP.str(self.id) end
  | a-id-var(l :: Loc, id :: String) with:
    tosource(self): PP.str("!" + self.id) end
  | a-id-letrec(l :: Loc, id :: String) with:
    tosource(self): PP.str("~" + self.id) end
end

fun strip-loc-prog(p :: AProg):
  cases(AProg) p:
    | a-program(_, imports, body) =>
      a-program(dummy-loc, imports.map(strip-loc-header), body^strip-loc-expr())
  end
end

fun strip-loc-header(h :: AImport):
  cases(AImport) h:
    | a-import-builtin(_, name, id) => a-import-builtin(dummy-loc, name, id)
    | a-import-file(_, file, id) => a-import-builtin(dummy-loc, file, id)
    | a-provide(_, val) => a-provide(dummy-loc, val)
  end
end

fun strip-loc-expr(expr :: AExpr):
  cases(AExpr) expr:
    | a-let(_, bind, val, body) =>
      a-let(dummy-loc, bind^strip-loc-bind(), val^strip-loc-lettable(), body^strip-loc-expr())
    | a-var(_, bind, val, body) =>
      a-var(dummy-loc, bind^strip-loc-bind(), val^strip-loc-lettable(), body^strip-loc-expr())
    | a-try(_, body, bind, _except) =>
      a-try(dummy-loc, body^strip-loc-expr(), bind^strip-loc-bind(), _except^strip-loc-expr())
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
    | a-help-app(_, f, args) =>
      a-help-app(dummy-loc, f, args.map(strip-loc-val))
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
    | a-if(_, c, t, e) =>
      a-if(dummy-loc, c^strip-loc-val(), t^strip-loc-expr(), e^strip-loc-expr())
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

