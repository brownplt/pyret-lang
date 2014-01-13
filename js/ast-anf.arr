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

fun EqualsExcept(fields-to-skip):
  {
    extend: fun(obj):
      obj.{
        _equals(self, other):
          negate1 = fun(f): fun(v): not f(v);;
          fields = builtins.keys(self).filter(negate1(fields-to-skip.member(_)))
          for fold(acc from true, f from fields):
            builtins.has-field(other, f) and 
              block:
                thisval = self:[f]
                otherval = other:[f]
                equal = if Method(thisval) or Function(thisval): true
                        else: thisval == otherval;
                acc and equal
              end
          end
        end
      }
    end,
    brand: fun(e): e;
  }
end

check:
  dl1 = error.location("somewhere", -1, -1)
  dl2 = error.location("somewhere-else", -1, -1)

  n1 = a-num(dl1, 5)
  n2 = a-num(dl2, 5)

  a-num(dl1, 5) is a-num(dl2, 5)
end

equals-except-loc = EqualsExcept(["l", "ann"])

data AProg deriving equals-except-loc:
  | a-program(l :: Loc, imports :: List<AImport>, body :: AExpr) with:
    tosource(self):
      PP.group(
        PP.flow_map(PP.hardline, fun(i): i.tosource() end, self.imports)
          + PP.hardline
          + self.body.tosource()
        )
    end
end

data AImport deriving equals-except-loc:
  | a-import-file(l :: Loc, file :: String, name :: String)
  | a-import-builtin(l :: Loc, lib :: String, name :: String)
  | a-provide(l :: Loc, val :: AExpr)
end

data AExpr deriving equals-except-loc:
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
  | a-if(l :: Loc, c :: AVal, t :: AExpr, e :: AExpr) with:
    tosource(self):
      str-if + break-one + self.c.tosource() + str-colon +
          PP.nest(INDENT, break-one + self.t.tosource()) +
        str-elsecolon
          PP.nest(INDENT, break-one + self.e.tosource())
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

data ABind deriving equals-except-loc:
  | a-bind(l :: Loc, id :: String, ann :: A.Ann) with:
    tosource(self): PP.str(self.id) end
end

data ALettable deriving equals-except-loc:
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

data AField deriving equals-except-loc:
  | a-field(l :: Loc, name :: String, value :: AVal) with:
    tosource(self): PP.nest(INDENT, self.name.tosource() + str-colonspace + self.value.tosource()) end,
end

data AVal deriving equals-except-loc:
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
end

