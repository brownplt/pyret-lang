#lang pyret

provide *
import ast as A

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
  | a-program(l :: Loc, imports :: List<AImport>, body :: AExpr) 
end

data AImport deriving equals-except-loc:
  | a-import-file(l :: Loc, file :: String, name :: String)
  | a-import-builtin(l :: Loc, lib :: String, name :: String)
  | a-provide(l :: Loc, val :: AExpr)
end

data AExpr deriving equals-except-loc:
  | a-let(l :: Loc, b :: ABind, e :: ALettable, body :: AExpr)
  | a-var(l :: Loc, b :: ABind, e :: ALettable, body :: AExpr)
  | a-if(l :: Loc, c :: AVal, t :: AExpr, e :: AExpr)
  | a-try(l :: Loc, body :: AExpr, b :: ABind, _except :: AExpr)
  | a-lettable(e :: ALettable)
end

data ALetBind deriving equals-except-loc:
  | a-let-bind(l :: Loc, variable :: Bool, b :: ABind, e :: ALettable)
end

data ABind deriving equals-except-loc:
  | a-bind(l :: Loc, id :: String, ann :: A.Ann)
end

data ALettable deriving equals-except-loc:
  | a-assign(l :: Loc, id :: String, value :: AVal)
  | a-app(l :: Loc, f :: AVal, args :: List<AVal>)
  | a-help-app(l :: Loc, f :: String, args :: List<AVal>)
  | a-split-app(l :: Loc, f :: AVal, args :: List<AVal>, helper :: String, helper-args :: List<AVal>)
  | a-obj(l :: Loc, fields :: List<AField>)
  | a-update(l :: Loc, super :: AVal, fields :: List<AField>)
  | a-dot(l :: Loc, obj :: AVal, field :: String)
  | a-colon(l :: Loc, obj :: AVal, field :: String)
  | a-get-bang(l :: Loc, obj :: AVal, field :: String)
  | a-lam(l :: Loc, args :: List<ABind>, body :: AExpr)
  | a-method(l :: Loc, args :: List<ABind>, body :: AExpr)
  | a-val(v :: AVal)
end

data AField deriving equals-except-loc:
  | a-field(l :: Loc, name :: String, value :: AVal) 
end

data AVal deriving equals-except-loc:
  | a-num(l :: Loc, n :: Number)
  | a-str(l :: Loc, s :: String)
  | a-bool(l :: Loc, b :: Bool)
  # used for letrec
  | a-undefined(l :: Loc)
  | a-id(l :: Loc, id :: String)
end

dummy-loc = error.location("", 0, 0)
