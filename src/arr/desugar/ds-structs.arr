provide *
provide-types *

include string-dict
import ast as AST
import global as _
import base as _

type Variable = {
  name :: String,
  serial :: Number,
  loc :: AST.Loc,
  sign :: Option<VarSign>,
  kind :: Option<VarKind>,
  shadows :: Boolean,
  global :: Boolean
}

data GenericPrimitive:
  | e-str(s :: String)
  | e-num(n :: Number)
  | e-bool(b :: Boolean)
  | e-loc(l :: AST.Loc)
end

data Term:
  | g-value(val :: GenericPrimitive)
  | g-core(op :: String, loc :: Option<AST.Loc>, args :: List<Term>)
  | g-surf(op :: String, loc :: Option<AST.Loc>, args :: List<Term>)
  | g-aux(op :: String, loc :: Option<AST.Loc>, args :: List<Term>)
  | g-var(v :: Variable)
  | g-list(lst :: List<Term>)
  | g-option(opt :: Option<Term>)
  | g-tag(lhs :: Pattern, rhs :: Pattern, body :: Term)
end

data VarSign:
  | var-decl
  | var-refn
end

data VarKind:
  | type-var
  | value-var
end

data Pattern:
  | pat-pvar(name :: String, typ :: Option<String>)
  | pat-value(val :: GenericPrimitive)
  | pat-core(op :: String, args :: List<Pattern>)
  | pat-surf(op :: String, args :: List<Pattern>)
  | pat-aux(op :: String, args :: List<Pattern>)
  | pat-var(name :: String)
  | pat-list(l :: SeqPattern)
  | pat-option(opt :: Option<Pattern>)
  | pat-tag(lhs :: Pattern, rhs :: Pattern, body :: Pattern)
  | pat-fresh(fresh :: Set<String>, body :: Pattern)
end

data SeqPattern:
  | seq-empty
  | seq-cons(first :: Pattern, rest :: SeqPattern)
  | seq-ellipsis(p :: Pattern, label :: String)
  | seq-ellipsis-list(patts :: List<Pattern>, label :: String)
end

data DsRule:
  | ds-rule(op :: String, kases :: List<DsRuleCase>)
end

data DsRuleCase:
  | ds-rule-case(lhs :: Pattern, rhs :: Pattern)
end

data ScopeRuleset:
  | scope-rule-set(map :: StringDict<ScopeRule>)
end

data ScopeRule:
  | scope-rule(
      exports :: List<Number>,         # i means export child i's declarations
      binds :: List<{Number; Number}>) # {i; j} means bind child j in child i
end

fun naked-var(name :: String) -> Variable:
  {
    name: name,
    serial: 0,
    loc: AST.dummy-loc,
    sign: none,
    kind: none,
    shadows: false,
    global: false
  }
end

fun panic(message :: String):
  raise({"Internal error when desugaring"; message})
end

fun fail(message :: String):
  raise({"Error when desugaring"; message})
end
