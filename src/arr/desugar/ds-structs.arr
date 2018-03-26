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
  | g-core(op :: String, args :: List<Term>)
  | g-surf(op :: String, args :: List<Term>)
  | g-aux(op :: String, args :: List<Term>)
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
  | pat-meta(op :: String, args :: List<Pattern>)
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

type DsRules = StringDict<List<DsRuleCase>>

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

fun rename-pat-pvar(p :: Pattern, before :: String, after :: String) -> Pattern:
  fun loop(shadow p :: Pattern):
    cases (Pattern) p:
      | pat-pvar(s, t) => if s == before: pat-pvar(after, t) else: p end
      | pat-value(_) => p
      | pat-core(op, args) => pat-core(op, args.map(loop))
      | pat-surf(op, args) => pat-surf(op, args.map(loop))
      | pat-aux(op, args) => pat-aux(op, args.map(loop))
      | pat-meta(op, args) => pat-meta(op, args.map(loop))
      | pat-var(_) => p
      | pat-list(l) => pat-list(loop-list(l))
      | pat-option(opt) => pat-option(opt.and-then(loop))
      | pat-tag(lhs, rhs, body) => pat-tag(loop(lhs), loop(rhs), loop(body))
      | pat-fresh(fresh, body) => pat-fresh(fresh, loop(body))
    end
  end
  fun loop-list(ps :: SeqPattern):
    cases (SeqPattern) ps:
      | seq-empty => seq-empty
      | seq-cons(shadow p, shadow ps) => seq-cons(loop(p), loop-list(ps))
      | seq-ellipsis(shadow p, l) => seq-ellipsis(loop(p), l)
      | seq-ellipsis-list(lst, l) => seq-ellipsis-list(lst.map(loop), l)
    end
  end
  loop(p)
end
