provide *
provide-types *

include string-dict
import ast as AST

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
  | g-prim(val :: GenericPrimitive)
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
  | p-pvar(name :: String, labels :: Set<String>, typ :: Option<String>)
  | p-prim(val :: GenericPrimitive)
  | p-core(op :: String, args :: List<Pattern>)
  | p-surf(op :: String, args :: List<Pattern>)
  | p-aux(op :: String, args :: List<Pattern>)
  | p-meta(op :: String, args :: List<Pattern>)
  | p-biject(op :: String, p :: Pattern)
  | p-var(name :: String)
  | p-list(l :: SeqPattern)
  | p-option(opt :: Option<Pattern>)
  | p-tag(lhs :: Pattern, rhs :: Pattern, body :: Pattern)
  | p-fresh(fresh :: Set<String>, body :: Pattern)
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

data Env:
  | environment(
      pvar-map :: StringDict<Term>,
      fresh-map :: StringDict<Variable>,
      ellipsis-map :: StringDict<List<Env>>)
end


################################################################################
#  Errors
#

fun panic(message :: String):
  raise({"Internal error when desugaring"; message})
end

fun fail(message :: String):
  raise({"Error when desugaring"; message})
end


################################################################################
#  Metafunctions and Bijections
#

data Metafunction:
  | metafunction(arity :: Number, f :: (List<Term>, Env -> Term))
end

__METAFUNCTIONS = [mutable-string-dict:]

fun add-metafunction(op :: String, arity :: Number, f :: (List<Term>, Env -> Term)):
  __METAFUNCTIONS.set-now(op, metafunction(arity, f))
end

fun lookup-metafunction(op :: String) -> Metafunction:
  cases (Option) __METAFUNCTIONS.get-now(op):
    | none => fail("Metafunction '" + op + "' not found")
    | some(metaf) => metaf
  end
end

__BIJECTIONS = [mutable-string-dict:]

fun add-bijection(op :: String, forward :: (Term -> Term), reverse :: (Term -> Term)):
  __BIJECTIONS.set-now(op, {forward; reverse})
end

fun lookup-bijection(op :: String) -> { (Term -> Term); (Term -> Term) }:
  # Note: above spacing must be preserved to satisfy Elder Gods.
  cases (Option) __BIJECTIONS.get-now(op):
    | none => fail("Bijection '" + op + "' not found")
    | some(bij) => bij
  end
end


################################################################################
#  Utilities
#

fun rename-p-pvar(p :: Pattern, before :: String, after :: String) -> Pattern:
  fun loop(shadow p :: Pattern):
    cases (Pattern) p:
      | p-pvar(s, labels, t) => if s == before: p-pvar(after, labels, t) else: p end
      | p-prim(_) => p
      | p-core(op, args) => p-core(op, args.map(loop))
      | p-surf(op, args) => p-surf(op, args.map(loop))
      | p-aux(op, args) => p-aux(op, args.map(loop))
      | p-meta(op, args) => p-meta(op, args.map(loop))
      | p-biject(op, shadow p) => p-biject(op, loop(p))
      | p-var(_) => p
      | p-list(l) => p-list(loop-list(l))
      | p-option(opt) => p-option(opt.and-then(loop))
      | p-tag(lhs, rhs, body) => p-tag(loop(lhs), loop(rhs), loop(body))
      | p-fresh(fresh, body) => p-fresh(fresh, loop(body))
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

term-dummy-loc = g-prim(e-loc(AST.dummy-loc))

fun strip-tags(e :: Term) -> Term:
  cases (Term) e:
    | g-prim(val) => g-prim(val)
    | g-core(op, args) => g-core(op, args.map(strip-tags))
    | g-aux(op, args) => g-aux(op, args.map(strip-tags))
    | g-surf(op, args) => g-surf(op, args.map(strip-tags))
    | g-list(seq) => g-list(seq.map(strip-tags))
    | g-option(opt) => g-option(opt.and-then(strip-tags))
    | g-var(v) => g-var(v)
    | g-tag(_, _, body) => strip-tags(body)
  end
end
