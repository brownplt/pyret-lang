provide {
    substitute-pattern: substitute-pattern,
    desugar-eval: desugar-eval
} end

include string-dict
include either
import ast as A

include file("ds-structs.arr")
include file("ds-parse.arr")
include file("ds-environment.arr")
include file("ds-biject.arr")
include file("ds-match.arr")

data Metafunction:
  | metafunction(arity :: Number, f :: (List<Term>, Env -> Term))
end

metafunctions = [string-dict:
  "get-loc-of", metafunction(1, lam(args, env):
    cases (Term) args.get(0):
      | g-core(_, shadow args) => args.get(0)
      # TODO: might want to support g-var
      | else =>
        fail("get-loc-of should be used on an already desugared value. Got " +
             tostring(args.get(0)))
    end
  end)]

fun find-option<S, T>(f :: (S -> Option<T>), lst :: List<S>) -> Option<T>:
  cases (List) lst:
    | empty => none
    | link(h, t) =>
      cases (Option) f(h):
        | none => find-option(f, t)
        | some(v) => some(v)
      end
  end
end

fun generate-pvars(n :: Number) -> List<Pattern>:
  range(0, n).map(lam(i): pat-pvar("_" + tostring(i), [set: ], none) end)
end

################################################################################
#  Substitution
#

fun subs(rules :: DsRules, env :: Env, p :: Pattern) -> Term:
  fun loop(shadow p):
    cases (Pattern) p block:
      | pat-pvar(name, _, _) =>
        cases (Option) get-pvar(env, name):
          | none => fail("Pattern variable '" + name + "' not found.")
          | some(e) => e
        end
      | pat-value(val) => g-value(val)
      | pat-core(op, args) => g-core(op, args.map(loop))
      | pat-aux(op, args)  => g-aux(op, args.map(loop))
      | pat-surf(op, args) => desugar-eval(rules, op, args.map(loop))
      | pat-biject(op, shadow p) =>
        cases (Option) bijections.get(op):
          | none => fail("Bijection '" + op + "' not found")
          | some({f; _}) => f(loop(p)) # TODO: need recur?
        end
      | pat-meta(op, args) =>
        term-args = args.map(loop)
        cases (Option) metafunctions.get(op):
          | none => fail("Metafunction '" + op + "' not found")
          | some(metaf) =>
            if term-args.length() == metaf.arity:
              metaf.f(term-args, env)
              # TODO: need recur?
            else:
              fail("Arity mismatch when calling metafunction '" + op + "'. " +
                   "Expect " + tostring(metaf.arity) + " arguments. Got " +
                   tostring(term-args.length()))
            end
        end
      | pat-var(name) =>
        cases (Option) get-fresh(env, name):
          | none => g-var(naked-var(name)) # TODO?
          | some(v) => g-var(v)
        end
      | pat-option(opt) => g-option(opt.and-then(loop))
      | pat-tag(lhs, rhs, body) => g-tag(lhs, rhs, loop(body))
      | pat-fresh(fresh, body) => subs(rules, assign-fresh-names(env, fresh), body)
      | pat-list(seq) => g-list(loop-list(seq))
    end
  end

  fun loop-list(ps :: SeqPattern) -> List<Term>:
    cases (SeqPattern) ps:
      | seq-empty => empty
      | seq-cons(f, r) => link(loop(f), loop-list(r))
      | seq-ellipsis(shadow p, l) =>
        cases (Option) get-ellipsis(env, l):
          | none => fail("Ellipsis label '" + l + "' not found.")
          | some(envs) => for map(shadow env from envs): subs(rules, env, p) end
        end
      | seq-ellipsis-list(shadow ps, l) =>
        cases (Option) get-ellipsis(env, l):
          | none => fail("Ellipsis label '" + l + "' not found.")
          | some(envs) =>
            for map2(shadow env from envs, shadow p from ps): subs(rules, env, p) end
        end
    end
  end
  loop(p)
end

rec substitute-pattern = subs

check:
  subs(set-ellipsis(empty-env(),
      "l1",
      [list:
        set-pvar(empty-env(), "a", parse-ast("(Foo)")),
        set-pvar(empty-env(), "a", parse-ast("5"))
      ]),
    parse-pattern(none, "(Bar [a ...])"))
    is parse-ast("(Bar [(Foo) 5])")
  
  subs(set-ellipsis(empty-env(),
      "l1",
      [list:
        set-pvar(empty-env(), "a", parse-ast("(Foo)")),
        set-pvar(empty-env(), "a", parse-ast("5"))
      ]),
    parse-pattern(none, "(Bar [a ...] [a ...])"), none)
    is parse-ast("(Bar [(Foo) 5] [(Foo) 5])")
  
end

fun desugar-eval(rules :: DsRules, op :: String, args :: List<Term>):
  cases (Option) rules.get(op) block:
    | none =>
      # TODO: this should eventually throw an error. Right now
      # allow it to work so that we can add sugars incrementally
      pvars = generate-pvars(args.length())
      pat-lhs = pat-surf(op, pvars)
      pat-rhs = pat-core(op, pvars)
      g-tag(pat-lhs, pat-rhs, g-core(op, args)) #^ pop-time
    | some(kases) =>
      #nothing ^ push-time("matching: " + op)
      opt = for find-option(kase from kases) block:
        cases (Either) match-pattern(g-surf(op, args), kase.lhs):
          | left({env; p}) => some({kase; env; p})
          | right(_) => none
        end
      end #^ pop-time ^ push-time("substitution: " + op)
      cases (Option) opt block:
        | none => fail("No case match in " + tostring(op) + " with " + tostring(args))
        | some({kase; env; pat-lhs}) =>
          g-tag(pat-lhs, kase.rhs, substitute-pattern(rules, env, kase.rhs))
      end
  end
end
