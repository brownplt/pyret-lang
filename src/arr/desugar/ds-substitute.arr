provide {
    substitute-pattern: substitute-pattern
} end

include string-dict
include either
import ast as A

include file("ds-structs.arr")
include file("ds-parse.arr")
include file("ds-environment.arr")

data Metafunction:
  | metafunction(
      arity :: Number,
      f :: (List<Term>, Env -> Term))
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

################################################################################
#  Substitution
#

fun subs(env :: Env, p :: Pattern) -> Term:
  cases (Pattern) p block:
    | pat-pvar(name, _, _) =>
      cases (Option) get-pvar(env, name):
        | none => fail("Pattern variable '" + name + "' not found.")
        | some(e) => e
      end
    | pat-value(val) => g-value(val)
    | pat-core(name, args) => g-core(name, map(subs(env, _), args))
    | pat-aux(name, args)  => g-aux(name, map(subs(env, _), args))
    | pat-surf(name, args) => g-surf(name, map(subs(env, _), args))
    | pat-meta(name, args) =>
      term-args = map(subs(env, _), args)
      cases (Option) metafunctions.get(name):
        | none => fail("Metafunction '" + name + "' not found")
        | some(metaf) =>
          if term-args.length() == metaf.arity:
            metaf.f(term-args, env)
          else:
            fail("Arity mismatch when calling metafunction '" + name + "'. " +
                 "Expect " + tostring(metaf.arity) + " arguments. Got " +
                 tostring(term-args.length()))
          end
      end
    | pat-var(name) =>
      cases (Option) get-fresh(env, name):
        | none => g-var(naked-var(name)) # TODO?
        | some(v) => g-var(v)
      end
    | pat-option(opt) =>
      cases (Option) opt:
        | none => g-option(none)
        | some(shadow p) => g-option(some(subs(env, p)))
      end
    | pat-tag(lhs, rhs, body) => g-tag(lhs, rhs, subs(env, body))
    | pat-fresh(fresh, body) => subs(assign-fresh-names(env, fresh), body)
    | pat-list(seq) => g-list(subs-list(env, seq))
  end
end

fun subs-list(env :: Env, ps :: SeqPattern) -> List<Term>:
  cases (SeqPattern) ps:
    | seq-empty => empty
    | seq-cons(f, r) => link(subs(env, f), subs-list(env, r))
    | seq-ellipsis(p, l) => 
      cases (Option) get-ellipsis(env, l):
        | none => fail("Ellipsis label '" + l + "' not found.")
        | some(envs) => for map(shadow env from envs): subs(env, p) end
      end
    | seq-ellipsis-list(shadow ps, l) => 
      cases (Option) get-ellipsis(env, l):
        | none => fail("Ellipsis label '" + l + "' not found.")
        | some(envs) => 
          for map2(shadow env from envs, p from ps): subs(env, p) end
      end
  end
end

substitute-pattern = subs

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
