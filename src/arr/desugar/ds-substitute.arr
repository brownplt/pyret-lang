provide {
    substitute-pattern: substitute-pattern
} end

include string-dict
include either
import ast as A

include file("ds-structs.arr")
include file("ds-parse.arr")
include file("ds-environment.arr")
include file("ds-resolve-ellipses.arr")

data Metafunction:
  | metafunction(
      arity :: Number,
      f :: (List<Pattern>, Env, Option<A.Loc> -> Term))
end

metafunctions = [string-dict:
  "get-loc", metafunction(0, lam(_, _, top-loc):
    cases (Option) top-loc:
      | none => panic("trying to get-loc with no srcloc")
      | some(loc) => g-value(e-loc(loc))
    end
  end)]

################################################################################
#  Substitution
#

fun subs(env :: Env, p :: Pattern, top-loc :: Option<A.Loc>) -> Term:
  cases (Pattern) p:
    | pat-pvar(name, _) =>
      cases (Option) get-pvar(env, name):
        | none => fail("Pattern variable '" + name + "' not found.")
        | some(e) => e
      end
    | pat-value(val) => g-value(val)
    | pat-core(name, args) => g-core(name, top-loc, map(subs(env, _, top-loc), args))
    | pat-aux(name, args)  => g-aux(name, top-loc, map(subs(env, _, top-loc), args))
    | pat-surf(name, args) => g-surf(name, top-loc, map(subs(env, _, top-loc), args))
    | pat-meta(name, args) =>
      cases (Option) metafunctions.get(name):
        | none => fail("Metafunction '" + name + "' not found")
        | some(metaf) =>
          if args.length() == metaf.arity:
            metaf.f(args, env, top-loc)
          else:
            fail("Arity mismatch when calling metafunction '" + name + "'. " +
                 "Expect " + tostring(metaf.arity) + " arguments. Got " + tostring(args.length()))
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
        | some(shadow p) => g-option(some(subs(env, p, top-loc)))
      end
    | pat-tag(lhs, rhs, body) => g-tag(lhs, rhs, subs(env, body, top-loc))
    | pat-fresh(fresh, body) => subs(assign-fresh-names(env, fresh), body, top-loc)
    | pat-list(seq) => g-list(subs-list(env, seq, top-loc))
  end
end

fun subs-list(env :: Env, ps :: SeqPattern, top-loc :: Option<A.Loc>) -> List<Term>:
  cases (SeqPattern) ps:
    | seq-empty => empty
    | seq-cons(f, r) => link(subs(env, f, top-loc), subs-list(env, r, top-loc))
    | seq-ellipsis(p, l) => 
      cases (Option) get-ellipsis(env, l):
        | none => fail("Ellipsis label '" + l + "' not found.")
        | some(envs) => for map(shadow env from envs): subs(env, p, top-loc) end
      end
    | seq-ellipsis-list(shadow ps, l) => 
      cases (Option) get-ellipsis(env, l):
        | none => fail("Ellipsis label '" + l + "' not found.")
        | some(envs) => 
          for map2(shadow env from envs, p from ps): subs(env, p, top-loc) end
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
    parse-pattern(none, "(Bar [a ...])"), none)
    is parse-ast("(Bar [(Foo) 5])")
  
  subs(set-ellipsis(empty-env(),
      "l1",
      [list:
        set-pvar(empty-env(), "a", parse-ast("(Foo)")),
        set-pvar(empty-env(), "a", parse-ast("5"))
      ]),
    resolve-ellipses(parse-pattern(none, "(Bar [a ...] [a ...])")), none)
    is parse-ast("(Bar [(Foo) 5] [(Foo) 5])")
  
end
