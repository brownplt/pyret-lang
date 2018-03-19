provide {
    substitute-pattern: substitute-pattern
} end

include string-dict
include either

include file("ds-structs.arr")
include file("ds-parse.arr")
include file("ds-environment.arr")
include file("ds-resolve-ellipses.arr")


################################################################################
#  Substitution
#

fun subs(env :: Env, p :: Pattern) -> Term:
  cases (Pattern) p:
    | p-pvar(name, _) => 
      cases (Option) get-pvar(env, name):
        | none => fail("Pattern variable '" + name + "' not found.")
        | some(e) => e
      end
    | p-value(val) => g-value(val)
    | p-core(name, args) => g-core(name, none, map(subs(env, _), args))
    | p-aux(name, args)  => g-aux(name, none, map(subs(env, _), args))
    | p-surf(name, args) => g-surf(name, none, map(subs(env, _), args))
    | p-var(name) => 
      cases (Option) get-fresh(env, name):
        | none => g-var(naked-var(name)) # TODO?
        | some(v) => g-var(v)
      end
    | p-option(opt) =>
      cases (Option) opt:
        | none => g-option(none)
        | some(shadow p) => g-option(some(subs(env, p)))
      end
    | p-tag(lhs, rhs, body) => g-tag(lhs, rhs, subs(env, body))
    | p-fresh(fresh, body) => subs(assign-fresh-names(env, fresh), body)
    | p-list(seq) => g-list(subs-list(env, seq))
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
    resolve-ellipses(parse-pattern(none, "(Bar [a ...] [a ...])")))
    is parse-ast("(Bar [(Foo) 5] [(Foo) 5])")
  
end
