provide {
    desugar: desugar    
} end

include string-dict
include either

include file("ds-structs.arr")
include file("ds-parse.arr")
include file("ds-environment.arr")
include file("ds-match.arr")


################################################################################
#  Notes
#

# Errors:
# - Desugaring needs to be able to raise errors,
#   and it needs to be able to raise multiple errors.
# - There should be a special kind of auxilliary called 'error',
#   whose children are a list of pyret error expressions.
# - If a sugar expands to a pattern with one or more 'error',
#   then desugaring will collect those errors and expand to an 'error'.
# - If one or more of a sugar's children expands into an 'error',
#   then that sugar expands into an 'error' (whose children are the union
#   of the sugars' childrens' errors' children).
# - If no sugar matches, that's an internal error, and will get automagically
#   turned into an 'error'.
# - If a sugar has no matching case when expanding, that's an error.
# - Possible feature: special 'leftover' rules for turning auxilliary
#   terms that are leftover after desugaring into errors.


################################################################################
#  Utilities
#

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
  range(0, n).map(lam(i): p-pvar("_" + tostring(i), [set: ], none) end)
end


################################################################################
#  Desugaring and Substitution
#
  
fun desugar(rules :: DsRules, e :: Term) -> Term:
  fun desugars(es :: List<Term>) -> List<Term>:
    es.map(desugar(rules, _))
  end
  cases (Term) e block:
    | g-prim(val) => g-prim(val)
    | g-core(op, args) => g-core(op, desugars(args))
    | g-aux(op, args) => g-aux(op, desugars(args))
    | g-var(v) => g-var(v)
    | g-list(lst) => g-list(desugars(lst))
    | g-option(opt) => g-option(opt.and-then(desugar(rules, _)))
    | g-tag(lhs, rhs, body) => g-tag(lhs, rhs, desugar(rules, body))
    | g-surf(op, args) => desugar-surf(rules, op, desugars(args))
  end
end

fun subs(rules :: DsRules, env :: Env, p :: Pattern) -> Term:
  fun loop(shadow p):
    cases (Pattern) p block:
      | p-pvar(name, _, _) =>
        cases (Option) get-pvar(env, name):
          | none => fail("Pattern variable '" + name + "' not found.")
          | some(e) => e
        end
      | p-prim(val) => g-prim(val)
      | p-core(op, args) => g-core(op, args.map(loop))
      | p-aux(op, args)  => g-aux(op, args.map(loop))
      | p-surf(op, args) => desugar-surf(rules, op, args.map(loop))
      | p-biject(op, shadow p) =>
        {f; _} = lookup-bijection(op)
        f(loop(p)) # TODO: need recur?
      | p-meta(op, args) =>
        term-args = args.map(loop)
        metaf = lookup-metafunction(op)
        if term-args.length() == metaf.arity:
          metaf.f(term-args, env)
          # TODO: need recur?
        else:
          fail("Arity mismatch when calling metafunction '" + op + "'. " +
               "Expect " + tostring(metaf.arity) + " arguments. Got " +
               tostring(term-args.length()))
        end
      | p-var(name) =>
        cases (Option) get-fresh(env, name):
          | none => g-var(naked-var(name)) # TODO?
          | some(v) => g-var(v)
        end
      | p-option(opt) => g-option(opt.and-then(loop))
      | p-tag(lhs, rhs, body) => g-tag(lhs, rhs, loop(body))
      | p-fresh(fresh, body) => subs(rules, assign-fresh-names(env, fresh), body)
      | p-list(seq) => g-list(loop-list(seq))
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

fun desugar-surf(rules :: DsRules, op :: String, args :: List<Term>):
  cases (Option) rules.get(op) block:
    | none =>
      # TODO: this should eventually throw an error. Right now
      # allow it to work so that we can add sugars incrementally
      pvars = generate-pvars(args.length())
      p-lhs = p-surf(op, pvars)
      p-rhs = p-core(op, pvars)
      g-tag(p-lhs, p-rhs, g-core(op, args)) #^ pop-time
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
        | some({kase; env; p-lhs}) =>
          g-tag(p-lhs, kase.rhs, subs(rules, env, kase.rhs))
      end
  end
end


################################################################################
#  Tests
#

check:
  rules = parse-ds-rules(
    ```
    sugar or:
    | (or @l a:Expr b) => (fresh [x] (let (bind x a) (if x x b)))
    end
    ```)
  e = parse-ast("(or p q)")
  desugar(rules, e) does-not-raise
end

check:
  subs([string-dict:], environment(
      [string-dict: "l", term-dummy-loc],
      [string-dict: ],
      [string-dict: "i", 
        [list:
          set-pvar(empty-env(), "a", parse-ast("<Foo>")),
          set-pvar(empty-env(), "a", parse-ast("5"))
      ]]),
    parse-pattern(none, "(Bar @l [a_{i} ...i])")) ^ strip-tags
    is parse-ast("<Bar [<Foo> 5]>")
 
  subs([string-dict:], environment(
      [string-dict: "l", term-dummy-loc],
      [string-dict: ],
      [string-dict:
        "i", 
        [list:
          set-pvar(empty-env(), "a", parse-ast("<Foo>")),
          set-pvar(empty-env(), "a", parse-ast("5"))
      ]]),
    parse-pattern(none, "(Bar @l [a_{i} ...i] [a_{i} ...i])")) ^ strip-tags
    is parse-ast("<Bar [<Foo> 5] [<Foo> 5]>")  
end
