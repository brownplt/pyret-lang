provide {
    resugar: resugar
} end

include either
include file("ds-structs.arr")
include file("ds-parse.arr")
include file("ds-environment.arr")
include file("ds-match.arr")
include file("debugging.arr")

fun chain-option<A, B>(
    f :: (A -> Option<B>),
    arg :: Option<A>)
  -> Option<B>:
  cases (Option) arg:
    | none => none
    | some(v) => f(v)
  end
end

fun map-option<A, B>(f :: (A -> Option<B>), lst :: List<A>) -> Option<List<B>>:
  cases (List) lst:
    | empty => some(empty)
    | link(h, t) =>
      cases (Option) f(h):
        | none => none
        | some(v) =>
          cases (Option) map-option(f, t):
            | none => none
            | some(vs) => some(link(v, vs))
          end
      end
  end
end

fun map-option-2<A, B, C>(f :: (A, B -> Option<C>), lstA :: List<A>, lstB :: List<B>)
  -> Option<List<C>>:
  cases (List) lstA:
    | empty =>
      cases (List) lstB:
        | empty => some(empty)
        | link(_, _) => none
      end
    | link(hA, tA) =>
      cases (List) lstB:
        | empty => none
        | link(hB, tB) =>
          cases (Option) f(hA, hB):
            | none => none
            | some(h) =>
              cases (Option) map-option(f, tA, tB):
                | none => none
                | some(t) => some(link(h, t))
              end
          end
      end
  end
end

fun resugar(e :: Term) -> Option<Term>:
  cases (Term) e:
    | g-prim(val) => g-prim(val) ^ some
    | g-core(op, args) =>
      map-option(resugar, args).and-then(g-core(op, _))
    | g-surf(op, args) =>
      map-option(resugar, args).and-then(g-surf(op, _))
    | g-aux(op, args) =>
      map-option(resugar, args).and-then(g-aux(op, _))
    | g-var(v) => g-var(v) ^ some
    | g-list(lst) => map-option(resugar, lst).and-then(g-list)
    | g-option(opt) =>
      for chain-option(shadow e from opt):
        for chain-option(shadow e from resugar(e)):
          some(g-option(some(e)))
        end
      end
    | g-tag(lhs, rhs, body) =>
      for chain-option(shadow body from resugar(body)):
        resugar-tag(lhs, rhs, body)
      end
  end
end

fun subs(env :: Env, p :: Pattern) -> Option<Term>:
  fun loop(shadow p):
    cases (Pattern) p block:
      | p-pvar(name, _, _) =>
        cases (Option) get-pvar(env, name):
          | none => #some(g-option(none))
            # TODO: handle dropped pvars (note: may be dropped due to empty ellipses)
            # IDEA: instantiate ellipsis patterns
            fail-rs("Pattern variable '" + name + "' not found.")
          | some(e) => some(e)
        end
      | p-prim(val) => g-prim(val) ^ some
      | p-core(op, args) => map-option(subs(env, _), args).and-then(g-core(op, _))
      | p-aux(op, args)  => map-option(subs(env, _), args).and-then(g-aux(op, _))
      | p-surf(op, args) => map-option(subs(env, _), args).and-then(g-surf(op, _))
      | p-biject(op, shadow p) =>
        {f; _} = lookup-bijection(op)
        loop(p).and-then(f) # TODO: need recur?
      | p-meta(op, args) =>
        metaf = lookup-metafunction(op)
        when args.length() <> metaf.arity:
          fail-rs("Arity mismatch when calling metafunction '" + op + "'. " +
            "Expect " + tostring(metaf.arity) + " arguments. Got " +
            tostring(args.length()))
        end
        for chain-option(term-args from map-option(subs(env, _), args)):
          metaf.f(term-args) ^ some
        end
      | p-var(name) =>
        cases (Option) get-fresh(env, name):
          | none => g-var(naked-var(name)) ^ some # TODO?
          | some(v) => g-var(v) ^ some
        end
      | p-option(opt) => 
        for chain-option(shadow p from opt):
          for chain-option(e from subs(env, p)):
            some(g-option(some(e)))
          end
        end
      | p-fresh(fresh, body) => subs(assign-fresh-names(env, fresh), body)
      | p-list(seq) => loop-list(seq).and-then(p-list)
      | p-tag(lhs, rhs, body) =>
        for chain-option(shadow body from subs(env, body)):
          resugar-tag(lhs, rhs, body)
        end
    end
  end

  fun loop-list(ps :: SeqPattern) -> Option<List<Term>>:
    cases (SeqPattern) ps:
      | seq-empty => empty ^ some
      | seq-cons(f, r) =>
        for chain-option(shadow f from loop(f)):
          for chain-option(shadow r from loop-list(r)):
            some(link(f, r))
          end
        end
      | seq-ellipsis(shadow p, l) =>
        cases (Option) get-ellipsis(env, l):
          | none => fail-rs("Ellipsis label '" + l + "' not found.")
          | some(envs) =>
            for map-option(shadow env from envs):
              subs(env, p)
            end
        end
      | seq-ellipsis-list(shadow ps, l) =>
        cases (Option) get-ellipsis(env, l):
          | none => fail-rs("Ellipsis label '" + l + "' not found.")
          | some(envs) =>
            for map-option-2(shadow p from ps, shadow env from envs):
              subs(env, p)
            end
        end
    end
  end
    
  loop(p)
end

fun resugar-tag(lhs :: Pattern, rhs :: Pattern, body :: Term) -> Option<Term>:
  cases (Either) match-pattern(body, rhs):
    | left({env; _}) => subs(env, lhs)
    | right(_) => none
  end
end
