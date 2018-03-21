provide {
  match-pattern: match-pattern
} end

provide-types {
  MatchResult :: MatchResult,
  MatchError :: MatchError,
  Env :: Env
}

include string-dict
include either

include file("ds-structs.arr")
include file("ds-parse.arr")
include file("ds-environment.arr")
include file("ds-resolve-ellipses.arr")


################################################################################
#  Errors
#

type MatchResult = Either<{Env; Pattern}, MatchError>

data MatchError:
  | m-error-match(e :: Term, p :: Pattern)
  | m-error-pvar(name :: String, e1 :: Term, e2 :: Term)
  | m-error-fresh(name :: String, v1 :: Variable, v2 :: Variable)
  | m-error-list(env1 :: List<Env>, env2 :: List<Env>) # different lens
end


################################################################################
#  Environment
#

fun bind-pvar(env :: Env, pvar :: String, e :: Term) -> Either<Env, MatchError>:
  cases (Option) env.pvar-map.get(pvar):
    | none => left(set-pvar(env, pvar, e))
    | some(e2) =>
      if e == e2:
        left(env)
      else:
        right(m-error-pvar(pvar, e, e2))
      end
  end
end

fun bind-fresh(env :: Env, pv :: String, ev :: Variable) -> Either<Env, MatchError>:
  cases (Option) env.fresh-map.get(pv):
    | none => left(set-fresh(env, pv, ev))
    | some(ev2) =>
      if ev == ev2: # TODO: Better test than equality?
        left(env)
      else:
        right(m-error-fresh(pv, ev, ev2))
      end
  end
end

fun bind-ellipsis(env :: Env, label :: String, env-list :: List<Env>)
  -> Either<Env, MatchError>:
  cases (Option) env.ellipsis-map.get(label):
    | none => left(set-ellipsis(env, label, env-list))
    | some(env-list2) =>
      for chain-either(shadow env-list from union-env-list(env-list, env-list2)):
        left(set-ellipsis(env, label, env-list))
      end
  end
end

fun union-pvar-map(map-1 :: StringDict<Term>, map-2 :: StringDict<Term>)
  -> Either<StringDict<Term>, MatchError>:
  for fold-either(
      shadow map from map-1,
      key from map-2.keys().to-list()):
    my-key :: String = key
    e2 = map-2.get-value(my-key)
    cases (Option) map.get(my-key):
      | some(e1) =>
        if e1 == e2:
          left(map)
        else:
          right(m-error-pvar(key, e1, e2))
        end
      | none => left(map.set(key, e2))
    end
  end
end

fun union-fresh-map(map-1 :: StringDict<Variable>, map-2 :: StringDict<Variable>)
  -> Either<StringDict<Variable>, MatchError>:
  for fold-either(
      shadow map from map-1,
      key from map-2.keys().to-list()):
    e2 = map-2.get-value(key)
    cases (Option) map.get(key):
      | some(e1) =>
        if e1 == e2:
          left(map)
        else:
          right(m-error-fresh(key, e1, e2))
        end
      | none => left(map.set(key, e2))
    end
  end
end

fun union-ellipsis-map(map-1 :: StringDict<List<Env>>, map-2 :: StringDict<List<Env>>)
  -> Either<StringDict<List<Env>>, MatchError>:
  for fold-either(
      shadow map from map-1,
      key from map-2.keys().to-list()):
    s2 = map-2.get-value(key)
    cases (Option) map.get(key):
      | some(s1) =>
        cases (Either) union-env-list(s1, s2):
          | left(env) => left(map.set(key, env))
          | right(err) => right(err)
        end
      | none => left(map.set(key, s2))
    end
  end
end

fun union-env(env1 :: Env, env2 :: Env) -> Either<Env, MatchError>:
  cases (Either) union-pvar-map(env1.pvar-map, env2.pvar-map):
    | right(err) => right(err)
    | left(pvar-map) =>
      cases (Either) union-fresh-map(env1.fresh-map, env2.fresh-map):
        | right(err) => right(err)
        | left(fresh-map) =>
          cases (Either) union-ellipsis-map(env1.ellipsis-map, env2.ellipsis-map):
            | right(err) => right(err)
            | left(ellipsis-map) => left(environment(pvar-map, fresh-map, ellipsis-map))
          end
      end
  end
end

fun union-env-list(
    env-list-1 :: List<Env>,
    env-list-2 :: List<Env>)
  -> Either<List<Env>, MatchError>:
  if env-list-1.length() == env-list-2.length():
    res = for fold-either2(env-list from empty, env1 from env-list-1, env2 from env-list-2):
      cases (Either) union-env(env1, env2):
        | left(env) => left(link(env, env-list))
        | right(err) => right(err)
      end
    end
    cases (Either) res:
      | left(env) => left(env.reverse())
      | right(err) => right(err)
    end
  else:
    right(m-error-list(env-list-1, env-list-2))
  end
end


################################################################################
#  Matching
#

fun match-pattern(e :: Term, p :: Pattern) -> MatchResult:
  match-rec([set: ], empty-env(), e, p)
end

fun match-rec(
    fresh :: Set<String>,
    env :: Env,
    e :: Term,
    p :: Pattern)
  -> MatchResult:

  fun match-error() -> MatchResult:
    right(m-error-match(e, p))
  end

  fun match-core-or-sugar(
      constructor :: (String, List<Pattern> -> Pattern),
      ename :: String,
      pname :: String,
      exprs :: List<Term>,
      patts :: List<Pattern>) -> MatchResult:
    if pname == ename:
      if patts.length() == exprs.length():
        either = for fold-either2(
            {shadow env; plist} from {env; [list:]},
            expr from exprs,
            patt from patts):
          for chain-either({shadow env; shadow patt} from match-rec(fresh, env, expr, patt)):
            left({env; link(patt, plist)})
          end
        end
        for chain-either({shadow env; plist :: List<Pattern>} from either):
          left({env; constructor(pname, plist.reverse())})
        end
      else:
        match-error()
      end
    else:
      match-error()
    end
  end

  fun match-list(
      shadow env :: Env,
      elist :: List<Term>,
      plist :: SeqPattern) -> MatchResult:
    cases (SeqPattern) plist:
      | seq-empty => 
        cases (List) elist:
          | empty => left({env; pat-list(seq-empty)})
          | link(_, _) => match-error()
        end
      | seq-cons(pfirst, prest) =>
        cases (List) elist:
          | empty => match-error()
          | link(efirst, erest) =>
            for chain-either(
                {shadow env; patt1} from match-rec(fresh, env, efirst, pfirst)):
              for chain-either(
                  {shadow env; patt2 :: Pattern} from match-list(env, erest, prest)):
                cases (Pattern) patt2:
                  | pat-list(seq) => left({env; pat-list(seq-cons(patt1, seq))})
                  | else => panic("Match: seq-cons case")
                end
              end
            end
        end
      | seq-ellipsis(shadow p :: Pattern, label :: String) =>
        res = for fold-either(result-list from empty, shadow e from elist):
          for chain-either({shadow env; patt} from match-rec(fresh, empty-env(), e, p)):
            left(link({env; patt}, result-list))
          end
        end
        for chain-either(result-list from res):
          shadow result-list = result-list.reverse()
          env-list = project-left(result-list)
          pattern-list = pat-list(seq-ellipsis-list(project-right(result-list), label))
          for chain-either(shadow env from bind-ellipsis(env, label, env-list)):
            left({env; pattern-list})
          end
        end
      | seq-ellipsis-list(_, _) =>
        panic("Match: unexpected seq-ellipsis-list")
    end
  end

  cases (Term) e:
    | g-tag(lhs, rhs, body) =>
      cases (Either) match-rec(fresh, env, body, p):
        | left({shadow env; shadow p}) => left({env; pat-tag(lhs, rhs, p)})
        | right(err) => right(err)
      end
    | else =>
      cases (Pattern) p:
        | pat-value(val) =>
          is-ok = cases (Term) e:
            | g-value(val2) => val == val2
            | else => false
          end
          if is-ok:
            left({env; p})
          else:
            match-error()
          end
        | pat-pvar(pvar, _) =>
          for chain-either(shadow env from bind-pvar(env, pvar, e)):
            left({env; p})
          end
        | pat-var(v) =>
          cases (Term) e:
            | g-var(v2) =>
              if fresh.member(v):
                for chain-either(shadow env from bind-fresh(env, v, v2)):
                  left({env; pat-var(v2.name)})
                end
              else if v == v2.name:
                left({env; pat-var(v2.name)})
              else:
                match-error()
              end
            | else => match-error()
          end
        | pat-core(pname, pargs) =>
          cases (Term) e:
            | g-core(ename, loc, eargs) =>
              match-core-or-sugar(pat-core, ename, pname, eargs, pargs)
            | else => match-error()
          end
        | pat-aux(pname, pargs) =>
          cases (Term) e:
            | g-aux(ename, loc, eargs) =>
              match-core-or-sugar(pat-aux, ename, pname, eargs, pargs)
            | else => match-error()
          end
        | pat-surf(pname, pargs) =>
          cases (Term) e:
            | g-surf(ename, loc, eargs) => 
              match-core-or-sugar(pat-surf, ename, pname, eargs, pargs)
            | else => match-error()
          end
        | pat-list(plist) =>
          cases (Term) e:
            | g-list(elist) => match-list(env, elist, plist)
            | else => match-error()
          end
        | pat-option(popt) =>
          cases (Term) e:
            | g-option(eopt) =>
              cases (Option) popt:
                | none =>
                  cases (Option) eopt:
                    | none => left({env; p})
                    | some(_) => match-error()
                  end
                | some(px) =>
                  cases (Option) eopt:
                    | none => match-error()
                    | some(ex) =>
                      for chain-either({shadow env; shadow p} from match-rec(fresh, env, ex, px)):
                        left({env; pat-option(some(p))})
                      end
                  end
              end
            | else => match-error()
          end
        | pat-tag(lhs, rhs, body) =>
          panic("Encountered a pat-tag while matching, but it should only be used internally.")
        | pat-fresh(fresh-vars, body) =>
          for chain-either({shadow env; shadow p} from match-rec(fresh.union(fresh-vars), env, e, body)):
            left({env; pat-fresh(fresh-vars, p)})
          end
      end
  end
end


################################################################################
#  Utility
#

fun project-left<A, B>(lst :: List<{A; B}>) -> List<A>:
  lst.map(lam({a; b}): a end)
end

fun project-right<A, B>(lst :: List<{A; B}>) -> List<B>:
  lst.map(lam({a; b}): b end)
end

fun chain-either<A, B, Err>(
    f :: (A -> Either<B, Err>),
    arg :: Either<A, Err>) -> Either<B, Err>:
  cases (Either) arg:
    | left(v) => f(v)
    | right(err) => right(err)
  end
end

fun fold-either<A, Res, Err>(
    f :: (Res, A -> Either<Res, Err>),
    base :: Res,
    lst :: List<A>) -> Either<Res, Err>:
  cases (List) lst:
    | empty => left(base)
    | link(head, tail) =>
      cases (Either) f(base, head):
        | left(ok) => fold-either(f, ok, tail)
        | right(err) => right(err)
      end
  end
end

fun fold-either2<A, B, Res, Err>(
    f :: (Res, A, B -> Either<Res, Err>),
    base :: Res,
    lst1 :: List<A>,
    lst2 :: List<B>) -> Either<Res, Err>:
  cases (List) lst1:
    | empty => left(base)
    | link(head1, tail1) =>
      cases (List) lst2:
        | empty => left(base)
        | link(head2, tail2) =>
          cases (Either) f(base, head1, head2):
            | left(ok) => fold-either2(f, ok, tail1, tail2)
            | right(err) => right(err)
          end
      end
  end
end


check:
  match-pattern(
    parse-ast("(hello {some jack} [[1 2] [3 4]])"),
    parse-pattern(none, "(hello {some j} [[a b] ...])"))
    is left({environment(
        [string-dict: "j", g-var(naked-var("jack"))],
        [string-dict: ],
        [string-dict: "l1", [list:
            environment(
              [string-dict: "a", g-value(e-num(1)), "b", g-value(e-num(2))],
              [string-dict: ],
              [string-dict: ]),
            environment(
              [string-dict: "a", g-value(e-num(3)), "b", g-value(e-num(4))],
              [string-dict: ],
              [string-dict: ])]]);
      pat-surf("hello", [list:
          pat-option(some(pat-pvar("j", none))),
          pat-list(seq-ellipsis-list(
              [list:
                pat-list(seq-cons(pat-pvar("a", none), seq-cons(pat-pvar("b", none), seq-empty))),
                pat-list(seq-cons(pat-pvar("a", none), seq-cons(pat-pvar("b", none), seq-empty)))],
              "l1"))
        ])
    })
  
  match-pattern(
    g-list([list:
        parse-ast("p"),
        g-tag(parse-pattern(none, "1"), parse-pattern(none, "2"),
          g-tag(parse-pattern(none, "x"), parse-pattern(none, "y"),
            parse-ast("q")))
      ]),
    parse-pattern(none, "[a ...]"))
    is left({environment(
        [string-dict: ],
        [string-dict: ],
        [string-dict: "l1", [list:
            environment(
              [string-dict: "a", parse-ast("p")],
              [string-dict: ],
              [string-dict: ]),
            environment(
              [string-dict: "a", parse-ast("q")],
              [string-dict: ],
              [string-dict: ])]]);
      pat-list(seq-ellipsis-list(
          [list:
            parse-pattern(none, "a"),
            pat-tag(parse-pattern(none, "1"), parse-pattern(none, "2"),
              pat-tag(parse-pattern(none, "x"), parse-pattern(none, "y"),
                parse-pattern(none, "a")))
          ], "l1"))})
  
  match-pattern(
    parse-ast("{some foobar}"),
    pat-fresh([set: "a"], parse-pattern(some([set:]), "{some a}")))
    is left({environment(
        [string-dict: ],
        [string-dict: "a", naked-var("foobar")],
        [string-dict: ]);
      pat-fresh([set: "a"], parse-pattern(some([set:]), "{some foobar}"))})
  
  match-pattern(
    parse-ast("(Foo [1 2] [3 4])"), 
    resolve-ellipses(parse-pattern(none, "(Foo [a ...] [a ...])")))
    is right(m-error-pvar("a", g-value(e-num(3)), g-value(e-num(1))))
end
