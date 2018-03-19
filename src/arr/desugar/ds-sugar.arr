provide {
    desugar: desugar,
    resugar: resugar
} end

include either
include file("ds-structs.arr")
include file("ds-parse.arr")
include file("ds-environment.arr")
include file("ds-substitute.arr")
include file("ds-match.arr")

fun find-ds-rule(rules :: List<DsRule>, op :: String) -> Option<DsRule>:
  for find(rule from rules):
    rule.op == op
  end
end

fun generate-pvars(n :: Number) -> List<Pattern>:
  range(0, n).map(lam(i): pat-pvar("_" + tostring(i), none) end)
end

fun chain-option<A, B>(
    f :: (A -> Option<B>),
    arg :: Option<A>)
  -> Option<B>:
  cases (Option) arg:
    | none => none
    | some(v) => f(v)
  end
end

fun find-option<A, B>(f :: (A -> Option<B>), lst :: List<A>) -> Option<B>:
  cases (List) lst:
    | empty => none
    | link(h, t) => 
      cases (Option) f(h):
        | none => find-option(f, t)
        | some(v) => some(v)
      end
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
  
fun desugar(rules :: List<DsRule>, e :: Term) -> Term:
  fun desugars(es :: List<Term>) -> List<Term>:
    es.map(desugar(rules, _))
  end
  cases (Term) e:
    | g-value(val) => g-value(val)
    | g-core(op, loc, args) => g-core(op, loc, desugars(args))
    | g-aux(op,  loc, args) => g-aux(op,  loc, desugars(args))
    | g-var(v) => g-var(v)
    | g-list(lst) => g-list(desugars(lst))
    | g-option(opt) => g-option(opt.and-then(desugar(rules, _)))
    | g-tag(lhs, rhs, body) => g-tag(lhs, rhs, desugar(rules, body))
    | g-surf(op, loc, args) =>
      shadow args = desugars(args)
      opt-rule = find-ds-rule(rules, op)
      cases (Option) opt-rule:
        | none =>
          pvars = generate-pvars(args.length())
          pat-lhs = pat-surf(op, pvars)
          pat-rhs = pat-core(op, pvars)
          g-tag(pat-lhs, pat-rhs, g-core(op, loc, args))
        | some(rule) =>
          opt = for find-option(kase from rule.kases):
            cases (Either) match-pattern(g-surf(op, loc, args), kase.lhs):
              | left({env; p}) => some({kase; env; p})
              | right(_) => none
            end
          end
          cases (Option) opt:
            | none => fail("No cases matched for sugar '" + op + "'.")
            | some({kase; env; pat-lhs}) =>
              g-tag(pat-lhs, kase.rhs,
                desugar(rules, substitute-pattern(env, kase.rhs)))
          end
      end
  end
end

check:
  rules = parse-ds-rules(
    ```
    sugar or:
    | (or a:Expr b) => (fresh [x] (let (bind x a) (if x x b)))
    end
    ```)
  e = parse-ast("(or p q)")
  desugar(rules, e) does-not-raise
  
  
end

fun strip-tags(e :: Term) -> Term:
  cases (Term) e:
    | g-value(val) => g-value(val)
    | g-core(op, loc, args) => g-core(op, loc, args.map(strip-tags))
    | g-aux(op, loc, args) => g-aux(op, loc, args.map(strip-tags))
    | g-surf(op, loc, args) => g-surf(op, loc, args.map(strip-tags))
    | g-list(seq) => g-list(seq.map(strip-tags))
    | g-option(opt) => g-option(opt.and-then(strip-tags))
    | g-var(v) => g-var(v)
    | g-tag(_, _, body) => strip-tags(body)
  end
end

fun resugar(e :: Term) -> Option<Term>:
  cases (Term) e:
    | g-value(val) => g-value(val) ^ some
    | g-core(op, loc, args) =>
      map-option(resugar, args).and-then(g-core(op, loc, _))
    | g-surf(op, loc, args) =>
      map-option(resugar, args).and-then(g-surf(op, loc, _))
    | g-aux(op,  loc, args) => 
      map-option(resugar, args).and-then(g-aux(op, loc, _))
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
        cases (Either) match-pattern(body, rhs):
          | left({env; _}) => resugar(substitute-pattern(env, lhs))
          | right(_) => none
        end
      end
  end
end

check:
  rules = parse-ds-rules(
    ```
    sugar or:
    | (or a:Expr b) => (fresh [x] (let (bind x a) (if x x b)))
    end
    sugar bind:
    | (bind x a) => {bind x a}
    end
    sugar let:
    | (let {bind x a} body) => (apply (lambda x body) a)
    end
    ```)
  
  e = parse-ast("(or p q)")
  resugar(desugar(rules, e)) is some(e)
  
  e1 = parse-ast("(bind 1 (+))")
  resugar(desugar(rules, e1)) is some(e1)
  
  e2 = parse-ast("(let (bind x (+ 1 2)) (- x 3))")
  resugar(desugar(rules, e2)) is some(e2)
end
