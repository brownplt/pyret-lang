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
    | g-surf(op, args) => desugar-eval(rules, op, desugars(args))
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
        cases (Either) match-pattern(body, rhs):
          | left({env; _}) => resugar(substitute-pattern(env, lhs)) # arity mismatch
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
