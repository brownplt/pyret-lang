#lang pyret

provide *
provide-types *

import string-dict as D
import equality as E
import srcloc as S

dummy-loc = S.builtin("dummy location")


### Utility ###

data Pair<A, B>:
  | pair(left :: A, right :: B)
end

fun bool-to-eq(b):
  if b:
    E.Equal
  else:
    E.NotEqual("")
  end
end

fun bool-opt(bool):
  if bool:
    some(nothing)
  else:
    none
  end
end

fun try-opt(func, opt):
  cases(Option) opt:
    | none      => none
    | some(val) => func(val)
  end
end

fun try-bool-opt(func, bool):
  try-opt(func, bool-opt(bool))
end

fun try-opts(func, default, lst):
  for fold(res from some(default), elem from lst):
    cases(Option) res:
      | none      => none
      | some(val) => func(val, elem)
    end
  end
end

fun dict-union(d1, d2):
  for fold(d from d1, key from d2.keys().to-list()):
    d.set(key, d2.get-value(key))
  end
end


### Atoms ###

type Name = String
type Id = Number # zero means 'free'

var NEXT_ID = 0

fun fresh-id():
  NEXT_ID := NEXT_ID + 1
  NEXT_ID
end

fun reset-ids():
  NEXT_ID := 0
end

data Var:
  | mk-var(name :: Name, id :: Id)
sharing:
  _tostring(self, shadow tostring):
    self.name
  end,
  _torepr(self, shadow torepr):
    self.name + ":" + tostring(self.id)
  end,
  freshen(self):
    mk-var(self.name, fresh-id())
  end,
  unfreshen(self):
    mk-var(self.name, 0)
  end,
  rename(self):
    if self.id == 0:
      self
    else:
      mk-var(self.name + "_" + tostring(self.id), 0)
    end
  end
end

fun free-var(name :: Name) -> Var:
  mk-var(name, 0)
end

fun var-to-str(v :: Var) -> String:
  v.name + ":" + num-to-string(v.id)
end

fun str-to-var(str :: String) -> Var:
  parts = string-split(str, ":")
  mk-var(parts.get(0), string-to-number(parts.get(1)).value)
end


### Permutations ###

type Permutation = D.StringDict<String>

fun permute-var(p :: Permutation, v :: Var) -> Var:
  cases(Option) p.get(var-to-str(v)):
    | none      => v
    | some(str) => str-to-var(str)
  end
end

fun permute-id(p :: Permutation, id :: Id) -> Id:
  cases(Option) p.get(num-to-string(id)):
    | none      => id
    | some(str) => string-to-number(str)
  end
end

p-empty = D.make-string-dict()

fun p-inverse(p :: Permutation) -> Permutation:
  for fold(inv from D.make-string-dict(), key from p.keys().to-list()):
    inv.set(key, p.get-value(key))
  end
end

fun p-node-swap(id1 :: Id, id2 :: Id) -> Permutation:
  shadow id1 = num-to-string(id1)
  shadow id2 = num-to-string(id2)
  if id1 == id2:
    p-empty
  else:
    D.make-string-dict()
    .set(id1, id2)
    .set(id2, id1)
  end
end

fun p-var-swap(v1 :: Var, v2 :: Var) -> Permutation:
  shadow v1 = var-to-str(v1)
  shadow v2 = var-to-str(v2)
  if v1 == v2:
    p-empty
  else:
    D.make-string-dict()
    .set(v1, v2)
    .set(v2, v1)
  end
end

fun p-compose(p1 :: Permutation, p2 :: Permutation) -> Permutation:
  fun apply(p, x):
    cases(Option) p.get(x):
      | none    => x
      | some(y) => y
    end
  end
  domain = p1.keys().union(p2.keys()).to-list()
  for fold(comp from p-empty, key from domain):
    comp.set(key, apply(p2, apply(p1, key)))
  end
end

fun p-union(p1 :: Permutation, p2 :: Permutation) -> Option<Permutation>:
  fun apply(p, x):
    cases(Option) p.get(x):
      | none    => x
      | some(y) => y
    end
  end
  domain = p1.keys().union(p2.keys()).to-list()
  for try-opts(union from p-empty, key from domain):
    x = p1.get(key)
    y = p2.get(key)
    if is-none(x):
      some(union.set(key, y.value))
    else if is-none(y):
      some(union.set(key, x.value))
    else if x.value == y.value:
      some(union.set(key, x.value))
    else:
      none
    end
  end
end


### Terms ###

data Term:
  | t-decl(v :: Var)
  | t-refn(v :: Var)
  | t-val(val :: Any)
  | t-node(con :: String, id :: Id, loc :: S.Srcloc, subterms :: List<Term>)
    with:
    get(self, n):
      self.subterms.get(n)
    end
  | t-hole(index :: Number)
  | t-tag(lhs :: Term, rhs :: Term, term :: Term)
sharing:
  _tostring(self, tostr):
    cases(Term) self:
      | t-decl(v)       => "$" + tostr(v)
      | t-refn(v)       => tostr(v)
      | t-val(val)      => "<" + tostr(val) + ">"
      | t-hole(i)       => "@" + tostr(i)
      | t-node(con, _, _, ts) =>
        con + "(" + map(tostr, ts).join-str(", ") + ")"
      | t-tag(left, right, term) =>
        "TAG[" + tostr(left) + ", " + tostr(right) + "] " + tostr(term)
    end
  end,
  _torepr(self, shadow torepr):
    cases(Term) self:
      | t-decl(v)       => "$" + torepr(v)
      | t-refn(v)       => torepr(v)
      | t-val(val)      => "<" + tostring(val) + ">"
      | t-hole(i)       => "@" + tostring(i)
      | t-node(con, id, _, ts) =>
        con + ":" + tostring(id) + "(" + map(torepr, ts).join-str(",") + ")"
      | t-tag(left, right, t) =>
        "TAG[" + torepr(left) + ", " + torepr(right) + "] " + torepr(t)
    end
  end,
  strip-tags(self):
    cases(Term) self:
      | t-decl(_)              => self
      | t-refn(_)              => self
      | t-val(_)               => self
      | t-hole(_)              => self
      | t-node(con, id, l, ts) => t-node(con, id, l, map(_.strip-tags(), ts))
      | t-tag(_, _, term)      => term.strip-tags()
    end
  end
end


fun permute-term(bij :: Permutation, t :: Term) -> Term:
  cases(Term) t:
    | t-decl(v)              => t-decl(permute-var(bij, v))
    | t-refn(v)              => t-refn(permute-var(bij, v))
    | t-val(_)               => t
    | t-hole(i)              => t
    | t-node(con, id, l, ts) =>
      t-node(con, permute-id(bij, id), l, map(permute-term(bij, _), ts))
    | t-tag(_, _, _)         => raise("Do not know how to permute term with tags")
  end
end

# TODO: what if the terms aren't resolved?
fun find-iso(t1 :: Term, t2 :: Term) -> Option<Permutation>:
  when is-t-tag(t1) or is-t-tag(t2):
    raise("Isomorphism checking: cannot compare tagged terms.")
  end
  ask:
    | is-t-decl(t1) and is-t-decl(t2) then: some(p-var-swap(t1.v, t2.v))
    | is-t-refn(t1) and is-t-refn(t2) then: some(p-var-swap(t1.v, t2.v))
    | is-t-hole(t1) and is-t-hole(t2) then:
      for try-bool-opt(_ from t1.index == t2.index):
        some(p-empty)
      end
    | is-t-val(t1) and is-t-val(t2) then:
      if t1.val == t2.val:
        some(p-empty)
      else:
        none
      end
    | is-t-node(t1) and is-t-node(t2) then:
      for try-bool-opt(_ from t1.con == t2.con):
        for try-bool-opt(_ from t1.subterms.length() ==
              t2.subterms.length()):
          for try-opts(bij from p-empty, n from range(0,
                t1.subterms.length())):
            for try-opt(bij2 from find-iso(t1.subterms.get(n),
                  t2.subterms.get(n))):
              p-union(bij, bij2)
            end
          end
        end
      end
    | otherwise:
      none
  end
end

fun iso(t1 :: Term, t2 :: Term) -> Boolean:
  is-some(find-iso(t1, t2))
end


### Signatures ###

data Sig:
  | s-empty
  | s-child(child :: Number)
  | s-compose(sig1 :: Sig, sig2 :: Sig)
  | s-union(sig1 :: Sig, sig2 :: Sig)
end

fun apply-sig(sig :: Sig, outs :: List<Permutation>) -> Permutation:
  cases(Sig) sig:
    | s-empty               => p-empty
    | s-child(i)            => outs.get(i)
    | s-compose(sig1, sig2) =>
      p-compose(apply-sig(sig1, outs), apply-sig(sig2, outs))
    | s-union(sig1, sig2)   =>
      opt = p-union(apply-sig(sig1, outs), apply-sig(sig2, outs))
      cases(Option) opt:
        | some(bij) => bij
        | none      =>
          raise("Overlapping variable declarations when applying scope signature.")
      end
  end
end

data Signature:
  | signature(childs :: List<Sig>, out :: Sig)
end

type SignatureSet = D.StringDict<Signature>

data TermOut:
  | term-out(term :: Term, out :: Permutation)
end

fun resolve(ss :: SignatureSet, t :: Term) -> Term:
  # TODO: check for unbound variables
  fun recur(shadow t :: Term) -> TermOut:
    cases(Term) t:
      | t-decl(v) =>
        old-var = v
        new-var = v.freshen()
        term-out(t-decl(new-var), p-var-swap(old-var, new-var))
      | t-refn(v) => term-out(t, p-empty)
      | t-hole(i) => term-out(t, p-empty)
      | t-val(_)  => term-out(t, p-empty)
      | t-node(con, id, l, ts) =>
        sig = cases(Option) ss.get(con):
          | none => raise("No signature found for " + con)
          | some(sig) => sig
        end
        tos = map(recur, ts)
        os = map(_.out, tos)
        shadow ts = map(_.term, tos)
        shadow ts = for map_n(n from 0, shadow t from ts):
          permute-term(apply-sig(sig.childs.get(n), os), t)
        end
        term-out(t-node(con, fresh-id(), l, ts), apply-sig(sig.out,
            os))
      | t-tag(_, _, _) =>
        raise("Scope resolution: cannot resolve tagged term.")
    end
  end
  recur(t).term
end

fun unresolve(ss :: SignatureSet, t :: Term) -> Term:
  # TODO: Make this efficient
  fun exports(shadow t :: Term) -> Permutation:
    cases(Term) t:
      | t-decl(v) =>
        old-var = v
        new-var = v.unfreshen()
        p-var-swap(old-var, new-var)
      | t-refn(v) => p-empty
      | t-hole(i) => p-empty
      | t-val(_)  => p-empty
      | t-node(con, id, _, ts) =>
        sig = cases(Option) ss.get(con):
          | none => raise("No signature found for " + con)
          | some(sig) => sig
        end
        os = map(exports, ts)
        apply-sig(sig.out, os)
      | t-tag(_, _, _) =>
        raise("Scope unresolution: cannot unresolve tagged term.")
    end
  end
  fun unresolve-1(shadow t :: Term) -> TermOut:
    cases(Term) t:
      | t-decl(v) =>
        old-var = v
        new-var = v.unfreshen()
        term-out(t-decl(new-var), p-var-swap(old-var, new-var))
      | t-refn(v) => term-out(t, p-empty)
      | t-hole(i) => term-out(t, p-empty)
      | t-val(_)  => term-out(t, p-empty)
      | t-node(con, id, l, ts) =>
        sig = cases(Option) ss.get(con):
          | none => raise("No signature found for " + con)
          | some(sig) => sig
        end
        os        = map(exports, ts)
        shadow ts = for map_n(n from 0, shadow t from ts):
          unresolve-1(permute-term(apply-sig(sig.childs.get(n), os),
              t)).term
        end
        term-out(t-node(con, 0, l, ts), apply-sig(sig.out, os))
      | t-tag(_, _, _) =>
        raise("Scope unresolution: cannot unresolve tagged term.")
    end
  end
  fun unresolve-2(shadow t :: Term) -> TermOut:
    cases(Term) t:
      | t-decl(v) =>
        old-var = v
        new-var = v.rename()
        term-out(t-decl(new-var), p-var-swap(old-var, new-var))
      | t-refn(v) => term-out(t, p-empty)
      | t-hole(i) => term-out(t, p-empty)
      | t-val(_)  => term-out(t, p-empty)
      | t-node(con, id, l, ts) =>
        sig = cases(Option) ss.get(con):
          | none => raise("No signature found for " + con)
          | some(sig) => sig
        end
        tos = map(unresolve-2, ts)
        os        = map(_.out, tos)
        shadow ts = map(_.term, tos)
        shadow ts = for map_n(n from 0, shadow t from ts):
          permute-term(apply-sig(sig.childs.get(n), os), t)
        end
        term-out(t-node(con, id, l, ts), apply-sig(sig.out, os))
      | t-tag(_, _, _) =>
        raise("Scope unresolution: cannot unresolve tagged term.")
    end
  end
  unresolve-2(unresolve-1(resolve(ss, t)).term).term
end



### Match / Substitute ###

type Subs = D.StringDict<Term>

fun subs-union(s1 :: Subs, s2 :: Subs) -> Option<Subs>:
  repetition = for fold(repetition from false, key from
      s2.keys().to-list()):
    repetition or s1.has-key(key)
  end
  if repetition:
    none
  else:
    some(dict-union(s1, s2))
  end
end

data MatchResult:
  | match-result(bij :: Permutation, subs :: Subs)
end

empty-match-result = match-result(p-empty, D.make-string-dict())

fun match-union(m1 :: MatchResult, m2 :: MatchResult) -> Option<MatchResult>:
  for try-opt(bij from p-union(m1.bij, m2.bij)):
    for try-opt(subs from subs-union(m1.subs, m2.subs)):
      some(match-result(bij, subs))
    end
  end
end

fun _match(t :: Term, ctx :: Term) -> Option<Subs>:
  fun recur(shadow t :: Term, shadow ctx :: Term):
    #    when is-t-tag(t) or is-t-tag(ctx):
    #      raise("Matching: cannot compare tagged terms.")
    #    end
    ask:
      | is-t-hole(ctx) then:
        some(match-result(p-empty, [D.string-dict:
              num-to-string(ctx.index), t]))
      | is-t-decl(t) and is-t-decl(ctx) then:
        some(match-result(p-var-swap(t.v, ctx.v),
            D.make-string-dict()))
      | is-t-refn(t) and is-t-refn(ctx) then:
        some(match-result(p-var-swap(t.v, ctx.v),
            D.make-string-dict()))
      | is-t-val(ctx) and is-t-val(t) then:
        if ctx.val == t.val:
          some(empty-match-result)
        else:
          none
        end
      | is-t-tag(t) and is-t-tag(ctx) then:
        for try-opt(res-l from recur(t.lhs, ctx.lhs)):
          for try-opt(res-r from recur(t.rhs, ctx.rhs)):
            for try-opt(res from recur(t.term, ctx.term)):
              match-union(res-l, match-union(res-r, res))
            end
          end
        end
      | is-t-node(t) and is-t-node(ctx) then:
        for try-bool-opt(_ from t.con == ctx.con):
          for try-bool-opt(_ from t.subterms.length() ==
                ctx.subterms.length()):
            for try-opts(res from empty-match-result, n from
                range(0, t.subterms.length())):
              for try-opt(res2 from recur(t.subterms.get(n),
                    ctx.subterms.get(n))):
                match-union(res, res2)
              end
            end
          end
        end
      | otherwise:
        none
    end
  end
  recur(t, ctx).and-then(_.subs)
end

fun substitute(subs :: Subs, ctx :: Term) -> Term:
  fun recur(shadow ctx :: Term):
    cases(Term) ctx:
      | t-hole(i) => subs.get-value(num-to-string(i))
      | t-decl(_) => ctx
      | t-refn(_) => ctx
      | t-val(_)  => ctx
      | t-node(con, id, l, ctxs) =>
        t-node(con, id, l, map(recur, ctxs))
      | t-tag(_, _, _) =>
        raise("Substitute: cannot substitute into tagged term.")
    end
  end
  recur(ctx)
end


### Desugar / Resugar ###

type Sugar = D.StringDict<(Term -> Term)>

fun head-of-term(sugar :: Sugar, con, id, l, ts) -> Term:
  var hole-id = 0
  fun new-hole():
    hole-id := hole-id + 1
    t-hole(hole-id)
  end
  fun recur(t :: Term):
    cases(Term) t:
      | t-decl(_) => new-hole()
      | t-refn(_) => new-hole()
      | t-val(_)  => new-hole()
      | t-node(shadow con, shadow id, shadow l, shadow ts) =>
        cases(Option) sugar.get(con):
          | some(_) => new-hole()
          | none    => t-node(con, id, l, map(recur, ts))
        end
      | t-tag(_, _, _) =>
        raise("Head: cannot take the head of tagged term.")
    end
  end
  t-node(con, id, l, map(recur, ts))
end

fun desugar(ss :: SignatureSet, sugar :: Sugar, t :: Term) -> Term:
  fun desugar-subs(subs):
    for fold(result from D.make-string-dict(), key from
        subs.keys().to-list()):
      result.set(key, desugar(ss, sugar, subs.get-value(key)))
    end
  end
  cases(Term) t:
    | t-decl(_) => t
    | t-refn(_) => t
    | t-val(_)  => t
    | t-node(con, id, l, ts) =>
      cases(Option) sugar.get(con):
        | some(rule) =>
          lhs-ctx = head-of-term(sugar, con, id, l, ts)
          rhs-ctx = resolve(ss, rule(lhs-ctx))
          subs = _match(t, lhs-ctx).value
          new-term = substitute(desugar-subs(subs), rhs-ctx)
          t-tag(lhs-ctx, rhs-ctx, new-term)
        | none       =>
          raise("Desugar: no desugaring rule found for " + con)
      end
    | t-tag(_, _, _) =>
      raise("Desugar: unexpected tagged term.")
  end
end

fun resugar(sugar :: Sugar, t :: Term) -> Option<Term>:
  fun resugar-subs(subs):
    for fold(result from some(D.make-string-dict()),
        key from subs.keys().to-list()):
      cases(Option) result:
        | none => none
        | some(shadow result) =>
          cases(Option) resugar(sugar, subs.get-value(key)):
            | none => none
            | some(shadow t) => some(result.set(key, t))
          end
      end
    end
  end
  cases(Term) t:
    | t-decl(_) => some(t)
    | t-refn(_) => some(t)
    | t-val(_)  => some(t)
    | t-node(con, id, _, ts) => none
    | t-tag(lhs, rhs, shadow t) =>
      cases(Option) _match(t, rhs):
        | none       => none
        | some(subs) =>
          cases(Option) resugar-subs(subs):
            | none => none
            | some(shadow subs) => some(substitute(subs, lhs))
          end
      end
  end
end


### Exports ###

fun node(con, l, ts): t-node(con, 0, l, ts) end
fun value(val): t-val(val) end


# QUESTION:
# Why can't there be a check block here?
#   Generates the following error:
#   TypeError: Object #<Object> has no method 'makeList'

# ### Tests ###

# check "Resolve and Unresolve":
#   lam-sig = signature([list: s-empty, s-child(0)], s-empty)
#   sigs = [D.string-dict: "lam", lam-sig]
  
#   X = free-var("x")
#   Y = free-var("y")
  
#   fun lam-node(param, body):
#     t-node("lam", 0, dummy-loc, [list: t-decl(param), body])
#   end
#   term1 = resolve(sigs, lam-node(X, lam-node(X, t-refn(X))))
#   term2 = resolve(sigs, lam-node(Y, lam-node(Y, t-refn(Y))))
#   term3 = resolve(sigs, lam-node(X, lam-node(Y, t-refn(X))))
#   term4 = resolve(sigs, lam-node(X, lam-node(Y, t-refn(Y))))
  
#   term1 is%(iso) term1
#   term1 is%(iso) term2
#   term1 is-not%(iso) term3
#   term1 is%(iso) term4
  
#   unresolve(sigs, term1) is lam-node(X, lam-node(X, t-refn(X)))
#   # TODO: more unresolve tests
# end

# check "Match/Subs":
#   lam-sig = signature([list: s-empty, s-child(0)], s-empty)
#   lam2-sig = signature([list: s-empty, s-empty, s-union(s-child(0),
#         s-child(1))], s-empty)
#   sigs = [D.string-dict: "lam", lam-sig, "lam2", lam2-sig]
  
#   X = free-var("x")
#   Y = free-var("y")
  
#   fun lam-node(param, body):
#     t-node("lam", 0, dummy-loc, [list: param, body])
#   end
  
#   fun lam2-node(param1, param2, body):
#     t-node("lam2", 0, dummy-loc, [list: param1, param2, body])
#   end
  
#   term1 = resolve(sigs, lam-node(t-decl(X), lam-node(t-decl(Y), t-refn(X))))
#   term2 = resolve(sigs, lam2-node(t-decl(X), t-decl(Y), t-refn(X)))
#   ctx1  = resolve(sigs, lam-node(t-hole(1), lam-node(t-hole(2), t-hole(3))))
#   ctx2  = resolve(sigs, lam2-node(t-hole(1), t-hole(2), t-hole(3)))
  
#   substitute(_match(term1, ctx1).value, ctx2) is%(iso) term2
#   substitute(_match(term2, ctx2).value, ctx1) is%(iso) term1
#   _match(term2, ctx1) is none
# end

# data Closure:
#   | closure(v :: Var, body :: Term, env :: D.StringDict)
# sharing:
#   _tostring(self, tostr):
#     "closure($" + self.v.name + ", " + tostr(self.body) + ")"
#   end,
#   _equals(self, other, eq):
#     E.NotEquals
#   end
# end

# check "Small Lang Test":
#   sigs = [D.string-dict:
#     "lam", signature([list: s-empty, s-child(0)], s-empty),
#     "app", signature([list: s-empty, s-empty], s-empty),
#     "plus", signature([list: s-empty, s-empty], s-empty),
#     "or", signature([list: s-empty, s-empty], s-empty),
#     "if", signature([list: s-empty, s-empty, s-empty], s-empty),
#     "let", signature([list: s-empty, s-empty, s-child(0)], s-empty),
#     "num", signature([list: s-empty], s-empty)]
  
#   fun interp(term, env, emit, tagged-emit):
#     cases(Term) term:
#       | t-val(val) =>
#         val
#       | t-refn(v)  =>
#         tagged-emit(term)
#         env.get-value(var-to-str(v))
#       | t-tag(left, right, shadow term) =>
#         fun frame(t): tagged-emit(t-tag(left, right, t)) end
#         interp(term, env, emit, frame)
#       | t-node(con, id, l, ts) =>
#         ask:
#           | con == "num" then:
#             tagged-emit(term)
#             interp(ts.get(0), env, emit, emit)
#           | con == "plus" then:
#             fun frame-x(t): tagged-emit(t-node(con, id, l, [list:
#                   t, ts.get(1)])) end
#             x = interp(ts.get(0), env, frame-x, frame-x)
#             fun frame-y(t): tagged-emit(t-node(con, id, l, [list:
#                   t-val(x), t])) end
#             y = interp(ts.get(1), env, frame-y, frame-y)
#             frame-y(t-val(y))
#             x + y
#           | con == "if" then:
#             fun frame(t): tagged-emit(t-node(con, id, l, [list: t,
#                   ts.get(1), ts.get(2)])) end
#             cond = interp(ts.get(0), env, frame, frame)
#             frame(t-val(cond))
#             if cond == 0:
#               interp(ts.get(1), env, emit, emit)
#             else:
#               interp(ts.get(2), env, emit, emit)
#             end
#           | con == "lam" then:
#             tagged-emit(term)
#             closure(ts.get(0).v, ts.get(1), env)
#           | con == "app" then:
#             fun frame-clos(t): tagged-emit(t-node(con, id, l,
#                 [list: t, ts.get(1)])) end
#             clos = interp(ts.get(0), env, frame-clos, frame-clos)
#             fun frame-arg(t): tagged-emit(t-node(con, id, l,
#                 [list: t-val(clos), t])) end
#             arg  = interp(ts.get(1), env, frame-arg, frame-arg)
#             frame-arg(t-val(arg))
#             interp(clos.body, clos.env.set(var-to-str(clos.v),
#                 arg), emit, emit)
#         end
#     end
#   end
  
#   X = free-var("x")
#   Y = free-var("y")
  
#   fun num-node(n):
#     t-node("num", 0, dummy-loc, [list: t-val(n)])
#   end
#   fun lam-node(param, body):
#     t-node("lam", 0, dummy-loc, [list: param, body])
#   end
#   fun app-node(func, arg):
#     t-node("app", 0, dummy-loc, [list: func, arg])
#   end
#   fun plus-node(left, right):
#     t-node("plus", 0, dummy-loc, [list: left, right])
#   end
#   fun if-node(cond, consq, altern):
#     t-node("if", 0, dummy-loc, [list: cond, consq, altern])
#   end
#   fun let-node(param, val, body):
#     t-node("let", 0, dummy-loc, [list: param, val, body])
#   end
  
#   fun desugar-let(ctx):
#     app-node(lam-node(ctx.get(0), ctx.get(2)), ctx.get(1))
#   end
  
#   fun desugar-or(ctx):
#     let-node(t-decl(X), ctx.get(0), if-node(t-refn(X), t-refn(X), ctx.get(1)))
#   end
  
#   fun desugar-plus(ctx):
#     ctx
#   end
  
#   sugar = [D.string-dict:
#     "let", desugar-let,
#     "or",  desugar-or,
#     "plus", desugar-plus]
  
#   term1 = resolve(sigs, app-node(
#       if-node(num-node(17), num-node(17),
#         lam-node(t-decl(X), plus-node(t-refn(X), num-node(1)))),
#       num-node(2)))
  
#   var STEPS = [list:]
#   fun show-core-step(t):
#     STEPS := STEPS + [list: tostring(t)]
#   end
#   fun show-surf-step(t):
#     cases(Option) resugar(sugar, t):
#       | none           => nothing
#       | some(shadow t) =>
#         STEPS := STEPS + [list: tostring(t)]
#     end
#   end
  
#   interp(term1, D.make-string-dict(), show-core-step, show-core-step)
#   STEPS.get(0) is "app(if(num(<17>), num(<17>), lam($x, plus(x, num(<1>)))), num(<2>))"
#   STEPS.get(1) is "app(if(<17>, num(<17>), lam($x, plus(x, num(<1>)))), num(<2>))"
#   STEPS.get(2) is "app(lam($x, plus(x, num(<1>))), num(<2>))"
#   STEPS.get(3) is "app(<closure($x, plus(x, num(<1>)))>, num(<2>))"
#   STEPS.get(4) is "app(<closure($x, plus(x, num(<1>)))>, <2>)"
#   STEPS.get(5) is "plus(x, num(<1>))"
#   STEPS.get(6) is "plus(<2>, num(<1>))"
#   STEPS.get(7) is "plus(<2>, <1>)"
  
#   term2 = resolve(sigs,
#     let-node(t-decl(X), num-node(3), t-refn(X)))
#   term2-expected = resolve(sigs,
#     app-node(lam-node(t-decl(X), t-refn(X)), num-node(3)))
#   desugar(sigs, sugar, term2).strip-tags() is%(iso) term2-expected
  
#   term3 = resolve(sigs,
#     let-node(t-decl(X), num-node(5),
#       let-node(t-decl(Y), plus-node(num-node(-6), t-refn(X)),
#         let-node(t-decl(X), plus-node(t-refn(X), t-refn(Y)),
#           plus-node(t-refn(X), t-refn(Y))))))
#   term3-des = desugar(sigs, sugar, term3)
  
#   STEPS := [list:]
#   interp(term3-des, D.make-string-dict(), show-surf-step, show-surf-step)
#   STEPS.get(0) is "let($x, num(<5>), let($y, plus(num(<-6>), x), let($x, plus(x, y), plus(x, y))))"
#   STEPS.get(1) is "let($y, plus(num(<-6>), x), let($x, plus(x, y), plus(x, y)))"
#   STEPS.get(2) is "let($x, plus(x, y), plus(x, y))"
#   STEPS.get(3) is "plus(x, y)"
#   STEPS.get(4) is "plus(<4>, y)"
#   STEPS.get(5) is "plus(<4>, <-1>)"
# end

# check "Lang Test":
#   sigs = [D.string-dict:
#     "lam", signature([list: s-empty, s-child(0)], s-empty),
#     "app", signature([list: s-empty, s-empty], s-empty),
#     "let", signature([list: s-empty, s-child(0)], s-empty),
#     "args", signature([list: s-empty, s-empty], s-empty),
#     "lam-bind", signature([list: s-empty, s-empty],
#       s-union(s-child(0), s-child(1))),
#     "let-bind", signature([list: s-empty, s-empty, s-empty],
#       s-union(s-child(0), s-child(2))),
#     "let-rec-bind", signature(
#       [list: s-empty, s-union(s-child(0), s-child(2)), s-child(0)],
#       s-union(s-child(0), s-child(2))),
#     "let-star-bind", signature([list: s-empty, s-empty, s-child(0)],
#       s-union(s-child(0), s-child(2))),
#     "no-bind", signature([list:], s-empty)]
  
#   #  fun desugar-let(ctx :: Term) -> Term:
#   #    t-node("app", 0, [list: let-binds-to-lam-binds(ctx.get(0)), ctx])
#   #  end
  
#   nothing
# end
