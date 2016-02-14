#lang pyret

provide *
provide-types *
import ast as A
import sets as S
import "compiler/ast-anf.arr" as N
import "compiler/js-ast.arr" as J
import "compiler/gensym.arr" as G
import "compiler/compile-structs.arr" as CS
import "compiler/concat-lists.arr" as CL
#import "compiler/live-ranges.arr" as LR
import string-dict as D
import srcloc as SL

type ConcatList = CL.ConcatList
type NameSet = D.MutableStringDict<A.Name>
type FrozenNameSet = D.StringDict<A.Name>

cl-sing = CL.concat-singleton
cl-empty = CL.concat-empty
cl-snoc = CL.concat-snoc
cl-cons = CL.concat-cons
ns-empty = D.make-mutable-string-dict

fun difference(s1 :: FrozenNameSet, s2 :: FrozenNameSet):
  s1-unfrozen-copy = s1.unfreeze()
  remove-overlap-now(s1-unfrozen-copy, s2.unfreeze())
  s1-unfrozen-copy.freeze()
end

fun copy-nameset(s :: NameSet) -> NameSet:
  s.freeze().unfreeze()
end

# does NOT mutate s1 or s2
fun difference-now(s1 :: NameSet, s2 :: NameSet) -> NameSet:
  s1-copy = copy-nameset(s1)
  remove-overlap-now(s1-copy, s2)
  s1-copy
end

# !mutates s1
fun remove-overlap-now(s1 :: NameSet, s2 :: NameSet) -> Nothing:
  for each(k2 from s2.keys-list-now()):
    s1.remove-now(k2)
  end
end

data GraphNode:
  | node(_from :: J.Label, _to :: ConcatList<J.Label>, case-body :: J.JCase,
      ref free-vars :: NameSet,
      ref used-vars :: NameSet,
      ref decl-vars :: NameSet,
      ref live-vars :: Option<NameSet>,
      ref live-after-vars :: Option<NameSet>,
      ref dead-vars :: Option<NameSet>,
      ref dead-after-vars :: Option<NameSet>)
    # note: _to is a set, determined by identical
end

data CaseResults:
  | c-exp(exp :: J.JExpr, other-stmts :: ConcatList<J.JStmt>)
  | c-field(field :: J.JField, other-stmts :: ConcatList<J.JStmt>)
  | c-block(block :: J.JBlock, new-cases :: ConcatList<J.JCase>)
end

data RegisterAllocation:
  | results(body :: ConcatList<J.JCase>, discardable-vars :: FrozenNameSet)
end

fun used-vars-jblock(b :: J.JBlock) -> NameSet:
  acc = ns-empty()
  for CL.each(s from b.stmts):
    acc.merge-now(used-vars-jstmt(s))
  end
  acc
end
fun declared-vars-jblock(b :: J.JBlock) -> NameSet:
  acc = ns-empty()
  for CL.each(s from b.stmts):
    acc.merge-now(declared-vars-jstmt(s))
  end
  acc
end
fun declared-vars-jstmt(s :: J.JStmt) -> NameSet:
  cases(J.JStmt) s:
    | j-var(name, rhs) => [D.mutable-string-dict: name.key(), name]
    | j-if1(cond, consq) => declared-vars-jblock(consq)
    | j-if(cond, consq, alt) => 
      ans = declared-vars-jblock(consq)
      ans.merge-now(declared-vars-jblock(alt))
      ans
    | j-return(expr) => ns-empty()
    | j-try-catch(body, exn, catch) => 
      ans = declared-vars-jblock(body)
      ans.merge-now(declared-vars-jblock(catch))
      ans
    | j-throw(exp) => ns-empty()
    | j-expr(expr) => ns-empty()
    | j-break => ns-empty()
    | j-continue => ns-empty()
    | j-switch(exp, branches) =>
      acc = ns-empty()
      for CL.each(b from branches):
        acc.merge-now(declared-vars-jcase(b))
      end
      acc
    | j-while(cond, body) => declared-vars-jblock(body)
    | j-for(create-var, init, cont, update, body) =>
      ans = declared-vars-jblock(body)
      when create-var and J.is-j-assign(init):
        ans.set-now(init.name.key(), init.name)
      end
      ans
  end
end
fun used-vars-jstmt(s :: J.JStmt) -> NameSet:
  cases(J.JStmt) s:
    | j-var(name, rhs) => 
      ans = used-vars-jexpr(rhs)
      ans.remove-now(name.key())
      ans
    | j-if1(cond, consq) => 
      ans = used-vars-jexpr(cond)
      ans.merge-now(used-vars-jblock(consq))
      ans
    | j-if(cond, consq, alt) =>
      ans = used-vars-jexpr(cond)
      ans.merge-now(used-vars-jblock(consq))
      ans.merge-now(used-vars-jblock(alt))
      ans
    | j-return(expr) => used-vars-jexpr(expr)
    | j-try-catch(body, exn, catch) =>
      ns-catch = used-vars-jblock(catch)
      ns-catch.remove-now(exn.key())
      ans = used-vars-jblock(body)
      ans.merge-now(ns-catch)
      ans
    | j-throw(exp) => used-vars-jexpr(exp)
    | j-expr(expr) => used-vars-jexpr(expr)
    | j-break => ns-empty()
    | j-continue => ns-empty()
    | j-switch(exp, branches) =>
      acc = used-vars-jexpr(exp)
      for CL.each(b from branches):
        acc.merge-now(used-vars-jcase(b))
      end
      acc
    | j-while(cond, body) =>
      ans = used-vars-jexpr(cond)
      ans.merge-now(used-vars-jblock(body))
      ans
    | j-for(create-var, init, cont, update, body) =>
      ans = used-vars-jexpr(init)
      ans.merge-now(used-vars-jexpr(cont))
      ans.merge-now(used-vars-jexpr(update))
      ans.merge-now(used-vars-jblock(body))
      when create-var and J.is-j-assign(init):
        ans.remove-now(init.name.key())
      end
      ans
  end
end
fun used-vars-jexpr(e :: J.JExpr) -> NameSet:
  cases(J.JExpr) e:
    | j-parens(exp) => used-vars-jexpr(exp)
    | j-unop(exp, op) => used-vars-jexpr(exp)
    | j-binop(left, op, right) => 
      ans = used-vars-jexpr(left)
      ans.merge-now(used-vars-jexpr(right))
      ans
    | j-fun(args, body) =>
      acc = difference-now(used-vars-jblock(body), declared-vars-jblock(body))
      for CL.each(a from args):
        acc.remove-now(a.key())
      end
      acc
    | j-new(func, args) =>
      acc = used-vars-jexpr(func)
      for CL.each(a from args):
        acc.merge-now(used-vars-jexpr(a))
      end
      acc
    | j-app(func, args) =>
      acc = used-vars-jexpr(func)
      for CL.each(a from args):
        acc.merge-now(used-vars-jexpr(a))
      end
      acc
    | j-method(obj, meth, args) =>
      acc = used-vars-jexpr(obj)
      for CL.each(a from args):
        acc.merge-now(used-vars-jexpr(a))
      end
      acc
    | j-ternary(test, consq, altern) =>
      ans = used-vars-jexpr(test)
      ans.merge-now(used-vars-jexpr(consq))
      ans.merge-now(used-vars-jexpr(altern))
      ans
    | j-assign(name, rhs) => 
      ans = used-vars-jexpr(rhs)
      ans.set-now(name.key(), name)
      ans
    | j-bracket-assign(obj, field, rhs) =>
      ans = used-vars-jexpr(obj)
      ans.merge-now(used-vars-jexpr(field))
      ans.merge-now(used-vars-jexpr(rhs))
      ans
    | j-dot-assign(obj, name, rhs) =>
      ans = used-vars-jexpr(obj)
      ans.merge-now(used-vars-jexpr(rhs))
      ans
    | j-dot(obj, field) => used-vars-jexpr(obj)
    | j-bracket(obj, field) => 
      ans = used-vars-jexpr(obj)
      ans.merge-now(used-vars-jexpr(field))
      ans
    | j-list(_, elts) =>
      acc = ns-empty()
      for CL.each(elt from elts):
        acc.merge-now(used-vars-jexpr(elt))
      end
      acc
    | j-obj(fields) =>
      acc = ns-empty()
      for CL.each(f from fields):
        acc.merge-now(used-vars-jfield(f))
      end
      acc
    | j-id(id) => [D.mutable-string-dict: id.key(), id]
    | j-str(_) => ns-empty()
    | j-num(_) => ns-empty()
    | j-true => ns-empty()
    | j-false => ns-empty()
    | j-null => ns-empty()
    | j-undefined => ns-empty()
    | j-label(_) => ns-empty()
  end
end
fun declared-vars-jcase(c :: J.JCase) -> NameSet:
  cases(J.JCase) c:
    | j-case(exp, body) => declared-vars-jblock(body)
    | j-default(body) => declared-vars-jblock(body)
  end
end
fun used-vars-jcase(c :: J.JCase) -> NameSet:
  cases(J.JCase) c:
    | j-case(exp, body) => 
      ans = used-vars-jexpr(exp)
      ans.merge-now(used-vars-jblock(body))
      ans
    | j-default(body) => used-vars-jblock(body)
  end
end
fun used-vars-jfield(f :: J.JField) -> NameSet:
  used-vars-jexpr(f.value)
end

fun compute-live-vars(n :: GraphNode, dag :: D.StringDict<GraphNode>) -> NameSet:
  cases(Option) n!live-vars:
    | some(live) => 
      live
    | none =>
      live-after = copy-nameset(n!free-vars)
      for CL.each(follow from n._to):
        next-opt = dag.get(tostring(follow.get()))
        when is-some(next-opt):
          next = next-opt.value
          next-vars = compute-live-vars(next, dag)
          live-after.merge-now(next-vars)
        end
      end
      decls = n!decl-vars
      live = difference-now(live-after, decls)
      dead-after = difference-now(decls, live-after)
      dead = difference-now(dead-after, n!used-vars)

      n!{live-after-vars: some(live-after), live-vars: some(live),
        dead-after-vars: some(dead-after), dead-vars: some(dead)}
      live
  end
end

fun find-steps-to(stmts :: ConcatList<J.JStmt>, step :: A.Name):
  var looking-for = none
  for CL.foldr(acc from cl-empty, stmt from stmts):
    cases(J.JStmt) stmt:
      | j-var(name, rhs) =>
        if is-some(looking-for) and (looking-for.value == name):
          looking-for := none
          for CL.foldl(shadow acc from acc, field from rhs.fields):
            cl-snoc(acc, field.value.label)
          end
        else:
          acc
        end
      | j-if1(cond, consq) => acc + find-steps-to(consq.stmts, step)
      | j-if(cond, consq, alt) =>
        acc + find-steps-to(consq.stmts, step) + find-steps-to(alt.stmts, step)
      | j-return(expr) => acc
      | j-try-catch(body, exn, catch) => acc # ignoring for now, because we know we don't use these
      | j-throw(exp) => acc
      | j-expr(expr) =>
        if J.is-j-assign(expr) and (expr.name == step):
          if J.is-j-label(expr.rhs):
            # simple assignment statement to $step
            cl-snoc(acc, expr.rhs.label)
          else if J.is-j-binop(expr.rhs) and (expr.rhs.op == J.j-or):
            # $step gets a cases dispatch
            # ASSUMES that the dispatch table is assigned two statements before this one
            looking-for := some(expr.rhs.left.obj.id)
            cl-snoc(acc, expr.rhs.right.label)
          else:
            raise("Should not happen")
          end
        else:
          acc
        end
      | j-break => acc
      | j-continue => acc
      | j-switch(exp, branches) => acc
      | j-while(cond, body) => acc
      | j-for(create-var, init, cont, update, body) => acc
    end
  end
end

fun ignorable(rhs):
  if J.is-j-app(rhs):
    (J.is-j-id(rhs.func) and (rhs.func.id.toname() == "G"))
  else if J.is-j-method(rhs):
    (J.is-j-id(rhs.obj) and (rhs.obj.id.toname() == "R") and ((rhs.meth == "getFieldRef") or (rhs.meth == "getDotAnn")))
  else:
    J.is-j-id(rhs)
    or (J.is-j-dot(rhs) and ignorable(rhs.obj))
    or (J.is-j-bracket(rhs) and ignorable(rhs.obj) and ignorable(rhs.field))
  end
end


fun elim-dead-vars-jblock(block :: J.JBlock, dead-vars :: FrozenNameSet):
  J.j-block(elim-dead-vars-jstmts(block.stmts, dead-vars))
end
fun elim-dead-vars-jstmts(stmts :: ConcatList<J.JStmt>, dead-vars :: FrozenNameSet):
  for CL.foldl(acc from cl-empty, s from stmts):
    cases(J.JStmt) s:
      | j-var(name, rhs) =>
        if dead-vars.has-key(name.key()):
          if ignorable(rhs): acc
          else: cl-snoc(acc, J.j-expr(rhs))
          end
        else:
          cl-snoc(acc, s)
        end
      | j-if1(cond, consq) =>
        cl-snoc(acc, J.j-if1(cond, elim-dead-vars-jblock(consq, dead-vars)))
      | j-if(cond, consq, alt) =>
        cl-snoc(acc,
          J.j-if(cond, elim-dead-vars-jblock(consq, dead-vars), elim-dead-vars-jblock(alt, dead-vars)))
      | j-return(expr) => cl-snoc(acc, s)
      | j-try-catch(body, exn, catch) =>
        cl-snoc(acc,
          J.j-try-catch(elim-dead-vars-jblock(body, dead-vars), exn, elim-dead-vars-jblock(catch, dead-vars)))
      | j-throw(exp) => cl-snoc(acc, s)
      | j-expr(expr) => cl-snoc(acc, s)
      | j-break => cl-snoc(acc, s)
      | j-continue => cl-snoc(acc, s)
      | j-switch(exp, branches) =>
        new-switch-branches = for map(b from branches):
          elim-dead-vars-jcase(b, dead-vars)
        end
        cl-snoc(acc, J.j-switch(exp, new-switch-branches))
      | j-while(cond, body) =>
        cl-snoc(acc, J.j-while(cond, elim-dead-vars-jblock(body, dead-vars)))
      | j-for(create-var, init, cont, update, body) =>
        cl-snoc(acc, J.j-for(create-var, init, cont, update, elim-dead-vars-jblock(body, dead-vars)))
    end
  end
end
fun elim-dead-vars-jcase(c :: J.JCase, dead-vars :: FrozenNameSet):
  cases(J.JCase) c:
    | j-default(body) => J.j-default(elim-dead-vars-jblock(body, dead-vars))
    | j-case(exp, body) => J.j-case(exp, elim-dead-vars-jblock(body, dead-vars))
  end
end

# fun compute-live-ranges(labels :: List<Number>, dag):
#   doc: ```
#        Assumes labels are already sorted (which is a given since they come from
#        the original ConcatList of body-cases in the compiled function
#        ```
#   ranges = D.make-mutable-string-dict()
#   for each(lbl from labels):
#     n = dag.get-value(tostring(lbl))
#     for each(v from n!live-vars.value.keys-list()):
#       LR.record-range(v, lbl, ranges)
#     end
#   end
#   ranges
# end
fun simplify(body-cases :: ConcatList<J.JCase>, step :: A.Name) -> RegisterAllocation:
  # print("Step 1: " + step + " num cases: " + tostring(body-cases.length()))
  acc-dag = D.make-mutable-string-dict()
  for CL.each(body-case from body-cases):
    when J.is-j-case(body-case):
      acc-dag.set-now(tostring(body-case.exp.label.get()),
        node(body-case.exp.label, find-steps-to(body-case.body.stmts, step), body-case,
          ns-empty(), ns-empty(), ns-empty(), none, none, none, none))
    end
  end
  dag = acc-dag.freeze()
  # print("Step 2")
  labels = for CL.foldr(acc from empty, body-case from body-cases):
    if J.is-j-case(body-case): link(body-case.exp.label.get(), acc)
    else: acc
    end
  end
  str-labels = dag.keys-list()
  # for each(lbl from labels):
  #   n = dag.get-value(lbl)
  #   print(tostring(n._from.get()) + " ==> " + tostring(n._to.to-list().map(_.get())))
  #   print("\n" + n.case-body.to-ugly-source())
  # end
  # print("Step 3")
  for each(lbl from str-labels):
    n = dag.get-value(lbl)
    n!{decl-vars: declared-vars-jcase(n.case-body)}
    n!{used-vars: used-vars-jcase(n.case-body)}
    n!{free-vars: difference-now(n!used-vars, n!decl-vars)}
  end
  for each(lbl from str-labels):
    n = dag.get-value(lbl)
    compute-live-vars(n, dag)
  end
  # for each(lbl from str-labels):
  #   n = dag.get-value(lbl)
  #   print("Used vars for " + lbl + ": " + torepr(n!used-vars))
  #   print("Decl vars for " + lbl + ": " + torepr(n!decl-vars.to-list()))
  #   print("Free vars for " + lbl + ": " + torepr(n!free-vars))
  #   print("Live-after vars for " + lbl + ": " + torepr(n!live-after-vars.value.to-list()))
  #   print("Live vars for " + lbl + ": " + torepr(n!live-vars.value.to-list()))
  #   print("Dead vars for " + lbl + ": " + torepr(n!dead-vars.value.to-list()))
  #   print("Dead-after vars for " + lbl + ": " + torepr(n!dead-after-vars.value.to-list()))
  #   print("\n")
  # end
  # print("Step 4")
  # live-ranges = D.make-mutable-string-dict()
  # for each(lbl from labels):
  #   n = dag.get-value(lbl)
  #   for each(v from n!live-vars.value.keys-list()):
  #     cur = if live-ranges.has-key-now(v.tosourcestring()): live-ranges.get-value-now(v) else: ns-empty end
  #     live-ranges.set-now(v.tosourcestring(), cur.add(lbl))
  #   end
  # end
  acc = ns-empty()
  for each(lbl from str-labels):
    n = dag.get-value(lbl)
    when is-some(n!dead-after-vars):
      acc.merge-now(n!dead-after-vars.value)
    end
  end
  discardable-vars = acc.freeze()

  dead-assignment-eliminated = for CL.map(body-case from body-cases):
    n = dag.get-value(tostring(body-case.exp.label.get()))
    cases(Option) n!dead-vars:
      | none => body-case
      | some(dead-vars) => elim-dead-vars-jcase(body-case, dead-vars.freeze())
    end
  end

  # print("Done")
  results(dead-assignment-eliminated, discardable-vars)
end
