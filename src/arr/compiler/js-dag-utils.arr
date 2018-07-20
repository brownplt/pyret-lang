#lang pyret

provide *
provide-types *
import file("ast.arr") as A
import sets as S
import file("ast-anf.arr") as N
import file("js-ast.arr") as J
import file("gensym.arr") as G
import file("compile-structs.arr") as CS
import file("concat-lists.arr") as CL
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

var copy-overhead = 0
var j-fun-difference = 0

fun debugprint(str):
  when false:
    print(str)
  end
end

fun difference(s1 :: FrozenNameSet, s2 :: FrozenNameSet) block:

  start = time-now()
  s1-unfrozen-copy = s1.unfreeze()
  s2-unfrozen = s2.unfreeze()
  copy-overhead := copy-overhead + (time-now() - start)


  remove-overlap-now(s1-unfrozen-copy, s2-unfrozen)

  start2 = time-now()
  ans = s1-unfrozen-copy.freeze()
  copy-overhead := copy-overhead + (time-now() - start2)
  ans
end

fun copy-nameset(s :: NameSet) -> NameSet block:
  start = time-now()
  ans = s.clone-now()
  copy-overhead := copy-overhead + (time-now() - start)
  ans
end

# does NOT mutate s1 or s2
fun difference-now(s1 :: NameSet, s2 :: NameSet) -> NameSet block:
  s1-copy = copy-nameset(s1)
  remove-overlap-now(s1-copy, s2)
  s1-copy
end

# !mutates s1
fun remove-overlap-now(s1 :: NameSet, s2 :: NameSet) -> Nothing:
  for D.each-key-now(k2 from s2):
    s1.remove-now(k2)
  end
end

data GraphNode:
  | node(_from :: String, _to :: D.MutableStringDict<J.Label>, case-body :: J.JCase,
      ref free-vars :: NameSet,
      ref used-vars :: NameSet,
      ref decl-vars :: NameSet,
      ref live-vars :: Option<NameSet>,
      ref live-after-vars :: Option<NameSet>,
      ref dead-vars :: Option<NameSet>,
      ref dead-after-vars :: Option<NameSet>)
end

data CaseResults:
  | c-exp(exp :: J.JExpr, other-stmts :: ConcatList<J.JStmt>)
  | c-field(field :: J.JField, other-stmts :: ConcatList<J.JStmt>)
  | c-block(block :: J.JBlock, new-cases :: ConcatList<J.JCase>)
end

data RegisterAllocation:
  | results(body :: ConcatList<J.JCase>, discardable-vars :: FrozenNameSet)
end

fun-decl-vars :: D.MutableStringDict<NameSet> = D.make-mutable-string-dict()
fun-used-vars :: D.MutableStringDict<NameSet> = D.make-mutable-string-dict()
var from-hit = 0
var from-miss = 0

fun used-vars-jblock(b :: J.JBlock, so-far :: NameSet) -> NameSet block:
  cases(J.JBlock) b block:
    | j-block1(s) => used-vars-jstmt(s, so-far)
    | j-block(stmts) =>
      for CL.each(s from stmts):
        used-vars-jstmt(s, so-far)
      end
      so-far
  end
end
fun declared-vars-jblock(b :: J.JBlock, so-far :: NameSet) -> NameSet block:
  cases(J.JBlock) b block:
    | j-block1(s) => declared-vars-jstmt(s, so-far)
    | j-block(stmts) =>
      for CL.each(s from stmts):
        declared-vars-jstmt(s, so-far)
      end
      so-far
  end
end
fun declared-vars-jstmt(s :: J.JStmt, so-far :: NameSet) -> NameSet:
  cases(J.JStmt) s block:
    | j-var(name, rhs) =>
      so-far.set-now(name.key(), name)
      so-far
    | j-if1(cond, consq) => declared-vars-jblock(consq, so-far)
    | j-if(cond, consq, alt) =>
      shadow so-far = declared-vars-jblock(consq, so-far)
      declared-vars-jblock(alt, so-far)
    | j-return(expr) => so-far
    | j-try-catch(body, exn, catch) =>
      shadow so-far = declared-vars-jblock(body, so-far)
      declared-vars-jblock(catch, so-far)
    | j-throw(exp) => so-far
    | j-expr(expr) => so-far
    | j-break => so-far
    | j-continue => so-far
    | j-switch(exp, branches) =>
      for CL.each(b from branches):
        declared-vars-jcase(b, so-far)
      end
      so-far
    | j-while(cond, body) =>
      declared-vars-jblock(body, so-far)
    | j-for(create-var, init, cont, update, body) =>
      shadow so-far = declared-vars-jblock(body, so-far)
      when create-var and J.is-j-assign(init):
        so-far.set-now(init.name.key(), init.name)
      end
      so-far
  end
end
fun used-vars-jstmt(s :: J.JStmt, so-far :: NameSet) -> NameSet:
  cases(J.JStmt) s block:
    | j-var(name, rhs) =>
      shadow so-far = used-vars-jexpr(rhs, so-far)
      so-far.remove-now(name.key())
      so-far
    | j-if1(cond, consq) =>
      shadow so-far = used-vars-jexpr(cond, so-far)
      used-vars-jblock(consq, so-far)
    | j-if(cond, consq, alt) =>
      shadow so-far = used-vars-jexpr(cond, so-far)
      shadow so-far = used-vars-jblock(consq, so-far)
      used-vars-jblock(alt, so-far)
    | j-return(expr) =>
      used-vars-jexpr(expr, so-far)
    | j-try-catch(body, exn, catch) =>
      shadow so-far = used-vars-jblock(catch, so-far)
      so-far.remove-now(exn.key())
      used-vars-jblock(body, so-far)
    | j-throw(exp) => used-vars-jexpr(exp, so-far)
    | j-expr(expr) => used-vars-jexpr(expr, so-far)
    | j-break => so-far
    | j-continue => so-far
    | j-switch(exp, branches) =>
      shadow so-far = used-vars-jexpr(exp, so-far)
      for CL.each(b from branches):
        used-vars-jcase(b, so-far)
      end
      so-far
    | j-while(cond, body) =>
      shadow so-far = used-vars-jexpr(cond, so-far)
      used-vars-jblock(body, so-far)
    | j-for(create-var, init, cont, update, body) =>
      shadow so-far = used-vars-jexpr(init, so-far)
      shadow so-far = used-vars-jexpr(update, so-far)
      shadow so-far = used-vars-jblock(body, so-far)
      when create-var and J.is-j-assign(init):
        so-far.remove-now(init.name.key())
      end
      so-far
  end
end
fun used-vars-jexpr(e :: J.JExpr, so-far :: NameSet) -> NameSet:
  cases(J.JExpr) e block:
    | j-sourcenode(_, _, expr) => used-vars-jexpr(expr, so-far)
    | j-parens(exp) => used-vars-jexpr(exp, so-far)
    | j-unop(exp, op) => used-vars-jexpr(exp, so-far)
    | j-binop(left, op, right) =>
      shadow so-far = used-vars-jexpr(left, so-far)
      used-vars-jexpr(right, so-far)
    | j-fun(id, _, args, body) =>
      start = time-now()
      total-before = j-fun-difference
      declared =
        if fun-decl-vars.has-key-now(id) block:
          fun-decl-vars.get-value-now(id)
        else:
          ans = declared-vars-jblock(body, ns-empty())
          fun-decl-vars.set-now(id, ans)
          ans
        end
      from-body =
        if fun-used-vars.has-key-now(id) block:
          from-hit := from-hit + 1
          so-far.merge-now(fun-used-vars.get-value-now(id))
          so-far
        else:
          from-miss := from-miss + 1
          clean-from-body = used-vars-jblock(body, ns-empty())
          fun-used-vars.set-now(id, clean-from-body)
          so-far.merge-now(clean-from-body)
          so-far
        end
      for D.each-key-now(d from declared):
        from-body.remove-now(d)
      end
      j-fun-difference := j-fun-difference + (time-now() - start - (j-fun-difference - total-before))
      for CL.each(a from args):
        from-body.remove-now(a.key())
      end
      so-far
    | j-new(func, args) =>
      shadow so-far = used-vars-jexpr(func, so-far)
      for CL.each(a from args):
        used-vars-jexpr(a, so-far)
      end
      so-far
    | j-app(func, args) =>
      shadow so-far = used-vars-jexpr(func, so-far)
      for CL.each(a from args):
        used-vars-jexpr(a, so-far)
      end
      so-far
    | j-method(obj, meth, args) =>
      shadow so-far = used-vars-jexpr(obj, so-far)
      for CL.each(a from args):
        used-vars-jexpr(a, so-far)
      end
      so-far
    | j-ternary(test, consq, altern) =>
      shadow so-far = used-vars-jexpr(test, so-far)
      shadow so-far = used-vars-jexpr(consq, so-far)
      used-vars-jexpr(altern, so-far)
    | j-assign(name, rhs) =>
      shadow so-far = used-vars-jexpr(rhs, so-far)
      so-far.set-now(name.key(), name)
      so-far
    | j-bracket-assign(obj, field, rhs) =>
      shadow so-far = used-vars-jexpr(obj, so-far)
      shadow so-far = used-vars-jexpr(field, so-far)
      used-vars-jexpr(rhs, so-far)
    | j-dot-assign(obj, name, rhs) =>
      shadow so-far = used-vars-jexpr(obj, so-far)
      used-vars-jexpr(rhs, so-far)
    | j-dot(obj, field) => used-vars-jexpr(obj, so-far)
    | j-bracket(obj, field) =>
      shadow so-far = used-vars-jexpr(obj, so-far)
      used-vars-jexpr(field, so-far)
    | j-list(_, elts) =>
      for CL.each(elt from elts):
        used-vars-jexpr(elt, so-far)
      end
      so-far
    | j-obj(fields) =>
      for CL.each(f from fields):
        used-vars-jfield(f, so-far)
      end
      so-far
    | j-id(id) =>
      so-far.set-now(id.key(), id)
      so-far
    | j-str(_) => so-far
    | j-num(_) => so-far
    | j-true => so-far
    | j-false => so-far
    | j-null => so-far
    | j-undefined => so-far
    | j-label(_) => so-far
    | j-raw-code(_) => so-far
  end
end
fun declared-vars-jcase(c :: J.JCase, so-far :: NameSet) -> NameSet:
  cases(J.JCase) c:
    | j-case(exp, body) => declared-vars-jblock(body, so-far)
    | j-default(body) => declared-vars-jblock(body, so-far)
  end
end
fun used-vars-jcase(c :: J.JCase, so-far :: NameSet) -> NameSet:
  cases(J.JCase) c block:
    | j-case(exp, body) =>
      shadow so-far = used-vars-jexpr(exp, so-far)
      used-vars-jblock(body, so-far)
    | j-default(body) => used-vars-jblock(body, so-far)
  end
end
fun used-vars-jfield(f :: J.JField, so-far :: NameSet) -> NameSet:
  used-vars-jexpr(f.value, so-far)
end

fun compute-live-vars(n :: GraphNode, dag :: D.StringDict<GraphNode>) -> NameSet:
  cases(Option) n!live-vars block:
    | some(live) =>
      live
    | none =>
      live-after = copy-nameset(n!free-vars)
      for D.each-key-now(follow-key from n._to):
        # Note: this is false only for the exit block of the function
        # which isn't currently present in the DAG (todo: why not?)
        when dag.has-key(follow-key):
          next = dag.get-value(follow-key)
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

fun stmts-of(blk :: J.JBlock):
  cases(J.JBlock) blk:
    | j-block1(s) => cl-sing(s)
    | j-block(stmts) => stmts
  end
end

fun find-steps-to(stmts :: ConcatList<J.JStmt>, step :: A.Name, acc :: D.MutableStringDict<J.Label>, cases-dispatches :: ConcatList<J.JStmt>) -> D.MutableStringDict<J.Label>:
  for CL.foldr(shadow acc from acc, stmt from stmts):
    cases(J.JStmt) stmt:
      | j-var(name, rhs) => acc
      | j-if1(cond, consq) => find-steps-to(stmts-of(consq), step, acc, cases-dispatches)
      | j-if(cond, consq, alt) =>
        acc
          ^ find-steps-to(stmts-of(consq), step, _, cases-dispatches)
          ^ find-steps-to(stmts-of(alt), step, _, cases-dispatches)
      | j-return(expr) => acc
      | j-try-catch(body, exn, catch) => acc # ignoring for now, because we know we don't use these
      | j-throw(exp) => acc
      | j-expr(expr) =>
        if J.is-j-assign(expr) and (expr.name == step):
          if J.is-j-label(expr.rhs) block:
            # simple assignment statement to $step
            acc.set-now(tostring(expr.rhs.label.get()), expr.rhs.label)
            acc
          else if J.is-j-binop(expr.rhs) and (expr.rhs.op == J.j-or):
            # $step gets a cases dispatch
            # ASSUMES that the dispatch table is assigned before toplevel is defined.
            # (see cases-dispatches in anf-loop-compiler.arr)
            acc.set-now(tostring(expr.rhs.right.label.get()), expr.rhs.right.label)
            now-looking = cases-dispatches.find({(elt :: J.JStmt): elt.name == expr.rhs.left.obj.id}).value.rhs
            for CL.foldl(shadow acc from acc, field from now-looking.fields) block:
              acc.set-now(tostring(field.value.label.get()), field.value.label)
              acc
            end
            acc
          else if J.is-j-num(expr.rhs):
            acc
          else if J.is-j-ternary(expr.rhs):
            # ASSUMES that the only current use of $step = ( ? : )
            # comes from compile-split-if
            acc.set-now(tostring(expr.rhs.consq.label.get()), expr.rhs.consq.label)
            acc.set-now(tostring(expr.rhs.altern.label.get()), expr.rhs.altern.label)
            acc
          else:
            raise({err: "Should not happen", expr: expr})
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
  cases(J.JExpr) rhs:
    | j-sourcenode(_, _, e) => ignorable(e)
    | j-parens(e) => ignorable(e)
    | j-ternary(test, consq, alt) => ignorable(test) and ignorable(consq) and ignorable(alt)
    | j-dot(obj, field) => ignorable(obj)
    | j-bracket(obj, field) => ignorable(obj) and ignorable(field)
    | j-list(_, elts) => elts.all(ignorable)
    | j-obj(fields) => fields.all(lam(f): ignorable(f.value) end)
    | j-id(_) => true
    | j-str(_) => true
    | j-num(_) => true
    | j-true => true
    | j-false => true
    | j-undefined => true
    | j-null => true
    | else => false
  end
end



fun elim-dead-vars-jblock(b :: J.JBlock, dead-vars :: FrozenNameSet):
  cases(J.JBlock) b:
    | j-block1(s) =>
      if is-pointless-j-var(s, dead-vars): J.j-block(cl-empty)
      else: b
      end
    | j-block(stmts) => J.j-block(elim-dead-vars-jstmts(stmts, dead-vars))
  end
end

fun is-pointless-j-var(s, dead-vars):
  cases(J.JStmt) s:
    | j-var(name, rhs) => dead-vars.has-key(name.key()) and ignorable(rhs)
    | else => false
  end
end

fun elim-dead-vars-jstmts(stmts :: ConcatList<J.JStmt>, dead-vars :: FrozenNameSet):
  for CL.foldl(acc from cl-empty, s from stmts):
    cases(J.JStmt) s:
      | j-var(name, rhs) =>
        if dead-vars.has-key(name.key()):
          if ignorable(rhs): acc
          else: acc ^ cl-snoc(_, J.j-expr(rhs))
          end
        else: acc ^ cl-snoc(_, s)
        end
      | j-if1(cond, consq) =>
        acc ^ cl-snoc(_, J.j-if1(cond, elim-dead-vars-jblock(consq, dead-vars)))
      | j-if(cond, consq, alt) =>
        acc ^ cl-snoc(_,
          J.j-if(cond, elim-dead-vars-jblock(consq, dead-vars), elim-dead-vars-jblock(alt, dead-vars)))
      | j-return(expr) => acc ^ cl-snoc(_, s)
      | j-try-catch(body, exn, catch) =>
        acc ^ cl-snoc(_,
          J.j-try-catch(elim-dead-vars-jblock(body, dead-vars), exn, elim-dead-vars-jblock(catch, dead-vars)))
      | j-throw(exp) => acc ^ cl-snoc(_, s)
      | j-expr(expr) => acc ^ cl-snoc(_, s)
      | j-break => acc ^ cl-snoc(_, s)
      | j-continue => acc ^ cl-snoc(_, s)
      | j-switch(exp, branches) =>
        new-switch-branches = for map(b from branches):
          elim-dead-vars-jcase(b, dead-vars)
        end
        acc ^ cl-snoc(_, J.j-switch(exp, new-switch-branches))
      | j-while(cond, body) =>
        acc ^ cl-snoc(_, J.j-while(cond, elim-dead-vars-jblock(body, dead-vars)))
      | j-for(create-var, init, cont, update, body) =>
        acc ^ cl-snoc(_, J.j-for(create-var, init, cont, update, elim-dead-vars-jblock(body, dead-vars)))
    end
  end
end
fun elim-dead-vars-jcase(c :: J.JCase, dead-vars :: FrozenNameSet):
  cases(J.JCase) c:
    | j-default(body) => J.j-default(elim-dead-vars-jblock(body, dead-vars))
    | j-case(exp, body) => J.j-case(exp, elim-dead-vars-jblock(body, dead-vars))
  end
end

var step-1-total = 0
var step-2-total = 0
var step-3-total = 0
var step-4-total = 0

# fun compute-live-ranges(labels :: List<Number>, dag):
#   doc: ```
#        Assumes labels are already sorted (which is a given since they come from
#        the original ConcatList of body-cases in the compiled function
#        ```
#   ranges = D.make-mutable-string-dict()
#   for each(lbl from labels):
#     n = dag.get-value(tostring(lbl))
#     for SD.each-key(v from n!live-vars.value):
#       LR.record-range(v, lbl, ranges)
#     end
#   end
#   ranges
# end

fun simplify(add-phase, body-cases :: ConcatList<J.JCase>, step :: A.Name, cases-dispatches :: ConcatList<J.JStmt>) -> RegisterAllocation block:
  start = time-now()
  from-hit := 0
  from-miss := 0
  # print("Step 1: " + step + " num cases: " + tostring(body-cases.length()))
  acc-dag = D.make-mutable-string-dict()
  for CL.each(body-case from body-cases):
    when J.is-j-case(body-case):
      label = tostring(body-case.exp.label.get())
      acc-dag.set-now(label,
        node(label,
          cases(J.JBlock) body-case.body:
            | j-block1(s) => find-steps-to(cl-sing(s), step, ns-empty(), cases-dispatches)
            | j-block(stmts) => find-steps-to(stmts, step, ns-empty(), cases-dispatches)
          end, body-case,
          ns-empty(), ns-empty(), ns-empty(), none, none, none, none))
    end
  end
  start-copy = time-now()
  dag = acc-dag.freeze()
  copy-overhead := copy-overhead + (time-now() - start-copy)
  step-1 = time-now() - start
  step-1-total := step-1-total + step-1
#  add-phase("Step 1: " + torepr(step) + " " + torepr(step-1) + "/" + torepr(step-1-total), nothing)
  # print("Step 2")
  labels = for CL.foldr(acc from empty, body-case from body-cases):
    if J.is-j-case(body-case): link(body-case.exp.label.get(), acc)
    else: acc
    end
  end
  step-2 = time-now() - step-1 - start
  step-2-total := step-2-total + step-2
#  add-phase("Step 2: " + torepr(step) + " " + torepr(step-2) + "/" + torepr(step-2-total), nothing)
  # for each(lbl from labels):
  #   n = dag.get-value(lbl)
  #   print(tostring(n._from.get()) + " ==> " + tostring(n._to.to-list().map(_.get())))
  #   print("\n" + n.case-body.to-ugly-source())
  # end
  # print("Step 3")
  for D.each-key(lbl from dag) block:
    n = dag.get-value(lbl)
    n!{decl-vars: declared-vars-jcase(n.case-body, D.make-mutable-string-dict())}
    n!{used-vars: used-vars-jcase(n.case-body, D.make-mutable-string-dict())}
    n!{free-vars: difference-now(n!used-vars, n!decl-vars)}
  end
  for D.each-key(lbl from dag):
    n = dag.get-value(lbl)
    compute-live-vars(n, dag)
  end
  # for D.each-key(lbl from dag) block:
  #   n = dag.get-value(lbl)
  #   print("\nUsed vars for " + lbl + ": " + torepr(n!used-vars))
  #   print("\nDecl vars for " + lbl + ": " + torepr(n!decl-vars.keys-now()))
  #   print("\nFree vars for " + lbl + ": " + torepr(n!free-vars))
  #   print("\nLive-after vars for " + lbl + ": " + torepr(n!live-after-vars.value.keys-now()))
  #   print("\nLive vars for " + lbl + ": " + torepr(n!live-vars.value.keys-now()))
  #   print("\nDead vars for " + lbl + ": " + torepr(n!dead-vars.value.keys-now()))
  #   print("\nDead-after vars for " + lbl + ": " + torepr(n!dead-after-vars.value.keys-now()))
  #   print("\n")
  # end
  step-3 = time-now() - step-2 - step-1 - start
  step-3-total := step-3-total + step-3
#  add-phase("Step 3: " + torepr(step) + " " + torepr(step-3) + "/" + torepr(step-3-total), nothing)
  # print("Step 4")
  # live-ranges = D.make-mutable-string-dict()
  # for each(lbl from labels):
  #   n = dag.get-value(lbl)
  #   for SD.each-key(v from n!live-vars.value):
  #     cur = if live-ranges.has-key-now(v.tosourcestring()): live-ranges.get-value-now(v) else: ns-empty end
  #     live-ranges.set-now(v.tosourcestring(), cur.add(lbl))
  #   end
  # end
  acc = ns-empty()
  for D.each-key(lbl from dag):
    n = dag.get-value(lbl)
    when is-some(n!dead-after-vars):
      acc.merge-now(n!dead-after-vars.value)
    end
  end
  shadow start-copy = time-now()
  discardable-vars = acc.freeze()
  copy-overhead := copy-overhead + (time-now() - start-copy)

  dead-assignment-eliminated = for CL.map(body-case from body-cases):
    n = dag.get-value(tostring(body-case.exp.label.get()))
    cases(Option) n!dead-vars block:
      | none => body-case
      | some(dead-vars) =>
        shadow start-copy = time-now()
        dead-frozen = dead-vars.freeze()
        copy-overhead := copy-overhead + (time-now() - start-copy)
        elim-dead-vars-jcase(body-case, dead-frozen)
    end
  end
  step-4 = time-now() - step-3 - step-2 - step-1 - start
  step-4-total := step-4-total + step-4
#  add-phase("Step 4: " + torepr(step) + " " + torepr(step-4) + "/" + torepr(step-4-total), nothing)
#  add-phase("Step 5: from-hit " + torepr(from-hit) + " / from-miss " + torepr(from-miss), nothing)

  debugprint("Cumulative overhead from copying string-dicts: " + torepr(copy-overhead) + "\n")
  debugprint("Cumulative overhead from differencing function sets: " + torepr(j-fun-difference) + "\n")

  # print("Done")
  results(dead-assignment-eliminated, discardable-vars)
end
