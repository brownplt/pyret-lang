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
import string-dict as D
import srcloc as SL

type ConcatList = CL.ConcatList

cl-sing = CL.concat-singleton
cl-empty = CL.concat-empty 
data GraphNode:
  | node(_from :: J.Label, _to :: ConcatList<J.Label>, case-body :: J.JCase,
      ref free-vars :: Set<String>,
      ref used-vars :: Set<String>,
      ref decl-vars :: ConcatList<String>,
      ref live-vars :: Option<Set<String>>,
      ref live-after-vars :: Option<Set<String>>,
      ref dead-vars :: Option<Set<String>>,
      ref dead-after-vars :: Option<Set<String>>)
    # note: _to is a set, determined by identical
end

data CaseResults:
  | c-exp(exp :: J.JExpr, other-stmts :: List<J.JStmt>)
  | c-field(field :: J.JField, other-stmts :: List<J.JStmt>)
  | c-block(block :: J.JBlock, new-cases :: ConcatList<J.JCase>)
end

data RegisterAllocation:
  | results(body :: ConcatList<J.JCase>, discardable-vars :: Set<String>)
end

fun used-vars-jblock(b :: J.JBlock):
  for fold(acc from D.make-string-dict(), s from b.stmts):
    acc.merge(used-vars-jstmt(s))
  end
end
fun declared-vars-jblock(b :: J.JBlock):
  for fold(acc from cl-empty, s from b.stmts):
    acc + declared-vars-jstmt(s)
  end
end
fun declared-vars-jstmt(s :: J.JStmt):
  cases(J.JStmt) s:
    | j-var(name, rhs) => cl-sing(name)
    | j-if1(cond, consq) => declared-vars-jblock(consq)
    | j-if(cond, consq, alt) => declared-vars-jblock(consq) + declared-vars-jblock(alt)
    | j-return(expr) => cl-empty
    | j-try-catch(body, exn, catch) => declared-vars-jblock(body) + declared-vars-jblock(catch)
    | j-throw(exp) => cl-empty
    | j-expr(expr) => cl-empty
    | j-break => cl-empty
    | j-continue => cl-empty
    | j-switch(exp, branches) =>
      for fold(acc from cl-empty, b from branches):
        acc + declared-vars-jcase(b)
      end
    | j-while(cond, body) => declared-vars-jblock(body)
    | j-for(create-var, init, cont, update, body) =>
      ans = declared-vars-jblock(body)
      if create-var and J.is-j-assign(init): cl-sing(init.name) + ans
      else: ans
      end
  end
end
fun used-vars-jstmt(s :: J.JStmt):
  cases(J.JStmt) s:
    | j-var(name, rhs) => used-vars-jexpr(rhs).remove(name)
    | j-if1(cond, consq) => used-vars-jexpr(cond).merge(used-vars-jblock(consq))
    | j-if(cond, consq, alt) =>
      used-vars-jexpr(cond).merge(used-vars-jblock(consq)).merge(used-vars-jblock(alt))
    | j-return(expr) => used-vars-jexpr(expr)
    | j-try-catch(body, exn, catch) => used-vars-jblock(body).merge(used-vars-jblock(catch).remove(exn))
    | j-throw(exp) => used-vars-jexpr(exp)
    | j-expr(expr) => used-vars-jexpr(expr)
    | j-break => D.make-string-dict()
    | j-continue => D.make-string-dict()
    | j-switch(exp, branches) =>
      for fold(acc from used-vars-jexpr(exp), b from branches):
        acc.merge(used-vars-jcase(b))
      end
    | j-while(cond, body) => used-vars-jexpr(cond).merge(used-vars-jblock(body))
    | j-for(create-var, init, cont, update, body) =>
      ans =
        used-vars-jexpr(init).merge(used-vars-jexpr(cont)).merge(used-vars-jexpr(update)).merge(used-vars-jblock(body))
      if create-var and J.is-j-assign(init): ans.remove(init.name)
      else: ans
      end
  end
end
fun used-vars-jexpr(e :: J.JExpr):
  cases(J.JExpr) e:
    | j-parens(exp) => used-vars-jexpr(exp)
    | j-unop(exp, op) => used-vars-jexpr(exp)
    | j-binop(left, op, right) => used-vars-jexpr(left).merge(used-vars-jexpr(right))
    | j-fun(args, body) =>
      used = used-vars-jblock(body)
      shadow used = for fold(acc from used, a from args):
        acc.remove(a)
      end
      shadow used = for CL.foldl(acc from used, d from declared-vars-jblock(body)):
        acc.remove(d)
      end
      used
    | j-new(func, args) =>
      for fold(acc from used-vars-jexpr(func), a from args):
        acc.merge(used-vars-jexpr(a))
      end
    | j-app(func, args) =>
      for fold(acc from used-vars-jexpr(func), a from args):
        acc.merge(used-vars-jexpr(a))
      end
    | j-method(obj, meth, args) =>
      for fold(acc from used-vars-jexpr(obj), a from args):
        acc.merge(used-vars-jexpr(a))
      end
    | j-ternary(test, consq, altern) =>
      used-vars-jexpr(test).merge(used-vars-jexpr(consq)).merge(used-vars-jexpr(altern))
    | j-assign(name, rhs) => used-vars-jexpr(rhs).set(name, true)
    | j-bracket-assign(obj, field, rhs) =>
      used-vars-jexpr(obj).merge(used-vars-jexpr(field)).merge(used-vars-jexpr(rhs))
    | j-dot-assign(obj, name, rhs) => used-vars-jexpr(obj).merge(used-vars-jexpr(rhs))
    | j-dot(obj, field) => used-vars-jexpr(obj)
    | j-bracket(obj, field) => used-vars-jexpr(obj).merge(used-vars-jexpr(obj))
    | j-list(_, elts) =>
      for fold(acc from D.make-string-dict(), elt from elts):
        acc.merge(used-vars-jexpr(elt))
      end
    | j-obj(fields) =>
      for fold(acc from D.make-string-dict(), f from fields):
        acc.merge(used-vars-jfield(f))
      end
    | j-id(id) => D.make-string-dict().set(id, true)
    | j-str(_) => D.make-string-dict()
    | j-num(_) => D.make-string-dict()
    | j-true => D.make-string-dict()
    | j-false => D.make-string-dict()
    | j-null => D.make-string-dict()
    | j-undefined => D.make-string-dict()
    | j-label(_) => D.make-string-dict()
  end
end
fun declared-vars-jcase(c :: J.JCase):
  cases(J.JCase) c:
    | j-case(exp, body) => declared-vars-jblock(body)
    | j-default(body) => declared-vars-jblock(body)
  end
end
fun used-vars-jcase(c :: J.JCase):
  cases(J.JCase) c:
    | j-case(exp, body) => used-vars-jexpr(exp).merge(used-vars-jblock(body))
    | j-default(body) => used-vars-jblock(body)
  end
end
fun used-vars-jfield(f :: J.JField):
  used-vars-jexpr(f.value)
end

fun compute-live-vars(n :: GraphNode, dag :: D.StringDict<GraphNode>):
  cases(Option) n!live-vars:
    | some(live) => live
    | none =>
      live-after = for CL.foldl(acc from sets.list-to-tree-set(n!free-vars.to-list()), follow from n._to):
        cases(Option) dag.get(tostring(follow.get())):
          | none => acc
          | some(next) => acc.union(compute-live-vars(next, dag))
        end
      end
      decls = sets.list-to-tree-set(n!decl-vars.to-list())
      live = live-after.difference(decls)
      dead-after = decls.difference(live-after)
      dead = dead-after.difference(n!used-vars)
      n!{live-after-vars: some(live-after), live-vars: some(live),
        dead-after-vars: some(dead-after), dead-vars: some(dead)}
      live
  end
end

fun simplify(body-cases :: ConcatList<J.JCase>, step :: String) -> RegisterAllocation:
  fun find-steps-to(rev-stmts :: List<J.JStmt>):
    cases(List) rev-stmts:
      | empty => cl-empty
      | link(stmt, rest) =>
        cases(J.JStmt) stmt:
          | j-var(name, rhs) => find-steps-to(rest)
          | j-if1(cond, consq) => find-steps-to(consq.stmts.reverse()) + find-steps-to(rest)
          | j-if(cond, consq, alt) =>
            find-steps-to(consq.stmts.reverse()) + find-steps-to(alt.stmts.reverse()) + find-steps-to(rest)
          | j-return(expr) => find-steps-to(rest)
          | j-try-catch(body, exn, catch) => find-steps-to(rest)
          | j-throw(exp) => find-steps-to(rest)
          | j-expr(expr) =>
            if J.is-j-assign(expr) and (expr.name == step):
              if J.is-j-label(expr.rhs):
                # simple assignment statement to $step
                cl-sing(expr.rhs.label) + find-steps-to(rest)
              else if J.is-j-binop(expr.rhs) and (expr.rhs.op == J.j-or):
                # $step gets a cases dispatch
                # ASSUMES that the dispatch table is assigned two statements before this one
                dispatch-table = rev-stmts.rest.rest.first.rhs
                for fold(acc from cl-sing(expr.rhs.right.label), field from dispatch-table.fields):
                  acc + cl-sing(field.value.label)
                end
                  + find-steps-to(rest)
              else:
                raise("Should not happen")
              end
            else:
              find-steps-to(rest)
            end
          | j-break => find-steps-to(rest)
          | j-continue => find-steps-to(rest)
          | j-switch(exp, branches) => find-steps-to(rest)
          | j-while(cond, body) => find-steps-to(rest)
          | j-for(create-var, init, cont, update, body) => find-steps-to(rest)
        end
    end
  end
  # print("Step 1: " + step + " num cases: " + tostring(body-cases.length()))
  dag = (for CL.foldl(acc from D.make-mutable-string-dict(), body-case from body-cases):
      if J.is-j-case(body-case):
        acc.set-now(tostring(body-case.exp.label.get()),
          node(body-case.exp.label, find-steps-to(body-case.body.stmts.reverse()), body-case,
            [tree-set: ], [tree-set: ], cl-empty, none, none, none, none))
        acc
      else:
        acc
      end
    end).freeze()
  # print("Step 2")
  labels = dag.keys-list()
  # for each(lbl from labels):
  #   n = dag.get-value(lbl)
  #   print(tostring(n._from.get()) + " ==> " + tostring(n._to.to-list().map(_.get())))
  #   print("\n" + n.case-body.to-ugly-source())
  # end
  # print("Step 3")
  for each(lbl from labels):
    n = dag.get-value(lbl)
    n!{decl-vars: declared-vars-jcase(n.case-body)}
    n!{used-vars: used-vars-jcase(n.case-body).keys()}
    free = for CL.foldl(acc from n!used-vars, v from n!decl-vars):
      acc.remove(v)
    end
    n!{free-vars: free}
  end
  for each(lbl from labels):
    n = dag.get-value(lbl)
    compute-live-vars(n, dag)
  end
  # for each(lbl from labels):
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
  live-ranges = D.make-mutable-string-dict()
  for each(lbl from labels):
    n = dag.get-value(lbl)
    for each(v from n!live-vars.value.to-list()):
      cur = if live-ranges.has-key-now(v): live-ranges.get-value-now(v) else: sets.empty-tree-set end
      live-ranges.set-now(v, cur.add(lbl))
    end
  end

  discardable-vars = for fold(acc from sets.empty-tree-set, lbl from labels):
    n = dag.get-value(lbl)
    cases(Option) n!dead-after-vars:
      | none => acc
      | some(dead) => acc.union(dead)
    end
  end
  
  # print("Done")
  results(body-cases, discardable-vars)
end
