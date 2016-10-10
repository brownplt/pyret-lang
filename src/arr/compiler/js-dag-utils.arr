#lang pyret

provide *
provide-types *
import ast as A
import lists as L
import sets as S
import file("ast-anf.arr") as N
import file("js-ast.arr") as J
import file("gensym.arr") as G
import file("compile-structs.arr") as CS
import file("concat-lists.arr") as CL
import file("data-struct-utils.arr") as DSU
#import "compiler/live-ranges.arr" as LR
import string-dict as D
import srcloc as SL
import valueskeleton as VS

type ConcatList = CL.ConcatList
type NameSet = D.MutableStringDict<A.Name>
type FrozenNameSet = D.StringDict<A.Name>

cl-sing = CL.concat-singleton
cl-empty = CL.concat-empty
cl-snoc = CL.concat-snoc
cl-cons = CL.concat-cons
ns-empty = D.make-mutable-string-dict

fun difference(s1 :: FrozenNameSet, s2 :: FrozenNameSet) block:
  s1-unfrozen-copy = s1.unfreeze()
  remove-overlap-now(s1-unfrozen-copy, s2.unfreeze())
  s1-unfrozen-copy.freeze()
end

fun copy-nameset(s :: NameSet) -> NameSet:
  s.freeze().unfreeze()
end

# does NOT mutate s1 or s2
fun difference-now(s1 :: NameSet, s2 :: NameSet) -> NameSet block:
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
      location :: Number,
      ref free-vars :: NameSet,
      ref used-vars :: NameSet, # use[n]
      ref decl-vars :: NameSet, # def[n]
      ref live-vars :: Option<NameSet>, # in[n]
      ref live-after-vars :: Option<NameSet>, # out[n]
      ref dead-vars :: Option<NameSet>,
      ref dead-after-vars :: Option<NameSet>,
      ref protected-after-vars :: NameSet)
    # note: _to is a set, determined by identical
    # _from = prec[n]
    # _to = succ[n]
    with:
    # For sorting
    method _lessthan(self, other):
      self.location < other.location
    end
end

data LiveInterval:
  | live-interval(variable :: A.Name, first :: String, last :: String, ref protekted :: Boolean, dag)
    with:
    method startloc(self): self.dag.get-value(self.first).location end,
    method endloc(self): self.dag.get-value(self.last).location end,
    method withlast(self, last): live-interval(self.variable, self.first, last, self!protekted, self.dag) end,
    method set-protected(self, val):
      self!{protekted: val}
    end,
    method is-protected(self):
      self!protekted
    end,
    method _lessthan(self, other): self.endloc() < other.endloc() end,
    method _output(self):
      VS.vs-constr("live-interval",
        # DAG Omitted
        [list: VS.vs-value({variable: self.variable, first: self.first, last: self.last, protekted: self!protekted})])
    end
end

data CaseResults:
  | c-exp(exp :: J.JExpr, other-stmts :: ConcatList<J.JStmt>)
  | c-field(field :: J.JField, other-stmts :: ConcatList<J.JStmt>)
  | c-block(block :: J.JBlock, new-cases :: ConcatList<J.JCase>)
end

data RegisterAllocation:
  | results(body :: ConcatList<J.JCase>, discardable-vars :: FrozenNameSet)
end


fun used-vars-jblock(b :: J.JBlock) -> {NameSet; NameSet} block:
  acc = ns-empty()
  prot = ns-empty()
  cases(J.JBlock) b block:
    | j-block1(s) =>
      {acc-s; prot-s} = used-vars-jstmt(s)
      acc.merge-now(acc-s)
      prot.merge-now(prot-s)
    | j-block(stmts) =>
      for CL.each(s from stmts) block:
        {acc-s; prot-s} = used-vars-jstmt(s)
        acc.merge-now(acc-s)
        prot.merge-now(prot-s)
      end
  end
  {acc; prot}
end
fun declared-vars-jblock(b :: J.JBlock) -> NameSet block:
  acc = ns-empty()
  cases(J.JBlock) b:
    | j-block1(s) => acc.merge-now(declared-vars-jstmt(s))
    | j-block(stmts) =>
      for CL.each(s from stmts):
        acc.merge-now(declared-vars-jstmt(s))
      end
  end
  acc
end
fun declared-vars-jstmt(s :: J.JStmt) -> NameSet:
  cases(J.JStmt) s block:
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
fun used-vars-jstmt(s :: J.JStmt) -> {NameSet; NameSet}:
  cases(J.JStmt) s block:
    | j-var(name, rhs) =>
      {acc; prot} = used-vars-jexpr(rhs)
      acc.remove-now(name.key())
      {acc; prot}
    | j-if1(cond, consq) =>
      {acc; prot} = used-vars-jexpr(cond)
      {acc-consq; prot-consq} = used-vars-jblock(consq)
      acc.merge-now(acc-consq)
      prot.merge-now(prot-consq)
      {acc; prot}
    | j-if(cond, consq, alt) =>
      {acc; prot} = used-vars-jexpr(cond)
      {acc-consq; prot-consq} = used-vars-jblock(consq)
      {acc-alt; prot-alt} = used-vars-jblock(alt)
      acc.merge-now(acc-consq)
      prot.merge-now(prot-consq)
      acc.merge-now(acc-alt)
      prot.merge-now(prot-alt)
      {acc; prot}
    | j-return(expr) => used-vars-jexpr(expr)
    | j-try-catch(body, exn, catch) =>
      {ns-catch; ns-catch-prot} = used-vars-jblock(catch)
      ns-catch.remove-now(exn.key())
      {acc-body; prot-body} = used-vars-jblock(body)
      acc-body.merge-now(ns-catch)
      prot-body.merge-now(ns-catch-prot)
      {acc-body; prot-body}
    | j-throw(exp) => used-vars-jexpr(exp)
    | j-expr(expr) => used-vars-jexpr(expr)
    | j-break => {ns-empty(); ns-empty()}
    | j-continue => {ns-empty(); ns-empty()}
    | j-switch(exp, branches) =>
      {acc; prot} = used-vars-jexpr(exp)
      for CL.each(b from branches) block:
        {acc-b; prot-b} = used-vars-jcase(b)
        acc.merge-now(acc-b)
        prot.merge-now(prot-b)
      end
      {acc; prot}
    | j-while(cond, body) =>
      {acc; prot} = used-vars-jexpr(cond)
      {acc-body; prot-body} = used-vars-jblock(body)
      acc.merge-now(acc-body)
      prot.merge-now(prot-body)
      {acc; prot}
    | j-for(create-var, init, cont, update, body) =>
      {acc; prot} = used-vars-jexpr(init)
      {acc-cont; prot-cont} = used-vars-jexpr(cont)
      {acc-update; prot-update} = used-vars-jexpr(update)
      {acc-body; prot-body} = used-vars-jblock(body)
      acc.merge-now(acc-cont)
      prot.merge-now(prot-cont)
      acc.merge-now(acc-update)
      prot.merge-now(prot-update)
      acc.merge-now(acc-body)
      prot.merge-now(prot-body)
      when create-var and J.is-j-assign(init):
        acc.remove-now(init.name.key())
      end
      {acc; prot}
  end
end
fun used-vars-jexpr(e :: J.JExpr) -> {NameSet; NameSet}:
  cases(J.JExpr) e block:
    | j-parens(exp) => used-vars-jexpr(exp)
    | j-unop(exp, op) => used-vars-jexpr(exp)
    | j-binop(left, op, right) => 
      {ans-left; protected-left} = used-vars-jexpr(left)
      {ans-right; protected-right} = used-vars-jexpr(right)
      ans-left.merge-now(ans-right)
      protected-left.merge-now(protected-right)
      {ans-left; protected-left}
    | j-fun(args, body) =>
      {used-body; protected-body} = used-vars-jblock(body)
      # Remove local-scope-only variables
      acc = difference-now(used-body, declared-vars-jblock(body))
      # Remove arguments
      for CL.each(a from args) block:
        acc.remove-now(a.key())
        protected-body.remove-now(a.key())
      end
      # Merge with *protected*, since we want to protect
      # everything that we close over
      protected-body.merge-now(acc)
      {acc; protected-body}
    | j-new(func, args) =>
      {acc; prot} = used-vars-jexpr(func)
      for CL.each(a from args) block:
        {arg-acc; arg-prot} = used-vars-jexpr(a)
        acc.merge-now(arg-acc)
        prot.merge-now(arg-prot)
      end
      {acc; prot}
    | j-app(func, args) =>
      {acc; prot} = used-vars-jexpr(func)
      for CL.each(a from args) block:
        {arg-acc; arg-prot} = used-vars-jexpr(a)
        acc.merge-now(arg-acc)
        prot.merge-now(arg-prot)
      end
      {acc; prot}
    | j-method(obj, meth, args) =>
      {acc; prot} = used-vars-jexpr(obj)
      for CL.each(a from args) block:
        {arg-acc; arg-prot} = used-vars-jexpr(a)
        acc.merge-now(arg-acc)
        prot.merge-now(arg-prot)
      end
      {acc; prot}
    | j-ternary(test, consq, altern) =>
      {acc; prot} = used-vars-jexpr(test)
      {acc-consq; prot-consq} = used-vars-jexpr(consq)
      {acc-altern; prot-altern} = used-vars-jexpr(altern)
      acc.merge-now(acc-consq)
      prot.merge-now(prot-consq)
      acc.merge-now(acc-altern)
      prot.merge-now(prot-altern)
      {acc; prot}
    | j-assign(name, rhs) => 
      {acc; prot} = used-vars-jexpr(rhs)
      # Should be safe, since this should never be in
      # the protected set
      acc.set-now(name.key(), name)
      {acc; prot}
    | j-bracket-assign(obj, field, rhs) =>
      {acc; prot} = used-vars-jexpr(obj)
      {acc-field; prot-field} = used-vars-jexpr(field)
      {acc-rhs; prot-rhs} = used-vars-jexpr(rhs)
      acc.merge-now(acc-field)
      prot.merge-now(prot-field)
      acc.merge-now(acc-rhs)
      prot.merge-now(prot-rhs)
      {acc; prot}
    | j-dot-assign(obj, name, rhs) =>
      {acc; prot} = used-vars-jexpr(obj)
      {acc-rhs; prot-rhs} = used-vars-jexpr(rhs)
      acc.merge-now(acc-rhs)
      prot.merge-now(prot-rhs)
      {acc; prot}
    | j-dot(obj, field) => used-vars-jexpr(obj)
    | j-bracket(obj, field) => 
      {acc; prot} = used-vars-jexpr(obj)
      {acc-field; prot-field} = used-vars-jexpr(field)
      acc.merge-now(acc-field)
      prot.merge-now(prot-field)
      {acc; prot}
    | j-list(_, elts) =>
      acc = ns-empty()
      prot = ns-empty()
      for CL.each(elt from elts) block:
        {acc-elt; prot-elt} = used-vars-jexpr(elt)
        acc.merge-now(acc-elt)
        prot.merge-now(prot-elt)
      end
      {acc; prot}
    | j-obj(fields) =>
      acc = ns-empty()
      prot = ns-empty()
      for CL.each(f from fields) block:
        {acc-f; prot-f} = used-vars-jfield(f)
        acc.merge-now(acc-f)
        prot.merge-now(prot-f)
      end
      {acc; prot}
    | j-id(id) => {[D.mutable-string-dict: id.key(), id]; ns-empty()}
    | j-str(_) => {ns-empty(); ns-empty()}
    | j-num(_) => {ns-empty(); ns-empty()}
    | j-true => {ns-empty(); ns-empty()}
    | j-false => {ns-empty(); ns-empty()}
    | j-null => {ns-empty(); ns-empty()}
    | j-undefined => {ns-empty(); ns-empty()}
    | j-label(_) => {ns-empty(); ns-empty()}
  end
end
fun declared-vars-jcase(c :: J.JCase) -> NameSet:
  cases(J.JCase) c:
    | j-case(exp, body) => declared-vars-jblock(body)
    | j-default(body) => declared-vars-jblock(body)
  end
end
fun used-vars-jcase(c :: J.JCase) -> {NameSet; NameSet}:
  cases(J.JCase) c block:
    | j-case(exp, body) =>
      {acc; prot} = used-vars-jexpr(exp)
      {acc-body; prot-body} = used-vars-jblock(body)
      acc.merge-now(acc-body)
      prot.merge-now(prot-body)
      {acc; prot}
    | j-default(body) => used-vars-jblock(body)
  end
end
fun used-vars-jfield(f :: J.JField) -> {NameSet; NameSet}:
  used-vars-jexpr(f.value)
end

fun compute-live-vars(n :: GraphNode, dag :: D.StringDict<GraphNode>) -> NameSet:
  cases(Option) n!live-vars block:
    | some(live) => 
      live
    | none =>
      live-after = copy-nameset(n!free-vars) # use[n]
      for CL.each(follow from n._to) block: # add out[n] to live-after
        next-opt = dag.get(tostring(follow.get())) # Look up nodes in DAG
        when is-some(next-opt): # Sanity check (?)
          next = next-opt.value # Actual node in dag
          next-vars = compute-live-vars(next, dag) # in[next]
          live-after.merge-now(next-vars) # out[n] U= in[next]
        end
      end
      decls = n!decl-vars
      # in[n] = use[n] U (out[n] - def[n])
      live = difference-now(live-after, decls) # subtract def[n] to get in[n]
      dead-after = difference-now(decls, live-after)
      dead = difference-now(dead-after, n!used-vars)

      n!{live-after-vars: some(live-after), live-vars: some(live),
        dead-after-vars: some(dead-after), dead-vars: some(dead)}
      live # returns in[n]
  end
end

fun compute-live-ranges(dag :: D.StringDict<GraphNode>) -> List<LiveInterval> block:
  live-ranges = D.make-mutable-string-dict()
  sorted-keys = dag.keys-list().sort-by(
    lam(x, y): dag.get-value(x).location <  dag.get-value(y).location end,
    lam(x, y): dag.get-value(x).location == dag.get-value(y).location end)
  for each(lbl from sorted-keys) block:
    n = dag.get-value(lbl)
    for each(v from n!decl-vars.keys-list-now()): # For v in def[n]
      if live-ranges.has-key-now(v) block:
        redeclared = live-ranges.get-value-now(v).variable.tosourcestring()
        region = n.case-body.tosource().pretty(80).join-str("\n")
        live-ranges.set-now(v, live-ranges.get-value-now(v).withlast(lbl))
      else:
        live-ranges.set-now(v, live-interval(n!decl-vars.get-value-now(v), lbl, lbl, n!protected-after-vars.has-key-now(v), dag))
      end
    end
    for each(v from n!live-vars.value.keys-list-now()):
      when live-ranges.has-key-now(v):
        live-ranges.set-now(v, live-ranges.get-value-now(v).withlast(lbl))
      end
    end
    for each(v from n!protected-after-vars.keys-list-now()):
      when live-ranges.has-key-now(v) block:
        #print("Protected: " + v + "\n")
        live-ranges.get-value-now(v).set-protected(true)
      end
    end
  end
  map(live-ranges.get-value-now(_), live-ranges.keys-now().to-list()).sort-by(
    lam(x, y): x.startloc() < y.startloc() end,
    lam(x, y): x.startloc() == y.startloc() end)
end

fun allocate-variables(dag :: D.StringDict<GraphNode>) block:
  #doc: ```
  #     Returns a mapping of name.key() -> <allocated name>.
  #     We use a variation of Poletto and Sarkar's Linear Register Allocation
  #     scan to condense variable names with different live ranges.
  #     Further reading: http://belph.github.io/docs/pyret-register-alloc.pdf
  #     ```
  live-ranges = compute-live-ranges(dag)
  # Dynamically grown set of "registers"
  pool = DSU.make-mutable-stack()
  # Allocation dictionary
  ret = D.make-mutable-string-dict()
  # Priority heap (min = element with earliest ending)
  active = DSU.make-mutable-pairing-heap()
  # Should be the same as ARGUMENTS name in anf-loop-compiler
  ARGUMENTS = A.s-name(A.dummy-loc, "arguments")
  
  fun prune-dead(i-cur):
    cases(Option) active.find-min():
      | none => nothing
      | some(i-min) =>
        # Interval is expired
        if i-min.endloc() < i-cur.startloc() block:
          active.delete-min()
          # Push the register allocated to i-min back into pool
          when (i-min.variable <> ARGUMENTS) and not(i-min.is-protected()):
            pool.push(ret.get-value-now(i-min.variable.key()))
          end
          # Recur to prune any additional intervals
          prune-dead(i-cur)
        else:
          nothing
        end
    end
  end

  var num-ranges = 0
  var num-registers = 0
  # Perform allocation
  for each(i from live-ranges) block:
    num-ranges := num-ranges + 1
    prune-dead(i)
    active.insert(i)
    cases(Option) pool.pop() block:
      | none => ret.set-now(i.variable.key(), i.variable)
        num-registers := num-registers + 1
      | some(reg) =>
        ret.set-now(i.variable.key(), reg)
    end
  end
  # Return assignments
  {num-ranges; num-registers; ret.freeze()}
end

fun stmts-of(blk :: J.JBlock):
  cases(J.JBlock) blk:
    | j-block1(s) => cl-sing(s)
    | j-block(stmts) => stmts
  end
end

fun find-steps-to(stmts :: ConcatList<J.JStmt>, step :: A.Name):
  var looking-for = none
  for CL.foldr(acc from cl-empty, stmt from stmts):
    cases(J.JStmt) stmt:
      | j-var(name, rhs) =>
        if is-some(looking-for) and (looking-for.value == name) block:
          looking-for := none
          for CL.foldl(shadow acc from acc, field from rhs.fields):
            cl-snoc(acc, field.value.label)
          end
        else:
          acc
        end
      | j-if1(cond, consq) => acc + find-steps-to(stmts-of(consq), step)
      | j-if(cond, consq, alt) =>
        acc + find-steps-to(stmts-of(consq), step) + find-steps-to(stmts-of(alt), step)
      | j-return(expr) => acc
      | j-try-catch(body, exn, catch) => acc # ignoring for now, because we know we don't use these
      | j-throw(exp) => acc
      | j-expr(expr) =>
        if J.is-j-assign(expr) and (expr.name == step):
          if J.is-j-label(expr.rhs) block:
            # simple assignment statement to $step
            cl-snoc(acc, expr.rhs.label)
          else if J.is-j-binop(expr.rhs) and (expr.rhs.op == J.j-or):
            # $step gets a cases dispatch
            # ASSUMES that the dispatch table is assigned two statements before this one
            looking-for := some(expr.rhs.left.obj.id)
            cl-snoc(acc, expr.rhs.right.label)
          else if J.is-j-ternary(expr.rhs):
            # ASSUMES that the only current use of $step = ( ? : )
            # comes from compile-split-if
            cl-snoc(cl-snoc(acc, expr.rhs.consq.label), expr.rhs.altern.label)
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



fun elim-dead-vars-jblock(b :: J.JBlock, dead-vars :: FrozenNameSet, allocation-visitor):
  cases(J.JBlock) b:
    | j-block1(s) =>
      if is-pointless-j-var(s, dead-vars): J.j-block(cl-empty)
      else: b.visit(allocation-visitor)
      end
    | j-block(stmts) => J.j-block(elim-dead-vars-jstmts(stmts, dead-vars, allocation-visitor))
  end
end

fun is-pointless-j-var(s, dead-vars):
  cases(J.JStmt) s:
    | j-var(name, rhs) => dead-vars.has-key(name.key()) and ignorable(rhs)
    | else => false
  end
end

fun elim-dead-vars-jstmts(stmts :: ConcatList<J.JStmt>, dead-vars :: FrozenNameSet, allocation-visitor):
  for CL.foldl(acc from cl-empty, s from stmts):
    cases(J.JStmt) s:
      | j-var(name, rhs) =>
        if dead-vars.has-key(name.key()):
          if ignorable(rhs): acc
          else: acc ^ cl-snoc(_, J.j-expr(rhs.visit(allocation-visitor)))
          end
        else: acc ^ cl-snoc(_, s.visit(allocation-visitor))
        end
      | j-if1(cond, consq) =>
        acc ^ cl-snoc(_, J.j-if1(cond.visit(allocation-visitor), elim-dead-vars-jblock(consq, dead-vars, allocation-visitor)))
      | j-if(cond, consq, alt) =>
        acc ^ cl-snoc(_,
          J.j-if(cond.visit(allocation-visitor), elim-dead-vars-jblock(consq, dead-vars, allocation-visitor), elim-dead-vars-jblock(alt, dead-vars, allocation-visitor)))
      | j-return(expr) => acc ^ cl-snoc(_, s.visit(allocation-visitor))
      | j-try-catch(body, exn, catch) =>
        acc ^ cl-snoc(_,
          J.j-try-catch(elim-dead-vars-jblock(body, dead-vars, allocation-visitor), exn.visit(allocation-visitor), elim-dead-vars-jblock(catch, dead-vars, allocation-visitor)))
      | j-throw(exp) => acc ^ cl-snoc(_, s.visit(allocation-visitor))
      | j-expr(expr) => acc ^ cl-snoc(_, s.visit(allocation-visitor))
      | j-break => acc ^ cl-snoc(_, s.visit(allocation-visitor))
      | j-continue => acc ^ cl-snoc(_, s.visit(allocation-visitor))
      | j-switch(exp, branches) =>
        new-switch-branches = for map(b from branches):
          elim-dead-vars-jcase(b, dead-vars, allocation-visitor)
        end
        acc ^ cl-snoc(_, J.j-switch(exp.visit(allocation-visitor), new-switch-branches))
      | j-while(cond, body) =>
        acc ^ cl-snoc(_, J.j-while(cond.visit(allocation-visitor), elim-dead-vars-jblock(body, dead-vars, allocation-visitor)))
      | j-for(create-var, init, cont, update, body) =>
        acc ^ cl-snoc(_, J.j-for(create-var, init.visit(allocation-visitor), cont.visit(allocation-visitor), update.visit(allocation-visitor), elim-dead-vars-jblock(body, dead-vars, allocation-visitor)))
    end
  end
end
fun elim-dead-vars-jcase(c :: J.JCase, dead-vars :: FrozenNameSet, allocation-visitor):
  cases(J.JCase) c:
    | j-default(body) => J.j-default(elim-dead-vars-jblock(body, dead-vars, allocation-visitor))
    | j-case(exp, body) => J.j-case(exp, elim-dead-vars-jblock(body, dead-vars, allocation-visitor))
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
fun simplify(body-cases :: ConcatList<J.JCase>, step :: A.Name, loc) -> RegisterAllocation block:
  # print("Step 1: " + step + " num cases: " + tostring(body-cases.length()))
  acc-dag = D.make-mutable-string-dict()
  var case-num = 0
  for CL.each(body-case from body-cases) block:
    case-num := case-num + 1
    when J.is-j-case(body-case):
      acc-dag.set-now(tostring(body-case.exp.label.get()),
        node(body-case.exp.label,
          cases(J.JBlock) body-case.body:
            | j-block1(s) => find-steps-to(cl-sing(s), step)
            | j-block(stmts) => find-steps-to(stmts, step)
          end, body-case, case-num,
          ns-empty(), ns-empty(), ns-empty(), none, none, none, none, ns-empty()))
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
  for each(lbl from str-labels) block:
    n = dag.get-value(lbl)
    n!{decl-vars: declared-vars-jcase(n.case-body)}
    {used; protekted} = used-vars-jcase(n.case-body)
    n!{used-vars: used}
    free-vars = difference-now(n!used-vars, n!decl-vars)
    protekted.merge-now(free-vars)
    n!{protected-after-vars: protekted}
    n!{free-vars: free-vars}
  end
  for each(lbl from str-labels):
    n = dag.get-value(lbl)
    compute-live-vars(n, dag)
  end
  # for each(lbl from str-labels) block:
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
  # print("Step 4")
  # live-ranges = D.make-mutable-string-dict()
  # for each(lbl from labels):
  #   n = dag.get-value(lbl)
  #   for each(v from n!live-vars.value.keys-list()):
  #     cur = if live-ranges.has-key-now(v.tosourcestring()): live-ranges.get-value-now(v) else: ns-empty end
  #     live-ranges.set-now(v.tosourcestring(), cur.add(lbl))
  #   end
  # end
  {num-before; num-after; allocated} = allocate-variables(dag)
  assigned = D.make-mutable-string-dict()
  fix-name = lam(name):
    cases(Option) allocated.get(name.key()):
      | none => name # Dead variables not included in allocation
      | some(n) => n
    end
  end
  allocation-visitor = if false block:
    print("\nSkipping register allocation\n")
    J.default-map-visitor
  else:
    print("\nDoing register allocation at " + tostring(loc) + "\n")
    J.default-map-visitor.{
      method j-var(self, name, rhs) block:
        # Need to avoid duplicating var declarations
        fixed = fix-name(name)
        if assigned.has-key-now(fixed.key()) block:
          # For cases expressions...
          #
          # I do not have proof
          # That this works in any way
          # But I think it does.
          #                 -- Basho
          if fixed.key() == name.key():
            J.j-var(fixed, rhs.visit(self))
          else:
            J.j-expr(J.j-assign(fixed, rhs.visit(self)))
          end
        else:
          assigned.set-now(fixed.key(), true)
          J.j-var(fixed, rhs.visit(self))
        end
      end,
      method j-try-catch(self, body, exn, catch):
        J.j-try-catch(body.visit(self), fix-name(exn), catch.visit(self))
      end,
      method j-fun(self, args, body):
        J.j-fun(CL.map(fix-name, args), body.visit(self))
      end,
      method j-assign(self, name, rhs):
        J.j-assign(fix-name(name), rhs.visit(self))
      end,
      method j-id(self, id):
        J.j-id(fix-name(id))
      end
    }
  end
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
      | none => body-case.visit(allocation-visitor)
      | some(dead-vars) => elim-dead-vars-jcase(body-case, dead-vars.freeze(), allocation-visitor)
    end
  end

  registers = D.make-mutable-string-dict()
  for each(v from allocated.keys().to-list()):
    registers.set-now(allocated.get-value(v).key(), true)
  end
  # When statement to only show interesting cases
  print("# of live variables Before: \t" + tostring(num-before) + "\tAfter: \t" + tostring(num-after) + "\n")
  COLOR-PRE = string-from-code-point(27) + "[96m"
  COLOR-POST = string-from-code-point(27) + "[0m"
  when num-before <> num-after block:
    print("Renaming:\n")
    for each(v from allocated.keys().to-list()):
      dest = allocated.get-value(v).key()
      print("\t" + if dest <> v:
          (COLOR-PRE + v + " => " + allocated.get-value(v).key() + COLOR-POST)
        else:
          (v + " => " + allocated.get-value(v).key())
        end + "\n")
    end
  end

  # print("Done")
  results(dead-assignment-eliminated, discardable-vars)
end
