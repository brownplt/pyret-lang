#lang pyret

provide *
import ast as A
import sets as Sets
import "compiler/ast-anf.arr" as N
import "compiler/ast-split.arr" as S
import "compiler/js-ast.arr" as J
import "compiler/gensym.arr" as G
import "compiler/compile-structs.arr" as CS
import string-dict as D
import srcloc as SL

type Loc = SL.Srcloc

fun type-name(str):
  "$type$" + str
end

j-fun = J.j-fun
j-var = J.j-var
j-id = J.j-id
j-method = J.j-method
j-block = J.j-block
j-true = J.j-true
j-false = J.j-false
j-num = J.j-num
j-str = J.j-str
j-return = J.j-return
j-assign = J.j-assign
j-if = J.j-if
j-if1 = J.j-if1
j-app = J.j-app
j-list = J.j-list
j-obj = J.j-obj
j-dot = J.j-dot
j-bracket = J.j-bracket
j-field = J.j-field
j-dot-assign = J.j-dot-assign
j-bracket-assign = J.j-bracket-assign
j-try-catch = J.j-try-catch
j-throw = J.j-throw
j-expr = J.j-expr
j-binop = J.j-binop
j-eq = J.j-eq
j-neq = J.j-neq
j-unop = J.j-unop
j-decr = J.j-decr
j-incr = J.j-incr
j-not = J.j-not
j-ternary = J.j-ternary
j-null = J.j-null
j-parens = J.j-parens
j-switch = J.j-switch
j-case = J.j-case
j-default = J.j-default
j-label = J.j-label
j-break = J.j-break
j-while = J.j-while
make-label-sequence = J.make-label-sequence

get-field-loc = j-id("G")
throw-uninitialized = j-id("U")
source-name = j-id("M")
undefined = j-id("D")

data ConcatList<a>:
  | concat-empty with:
    to-list-acc(self, rest): rest end,
    map(self, f): self end,
    each(self, f): nothing end,
    foldl(self, f, base): base end,
    foldr(self, f, base): base end
  | concat-singleton(element) with:
    to-list-acc(self, rest): link(self.element, rest) end,
    map(self, f): concat-singleton(f(self.element)) end,
    each(self, f):
      f(self.element)
      nothing
    end,
    foldl(self, f, base): f(base, self.element) end,
    foldr(self, f, base): f(self.element, base) end
  | concat-append(left :: ConcatList<a>, right :: ConcatList<a>) with:
    to-list-acc(self, rest :: List):
      self.left.to-list-acc(self.right.to-list-acc(rest))
    end,
    map(self, f): concat-append(self.left.map(f), self.right.map(f)) end,
    each(self, f):
      self.left.each(f)
      self.right.each(f)
    end,
    foldl(self, f, base): self.right.foldl(f, self.left.foldl(f, base)) end,
    foldr(self, f, base): self.left.foldr(f, self.right.foldr(f, base)) end
  | concat-cons(first :: a, rest :: ConcatList<a>) with:
    to-list-acc(self, rest): link(self.first, self.rest.to-list-acc(rest)) end,
    map(self, f): concat-cons(f(self.first), self.rest.map(f)) end,
    each(self, f):
      f(self.first)
      self.rest.each(f)
    end,
    foldl(self, f, base): self.rest.foldl(f, f(base, self.first)) end,
    foldr(self, f, base): f(self.first, self.rest.foldr(f, base)) end
  | concat-snoc(head :: ConcatList<a>, last :: a) with:
    to-list-acc(self, rest): self.head.to-list-acc(link(self.last, rest)) end,
    map(self, f): concat-snoc(self.head.map(f), f(self.last)) end,
    each(self, f):
      self.head.each(f)
      f(self.last)
      nothing
    end,
    foldl(self, f, base): f(self.head.foldl(f, base), self.last) end,
    foldr(self, f, base): self.head.foldr(f, f(self.last, base)) end
sharing:
  _plus(self, other :: ConcatList):
    concat-append(self, other)
  end,
  to-list(self): self.to-list-acc([list: ]) end
where:
  ce = concat-empty
  co = concat-singleton
  ca = concat-append
  cc = concat-cons
  cs = concat-snoc
  l1 = ca(cs(cc(1, ce), 2), cc(3, cs(ce, 4)))
  l1.foldl(lam(base, e): base + tostring(e * e) end, "B") is "B14916"
  l1.foldr(lam(e, base): tostring(e * e) + base end, "B") is "14916B"
end
fun concat-foldl(f, base, lst): lst.foldl(f, base) end
fun concat-foldr(f, base, lst): lst.foldr(f, base) end

js-id-of = block:
  var js-ids = D.string-dict()
  lam(id :: String):
    when not(is-string(id)): raise("js-id-of got non-string: " + torepr(id));
    if js-ids.has-key(id):
      js-ids.get(id)
    else:
      no-hyphens = string-replace(id, "-", "_DASH_")
      safe-id = G.make-name(no-hyphens)
      js-ids.set(id, safe-id)
      safe-id
    end
  end
end

fun compiler-name(id):
  G.make-name("$" + id)
end

fun obj-of-loc(l):
  j-list(false, [list: 
    j-id("M"),
    j-num(l.start-line),
    j-num(l.start-column),
    j-num(l.start-char),
    j-num(l.end-line),
    j-num(l.end-column),
    j-num(l.end-char)
  ])
end

fun get-field(obj, field, loc):
  j-app(get-field-loc, [list: obj, field, loc])
end

fun raise-id-exn(loc, name):
  j-app(throw-uninitialized, [list: loc, j-str(name)])
end

fun add-stack-frame(exn-id, loc):
  j-method(j-dot(j-id(exn-id), "pyretStack"), "push", [list: loc])
end

fun rt-field(name): j-dot(j-id("R"), name);
fun rt-method(name, args): j-method(j-id("R"), name, args);

fun app(l, f, args):
  j-method(f, "app", args)
end

fun check-fun(l, f):
  j-if1(j-unop(j-parens(rt-method("isFunction", [list: f])), j-not),
    j-block([list: j-expr(j-method(rt-field("ffi"), "throwNonFunApp", [list: l, f]))]))
end

fun thunk-app(block):
  j-app(j-parens(j-fun([list: ], block)), [list: ])
end

fun thunk-app-stmt(stmt):
  thunk-app(j-block([list: stmt]))
end

fun helper-name(s :: String): "$H" + js-id-of(s.tostring());


data CaseResults:
  | c-exp(exp :: J.JExpr, other-stmts :: List<J.JStmt>)
  | c-field(field :: J.JField, other-stmts :: List<J.JStmt>)
  | c-block(block :: J.JBlock, new-cases :: ConcatList<J.JCase>)
end

fun compile-ann(ann :: A.Ann, visitor) -> CaseResults:
  cases(A.Ann) ann:
    | a-name(_, n) => c-exp(j-id(js-id-of(n.tostring())), empty)
    | a-arrow(_, _, _, _) => c-exp(rt-field("Function"), empty)
    | a-method(_, _, _) => c-exp(rt-field("Method"), empty)
    | a-app(l, base, _) => compile-ann(base, visitor)
    | a-record(l, fields) =>
      names = j-list(false, fields.map(_.name).map(j-str))
      locs = j-list(false, fields.map(_.l).map(visitor.get-loc))
      anns = for fold(acc from {fields: empty, others: empty}, f from fields):
        compiled = compile-ann(f.ann, visitor)
        {
          fields: j-field(f.name, compiled.exp) ^ link(_, acc.fields),
          others: compiled.other-stmts.reverse() + acc.others
        }
      end
      c-exp(
        rt-method("makeRecordAnn", [list:
            names,
            locs,
            j-obj(anns.fields.reverse())
          ]),
        anns.others.reverse()
        )
    | a-pred(l, base, exp) =>
      name = cases(A.AExpr) exp:
        | s-id(_, id) => id.toname()
        | s-id-letrec(_, id, _) => id.toname()
      end
      expr-to-compile = cases(A.Expr) exp:
        | s-id(l2, id) => N.a-id(l2, id)
        | s-id-letrec(l2, id, ok) => N.a-id-letrec(l2, id, ok)
      end
      compiled-base = compile-ann(base, visitor)
      compiled-exp = expr-to-compile.visit(visitor)
      c-exp(
        rt-method("makePredAnn", [list: compiled-base.exp, compiled-exp.exp, j-str(name)]),
        compiled-base.other-stmts +
        compiled-exp.other-stmts
        )
    | a-dot(l, m, field) =>
      c-exp(
        rt-method("getDotAnn", [list:
            visitor.get-loc(l),
            j-str(m.toname()),
            j-id(js-id-of(m.tostring())),
            j-str(field)]),
        empty)
    | a-blank => c-exp(rt-field("Any"), empty)
    | a-any => c-exp(rt-field("Any"), empty)
  end
end

fun arity-check(loc-expr, arity):
  j-if1(j-binop(j-dot(j-id("arguments"), "length"), j-neq, j-num(arity)),
    j-block([list:
        j-expr(j-method(rt-field("ffi"), "throwArityErrorC",
            [list: loc-expr, j-num(arity), j-id("arguments")]))]))
end

local-bound-vars-visitor = {
  j-field(self, name, value): value.visit(self) end,
  j-parens(self, exp): exp.visit(self) end,
  j-unop(self, exp, op): exp.visit(self) end,
  j-binop(self, left, op, right): left.visit(self).union(right.visit(self)) end,
  j-fun(self, args, body): sets.empty-tree-set end,
  j-app(self, func, args): args.foldl(lam(arg, base): base.union(arg.visit(self)) end, func.visit(self)) end,
  j-method(self, obj, meth, args): sets.empty-tree-set end,
  j-ternary(self, test, consq, alt): test.visit(self).union(consq.visit(self)).union(alt.visit(self)) end,
  j-assign(self, name, rhs): rhs.visit(self) end,
  j-bracket-assign(self, obj, field, rhs): obj.visit(self).union(field.visit(self)).union(rhs.visit(self)) end,
  j-dot-assign(self, obj, name, rhs): obj.visit(self).union(rhs.visit(self)) end,
  j-dot(self, obj, name): obj.visit(self) end,
  j-bracket(self, obj, field): obj.visit(self).union(field.visit(self)) end,
  j-list(self, multi-line, elts):
    elts.foldl(lam(arg, base): base.union(arg.visit(self)) end, sets.empty-tree-set)
  end,
  j-obj(self, fields): fields.foldl(lam(f, base): base.union(f.visit(self)) end, sets.empty-tree-set) end,
  j-id(self, id): sets.empty-tree-set end,
  j-str(self, s): sets.empty-tree-set end,
  j-num(self, n): sets.empty-tree-set end,
  j-true(self): sets.empty-tree-set end,
  j-false(self): sets.empty-tree-set end,
  j-null(self): sets.empty-tree-set end,
  j-undefined(self): sets.empty-tree-set end,
  j-label(self, label): sets.empty-tree-set end,
  j-case(self, exp, body): exp.visit(self).union(body.visit(self)) end,
  j-default(self, body): body.visit(self) end,
  j-block(self, stmts): stmts.foldl(lam(s, base): base.union(s.visit(self)) end, sets.empty-tree-set) end,
  j-var(self, name, rhs): [tree-set: name].union(rhs.visit(self)) end,
  j-if1(self, cond, consq): cond.visit(self).union(consq.visit(self)) end,
  j-if(self, cond, consq, alt): cond.visit(self).union(consq.visit(self)).union(alt.visit(self)) end,
  j-return(self, exp): exp.visit(self) end,
  j-try-catch(self, body, exn, catch): body.visit(self).union(catch.visit(self)) end,
  j-throw(self, exp): exp.visit(self) end,
  j-expr(self, exp): exp.visit(self) end,
  j-break(self): sets.empty-tree-set end,
  j-continue(self): sets.empty-tree-set end,
  j-switch(self, exp, branches):
    branches.foldl(lam(b, base): base.union(b.visit(self)) end, exp.visit(self))
  end,
  j-while(self, cond, body): cond.visit(self).union(body.visit(self)) end
}


fun goto-case(step, label):
  [list: j-expr(j-assign(step, label)), j-break]
end

fun compile-fun-body(l :: Loc, step :: String, fun-name :: String, compiler, args :: List<N.ABind>, arity :: Number, body :: N.AExpr) -> J.JBlock:
  make-label = make-label-sequence(0)
  ret-label = make-label()
  ans = js-id-of(compiler-name("ans"))
  local-compiler = compiler.{make-label: make-label, cur-target: ret-label, cur-step: step, cur-ans: ans}
  visited-body = body.visit(local-compiler)
  checker =
    j-block([list:
        arity-check(local-compiler.get-loc(l), arity)])
  ann-cases = compile-anns(local-compiler, step, args, local-compiler.make-label())
  switch-cases =
    concat-empty
  ^ concat-append(_, ann-cases.new-cases)
  ^ concat-snoc(_, j-case(ann-cases.new-label, visited-body.block))
  ^ concat-append(_, visited-body.new-cases)
  ^ concat-snoc(_, j-case(local-compiler.cur-target, j-block([list:
          j-expr(j-unop(rt-field("GAS"), j-incr)),
          j-return(j-id(local-compiler.cur-ans))])))
  ^ concat-snoc(_, j-default(j-block([list:
          j-throw(j-binop(j-binop(j-str("No case numbered "), J.j-plus, j-id(step)), J.j-plus,
              j-str(" in " + fun-name)))])))
  # Initialize the case numbers, for more legible output...
  switch-cases.each(lam(c): when J.is-j-case(c): c.exp.label.get() end end) 
  vars = (for concat-foldl(base from Sets.empty-tree-set, case-expr from switch-cases):
      base.union(case-expr.visit(local-bound-vars-visitor))
    end).to-list()
  act-record = rt-method("makeActivationRecord", [list:
      local-compiler.get-loc(l),
      j-id(fun-name),
      j-id(step),
      j-id(local-compiler.cur-ans),
      j-list(false, args.map(lam(a): j-id(js-id-of(tostring(a.id))) end)),
      j-list(false, vars.map(lam(v): j-id(v) end))
    ])  
  e = js-id-of(compiler-name("e"))
  first-arg = js-id-of(tostring(args.first.id))
  ar = js-id-of(compiler-name("ar"))
  j-block([list:
      j-var(step, j-num(0)),
      j-var(local-compiler.cur-ans, undefined),
      j-try-catch(
        j-block([list:
            j-if(rt-method("isActivationRecord", [list: j-id(first-arg)]),
              j-block(
                [list:
                  j-var(ar, j-id(first-arg)),
                  j-expr(j-assign(step, j-dot(j-id(ar), "step"))),
                  j-expr(j-assign(local-compiler.cur-ans, j-dot(j-id(ar), "ans")))
                ] +
                for map_n(i from 0, arg from args):
                  j-expr(j-assign(js-id-of(tostring(arg.id)), j-bracket(j-dot(j-id(ar), "args"), j-num(i))))
                end +
                for map_n(i from 0, v from vars):
                  j-expr(j-assign(v, j-bracket(j-dot(j-id(ar), "vars"), j-num(i))))
                end),
              checker),
            j-if1(j-binop(j-unop(rt-field("GAS"), j-decr), J.j-leq, j-num(0)),
              j-block([list: j-expr(j-dot-assign(j-id("R"), "EXN_STACKHEIGHT", j-num(0))),
                  # j-expr(j-app(j-id("console.log"), [list: j-str("Out of gas in " + fun-name)])),
                  # j-expr(j-app(j-id("console.log"), [list: j-str("GAS is "), rt-field("GAS")])),
                  j-throw(rt-method("makeCont", empty))])),
            j-while(j-true,
              j-block([list:
                  # j-expr(j-app(j-id("console.log"), [list: j-str("In " + fun-name + ", step "), j-id(step), j-str(", GAS = "), rt-field("GAS"), j-str(", ans = "), j-id(local-compiler.cur-ans)])),
                  j-switch(j-id(step), switch-cases.to-list())]))]),
        e,
        j-block([list:
            j-if1(rt-method("isCont", [list: j-id(e)]),
              j-block([list: 
                  j-expr(j-bracket-assign(j-dot(j-id(e), "stack"),
                      j-unop(rt-field("EXN_STACKHEIGHT"), J.j-postincr), act-record))
                ])),
            j-if1(rt-method("isPyretException", [list: j-id(e)]),
              j-block([list: 
                  j-expr(add-stack-frame(e, local-compiler.get-loc(l)))
                ])),
            j-throw(j-id(e))]))
  ])
end

fun compile-anns(visitor, step, binds :: List<N.ABind>, entry-label):
  var cur-target = entry-label
  new-cases = for lists.fold(acc from concat-empty, b from binds):
    if A.is-a-blank(b.ann) or A.is-a-any(b.ann):
      acc
    else:
      compiled-ann = compile-ann(b.ann, visitor)
      new-label = visitor.make-label()
      new-case = j-case(cur-target,
        j-block(compiled-ann.other-stmts +
          [list:
            j-expr(j-assign(step, new-label)),
            j-expr(rt-method("_checkAnn",
              [list: visitor.get-loc(b.ann.l), compiled-ann.exp, j-id(js-id-of(b.id.tostring()))])),
            j-break]))
      cur-target := new-label
      concat-snoc(acc, new-case)
    end
  end
  { new-cases: new-cases, new-label: cur-target }
end

compiler-visitor = {
  a-module(self, l, answer, provides, types, checks):
    types-obj-fields = for fold(acc from {fields: empty, others: empty}, ann from types):
      compiled = compile-ann(ann.ann, self)
      {
        fields: j-field(ann.name, compiled.exp) ^ link(_, acc.fields),
        others: compiled.other-stmts.reverse() + acc.others
      }
    end

    compiled-provides = provides.visit(self)
    compiled-answer = answer.visit(self)
    compiled-checks = checks.visit(self)
    c-exp(
      rt-method("makeObject", [list:
          j-obj([list:
              j-field("answer", compiled-answer.exp),
              j-field("provide-plus-types",
                rt-method("makeObject", [list: j-obj([list:
                        j-field("values", compiled-provides.exp),
                        j-field("types", j-obj(types-obj-fields.fields.reverse()))
                    ])])),
              j-field("checks", compiled-checks.exp)])]),
      types-obj-fields.others
        + compiled-provides.other-stmts + compiled-answer.other-stmts + compiled-checks.other-stmts)
  end,
  a-type-let(self, l, bind, body):
    cases(N.ATypeBind) bind:
      | a-type-bind(l2, name, ann) =>
        visited-body = body.visit(self)
        compiled-ann = compile-ann(ann, self)
        c-block(
          j-block(
            compiled-ann.other-stmts +
            [list: j-var(js-id-of(name.tostring()), compiled-ann.exp)] +
            visited-body.block.stmts
            ),
          visited-body.new-cases)
      | a-newtype-bind(l2, name, nameb) =>
        brander-id = js-id-of(nameb.tostring())
        visited-body = body.visit(self)
        c-block(
          j-block(
            [list:
              j-var(brander-id, rt-method("namedBrander", [list: j-str(name.toname())])),
              j-var(js-id-of(name.tostring()), rt-method("makeBranderAnn", [list: j-id(brander-id), j-str(name.toname())]))
            ] +
            visited-body.block.stmts),
          visited-body.new-cases)
    end
  end,
  a-let(self, l :: Loc, b :: N.ABind, e :: N.ALettable, body :: N.AExpr):
    compiled-e = e.visit(self)
    compiled-body = body.visit(self)
    if A.is-a-blank(b.ann) or A.is-a-any(b.ann):
      c-block(
        j-block(
          compiled-e.other-stmts +
          link(
            j-var(js-id-of(b.id.tostring()), compiled-e.exp),
            compiled-body.block.stmts
            )
          ),
        compiled-body.new-cases
        )
    else:
      step = self.cur-step
      after-ann = self.make-label()
      after-ann-case = j-case(after-ann, j-block(compiled-body.block.stmts))
      compiled-ann = compile-ann(b.ann, self)
      c-block(
        j-block(
          compiled-e.other-stmts +
          compiled-ann.other-stmts +
          [list:
            j-var(js-id-of(b.id.tostring()), compiled-e.exp),
            j-expr(j-assign(step, after-ann)),
            j-expr(rt-method("_checkAnn", [list:
                  self.get-loc(b.ann.l),
                  compiled-ann.exp,
                  j-id(js-id-of(b.id.tostring()))])),
            j-break
          ]),
        concat-cons(after-ann-case, compiled-body.new-cases))
    end
  end,
  a-var(self, l :: Loc, b :: N.ABind, e :: N.ALettable, body :: N.AExpr):
    compiled-body = body.visit(self)
    compiled-e = e.visit(self)
    # TODO: annotations here?
    c-block(
      j-block(
        j-var(js-id-of(b.id.tostring()),
          j-obj([list: j-field("$var", compiled-e.exp), j-field("$name", j-str(b.id.toname()))]))
        ^ link(_, compiled-body.block.stmts)),
      compiled-body.new-cases)
  end,
  a-tail-app(self, l :: Loc, f :: N.AVal, args :: List<N.AVal>):
    ans = self.cur-ans
    step = self.cur-step
    compiled-f = f.visit(self).exp
    compiled-args = args.map(lam(a): a.visit(self).exp end)
    c-block(
      j-block([list:
          check-fun(self.get-loc(l), compiled-f),
          # Update step before the call, so that if it runs out of gas, the resumer goes to the right step
          j-expr(j-assign(step,  self.cur-target)),
          j-expr(j-assign(ans, app(self.get-loc(l), compiled-f, compiled-args))),
          j-break]),
      concat-empty)
  end,
  a-split-app(self, l :: Loc, is-var :: Boolean, f :: N.AVal, args :: List<N.AVal>, name :: A.Name, helper-args :: List<N.AVal>):
    ans = self.cur-ans
    step = self.cur-step
    compiled-f = f.visit(self).exp
    compiled-args = args.map(lam(a): a.visit(self).exp end)
    var new-cases = concat-empty
    helper = self.helpers.get(name.key())
    visited-helper = helper.body.visit(self)
    helper-label =
      if (visited-helper.block.stmts.length() == 3):
        stmts = visited-helper.block.stmts
        e1 = stmts.first
        e2 = stmts.rest.first
        e3 = stmts.rest.rest.first
        if J.is-j-expr(e1) and J.is-j-assign(e1.expr)
          and (e1.expr.name == step)
          and J.is-j-expr(e2) and J.is-j-assign(e2.expr)
          and (e2.expr.name == ans) and J.is-j-id(e2.expr.rhs)
          and (e2.expr.rhs.id == js-id-of(helper.args.first.tostring()))
          and J.is-j-break(e3):
          self.cur-target
        else:
          lbl = self.make-label()
          new-cases := concat-cons(
            j-case(lbl, j-block([list:
                  j-var(helper.args.first.tostring()^js-id-of, j-id(ans)),
                  visited-helper.block])),
            visited-helper.new-cases)
          lbl
        end
      else:
        lbl = self.make-label()
        new-cases := concat-cons(
          j-case(lbl, j-block([list:
                j-var(helper.args.first.tostring()^js-id-of, j-id(ans)),
                visited-helper.block])),
          visited-helper.new-cases)
        lbl
      end
    c-block(
      j-block([list:
          check-fun(self.get-loc(l), compiled-f),
          # Update step before the call, so that if it runs out of gas, the resumer goes to the right step
          j-expr(j-assign(step,  helper-label)),
          j-expr(j-assign(ans, app(self.get-loc(l), compiled-f, compiled-args))),
          j-break]),
      new-cases)
  end,
  a-seq(self, l, e1, e2):
    e1-visit = e1.visit(self).exp
    e2-visit = e2.visit(self)
    if J.JStmt(e1-visit):
      c-block(
        j-block(link(e1-visit, e2-visit.block.stmts)),
        e2-visit.new-cases)
    else:
      c-block(
        j-block(link(j-expr(e1-visit), e2-visit.block.stmts)),
        e2-visit.new-cases)
    end
  end,
  a-if(self, l :: Loc, cond :: N.AVal, consq :: N.AExpr, alt :: N.AExpr):
    compiled-consq = consq.visit(self)
    compiled-alt = alt.visit(self)

    consq-label = self.make-label()
    alt-label = self.make-label()
    new-cases =
      concat-cons(j-case(consq-label, compiled-consq.block), compiled-consq.new-cases)
      + concat-cons(j-case(alt-label, compiled-alt.block), compiled-alt.new-cases)
    c-block(
      j-block([list: 
          j-if(rt-method("isPyretTrue", [list: cond.visit(self).exp]),
            j-block(goto-case(self.cur-step, consq-label)), j-block(goto-case(self.cur-step, alt-label)))
        ]),
      new-cases)
  end,
  a-lettable(self, e :: N.ALettable): # Need to add back the location field
    visit-e = e.visit(self)
    c-block(
      j-block(
        j-expr(j-assign(self.cur-step, self.cur-target))
        ^ link(_, visit-e.other-stmts
            + [list:
            j-expr(j-assign(self.cur-ans, visit-e.exp)),
            j-break])),
      concat-empty)
  end,
  a-assign(self, l :: Loc, id :: A.Name, value :: N.AVal):
    visit-value = value.visit(self)
    c-exp(j-dot-assign(j-id(js-id-of(id.tostring())), "$var", visit-value.exp), visit-value.other-stmts)
  end,
  a-app(self, l :: Loc, f :: N.AVal, args :: List<N.AVal>):
    visit-f = f.visit(self)
    visit-args = args.map(_.visit(self))
    other-stmts = visit-args.foldr(lam(va, acc): va.other-stmts + acc end, visit-f.other-stmts)
    c-exp(app(self.get-loc(l), visit-f.exp, visit-args.map(_.exp)), other-stmts)
  end,
  a-prim-app(self, l :: Loc, f :: String, args :: List<N.AVal>):
    visit-args = args.map(_.visit(self))
    other-stmts = visit-args.foldr(lam(va, acc): va.other-stmts + acc end, empty)
    c-exp(rt-method(f, visit-args.map(_.exp)), other-stmts)
  end,
  
  a-obj(self, l :: Loc, fields :: List<N.AField>):
    visit-fields = fields.map(lam(f): f.visit(self) end)
    other-stmts = visit-fields.foldr(lam(vf, acc): vf.other-stmts + acc end, empty)
    c-exp(rt-method("makeObject", [list: j-obj(visit-fields.map(_.field))]), other-stmts)
  end,
  a-extend(self, l :: Loc, obj :: N.AVal, fields :: List<N.AField>):
    visit-obj = obj.visit(self)
    visit-fields = fields.map(lam(f): f.visit(self) end)
    other-stmts = visit-fields.foldr(lam(vf, acc): vf.other-stmts + acc end, visit-obj.other-stmts)
    c-exp(j-method(visit-obj.exp, "extendWith", [list: j-obj(visit-fields.map(_.field))]),
      other-stmts)
  end,
  a-dot(self, l :: Loc, obj :: N.AVal, field :: String):
    visit-obj = obj.visit(self)
    c-exp(get-field(visit-obj.exp, j-str(field), self.get-loc(l)), visit-obj.other-stmts)
  end,
  a-colon(self, l :: Loc, obj :: N.AVal, field :: String):
    visit-obj = obj.visit(self)
    c-exp(rt-method("getColonField", [list: visit-obj.exp, j-str(field)]), visit-obj.other-stmts)
  end,
  a-lam(self, l :: Loc, args :: List<N.ABind>, ret :: A.Ann, body :: N.AExpr):
    new-step = js-id-of(compiler-name("step"))
    temp = js-id-of(compiler-name("temp_lam"))
    # NOTE: args may be empty, so we need at least one name ("resumer") for the stack convention
    effective-args =
      if args.length() > 0: args
      else: [list: N.a-bind(l, A.s-name(l, compiler-name("resumer")), A.a-blank)]
      end
    c-exp(
      j-id(temp),
      [list:
        j-var(temp,
          rt-method("makeFunction", [list: j-fun(effective-args.map(_.id).map(_.tostring()).map(js-id-of),
                compile-fun-body(l, new-step, temp, self, effective-args, args.length(), body))]))])
  end,
  a-method(self, l :: Loc, args :: List<N.ABind>, ret :: A.Ann, body :: N.AExpr):
    # step-method = js-id-of(compiler-name("step"))
    # temp-method = compiler-name("temp_method")
    # compiled-body-method = compile-fun-body(l, step-method, temp-method, self, args, args.length() - 1, body)
    # method-var = j-var(temp-method,
    #   j-fun(args.map(lam(a): js-id-of(a.id.tostring()) end), compiled-body-method))
    step-curry = js-id-of(compiler-name("step"))
    temp-curry = js-id-of(compiler-name("temp_curry"))
    # NOTE: excluding self, args may be empty, so we need at least one name ("resumer") for the stack convention
    effective-curry-args =
      if args.length() > 1: args.rest
      else: [list: N.a-bind(l, A.s-name(l, compiler-name("resumer")), A.a-blank)]
      end
    compiled-body-curry =
      compile-fun-body(l, step-curry, temp-curry, self, effective-curry-args, args.length() - 1, body)
    curry-var = j-var(temp-curry,
      j-fun(effective-curry-args.map(lam(a): js-id-of(a.id.tostring()) end), compiled-body-curry))
    #### TODO!
    c-exp(
      rt-method("makeMethod", [list: j-fun([list: js-id-of(args.first.id.tostring())],
            j-block([list: curry-var, j-return(j-id(temp-curry))])),
          j-obj([list: j-field("length", j-num(args.length()))])]),
      empty)
  end,
  a-val(self, v :: N.AVal):
    v.visit(self)
  end,
  a-field(self, l :: Loc, name :: String, value :: N.AVal):
    visit-v = value.visit(self)
    c-field(j-field(name, visit-v.exp), visit-v.other-stmts)
  end,
  a-array(self, l, values):
    visit-vals = values.map(_.visit(self))
    other-stmts = visit-vals.foldr(lam(v, acc): v.other-stmts + acc end, empty)
    c-exp(j-list(false, visit-vals.map(_.exp)), other-stmts)
  end,
  a-srcloc(self, l, loc):
    c-exp(self.get-loc(loc), empty)
  end,
  a-num(self, l :: Loc, n :: Number):
    if num-is-fixnum(n):
      c-exp(j-parens(j-num(n)), empty)
    else:
      c-exp(rt-method("makeNumberFromString", [list: j-str(tostring(n))]), empty)
    end
  end,
  a-str(self, l :: Loc, s :: String):
    c-exp(j-parens(j-str(s)), empty)
  end,
  a-bool(self, l :: Loc, b :: Boolean):
    c-exp(j-parens(if b: j-true else: j-false end), empty)
  end,
  a-undefined(self, l :: Loc):
    c-exp(undefined, empty)
  end,
  a-id(self, l :: Loc, id :: A.Name):
    c-exp(j-id(js-id-of(id.tostring())), empty)
  end,
  a-id-var(self, l :: Loc, id :: A.Name):
    c-exp(j-dot(j-id(js-id-of(id.tostring())), "$var"), empty)
  end,
  a-id-letrec(self, l :: Loc, id :: A.Name, safe :: Boolean):
    s = id.tostring()
    if safe:
      c-exp(j-dot(j-id(js-id-of(s)), "$var"), empty)
    else:
      c-exp(
        j-ternary(
          j-binop(j-dot(j-id(js-id-of(s)), "$var"), j-eq, undefined),
          raise-id-exn(self.get-loc(l), id.toname()),
          j-dot(j-id(js-id-of(s)), "$var")),
        empty)
    end
  end,

  a-data-expr(self, l, name, namet, variants, shared):
    fun brand-name(base):
      compiler-name("brand-" + base)
    end

    visit-shared-fields = shared.map(_.visit(self))
    shared-fields = visit-shared-fields.map(_.field)
    shared-stmts = visit-shared-fields.foldr(lam(vf, acc): vf.other-stmts + acc end, empty)
    external-brand = j-id(js-id-of(namet.tostring()))

    fun make-brand-predicate(loc :: Loc, b :: J.JExpr, pred-name :: String):
      j-field(
        pred-name,
        rt-method("makeFunction", [list: 
            j-fun(
              [list: "val"],
              j-block([list:
                  arity-check(self.get-loc(loc), 1),
                  j-return(rt-method("makeBoolean", [list: rt-method("hasBrand", [list: j-id("val"), b])]))
                ])
              )
          ])
        )
    end

    fun make-variant-constructor(l2, base-id, brands-id, vname, members):
      member-names = members.map(lam(m): m.bind.id.toname();)
      member-ids = members.map(lam(m): m.bind.id.tostring();)

      constr-body = [list:
        j-var("dict", rt-method("create", [list: j-id(base-id)]))
      ] +
      for map3(n from member-names, m from members, id from member-ids):
        cases(N.AMemberType) m.member-type:
          | a-normal => j-expr(j-bracket-assign(j-id("dict"), j-str(n), j-id(js-id-of(id))))
          | a-cyclic => raise("Cannot handle cyclic fields yet")
          | a-mutable => raise("Cannot handle mutable fields yet")
        end
      end +
      [list: 
        j-return(rt-method("makeBrandedObject", [list: j-id("dict"), j-id(brands-id)]))
      ]

      nonblank-anns = for filter(m from members):
        not(A.is-a-blank(m.bind.ann)) and not(A.is-a-any(m.bind.ann))
      end
      compiled-anns = for fold(acc from {anns: empty, others: empty}, m from nonblank-anns):
        compiled = compile-ann(m.bind.ann, self)
        {
          anns: compiled.exp ^ link(_, acc.anns),
          others: compiled.other-stmts.reverse() + acc.others
        }
      end
      compiled-locs = for map(m from nonblank-anns): self.get-loc(m.bind.ann.l) end
      compiled-vals = for map(m from nonblank-anns): j-id(js-id-of(m.bind.id.tostring())) end
      
      # NOTE(joe 6-14-2014): We cannot currently statically check for if an annotation
      # is a refinement because of type aliases.  So, we use checkAnnArgs, which takes
      # a continuation and manages all of the stack safety of annotation checking itself.
      c-exp(
        rt-method("makeFunction", [list:
            j-fun(
              member-ids.map(js-id-of),
              j-block(
                [list:
                  arity-check(self.get-loc(l2), member-names.length())
                ] +
                compiled-anns.others.reverse() +
                [list:
                  j-return(rt-method("checkAnnArgs", [list:
                        j-list(false, compiled-anns.anns.reverse()),
                        j-list(false, compiled-vals),
                        j-list(false, compiled-locs),
                        j-fun(empty, j-block(constr-body))
                      ]))
              ]))]),
        empty)
    end

    fun compile-variant(v :: N.AVariant):
      vname = v.name
      variant-base-id = js-id-of(compiler-name(vname + "-base"))
      variant-brand = brand-name(vname)
      variant-brand-obj-id = js-id-of(compiler-name(vname + "-brands"))
      variant-brands = j-obj([list: 
          j-field(variant-brand, j-true)
        ])
      visit-with-fields = v.with-members.map(_.visit(self))
      
      stmts =
        visit-with-fields.foldr(lam(vf, acc): vf.other-stmts + acc end,
          [list: 
            j-var(variant-base-id, j-obj(shared-fields + visit-with-fields.map(_.field))),
            j-var(variant-brand-obj-id, variant-brands),
            j-expr(j-bracket-assign(
              j-id(variant-brand-obj-id),
              j-dot(external-brand, "_brand"),
              j-true))
        ])
      predicate = make-brand-predicate(v.l, j-str(variant-brand), A.make-checker-name(vname))

      cases(N.AVariant) v:
        | a-variant(l2, constr-loc, _, members, with-members) =>
          constr-vname = js-id-of(vname)
          compiled-constr =
            make-variant-constructor(constr-loc, variant-base-id, variant-brand-obj-id, constr-vname, members)
          {
            stmts: stmts + compiled-constr.other-stmts + [list: j-var(constr-vname, compiled-constr.exp)],
            constructor: j-field(vname, j-id(constr-vname)),
            predicate: predicate
          }
        | a-singleton-variant(_, _, with-members) =>
          {
            stmts: stmts,
            constructor: j-field(vname, rt-method("makeBrandedObject", [list: j-id(variant-base-id), j-id(variant-brand-obj-id)])),
            predicate: predicate
          }
      end
    end

    variant-pieces = variants.map(compile-variant)

    header-stmts = for fold(acc from [list: ], piece from variant-pieces):
      piece.stmts.reverse() + acc
    end.reverse()
    obj-fields = for fold(acc from [list: ], piece from variant-pieces):
      [list: piece.constructor] + [list: piece.predicate] + acc
    end.reverse()

    data-predicate = make-brand-predicate(l, j-dot(external-brand, "_brand"), name)

    data-object = rt-method("makeObject", [list: j-obj([list: data-predicate] + obj-fields)])

    c-exp(thunk-app(j-block(shared-stmts + header-stmts + [list: j-return(data-object)])), empty)
  end
}

remove-useless-if-visitor = N.default-map-visitor.{
  a-if(self, l, c, t, e):
    cases(N.AVal) c:
      | a-bool(_, test) =>
        if test: t.visit(self) else: e.visit(self) end
      | else => N.a-if(l, c.visit(self), t.visit(self), e.visit(self))
    end
  end
}

check:
  d = N.dummy-loc
  true1 = N.a-if(d, N.a-bool(d, true),
    N.a-lettable(N.a-val(N.a-num(d, 1))),
    N.a-lettable(N.a-val(N.a-num(d, 2))))
  true1.visit(remove-useless-if-visitor) is N.a-lettable(N.a-val(N.a-num(d, 1)))

  false4 = N.a-if(d, N.a-bool(d, false),
    N.a-lettable(N.a-val(N.a-num(d, 3))),
    N.a-lettable(N.a-val(N.a-num(d, 4))))
  false4.visit(remove-useless-if-visitor) is N.a-lettable(N.a-val(N.a-num(d, 4)))

  N.a-if(d, N.a-id(d, A.s-name(d, "x")), true1, false4).visit(remove-useless-if-visitor)
    is N.a-if(d, N.a-id(d, A.s-name(d, "x")),
    N.a-lettable(N.a-val(N.a-num(d, 1))),
    N.a-lettable(N.a-val(N.a-num(d, 4))))
  
end

fun mk-abbrevs(l):
  [list: 
    j-var("G", rt-field("getFieldLoc")),
    j-var("U", j-fun([list: "loc", "name"],
        j-block([list: j-method(rt-field("ffi"), "throwUninitializedIdMkLoc",
                          [list: j-id("loc"), j-id("name")])]))),
    j-var("M", j-str(l.source)),
    j-var("D", rt-field("undefined"))
  ]
end


fun compile-program(self, l, imports, split, env):
  fun inst(id): j-app(j-id(id), [list: j-id("R"), j-id("NAMESPACE")]);
  free-ids = S.freevars-split-result(split).difference(sets.list-to-tree-set(imports.map(_.name))).difference(sets.list-to-tree-set(imports.map(_.types)))
  namespace-binds = for map(n from free-ids.to-list()):
    bind-name = cases(A.Name) n:
      | s-global(s) => n.toname()
      | s-type-global(s) => type-name(n.toname())
    end
    j-var(js-id-of(n.tostring()), j-method(j-id("NAMESPACE"), "get", [list: j-str(bind-name)]))
  end
  ids = imports.map(_.name).map(_.tostring()).map(js-id-of)
  type-imports = imports.filter(N.is-a-import-types)
  type-ids = type-imports.map(_.types).map(_.tostring()).map(js-id-of)
  filenames = imports.map(lam(i):
      cases(N.AImportType) i.import-type:
        | a-import-builtin(_, name) => "trove/" + name
        | a-import-file(_, file) => file
      end
    end)
  module-id = compiler-name(l.source)
  module-ref = lam(name): j-bracket(rt-field("modules"), j-str(name));
  input-ids = ids.map(lam(f): compiler-name(f) end)
  fun wrap-modules(modules, body-name, body-fun):
    mod-input-names = modules.map(_.input-id)
    mod-input-ids = mod-input-names.map(j-id)
    mod-val-ids = modules.map(_.id)
    j-return(rt-method("loadModulesNew",
        [list: j-id("NAMESPACE"), j-list(false, mod-input-ids),
          j-fun(mod-input-names,
            j-block(
              for map2(m from mod-val-ids, in from mod-input-ids):
                j-var(m, rt-method("getField", [list: in, j-str("values")]))
              end +
              for map2(mt from type-ids, in from mod-input-ids):
                j-var(mt, rt-method("getField", [list: in, j-str("types")]))
              end +
              [list: 
                j-var(body-name, body-fun),
                j-return(rt-method(
                    "safeCall", [list: 
                      j-id(body-name),
                      j-fun([list: "moduleVal"],
                        j-block([list: 
                            j-expr(j-bracket-assign(rt-field("modules"), j-str(module-id), j-id("moduleVal"))),
                            j-return(j-id("moduleVal"))
                    ]))]))]))]))
  end
  module-specs = for map2(id from ids, in-id from input-ids):
    { id: id, input-id: in-id }
  end
  var locations = concat-empty
  var loc-count = 0
  var loc-cache = D.string-dict()
  locs = "L"
  fun get-loc(shadow l :: Loc):
    as-str = torepr(l)
    if loc-cache.has-key(as-str):
      loc-cache.get(as-str)
    else:
      ans = j-bracket(j-id(locs), j-num(loc-count))
      loc-cache.set(as-str, ans)
      loc-count := loc-count + 1
      locations := concat-snoc(locations, obj-of-loc(l))
      ans
    end
  end

  step = js-id-of(compiler-name("step"))
  toplevel-name = js-id-of(compiler-name("toplevel"))
  resumer = N.a-bind(l, A.s-name(l, compiler-name("resumer")), A.a-blank)
  visited-body = compile-fun-body(l, step, toplevel-name, self.{get-loc: get-loc}, [list: resumer], 0, split.body)
  toplevel-fun = j-fun([list: js-id-of(tostring(resumer.id))], visited-body)
  define-locations = j-var(locs, j-list(true, locations.to-list()))
  j-app(j-id("define"), [list: j-list(true, filenames.map(j-str)), j-fun(input-ids, j-block([list: 
            j-return(j-fun([list: "R", "NAMESPACE"],
                j-block([list: 
                    j-if(module-ref(module-id),
                      j-block([list: j-return(module-ref(module-id))]),
                      j-block(mk-abbrevs(l) +
                        [list: define-locations] + 
                        namespace-binds +
                        [list: wrap-modules(module-specs, toplevel-name, toplevel-fun)]))])))]))])
end

fun splitting-compiler(env):
  compiler-visitor.{
    a-program(self, l, imports, body):
      simplified = body.visit(remove-useless-if-visitor)
      split = S.ast-split(simplified)
      helpers-dict = D.string-dict()
      for each(h from split.helpers):
        helpers-dict.set(h.name.key(), h)
      end
      compile-program(self.{helpers: helpers-dict}, l, imports, split, env)
    end
  }
end

fun non-splitting-compiler(env):
  compiler-visitor.{
    a-program(self, l, imports, body):
      simplified = body.visit(remove-useless-if-visitor)
      split = S.split-result([list: ], simplified, N.freevars-e(simplified))
      compile-program(self, l, imports, split, env)
    end
  }
end

