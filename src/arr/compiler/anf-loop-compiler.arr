#lang pyret

provide *
import ast as A
import "compiler/ast-anf.arr" as N
import "compiler/js-ast.arr" as J
import "compiler/gensym.arr" as G
import "compiler/compile-structs.arr" as CS
import "compiler/concat-lists.arr" as CL
import "compiler/js-dag-utils.arr" as DAG
import string-dict as D
import srcloc as SL
import sets as S

type Loc = SL.Srcloc
type ConcatList = CL.ConcatList

fun get-exp(o): o.exp end
fun get-id(o): o.id end
fun get-name(o): o.name end
fun get-l(o): o.l end
fun get-bind(o): o.bind end
fun o-get-field(o): o.field end

concat-empty = CL.concat-empty
concat-singleton = CL.concat-singleton
concat-append = CL.concat-append
concat-cons = CL.concat-cons
concat-snoc = CL.concat-snoc

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
j-new = J.j-new
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
j-and = J.j-and
j-lt = J.j-lt
j-eq = J.j-eq
j-neq = J.j-neq
j-geq = J.j-geq
j-unop = J.j-unop
j-decr = J.j-decr
j-incr = J.j-incr
j-not = J.j-not
j-instanceof = J.j-instanceof
j-ternary = J.j-ternary
j-null = J.j-null
j-parens = J.j-parens
j-switch = J.j-switch
j-case = J.j-case
j-default = J.j-default
j-label = J.j-label
j-break = J.j-break
j-while = J.j-while
j-for = J.j-for
make-label-sequence = J.make-label-sequence



js-names = A.MakeName(0)
js-ids = D.make-mutable-string-dict()
fun fresh-id(id :: A.Name) -> A.Name:
  base-name = if A.is-s-type-global(id): id.tosourcestring() else: id.toname() end
  no-hyphens = string-replace(base-name, "-", "$")
  js-names.make-atom(no-hyphens)
end
fun js-id-of(id :: A.Name) -> A.Name:
  s = id.tosourcestring()
  if js-ids.has-key-now(s):
    js-ids.get-value-now(s)
  else:
    safe-id = fresh-id(id)
    js-ids.set-now(s, safe-id)
    safe-id
  end
end

fun const-id(name :: String):
  A.s-name(A.dummy-loc, name)
end

fun compiler-name(id):
  const-id("$" + id)
end

fun formal-shadow-name(id :: A.Name) -> A.Name:
  js-id = js-id-of(id)
  A.s-name(A.dummy-loc, "$" + js-id.tosourcestring())
end

get-field-loc = j-id(const-id("G"))
throw-uninitialized = j-id(const-id("U"))
source-name = j-id(const-id("M"))
undefined = j-id(const-id("D"))
RUNTIME = j-id(const-id("R"))
NAMESPACE = j-id(const-id("NAMESPACE"))
THIS = j-id(const-id("this"))
ARGUMENTS = j-id(const-id("arguments"))

j-bool = lam(b):
  if b: j-true else: j-false end
end

fun obj-of-loc(l):
  cases(Loc) l:
    | builtin(name) => j-list(false, [list: j-str(name)])
    | srcloc(_, start-line, start-col, start-char, end-line, end-col, end-char) =>
      j-list(false, [list: 
          j-id(const-id("M")),
          j-num(start-line),
          j-num(start-col),
          j-num(start-char),
          j-num(end-line),
          j-num(end-col),
          j-num(end-char)
        ])
  end
end

fun get-dict-field(obj, field):
  j-bracket(j-dot(obj, "dict"), field)
end

fun get-field(obj :: J.JExpr, field :: J.JExpr, loc :: J.JExpr):
  j-app(get-field-loc, [list: obj, field, loc])
end

fun get-field-ref(obj :: J.JExpr, field :: J.JExpr, loc :: J.JExpr):
  rt-method("getFieldRef", [list: obj, field, loc])
end

fun raise-id-exn(loc, name):
  j-app(throw-uninitialized, [list: loc, j-str(name)])
end

fun add-stack-frame(exn-id, loc):
  j-method(j-dot(j-id(exn-id), "pyretStack"), "push", [list: loc])
end

fun rt-field(name): j-dot(RUNTIME, name);
fun rt-method(name, args): j-method(RUNTIME, name, args);

fun app(l, f, args):
  j-method(f, "app", args)
end

fun check-fun(l, f):
  j-if1(j-unop(j-parens(rt-method("isFunction", [list: f])), j-not),
    j-block([list: j-expr(j-method(rt-field("ffi"), "throwNonFunApp", [list: l, f]))]))
end

fun thunk-app(block):
  j-app(j-parens(j-fun(empty, block)), empty)
end

fun thunk-app-stmt(stmt):
  thunk-app(j-block([list: stmt]))
end

c-exp = DAG.c-exp
c-field = DAG.c-field
c-block = DAG.c-block
is-c-exp = DAG.is-c-exp
is-c-field = DAG.is-c-field
is-c-block = DAG.is-c-block


fun compile-ann(ann :: A.Ann, visitor) -> DAG.CaseResults%(is-c-exp):
  cases(A.Ann) ann:
    | a-name(_, n) => c-exp(j-id(js-id-of(n)), empty)
    | a-type-var(_, _) => c-exp(rt-field("Any"), empty)
    | a-arrow(_, _, _, _) => c-exp(rt-field("Function"), empty)
    | a-method(_, _, _) => c-exp(rt-field("Method"), empty)
    | a-app(l, base, _) => compile-ann(base, visitor)
    | a-record(l, fields) =>
      names = j-list(false, fields.map(get-name).map(j-str))
      locs = j-list(false, fields.map(get-l).map(visitor.get-loc))
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
      name = cases(A.Expr) exp:
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
            j-id(js-id-of(m)),
            j-str(field)]),
        empty)
    | a-blank => c-exp(rt-field("Any"), empty)
    | a-any => c-exp(rt-field("Any"), empty)
  end
end

fun arity-check(loc-expr, arity :: Number):
  #|[list:
    j-if1(j-binop(j-dot(ARGUMENTS, "length"), j-neq, j-num(arity)),
      j-block([list:
          j-expr(rt-method("checkArityC", [list: loc-expr, j-num(arity), j-method(rt-field("cloneArgs"), "apply", [list: j-null, ARGUMENTS])]))
      ]))]|#
  len = j-id(compiler-name("l"))
  iter = j-id(compiler-name("i"))
  t = j-id(compiler-name("t"))
  [list:
    j-var(len.id, j-dot(ARGUMENTS, "length")),
    j-if1(j-binop(len, j-neq, j-num(arity)),
      j-block([list:
          j-var(t.id, j-new(j-id(const-id("Array")), [list: len])),
          j-for(true, j-assign(iter.id, j-num(0)), j-binop(iter, j-lt, len), j-unop(iter, j-incr),
            j-block([list: j-expr(j-bracket-assign(t, iter, j-bracket(ARGUMENTS, iter)))])),
          j-expr(rt-method("checkArityC", [list: loc-expr, j-num(arity), t]))]))]
end

no-vars = D.make-string-dict()

local-bound-vars-visitor = {
  j-field(self, name, value): value.visit(self) end,
  j-parens(self, exp): exp.visit(self) end,
  j-unop(self, exp, op): exp.visit(self) end,
  j-binop(self, left, op, right): left.visit(self).merge(right.visit(self)) end,
  j-fun(self, args, body): no-vars end,
  j-new(self, func, args): args.foldl(lam(arg, base): base.merge(arg.visit(self)) end, func.visit(self)) end,
  j-app(self, func, args): args.foldl(lam(arg, base): base.merge(arg.visit(self)) end, func.visit(self)) end,
  j-method(self, obj, meth, args): no-vars end,
  j-ternary(self, test, consq, alt): test.visit(self).merge(consq.visit(self)).merge(alt.visit(self)) end,
  j-assign(self, name, rhs): rhs.visit(self) end,
  j-bracket-assign(self, obj, field, rhs): obj.visit(self).merge(field.visit(self)).merge(rhs.visit(self)) end,
  j-dot-assign(self, obj, name, rhs): obj.visit(self).merge(rhs.visit(self)) end,
  j-dot(self, obj, name): obj.visit(self) end,
  j-bracket(self, obj, field): obj.visit(self).merge(field.visit(self)) end,
  j-list(self, multi-line, elts):
    elts.foldl(lam(arg, base): base.merge(arg.visit(self)) end, no-vars)
  end,
  j-obj(self, fields): fields.foldl(lam(f, base): base.merge(f.visit(self)) end, no-vars) end,
  j-id(self, id): no-vars end,
  j-str(self, s): no-vars end,
  j-num(self, n): no-vars end,
  j-true(self): no-vars end,
  j-false(self): no-vars end,
  j-null(self): no-vars end,
  j-undefined(self): no-vars end,
  j-label(self, label): no-vars end,
  j-case(self, exp, body): exp.visit(self).merge(body.visit(self)) end,
  j-default(self, body): body.visit(self) end,
  j-block(self, stmts): stmts.foldl(lam(s, base): base.merge(s.visit(self)) end, no-vars) end,
  j-var(self, name, rhs):
    # Ignore all variables named $underscore#####
    if A.is-s-atom(name) and (name.base == "$underscore"): rhs.visit(self)
    else: rhs.visit(self).set(name.key(), name)
    end
  end,
  j-if1(self, cond, consq): cond.visit(self).merge(consq.visit(self)) end,
  j-if(self, cond, consq, alt): cond.visit(self).merge(consq.visit(self)).merge(alt.visit(self)) end,
  j-return(self, exp): exp.visit(self) end,
  j-try-catch(self, body, exn, catch): body.visit(self).merge(catch.visit(self)) end,
  j-throw(self, exp): exp.visit(self) end,
  j-expr(self, exp): exp.visit(self) end,
  j-break(self): no-vars end,
  j-continue(self): no-vars end,
  j-switch(self, exp, branches):
    branches.foldl(lam(b, base): base.merge(b.visit(self)) end, exp.visit(self))
  end,
  j-while(self, cond, body): cond.visit(self).merge(body.visit(self)) end,
  j-for(self, create-var, init, cond, update, body):
    init.visit(self).merge(cond.visit(self)).merge(update.visit(self)).merge(body.visit(self))
  end
}


show-stack-trace = false
fun compile-fun-body(l :: Loc, step :: A.Name, fun-name :: A.Name, compiler, args :: List<N.ABind>, opt-arity :: Option<Number>, body :: N.AExpr, should-report-error-frame :: Boolean) -> J.JBlock:
  make-label = make-label-sequence(0)
  ret-label = make-label()
  ans = fresh-id(compiler-name("ans"))
  apploc = fresh-id(compiler-name("al"))
  local-compiler = compiler.{make-label: make-label, cur-target: ret-label, cur-step: step, cur-ans: ans, cur-apploc: apploc}
  visited-body = body.visit(local-compiler)
  # To avoid penalty for assigning to formal parameters and also using the arguments object,
  # we create a shadow set of formal arguments, and immediately assign them to the "real" ones
  # in the normal entry case.  This expands the function preamble, but might enable JS optimizations,
  # so it should be worth it
  formal-args = for map(arg from args):
    N.a-bind(arg.l, formal-shadow-name(arg.id), arg.ann)
  end
  copy-formals-to-args = for map2(formal-arg from formal-args, arg from args):
    j-var(js-id-of(arg.id), j-id(formal-arg.id))
  end
  ann-cases = compile-anns(local-compiler, step, args, local-compiler.make-label())
  main-body-cases =
    concat-empty
  ^ concat-append(_, ann-cases.new-cases)
  ^ concat-snoc(_, j-case(ann-cases.new-label, visited-body.block))
  ^ concat-append(_, visited-body.new-cases)
  # Initialize the case numbers, for more legible output...
  main-body-cases.each(lam(c): when J.is-j-case(c): c.exp.label.get() end end)
  main-body-cases-and-dead-vars = DAG.simplify(main-body-cases, step)
  shadow main-body-cases = main-body-cases-and-dead-vars.body
  all-vars = for CL.foldl(base from no-vars, case-expr from main-body-cases):
      base.merge(case-expr.visit(local-bound-vars-visitor))
    end
  all-needed-vars = for fold(acc from all-vars, d from main-body-cases-and-dead-vars.discardable-vars.keys-list()):
    acc.remove(d)
  end
  vars = all-needed-vars.keys-list().map(all-needed-vars.get-value(_))
  switch-cases =
    main-body-cases
  ^ concat-snoc(_, j-case(local-compiler.cur-target, j-block(
        if show-stack-trace:
          [list: j-expr(rt-method("traceExit", [list: j-str(tostring(l)), j-num(vars.length())]))]
        else:
          empty
        end + [list:
          j-expr(j-unop(rt-field("GAS"), j-incr)),
          j-return(j-id(local-compiler.cur-ans))])))
  ^ concat-snoc(_, j-default(j-block([list:
          j-throw(j-binop(j-binop(j-str("No case numbered "), J.j-plus, j-id(step)), J.j-plus,
              j-str(" in " + fun-name.tosourcestring())))])))
  # fun check-no-dups(seen, kases):
  #   cases(List) kases:
  #     | empty => nothing
  #     | link(hd, tl) =>
  #       lbl = if J.is-j-case(hd): hd.exp.label.get() else: -1 end
  #       when seen.member(lbl):
  #         raise("Duplicate case found: " + hd.to-ugly-source())
  #       end
  #       check-no-dups(seen.add(lbl), tl)
  #   end
  # end        
  # check-no-dups(sets.empty-tree-set, switch-cases.to-list())
  act-record = rt-method("makeActivationRecord", [list:
      j-id(apploc),
      j-id(fun-name),
      j-id(step),
      j-list(false, args.map(lam(a): j-id(js-id-of(a.id)) end)),
      j-list(false, vars.map(lam(v): j-id(v) end))
    ])  
  e = fresh-id(compiler-name("e"))
  first-arg = formal-args.first.id
  entryExit = [list:
    j-str(tostring(l)),
    j-num(vars.length())
  ]
  preamble = block:
    restorer =
      j-block(
        [list:
          j-expr(j-assign(step, j-dot(j-id(first-arg), "step"))),
          j-expr(j-assign(apploc, j-dot(j-id(first-arg), "from"))),
          j-expr(j-assign(local-compiler.cur-ans, j-dot(j-id(first-arg), "ans")))
        ] +
        for map_n(i from 0, arg from args):
          j-expr(j-assign(js-id-of(arg.id), j-bracket(j-dot(j-id(first-arg), "args"), j-num(i))))
        end +
        for map_n(i from 0, v from vars):
          j-expr(j-assign(v, j-bracket(j-dot(j-id(first-arg), "vars"), j-num(i))))
        end)
    cases(Option) opt-arity:
      | some(arity) =>
        j-if(rt-method("isActivationRecord", [list: j-id(first-arg)]),
          restorer,
          j-block(
            arity-check(local-compiler.get-loc(l), arity) +
            copy-formals-to-args +
            if show-stack-trace:
              [list: rt-method("traceEnter", entryExit)]
            else:
              empty
            end))
      | none =>
        if show-stack-trace:
          j-if(rt-method("isActivationRecord", [list: j-id(first-arg)]),
            restorer,
            j-block([list: rt-method("traceEnter", entryExit)] + copy-formals-to-args))
        else:
          j-if(rt-method("isActivationRecord", [list: j-id(first-arg)]),
            restorer, j-block(copy-formals-to-args))
        end
    end
  end
  stack-attach-guard =
    if compiler.options.proper-tail-calls:
      j-binop(rt-method("isCont", [list: j-id(e)]),
        j-and,
        j-parens(j-binop(j-id(step), j-neq, ret-label)))
    else:
      rt-method("isCont", [list: j-id(e)])
    end

    
  j-block([list:
      j-var(step, j-num(0)),
      j-var(local-compiler.cur-ans, undefined),
      j-var(apploc, local-compiler.get-loc(l)),
      j-try-catch(
        j-block([list:
            preamble,
            j-if1(j-binop(j-unop(rt-field("GAS"), j-decr), J.j-leq, j-num(0)),
              j-block([list: j-expr(j-dot-assign(RUNTIME, "EXN_STACKHEIGHT", j-num(0))),
                  # j-expr(j-app(j-id("console.log"), [list: j-str("Out of gas in " + fun-name)])),
                  # j-expr(j-app(j-id("console.log"), [list: j-str("GAS is "), rt-field("GAS")])),
                  j-throw(rt-method("makeCont", empty))])),
            j-while(j-true,
              j-block([list:
                  # j-expr(j-app(j-id("console.log"), [list: j-str("In " + fun-name + ", step "), j-id(step), j-str(", GAS = "), rt-field("GAS"), j-str(", ans = "), j-id(local-compiler.cur-ans)])),
                  j-switch(j-id(step), switch-cases.to-list())]))]),
        e,
        j-block(
          [list:
            j-if1(stack-attach-guard,
              j-block([list: 
                  j-expr(j-bracket-assign(j-dot(j-id(e), "stack"),
                      j-unop(rt-field("EXN_STACKHEIGHT"), J.j-postincr), act-record))
              ]))] +
          if should-report-error-frame:
            [list:
              j-if1(rt-method("isPyretException", [list: j-id(e)]),
                j-block(
                  [list: j-expr(add-stack-frame(e, j-id(apploc)))] +
                  if show-stack-trace:
                    [list: j-expr(rt-method("traceErrExit", entryExit))]
                  else:
                    empty
                  end
                  ))]
          else if show-stack-trace:
            [list:
              j-if1(rt-method("isPyretException", [list: j-id(e)]),
                j-block([list: 
                    j-expr(add-stack-frame(e, j-id(apploc)))
                ]))]
          else:
            empty
          end +
          [list: j-throw(j-id(e))]))
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
              [list: visitor.get-loc(b.ann.l), compiled-ann.exp, j-id(js-id-of(b.id))])),
            j-break]))
      cur-target := new-label
      concat-snoc(acc, new-case)
    end
  end
  { new-cases: new-cases, new-label: cur-target }
end

fun compile-annotated-let(visitor, b :: N.ABind, compiled-e :: DAG.CaseResults%(is-c-exp), compiled-body :: DAG.CaseResults%(is-c-block)) -> DAG.CaseResults%(is-c-block):
  if A.is-a-blank(b.ann) or A.is-a-any(b.ann):
    c-block(
      j-block(
        compiled-e.other-stmts +
        link(
          j-var(js-id-of(b.id), compiled-e.exp),
          compiled-body.block.stmts
          )
        ),
      compiled-body.new-cases
      )
  else:
    step = visitor.cur-step
    after-ann = visitor.make-label()
    after-ann-case = j-case(after-ann, j-block(compiled-body.block.stmts))
    compiled-ann = compile-ann(b.ann, visitor)
    c-block(
      j-block(
        compiled-e.other-stmts +
        [list: j-var(js-id-of(b.id), compiled-e.exp)]  +
        compiled-ann.other-stmts +
        [list:
          j-expr(j-assign(step, after-ann)),
          j-expr(rt-method("_checkAnn", [list:
                visitor.get-loc(b.ann.l),
                compiled-ann.exp,
                j-id(js-id-of(b.id))])),
          j-break
        ]),
      concat-cons(after-ann-case, compiled-body.new-cases))
  end
end

fun get-new-cases(compiler, opt-dest, opt-body, after-label, ans):
  opt-compiled-body = opt-body.and-then(lam(b): b.visit(compiler) end)
  cases(Option) opt-dest:
    | some(dest) =>
      cases(Option) opt-compiled-body:
        | some(compiled-body) =>
          compiled-binding = compile-annotated-let(compiler, dest, c-exp(j-id(ans), empty), compiled-body)
          concat-cons(
            j-case(after-label, compiled-binding.block),
            compiled-binding.new-cases)
        | none => raise("Impossible: compile-split-app can't have a dest without a body")
      end
    | none =>
      cases(Option) opt-compiled-body:
        | some(compiled-body) =>
          concat-cons(j-case(after-label, compiled-body.block), compiled-body.new-cases)
        | none => concat-empty
      end
  end
end

fun compile-split-method-app(l, compiler, opt-dest, obj, methname, args, opt-body):
  ans = compiler.cur-ans
  step = compiler.cur-step
  compiled-obj = obj.visit(compiler).exp
  compiled-args = args.map(lam(a): a.visit(compiler).exp end)
  num-args = args.length()

  if J.is-j-id(compiled-obj):
    colon-field = rt-method("getColonField", [list: compiled-obj, j-str(methname)])
    colon-field-id = j-id(fresh-id(compiler-name("field")))
    check-method = rt-method("isMethod", [list: colon-field-id])
    after-app-label = if is-none(opt-body): compiler.cur-target else: compiler.make-label() end
    new-cases = get-new-cases(compiler, opt-dest, opt-body, after-app-label, ans)
    c-block(
      j-block([list:
          # Update step before the call, so that if it runs out of gas, the resumer goes to the right step
          j-expr(j-assign(step,  after-app-label)),
          j-expr(j-assign(compiler.cur-apploc, compiler.get-loc(l))),
          j-expr(j-assign(colon-field-id.id, colon-field)),
          # if num-args < 6:
          #   j-expr(j-assign(ans, rt-method("callIfPossible" + tostring(num-args),
          #         link(compiler.get-loc(l), link(j-id(colon-field-id), link(compiled-obj, compiled-args))))))
          # else:
            j-if(check-method, j-block([list: 
                  j-expr(j-assign(ans, j-app(j-dot(colon-field-id, "full_meth"),
                        link(compiled-obj, compiled-args))))
                ]),
              j-block([list:
                  check-fun(compiler.get-loc(l), colon-field-id),
                  j-expr(j-assign(ans, app(compiler.get-loc(l), colon-field-id, compiled-args)))
                ]))
          # end
          ,
          j-break]),
      new-cases)
  else:
    obj-id = j-id(fresh-id(compiler-name("obj")))
    colon-field = rt-method("getColonField", [list: obj-id, j-str(methname)])
    colon-field-id = j-id(fresh-id(compiler-name("field")))
    check-method = rt-method("isMethod", [list: colon-field-id])
    after-app-label = if is-none(opt-body): compiler.cur-target else: compiler.make-label() end
    new-cases = get-new-cases(compiler, opt-dest, opt-body, after-app-label, ans)
    c-block(
      j-block([list:
          # Update step before the call, so that if it runs out of gas, the resumer goes to the right step
          j-expr(j-assign(step,  after-app-label)),
          j-expr(j-assign(compiler.cur-apploc, compiler.get-loc(l))),
          j-var(obj-id.id, compiled-obj),
          j-var(colon-field-id.id, colon-field),
          # if num-args < 6:
          #   j-expr(j-assign(ans, rt-method("callIfPossible" + tostring(num-args),
          #         link(compiler.get-loc(l), link(colon-field-id, link(obj-id, compiled-args))))))
          # else:
            j-if(check-method, j-block([list: 
                  j-expr(j-assign(ans, j-app(j-dot(colon-field-id, "full_meth"),
                        link(obj-id, compiled-args))))
                ]),
              j-block([list:
                  check-fun(compiler.get-loc(l), colon-field-id),
                  j-expr(j-assign(ans, app(compiler.get-loc(l), colon-field-id, compiled-args)))
                ]))
          # end
          ,
          j-break]),
      new-cases)
  end
end

fun compile-split-app(l, compiler, opt-dest, f, args, opt-body):
  ans = compiler.cur-ans
  step = compiler.cur-step
  compiled-f = f.visit(compiler).exp
  compiled-args = args.map(lam(a): a.visit(compiler).exp end)
  after-app-label = if is-none(opt-body): compiler.cur-target else: compiler.make-label() end
  new-cases = get-new-cases(compiler, opt-dest, opt-body, after-app-label, ans)
  c-block(
    j-block([list:
        check-fun(compiler.get-loc(l), compiled-f),
        # Update step before the call, so that if it runs out of gas, the resumer goes to the right step
        j-expr(j-assign(step, after-app-label)),
        j-expr(j-assign(compiler.cur-apploc, compiler.get-loc(l))),
        j-expr(j-assign(ans, app(compiler.get-loc(l), compiled-f, compiled-args))),
        j-break]),
    new-cases)
end

fun compile-split-if(compiler, opt-dest, cond, consq, alt, opt-body):
  consq-label = compiler.make-label()
  alt-label = compiler.make-label()
  after-if-label = if is-none(opt-body): compiler.cur-target else: compiler.make-label() end
  ans = compiler.cur-ans
  compiler-after-if = compiler.{cur-target: after-if-label}
  compiled-consq = consq.visit(compiler-after-if)
  compiled-alt = alt.visit(compiler-after-if)
  new-cases =
    concat-cons(j-case(consq-label, compiled-consq.block), compiled-consq.new-cases)
    + concat-cons(j-case(alt-label, compiled-alt.block), compiled-alt.new-cases)
    + get-new-cases(compiler, opt-dest, opt-body, after-if-label, ans)
  c-block(
    j-block([list: 
        j-if(rt-method("isPyretTrue", [list: cond.visit(compiler).exp]),
          j-block([list: j-expr(j-assign(compiler.cur-step, consq-label))]),
          j-block([list: j-expr(j-assign(compiler.cur-step, alt-label))])),
        j-break
      ]),
    new-cases)
end
fun compile-cases-branch(compiler, compiled-val, branch :: N.ACasesBranch):
  compiled-body = branch.body.visit(compiler)
  temp-branch = fresh-id(compiler-name("temp_branch"))
  branch-args =
    if N.is-a-cases-branch(branch) and (branch.args.length() > 0): branch.args.map(get-bind)
    else: [list: N.a-bind(branch.body.l, compiler.resumer, A.a-blank)]
    end
  step = fresh-id(compiler-name("step"))
  ref-binds-mask = if N.is-a-cases-branch(branch):
    j-list(false, for map(cb from branch.args):
      j-bool(A.is-s-cases-bind-ref(cb.field-type))
    end)
  else:
    j-list(false, [list:])
  end
  compiled-branch-fun =
    compile-fun-body(branch.body.l, step, temp-branch, compiler, branch-args, none, branch.body, true)
  preamble = cases(N.ACasesBranch) branch:
    | a-cases-branch(_, pat-loc, name, args, body) =>
      branch-given-arity = j-num(args.length())
      obj-expected-arity = j-dot(compiled-val, "$arity")
      checker = j-if(j-binop(obj-expected-arity, j-geq, j-num(0)),
        j-block([list:
            j-if1(j-binop(branch-given-arity, j-neq, obj-expected-arity),
              j-block([list:
                  j-expr(j-method(rt-field("ffi"), "throwCasesArityErrorC",
                      [list: compiler.get-loc(pat-loc), branch-given-arity, obj-expected-arity]))]))]),
        j-block([list:
            j-expr(j-method(rt-field("ffi"), "throwCasesSingletonErrorC",
                [list: compiler.get-loc(pat-loc), j-true]))]))
      [list: checker]
    | a-singleton-cases-branch(_, pat-loc, _, _) =>
      checker =
        j-if1(j-binop(j-dot(compiled-val, "$arity"), j-neq, j-num(-1)),
          j-block([list:
              j-expr(j-method(rt-field("ffi"), "throwCasesSingletonErrorC",
                  [list: compiler.get-loc(pat-loc), j-false]))]))
      [list: checker]
  end
  deref-fields = j-expr(j-assign(compiler.cur-ans, j-method(compiled-val, "$app_fields", [list: j-id(temp-branch), ref-binds-mask])))
  actual-app =
    [list:
      j-expr(j-assign(compiler.cur-step, compiler.cur-target)),
      j-var(temp-branch,
        j-fun(branch-args.map(lam(arg): formal-shadow-name(arg.id) end), compiled-branch-fun)),
      deref-fields,
      j-break]

  c-block(
    j-block(preamble + actual-app),
    concat-empty)
end

fun compile-split-cases(compiler, cases-loc, opt-dest, typ, val :: N.AVal, branches :: List<N.ACasesBranch>, _else :: N.AExpr, opt-body :: Option<N.AExpr>):
  compiled-val = val.visit(compiler).exp
  after-cases-label = if is-none(opt-body): compiler.cur-target else: compiler.make-label() end
  compiler-after-cases = compiler.{cur-target: after-cases-label}
  compiled-branches = branches.map(compile-cases-branch(compiler-after-cases, compiled-val, _))
  compiled-else = _else.visit(compiler-after-cases)
  branch-labels = branches.map(lam(_): compiler.make-label() end)
  else-label = compiler.make-label()
  branch-cases = for fold2(acc from concat-empty, label from branch-labels, branch from compiled-branches):
    acc
    ^ concat-snoc(_, j-case(label, branch.block))
    ^ concat-append(_, branch.new-cases)
  end
  branch-else-cases =
    (branch-cases
      ^ concat-snoc(_, j-case(else-label, compiled-else.block))
      ^ concat-append(_, compiled-else.new-cases))
  dispatch-table = j-obj(for map2(branch from branches, label from branch-labels): j-field(branch.name, label) end)
  dispatch = j-id(fresh-id(compiler-name("cases_dispatch")))
  # NOTE: Ignoring typ for the moment!
  new-cases =
    branch-else-cases
    + get-new-cases(compiler, opt-dest, opt-body, after-cases-label, compiler.cur-ans)
  c-block(
    j-block([list:
        j-var(dispatch.id, dispatch-table),
        # j-expr(j-app(j-dot(j-id("console"), "log"),
        #     [list: j-str("$name is "), j-dot(compiled-val, "$name"),
        #       j-str("val is "), compiled-val,
        #       j-str("dispatch is "), dispatch])),
        j-expr(j-assign(compiler.cur-apploc, compiler.get-loc(cases-loc))),
        j-expr(j-assign(compiler.cur-step,
            j-binop(j-bracket(dispatch, j-dot(compiled-val, "$name")), J.j-or, else-label))),
        j-break]),
    new-cases)
end

fun compile-split-update(compiler, opt-dest, obj :: N.AVal, fields :: List<N.AField>, opt-body :: Option<N.AExpr>):
  ans = compiler.cur-ans
  step = compiler.cur-step
  compiled-obj = obj.visit(compiler).exp
  compiled-field-vals = fields.map(lam(a): a.value.visit(compiler).exp end)
  field-names = fields.map(lam(f): j-str(f.name) end)
  field-locs = fields.map(lam(f): compiler.get-loc(f.l) end)
  after-update-label = if is-none(opt-body): compiler.cur-target else: compiler.make-label() end
  new-cases = get-new-cases(compiler, opt-dest, opt-body, after-update-label, ans)
  c-block(
    j-block([list:
        # Update step before the call, so that if it runs out of gas, the resumer goes to the right step
        j-expr(j-assign(step, after-update-label)),
        j-expr(j-assign(ans, rt-method("checkRefAnns", [list: compiled-obj, j-list(false, field-names), j-list(false, compiled-field-vals), j-list(false, field-locs)]))),
        j-break]),
    new-cases)

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
            [list: j-var(js-id-of(name), compiled-ann.exp)] +
            visited-body.block.stmts
            ),
          visited-body.new-cases)
      | a-newtype-bind(l2, name, nameb) =>
        brander-id = js-id-of(nameb)
        visited-body = body.visit(self)
        c-block(
          j-block(
            [list:
              j-var(brander-id, rt-method("namedBrander", [list: j-str(name.toname())])),
              j-var(js-id-of(name), rt-method("makeBranderAnn", [list: j-id(brander-id), j-str(name.toname())]))
            ] +
            visited-body.block.stmts),
          visited-body.new-cases)
    end
  end,
  a-let(self, l :: Loc, b :: N.ABind, e :: N.ALettable, body :: N.AExpr):
    cases(N.ALettable) e:
      | a-app(l2, f, args) =>
        compile-split-app(l2, self, some(b), f, args, some(body))
      | a-method-app(l2, obj, m, args) =>
        compile-split-method-app(l2, self, some(b), obj, m, args, some(body))
      | a-if(l2, cond, then, els) =>
        compile-split-if(self, some(b), cond, then, els, some(body))
      | a-cases(l2, typ, val, branches, _else) =>
        compile-split-cases(self, l2, some(b), typ, val, branches, _else, some(body))
      | a-update(l2, obj, fields) =>
        compile-split-update(self, some(b), obj, fields, some(body))
      | else =>
        compiled-e = e.visit(self)
        compiled-body = body.visit(self)
        compile-annotated-let(self, b, compiled-e, compiled-body)
    end
  end,
  a-var(self, l :: Loc, b :: N.ABind, e :: N.ALettable, body :: N.AExpr):
    compiled-body = body.visit(self)
    compiled-e = e.visit(self)
    # TODO: annotations here?
    c-block(
      j-block(
        j-var(js-id-of(b.id),
          j-obj([list: j-field("$var", compiled-e.exp)
# NOTE(joe): This can be useful to turn on for debugging
#                     , j-field("$name", j-str(b.id.toname()))
                ]))
        ^ link(_, compiled-body.block.stmts)),
      compiled-body.new-cases)
  end,
  a-seq(self, l, e1, e2):
    cases(N.ALettable) e1:
      | a-app(l2, f, args) =>
        compile-split-app(l2, self, none, f, args, some(e2))
      | a-method-app(l2, obj, m, args) =>
        compile-split-method-app(l2, self, none, obj, m, args, some(e2))
      | a-if(l2, cond, consq, alt) =>
        compile-split-if(self, none, cond, consq, alt, some(e2))
      | a-cases(l2, typ, val, branches, _else) =>
        compile-split-cases(self, l2, none, typ, val, branches, _else, some(e2))
      | a-update(l2, obj, fields) =>
        compile-split-update(self, none, obj, fields, some(e2))
      | else =>
        e1-visit = e1.visit(self)
        e2-visit = e2.visit(self)
        first-stmt = if J.JStmt(e1-visit.exp): e1-visit.exp else: j-expr(e1-visit.exp) end
        c-block(
          j-block(e1-visit.other-stmts + link(first-stmt, e2-visit.block.stmts)),
          e2-visit.new-cases
        )
    end
  end,
  a-if(self, l :: Loc, cond :: N.AVal, consq :: N.AExpr, alt :: N.AExpr):
    raise("Impossible: a-if directly in compiler-visitor should never happen")
  end,
  a-cases(self, l :: Loc, typ :: A.Ann, val :: N.AVal, branches :: List<N.ACasesBranch>, _else :: N.AExpr):
    raise("Impossible: a-cases directly in compiler-visitor should never happen")
  end,
  a-update(self, l, obj, fields):
    raise("Impossible: a-update directly in compiler-visitor should never happen")
  end,
  a-lettable(self, _, e :: N.ALettable):
    cases(N.ALettable) e:
      | a-app(l, f, args) =>
        compile-split-app(l, self, none, f, args, none)
      | a-method-app(l2, obj, m, args) =>
        compile-split-method-app(l2, self, none, obj, m, args, none)
      | a-if(l, cond, consq, alt) =>
        compile-split-if(self, none, cond, consq, alt, none)
      | a-cases(l, typ, val, branches, _else) =>
        compile-split-cases(self, l, none, typ, val, branches, _else, none)
      | a-update(l, obj, fields) =>
        compile-split-update(self, none, obj, fields, none)
      | else =>
         visit-e = e.visit(self)
         c-block(
           j-block(
             j-expr(j-assign(self.cur-step, self.cur-target))
             ^ link(_, visit-e.other-stmts
                 + [list:
                 j-expr(j-assign(self.cur-ans, visit-e.exp)),
                 j-break])),
           concat-empty)
    end
  end,
  a-assign(self, l :: Loc, id :: A.Name, value :: N.AVal):
    visit-value = value.visit(self)
    c-exp(j-dot-assign(j-id(js-id-of(id)), "$var", visit-value.exp), visit-value.other-stmts)
  end,
  a-app(self, l :: Loc, f :: N.AVal, args :: List<N.AVal>):
    raise("Impossible: a-app directly in compiler-visitor should never happen")
  end,
  a-prim-app(self, l :: Loc, f :: String, args :: List<N.AVal>):
    visit-args = args.map(_.visit(self))
    set-loc = [list:
      j-expr(j-assign(self.cur-apploc, self.get-loc(l)))
    ]
    other-stmts = visit-args.foldr(lam(va, acc): va.other-stmts + acc end, set-loc)
    c-exp(rt-method(f, visit-args.map(get-exp)), other-stmts)
  end,
  
  a-ref(self, l, maybe-ann):
    cases(Option) maybe-ann:
      | none => c-exp(rt-method("makeGraphableRef", empty), empty)
      | some(ann) => raise("Cannot handle annotations in refs yet")
    end
  end,
  a-obj(self, l :: Loc, fields :: List<N.AField>):
    visit-fields = fields.map(lam(f): f.visit(self) end)
    other-stmts = visit-fields.foldr(lam(vf, acc): vf.other-stmts + acc end, empty)
    c-exp(rt-method("makeObject", [list: j-obj(visit-fields.map(o-get-field))]), other-stmts)
  end,
  a-get-bang(self, l :: Loc, obj :: N.AVal, field :: String):
    visit-obj = obj.visit(self)
    c-exp(rt-method("getFieldRef", [list: visit-obj.exp, j-str(field), self.get-loc(l)]), visit-obj.other-stmts)
  end,
  a-extend(self, l :: Loc, obj :: N.AVal, fields :: List<N.AField>):
    visit-obj = obj.visit(self)
    visit-fields = fields.map(lam(f): f.visit(self) end)
    other-stmts = visit-fields.foldr(lam(vf, acc): vf.other-stmts + acc end, visit-obj.other-stmts)
    c-exp(rt-method("extendObj", [list: self.get-loc(l), visit-obj.exp, j-obj(visit-fields.map(o-get-field))]),
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
    new-step = fresh-id(compiler-name("step"))
    temp = fresh-id(compiler-name("temp_lam"))
    len = args.length()
    # NOTE: args may be empty, so we need at least one name ("resumer") for the stack convention
    effective-args =
      if len > 0: args
      else: [list: N.a-bind(l, self.resumer, A.a-blank)]
      end
    c-exp(
      rt-method("makeFunction", [list: j-id(temp)]),
      [list:
        j-var(temp,
          j-fun(effective-args.map(lam(arg): formal-shadow-name(arg.id) end),
                compile-fun-body(l, new-step, temp, self, effective-args, some(len), body, true)))])
  end,
  a-method(self, l :: Loc, args :: List<N.ABind>, ret :: A.Ann, body :: N.AExpr):
    step = fresh-id(compiler-name("step"))
    temp-full = fresh-id(compiler-name("temp_full"))
    len = args.length()
    full-var = 
      j-var(temp-full,
        j-fun(args.map(lam(a): formal-shadow-name(a.id) end),
          compile-fun-body(l, step, temp-full, self, args, some(len), body, true)
        ))
    method-expr = if len < 9:
      rt-method("makeMethod" + tostring(len - 1), [list: j-id(temp-full)])
    else:
      rt-method("makeMethodN", [list: j-id(temp-full)])
    end
    c-exp(method-expr, [list: full-var])
  end,
  a-val(self, l :: Loc, v :: N.AVal):
    v.visit(self)
  end,
  a-field(self, l :: Loc, name :: String, value :: N.AVal):
    visit-v = value.visit(self)
    c-field(j-field(name, visit-v.exp), visit-v.other-stmts)
  end,
  a-array(self, l, values):
    visit-vals = values.map(_.visit(self))
    other-stmts = visit-vals.foldr(lam(v, acc): v.other-stmts + acc end, empty)
    c-exp(j-list(false, visit-vals.map(get-exp)), other-stmts)
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
    c-exp(j-id(js-id-of(id)), empty)
  end,
  a-id-var(self, l :: Loc, id :: A.Name):
    c-exp(j-dot(j-id(js-id-of(id)), "$var"), empty)
  end,
  a-id-letrec(self, l :: Loc, id :: A.Name, safe :: Boolean):
    s = j-id(js-id-of(id))
    if safe:
      c-exp(j-dot(s, "$var"), empty)
    else:
      c-exp(
        j-ternary(
          j-binop(j-dot(s, "$var"), j-eq, undefined),
          raise-id-exn(self.get-loc(l), id.toname()),
          j-dot(s, "$var")),
        empty)
    end
  end,

  a-data-expr(self, l, name, namet, variants, shared):
    fun brand-name(base):
      js-id-of(compiler-name("brand-" + base)).toname()
    end

    visit-shared-fields = shared.map(_.visit(self))
    shared-fields = visit-shared-fields.map(o-get-field)
    shared-stmts = visit-shared-fields.foldr(lam(vf, acc): vf.other-stmts + acc end, empty)
    external-brand = j-id(js-id-of(namet))

    fun make-brand-predicate(loc :: Loc, b :: J.JExpr, pred-name :: String):
      val = fresh-id(compiler-name("val"))
      j-field(
        pred-name,
        rt-method("makeFunction", [list: 
            j-fun(
              [list: val],
              j-block(
                arity-check(self.get-loc(loc), 1) +
                [list: j-return(rt-method("makeBoolean", [list: rt-method("hasBrand", [list: j-id(val), b])]))]
                )
              )
          ])
        )
    end

    fun make-variant-constructor(l2, base-id, brands-id, members, refl-name, refl-ref-fields, refl-ref-fields-mask, refl-fields, constructor-id):
      
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
      compiled-vals = for map(m from nonblank-anns): j-str(js-id-of(m.bind.id).tosourcestring()) end
      
      # NOTE(joe 6-14-2014): We cannot currently statically check for if an annotation
      # is a refinement because of type aliases.  So, we use checkAnnArgs, which takes
      # a continuation and manages all of the stack safety of annotation checking itself.

      # NOTE(joe 5-26-2015): This has been moved to a hybrid static/dynamic solution by
      # passing the check off to a runtime function that uses JavaScript's Function
      # to only do the refinement check once.
      c-exp(
        rt-method("makeVariantConstructor", [list:
            self.get-loc(l2),
            # NOTE(joe): Thunked at the JS level because compiled-anns might contain
            # references to rec ids that should be resolved later
            j-fun(empty, j-block([list: j-return(j-list(false, compiled-anns.anns.reverse()))])),
            j-list(false, compiled-vals),
            j-list(false, compiled-locs),
            j-list(false, for map(m from members):
              j-bool(N.is-a-mutable(m.member-type))
            end),
            j-list(false, members.map(lam(m): j-str(js-id-of(m.bind.id).tosourcestring()) end)),
            refl-ref-fields-mask,
            j-id(base-id),
            j-id(brands-id),
            refl-name,
            refl-ref-fields,
            refl-fields,
            constructor-id
          ]),
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

      refl-base-fields =
        cases(N.AVariant) v:
          | a-singleton-variant(_, _, _) => empty
          | a-variant(_, _, _, members, _) =>
            [list:
              j-field("$fieldNames",
                j-list(false, members.map(lam(m): j-str(m.bind.id.toname()) end)))]
        end

      f-id = const-id("f")
      refl-name = j-str(vname)
      refl-ref-fields-id = js-id-of(compiler-name(vname + "_getfieldsref"))
      refl-ref-fields =
        cases(N.AVariant) v:
          | a-variant(_, _, _, members, _) =>
            refmask-id = const-id("refmask")
            j-fun([list: f-id, refmask-id], j-block([list: j-return(j-app(j-id(f-id), 
                for map_n(n from 0, m from members):
                  field = get-dict-field(THIS, j-str(m.bind.id.toname()))
                  mask = j-bracket(j-id(refmask-id), j-num(n))
                  rt-method("derefField", [list: field, j-bool(N.is-a-mutable(m.member-type)), mask])
                end))]))
          | a-singleton-variant(_, _, _) =>
            j-fun([list: f-id], j-block([list: j-return(j-app(j-id(f-id), empty))]))
        end

      refl-ref-fields-mask-id = js-id-of(compiler-name(vname + "_mutablemask"))
      refl-ref-fields-mask =
        cases(N.AVariant) v:
          | a-singleton-variant(_, _, _) => j-list(false, empty)
          | a-variant(_, _, _, members, _) =>
            j-list(false, members.map(lam(m): if N.is-a-mutable(m.member-type): j-true else: j-false end end))
        end
      
      refl-fields-id = js-id-of(compiler-name(vname + "_getfields"))
      refl-fields =
        cases(N.AVariant) v:
          | a-variant(_, _, _, members, _) =>
            j-fun([list: const-id("f")], j-block([list: j-return(j-app(j-id(f-id), 
                      members.map(lam(m):
                          get-dict-field(THIS, j-str(m.bind.id.toname()))
                        end)))]))
          | a-singleton-variant(_, _, _) =>
            j-fun([list: const-id("f")], j-block([list: j-return(j-app(j-id(f-id), empty))]))
        end

      fun member-count(shadow v):
        cases(N.AVariant) v:
          | a-variant(_, _, _, members, _) => members.length()
          | a-singleton-variant(_, _, _) => 0
        end
      end

      match-field = j-field("_match", rt-method("makeMatch", [list: refl-name, j-num(member-count(v))]))
      
      stmts =
        visit-with-fields.foldr(lam(vf, acc): vf.other-stmts + acc end,
          [list: 
            j-var(refl-fields-id, refl-fields),
            j-var(refl-ref-fields-id, refl-ref-fields),
            j-var(refl-ref-fields-mask-id, refl-ref-fields-mask),
            j-var(variant-base-id, j-obj(refl-base-fields + shared-fields + visit-with-fields.map(o-get-field) + [list: match-field])),
            j-var(variant-brand-obj-id, variant-brands),
            j-expr(j-bracket-assign(
              j-id(variant-brand-obj-id),
              j-dot(external-brand, "_brand"),
              j-true))
        ])
      predicate = make-brand-predicate(v.l, j-str(variant-brand), A.make-checker-name(vname))

      cases(N.AVariant) v:
        | a-variant(l2, constr-loc, _, members, with-members) =>
          constr-vname = js-id-of(const-id(vname))
          compiled-constr =
            make-variant-constructor(constr-loc, variant-base-id, variant-brand-obj-id, members,
              refl-name, j-id(refl-ref-fields-id), j-id(refl-ref-fields-mask-id), j-id(refl-fields-id), j-id(variant-base-id))
          {
            stmts: stmts + compiled-constr.other-stmts + [list: j-var(constr-vname, compiled-constr.exp)],
            constructor: j-field(vname, j-id(constr-vname)),
            predicate: predicate
          }
        | a-singleton-variant(_, _, with-members) =>
          {
            stmts: stmts,
            constructor: j-field(vname, rt-method("makeDataValue", [list: j-id(variant-base-id), j-id(variant-brand-obj-id), refl-name, j-id(refl-ref-fields-id), j-id(refl-fields-id), j-num(-1), j-id(refl-ref-fields-mask-id), j-id(variant-base-id)])),
            predicate: predicate
          }
      end
    end

    variant-pieces = variants.map(compile-variant)

    header-stmts = for fold(acc from empty, piece from variant-pieces):
      piece.stmts.reverse() + acc
    end.reverse()
    obj-fields = for fold(acc from empty, piece from variant-pieces):
      [list: piece.constructor] + [list: piece.predicate] + acc
    end.reverse()

    data-predicate = make-brand-predicate(l, j-dot(external-brand, "_brand"), name)

    data-object = rt-method("makeObject", [list: j-obj([list: data-predicate] + obj-fields)])

    c-exp(data-object, shared-stmts + header-stmts)
  end
}

remove-useless-if-visitor = N.default-map-visitor.{
  a-if(self, l, c, t, e):
    cases(N.AVal) c:
      | a-bool(_, test) =>
        if test:
          visit-t = t.visit(self)
          if N.is-a-lettable(visit-t): visit-t.e else: N.a-if(l, c.visit(self), visit-t, e.visit(self)) end
        else:
          visit-e = e.visit(self)
          if N.is-a-lettable(visit-e): visit-e.e else: N.a-if(l, c.visit(self), t.visit(self), visit-e) end
        end
      | else => N.a-if(l, c.visit(self), t.visit(self), e.visit(self))
    end
  end
}

check:
  d = N.dummy-loc
  true1 = N.a-if(d, N.a-bool(d, true),
    N.a-lettable(d, N.a-val(d, N.a-num(d, 1))),
    N.a-lettable(d, N.a-val(d, N.a-num(d, 2))))
  true1.visit(remove-useless-if-visitor) is N.a-val(d, N.a-num(d, 1))

  false4 = N.a-if(d, N.a-bool(d, false),
    N.a-lettable(d, N.a-val(d, N.a-num(d, 3))),
    N.a-lettable(d, N.a-val(d, N.a-num(d, 4))))
  false4.visit(remove-useless-if-visitor) is N.a-val(d, N.a-num(d, 4))

  N.a-if(d, N.a-id(d, A.s-name(d, "x")), N.a-lettable(d, true1), N.a-lettable(d, false4)
    ).visit(remove-useless-if-visitor)
    is N.a-if(d, N.a-id(d, A.s-name(d, "x")),
    N.a-lettable(d, N.a-val(d, N.a-num(d, 1))),
    N.a-lettable(d, N.a-val(d, N.a-num(d, 4))))
  
end

fun mk-abbrevs(l):
  loc = const-id("loc")
  name = const-id("name")
  [list: 
    j-var(const-id("G"), rt-field("getFieldLoc")),
    j-var(const-id("U"), j-fun([list: loc, name],
        j-block([list: j-method(rt-field("ffi"), "throwUninitializedIdMkLoc",
                          [list: j-id(loc), j-id(name)])]))),
    j-var(const-id("M"), j-str(l.source)),
    j-var(const-id("D"), rt-field("undefined"))
  ]
end


fun compile-program(self, l, imports, prog, freevars, env):
  fun inst(id): j-app(j-id(id), [list: RUNTIME, NAMESPACE]);
  remove-imports = for fold(shadow freevars from freevars, elt from imports.map(get-name)):
    freevars.remove(elt.key())
  end
  remove-types = for fold(shadow freevars from remove-imports, elt from imports.map(_.types)):
    freevars.remove(elt.key())
  end
  free-ids = remove-types.keys-list().map(remove-types.get-value(_))
  namespace-binds = for map(n from free-ids):
    bind-name = cases(A.Name) n:
      | s-global(s) => n.toname()
      | s-type-global(s) => type-name(n.toname())
    end
    j-var(js-id-of(n), j-method(NAMESPACE, "get", [list: j-str(bind-name)]))
  end
  ids = imports.map(lam(i): js-id-of(i.name) end)
  type-imports = imports.filter(N.is-a-import-types)
  type-ids = type-imports.map(lam(i): js-id-of(i.types) end)
  filenames = imports.map(lam(i):
      cases(N.AImportType) i.import-type:
        | a-import-builtin(_, name) => "trove/" + name
        | a-import-file(_, file) => file
        | a-import-special(_, typ, args) =>
          if typ == "my-gdrive":
            "@my-gdrive/" + args.first
          else if typ == "shared-gdrive":
            "@shared-gdrive/" + args.first + "/" + args.rest.first
          else if typ == "js-http":
            "@js-http/" + args.first
          else if typ == "gdrive-js":
            "@gdrive-js/" + args.first + "/" + args.rest.first
          else:
            raise("Should have been caught earlier: unhandled import-special-type")
          end
      end
    end)
  # this needs to be freshened to support multiple repl interactions with the "same" source
  module-id = fresh-id(compiler-name(l.source)).tosourcestring()
  module-ref = lam(name): j-bracket(rt-field("modules"), j-str(name));
  input-ids = imports.map(lam(i): js-id-of(compiler-name(i.name.toname())) end)
  fun wrap-modules(mod-val-ids, mod-input-names, body-name, body-fun):
    mod-input-ids = mod-input-names.map(j-id)
    moduleVal = const-id("moduleVal")
    j-return(rt-method("loadModulesNew",
        [list: NAMESPACE, j-list(false, mod-input-ids),
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
                      j-fun([list: moduleVal],
                        j-block([list: 
                            j-expr(j-bracket-assign(rt-field("modules"), j-str(module-id), j-id(moduleVal))),
                            j-return(j-id(moduleVal))
                          ])),
                      j-str("Evaluating " + body-name.toname())
                ]))]))]))
  end
  var locations = concat-empty
  var loc-count = 0
  var loc-cache = D.make-mutable-string-dict()
  LOCS = const-id("L")
  fun get-loc(shadow l :: Loc):
    as-str = l.key()
    if loc-cache.has-key-now(as-str):
      loc-cache.get-value-now(as-str)
    else:
      ans = j-bracket(j-id(LOCS), j-num(loc-count))
      loc-cache.set-now(as-str, ans)
      loc-count := loc-count + 1
      locations := concat-snoc(locations, obj-of-loc(l))
      ans
    end
  end

  step = fresh-id(compiler-name("step"))
  toplevel-name = fresh-id(compiler-name("toplevel"))
  apploc = fresh-id(compiler-name("al"))
  resumer = compiler-name("resumer")
  resumer-bind = N.a-bind(l, resumer, A.a-blank)
  visited-body = compile-fun-body(l, step, toplevel-name,
    self.{get-loc: get-loc, cur-apploc: apploc, resumer: resumer}, # resumer gets js-id-of'ed in compile-fun-body
    [list: resumer-bind], none, prog, true)
  toplevel-fun = j-fun([list: formal-shadow-name(resumer)], visited-body)
  define-locations = j-var(LOCS, j-list(true, locations.to-list()))
  j-app(j-id(const-id("define")), [list: j-list(true, filenames.map(j-str)), j-fun(input-ids, j-block([list: 
            j-return(j-fun([list: RUNTIME.id, NAMESPACE.id],
                j-block([list: 
                    #j-expr(j-str("use strict")),
                    j-if(module-ref(module-id),
                      j-block([list: j-return(module-ref(module-id))]),
                      j-block(mk-abbrevs(l) +
                        [list: define-locations] + 
                        namespace-binds +
                        [list: wrap-modules(ids, input-ids, toplevel-name, toplevel-fun)]))])))]))])
end

fun non-splitting-compiler(env, options):
  compiler-visitor.{
    options: options,
    a-program(self, l, imports, body):
      simplified = body.visit(remove-useless-if-visitor)
      freevars = N.freevars-e(simplified)
      compile-program(self, l, imports, simplified, freevars, env)
    end
  }
end

splitting-compiler = non-splitting-compiler
