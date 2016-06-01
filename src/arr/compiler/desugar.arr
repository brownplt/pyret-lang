#lang pyret

provide *
provide-types *
import ast as A
import parse-pyret as PP
import string-dict as SD
import "compiler/compile-structs.arr" as C
import "compiler/ast-util.arr" as U

names = A.global-names

data DesugarEnv:
  | d-env(ids :: Set<String>, vars :: Set<String>, letrecs :: Set<String>)
end

data Pair:
  | pair(left, right)
end

mt-d-env = d-env([tree-set: ], [tree-set: ], [tree-set: ])

fun g(id): A.s-global(id);
fun gid(l, id): A.s-id(l, g(id));

fun check-bool<T>(l, e, cont :: (A.Expr -> T)) -> T:
  cont(A.s-prim-app(l, "checkWrapBoolean", [list: e]))
end

fun no-branches-exn(l, typ):
  A.s-prim-app(l, "throwNoBranchesMatched", [list: A.s-srcloc(l, l), A.s-str(l, typ)])
end

fun desugar-afield(f :: A.AField) -> A.AField:
  A.a-field(f.l, f.name, desugar-ann(f.ann))
end
fun desugar-ann(a :: A.Ann) -> A.Ann:
  cases(A.Ann) a:
    | a-blank => a
    | a-any(_) => a
    | a-name(_, _) => a
    | a-type-var(_, _) => a
    | a-dot(_, _, _) => a
    | a-arrow(l, args, ret, use-parens) =>
      A.a-arrow(l, args.map(desugar-ann), desugar-ann(ret), use-parens)
    | a-method(l, args, ret) =>
      A.a-arrow(l, args.map(desugar-ann), desugar-ann(ret), true)
    | a-app(l, base, args) =>
      A.a-app(l, desugar-ann(base), args.map(desugar-ann))
    | a-record(l, fields) =>
      A.a-record(l, fields.map(desugar-afield))
    | a-pred(l, ann, exp) =>
      A.a-pred(l, desugar-ann(ann), desugar-expr(exp))
  end
end

fun desugar(program :: A.Program):
  doc: ```
        Desugar non-scope and non-check based constructs.
        Preconditions on program:
          - well-formed
          - contains no s-var, s-fun, s-data, s-check, or s-check-test
          - contains no s-provide in headers
          - all where blocks are none
          - contains no s-name (e.g. call resolve-names first)
        Postconditions on program:
          - in addition to preconditions,
            contains no s-for, s-if (will all be s-if-else), s-op, s-method-field,
                        s-cases (will all be s-cases-else), s-not, s-when, s-if-pipe, s-paren
          - contains no s-underscore in expression position (but it may
            appear in binding positions as in s-let-bind, s-letrec-bind)
        ```
  cases(A.Program) program:
    | s-program(l, _provide, provided-types, imports, body) =>
      A.s-program(l, _provide, provided-types, imports, desugar-expr(body))
    | else => raise("Attempt to desugar non-program: " + torepr(program))
  end
end

fun mk-id-ann(loc, base, ann):
  a = names.make-atom(base)
  { id: a, id-b: A.s-bind(loc, false, a, ann), id-e: A.s-id(loc, a) }
end

fun mk-id(loc, base): mk-id-ann(loc, base, A.a-blank);

fun get-arith-op(str):
  if str == "op+": some("_plus")
  else if str == "op-": some("_minus")
  else if str == "op*": some("_times")
  else if str == "op/": some("_divide")
  else if str == "op<": some("_lessthan")
  else if str == "op>": some("_greaterthan")
  else if str == "op>=": some("_greaterequal")
  else if str == "op<=": some("_lessequal")
  else: none
  end
end

fun desugar-if(l, branches, _else :: A.Expr):
  for fold(acc from desugar-expr(_else), branch from branches.reverse()):
    check-bool(branch.test.l, desugar-expr(branch.test), lam(test-id):
        A.s-if-else(l,
          [list: A.s-if-branch(branch.l, test-id, desugar-expr(branch.body))],
          acc)
      end)
  end
end

fun desugar-cases-bind(cb):
  cases(A.CasesBind) cb:
    | s-cases-bind(l, typ, bind) => A.s-cases-bind(l, typ, desugar-bind(bind))
  end
end

fun desugar-case-branch(c):
  cases(A.CasesBranch) c:
    | s-cases-branch(l, pat-loc, name, args, body) =>
      # desugar-member(
      #   A.s-data-field(
      #     pat-loc,
      #     name,
      #     A.s-lam(pat-loc, [list: ], args.map(desugar-bind), A.a-blank, "", body, none)))
      A.s-cases-branch(l, pat-loc, name, args.map(desugar-cases-bind), desugar-expr(body))
    | s-singleton-cases-branch(l, pat-loc, name, body) =>
      # desugar-member(
      #   A.s-data-field(
      #     pat-loc,
      #     name,
      #     A.s-lam(pat-loc, [list: ], empty, A.a-blank, "", body, none)))
      A.s-singleton-cases-branch(l, pat-loc, name, desugar-expr(body))
  end
end

fun desugar-variant-member(m):
  cases(A.VariantMember) m:
    | s-variant-member(l, typ, bind) =>
      A.s-variant-member(l, typ, desugar-bind(bind))
  end
end

fun desugar-member(f):
  cases(A.Member) f:
    | s-method-field(l, name, params, args, ann, doc, body, _check) =>
      A.s-data-field(l, name, desugar-expr(A.s-method(l, params, args, ann, doc, body, _check)))
    | s-data-field(l, name, value) =>
      A.s-data-field(l, name, desugar-expr(value))
    | else =>
      raise("NYI(desugar-member): " + torepr(f))
  end
end

fun is-underscore(e):
  A.is-s-id(e) and A.is-s-underscore(e.id)
end

fun ds-curry-args(l, args):
  params-and-args = for fold(acc from pair([list: ], [list: ]), arg from args):
      if is-underscore(arg):
        arg-id = mk-id(l, "arg_")
        pair(link(arg-id.id-b, acc.left), link(arg-id.id-e, acc.right))
      else:
        pair(acc.left, link(arg, acc.right))
      end
    end
  pair(params-and-args.left.reverse(), params-and-args.right.reverse())
end

fun ds-curry-nullary(rebuild-node, l, obj, m):
  if is-underscore(obj):
    curried-obj = mk-id(l, "recv_")
    A.s-lam(l, [list: ], [list: curried-obj.id-b], A.a-blank, "", rebuild-node(l, curried-obj.id-e, m), none)
  else:
    rebuild-node(l, desugar-expr(obj), m)
  end
where:
  nothing
  #d = A.dummy-loc
  #ds-ed = ds-curry-nullary(A.s-dot, d, A.s-id(d, "_"), A.s-id(d, "x"))
#  ds-ed satisfies
end

fun ds-curry-binop(s, e1, e2, rebuild):
  params-and-args = ds-curry-args(s, [list: e1, e2])
  params = params-and-args.left
  cases(List) params:
    | empty => rebuild(e1, e2)
    | link(f, r) =>
      curry-args = params-and-args.right
      A.s-lam(s, [list: ], params, A.a-blank, "", rebuild(curry-args.first, curry-args.rest.first), none)
  end
end

fun ds-curry(l, f, args):
  fun fallthrough():
    params-and-args = ds-curry-args(l, args)
    params = params-and-args.left
    if is-underscore(f):
      f-id = mk-id(l, "f_")
      A.s-lam(l, empty, link(f-id.id-b, params), A.a-blank, "", A.s-app(l, f-id.id-e, params-and-args.right), none)
    else:
      ds-f = desugar-expr(f)
      if is-empty(params): A.s-app(l, ds-f, args)
      else: A.s-lam(l, [list: ], params, A.a-blank, "", A.s-app(l, ds-f, params-and-args.right), none)
      end
    end
  end
  cases(A.Expr) f:
    | s-dot(l2, obj, m) =>
      if is-underscore(obj):
        curried-obj = mk-id(l, "recv_")
        params-and-args = ds-curry-args(l, args)
        params = params-and-args.left
        A.s-lam(l, [list: ], link(curried-obj.id-b, params), A.a-blank, "",
            A.s-app(l, A.s-dot(l, curried-obj.id-e, m), params-and-args.right), none)
      else:
        fallthrough()
      end
    | else => fallthrough()
  end
where:
  d = A.dummy-loc
  n = A.s-global
  id = lam(s): A.s-id(d, A.s-global(s));
  under = A.s-id(d, A.s-underscore(d))
  ds-ed = ds-curry(
      d,
      id("f"),
      [list:  under, id("x") ]
    )
  ds-ed satisfies A.is-s-lam
  ds-ed.args.length() is 1

  ds-ed2 = ds-curry(
      d,
      id("f"),
      [list:  under, under ]
    )
  ds-ed2 satisfies A.is-s-lam
  ds-ed2.args.length() is 2

  ds-ed3 = ds-curry(
      d,
      id("f"),
      [list:
        id("x"),
        id("y")
      ]
    )
  ds-ed3.visit(A.dummy-loc-visitor) is A.s-app(d, id("f"), [list: id("x"), id("y")])

  ds-ed4 = ds-curry(
      d,
      A.s-dot(d, under, "f"),
      [list:
        id("x")
      ])
  ds-ed4 satisfies A.is-s-lam
  ds-ed4.args.length() is 1

end

fun desugar-opt<T>(f :: (T -> T), opt :: Option<T>):
  cases(Option) opt:
    | none => none
    | some(e) => some(f(e))
  end
end


fun desugar-bind(b :: A.Bind):
  cases(A.Bind) b:
    | s-bind(l, shadows, name, ann) =>
      A.s-bind(l, shadows, name, desugar-ann(ann))
    | else => raise("Non-bind given to desugar-bind: " + torepr(b))
  end
end

fun desugar-let-binds(binds):
  for map(bind from binds):
    cases(A.LetBind) bind:
      | s-let-bind(l2, b, val) =>
        A.s-let-bind(l2, desugar-bind(b), desugar-expr(val))
      | s-var-bind(l2, b, val) =>
        A.s-var-bind(l2, desugar-bind(b), desugar-expr(val))
    end
  end
end

fun desugar-letrec-binds(binds):
  for map(bind from binds):
    cases(A.LetrecBind) bind:
      | s-letrec-bind(l2, b, val) =>
        A.s-letrec-bind(l2, desugar-bind(b), desugar-expr(val))
    end
  end
end

fun desugar-expr(expr :: A.Expr):
  cases(A.Expr) expr:
    | s-module(l, answer, dv, dt, provides, types, checks) =>
      A.s-module(l, desugar-expr(answer), dv, dt, desugar-expr(provides), types.map(desugar-afield), desugar-expr(checks))
    | s-instantiate(l, inner-expr, params) =>
      A.s-instantiate(l, desugar-expr(inner-expr), params.map(desugar-ann))
    | s-block(l, stmts) =>
      A.s-block(l, stmts.map(desugar-expr))
    | s-user-block(l, body) =>
      desugar-expr(body)
    | s-app(l, f, args) =>
      ds-curry(l, f, args.map(desugar-expr))
    | s-prim-app(l, f, args) =>
      A.s-prim-app(l, f, args.map(desugar-expr))
    | s-lam(l, params, args, ann, doc, body, _check) =>
      A.s-lam(l, params, args.map(desugar-bind), desugar-ann(ann), doc, desugar-expr(body), desugar-opt(desugar-expr, _check))
    | s-method(l, params, args, ann, doc, body, _check) =>
      A.s-method(l, params, args.map(desugar-bind), desugar-ann(ann), doc, desugar-expr(body), desugar-opt(desugar-expr, _check))
    | s-type(l, name, params, ann) => A.s-type(l, name, params, desugar-ann(ann))
    | s-newtype(l, name, namet) => expr
    | s-type-let-expr(l, binds, body) =>
      fun desugar-type-bind(tb):
        cases(A.TypeLetBind) tb:
          | s-type-bind(l2, name, params, ann) => A.s-type-bind(l2, name, params, desugar-ann(ann))
          | s-newtype-bind(l2, name, nameb) => tb
        end
      end
      A.s-type-let-expr(l, binds.map(desugar-type-bind), desugar-expr(body))
    | s-let-expr(l, binds, body) =>
      new-binds = desugar-let-binds(binds)
      A.s-let-expr(l, new-binds, desugar-expr(body))
    | s-letrec(l, binds, body) =>
      A.s-letrec(l, desugar-letrec-binds(binds), desugar-expr(body))
    | s-data-expr(l, name, namet, params, mixins, variants, shared, _check) =>
      fun extend-variant(v):
        cases(A.Variant) v:
          | s-variant(l2, constr-loc, vname, members, with-members) =>
            A.s-variant(
              l2,
              constr-loc,
              vname,
              members.map(desugar-variant-member),
              with-members.map(desugar-member))
          | s-singleton-variant(l2, vname, with-members) =>
            A.s-singleton-variant(
              l2,
              vname,
              with-members.map(desugar-member))
        end
      end
      A.s-data-expr(l, name, namet, params, mixins.map(desugar-expr), variants.map(extend-variant),
        shared.map(desugar-member), desugar-opt(desugar-expr, _check))
    | s-when(l, test, body) =>
      check-bool(test.l, desugar-expr(test), lam(test-id-e):
          A.s-if-else(l,
            [list: A.s-if-branch(l, test-id-e, A.s-block(l, [list: desugar-expr(body), gid(l, "nothing")]))],
            A.s-block(l, [list: gid(l, "nothing")]))
        end)
    | s-if(l, branches) =>
      desugar-if(l, branches, A.s-block(l, [list: no-branches-exn(l, "if")]))
    | s-if-else(l, branches, _else) =>
      desugar-if(l, branches, _else)
    | s-if-pipe(l, branches) =>
      desugar-if(l, branches, A.s-block(l, [list: no-branches-exn(l, "ask")]))
    | s-if-pipe-else(l, branches, _else) =>
      desugar-if(l, branches, _else)
    | s-cases(l, typ, val, branches) =>
      A.s-cases(l, desugar-ann(typ), desugar-expr(val), branches.map(desugar-case-branch))
      # desugar-cases(l, typ, desugar-expr(val), branches.map(desugar-case-branch),
    | s-cases-else(l, typ, val, branches, _else) =>
      A.s-cases-else(l, desugar-ann(typ), desugar-expr(val),
        branches.map(desugar-case-branch),
        desugar-expr(_else))
      # desugar-cases(l, typ, desugar-expr(val), branches.map(desugar-case-branch), desugar-expr(_else))
    | s-assign(l, id, val) => A.s-assign(l, id, desugar-expr(val))
    | s-dot(l, obj, field) => ds-curry-nullary(A.s-dot, l, obj, field)
    | s-get-bang(l, obj, field) => ds-curry-nullary(A.s-get-bang, l, obj, field)
    | s-update(l, obj, fields) => ds-curry-nullary(A.s-update, l, obj, fields.map(desugar-member))
    | s-extend(l, obj, fields) => ds-curry-nullary(A.s-extend, l, obj, fields.map(desugar-member))
    | s-for(l, iter, bindings, ann, body) =>
      values = bindings.map(_.value).map(desugar-expr)
      the-function = A.s-lam(l, [list: ], bindings.map(_.bind).map(desugar-bind), desugar-ann(ann), "", desugar-expr(body), none)
      A.s-app(l, desugar-expr(iter), link(the-function, values))
    | s-op(l, op-l, op, left, right) =>
      cases(Option) get-arith-op(op):
        | some(field) =>
          ds-curry-binop(l, desugar-expr(left), desugar-expr(right),
            lam(e1, e2):
              A.s-app(l, gid(l, field), [list: e1, e2])
            end)
        | none =>
          fun thunk(e): A.s-lam(l, [list: ], [list: ], A.a-blank, "", A.s-block(l, [list: e]), none) end
          fun opbool(fld):
            A.s-app(l, A.s-dot(l, desugar-expr(left), fld), [list: thunk(desugar-expr(right))])
          end
          fun collect-op(opname, exp):
            if A.is-s-op(exp):
              if exp.op == opname: collect-op(opname, exp.left) + collect-op(opname, exp.right)
              else: [list: exp]
              end
            else: [list: exp]
            end
          end
          collect-ors = collect-op("opor", _)
          collect-ands = collect-op("opand", _)
          collect-carets = collect-op("op^", _)
          fun eq-op(fun-name):
            ds-curry-binop(l, desugar-expr(left), desugar-expr(right),
              lam(e1, e2):
                A.s-app(l, gid(l, fun-name), [list: e1, e2])
              end)
          end
          if op == "op==": eq-op("equal-always")
          else if op == "op=~": eq-op("equal-now")
          else if op == "op<=>": eq-op("identical")
          else if op == "op<>":
            ds-curry-binop(l, desugar-expr(left), desugar-expr(right),
              lam(e1, e2):
                A.s-prim-app(l, "not", [list: A.s-app(l, gid(l, "equal-always"), [list: e1, e2])])
              end)
          else if op == "opor":
            fun helper(operands):
              cases(List) operands.rest:
                | empty =>
                  check-bool(operands.first.l, desugar-expr(operands.first), lam(or-oper): or-oper;)
                | link(_, _) =>
                  check-bool(operands.first.l, desugar-expr(operands.first), lam(or-oper):
                      A.s-if-else(l,
                        [list: A.s-if-branch(l, or-oper, A.s-bool(l, true))],
                        helper(operands.rest))
                    end)
              end
            end
            operands = collect-ors(expr)
            helper(operands)
          else if op == "opand":
            fun helper(operands):
              cases(List) operands.rest:
                | empty =>
                  check-bool(operands.first.l, desugar-expr(operands.first), lam(and-oper): and-oper;)
                | link(_, _) =>
                  check-bool(operands.first.l, desugar-expr(operands.first), lam(and-oper):
                      A.s-if-else(l,
                        [list: A.s-if-branch(l, and-oper, helper(operands.rest))],
                        A.s-bool(l, false))
                    end)
              end
            end
            operands = collect-ands(expr)
            helper(operands)
          else if op == "op^":
            operands = collect-carets(expr)
            for fold(acc from desugar-expr(operands.first), f from operands.rest):
              A.s-app(f.l, desugar-expr(f), [list: acc])
            end
          else:
            raise("No implementation for " + op)
          end
      end
    | s-id(l, x) => expr
    | s-id-var(l, x) => expr
    | s-id-letrec(_, _, _) => expr
    | s-srcloc(_, _) => expr
    | s-num(_, _) => expr
    | s-frac(l, num, den) => A.s-num(l, num / den) # NOTE: Possibly must preserve further?
    | s-str(_, _) => expr
    | s-bool(_, _) => expr
    | s-obj(l, fields) => A.s-obj(l, fields.map(desugar-member))
    | s-ref(l, ann) => A.s-ann(l, desugar-ann(ann))
    | s-construct(l, modifier, constructor, elts) =>
      cases(A.ConstructModifier) modifier:
        | s-construct-normal =>
          len = elts.length()
          desugared-elts = elts.map(desugar-expr)
          if len <= 5:
            A.s-app(constructor.l, desugar-expr(A.s-dot(constructor.l, constructor, "make" + tostring(len))),
              desugared-elts)
          else:
            A.s-app(constructor.l, desugar-expr(A.s-dot(constructor.l, constructor, "make")),
              [list: A.s-array(l, desugared-elts)])
          end
        | s-construct-lazy =>
          A.s-app(constructor.l, desugar-expr(A.s-dot(constructor.l, constructor, "lazy-make")),
            [list: A.s-array(l,
                  elts.map(lam(elt): desugar-expr(A.s-lam(elt.l, empty, empty, A.a-blank, "", elt, none)) end))])
      end
    | s-paren(l, e) => desugar-expr(e)
    # NOTE(john): see preconditions; desugar-scope should have already happened
    | s-let(_, _, _, _)           => raise("s-let should have already been desugared")
    | s-var(_, _, _)              => raise("s-var should have already been desugared")
    # NOTE(joe): see preconditions; desugar-checks should have already happened
    | s-check(l, name, body, keyword-check) =>
      A.s-check(l, name, desugar-expr(body), keyword-check)
    | s-check-test(l, op, refinement, left, right) =>
      A.s-check-test(l, op, desugar-opt(desugar-expr, refinement), desugar-expr(left), desugar-opt(desugar-expr, right))
    | else => raise("NYI (desugar): " + torepr(expr))
  end
where:
  d = A.dummy-loc
  unglobal = A.default-map-visitor.{
    s-global(self, s): A.s-name(d, s) end,
    s-atom(self, base, serial): A.s-name(d, base) end
  }
  p = lam(str): PP.surface-parse(str, "test").block.visit(A.dummy-loc-visitor);
  ds = lam(prog): desugar-expr(prog).visit(unglobal).visit(A.dummy-loc-visitor) end
  id = lam(s): A.s-id(d, A.s-name(d, s));
  one = A.s-num(d, 1)
  two = A.s-num(d, 2)
  pretty = lam(prog): prog.tosource().pretty(80).join-str("\n") end

  if-else = "if true: 5 else: 6 end"
  ask-otherwise = "ask: | true then: 5 | otherwise: 6 end"
  p(if-else) ^ pretty is if-else
  p(ask-otherwise) ^ pretty is ask-otherwise

  prog2 = p("[list: 1,2,1 + 2]")
  ds(prog2)
    is A.s-block(d,
    [list:  A.s-app(d, A.s-dot(d, A.s-id(d, A.s-name(d, "list")), "make3"),
        [list:  one, two, A.s-app(d, id("_plus"), [list: one, two])])])

  prog3 = p("[list: 1,2,1 + 2,1,2,2 + 1]")
  ds(prog3)
    is A.s-block(d,
    [list:  A.s-app(d, A.s-dot(d, A.s-id(d, A.s-name(d, "list")), "make"),
        [list:  A.s-array(d,
            [list: one, two, A.s-app(d, id("_plus"), [list: one, two]),
              one, two, A.s-app(d, id("_plus"), [list: two, one])])])])

  prog4 = p("for map(elt from l): elt + 1 end")
  ds(prog4) is p("map(lam(elt): _plus(elt, 1) end, l)")

  # Some kind of bizarre parse error here
  # prog4 = p("(((5 + 1)) == 6) or o^f")
  #  ds(prog4) is p("builtins.equiv(5._plus(1), 6)._or(lam(): f(o) end)")

  # ds(p("(5)")) is ds(p("5"))

  # prog5 = p("cases(List) l: | empty => 5 + 4 | link(f, r) => 10 end")
  # dsed5 = ds(prog5)
  # cases-name = tostring(dsed5.stmts.first.binds.first.b.id)
  # compare = (cases-name + " = l " +
  #   cases-name + "._match({empty: lam(): 5._plus(4) end, link: lam(f, r): 10 end},
  #   lam(): raise('no cases matched') end)")
  # dsed5 is ds(p(compare))

end
