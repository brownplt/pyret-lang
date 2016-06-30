#lang pyret

provide *
provide-types *
import ast as A
import parse-pyret as PP
import string-dict as SD
import srcloc as S
import file("compile-structs.arr") as C
import file("ast-util.arr") as U

names = A.global-names

data DesugarEnv:
  | d-env(ids :: Set<String>, vars :: Set<String>, letrecs :: Set<String>)
end

data Pair:
  | pair(left, right)
end

mt-d-env = d-env([tree-set: ], [tree-set: ], [tree-set: ])

fun g(id): A.s-global(id) end
fun gid(l, id): A.s-id(l, g(id)) end

fun check-bool<T>(l, e, cont :: (A.Expr -> T)) -> T:
  cont(A.s-prim-app(l, "checkWrapBoolean", [list: e]))
end

fun check-table<T>(l, e, cont :: (A.Expr -> T)) -> T:
  cont(A.s-prim-app(l, "checkWrapTable", [list: e]))
end

fun check-ann(l :: S.Srcloc, expr :: A.Expr, ann :: A.Ann) -> A.Expr:
  id = mk-id-ann(l, "ann-check_", ann)
  A.s-let-expr(l, [list: A.s-let-bind(l, id.id-b, expr)], id.id-e, true)
end

fun get-table-column(l, e, column):
  A.s-app(A.dummy-loc,
    A.s-dot(A.dummy-loc, e, "_column-index"),
    [list: 
      A.s-srcloc(A.dummy-loc, l),
      column.name, 
      A.s-srcloc(A.dummy-loc, column.l)])
end

fun check-has-column(tbl, tbl-l, col, col-l):
  A.s-app(A.dummy-loc,
    A.s-dot(A.dummy-loc, tbl, "_column-index"),
    [list: 
      A.s-srcloc(A.dummy-loc, tbl-l),
      A.s-str(A.dummy-loc, tbl-l), 
      A.s-srcloc(A.dummy-loc, col-l)])
end

fun check-no-column(tbl, tbl-l, col, col-l):
  A.s-app(A.dummy-loc,
    A.s-dot(A.dummy-loc, tbl, "_no-column"),
    [list: 
      A.s-srcloc(A.dummy-loc, tbl-l),
      A.s-str(A.dummy-loc, col), 
      A.s-srcloc(A.dummy-loc, col-l)])
end


fun no-branches-exn(l, typ):
  A.s-prim-app(l, "throwNoBranchesMatched", [list: A.s-srcloc(l, l), A.s-str(l, typ)])
end
fun bool-exn(l, typ, val):
  A.s-prim-app(l, "throwNonBooleanCondition", [list: A.s-srcloc(l, l), A.s-str(l, typ), val])
end
fun bool-op-exn(l, position, typ, val):
  A.s-prim-app(l, "throwNonBooleanOp", [list: A.s-srcloc(l, l), A.s-str(l, position), A.s-str(l, typ), val])
end
fun template-exn(l):
  A.s-prim-app(l, "throwUnfinishedTemplate", [list: A.s-srcloc(l, l)])
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
    | a-tuple(l, fields) =>
      A.a-tuple(l, fields.map(desugar-ann))
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

fun mk-id(loc, base): mk-id-ann(loc, base, A.a-blank) end

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

fun desugar-if(l, branches, _else :: A.Expr, blocky):
  for fold(acc from desugar-expr(_else), branch from branches.reverse()):
    check-bool(branch.test.l, desugar-expr(branch.test), lam(test-id):
        A.s-if-else(l,
          [list: A.s-if-branch(branch.l, test-id, desugar-expr(branch.body))],
          acc, blocky)
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
    | s-method-field(l, name, params, args, ann, doc, body, _check, blocky) =>
      A.s-data-field(l, name, desugar-expr(A.s-method(l, params, args, ann, doc, body, _check, blocky)))
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
    A.s-lam(l, [list: ], [list: curried-obj.id-b], A.a-blank, "", rebuild-node(l, curried-obj.id-e, m), none, false)
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
      A.s-lam(s, [list: ], params, A.a-blank, "", rebuild(curry-args.first, curry-args.rest.first), none, false)
  end
end

fun ds-curry(l, f, args):
  fun fallthrough():
    params-and-args = ds-curry-args(l, args)
    params = params-and-args.left
    if is-underscore(f):
      f-id = mk-id(l, "f_")
      A.s-lam(l, empty, link(f-id.id-b, params), A.a-blank, "", A.s-app(l, f-id.id-e, params-and-args.right), none, false)
    else:
      ds-f = desugar-expr(f)
      if is-empty(params): A.s-app(l, ds-f, args)
      else: A.s-lam(l, [list: ], params, A.a-blank, "", A.s-app(l, ds-f, params-and-args.right), none, false)
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
            A.s-app(l, A.s-dot(l, curried-obj.id-e, m), params-and-args.right), none, false)
      else:
        fallthrough()
      end
    | else => fallthrough()
  end
where:
  d = A.dummy-loc
  n = A.s-global
  id = lam(s): A.s-id(d, A.s-global(s)) end
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
    | s-template(l) => template-exn(l)
    | s-app(l, f, args) =>
      ds-curry(l, f, args.map(desugar-expr))
    | s-prim-app(l, f, args) =>
      A.s-prim-app(l, f, args.map(desugar-expr))
    | s-lam(l, params, args, ann, doc, body, _check, blocky) =>
      A.s-lam(l, params, args.map(desugar-bind), desugar-ann(ann), doc, desugar-expr(body), desugar-opt(desugar-expr, _check), blocky)
    | s-method(l, params, args, ann, doc, body, _check, blocky) =>
      A.s-method(l, params, args.map(desugar-bind), desugar-ann(ann), doc, desugar-expr(body), desugar-opt(desugar-expr, _check), blocky)
    | s-type(l, name, params, ann) => A.s-type(l, name, params, desugar-ann(ann))
    | s-newtype(l, name, namet) => expr
    | s-type-let-expr(l, binds, body, blocky) =>
      fun desugar-type-bind(tb):
        cases(A.TypeLetBind) tb:
          | s-type-bind(l2, name, params, ann) => A.s-type-bind(l2, name, params, desugar-ann(ann))
          | s-newtype-bind(l2, name, nameb) => tb
        end
      end
      A.s-type-let-expr(l, binds.map(desugar-type-bind), desugar-expr(body), blocky)
    | s-let-expr(l, binds, body, blocky) =>
      new-binds = desugar-let-binds(binds)
      A.s-let-expr(l, new-binds, desugar-expr(body), blocky)
    | s-letrec(l, binds, body, blocky) =>
      A.s-letrec(l, desugar-letrec-binds(binds), desugar-expr(body), blocky)
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
    | s-when(l, test, body, blocky) =>
      check-bool(test.l, desugar-expr(test), lam(test-id-e):
          A.s-if-else(l,
            [list: A.s-if-branch(l, test-id-e, A.s-block(l, [list: desugar-expr(body), gid(l, "nothing")]))],
            A.s-block(l, [list: gid(l, "nothing")]),
            blocky)
        end)
    | s-if(l, branches, blocky) =>
      desugar-if(l, branches, A.s-block(l, [list: no-branches-exn(l, "if")]), blocky)
    | s-if-else(l, branches, _else, blocky) =>
      desugar-if(l, branches, _else, blocky)
    | s-if-pipe(l, branches, blocky) =>
      desugar-if(l, branches, A.s-block(l, [list: no-branches-exn(l, "ask")]), blocky)
    | s-if-pipe-else(l, branches, _else, blocky) =>
      desugar-if(l, branches, _else, blocky)
    | s-cases(l, typ, val, branches, blocky) =>
      A.s-cases(l, desugar-ann(typ), desugar-expr(val), branches.map(desugar-case-branch), blocky)
      # desugar-cases(l, typ, desugar-expr(val), branches.map(desugar-case-branch),
    | s-cases-else(l, typ, val, branches, _else, blocky) =>
      A.s-cases-else(l, desugar-ann(typ), desugar-expr(val),
        branches.map(desugar-case-branch),
        desugar-expr(_else),
        blocky)
      # desugar-cases(l, typ, desugar-expr(val), branches.map(desugar-case-branch), desugar-expr(_else))
    | s-assign(l, id, val) => A.s-assign(l, id, desugar-expr(val))
    | s-dot(l, obj, field) => ds-curry-nullary(A.s-dot, l, obj, field)
    | s-get-bang(l, obj, field) => ds-curry-nullary(A.s-get-bang, l, obj, field)
    | s-update(l, obj, fields) => ds-curry-nullary(A.s-update, l, obj, fields.map(desugar-member))
    | s-extend(l, obj, fields) => ds-curry-nullary(A.s-extend, l, obj, fields.map(desugar-member))
    | s-for(l, iter, bindings, ann, body, blocky) =>
      values = bindings.map(_.value).map(desugar-expr)
      the-function = A.s-lam(l, [list: ], bindings.map(_.bind).map(desugar-bind), desugar-ann(ann), "", desugar-expr(body), none, blocky)
      A.s-app(l, desugar-expr(iter), link(the-function, values))
    | s-for-do(l, from-clause, dos) =>
      for fold(shadow expr from desugar-expr(from-clause.value), fdo from dos):
        A.s-app(l, desugar-expr(fdo.iterator),
          link(A.s-lam(l, [list: ], 
                link(desugar-bind(from-clause.bind), fdo.bindings.map(_.bind).map(desugar-bind)), 
                desugar-ann(fdo.ann), "", desugar-expr(fdo.body), none),
               fdo.bindings.map(_.value).map(desugar-expr) + [list: expr])) end
    | s-sql(l, inspect-clause, where-clause, project-clause) =>
      d-map    = lam(e): A.s-dot(e.l, e, "map") end
      d-filter = lam(e): A.s-dot(e.l, e, "filter") end
      d-app    = lam(m, p):
        A.s-app(A.dummy-loc, m, [list:
          A.s-lam(p.l, empty, [list: inspect-clause.bind], A.a-blank, "", p, none)]) end
      desugar-expr(
        cases(Option) where-clause:
          | some(pred) =>
            d-app(d-map(d-app(d-filter(inspect-clause.value), pred)), project-clause)
          | none       =>
            d-app(d-map(inspect-clause.value) , project-clause)
        end)
    | s-op(l, op-l, op, left, right) =>
      cases(Option) get-arith-op(op):
        | some(field) =>
          ds-curry-binop(l, desugar-expr(left), desugar-expr(right),
            lam(e1, e2):
              A.s-app(l, gid(l, field), [list: e1, e2])
            end)
        | none =>
          fun thunk(e): A.s-lam(l, [list: ], [list: ], A.a-blank, "", A.s-block(l, [list: e]), none, false) end
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
                  check-bool(operands.first.l, desugar-expr(operands.first), lam(or-oper): or-oper end)
                | link(_, _) =>
                  check-bool(operands.first.l, desugar-expr(operands.first), lam(or-oper):
                      A.s-if-else(l,
                        [list: A.s-if-branch(l, or-oper, A.s-bool(l, true))],
                        helper(operands.rest), false)
                    end)
              end
            end
            operands = collect-ors(expr)
            helper(operands)
          else if op == "opand":
            fun helper(operands):
              cases(List) operands.rest:
                | empty =>
                  check-bool(operands.first.l, desugar-expr(operands.first), lam(and-oper): and-oper end)
                | link(_, _) =>
                  check-bool(operands.first.l, desugar-expr(operands.first), lam(and-oper):
                      A.s-if-else(l,
                        [list: A.s-if-branch(l, and-oper, helper(operands.rest))],
                        A.s-bool(l, false), false)
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
    | s-tuple(l, fields) => A.s-tuple(l, fields.map(desugar-expr))
    | s-tuple-get(l, tup, index, index-loc) => A.s-tuple-get(l, desugar-expr(tup), index, index-loc)
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
                  elts.map(lam(elt): desugar-expr(A.s-lam(elt.l, empty, empty, A.a-blank, "", elt, none, false)) end))])
      end
    | s-table(l, headers, rows) =>
      shadow l = A.dummy-loc
      column-names = for map(header from headers):
        A.s-str(header.l, header.name)
      end
      anns = for map(header from headers):
        desugar-ann(header.ann)
      end
      shadow rows = for map(row from rows):
        elems = for map_n(n from 0, elem from row.elems):
          check-ann(elem.l, desugar-expr(elem), anns.get(n))
        end
        A.s-array(l, elems)
      end
      A.s-prim-app(l, "makeTable",
        [list: A.s-array(l, column-names),
               A.s-array(l, rows)])
    | s-paren(l, e) => desugar-expr(e)
    # NOTE(john): see preconditions; desugar-scope should have already happened
    | s-let(_, _, _, _)           => raise("s-let should have already been desugared")
    | s-var(_, _, _)              => raise("s-var should have already been desugared")
    # NOTE(joe): see preconditions; desugar-checks should have already happened
    | s-check(l, name, body, keyword-check) =>
      A.s-check(l, name, desugar-expr(body), keyword-check)
    | s-check-test(l, op, refinement, left, right) =>
      A.s-check-test(l, op, desugar-opt(desugar-expr, refinement), desugar-expr(left), desugar-opt(desugar-expr, right))
    | s-table-extend(l, column-binds, extensions) =>
      # NOTE(philip): I am fairly certain that this will need to be moved
      #               to post-type-check desugaring, since the variables used
      #               by reducers is not well-typed
      row = mk-id(A.dummy-loc, "row")
      tbl = mk-id(A.dummy-loc, "table")
            
      columns = 
        column-binds.binds.map(lam(c):
          {name: A.s-str(A.dummy-loc, c.id.base),
           l:  c.l,
           idx:  mk-id(A.dummy-loc, c.id.base),
           val: {id-b: c,
                 id-e: A.s-id(c.l, c.id)}} end)

      split-exts = partition(A.is-s-table-extend-reducer, extensions)
      simple-exts = split-exts.is-false
      reducer-exts = split-exts.is-true

      fun mk-reducer-ann(loc, ret-type):
        one = A.a-field(loc, "one", A.a-arrow(loc, [list: A.a-any(loc)], ret-type, true))
        reduce = A.a-field(loc, "reduce",
          A.a-arrow(loc, [list: ret-type, A.a-any(loc)], ret-type, true))
        A.a-record(loc, [list: one, reduce])
      end

      reducer-vars =
        for fold(acc from pair([SD.string-dict:],[SD.string-dict:]),
            extension from reducer-exts):

          reducer-id = mk-id-ann(A.dummy-loc,
            "reducer" + extension.name,
            mk-reducer-ann(extension.l, extension.ann))

          acc-id = mk-id(A.dummy-loc, "acc" + extension.name)

          pair(acc.left.set(extension.name, reducer-id),
            acc.right.set(extension.name, acc-id))
        end
      reducers = reducer-vars.left
      accs = reducer-vars.right

      initialized-reducers =
        cases(List) reducer-exts:
          | empty => none
          | link(_,_) =>
            some((for fold(reducers-acc from empty, ext from reducer-exts):
                  cases(A.TableExtendField) ext:
                    | s-table-extend-field(_, _, _, _) => raise("Impossible")
                    | s-table-extend-reducer(shadow l, name, reducer-expr, _, _) =>
                      reducer = reducers.get-value(name)
                      acc = accs.get-value(name)
                      nothing-expr = A.s-id(l, A.s-global("nothing"))
                      link(A.s-let-bind(l, reducer.id-b, reducer-expr),
                        link(A.s-var-bind(l, acc.id-b, nothing-expr),
                          reducers-acc))
                  end
                end).reverse())
        end

      with-initialized-reducers =
        cases(Option) initialized-reducers:
          | none => lam(body): body end
          | some(binds) => lam(body): A.s-let-expr(A.dummy-loc, binds, body, true) end
        end

      fun process-extension(is-first):
        lam(extension):
          cases(A.TableExtendField) extension:
            | s-table-extend-field(_, _, _, _) => desugar-expr(extension.value)
            | s-table-extend-reducer(shadow l, name, _, col, _) =>
              reducer = reducers.get-value(name)
              acc = accs.get-value(name)
              # Dereferenced accumulator
              acc-id-e = A.s-id-var(acc.id-e.l, acc.id-e.id)
              col-id = find(lam(x): x.name.s == col.s end, columns)
              # Lift from Option monad
              shadow col-id = cases(Option) col-id:
                | none => # Dummy values; will end up unbound
                  # (TODO: Figure out how to make only one 'unbound' error show up
                  # since the desugaring produces the unbound column twice)
                  {id: col,
                    id-b: A.s-bind(l, false, col, A.a-blank),
                    id-e: A.s-id(l, col)}
                | some(v) => v.val
              end
              if is-first:
                A.s-block(A.dummy-loc,
                  [list:
                    A.s-assign(l, acc.id,
                      A.s-app(l, A.s-dot(l, reducer.id-e, "one"), [list: col-id.id-e])),
                    acc-id-e])
              else:
                A.s-block(A.dummy-loc,
                  [list:
                    A.s-assign(l, acc.id,
                      A.s-app(l, A.s-dot(l, reducer.id-e, "reduce"),
                        [list: acc-id-e, col-id.id-e])),
                    acc-id-e])
              end
          end
        end
      end

      fun data-pop-mapfun(first):
        A.s-lam(A.dummy-loc, empty,  [list: row.id-b], A.a-blank, "",
          A.s-let-expr(A.dummy-loc,
            columns.map(lam(column):
                A.s-let-bind(A.dummy-loc, column.val.id-b, 
                  A.s-prim-app(A.dummy-loc, "raw_array_get",
                    [list: row.id-e, column.idx.id-e])) end),
              A.s-prim-app(A.dummy-loc, "raw_array_concat", [list:
                  row.id-e,
                  A.s-array(A.dummy-loc,
                    extensions.map(process-extension(first)))]), true),
          none, true)
      end

      A.s-let-expr(A.dummy-loc,
        link(A.s-let-bind(A.dummy-loc, tbl.id-b,
          check-table(column-binds.table.l, desugar-expr(column-binds.table), lam(t): t end)),
        # Column Index Bindings
        columns.map(lam(column): 
          A.s-let-bind(A.dummy-loc, column.idx.id-b,
            get-table-column(column-binds.table.l, tbl.id-e, column)) end)),
        # Table Construction
        A.s-block(A.dummy-loc, [list:
          A.s-block(A.dummy-loc, extensions.map(lam(extension):
            check-no-column(tbl.id-e, column-binds.l, extension.name, extension.l) end)),
          A.s-prim-app(A.dummy-loc, "makeTable", [list:
            # Header
            A.s-prim-app(A.dummy-loc, "raw_array_concat", [list:
              A.s-dot(A.dummy-loc, tbl.id-e, "_header-raw-array"),
              A.s-array(A.dummy-loc,  extensions.map(lam(e):A.s-str(e.l, e.name) end))]),
            # Data
              with-initialized-reducers(
                A.s-prim-app(A.dummy-loc, "raw_array_map1", [list:
                    data-pop-mapfun(true),
                    data-pop-mapfun(false),
                    A.s-dot(A.dummy-loc, tbl.id-e, "_rows-raw-array")]))])]), true)
    | s-table-update(l, column-binds, updates) =>
      row = mk-id(A.dummy-loc, "row")
      new-row = mk-id(A.dummy-loc, "new-row-row")
      tbl = mk-id(l, "table")
            
      columns = 
        column-binds.binds.map(lam(c):
          {name: A.s-str(A.dummy-loc, c.id.base),
           l:  c.l,
           idx:  mk-id(A.dummy-loc, c.id.base),
           val: {id-b: c,
                 id-e: A.s-id(c.l, c.id)}} end)
                
      shadow updates = 
        updates.map(lam(u):
          {name: A.s-str(A.dummy-loc, u.name),
           l:  u.l,
           idx:  mk-id(A.dummy-loc, u.name),
           val:  desugar-expr(u.value)} end)
                 
      A.s-let-expr(A.dummy-loc,
        link(A.s-let-bind(A.dummy-loc, tbl.id-b,
          check-table(column-binds.table.l, desugar-expr(column-binds.table), lam(t): t end)),
        # Column Index Bindings
        columns.map(lam(column): 
          A.s-let-bind(A.dummy-loc, column.idx.id-b,
            get-table-column(column-binds.table.l, tbl.id-e, column)) end))
        .append(updates.map(lam(update): 
            A.s-let-bind(A.dummy-loc, update.idx.id-b,
              get-table-column(column-binds.table.l, tbl.id-e, update)) end)),
        # Table Construction
          A.s-prim-app(A.dummy-loc, "makeTable", [list:
            # Header
            A.s-dot(A.dummy-loc, tbl.id-e, "_header-raw-array"),
            # Data
            A.s-prim-app(A.dummy-loc, "raw_array_map", [list:
              A.s-lam(A.dummy-loc, empty,  [list: row.id-b], A.a-blank, "",
                A.s-let-expr(A.dummy-loc,
                  link(
                    A.s-let-bind(A.dummy-loc, new-row.id-b,
                      A.s-prim-app(A.dummy-loc, "raw_array_concat", [list:
                        row.id-e, A.s-array(A.dummy-loc, empty)])),
                    columns.map(lam(column):
                      A.s-let-bind(A.dummy-loc, column.val.id-b, 
                        A.s-prim-app(A.dummy-loc, "raw_array_get",
                            [list: new-row.id-e, column.idx.id-e])) end)),
                    A.s-let-expr(A.dummy-loc,
                      updates.map(lam(update):
                        A.s-let-bind(A.dummy-loc, new-row.id-b, 
                          A.s-prim-app(A.dummy-loc, "raw_array_set", [list:
                            new-row.id-e, update.idx.id-e, update.val])) end),
                      new-row.id-e, true), true), none, true),
              A.s-dot(A.dummy-loc, tbl.id-e, "_rows-raw-array")])]), true)
    | s-table-select(l, columns, table) =>
      row = mk-id(A.dummy-loc, "row")
      tbl = mk-id(l, "table")
      shadow columns = 
        columns.map(lam(c):
          { l: c.l,
            idx:  mk-id(c.l, c.s), 
            name: A.s-str(c.l, c.s)} end)
      A.s-let-expr(A.dummy-loc,
        link(A.s-let-bind(A.dummy-loc, tbl.id-b,
          check-table(table.l, desugar-expr(table), lam(t): t end)),
        # Column Index Bindings
        columns.map(lam(column): 
          A.s-let-bind(A.dummy-loc, column.idx.id-b,
            get-table-column(table.l, tbl.id-e, column)) end)),
        # Table Construction
        A.s-prim-app(A.dummy-loc, "makeTable", [list:
          # Header
          A.s-array(A.dummy-loc,  columns.map(_.name)),
          # Data
          A.s-prim-app(A.dummy-loc, "raw_array_map", [list:
            A.s-lam(A.dummy-loc, empty,  [list: row.id-b], A.a-blank, "",
              A.s-array(A.dummy-loc,
                columns.map(lam(c):
                  A.s-prim-app(A.dummy-loc, "raw_array_get", 
                      [list: row.id-e, c.idx.id-e]) end)), none, true),
            A.s-dot(A.dummy-loc, tbl.id-e, "_rows-raw-array")])]), true)
    | s-table-extract(l, column, table) =>
      tbl = mk-id(table.l, "table")
      col = mk-id(A.dummy-loc, column.s)
      row = mk-id(A.dummy-loc, column.s)
      A.s-let-expr(A.dummy-loc, [list:
        A.s-let-bind(A.dummy-loc, tbl.id-b,
          check-table(table.l, desugar-expr(table), lam(t): t end)),
        A.s-let-bind(A.dummy-loc, col.id-b,
          get-table-column(table.l, tbl.id-e, {l: column.l, name: A.s-str(A.dummy-loc,column.s)}))],
        # Table Construction
        A.s-prim-app(A.dummy-loc, "raw_array_to_list", [list:
          A.s-prim-app(A.dummy-loc, "raw_array_map", [list:
            A.s-lam(A.dummy-loc, empty,  [list: row.id-b], A.a-blank, "",
              A.s-prim-app(A.dummy-loc, "raw_array_get", [list: row.id-e, col.id-e]), none, true),
             A.s-dot(A.dummy-loc, tbl.id-e, "_rows-raw-array")])]), true)
    | s-table-order(l, table, ordering) =>
      row1 = mk-id(S.builtin("1"), "row1")
      row2 = mk-id(S.builtin("2"), "row2")
      tbl = mk-id(S.builtin("3"), "table")
      hdr = mk-id(S.builtin("4"), "header")
      column = ordering.column
      col = mk-id(S.builtin("5"), column.s)
      A.s-let-expr(A.dummy-loc, [list:
        A.s-let-bind(A.dummy-loc, tbl.id-b,
          check-table(table.l, desugar-expr(table), lam(t): t end)),
        A.s-let-bind(A.dummy-loc, col.id-b,
          get-table-column(table.l, tbl.id-e, {l:column.l, name:A.s-str(A.dummy-loc,column.s)}))],
        # Table Construction
        A.s-prim-app(S.builtin("14"), "makeTable", [list:
          # Header
          A.s-dot(A.dummy-loc, tbl.id-e, "_header-raw-array"),
          # Data
          A.s-prim-app(A.dummy-loc, "toArray", [list:
          A.s-app(A.dummy-loc, A.s-dot(A.dummy-loc, 
            A.s-prim-app(A.dummy-loc, "raw_array_to_list", [list: A.s-dot(A.dummy-loc, tbl.id-e, "_rows-raw-array")]), "sort-by"), [list:
              A.s-lam(S.builtin("18"), empty,  [list: row1.id-b, row2.id-b], A.a-blank, "",
                desugar-expr(A.s-op(S.builtin("19"), A.dummy-loc,
                  cases(A.ColumnSortOrder) ordering.direction:
                    | ASCENDING  => "op<"
                    | DESCENDING => "op>"
                  end,
                  A.s-prim-app(S.builtin("20"), "raw_array_get", [list: row1.id-e, col.id-e]),
                  A.s-prim-app(S.builtin("21"), "raw_array_get", [list: row2.id-e, col.id-e]))), none, true),
              A.s-lam(S.builtin("22"), empty,  [list: row1.id-b, row2.id-b], A.a-blank, "",
                desugar-expr(A.s-op(S.builtin("23"), S.builtin(""), "op==",
                  A.s-prim-app(S.builtin("24"), "raw_array_get", [list: row1.id-e, col.id-e]),
                  A.s-prim-app(S.builtin("25"), "raw_array_get", [list: row2.id-e, col.id-e]))), none, true)])])]), true)
    | s-table-filter(l, column-binds, predicate) =>
      row = mk-id(A.dummy-loc, "row")
      tbl = mk-id(l, "table")
            
      columns = 
        column-binds.binds.map(lam(c):
          {name: A.s-str(A.dummy-loc, c.id.base),
           l:  c.l,
           idx:  mk-id(A.dummy-loc, c.id.base),
           val: {id-b: c,
                 id-e: A.s-id(c.l, c.id)}} end)
                 
      A.s-let-expr(A.dummy-loc,
        link(A.s-let-bind(A.dummy-loc, tbl.id-b,
          check-table(column-binds.table.l, desugar-expr(column-binds.table), lam(t): t end)),
        # Column Index Bindings
        columns.map(lam(column): 
          A.s-let-bind(A.dummy-loc, column.idx.id-b,
            get-table-column(column-binds.table.l, tbl.id-e, column)) end)),
        # Table Construction
        A.s-prim-app(A.dummy-loc, "makeTable", [list:
          # Header
          A.s-dot(A.dummy-loc, tbl.id-e, "_header-raw-array"),
          # Data
          A.s-prim-app(A.dummy-loc, "raw_array_filter", [list:
            A.s-lam(A.dummy-loc, empty,  [list: row.id-b], A.a-blank, "",
              A.s-let-expr(A.dummy-loc,
                columns.map(lam(column):
                  A.s-let-bind(A.dummy-loc, column.val.id-b, 
                    A.s-prim-app(A.dummy-loc, "raw_array_get",
                        [list: row.id-e, column.idx.id-e])) end),
                    desugar-expr(predicate), true), none, true),
            A.s-dot(A.dummy-loc, tbl.id-e, "_rows-raw-array")])]), true)
    | else => raise("NYI (desugar): " + torepr(expr))
  end
where:
  d = A.dummy-loc
  unglobal = A.default-map-visitor.{
    method s-global(self, s): A.s-name(d, s) end,
    method s-atom(self, base, serial): A.s-name(d, base) end
  }
  p = lam(str): PP.surface-parse(str, "test").block.visit(A.dummy-loc-visitor) end
  ds = lam(prog): desugar-expr(prog).visit(unglobal).visit(A.dummy-loc-visitor) end
  id = lam(s): A.s-id(d, A.s-name(d, s)) end
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
