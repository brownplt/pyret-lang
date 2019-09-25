#lang pyret

provide *
provide-types *
import file("ast.arr") as A
import js-file("parse-pyret") as PP
import string-dict as SD
import srcloc as S
import lists as L
import file("compile-structs.arr") as C
import file("ast-util.arr") as U
import file("resolve-scope.arr") as R

names = A.global-names

data DesugarEnv:
  | d-env(ids :: Set<String>, vars :: Set<String>, letrecs :: Set<String>)
end

data Pair:
  | pair(left, right)
end

mt-d-env = d-env([tree-set: ], [tree-set: ], [tree-set: ])
var generated-binds = SD.make-mutable-string-dict()

fun g(id): A.s-global(id) end
fun gid(l, id): A.s-id(l, g(id)) end
fun bid(l, name): A.s-dot(l, A.s-id(l, g("builtins")), name) end

flat-prim-app = A.prim-app-info-c(false)

fun get-table-column(op-l, l, e, column):
  A.s-app(l,
    A.s-dot(A.dummy-loc, e, "_column-index"),
    [list:
      A.s-srcloc(A.dummy-loc, op-l),
      A.s-srcloc(A.dummy-loc, l),
      column.name,
      A.s-srcloc(A.dummy-loc, column.l)])
end

fun no-branches-exn(l, typ):
  A.s-prim-app(l, "throwNoBranchesMatched", [list: A.s-srcloc(l, l), A.s-str(l, typ)], flat-prim-app)
end
fun bool-exn(l, typ, val):
  A.s-prim-app(l, "throwNonBooleanCondition", [list: A.s-srcloc(l, l), A.s-str(l, typ), val], flat-prim-app)
end
fun bool-op-exn(l, position, typ, val):
  A.s-prim-app(l, "throwNonBooleanOp", [list: A.s-srcloc(l, l), A.s-str(l, position), A.s-str(l, typ), val], flat-prim-app)
end
fun template-exn(l):
  A.s-prim-app(l, "throwUnfinishedTemplate", [list: A.s-srcloc(l, l)], flat-prim-app)
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
    | a-arrow-argnames(l, args, ret, use-parens) =>
      A.a-arrow-argnames(l, args.map(desugar-ann), desugar-ann(ret), use-parens)
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
          - NOTE(tiffay): Outdated postconditions
          - in addition to preconditions,
            contains no s-for, s-if (will all be s-if-else), s-op, s-method-field,
                        s-cases (will all be s-cases-else), s-not, s-when, s-if-pipe, s-paren
          - contains no s-underscore in expression position (but it may
            appear in binding positions as in s-let-bind, s-letrec-bind)
        Postconditions on program:
          - contains no s-if, s-if-pipe, s-if-else-pipe, s-cases, s-when, s-paren
          - s-for will remain: it is useful to know for the code generator
          - s-op will remain: code generated for these ops are an implementation detail
          - s-method-field will remain: different code generated properties based on position
          - s-not does not exist
          - contains no s-underscore in expression position (but it may
            appear in binding positions as in s-let-bind, s-letrec-bind)
          - s-construct is not desugared
        ```
  cases(A.Program) program block:
    | s-program(l, _provide, provided-types, provides, imports, body) =>
      generated-binds := SD.make-mutable-string-dict()
      {ast: A.s-program(l, _provide, provided-types, provides, imports, desugar-expr(body)), new-binds: generated-binds}
    | else => raise("Attempt to desugar non-program: " + torepr(program))
  end
end

fun mk-id-ann(loc, base, ann) block:
  a = names.make-atom(base)
  generated-binds.set-now(a.key(), C.value-bind(C.bo-local(loc, a), C.vb-let, a, ann))
  { id: a, id-b: A.s-bind(loc, false, a, ann), id-e: A.s-id(loc, a) }
end

fun mk-id-var-ann(loc, base, ann) block:
  a = names.make-atom(base)
  generated-binds.set-now(a.key(), C.value-bind(C.bo-local(loc, a), C.vb-var, a, ann))
  { id: a, id-b: A.s-bind(loc, false, a, ann), id-e: A.s-id-var(loc, a) }
end

fun mk-id(loc, base): mk-id-ann(loc, base, A.a-blank) end

fun mk-id-var(loc, base): mk-id-var-ann(loc, base, A.a-blank) end

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
    A.s-if-else(l,
      [list: A.s-if-branch(branch.l, desugar-expr(branch.test), desugar-expr(branch.body))],
      acc, blocky)
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
      A.s-cases-branch(l, pat-loc, name, args.map(desugar-cases-bind), desugar-expr(body))
    | s-singleton-cases-branch(l, pat-loc, name, body) =>
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
    | s-method-field(l, name, params, args, ann, doc, body, _check-loc, _check, blocky) =>
      A.s-method-field(l, name, params, args, ann, doc, desugar-expr(body),
      _check-loc, cases(Option) _check:
        | some(c) => some(desugar-expr(c))
        | none => none
      end, blocky)
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
    A.s-lam(l, "", [list: ], [list: curried-obj.id-b], A.a-blank, "", rebuild-node(l, curried-obj.id-e, m), none, none, false)
  else:
    rebuild-node(l, desugar-expr(obj), m)
  end
# where:
#   nothing
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
      A.s-lam(s, "", [list: ], params, A.a-blank, "", rebuild(curry-args.first, curry-args.rest.first), none, none, false)
  end
end

fun ds-curry(l, f, args):
  fun fallthrough():
    params-and-args = ds-curry-args(l, args)
    params = params-and-args.left
    if is-underscore(f):
      f-id = mk-id(l, "f_")
      A.s-lam(l, "", empty, link(f-id.id-b, params), A.a-blank, "", A.s-app(l, f-id.id-e, params-and-args.right), none, none, false)
    else:
      ds-f = desugar-expr(f)
      if is-empty(params): A.s-app(l, ds-f, args)
      else: A.s-lam(l, "", [list: ], params, A.a-blank, "", A.s-app(l, ds-f, params-and-args.right), none, none, false)
      end
    end
  end
  cases(A.Expr) f:
    | s-dot(l2, obj, m) =>
      if is-underscore(obj):
        curried-obj = mk-id(l, "recv_")
        params-and-args = ds-curry-args(l, args)
        params = params-and-args.left
        A.s-lam(l, "", [list: ], link(curried-obj.id-b, params), A.a-blank, "",
            A.s-app(l, A.s-dot(l, curried-obj.id-e, m), params-and-args.right), none, none, false)
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
    | s-module(l, answer, dm, dv, dt, checks) =>
      A.s-module(l, desugar-expr(answer), dm, dv, dt, desugar-expr(checks))
    | s-instantiate(l, inner-expr, params) =>
      A.s-instantiate(l, desugar-expr(inner-expr), params.map(desugar-ann))
    | s-block(l, stmts) =>
      A.s-block(l, stmts.map(desugar-expr))
    | s-user-block(l, body) =>
      desugar-expr(body)
    | s-template(l) => expr # template-exn(l)
    | s-app(l, f, args) =>
      ds-curry(l, f, args.map(desugar-expr))
    | s-prim-app(l, f, args, app-info) =>
      A.s-prim-app(l, f, args.map(desugar-expr), app-info)
    | s-lam(l, name, params, args, ann, doc, body, _check-loc, _check, blocky) =>
      A.s-lam(l, name, params, args.map(desugar-bind), desugar-ann(ann), doc, desugar-expr(body), _check-loc, desugar-opt(desugar-expr, _check), blocky)
    | s-method(l, name, params, args, ann, doc, body, _check-loc, _check, blocky) =>
      A.s-method(l, name, params, args.map(desugar-bind), desugar-ann(ann), doc, desugar-expr(body), _check-loc, desugar-opt(desugar-expr, _check), blocky)
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
    | s-data-expr(l, name, namet, params, mixins, variants, shared, _check-loc, _check) =>
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
        shared.map(desugar-member), _check-loc, desugar-opt(desugar-expr, _check))
    | s-when(l, test, body, blocky) =>
      ds-test = desugar-expr(test)
      g-nothing = gid(l, "nothing")
      ds-body = desugar-expr(body)
      A.s-if-else(l,
        [list:
          A.s-if-branch(l, ds-test, if A.is-s-block(body): A.s-block(l, ds-body.stmts + [list: g-nothing])
            else: A.s-block(l, [list: ds-body, g-nothing])
            end)],
        A.s-block(l, [list: g-nothing]),
        blocky)
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
    | s-bracket(l, obj, key) =>
      ds-curry-binop(l, desugar-expr(obj), desugar-expr(key), lam(e1, e2):
          A.s-prim-app(l, "getBracket", [list: A.s-srcloc(l, l), e1, e2], A.prim-app-info-c(true))
        end)
    | s-get-bang(l, obj, field) => ds-curry-nullary(A.s-get-bang, l, obj, field)
    | s-update(l, obj, fields) => ds-curry-nullary(A.s-update, l, obj, fields.map(desugar-member))
    | s-extend(l, obj, fields) => ds-curry-nullary(A.s-extend, l, obj, fields.map(desugar-member))
    | s-for(l, iter, bindings, ann, body, blocky) =>
      values = bindings.map(lam(to-map): A.s-for-bind(to-map.l, to-map.bind, desugar-expr(to-map.value)) end)
      A.s-for(l, iter, values, ann, desugar-expr(body), blocky)
    | s-op(l, op-l, op, left, right) =>
      A.s-op(l, op-l, op, desugar-expr(left), desugar-expr(right))
    | s-id(l, x) => expr
    | s-id-modref(_, _, _, _) => expr
    | s-id-var-modref(_, _, _, _) => expr
    | s-id-var(l, x) => expr
    | s-id-letrec(_, _, _) => expr
    | s-srcloc(_, _) => expr
    | s-num(_, _) => expr
      # num, den are exact ints, and s-frac desugars to the exact rational num/den
    | s-frac(l, num, den) => A.s-num(l, num / den) # NOTE: Possibly must preserve further?
      # num, den are exact ints, and s-rfrac desugars to the roughnum fraction corresponding to num/den
    | s-rfrac(l, num, den) => A.s-num(l, num-to-roughnum(num / den)) # NOTE: Possibly must preserve further?
    | s-str(_, _) => expr
    | s-bool(_, _) => expr
    | s-obj(l, fields) => A.s-obj(l, fields.map(desugar-member))
    | s-tuple(l, fields) => A.s-tuple(l, fields.map(desugar-expr))
    | s-tuple-get(l, tup, index, index-loc) => A.s-tuple-get(l, desugar-expr(tup), index, index-loc)
    | s-ref(l, ann) => A.s-ref(l, desugar-ann(ann))
    | s-construct(l, modifier, constructor, elts) =>
      A.s-construct(l, modifier, desugar-expr(constructor), elts.map(lam(e): desugar-expr(e) end))
    | s-reactor(l, fields) =>
      fields-by-name = SD.make-mutable-string-dict()
      init-and-non-init = for lists.partition(f from fields) block:
        when f.name <> "init": fields-by-name.set-now(f.name, f.value) end
        f.name == "init"
      end
      init = init-and-non-init.is-true.first.value
      non-init-fields = init-and-non-init.is-false
      field-names = C.reactor-optional-fields
      option-fields = for SD.map-keys(f from field-names):
        if fields-by-name.has-key-now(f):
          this-field = fields-by-name.get-value-now(f)
          this-field-l = this-field.l
          A.s-data-field(this-field-l, f, A.s-prim-app(this-field-l, "makeSome",
              [list: A.s-check-expr(this-field-l, desugar-expr(this-field), field-names.get-value(f)(this-field-l))],
              flat-prim-app))
        else:
          A.s-data-field(l, f, A.s-prim-app(l, "makeNone", [list:], flat-prim-app))
        end
      end
      A.s-prim-app(l, "makeReactor", [list: desugar-expr(init), A.s-obj(l, option-fields)], flat-prim-app)
    | s-table(l, headers, rows) =>
      A.s-table(l, headers, rows.map(lam(r): A.s-table-row(r.l, r.elems.map(lam(e): desugar-expr(e) end)) end))
    | s-paren(l, e) => desugar-expr(e)
    # NOTE(john): see preconditions; desugar-scope should have already happened
    | s-let(_, _, _, _)           => raise("s-let should have already been desugared")
    | s-var(_, _, _)              => raise("s-var should have already been desugared")
    # NOTE(joe): see preconditions; desugar-checks should have already happened
    | s-check(l, name, body, keyword-check) =>
      A.s-check(l, name, desugar-expr(body), keyword-check)
    | s-check-test(l, op, refinement, left, right, cause) =>
      A.s-check-test(l, op, desugar-opt(desugar-expr, refinement), desugar-expr(left), desugar-opt(desugar-expr, right), desugar-opt(desugar-expr, cause))
    | s-load-table(l, headers, spec) =>
      A.s-load-table(l, headers, spec.map(lam(s):
        cases(A.LoadTableSpec) s:
          | s-sanitize(shadow l, name, sanitizer) => A.s-sanitize(l, name, desugar-expr(sanitizer))
          | s-table-src(shadow l, src) => A.s-table-src(l, desugar-expr(src))
        end
      end))
    | s-table-extend(l, column-binds, extensions) =>
      A.s-table-extend(l, column-binds, extensions.map(lam(e):
        cases(A.TableExtendField) e:
          | s-table-extend-field(shadow l, name, value, ann) =>
            A.s-table-extend-field(l, name, desugar-expr(value), ann)
          | s-table-extend-reducer(shadow l, name, reducer, col, ann) =>
            A.s-table-extend-reducer(l, name, desugar-expr(reducer), col, ann)
        end
      end))
    | s-table-update(l, column-binds, updates) =>
      A.s-table-update(l, A.s-column-binds(column-binds.l, column-binds.binds, desugar-expr(column-binds.table)),
        updates.map(lam(u): desugar-member(u) end))
    | s-table-select(l, columns, table) =>
      A.s-table-select(l, columns, desugar-expr(table))
    | s-table-extract(l, column, table) =>
      A.s-table-extract(l, column, desugar-expr(table))
    | s-table-order(l, table, ordering) =>
      A.s-table-order(l, desugar-expr(table), ordering)
    | s-table-filter(l, column-binds, predicate) =>
      A.s-table-filter(l, A.s-column-binds(column-binds.l, column-binds.binds, desugar-expr(column-binds.table)),
        desugar-expr(predicate))
    | s-spy-block(l, message, contents) =>
      A.s-spy-block(l, cases(Option) message:
        | some(shadow message) => some(desugar-expr(message))
        | none => none
        end, contents.map(lam(c): A.s-spy-expr(c.l, c.name, desugar-expr(c.value), c.implicit-label) end))
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
    [list:  A.s-app(d,
        A.s-prim-app(d, "getMaker3", [list: A.s-id(d, A.s-name(d, "list")), A.s-str(d, "make3"), A.s-srcloc(d, d), A.s-srcloc(d, d)], flat-prim-app),
        [list:  one, two, A.s-app(d, id("_plus"), [list: one, two])])])

  prog3 = p("[list: 1,2,1 + 2,1,2,2 + 1]")
  ds(prog3)
    is A.s-block(d,
    [list:  A.s-app(d,
        A.s-prim-app(d, "getMaker", [list: A.s-id(d, A.s-name(d, "list")), A.s-str(d, "make"), A.s-srcloc(d, d), A.s-srcloc(d, d)], flat-prim-app),
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
