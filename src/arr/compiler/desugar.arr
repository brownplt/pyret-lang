#lang pyret

provide *
import ast as A
import parse-pyret as PP
import "compiler/compile-structs.arr" as C
import "compiler/ast-util.arr" as U

names = A.MakeName(0)

data DesugarEnv:
  | d-env(ids :: Set<String>, vars :: Set<String>, letrecs :: Set<String>)
end

data Pair:
  | pair(left, right)
end

mt-d-env = d-env(set([list: ]), set([list: ]), set([list: ]))

fun g(id): A.s-global(id);
fun gid(l, id): A.s-id(l, g(id));

fun check-bool(l, id, e, then, err):
  A.s-let-expr(l, [list: A.s-let-bind(l, id.id-b, e)],
    A.s-if-else(l,
      [list: A.s-if-branch(l, A.s-prim-app(l, "isBoolean", [list: id.id-e]), then)],
      err))
end
fun no-branches-exn(l, typ):
  A.s-prim-app(l, "throwNoBranchesMatched", [list: A.s-srcloc(l, l), A.s-str(l, typ)])
end
fun make-message-exception(l, msg):
  make-message-exception-e(l, A.s-str(l, msg))
end
fun make-message-exception-e(l, msg-e):
  A.s-prim-app(l, "throwMessageException", [list: msg-e])
end
fun bool-exn(l, typ, val):
  A.s-prim-app(l, "throwNonBooleanCondition", [list: A.s-srcloc(l, l), A.s-str(l, typ), val])
end
fun bool-op-exn(l, position, typ, val):
  A.s-prim-app(l, "throwNonBooleanOp", [list: A.s-srcloc(l, l), A.s-str(l, position), A.s-str(l, typ), val])
end

fun desugar-afield(f :: A.AField) -> A.AField:
  A.a-field(f.l, f.name, desugar-ann(f.ann))
end
fun desugar-ann(a :: A.Ann) -> A.Ann:
  cases(A.Ann) a:
    | a-blank => a
    | a-any => a
    | a-name(_, _) => a
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

fun desugar(program :: A.Program, compile-env :: C.CompileEnvironment):
  doc: ```
        Desugar non-scope and non-check based constructs.
        Preconditions on program:
          - well-formed
          - contains no s-let, s-var, s-data, s-check, or s-check-test
          - contains no s-provide in headers
          - all where blocks are none
          - contains no s-name (e.g. call resolve-names first)
        Postconditions on program:
          - in addition to preconditions,
            contains no s-for, s-if, s-op, s-method-field,
                        s-cases, s-not, s-when, s-if-pipe, s-paren
          - contains no s-underscore in expression position (but it may
            appear in binding positions as in s-let-bind, s-letrec-bind)
        ```
  cases(A.Program) program:
    | s-program(l, _provide, provide-types, imports, body) =>
      A.s-program(l, _provide, provide-types, imports, desugar-expr(body))
    | else => raise("Attempt to desugar non-program: " + torepr(program))
  end
end

fun mk-bind(l, id): A.s-bind(l, false, id, A.a-blank);

fun mk-id(loc, base):
  a = names.make-atom(base)
  { id: a, id-b: mk-bind(loc, a), id-e: A.s-id(loc, a) }
end

fun make-torepr(l, vname, fields, is-singleton):
  self = mk-id(l, "self")
  fun str(s): A.s-str(l, s) end
  fun call-torepr(val):
    A.s-app(l, gid(l, "torepr"), [list: A.s-dot(l, self.id-e, val.bind.id.toname())])
  end
  fun concat(v1, v2):
    A.s-op(l, "op+", v1, v2)
  end
  argstrs = cases(List) fields:
    | empty => str("")
    | link(f, r) =>
      r.foldl(
          lam(val, acc): concat(acc, concat(str(", "), call-torepr(val))) end,
          call-torepr(f)
        )
  end
  if is-singleton:
    A.s-method(l, [list: self.id-b], A.a-blank, "", str(vname), none)
  else:
    A.s-method(l, [list: self.id-b], A.a-blank, "",
      concat(str(vname), concat(str("("), concat(argstrs, str(")")))),
      none)
  end
end

fun make-match(l, case-name, fields):
  call-match-case = mk-id(l, "call-" + case-name)
  self-id = mk-id(l, "self")
  cases-id = mk-id(l, "cases-funs")
  else-id = mk-id(l, "else-clause")
  args = for map(f from fields):
      cases(A.VariantMember) f:
        | s-variant-member(l2, mtype, bind) =>
          when mtype <> A.s-normal:
            raise("Non-normal member in variant, NYI: " + torepr(f))
          end
          A.s-dot(l2, self-id.id-e, bind.id.toname())
      end
    end
  A.s-method(l, [list: self-id, cases-id, else-id].map(_.id-b), A.a-blank, "",
      A.s-if-else(l, [list: 
          A.s-if-branch(l,
              A.s-prim-app(
                  l,
                  "hasField",
                  [list: cases-id.id-e, A.s-str(l, case-name)]
                ),
              A.s-let-expr(l, [list: A.s-let-bind(l, call-match-case.id-b, A.s-dot(l, cases-id.id-e, case-name))],
                  A.s-app(l, call-match-case.id-e, args)
                )
            )
        ],
        A.s-app(l, else-id.id-e, [list: ])),
      none)
end


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
    test-id = mk-id(l, "if-")
    check-bool(branch.l, test-id, desugar-expr(branch.test),
      A.s-if-else(l,
        [list: A.s-if-branch(branch.l, test-id.id-e, desugar-expr(branch.body))],
        acc),
      bool-exn(branch.test.l, "if", test-id.id-e))
  end
end

fun desugar-case-branch(c):
  cases(A.CasesBranch) c:
    | s-cases-branch(l2, name, args, body) =>  
      desugar-member(
        A.s-data-field(
          l2,
          A.s-str(l2, name),
          A.s-lam(l2, [list: ], args.map(desugar-bind), A.a-blank, "", body, none)))
  end
end

fun desugar-cases(l, ann, val, branches, else-block):
  val-id = mk-id(l, "cases-val")
  cases-object = A.s-obj(l, branches)
  else-thunk = A.s-lam(l, [list: ], [list: ], A.a-blank, "", else-block, none)
  A.s-let-expr(l, [list: 
        A.s-let-bind(l, val-id.id-b, val)
      ],
      A.s-app(l, A.s-dot(l, val-id.id-e, "_match"), [list: cases-object, else-thunk])
    )
where:
  d = A.dummy-loc
  prog = desugar-cases(
      d,
      A.a-blank, 
      A.s-num(d, 1),
      [list: 
        A.s-data-field(d, A.s-str(d, "empty"), A.s-lam(d, [list: ], [list: ], A.a-blank, "", A.s-num(d, 5), none))
      ],
      A.s-num(d, 4)
    )
  id = prog.binds.first.b
    
  prog.visit(A.dummy-loc-visitor) is A.s-let-expr(d, [list: 
      A.s-let-bind(d, id, A.s-num(d, 1))
    ],
    A.s-app(d, A.s-dot(d, A.s-id(d, id.id), "_match"), [list: 
        A.s-obj(d, [list: 
            A.s-data-field(d, A.s-str(d, "empty"), A.s-lam(d, [list: ], [list: ], A.a-blank, "", A.s-num(d, 5), none))
          ]),
        A.s-lam(d, [list: ], [list: ], A.a-blank, "", A.s-num(d, 4), none)]))

end

fun desugar-variant-member(m):
  cases(A.VariantMember) m:
    | s-variant-member(l, typ, bind) =>
      A.s-variant-member(l, typ, desugar-bind(bind))
  end
end

fun desugar-member(f): 
  cases(A.Member) f:
    | s-method-field(l, name, args, ann, doc, body, _check) =>
      A.s-data-field(l, desugar-expr(name), desugar-expr(A.s-method(l, args, ann, doc, body, _check)))
    | s-data-field(l, name, value) =>
      A.s-data-field(l, desugar-expr(name), desugar-expr(value))
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
        arg-id = mk-id(l, "arg-")
        pair(link(arg-id.id-b, acc.left), link(arg-id.id-e, acc.right))
      else:
        pair(acc.left, link(arg, acc.right))
      end
    end
  pair(params-and-args.left.reverse(), params-and-args.right.reverse())
end

fun ds-curry-nullary(rebuild-node, l, obj, m):
  if is-underscore(obj):
    curried-obj = mk-id(l, "recv-")
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
    ds-f = desugar-expr(f)
    if is-empty(params): A.s-app(l, ds-f, args)
    else: A.s-lam(l, [list: ], params, A.a-blank, "", A.s-app(l, ds-f, params-and-args.right), none)
    end
  end
  cases(A.Expr) f:
    | s-dot(l2, obj, m) =>
      if is-underscore(obj):
        curried-obj = mk-id(l, "recv-")
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

fun<T> desugar-opt(f :: (T -> T), opt :: Option<T>):
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

fun desugar-expr(expr :: A.Expr):
  cases(A.Expr) expr:
    | s-module(l, answer, provides, types, checks) =>
      A.s-module(l, desugar-expr(answer), desugar-expr(provides), types.map(desugar-afield), desugar-expr(checks))
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
    | s-method(l, args, ann, doc, body, _check) =>
      A.s-method(l, args.map(desugar-bind), desugar-ann(ann), doc, desugar-expr(body), desugar-opt(desugar-expr, _check))
    | s-let(l, name, value, keyword-val) =>
      A.s-let(l, name, desugar-expr(value), keyword-val)
    | s-type(l, name, ann) => A.s-type(l, name, desugar-ann(ann))
    | s-newtype(l, name, namet) => expr
    | s-type-let-expr(l, binds, body) =>
      fun desugar-type-bind(tb):
        cases(A.TypeLetBind) tb:
          | s-type-bind(l2, name, ann) => A.s-type-bind(l2, name, desugar-ann(ann))
          | s-newtype-bind(l2, name, nameb) => tb
        end
      end
      A.s-type-let-expr(l, binds.map(desugar-type-bind), desugar-expr(body))
    | s-let-expr(l, binds, body) =>
      new-binds = for map(bind from binds):
        cases(A.LetBind) bind:
          | s-let-bind(l2, b, val) =>
            A.s-let-bind(l2, desugar-bind(b), desugar-expr(val))
          | s-var-bind(l2, b, val) =>
            A.s-var-bind(l2, desugar-bind(b), desugar-expr(val))
        end
      end
      A.s-let-expr(l, new-binds, desugar-expr(body))
    | s-letrec(l, binds, body) =>
      new-binds = for map(bind from binds):
          cases(A.LetrecBind) bind:
            | s-letrec-bind(l2, b, val) =>
              A.s-letrec-bind(l2, desugar-bind(b), desugar-expr(val))
          end
        end
      A.s-letrec(l, new-binds, desugar-expr(body))
    | s-data-expr(l, name, namet, params, mixins, variants, shared, _check) =>
      fun extend-variant(v):
        fun make-methods(l2, vname, members, is-singleton):
          [list: 
            A.s-data-field(l2, A.s-str(l2, "_match"), make-match(l2, vname, members)),
            A.s-data-field(l2, A.s-str(l2, "_torepr"), make-torepr(l2, vname, members, is-singleton))
          ]
        end
        cases(A.Variant) v:
          | s-variant(l2, constr-loc, vname, members, with-members) =>
            methods = make-methods(l2, vname, members, false)
            A.s-variant(
              l2,
              constr-loc,
              vname,
              members.map(desugar-variant-member),
              (methods + with-members).map(desugar-member))
          | s-singleton-variant(l2, vname, with-members) =>
            methods = make-methods(l2, vname, [list: ], true)
            A.s-singleton-variant(
              l2,
              vname,
              (methods + with-members).map(desugar-member))
        end
      end
      A.s-data-expr(l, name, namet, params, mixins.map(desugar-expr), variants.map(extend-variant),
        shared.map(desugar-member), desugar-opt(desugar-expr, _check))
    | s-when(l, test, body) =>
      test-id = mk-id(l, "when-")
      check-bool(l, test-id, desugar-expr(test),
        A.s-if-else(l,
          [list: A.s-if-branch(l, test-id.id-e, A.s-block(l, [list: desugar-expr(body), gid(l, "nothing")]))],
          A.s-block(l, [list: gid(l, "nothing")])),
        bool-exn(test.l, "when", test-id.id-e))
    | s-if(l, branches) =>
      desugar-if(l, branches, A.s-block(l, [list: no-branches-exn(l, "if")]))
    | s-if-else(l, branches, _else) =>
      desugar-if(l, branches, _else)
    | s-if-pipe(l, branches) =>
      desugar-if(l, branches, A.s-block(l, [list: no-branches-exn(l, "ask")]))
    | s-if-pipe-else(l, branches, _else) =>
      desugar-if(l, branches, _else)
    | s-cases(l, typ, val, branches) =>
      desugar-cases(l, typ, desugar-expr(val), branches.map(desugar-case-branch),
        A.s-block(l, [list: no-branches-exn(l, "cases")]))
    | s-cases-else(l, typ, val, branches, _else) =>
      desugar-cases(l, typ, desugar-expr(val), branches.map(desugar-case-branch), desugar-expr(_else))
    | s-assign(l, id, val) => A.s-assign(l, id, desugar-expr(val))
    | s-dot(l, obj, field) => ds-curry-nullary(A.s-dot, l, obj, field)
    | s-extend(l, obj, fields) => A.s-extend(l, desugar-expr(obj), fields.map(desugar-member))
    | s-for(l, iter, bindings, ann, body) => 
      values = bindings.map(_.value).map(desugar-expr)
      the-function = A.s-lam(l, [list: ], bindings.map(_.bind).map(desugar-bind), desugar-ann(ann), "", desugar-expr(body), none)
      A.s-app(l, desugar-expr(iter), link(the-function, values))
    | s-op(l, op, left, right) =>
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
          if op == "op==":
            ds-curry-binop(l, desugar-expr(left), desugar-expr(right),
              lam(e1, e2):
                A.s-prim-app(l, "equiv", [list: e1, e2])
              end)
          else if op == "op<>":
            ds-curry-binop(l, desugar-expr(left), desugar-expr(right),
              lam(e1, e2):
                A.s-prim-app(l, "not", [list: A.s-prim-app(l, "equiv", [list: e1, e2])])
              end)
          else if op == "opor":
            fun helper(operands):
              or-oper = mk-id(l, "or-oper-")
              cases(List) operands.rest:
                | empty =>
                  check-bool(l, or-oper, desugar-expr(operands.first), or-oper.id-e,
                    bool-op-exn(operands.first.l, "second", "or", or-oper.id-e))
                | link(_, _) =>
                  check-bool(l, or-oper, desugar-expr(operands.first),
                    A.s-if-else(l,
                      [list: A.s-if-branch(l, or-oper.id-e, A.s-bool(l, true))],
                      helper(operands.rest)),
                    bool-op-exn(operands.first.l, "first", "or", or-oper.id-e))
              end
            end
            operands = collect-ors(expr)
            helper(operands)
          else if op == "opand":
            fun helper(operands):
              and-oper = mk-id(l, "and-oper-")
              cases(List) operands.rest:
                | empty =>
                  check-bool(l, and-oper, desugar-expr(operands.first), and-oper.id-e,
                    bool-op-exn(operands.first.l, "second", "and", and-oper.id-e))
                | link(_, _) =>
                  check-bool(l, and-oper, desugar-expr(operands.first),
                    A.s-if-else(l,
                      [list: A.s-if-branch(l, and-oper.id-e, helper(operands.rest))],
                      A.s-bool(l, false)),
                    bool-op-exn(operands.first.l, "first", "and", and-oper.id-e))
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
    | s-construct(l, modifier, constructor, elts) =>
      cases(A.ConstructModifier) modifier:
        | s-construct-normal =>
          A.s-app(l, desugar-expr(A.s-dot(l, constructor, "make")),
            [list: A.s-array(l, elts.map(desugar-expr))])
        | s-construct-lazy =>
          A.s-app(l, desugar-expr(A.s-dot(l, constructor, "lazy-make")),
            [list: A.s-array(l,
                  elts.map(lam(elt): desugar-expr(A.s-lam(elt.l, empty, empty, A.a-blank, "", elt, none)) end))])
      end
    | s-paren(l, e) => desugar-expr(e)
    # NOTE(joe): see preconditions; desugar-checks should have already happened
    | s-check(l, _, _, _) => A.s-str(l, "Checks should have been desugared")
    | s-check-test(l, _, _, _) => make-message-exception(l, "Checks should have been desugared")
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
    [list:  A.s-app(d, A.s-dot(d, A.s-id(d, A.s-name(d, "list")), "make"),
        [list:  A.s-array(d, [list: one, two, A.s-app(d, id("_plus"), [list: one, two])])])])

  prog3 = p("for map(elt from l): elt + 1 end")
  ds(prog3) is p("map(lam(elt): _plus(elt, 1) end, l)")
  
  # Some kind of bizarre parse error here
  # prog4 = p("(((5 + 1)) == 6) or o^f")
  #  ds(prog4) is p("builtins.equiv(5._plus(1), 6)._or(lam(): f(o) end)")
  
  # ds(p("(5)")) is ds(p("5"))
  
  # prog5 = p("cases(List) l: | empty => 5 + 4 | link(f, r) => 10 end")
  # dsed5 = ds(prog5)
  # cases-name = dsed5.stmts.first.binds.first.b.id.tostring()
  # compare = (cases-name + " = l " +
  #   cases-name + "._match({empty: lam(): 5._plus(4) end, link: lam(f, r): 10 end},
  #   lam(): raise('no cases matched') end)")
  # dsed5 is ds(p(compare))
  
end

