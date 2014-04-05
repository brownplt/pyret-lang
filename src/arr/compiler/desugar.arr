#lang pyret

provide *
import ast as A
import parse-pyret as PP
import "./compile-structs.arr" as C
import "./ast-util.arr" as U

names = A.MakeName(0)

data DesugarEnv:
  | d-env(ids :: Set<String>, vars :: Set<String>, letrecs :: Set<String>)
end

data Pair:
  | pair(left, right)
end

mt-d-env = d-env(set([]), set([]), set([]))

fun check-bool(l, id, e, then, err):
  A.s-let-expr(l, [A.s-let-bind(l, id.id-b, e)],
    A.s-if-else(l,
      [A.s-if-branch(l, A.s-prim-app(l, "isBoolean", [id.id-e]), then)],
      err))
end
fun make-message-exception(l, msg):
  A.s-prim-app(l, "raise", [A.s-str(l, msg)])
end


fun desugar-ann(a :: A.Ann) -> A.Ann:
  cases(A.Ann) a:
    | a-blank => a
    | a-any => a
    | a-name(_, _) => a
    | a-dot(_, _, _) => a
    | a-arrow(l, args, ret) =>
      A.a-arrow(l, args.map(desugar-ann), desugar-ann(ret))
    | a-method(l, args, ret) =>
      A.a-arrow(l, args.map(desugar-ann), desugar-ann(ret))
    | a-app(l, base, args) =>
      A.a-app(l, desugar-ann(base), args.map(desugar-ann))
    | a-record(l, fields) =>
      A.a-record(l, for map(f from fields):
        A.a-field(l, f.name, desugar-ann(f.ann))
      end)
    | a-pred(l, ann, exp) =>
      A.a-pred(l, desugar-ann(ann), desugar-expr(exp))
  end
end

fun desugar(program :: A.Program, compile-env :: C.CompileEnvironment):
  doc: "Desugar non-scope and non-check based constructs.
        Preconditions on program:
          - well-formed
          - contains no s-let, s-var, s-data, s-check, or s-check-test
          - contains no s-provide in headers
          - all where blocks are none
          - contains no s-name (e.g. call resolve-names first)
        Postconditions on program:
          - in addition to preconditions,
            contains no s-for, s-if, s-op, s-method-field,
                        s-cases, s-left-app, s-not, s-when, s-if-pipe, s-list
                        s-paren
          - contains no s-underscore in expression position (but it may
            appear in binding positions as in s-let-bind, s-letrec-bind)"
  cases(A.Program) program:
    | s-program(l, _provide, imports, body) =>
      A.s-program(l, _provide, imports, desugar-expr(body))
    | else => raise("Attempt to desugar non-program: " + torepr(program))
  end
end

fun mk-bind(l, id): A.s-bind(l, false, id, A.a-blank);

fun mk-id(loc, base):
  a = names.make-atom(base)
  { id: a, id-b: mk-bind(loc, a), id-e: A.s-id(loc, a) }
end

fun make-torepr(l, vname, fields):
  self = mk-id(l, "self")
  fun str(s): A.s-str(l, s) end
  fun call-torepr(val):
    A.s-app(l, A.s-id(l, A.s-global("torepr")), [A.s-dot(l, self.id-e, val.bind.id.toname())])
  end
  fun concat(v1, v2):
    A.s-op(l, "op+", v1, v2)
  end
  argstrs = cases(List) fields:
    | empty => str("")
    | link(f, r) =>
      r.foldl(
          fun(val, acc): concat(acc, concat(str(","), call-torepr(val))) end,
          call-torepr(f)
        )
  end
  A.s-method(l, [self.id-b], A.a-blank, "",
    concat(str(vname), concat(str("("), concat(argstrs, str(")")))),
    none)
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
  A.s-method(l, [self-id, cases-id, else-id].map(_.id-b), A.a-blank, "",
      A.s-if-else(l, [
          A.s-if-branch(l,
              A.s-prim-app(
                  l,
                  "hasField",
                  [cases-id.id-e, A.s-str(l, case-name)]
                ),
              A.s-let-expr(l, [A.s-let-bind(l, call-match-case.id-b, A.s-dot(l, cases-id.id-e, case-name))],
                  A.s-app(l, call-match-case.id-e, args)
                )
            )
        ],
        A.s-app(l, else-id.id-e, [])),
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
        [A.s-if-branch(branch.l, test-id.id-e, desugar-expr(branch.body))],
        acc),
      make-message-exception(l, "Pyret Type Error: Condition for 'if' was not a boolean"))
  end
end

fun desugar-case-branch(c):
  cases(A.CasesBranch) c:
    | s-cases-branch(l2, name, args, body) =>  
      desugar-member(
        A.s-data-field(
          l2,
          A.s-str(l2, name),
          A.s-lam(l2, [], args.map(desugar-bind), A.a-blank, "", body, none)))
  end
end

fun desugar-cases(l, ann, val, branches, else-block):
  val-id = mk-id(l, "cases-val")
  cases-object = A.s-obj(l, branches)
  else-thunk = A.s-lam(l, [], [], A.a-blank, "", else-block, none)
  A.s-let-expr(l, [
        A.s-let-bind(l, val-id.id-b, val)
      ],
      A.s-app(l, A.s-dot(l, val-id.id-e, "_match"), [cases-object, else-thunk])
    )
where:
  d = A.dummy-loc
  prog = desugar-cases(
      d,
      A.a-blank, 
      A.s-num(d, 1),
      [
        A.s-data-field(d, A.s-str(d, "empty"), A.s-lam(d, [], [], A.a-blank, "", A.s-num(d, 5), none))
      ],
      A.s-num(d, 4)
    )
  id = prog.binds.first.b
    
  prog satisfies A.equiv-ast(_, A.s-let-expr(d, [
          A.s-let-bind(d, id, A.s-num(d, 1))
        ],
        A.s-app(d, A.s-dot(d, A.s-id(d, id.id), "_match"), [
          A.s-obj(d, [
              A.s-data-field(d, A.s-str(d, "empty"), A.s-lam(d, [], [], A.a-blank, "", A.s-num(d, 5), none))
            ]),
          A.s-lam(d, [], [], A.a-blank, "", A.s-num(d, 4), none)])))

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
  A.is-s-id(e) and (e.id == A.s-underscore)
end

fun ds-curry-args(l, args):
  params-and-args = for fold(acc from pair([], []), arg from args):
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
    A.s-lam(l, [], [curried-obj.id-b], A.a-blank, "", rebuild-node(l, curried-obj.id-e, m), none)
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
  params-and-args = ds-curry-args(s, [e1, e2])
  params = params-and-args.left
  cases(List) params:
    | empty => rebuild(e1, e2)
    | link(f, r) =>
      curry-args = params-and-args.right
      A.s-lam(s, [], params, A.a-blank, "", rebuild(curry-args.first, curry-args.rest.first), none)
  end
end

fun ds-curry(l, f, args):
  fun fallthrough():
    params-and-args = ds-curry-args(l, args)
    params = params-and-args.left
    ds-f = desugar-expr(f)
    if is-empty(params): A.s-app(l, ds-f, args)
    else: A.s-lam(l, [], params, A.a-blank, "", A.s-app(l, ds-f, params-and-args.right), none)
    end
  end
  cases(A.Expr) f:
    | s-dot(l2, obj, m) =>
      if is-underscore(obj):
        curried-obj = mk-id(l, "recv-")
        params-and-args = ds-curry-args(l, args)
        params = params-and-args.left
        A.s-lam(l, [], link(curried-obj.id-b, params), A.a-blank, "",
            A.s-app(l, A.s-dot(l, curried-obj.id-e, m), params-and-args.right), none)
      else:
        fallthrough()
      end
    | else => fallthrough()
  end
where:
  d = A.dummy-loc
  n = A.s-global
  id = fun(s): A.s-id(d, A.s-global(s));
  under = A.s-id(d, A.s-underscore)
  ds-ed = ds-curry(
      d,
      id("f"),
      [ under, id("x") ]
    )
  ds-ed satisfies A.is-s-lam
  ds-ed.args.length() is 1

  ds-ed2 = ds-curry(
      d,
      id("f"),
      [ under, under ]
    )
  ds-ed2 satisfies A.is-s-lam
  ds-ed2.args.length() is 2

  ds-ed3 = ds-curry(
      d,
      id("f"),
      [
        id("x"),
        id("y")
      ]
    )
  ds-ed3 satisfies A.equiv-ast(_, A.s-app(d, id("f"), [id("x"), id("y")]))
    
  ds-ed4 = ds-curry(
      d,
      A.s-dot(d, under, "f"),
      [
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
    | s-block(l, stmts) =>
      A.s-block(l, stmts.map(desugar-expr))
    | s-user-block(l, body) =>
      desugar-expr(body)
    | s-app(l, f, args) =>
      ds-curry(l, f, args.map(desugar-expr))
    | s-prim-app(l, f, args) =>
      A.s-prim-app(l, f, args.map(desugar-expr))
    | s-left-app(l, o, f, args) =>
      ds-curry(l, f, ([o] + args).map(desugar-expr))
    | s-lam(l, params, args, ann, doc, body, _check) =>
      A.s-lam(l, params, args.map(desugar-bind), desugar-ann(ann), doc, desugar-expr(body), desugar-opt(desugar-expr, _check))
    | s-method(l, args, ann, doc, body, _check) =>
      A.s-method(l, args.map(desugar-bind), desugar-ann(ann), doc, desugar-expr(body), desugar-opt(desugar-expr, _check))
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
    | s-data-expr(l, name, params, mixins, variants, shared, _check) =>
      fun extend-variant(v):
        cases(A.Variant) v:
          | s-variant(l2, vname, members, with-members) =>
            m = A.s-data-field(l2, A.s-str(l2, "_match"), make-match(l2, vname, members))
            tr = A.s-data-field(l2, A.s-str(l2, "_torepr"), make-torepr(l2, vname, members))
            A.s-variant(
              l2,
              vname,
              members.map(desugar-variant-member(_)),
              ([m, tr] + with-members).map(desugar-member(_)))
          | s-singleton-variant(l2, vname, with-members) =>
            m = A.s-data-field(l2, A.s-str(l2, "_match"), make-match(l2, vname, []))
            tr = A.s-data-field(l2, A.s-str(l2, "_torepr"), make-torepr(l2, vname, []))
            A.s-singleton-variant(
              l2,
              vname,
              ([m, tr] + with-members).map(desugar-member(_)))
        end
      end
      A.s-data-expr(l, name, params, mixins.map(desugar-expr), variants.map(extend-variant),
        shared.map(desugar-member), desugar-opt(desugar-expr, _check))
    | s-not(l, test) =>
      not-oper = mk-id(l, "not-oper-")
      check-bool(l, not-oper, desugar-expr(test),
        A.s-if-else(l,
          [A.s-if-branch(l, not-oper.id-e, A.s-block(l, [A.s-bool(l, false)]))],
          A.s-block(l, [A.s-bool(l, true)])),
        make-message-exception(l, "Pyret Type Error: Argument to 'not' was not a boolean"))
    | s-when(l, test, body) =>
      test-id = mk-id(l, "when-")
      check-bool(l, test-id, desugar-expr(test),
        A.s-if-else(l,
          [A.s-if-branch(l, test-id.id-e, A.s-block(l, [desugar-expr(body), A.s-id(l, A.s-global("nothing"))]))],
          A.s-block(l, [A.s-id(l, A.s-global("nothing"))])),
        make-message-exception(l, "Pyret Type Error: Condition for 'when' was not a boolean"))
    | s-if(l, branches) =>
      raise("If must have else for now")
    | s-if-else(l, branches, _else) =>
      desugar-if(l, branches, _else)
    | s-if-pipe(l, branches) =>
      raise("If-pipe must have else for now")
    | s-if-pipe-else(l, branches, _else) =>
      desugar-if(l, branches, _else)
    | s-cases(l, type, val, branches) =>
      desugar-cases(l, type, desugar-expr(val), branches.map(desugar-case-branch), A.s-block(l, [make-message-exception(l, "No cases matched")]))
    | s-cases-else(l, type, val, branches, _else) =>
      desugar-cases(l, type, desugar-expr(val), branches.map(desugar-case-branch), desugar-expr(_else))
    | s-assign(l, id, val) => A.s-assign(l, id, desugar-expr(val))
    | s-dot(l, obj, field) => ds-curry-nullary(A.s-dot, l, obj, field)
    | s-colon(l, obj, field) => ds-curry-nullary(A.s-colon, l, obj, field)
    | s-extend(l, obj, fields) => A.s-extend(l, desugar-expr(obj), fields.map(desugar-member))
    | s-for(l, iter, bindings, ann, body) => 
      values = bindings.map(_.value).map(desugar-expr)
      the-function = A.s-lam(l, [], bindings.map(_.bind).map(desugar-bind), desugar-ann(ann), "", desugar-expr(body), none)
      A.s-app(l, desugar-expr(iter), link(the-function, values))
    | s-op(l, op, left, right) =>
      cases(Option) get-arith-op(op):
        | some(field) =>
          A.s-app(l, A.s-id(l, A.s-global(field)), [desugar-expr(left), desugar-expr(right)])
        | none =>
          fun thunk(e): A.s-lam(l, [], [], A.a-blank, "", A.s-block(l, [e]), none) end
          fun opbool(fld):
            A.s-app(l, A.s-dot(l, desugar-expr(left), fld), [thunk(desugar-expr(right))])
          end
          fun collect-op(opname, exp):
            if A.is-s-op(exp):
              if exp.op == opname: collect-op(opname, exp.left) + collect-op(opname, exp.right)
              else: [exp]
              end
            else: [exp]
            end
          end
          collect-ors = collect-op("opor", _)
          collect-ands = collect-op("opand", _)
          if op == "op==":
            ds-curry-binop(l, desugar-expr(left), desugar-expr(right),
                fun(e1, e2):
                  A.s-prim-app(l, "equiv", [e1, e2])
                end)
          else if op == "op<>":
            A.s-if-else(l,
              [A.s-if-branch(l,
                ds-curry-binop(l, desugar-expr(left), desugar-expr(right),
                    fun(e1, e2):
                      A.s-prim-app(l, "equiv", [e1, e2])
                    end),
                  A.s-bool(l, false))],
              A.s-bool(l, true))
          else if op == "opor":
            fun helper(operands):
              or-oper = mk-id(l, "or-oper-")
              cases(List) operands.rest:
                | empty =>
                  check-bool(l, or-oper, desugar-expr(operands.first), or-oper.id-e,
                    make-message-exception(l, "Pyret Type Error: Second argument to 'or' was not a boolean"))
                | link(_, _) =>
                  check-bool(l, or-oper, desugar-expr(operands.first),
                    A.s-if-else(l,
                      [A.s-if-branch(l, or-oper.id-e, A.s-bool(l, true))],
                      helper(operands.rest)),
                    make-message-exception(l, "Pyret Type Error: First argument 'or' was not a boolean"))
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
                    make-message-exception(l, "Pyret Type Error: Second argument to 'and' was not a boolean"))
                | link(_, _) =>
                  check-bool(l, and-oper, desugar-expr(operands.first),
                    A.s-if-else(l,
                      [A.s-if-branch(l, and-oper.id-e, helper(operands.rest))],
                      A.s-bool(l, false)),
                    make-message-exception(l, "Pyret Type Error: First argument 'and' was not a boolean"))
              end
            end
            operands = collect-ands(expr)
            helper(operands)
          else:
            raise("No implementation for " + op)
          end
      end
    | s-id(l, x) => expr
    | s-id-var(l, x) => expr
    | s-id-letrec(l, x) => expr
    | s-num(_, _) => expr
    | s-str(_, _) => expr
    | s-bool(_, _) => expr
    | s-obj(l, fields) => A.s-obj(l, fields.map(desugar-member))
    | s-list(l, elts) =>
      elts.foldr(fun(elt, list-expr):
          A.s-app(
              l,
              A.s-id(l, A.s-global("_link")),
              [desugar-expr(elt), list-expr]
            )
        end,
        desugar-expr(A.s-id(l, A.s-global("_empty"))))
    | s-paren(l, e) => desugar-expr(e)
    # NOTE(joe): see preconditions; desugar-checks should have already happened
    | s-check(l, _, _) => A.s-str(l, "Checks should have been desugared")
    | s-check-test(l, _, _, _) => make-message-exception(l, "Checks should have been desugared")
    | else => raise("NYI (desugar): " + torepr(expr))
  end
where:
  p = fun(str): PP.surface-parse(str, "test").block;
  d = A.dummy-loc
  ds = desugar-expr
  one = A.s-num(d, 1)
  two = A.s-num(d, 2)
  equiv = fun(e): A.equiv-ast(_, e) end

  "no tests right now"

#  prog2 = p("[1,2,1 + 2]")
#  ds(prog2) satisfies
#    equiv(p("_link(1, _link(2, _link(_plus(1, 2), _empty)))"))

#  prog3 = p("for map(elt from l): elt + 1 end")
#  ds(prog3) satisfies
#    equiv(p("map(fun(elt): _plus(elt, 1) end, l)"))

# Some kind of bizarre parse error here
#  prog4 = p("(((5 + 1)) == 6) or o^f()")
#  ds(prog4) satisfies
#    equiv(p("builtins.equiv(5._plus(1), 6)._or(fun(): f(o) end)"))

#  ds(p("(5)")) satisfies equiv(ds(p("5")))

#  prog5 = p("cases(List) l: | empty => 5 + 4 | link(f, r) => 10 end")
#  dsed5 = ds(prog5)
#  cases-name = dsed5.stmts.first.binds.first.b.id
#  compare = (cases-name + " = l " +
#             cases-name + "._match({empty: fun(): 5._plus(4) end, link: fun(f, r): 10 end},
#                                   fun(): raise('no cases matched') end)")
#  dsed5 satisfies equiv(ds(p(compare)))

end

