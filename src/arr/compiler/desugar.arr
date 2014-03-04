#lang pyret

provide *
import ast as A
import parse-pyret as PP
import "./compile-structs.arr" as C
import "./gensym.arr" as G
import "./ast-util.arr" as U

names = A.MakeName(0)

data DesugarEnv:
  | d-env(ids :: Set<String>, vars :: Set<String>, letrecs :: Set<String>)
end

data Pair:
  | pair(left, right)
end

mt-d-env = d-env(set([]), set([]), set([]))

fun extend-id(nv :: DesugarEnv, id :: A.Name):
  cases(A.Name) id:
    | s_name(s) => d-env(nv.ids.add(s), nv.vars, nv.letrecs)
    | else => nv
  end
end

fun extend-var(nv :: DesugarEnv, id :: A.Name):
  cases(A.Name) id:
    | s_name(s) => d-env(nv.ids, nv.vars.add(s), nv.letrecs)
    | else => nv
  end
end

fun extend-letrec(nv :: DesugarEnv, id :: A.Name):
  cases(A.Name) id:
    | s_name(s) => d-env(nv.ids, nv.vars, nv.letrecs.add(s))
    | else => nv
  end
end

fun desugar-header(h :: A.Header, b :: A.Expr):
  cases(A.Header) h:
    | s_provide_all(l) =>
      ids = A.block-ids(b)
      obj = A.s_obj(l, for map(id from ids): A.s_data_field(l, A.s_str(l, id), A.s_id(l, id)) end)
      A.s_provide(l, obj)
    | s_import(l, imp, name) =>
      cases(A.ImportType) imp:
        | s_file_import(file) =>
          if file.contains("/"): h
          else: A.s_import(l, A.s_file_import("./" + file), name)
          end
        | else => h
      end
    | else => h
  end
end

fun desugar(program :: A.Program, compile-env :: C.CompileEnvironment):
  doc: "Desugar non-scope and non-check based constructs.
        Preconditions on program:
          - well-formed
          - contains no s_provide in headers
          - contains no s_let, s_var, s_data, s_check, or s_check_test
          - all where blocks are none
        Postconditions on program:
          - in addition to preconditions,
            contains no s_for, s_if, s_op, s_method_field,
                        s_cases, s_left_app, s_not, s_when, s_if_pipe, s_list
                        s_paren
          - contains no s_underscore in expression position (but it may
            appear in binding positions as in s_let_bind, s_letrec_bind)"
  cases(A.Program) program:
    | s_program(l, headers, body) =>
      A.s_program(l, headers, desugar-expr(mt-d-env, body))
    | else => raise("Attempt to desugar non-program: " + torepr(program))
  end
end

fun mk-bind(l, id): A.s_bind(l, false, id, A.a_blank);

fun mk-id(loc, base):
  a = names.s_name(base)
  { id: a, id-b: mk-bind(loc, a), id-e: A.s_id(loc, a) }
end

fun make-torepr(l, vname, fields):
  self = mk-id(l, "self")
  fun str(s): A.s_str(l, s) end
  fun call-torepr(val):
    A.s_app(l, A.s_id(l, A.s_name("torepr")), [A.s_dot(l, self.id-e, val.bind.id.tostring())])
  end
  fun concat(v1, v2):
    A.s_op(l, "op+", v1, v2)
  end
  argstrs = cases(List) fields:
    | empty => str("")
    | link(f, r) =>
      r.foldl(
          fun(val, acc): concat(acc, concat(str(","), call-torepr(val))) end,
          call-torepr(f)
        )
  end
  A.s_method(l, [self.id-b], A.a_blank, "",
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
        | s_variant_member(l2, mtype, bind) =>
          when mtype <> A.s_normal:
            raise("Non-normal member in variant, NYI: " + torepr(f))
          end
          A.s_dot(l2, self-id.id-e, bind.id)
      end
    end
  A.s_method(l, [self-id, cases-id, else-id].map(_.id-b), A.a_blank, "",
      A.s_if_else(l, [
          A.s_if_branch(l,
              A.s_app(
                  l,
                  A.s_dot(l, A.s_id(l, A.s_name("builtins")), "has-field"),
                  [cases-id.id-e, A.s_str(l, case-name)]
                ),
              A.s_let_expr(l, [A.s_let_bind(l, call-match-case.id-b, A.s_dot(l, cases-id.id-e, case-name))],
                  A.s_app(l, call-match-case.id-e, args)
                )
            )
        ],
        A.s_app(l, else-id.id-e, [])),
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

fun desugar-if-branch(nv :: DesugarEnv, expr :: A.IfBranch):
  cases(A.IfBranch) expr:
    | s_if_branch(l, test, body) =>
      A.s_if_branch(l, desugar-expr(nv, test), desugar-expr(nv, body))
  end
end

fun desugar-if-pipe-branch(nv :: DesugarEnv, expr :: A.IfPipeBranch):
  cases(A.IfPipeBranch) expr:
    | s_if_pipe_branch(l, test, body) =>
      A.s_if_branch(l, desugar-expr(nv, test), desugar-expr(nv, body))
  end
end

fun desugar-case-branch(nv, c):
  cases(A.CasesBranch) c:
    | s_cases_branch(l2, name, args, body) =>  
      desugar-member(nv,
        A.s_data_field(l2, A.s_str(l2, name), A.s_lam(l2, [], args, A.a_blank, "", body, none)))
  end
end
fun desugar-cases(l, ann, val, branches, else-block):
  val-id = mk-id(l, "cases-val")
  cases-object = A.s_obj(l, branches)
  else-thunk = A.s_lam(l, [], [], A.a_blank, "", else-block, none)
  A.s_let_expr(l, [
        A.s_let_bind(l, val-id.id-b, val)
      ],
      A.s_app(l, A.s_dot(l, val-id.id-e, "_match"), [cases-object, else-thunk])
    )
where:
  d = A.dummy-loc
  prog = desugar-cases(
      d,
      A.a_blank, 
      A.s_num(d, 1),
      [
        A.s_data_field(d, A.s_str(d, "empty"), A.s_lam(d, [], [], A.a_blank, "", A.s_num(d, 5), none))
      ],
      A.s_num(d, 4)
    )
  id = prog.binds.first.b
    
  prog satisfies A.equiv-ast(_, A.s_let_expr(d, [
          A.s_let_bind(d, id, A.s_num(d, 1))
        ],
        A.s_app(d, A.s_dot(d, A.s_id(d, id.id), "_match"), [
          A.s_obj(d, [
              A.s_data_field(d, A.s_str(d, "empty"), A.s_lam(d, [], [], A.a_blank, "", A.s_num(d, 5), none))
            ]),
          A.s_lam(d, [], [], A.a_blank, "", A.s_num(d, 4), none)])))

end

fun desugar-member(nv, f): 
  cases(A.Member) f:
    | s_method_field(l, name, args, ann, doc, body, _check) =>
      A.s_data_field(l, desugar-expr(nv, name), desugar-expr(nv, A.s_method(l, args, ann, doc, body, _check)))
    | s_data_field(l, name, value) =>
      A.s_data_field(l, desugar-expr(nv, name), desugar-expr(nv, value))
    | else =>
      raise("NYI(desugar-member): " + torepr(f))
  end
end

fun is-underscore(e):
  A.is-s_id(e) and (e.id == A.s_underscore)
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

fun ds-curry-nullary(rebuild-node, nv, l, obj, m):
  if is-underscore(obj):
    curried-obj = mk-id(l, "recv-")
    A.s_lam(l, [], [curried-obj.id-b], A.a_blank, "", rebuild-node(l, curried-obj.id-e, m), none)
  else:
    rebuild-node(l, desugar-expr(nv, obj), m)
  end
where:
  nothing
  #d = A.dummy-loc
  #ds-ed = ds-curry-nullary(A.s_dot, d, A.s_id(d, "_"), A.s_id(d, "x"))
#  ds-ed satisfies
end

fun ds-curry-binop(s, e1, e2, rebuild):
  params-and-args = ds-curry-args(s, [e1, e2])
  params = params-and-args.left
  cases(List) params:
    | empty => rebuild(e1, e2)
    | link(f, r) =>
      curry-args = params-and-args.right
      A.s_lam(s, [], params, A.a_blank, "", rebuild(curry-args.first, curry-args.rest.first), none)
  end
end

fun ds-curry(nv, l, f, args):
  fun fallthrough():
    params-and-args = ds-curry-args(l, args)
    params = params-and-args.left
    ds-f = desugar-expr(nv, f)
    if is-empty(params): A.s_app(l, ds-f, args)
    else: A.s_lam(l, [], params, A.a_blank, "", A.s_app(l, ds-f, params-and-args.right), none)
    end
  end
  cases(A.Expr) f:
    | s_dot(l2, obj, m) =>
      if is-underscore(obj):
        curried-obj = mk-id(l, "recv-")
        params-and-args = ds-curry-args(l, args)
        params = params-and-args.left
        A.s_lam(l, [], link(curried-obj.id-b, params), A.a_blank, "",
            A.s_app(l, A.s_dot(l, curried-obj.id-e, m), params-and-args.right), none)
      else:
        fallthrough()
      end
    | else => fallthrough()
  end
where:
  d = A.dummy-loc
  n = A.s_name
  id = fun(s): A.s_id(d, A.s_name(s));
  under = A.s_id(d, A.s_underscore)
  ds-ed = ds-curry(
      mt-d-env,
      d,
      id("f"),
      [ under, id("x") ]
    )
  ds-ed satisfies A.is-s_lam
  ds-ed.args.length() is 1

  ds-ed2 = ds-curry(
      mt-d-env,
      d,
      id("f"),
      [ under, under ]
    )
  ds-ed2 satisfies A.is-s_lam
  ds-ed2.args.length() is 2

  ds-ed3 = ds-curry(
      mt-d-env,
      d,
      id("f"),
      [
        id("x"),
        id("y")
      ]
    )
  ds-ed3 satisfies A.equiv-ast(_, A.s_app(d, id("f"), [id("x"), id("y")]))
    
  ds-ed4 = ds-curry(
      mt-d-env,
      d,
      A.s_dot(d, under, "f"),
      [
        id("x")
      ])
  ds-ed4 satisfies A.is-s_lam
  ds-ed4.args.length() is 1
        
end

fun<T> desugar-opt(f :: (DesugarEnv, T -> T), nv :: DesugarEnv, opt :: Option<T>):
  cases(Option) opt:
    | none => none
    | some(e) => some(f(nv, e))
  end
end

fun desugar-expr(nv :: DesugarEnv, expr :: A.Expr):
  cases(A.Expr) expr:
    | s_block(l, stmts) =>
      A.s_block(l, stmts.map(desugar-expr(nv, _)))
    | s_user_block(l, body) =>
      desugar-expr(nv, body)
    | s_app(l, f, args) =>
      ds-curry(nv, l, f, args.map(desugar-expr(nv, _)))
    | s_left_app(l, o, f, args) =>
      ds-curry(nv, l, f, ([o] + args).map(desugar-expr(nv, _)))
    | s_lam(l, params, args, ann, doc, body, _check) =>
      new-env = for fold(nv2 from nv, arg from args): extend-id(nv2, arg.id) end
      A.s_lam(l, params, args, ann, doc, desugar-expr(new-env, body), desugar-opt(desugar-expr, nv, _check))
    | s_method(l, args, ann, doc, body, _check) =>
      new-env = for fold(nv2 from nv, arg from args): extend-id(nv2, arg.id) end
      A.s_method(l, args, ann, doc, desugar-expr(new-env, body), desugar-opt(desugar-expr, nv, _check))
    | s_let_expr(l, binds, body) =>
      new-binds = for fold(b-e from { b: [], e: nv }, bind from binds):
        cases(A.LetBind) bind:
          | s_let_bind(l2, b, val) =>
            new-env = b-e.e^extend-id(b.id)
            new-val = desugar-expr(b-e.e, val)
            { b: link(A.s_let_bind(l2, b, new-val), b-e.b),
              e: new-env }
          | s_var_bind(l2, b, val) =>
            new-env = b-e.e^extend-var(b.id)
            new-val = desugar-expr(b-e.e, val)
            { b: link(A.s_var_bind(l2, b, new-val), b-e.b),
              e: new-env }
        end
      end
      A.s_let_expr(l, new-binds.b.reverse(), desugar-expr(new-binds.e, body))
    | s_letrec(l, binds, body) =>
      letrec-ids = binds.map(_.b).map(_.id)
      new-env = for fold(acc from nv, id from letrec-ids):
          acc^extend-letrec(id)
        end
      new-binds = for map(bind from binds):
          cases(A.LetrecBind) bind:
            | s_letrec_bind(l2, b, val) =>
              A.s_letrec_bind(l2, b, desugar-expr(new-env, val))
          end
        end
      A.s_letrec(l, new-binds, desugar-expr(new-env, body))
    | s_data_expr(l, name, params, mixins, variants, shared, _check) =>
      fun extend-variant(v):
        cases(A.Variant) v:
          | s_variant(l2, vname, members, with-members) =>
            m = A.s_data_field(l2, A.s_str(l2, "_match"), make-match(l2, vname, members))
            tr = A.s_data_field(l2, A.s_str(l2, "_torepr"), make-torepr(l2, vname, members))
            A.s_variant(l2, vname, members, ([m, tr] + with-members).map(desugar-member(nv, _)))
          | s_singleton_variant(l2, vname, with-members) =>
            m = A.s_data_field(l2, A.s_str(l2, "_match"), make-match(l2, vname, []))
            tr = A.s_data_field(l2, A.s_str(l2, "_torepr"), make-torepr(l2, vname, []))
            A.s_singleton_variant(l2, vname, ([m, tr] + with-members).map(desugar-member(nv, _)))
        end
      end
      A.s_data_expr(l, name, params, mixins.map(desugar-expr(nv, _)), variants.map(extend-variant),
        shared.map(desugar-member(nv, _)), desugar-opt(desugar-expr, nv, _check))
    | s_not(l, test) => 
      A.s_if_else(l, [A.s_if_branch(l, desugar-expr(nv, test), A.s_block(l, [A.s_bool(l, false)]))], A.s_block(l, [A.s_bool(l, true)]))
    | s_when(l, test, body) =>
      A.s_if_else(l, [A.s_if_branch(l, desugar-expr(nv, test), A.s_block(l, [desugar-expr(nv, body), A.s_id(l, A.s_name("nothing"))]))], A.s_block(l, [A.s_id(l, A.s_name("nothing"))]))
    | s_if(l, branches) =>
      raise("If must have else for now")
    | s_if_else(l, branches, _else) =>
      A.s_if_else(l, branches.map(desugar-if-branch(nv, _)), desugar-expr(nv, _else))
    | s_if_pipe(l, branches) =>
      raise("If-pipe must have else for now")
    | s_if_pipe_else(l, branches, _else) =>
      A.s_if_else(l, branches.map(desugar-if-pipe-branch(nv, _)), desugar-expr(nv, _else))
    | s_cases(l, type, val, branches) =>
      desugar-cases(l, type, desugar-expr(nv, val), branches.map(desugar-case-branch(nv, _)), A.s_block(l, [A.s_app(l, A.s_id(l, A.s_name("raise")), [A.s_str(l, "no cases matched")])]))
    | s_cases_else(l, type, val, branches, _else) =>
      desugar-cases(l, type, desugar-expr(nv, val), branches.map(desugar-case-branch(nv, _)), desugar-expr(nv, _else))
    | s_assign(l, id, val) => A.s_assign(l, id, desugar-expr(nv, val))
    | s_dot(l, obj, field) => ds-curry-nullary(A.s_dot, nv, l, obj, field)
    | s_colon(l, obj, field) => ds-curry-nullary(A.s_colon, nv, l, obj, field)
    | s_extend(l, obj, fields) => A.s_extend(l, desugar-expr(nv, obj), fields.map(desugar-member(nv, _)))
    | s_for(l, iter, bindings, ann, body) => 
      values = bindings.map(_.value).map(desugar-expr(nv, _))
      the-function = A.s_lam(l, [], bindings.map(_.bind), ann, "", desugar-expr(nv, body), none)
      A.s_app(l, desugar-expr(nv, iter), link(the-function, values))
    | s_op(l, op, left, right) =>
      cases(Option) get-arith-op(op):
        | some(field) => A.s_app(l, A.s_dot(l, desugar-expr(nv, left), field), [desugar-expr(nv, right)])
        | none =>
          fun thunk(e): A.s_lam(l, [], [], A.a_blank, "", A.s_block(l, [e]), none) end
          fun opbool(fld):
            A.s_app(l, A.s_dot(l, desugar-expr(nv, left), fld), [thunk(desugar-expr(nv, right))])
          end
          if op == "op==":
            ds-curry-binop(l, desugar-expr(nv, left), desugar-expr(nv, right),
                fun(e1, e2):
                  A.s_app(l, A.s_dot(l, A.s_id(l, A.s_name("builtins")), "equiv"), [e1, e2])
                end)
          else if op == "op<>":
            A.s_if_else(l,
              [A.s_if_branch(l,
                ds-curry-binop(l, desugar-expr(nv, left), desugar-expr(nv, right),
                    fun(e1, e2):
                      A.s_app(l, A.s_dot(l, A.s_id(l, A.s_name("builtins")), "equiv"), [e1, e2])
                    end),
                A.s_bool(l, false))],
              A.s_bool(l, true))
          else if op == "opor": opbool("_or")
          else if op == "opand": opbool("_and")
          else:
            raise("Only arith ops so far, " + op + " did not match")
          end
      end
    | s_id(l, x) =>
      cases(A.Name) x:
        | s_name(s) =>
          if nv.vars.member(s): A.s_id_var(l, x)
          else if nv.letrecs.member(s): A.s_id_letrec(l, x)
          else: expr
          end
        | else => A.s_id(l, x)
      end
    | s_num(_, _) => expr
    | s_str(_, _) => expr
    | s_bool(_, _) => expr
    | s_obj(l, fields) => A.s_obj(l, fields.map(desugar-member(nv, _)))
    | s_list(l, elts) =>
      elts.foldr(fun(elt, list-expr):
          A.s_app(
              l,
              desugar-expr(nv, A.s_id(l, A.s_name("link"))),
              [desugar-expr(nv, elt), list-expr]
            )
        end,
        desugar-expr(nv, A.s_id(l, A.s_name("empty"))))
    | s_paren(l, e) => desugar-expr(nv, e)
    # TODO(joe): skipping checks for now, they should be unreachable
    | s_check(l, _, _) => A.s_str(l, "check mode not yet working (check block)")
    | s_check_test(l, _, _, _) => A.s_app(l, A.s_id(l, A.s_name("raise")), [A.s_str(l, "check mode not yet working (test stmt)")])
    | else => raise("NYI (desugar): " + torepr(expr))
  end
where:
  p = fun(str): PP.surface-parse(str, "test").block;
  d = A.dummy-loc
  ds = desugar-expr(mt-d-env, _)
  one = A.s_num(d, 1)
  two = A.s_num(d, 2)
  equiv = fun(e): A.equiv-ast(_, e) end

  prog2 = p("[1,2,1 + 2]")
  ds(prog2) satisfies
    equiv(p("link(1, link(2, link(1._plus(2), empty)))"))

  prog3 = p("for map(elt from l): elt + 1 end")
  ds(prog3) satisfies
    equiv(p("map(fun(elt): elt._plus(1) end, l)"))

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

  prog6 = p("when false: dostuff() end")
  compare6 = ds(p("if false: block: dostuff() end nothing else: nothing end"))
  ds(prog6) satisfies equiv(compare6)

  prog7 = p("not true")
  compare7 = ds(p("if true: false else: true end"))
  ds(prog7) satisfies equiv(compare7)

end

