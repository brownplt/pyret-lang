#lang pyret

provide *
import ast as A
import "compile-structs.arr" as C
import "gensym.arr" as G

data DesugarEnv:
  | d-env(ids :: Set<String>, vars :: Set<String>, letrecs :: Set<String>)
end

data Pair:
  | pair(left, right)
end

mt-d-env = d-env(set([]), set([]), set([]))

fun extend-id(nv :: DesugarEnv, id :: String):
  d-env(nv.ids.add(id), nv.vars, nv.letrecs)
end

fun extend-var(nv :: DesugarEnv, id :: String):
  d-env(nv.ids, nv.vars.add(id), nv.letrecs)
end

fun extend-letrec(nv :: DesugarEnv, id :: String):
  d-env(nv.ids, nv.vars, nv.letrecs.add(id))
end

fun is-binder(stmt):
  preds = [A.is-s_let, A.is-s_var, A.is-s_fun, A.is-s_data, A.is-s_graph]
  for list.any(f from preds): f(stmt) end
end

fun desugar-header(h :: A.Header, b :: A.Expr):
  cases(A.Header) h:
    | s_provide_all(l) =>
      ids = A.block-ids(b)
      obj = A.s_obj(l, for map(id from ids): A.s_data_field(l, A.s_str(l, id), A.s_id(l, id)) end)
      A.s_provide(l, obj)
    | else => h
  end
end

fun wrap-env-imports(l, expr :: A.Expr, env :: C.CompileEnvironment):
  cases(C.CompileEnvironment) env:
    | compile-env(modules) =>
      let-binds = for fold(lst from [], k from modules.keys()):
          lst +
            for map(name from modules.get(k)):
              A.s_let(l, A.s_bind(l, false, name, A.a_blank), A.s_dot(l, A.s_id(l, k), name))
            end
        end
      A.s_block(l, let-binds + [expr])
  end
end

fun desugar(program :: A.Program, compile-env :: C.CompileEnvironment):
  cases(A.Program) program:
    | s_program(l, headers-raw, body) =>
      headers = headers-raw.map(desugar-header(_, body))
      str = A.s_str(l, _)
      provides = headers.filter(A.is-s_provide)
      len = provides.length()
      prov = if len == 0:
        A.s_obj(l, [])
      else if len == 1:
        provides.first.block
      else:
        raise("More than one provide")
      end
      with-provides = cases(A.Expr) body:
        | s_block(l2, stmts) =>
          last = stmts.last()
          new-stmts = if is-binder(last):
            stmts + [A.s_obj(l2, [
                A.s_data_field(l2, str("answer"), A.s_id(l2, "nothing")),
                A.s_data_field(l2, str("provide"), prov)
              ])]
          else:
            stmts.take(stmts.length() - 1) + [A.s_obj(l2, [
                A.s_data_field(l2, str("answer"), last),
                A.s_data_field(l2, str("provide"), prov)
              ])]
          end
          A.s_block(l2, new-stmts)
        | else => body
      end
      to-desugar = wrap-env-imports(l, with-provides, compile-env)
      imports = headers.filter(fun(h): not A.is-s_provide(h) end)
      full-imports = imports + for map(k from compile-env.modules.keys()):
          A.s_import(l, A.s_const_import(k), k)
        end

      
      A.s_program(l, full-imports, desugar-expr(mt-d-env, to-desugar))
  end
where:
  d = A.dummy-loc
  str = A.s_str(d, _)
  ds = desugar(_, C.no-builtins)
  ds(A.surface-parse("provide x end x = 10", "test")) satisfies
    A.equiv-ast-prog(_, A.s_program(d, [],
      A.s_block(d, [
        A.s_block(d, [
          A.s_let_expr(d, [
              A.s_let_bind(d, mk-bind(d, "x"), A.s_num(d, 10))
            ],
            A.s_obj(d, [
                A.s_data_field(d, str("answer"), A.s_id(d, "nothing")),
                A.s_data_field(d, str("provide"), A.s_id(d, "x"))
              ]))
        ])
      ])))

  ds(A.surface-parse("provide x end import 'foo.arr' as F x = 10 F(x)", "test")) satisfies
    A.equiv-ast-prog(_, A.s_program(d, [
        A.s_import(d, A.s_file_import("foo.arr"), "F") 
      ],
      A.s_block(d, [
        A.s_block(d, [
          A.s_let_expr(d, [
              A.s_let_bind(d, mk-bind(d, "x"), A.s_num(d, 10))
            ],
            A.s_obj(d, [
                A.s_data_field(d, str("answer"), A.s_app(d, A.s_id(d, "F"), [A.s_id(d, "x")])),
                A.s_data_field(d, str("provide"), A.s_id(d, "x"))
              ]))
        ])
      ])))

end

fun mk-bind(l, id): A.s_bind(l, false, id, A.a_blank);

fun mk-id(loc, base):
  t = G.make-name(base)
  { id: t, id-b: mk-bind(loc, t), id-e: A.s_id(loc, t) }
end

fun make-torepr(l, vname, fields):
  self = mk-id(l, "self")
  fun str(s): A.s_str(l, s) end
  fun call-torepr(val):
    A.s_app(l, A.s_id(l, "torepr"), [A.s_dot(l, self.id-e, val.bind.id)])
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
    A.s_block(l, []))
end

fun make-match(l, case-name, fields):
  call-match-case = mk-id(l, "call-" + case-name)
  self-id = mk-id(l, "self")
  cases-id = mk-id(l, "cases-funs")
  else-id = mk-id(l, "else-clause")
  args = for map(f from fields):
      cases(A.VariantMember) f:
        | s_variant_member(l2, mtype, bind) =>
          when mtype <> "normal":
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
                  A.s_dot(l, A.s_id(l, "builtins"), "has-field"),
                  [cases-id.id-e, A.s_str(l, case-name)]
                ),
              A.s_let_expr(l, [A.s_let_bind(l, call-match-case.id-b, A.s_dot(l, cases-id.id-e, case-name))],
                  A.s_app(l, call-match-case.id-e, args)
                )
            )
        ],
        A.s_app(l, else-id.id-e, [])),
      A.s_block(l, []))
end

fun resolve-scope(stmts, let-binds, letrec-binds) -> List<Expr>:
  doc: "Treating stmts as a block, remove x = e, var x = e, and fun f(): e end
        and turn them into explicit let and letrec expressions."
  cases(List) stmts:
    | empty => raise("Empty block in resolve-scope")
    | link(f, rest-stmts) =>
      fun wrap-letrecs(expr):
        A.s_letrec(letrec-binds.first.l, letrec-binds.reverse(), expr)
      end
      fun wrap-lets(expr):
        A.s_let_expr(let-binds.first.l, let-binds.reverse(), expr)
      end
      fun handle-let-bind(l, new-bind):
        new-binds = link(new-bind, let-binds)
        resolved-inner = resolve-scope(rest-stmts, new-binds, [])
        if is-empty(letrec-binds):
          resolved-inner
        else:
          [wrap-letrecs(A.s_block(l, resolved-inner))]
        end
      end
      wrapper = 
        if is-link(let-binds): wrap-lets
        else if is-link(letrec-binds): wrap-letrecs
        else: fun(e): e;
        end
      cases(A.Expr) f:
        | s_let(l, bind, expr) =>
          handle-let-bind(l, A.s_let_bind(l, bind, expr))
        | s_var(l, bind, expr) =>
          handle-let-bind(l, A.s_var_bind(l, bind, expr))
        | s_fun(l, name, params, args, ann, doc, body, _check) =>
          new-letrecs = link(A.s_letrec_bind(
              l,
              A.s_bind(l, false, name, A.a_blank),
              A.s_lam(l, params, args, ann, doc, body, _check)
            ), letrec-binds)
          resolved-inner = resolve-scope(rest-stmts, [], new-letrecs)
          if is-empty(let-binds):
            resolved-inner
          else:
            [wrap-lets(A.s_block(l, resolved-inner))]
          end
        | s_data(l, name, params, mixins, variants, shared, _check) =>
          fun b(loc, id): A.s_bind(loc, false, id, A.a_blank);
          fun variant-member-to-field(vm, val-id):
            cases(A.VariantMember) vm:
              | s_variant_member(l2, member_type, bind) =>
                when not (member_type == "normal"):
                  raise("Non-normal member_type " + member_type + " at " + torepr(l))
                end
                A.s_data_field(l2, A.s_str(l2, bind.id), A.s_id(l2, val-id))
            end
          end
          fun variant-binds(v, main-brander, variant-brander, sharing-id :: String):
            fun brand(brander-id, arg): A.s_app(l, A.s_dot(l, A.s_id(l, brander-id), "brand"), [arg]);
            fun body-of-members(members, variant-bind-names, shared-id):
              obj = A.s_extend(l, A.s_id(l, shared-id), map2(variant-member-to-field, members, variant-bind-names))
              brand(main-brander, brand(variant-brander, obj))
            end
            cases(A.Variant) v:
              | s_variant(l2, vname, members, with-members) =>
                shared-id = G.make-name(vname)
                m = A.s_data_field(l2, A.s_str(l2, "_match"), make-match(l2, vname, members))
                tr = A.s_data_field(l2, A.s_str(l2, "_torepr"), make-torepr(l2, vname, members))
                variant-bind-names = members.map(_.bind).map(_.id).map(G.make-name)
                [
                  A.s_letrec_bind(l2, b(l2, shared-id), A.s_extend(l2, A.s_id(l2, sharing-id), [m, tr] + with-members)),
                  A.s_letrec_bind(l2, b(l2, vname),
                    A.s_lam(
                        l2,
                        [],
                        variant-bind-names.map(b(l2, _)),
                        A.a_blank,
                        "Creates a " + vname,
                        body-of-members(members, variant-bind-names, shared-id),
                        A.s_block(l, [])
                      ))
                ]
              | s_singleton_variant(l2, vname, with-members) =>
                shared-id = G.make-name(vname)
                m = A.s_data_field(l2, A.s_str(l2, "_match"), make-match(l2, vname, []))
                tr = A.s_data_field(l2, A.s_str(l2, "_torepr"), make-torepr(l2, vname, []))
                [
                  A.s_letrec_bind(l2, b(l2, vname), brand(main-brander, brand(variant-brander, A.s_extend(l2, A.s_id(l2, sharing-id), [m,tr] + with-members))))
                ]
            end
          end
          main-brand-id = G.make-name(name)
          shared-fields-id = G.make-name(name + "-shared")
          variant-names = variants.map(_.name)
          name-ids = variant-names.map(G.make-name)
          fun mk-brander(id):
            A.s_letrec_bind(l, b(l, id), A.s_app(l, A.s_id(l, "brander"), []))
          end
          fun tester(target, brander-id):
            A.s_letrec_bind(l, b(l, target), A.s_dot(l, A.s_id(l, brander-id), "test"))
          end

          data-initial-binds = link(
              mk-brander(main-brand-id),
              name-ids.map(mk-brander) +
                link(
                    tester(name, main-brand-id),
                    for map2(v from variant-names, id from name-ids):
                      tester(A.make-checker-name(v), id)
                    end
                  )
            )
          data-letrec-binds = for fold2(acc from [], v from variants, vn from name-ids):
              acc + variant-binds(v, main-brand-id, vn, shared-fields-id)
            end
          data-with-sharing =  data-letrec-binds + [
              A.s_letrec_bind(l, A.s_bind(l, false, shared-fields-id, A.a_blank), A.s_obj(l, shared))
            ]
          all-new-binds = data-with-sharing + data-initial-binds.reverse()
          if is-empty(letrec-binds):
            [wrapper(A.s_block(l, resolve-scope(rest-stmts, [], all-new-binds)))]
          else:
            resolve-scope(rest-stmts, [], all-new-binds + letrec-binds)
          end

        | else =>
          cases(List) rest-stmts:
            | empty => [wrapper(f)]
            | link(_, _) =>
              if not (is-link(let-binds) or is-link(letrec-binds)):
                link(f, resolve-scope(rest-stmts, [], []))
              else:
                [wrapper(A.s_block(f.l,
                  link(f, resolve-scope(rest-stmts, [], []))))]
              end
          end
      end
  end
where:
  p = fun(str): A.surface-parse(str, "test").block;
  d = A.dummy-loc
  b = A.s_bind(d, false, _, A.a_blank)
  bk = fun(e): A.s_block(d, [e]) end
  bs = fun(str):
    A.s_block(d, resolve-scope(p(str).stmts, [], []))
  end
  n = A.s_block(d, [])
  thunk = fun(e): A.s_lam(d, [], [], A.a_blank, "", bk(e), n) end

  resolve-scope(p("x = 5 y = 10 y").stmts, [], []).first
    satisfies 
      A.equiv-ast(_, A.s_let_expr(d, [A.s_let_bind(d, b("x"), A.s_num(d, 5)),
                                      A.s_let_bind(d, b("y"), A.s_num(d, 10))],
                        A.s_id(d, "y")))

  resolve-scope(p("x = 5 var y = 10 y").stmts, [], []).first
    satisfies 
      A.equiv-ast(_, A.s_let_expr(d, [A.s_let_bind(d, b("x"), A.s_num(d, 5)),
                                      A.s_var_bind(d, b("y"), A.s_num(d, 10))],
                        A.s_id(d, "y")))

  bs("x = 5 print(2) var y = 10 y")
    satisfies 
      A.equiv-ast(_,
                  A.s_block(d,
                    [ A.s_let_expr(d, [A.s_let_bind(d, b("x"), A.s_num(d, 5))],
                        A.s_block(d, [
                            A.s_app(d, A.s_id(d, "print"), [A.s_num(d, 2)]),
                            A.s_let_expr(d, [A.s_var_bind(d, b("y"), A.s_num(d, 10))],
                              A.s_id(d, "y"))
                          ]))]))

  prog = bs("fun f(): 4 end fun g(): 5 end f()")
  prog
    satisfies
      A.equiv-ast(_,
        A.s_block(d,
          [ A.s_letrec(d, [
              A.s_letrec_bind(d, b("f"), thunk(A.s_num(d, 4))),
              A.s_letrec_bind(d, b("g"), thunk(A.s_num(d, 5)))
            ],
            A.s_app(d, A.s_id(d, "f"), []))
          ]))

  p-s = fun(e): A.s_app(d, A.s_id(d, "print"), [e]);
  pretty = fun(e): e.tosource().pretty(80).join-str("\n");

  prog2 = bs("print(1) fun f(): 4 end fun g(): 5 end fun h(): 6 end x = 3 print(x)")
  prog2
    satisfies
      A.equiv-ast(_,
          A.s_block(d,
            [ p-s(A.s_num(d, 1)),
              A.s_letrec(d, [
                  A.s_letrec_bind(d, b("f"), thunk(A.s_num(d, 4))),
                  A.s_letrec_bind(d, b("g"), thunk(A.s_num(d, 5))),
                  A.s_letrec_bind(d, b("h"), thunk(A.s_num(d, 6)))
                ],
                A.s_block(d, [
                    A.s_let_expr(d, [A.s_let_bind(d, b("x"), A.s_num(d, 3))], p-s(A.s_id(d, "x")))
                  ]))]))

  resolve-scope([prog2], [], []).first satisfies A.equiv-ast(_, prog2)
  for each2(p1 from resolve-scope(prog2.stmts, [], []), p2 from prog2.stmts):
    p1 satisfies A.equiv-ast(_, p2)
  end

  prog3 = bs("print(x) x := 3 print(x)")
  prog3 satisfies
    A.equiv-ast(_,
      A.s_block(d,
          [
            p-s(A.s_id(d, "x")),
            A.s_assign(d, "x", A.s_num(d, 3)),
            p-s(A.s_id(d, "x"))
          ]
        )
      )

  prog4 = bs("var x = 10 fun f(): 4 end f()")
  prog4 satisfies
    A.equiv-ast(_,
      A.s_block(d, [
        A.s_let_expr(d, [
              A.s_var_bind(d, b("x"), A.s_num(d, 10))
            ],
            A.s_block(d, [
                A.s_letrec(d, [
                    A.s_letrec_bind(d, b("f"), thunk(A.s_num(d, 4)))
                  ],
                  A.s_app(d, A.s_id(d, "f"), []))
              ]))]))

  #prog5 = bs("data List: empty | link(f, r) end empty")
  #prog5.stmts.length() is 1
  #the-let = prog5.stmts.first
  #the-let satisfies A.is-s_let_expr
  #the-let.binds.length() is 6 # ListB, emptyB, linkB, List, is-empty, is-link
  #the-let.binds.take(3).map(_.value) satisfies list.all(fun(e): A.is-s_app(e) and (e._fun.id == "brander");, _)
  #the-let.binds.drop(3).map(_.value) satisfies list.all(fun(e): A.is-s_dot(e) and (e.field == "test");, _)
  #the-letrec = the-let.body
  #the-letrec satisfies A.is-s_letrec
  #the-letrec.binds.length() is 4 # emptyDict, linkDict, empty, link

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

fun desugar-case-branch(nv, c):
  cases(A.CasesBranch) c:
    | s_cases_branch(l2, name, args, body) =>  
      desugar-member(nv,
        A.s_data_field(l2, A.s_str(l2, name), A.s_lam(l2, [], args, A.a_blank, "", body, A.s_block(l2, []))))
  end
end
fun desugar-cases(l, ann, val, branches, else-block):
  val-id = mk-id(l, "cases-val")
  cases-object = A.s_obj(l, branches)
  else-thunk = A.s_lam(l, [], [], A.a_blank, "", else-block, A.s_block(l, []))
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
        A.s_data_field(d, A.s_str(d, "empty"), A.s_lam(d, [], [], A.a_blank, "", A.s_num(d, 5), A.s_block(d, [])))
      ],
      A.s_num(d, 4)
    )
  id = prog.binds.first.b
    
  prog satisfies A.equiv-ast(_, A.s_let_expr(d, [
          A.s_let_bind(d, id, A.s_num(d, 1))
        ],
        A.s_app(d, A.s_dot(d, A.s_id(d, id.id), "_match"), [
          A.s_obj(d, [
              A.s_data_field(d, A.s_str(d, "empty"), A.s_lam(d, [], [], A.a_blank, "", A.s_num(d, 5), A.s_block(d, [])))
            ]),
          A.s_lam(d, [], [], A.a_blank, "", A.s_num(d, 4), A.s_block(d, []))])))

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
  A.is-s_id(e) and (e.id == "_")
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
    A.s_lam(l, [], [curried-obj.id-b], A.a_blank, "", rebuild-node(l, curried-obj.id-e, m), A.s_block(l, []))
  else:
    rebuild-node(l, desugar-expr(nv, obj), m)
  end
where:
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
      A.s_lam(s, [], params, A.a_blank, "", rebuild(curry-args.first, curry-args.rest.first), A.s_block(s, []))
  end
end

fun ds-curry(nv, l, f, args):
  fun fallthrough():
    params-and-args = ds-curry-args(l, args)
    params = params-and-args.left
    ds-f = desugar-expr(nv, f)
    if is-empty(params): A.s_app(l, ds-f, args)
    else: A.s_lam(l, [], params, A.a_blank, "", A.s_app(l, ds-f, params-and-args.right), A.s_block(l, []))
    end
  end
  cases(A.Expr) f:
    | s_dot(l2, obj, m) =>
      if is-underscore(obj):
        curried-obj = mk-id(l, "recv-")
        params-and-args = ds-curry-args(l, args)
        params = params-and-args.left
        A.s_lam(l, [], link(curried-obj.id-b, params), A.a_blank, "",
            A.s_app(l, A.s_dot(l, curried-obj.id-e, m), params-and-args.right), A.s_block(l, []))
      else:
        fallthrough()
      end
    | else => fallthrough()
  end
where:
  d = A.dummy-loc
  ds-ed = ds-curry(
      mt-d-env,
      d,
      A.s_id(d, "f"),
      [
        A.s_id(d, "_"),
        A.s_id(d, "x")
      ]
    )
  ds-ed satisfies A.is-s_lam
  ds-ed.args.length() is 1

  ds-ed2 = ds-curry(
      mt-d-env,
      d,
      A.s_id(d, "f"),
      [
        A.s_id(d, "_"),
        A.s_id(d, "_")
      ]
    )
  ds-ed2 satisfies A.is-s_lam
  ds-ed2.args.length() is 2

  ds-ed3 = ds-curry(
      mt-d-env,
      d,
      A.s_id(d, "f"),
      [
        A.s_id(d, "x"),
        A.s_id(d, "y")
      ]
    )
  ds-ed3 satisfies A.equiv-ast(_, A.s_app(d, A.s_id(d, "f"), [A.s_id(d, "x"), A.s_id(d, "y")]))
    
  ds-ed4 = ds-curry(
      mt-d-env,
      d,
      A.s_dot(d, A.s_id(d, "_"), "f"),
      [
        A.s_id(d, "x")
      ])
  ds-ed4 satisfies A.is-s_lam
  ds-ed4.args.length() is 1
        
end

fun desugar-expr(nv :: DesugarEnv, expr :: A.Expr):
  cases(A.Expr) expr:
    | s_block(l, stmts) =>
      cases(List) stmts:
        | empty => expr
        | link(_, _) =>
          new-stmts = resolve-scope(stmts, [], [])
          A.s_block(l, resolve-scope(stmts, [], []).map(desugar-expr(nv, _)))
      end
    | s_user_block(l, body) =>
      desugar-expr(nv, body)
    | s_app(l, f, args) =>
      ds-curry(nv, l, f, args.map(desugar-expr(nv, _)))
    | s_left_app(l, o, f, args) =>
      ds-curry(nv, l, f, ([o] + args).map(desugar-expr(nv, _)))
    | s_lam(l, params, args, ann, doc, body, _check) =>
      new-env = for fold(nv2 from nv, arg from args): extend-id(nv2, arg.id) end
      A.s_lam(l, params, args, ann, doc, desugar-expr(new-env, body), desugar-expr(nv, _check))
    | s_method(l, args, ann, doc, body, _check) =>
      new-env = for fold(nv2 from nv, arg from args): extend-id(nv2, arg.id) end
      A.s_method(l, args, ann, doc, desugar-expr(new-env, body), desugar-expr(nv, _check))
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
    | s_not(l, test) => 
      A.s_if_else(l, [A.s_if_branch(l, desugar-expr(nv, test), A.s_block(l, [A.s_bool(l, false)]))], A.s_block(l, [A.s_bool(l, true)]))
    | s_when(l, test, body) =>
      A.s_if_else(l, [A.s_if_branch(l, desugar-expr(nv, test), A.s_block(l, [desugar-expr(nv, body), A.s_id(l, "nothing")]))], A.s_block(l, [A.s_id(l, "nothing")]))
    | s_if(l, branches) =>
      raise("If must have else for now")
    | s_if_else(l, branches, _else) =>
      A.s_if_else(l, branches.map(desugar-if-branch(nv, _)), desugar-expr(nv, _else))
    | s_cases(l, type, val, branches) =>
      desugar-cases(l, type, desugar-expr(nv, val), branches.map(desugar-case-branch(nv, _)), A.s_block(l, [A.s_app(l, A.s_id(l, "raise"), [A.s_str(l, "no cases matched")])]))
    | s_cases_else(l, type, val, branches, _else) =>
      desugar-cases(l, type, desugar-expr(nv, val), branches.map(desugar-case-branch(nv, _)), desugar-expr(nv, _else))
    | s_assign(l, id, val) => A.s_assign(l, id, desugar-expr(nv, val))
    | s_dot(l, obj, field) => ds-curry-nullary(A.s_dot, nv, l, obj, field)
    | s_colon(l, obj, field) => ds-curry-nullary(A.s_colon, nv, l, obj, field)
    | s_extend(l, obj, fields) => A.s_extend(l, desugar-expr(nv, obj), fields.map(desugar-member(nv, _)))
    | s_for(l, iter, bindings, ann, body) => 
      values = bindings.map(_.value).map(desugar-expr(nv, _))
      the-function = A.s_lam(l, [], bindings.map(_.bind), ann, "", desugar-expr(nv, body), A.s_block(l, []))
      A.s_app(l, desugar-expr(nv, iter), link(the-function, values))
    | s_op(l, op, left, right) =>
      cases(Option) get-arith-op(op):
        | some(field) => A.s_app(l, A.s_dot(l, desugar-expr(nv, left), field), [desugar-expr(nv, right)])
        | none =>
          fun thunk(e): A.s_lam(l, [], [], A.a_blank, "", A.s_block(l, [e]), A.s_block(l, [])) end
          fun opbool(fld):
            A.s_app(l, A.s_dot(l, desugar-expr(nv, left), fld), [thunk(desugar-expr(nv, right))])
          end
          if op == "op==":
            ds-curry-binop(l, desugar-expr(nv, left), desugar-expr(nv, right),
                fun(e1, e2):
                  A.s_app(l, A.s_dot(l, A.s_id(l, "builtins"), "equiv"), [e1, e2])
                end)
          else if op == "op<>":
            A.s_if_else(l,
              [A.s_if_branch(l,
                ds-curry-binop(l, desugar-expr(nv, left), desugar-expr(nv, right),
                    fun(e1, e2):
                      A.s_app(l, A.s_dot(l, A.s_id(l, "builtins"), "equiv"), [e1, e2])
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
      if nv.vars.member(x): A.s_id_var(l, x)
      else if nv.letrecs.member(x): A.s_id_letrec(l, x)
      else: expr
      end
    | s_num(_, _) => expr
    | s_str(_, _) => expr
    | s_bool(_, _) => expr
    | s_obj(l, fields) => A.s_obj(l, fields.map(desugar-member(nv, _)))
    | s_list(l, elts) =>
      elts.foldr(fun(elt, list-expr): A.s_app(l, desugar-expr(nv, A.s_id(l, "link")), [desugar-expr(nv, elt), list-expr]) end, desugar-expr(nv, A.s_id(l, "empty")))
    | s_paren(l, e) => desugar-expr(nv, e)
    # TODO(joe): skipping checks for now, they should be unreachable
    | s_check(l, _) => A.s_str(l, "check mode not yet working (check block)")
    | s_check_test(l, _, _, _) => A.s_app(l, A.s_id(l, "raise"), [A.s_str(l, "check mode not yet working (test stmt)")])
    | else => raise("NYI (desugar): " + torepr(expr))
  end
where:
  p = fun(str): A.surface-parse(str, "test").block;
  d = A.dummy-loc
  ds = desugar-expr(mt-d-env, _)
  one = A.s_num(d, 1)
  two = A.s_num(d, 2)
  b = A.s_bind(d, false, _, A.a_blank)
  equiv = fun(e): A.equiv-ast(_, e) end

  prog = p("var x = 10 x := 5 test-print(x)")
  ds(prog) satisfies
    equiv(A.s_block(d, [
        A.s_let_expr(d, [
            A.s_var_bind(d, b("x"), A.s_num(d, 10))
          ],
          A.s_block(d, [
              A.s_assign(d, "x", A.s_num(d, 5)),
              A.s_app(d, A.s_id(d, "test-print"), [A.s_id_var(d, "x")])
            ]))
        ]))

  prog2 = p("[1,2,1 + 2]")
  ds(prog2) satisfies
    equiv(p("link(1, link(2, link(1._plus(2), empty)))"))

  prog3 = p("for map(elt from l): elt + 1 end")
  ds(prog3) satisfies
    equiv(p("map(fun(elt): elt._plus(1) end, l)"))

  prog4 = p("((5 + 1) == 6) or o^f()")
  ds(prog4) satisfies
    equiv(p("builtins.equiv(5._plus(1), 6)._or(fun(): f(o) end)"))

  ds(p("(5)")) satisfies equiv(ds(p("5")))

  prog5 = p("cases(List) l: | empty => 5 + 4 | link(f, r) => 10 end")
  dsed5 = ds(prog5)
  cases-name = dsed5.stmts.first.binds.first.b.id
  compare = (cases-name + " = l " +
             cases-name + "._match({empty: fun(): 5._plus(4) end, link: fun(f, r): 10 end},
                                   fun(): raise('no cases matched') end)")
  dsed5 satisfies equiv(ds(p(compare)))

  prog6 = p("when false: dostuff() end")
  compare6 = ds(p("if false: block: dostuff() end nothing else: nothing end"))
  ds(prog6) satisfies equiv(compare6)

  prog7 = p("not true")
  compare7 = ds(p("if true: false else: true end"))
  ds(prog7) satisfies equiv(compare7)

end

