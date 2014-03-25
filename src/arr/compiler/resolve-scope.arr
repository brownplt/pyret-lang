#lang pyret

provide *
import ast as A
import parse-pyret as PP
import string-dict as SD
import "./compile-structs.arr" as C
import "./ast-util.arr" as U
import "./gensym.arr" as G

fun mk-bind(l, id): A.s_bind(l, false, id, A.a_blank);

fun mk-id(loc, base):
  t = A.s_name(base)
  { id: t, id-b: mk-bind(loc, t), id-e: A.s_id(loc, t) }
end

fun resolve-header(h :: A.Header, b :: A.Expr):
  cases(A.Header) h:
    | s_provide_all(l) =>
      ids = A.block-ids(b)
      obj = A.s_obj(l, for map(id from ids): A.s_data_field(l, A.s_str(l, tostring(id)), A.s_id(l, id)) end)
      A.s_provide(l, obj)
    | s_import(l, imp, name) =>
      cases(A.ImportType) imp:
        | s_file_import(file) =>
          if string-contains(file, "/"): h
          else: A.s_import(l, A.s_file_import("./" + file), name)
          end
        | else => h
      end
    | else => h
  end
end


fun desugar-scope-block(stmts, let-binds, letrec-binds) -> List<Expr>:
  doc: "Treating stmts as a block, resolve scope."
  cases(List) stmts:
    | empty => empty
    | link(f, rest-stmts) =>
      fun wrap-letrecs(expr):
        A.s_letrec(letrec-binds.first.l, letrec-binds.reverse(), expr)
      end
      fun wrap-lets(expr):
        A.s_let_expr(let-binds.first.l, let-binds.reverse(), expr)
      end
      fun handle-let-bind(l, new-bind):
        new-binds = link(new-bind, let-binds)
        resolved-inner = desugar-scope-block(rest-stmts, new-binds, [])
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
        | s_let(l, bind, expr, _) =>
          handle-let-bind(l, A.s_let_bind(l, bind, expr))
        | s_var(l, bind, expr, _) =>
          handle-let-bind(l, A.s_var_bind(l, bind, expr))
        | s_fun(l, name, params, args, ann, doc, body, _check) =>
          new-letrecs = link(A.s_letrec_bind(
              l,
              A.s_bind(l, false, A.s_name(name), A.a_blank),
              A.s_lam(l, params, args, ann, doc, body, _check)
            ), letrec-binds)
          resolved-inner = desugar-scope-block(rest-stmts, [], new-letrecs)
          if is-empty(let-binds):
            resolved-inner
          else:
            [wrap-lets(A.s_block(l, resolved-inner))]
          end
        | s_data(l, name, params, mixins, variants, shared, _check) =>
          fun b(loc, id): A.s_bind(loc, false, A.s_name(id), A.a_blank);
          fun variant-binds(data-blob-id, v):
            vname = v.name
            checker-name = A.make-checker-name(vname)
            get-part = A.s_dot(v.l, data-blob-id, _)
            [
              A.s_letrec_bind(v.l, b(v.l, vname), get-part(vname)),
              A.s_letrec_bind(v.l, b(v.l, checker-name), get-part(checker-name))
            ]
          end
          blob-id = G.make-name(name)
          data-expr = A.s_data_expr(l, name, params, mixins, variants, shared, _check)
          bind-data = A.s_letrec_bind(l, b(l, blob-id), data-expr)
          bind-data-pred = A.s_letrec_bind(l, b(l, name), A.s_dot(l, A.s_id(l, A.s_name(blob-id)), name))
          all-binds = for fold(acc from [bind-data-pred, bind-data], v from variants):
            variant-binds(A.s_id(l, A.s_name(blob-id)), v) + acc
          end

          if is-empty(letrec-binds):
            [wrapper(A.s_block(l, desugar-scope-block(rest-stmts, [], all-binds)))]
          else:
            desugar-scope-block(rest-stmts, [], all-binds + letrec-binds)
          end

        | else =>
          cases(List) rest-stmts:
            | empty => [wrapper(f)]
            | link(_, _) =>
              if not (is-link(let-binds) or is-link(letrec-binds)):
                link(f, desugar-scope-block(rest-stmts, [], []))
              else:
                [wrapper(A.s_block(f.l,
                  link(f, desugar-scope-block(rest-stmts, [], []))))]
              end
          end
      end
  end
where:
  p = fun(str): PP.surface-parse(str, "test").block;
  d = A.dummy-loc
  b = fun(s): A.s_bind(d, false, A.s_name(s), A.a_blank);
  id = fun(s): A.s_id(d, A.s_name(s));
  bk = fun(e): A.s_block(d, [e]) end
  bs = fun(str):
    A.s_block(d, desugar-scope-block(p(str).stmts, [], []))
  end
  n = none
  thunk = fun(e): A.s_lam(d, [], [], A.a_blank, "", bk(e), n) end


  compare1 = A.s_let_expr(d, [A.s_let_bind(d, b("x"), A.s_num(d, 15)),
                                      A.s_let_bind(d, b("y"), A.s_num(d, 10))],
                        id("y"))
  desugar-scope-block(p("x = 15 y = 10 y").stmts, [], []).first
    satisfies 
      A.equiv-ast(_, compare1)

  desugar-scope-block(p("x = 55 var y = 10 y").stmts, [], []).first
    satisfies 
      A.equiv-ast(_, A.s_let_expr(d, [A.s_let_bind(d, b("x"), A.s_num(d, 55)),
                                      A.s_var_bind(d, b("y"), A.s_num(d, 10))],
                        id("y")))

  bs("x = 7 print(2) var y = 10 y")
    satisfies 
      A.equiv-ast(_,
                  A.s_block(d,
                    [ A.s_let_expr(d, [A.s_let_bind(d, b("x"), A.s_num(d, 7))],
                        A.s_block(d, [
                            A.s_app(d, id("print"), [A.s_num(d, 2)]),
                            A.s_let_expr(d, [A.s_var_bind(d, b("y"), A.s_num(d, 10))],
                              id("y"))
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
            A.s_app(d, id("f"), []))
          ]))

  p-s = fun(e): A.s_app(d, id("print"), [e]);
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
                    A.s_let_expr(d, [A.s_let_bind(d, b("x"), A.s_num(d, 3))], p-s(id("x")))
                  ]))]))

  desugar-scope-block([prog2], [], []).first satisfies A.equiv-ast(_, prog2)
  for each2(p1 from desugar-scope-block(prog2.stmts, [], []), p2 from prog2.stmts):
    p1 satisfies A.equiv-ast(_, p2)
  end

  prog3 = bs("print(x) x := 3 print(x)")
  prog3 satisfies
    A.equiv-ast(_,
      A.s_block(d,
          [
            p-s(id("x")),
            A.s_assign(d, A.s_name("x"), A.s_num(d, 3)),
            p-s(id("x"))
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
                  A.s_app(d, id("f"), []))
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

desugar-scope-visitor = A.default-map-visitor.{
  s_block(self, l, stmts):
    A.s_block(l, desugar-scope-block(stmts.map(_.visit(self)), [], []))
  end
}

fun wrap-env-imports(l, expr :: A.Expr, env :: C.CompileEnvironment):
  cases(C.CompileEnvironment) env:
    | compile-env(compile-bindings) =>
      let-binds = for fold(lst from [], b from compile-bindings):
          cases(C.CompileBinding) b:
            | module-bindings(mname, bindings) =>
              lst + 
                for map(name from bindings):
                  A.s_let(l, A.s_bind(l, false, A.s_name(name), A.a_blank), A.s_dot(l, A.s_id(l, A.s_name(mname)), name), false)
                end
            | else => lst
          end
        end
      A.s_block(l, let-binds + [expr])
  end
end


fun desugar-scope(prog :: A.Program, compile-env:: C.CompileEnvironment):
  doc: "Remove x = e, var x = e, and fun f(): e end
        and turn them into explicit let and letrec expressions.
        Do this recursively through the whole program.
        Preconditions on prog:
          - well-formed
        Postconditions on prog:
          - contains no s_provide in headers
          - contains no s_let, s_var, s_data"
  cases(A.Program) prog:
    | s_program(l, headers-raw, body) =>
      headers = headers-raw.map(resolve-header(_, body))
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
          new-stmts = stmts.take(stmts.length() - 1) + [A.s_obj(l2, [
              A.s_data_field(l2, str("answer"), last),
              A.s_data_field(l2, str("provide"), prov),
              A.s_data_field(
                  l2,
                  str("checks"),
                  A.s_app(l2, A.s_dot(l2, U.checkers(l2), "results"), [])
                )
            ])]
          A.s_block(l2, new-stmts)
        | else => body
      end
      wrapped = wrap-env-imports(l, with-provides, compile-env)
      imports = headers.filter(fun(h): not A.is-s_provide(h) end)
      full-imports = imports + for map(k from compile-env.bindings.filter(C.is-module-bindings).map(_.name)):
          A.s_import(l, A.s_const_import(k), A.s_name(k))
        end

      A.s_program(l, full-imports, wrapped.visit(desugar-scope-visitor))
  end
  
where:
  d = A.dummy-loc
  b = fun(s): A.s_bind(d, false, A.s_name(s), A.a_blank);
  id = fun(s): A.s_id(d, A.s_name(s));
  checks = A.s_data_field(
                  d,
                  A.s_str(d, "checks"),
                  A.s_app(d, A.s_dot(d, U.checkers(d), "results"), [])
                )
  str = A.s_str(d, _)
  ds = desugar-scope(_, C.minimal-builtins)
  compare1 = A.s_program(d, [],
      A.s_block(d, [
        A.s_block(d, [
          A.s_let_expr(d, [
              A.s_let_bind(d, b("x"), A.s_num(d, 10))
            ],
            A.s_obj(d, [
                A.s_data_field(d, str("answer"), id("nothing")),
                A.s_data_field(d, str("provide"), id("x")),
                checks
              ]))
        ])
      ]))
  # NOTE(joe): Explicit nothing here because we expect to have
  # had append-nothing-if-necessary called
  ds(PP.surface-parse("provide x end x = 10 nothing", "test")) satisfies
    A.equiv-ast-prog(_, compare1)

  compare2 = A.s_program(d, [
        A.s_import(d, A.s_file_import("./foo.arr"), A.s_name("F"))
      ],
      A.s_block(d, [
        A.s_block(d, [
          A.s_let_expr(d, [
              A.s_let_bind(d, b("x"), A.s_num(d, 10))
            ],
            A.s_obj(d, [
                A.s_data_field(d, str("answer"), A.s_app(d, id("F"), [id("x")])),
                A.s_data_field(d, str("provide"), id("x")),
                checks
              ]))
        ])
      ]))
  ds(PP.surface-parse("provide x end import 'foo.arr' as F x = 10 F(x)", "test")) satisfies
    A.equiv-ast-prog(_, compare2)
    
end


names = A.MakeName(0)

data ScopeBinding:
  | letrec-bind(atom :: A.Name)
  | let-bind(atom :: A.Name)
  | var-bind(atom :: A.Name)
end

fun scope-env-from-env(initial :: C.CompileEnvironment):
  for fold(acc from SD.immutable-string-dict(), b from initial.bindings):
    cases(C.CompileBinding) b:
      | module-bindings(name, ids) =>
        for fold(
              acc2 from acc.set(name, let-bind(names.make-atom(name))),
              id from ids
            ):
          acc2.set(id, let-bind(names.make-atom(id)))
        end
      | else => acc
    end
  end
end

fun get-atom(name, env, type):
  cases(A.Name) name:
    | s_name(s) =>
      atom = names.make-atom(s)
      { atom: atom, env: env.set(s, type(atom)) }
    | s_underscore =>
      atom = names.make-atom("$underscore")
      { atom: atom, env: env }
    | else => raise("Unexpected atom type: " + torepr(name))
  end
end

fun resolve-names(p :: A.Program, initial-env :: C.CompileEnvironment):
  doc: "Turn all s_names into s_atom or s_global
        Preconditions on p:
          - Contains no s_let, s_var, s_data (e.g. call desugar-scope first)
        Postconditions on p:
          - Contains no s_name in names"
  fun handle-id(env, l, id):
    cases(A.Name) id:
      | s_name(s) =>
        if env.has-key(s):
          cases (ScopeBinding) env.get(s):
            | let-bind(atom) => A.s_id(l, atom)
            | letrec-bind(atom) => A.s_id_letrec(l, atom)
            | var-bind(atom) => A.s_id_var(l, atom)
          end
        else:
          A.s_id(l, names.s_global(s))
        end
      | s_underscore => A.s_id(l, A.s_underscore)
      | else => raise("Wasn't expecting a non-s_name in resolve-names id: " + torepr(id))
    end
  end
  names-visitor = A.default-map-visitor.{
    env: scope-env-from-env(initial-env),
    s_program(self, l, headers, body):
      headers-and-env = for fold(acc from { e: self.env, hs: [] }, h from headers):
        cases(A.Header) h:
          | s_import(l, file, name) =>
            atom-env = get-atom(name, acc.e, let-bind)
            new-header = A.s_import(l, file, atom-env.atom)
            { e: atom-env.env, hs: link(new-header, acc.hs) }
          | else => acc
        end
      end
      visit-body = body.visit(self.{env: headers-and-env.e})
      A.s_program(l, headers-and-env.hs.reverse(), visit-body)
    end,
    s_let_expr(self, l, binds, body):
      bound-env = for fold(acc from { e: self.env, bs : [] }, b from binds):
        cases(A.LetBind) b:
          | s_let_bind(l2, bind, expr) =>
            atom-env = get-atom(bind.id, acc.e, let-bind)
            visit-expr = expr.visit(self.{env: acc.e})
            new-bind = A.s_let_bind(l2, A.s_bind(l2, bind.shadows, atom-env.atom, bind.ann.visit(self.{env: acc.e})), visit-expr)
            {
              e: atom-env.env,
              bs: link(new-bind, acc.bs)
            }
          | s_var_bind(l2, bind, expr) =>
            atom-env = get-atom(bind.id, acc.e, var-bind)
            visit-expr = expr.visit(self.{env: acc.e})
            new-bind = A.s_var_bind(l2, A.s_bind(l2, bind.shadows, atom-env.atom, bind.ann.visit(self.{env: acc.e})), visit-expr)
            {
              e: atom-env.env,
              bs: link(new-bind, acc.bs)
            }
        end
      end
      visit-binds = bound-env.bs.reverse()
      visit-body = body.visit(self.{env: bound-env.e})
      A.s_let_expr(l, visit-binds, visit-body)
    end,
    s_letrec(self, l, binds, body):
      bind-env-and-atoms = for fold(acc from { env: self.env, atoms: [] }, b from binds):
        atom-env = get-atom(b.b.id, acc.env, letrec-bind)
        { env: atom-env.env, atoms: link(atom-env.atom, acc.atoms) }
      end
      new-visitor = self.{env: bind-env-and-atoms.env}
      visit-binds = for map2(b from binds, a from bind-env-and-atoms.atoms.reverse()):
        cases(A.LetrecBind) b:
          | s_letrec_bind(l2, bind, expr) =>
            new-bind = A.s_bind(l2, false, a, bind.ann.visit(self.{env: bind-env-and-atoms.env}))
            A.s_letrec_bind(l2, new-bind, expr.visit(new-visitor))
        end
      end
      visit-body = body.visit(new-visitor)
      A.s_letrec(l, visit-binds, visit-body)
    end,
    s_for(self, l, iter, binds, ann, body):
      env-and-binds = for fold(acc from { env: self.env, fbs: [] }, fb from binds):
        cases(ForBind) fb:
          | s_for_bind(l, bind, val) => 
            atom-env = get-atom(bind.id, acc.env, let-bind)
            new-bind = A.s_bind(bind.l, bind.shadows, atom-env.atom, bind.ann.visit(self.{env: acc.env}))
            new-fb = A.s_for_bind(l, new-bind, val.visit(self.{env: acc.env}))
            { env: atom-env.env, fbs: link(new-fb, acc.fbs) }
        end
      end
      A.s_for(l, iter.visit(self), env-and-binds.fbs.reverse(), ann.visit(self), body.visit(self.{env: env-and-binds.env}))
    end,
    s_cases_branch(self, l, name, args, body):
      env-and-atoms = for fold(acc from { env: self.env, atoms: [] }, a from args):
        atom-env = get-atom(a.id, acc.env, let-bind)
        { env: atom-env.env, atoms: link(atom-env.atom, acc.atoms) }
      end
      new-args = for map2(a from args, at from env-and-atoms.atoms.reverse()):
        cases(A.Bind) a:
          | s_bind(l2, shadows, id, ann) => A.s_bind(l2, false, at, ann.visit(self.{env: env-and-atoms.env}))
        end
      end
      new-body = body.visit(self.{env: env-and-atoms.env})
      A.s_cases_branch(l, name, new-args, new-body)
    end,
    s_lam(self, l, params, args, ann, doc, body, _check):
      env-and-atoms = for fold(acc from { env: self.env, atoms: [] }, a from args):
        atom-env = get-atom(a.id, acc.env, let-bind)
        { env: atom-env.env, atoms: link(atom-env.atom, acc.atoms) }
      end
      new-args = for map2(a from args, at from env-and-atoms.atoms.reverse()):
        cases(A.Bind) a:
          | s_bind(l2, shadows, id, ann) => A.s_bind(l2, false, at, ann.visit(self.{env: env-and-atoms.env}))
        end
      end
      new-body = body.visit(self.{env: env-and-atoms.env})
      new-check = self.{env: env-and-atoms.env}.option(_check)
      A.s_lam(l, params, new-args, ann, doc, new-body, new-check)
    end,
    s_method(self, l, args, ann, doc, body, _check):
      env-and-atoms = for fold(acc from { env: self.env, atoms: [] }, a from args):
        atom-env = get-atom(a.id, acc.env, let-bind)
        { env: atom-env.env, atoms: link(atom-env.atom, acc.atoms) }
      end
      new-args = for map2(a from args, at from env-and-atoms.atoms.reverse()):
        cases(A.Bind) a:
          | s_bind(l2, shadows, id, ann) => A.s_bind(l2, shadows, at, ann.visit(self.{env: env-and-atoms.env}))
        end
      end
      new-body = body.visit(self.{env: env-and-atoms.env})
      new-check = self.{env: env-and-atoms.env}.option(_check)
      A.s_method(l, new-args, ann, doc, new-body, new-check)
    end,
    s_method_field(self, l, name, args, ann, doc, body, _check):
      env-and-atoms = for fold(acc from { env: self.env, atoms: [] }, a from args):
        atom-env = get-atom(a.id, acc.env, let-bind)
        { env: atom-env.env, atoms: link(atom-env.atom, acc.atoms) }
      end
      new-args = for map2(a from args, at from env-and-atoms.atoms.reverse()):
        cases(A.Bind) a:
          | s_bind(l2, shadows, id, ann) => A.s_bind(l2, shadows, at, ann.visit(self.{env: env-and-atoms.env}))
        end
      end
      new-body = body.visit(self.{env: env-and-atoms.env})
      new-check = self.{env: env-and-atoms.env}.option(_check)
      A.s_method_field(l, name, new-args, ann, doc, new-body, new-check)
    end,
    s_assign(self, l, id, expr):
      cases(A.Name) id:
        | s_name(s) =>
          if self.env.has-key(s):
            cases (ScopeBinding) self.env.get(s):
              | var-bind(atom) => A.s_assign(l, atom, expr.visit(self))
              | else => raise("Assignment to non-var-binding " + torepr(l))
            end
          else:
            raise("Assignment to global " + torepr(l))
          end
        | s_underscore =>
          A.s_assign(l, id, expr)
        | else => raise("Wasn't expecting a non-s_name in resolve-names for assignment: " + torepr(id))
      end
    end,
    s_id(self, l, id): handle-id(self.env, l, id) end,
    s_id_letrec(self, l, id): handle-id(self.env, l, id) end,
    s_id_var(self, l, id): handle-id(self.env, l, id) end,
    s_variant_member(self, l, typ, bind):
      new-bind = cases(A.Bind) bind:
        | s_bind(l2, shadows, name, ann) =>
          atom-env = get-atom(name, self.env, let-bind)
          A.s_bind(l2, shadows, atom-env.atom, ann.visit(self))
      end
      A.s_variant_member(l, typ, new-bind)
    end,
    s_bind(self, l, shadows, id, ann):
      cases(A.Name) id:
        | s_underscore => A.s_bind(l, shadows, id, ann)
        | else => 
          raise("Should not reach non-underscore bindings in resolve-names" + torepr(l) + torepr(id))
      end
    end
  }
  p.visit(names-visitor)
end

