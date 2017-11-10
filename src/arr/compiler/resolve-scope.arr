#lang pyret

provide *
provide-types *
import ast as A
import either as E
import lists as L
import srcloc as S
import parse-pyret as PP
import string-dict as SD
import file("compile-structs.arr") as C
import file("ast-util.arr") as U
import file("gensym.arr") as G
import file("type-structs.arr") as T

type ValueBind = C.ValueBind
type TypeBind = C.TypeBind


string-dict = SD.string-dict
mtd = [string-dict:]

names = A.global-names

type NameResolution = C.NameResolution

fun mk-bind(l, id) -> A.Expr: A.s-bind(l, false, id, A.a-blank) end

fun mk-id(loc, base) -> { id :: A.Expr, id-b :: A.Expr, id-e :: A.Expr }:
  t = A.s-name(loc, base)
  { id: t, id-b: mk-bind(loc, t), id-e: A.s-id(loc, t) }
end

fun resolve-provide(p :: A.Provide, b :: A.Expr) -> A.Provide:
  cases(A.Provide) p:
    | s-provide-all(l) =>
#      s-provide-all(l)
      ids = A.block-ids(b)
      obj = A.s-obj(l, for map(id from ids): A.s-data-field(l, id.tosourcestring(), A.s-id(l, id)) end)
      A.s-provide(l, obj)
    | s-provide-none(l) =>
      A.s-provide(l, A.s-obj(l, [list: ]))
    | else =>
      p
  end
end

fun resolve-type-provide(p :: A.ProvideTypes, b :: A.Expr) -> A.ProvideTypes:
  cases(A.ProvideTypes) p:
    | s-provide-types-none(l) =>
      A.s-provide-types(l, [list:])
    | s-provide-types-all(l) =>
      ids = A.block-type-ids(b)
      type-fields = for map(id from ids):
        if id.bind-type == "data":
          A.a-field(l, id.name.toname(), A.a-name(l, id.name))
        else:
          A.a-field(l, id.name.toname(), A.a-name(l, id.name))
        end
      end
      A.s-provide-types(l, type-fields)
    | else => p
  end
end

fun resolve-module-provide(p :: A.ProvideModules, imports :: List<A.Import>) -> A.ProvideModules:
  cases(A.ProvideModules) p:
    | s-provide-modules-none(l) =>
      A.s-provide-modules(l, A.s-obj(l, [list:]))
    | s-provide-modules-all(l) =>
      mnames = for fold(acc from [list:], i from imports):
        cases(A.Import) i:
          | s-import(_, _, name) =>
            fld = A.s-data-field(l, name.tosourcestring(), A.s-id(l, name))
            link(fld, acc)
          | s-import-complete(_, _, _, _, name, _) =>
            if A.is-s-underscore(name):
              acc
            else:
              fld = A.s-data-field(l, name.tosourcestring(), A.s-id(l, name))
              link(fld, acc)
            end
          | else => acc
        end
      end
      A.s-provide-modules(l, A.s-obj(l, mnames))
    | else => p
  end
end

is-s-import-complete = A.is-s-import-complete

fun expand-import(imp :: A.Import, env :: C.CompileEnvironment) -> A.Import % (is-s-import-complete):
  cases(A.Import) imp:
    | s-import(l, shadow imp, name) =>
      A.s-import-complete(l, empty, empty, imp, name, name)
    | s-import-fields(l, fields, shadow imp) =>
      imp-str = if A.is-s-const-import(imp): imp.mod else: "mod-import" end
      A.s-import-complete(l, fields, empty, imp, A.s-underscore(l), A.s-underscore(l))
    | s-include(l, shadow imp) =>
      imp-str = if A.is-s-const-import(imp): imp.mod else: "mod-import" end
      imp-name = A.s-underscore(l)
      info-key = U.import-to-dep(imp).key()
      mod-info = env.mods.get(info-key)
      cases(Option<C.Provides>) mod-info:
        | none => raise("No compile-time information provided for module " + info-key)
        | some(provides) =>
          val-names = provides.values.map-keys(A.s-name(l, _))
          type-names = provides.aliases.map-keys(A.s-name(l, _))
          # TODO (Philip): What to do about provides.modules? 
          A.s-import-complete(l, val-names, type-names, imp, imp-name, imp-name)
      end
    | s-import-complete(_, _, _, _, _, _) => imp
  end
end

fun desugar-toplevel-types(stmts :: List<A.Expr>) -> List<A.Expr> block:
  doc: ```
       Treating stmts as a toplevel block, hoist any type-lets or newtype declarations
       to the top, turning them into a type-let-expression, and generate newtypes for all
       data expressions.
       ```
  var rev-type-binds = empty
  var rev-stmts = empty
  for lists.each(s from stmts):
    cases(A.Expr) s block:
      | s-type(l, name, params, ann) =>
        rev-stmts := link(s, rev-stmts)
      | s-newtype(l, name, namet) =>
        rev-type-binds := link(A.s-newtype-bind(l, name, namet), rev-type-binds)
      | s-data(l, name, params, mixins, variants, shared, _check-loc, _check) =>
        namet = names.make-atom(name)
        rev-type-binds := link(A.s-newtype-bind(l, A.s-name(l, name), namet), rev-type-binds)
        rev-stmts := link(A.s-data-expr(l, name, namet, params, mixins, variants, shared, _check-loc, _check), rev-stmts)
      | else =>
        rev-stmts := link(s, rev-stmts)
    end
  end
  if is-empty(rev-type-binds):
    stmts
  else:
    type-binds = rev-type-binds.reverse()
    new-stmts = rev-stmts.reverse()
    [list: A.s-type-let-expr(type-binds.first.l, type-binds,
        A.s-block(type-binds.first.l, new-stmts), is-link(new-stmts.rest))]
  end
end

is-s-contract = A.is-s-contract
type Contract = A.Expr%(is-s-contract)

# Note: binds are maintained in reversed order, for efficiency in adding new items to them
# They get reversed back to correct order in bind-wrap.
data BindingGroup:
  | let-binds(contracts :: List<Contract>, binds :: List<A.LetBind>)
  | letrec-binds(contracts :: List<Contract>, binds :: List<A.LetrecBind>)
  | type-let-binds(binds :: List<A.TypeLetBind>)
end

var errors = empty # THE MUTABLE LIST OF ERRORS

fun weave-contracts(contracts, rev-binds) block:
  # When weaving contracts, ensure that the contract location is before the definition
  # or else give a well-formedness error.
  # NOTE(Ben): This code is polymorphic over LetBind and LetrecBind
  contracts-sd = [SD.mutable-string-dict: ]
  for each(c from contracts):
    name = c.name.toname()
    cases(Option) contracts-sd.get-now(name):
      | none =>
        contracts-sd.set-now(name, c)
      | some(c2) =>
        errors := link(C.contract-redefined(c.l, name, c2.l), errors)
    end
  end
  fun rebuild-bind(bind, new-b, new-v):
    if      A.is-s-let-bind(bind): A.s-let-bind(bind.l, new-b, new-v)
    else if A.is-s-var-bind(bind): A.s-var-bind(bind.l, new-b, new-v)
    else if A.is-s-letrec-bind(bind): A.s-letrec-bind(bind.l, new-b, new-v)
    end
  end
  fun names-match(funargs :: List<A.Bind>, annargs :: List<A.AField>):
    if is-empty(funargs) and is-empty(annargs): true
    else if is-empty(funargs) or is-empty(annargs): false
    else:
      (funargs.first.id.toname() == annargs.first.name) and names-match(funargs.rest, annargs.rest)
    end
  end
  fun fun-to-lam(bind):
    new-v = cases(A.Expr) bind.value:
      | s-fun(l-fun, name, params, args, ret, doc, body, _check-loc, _check, blocky) =>
        A.s-lam(l-fun, name, params, args, ret, doc, body, _check-loc, _check, blocky)
      | else => bind.value
    end
    if      A.is-s-let-bind(bind): A.s-let-bind(bind.l, bind.b, new-v)
    else if A.is-s-var-bind(bind): A.s-var-bind(bind.l, bind.b, new-v)
    else if A.is-s-letrec-bind(bind): A.s-letrec-bind(bind.l, bind.b, new-v)
    end
  end
    
  ans = for fold(acc from empty, bind from rev-binds):
    cases(A.Bind) bind.b:
      | s-bind(l, shadows, id, ann) =>
        id-name = id.toname()
        cases(Option) contracts-sd.get-now(id-name) block:
          | none => link(fun-to-lam(bind), acc)
          | some(c) =>
            contracts-sd.remove-now(id-name)
            if A.is-a-blank(ann) block:
              if not(c.l.before(bind.value.l)) block:
                errors := link(C.contract-bad-loc(c.l, id-name, bind.value.l), errors)
                link(fun-to-lam(bind), acc)
              else:
                cases(A.Expr) bind.value:
                  | s-fun(l-fun, name, params, args, ret, doc, body, _check-loc, _check, blocky) =>
                    if not(args.all({(a): A.is-a-blank(a.ann)}) and A.is-a-blank(ret)) block:
                      errors := link(C.contract-redefined(c.l, id-name, l-fun), errors)
                      link(fun-to-lam(bind), acc)
                    else if A.is-a-arrow(c.ann):
                      if c.ann.args.length() <> args.length() block:
                        errors := link(C.contract-inconsistent-names(c.l, id-name, l-fun), errors)
                        link(fun-to-lam(bind), acc)
                      else:
                        new-args = for map2(a from args, shadow ann from c.ann.args):
                          A.s-bind(a.l, a.shadows, a.id, ann)
                        end
                        link(
                          rebuild-bind(bind,
                            bind.b,
                            A.s-lam(l, name, params, new-args, c.ann.ret, doc, body, _check-loc, _check, blocky)),
                          acc)
                      end
                    else if A.is-a-arrow-argnames(c.ann):
                      if not(names-match(args, c.ann.args)) block:
                        errors := link(C.contract-inconsistent-names(c.l, id-name, l), errors)
                        link(fun-to-lam(bind), acc)
                      else:
                        new-args = for map2(a from args, shadow ann from c.ann.args):
                          A.s-bind(a.l, a.shadows, a.id, ann.ann)
                        end
                        link(
                          rebuild-bind(bind,
                            bind.b,
                            A.s-lam(l, name, params, new-args, c.ann.ret, doc, body, _check-loc, _check, blocky)),
                          acc)                        
                      end
                    else:
                      errors := link(C.contract-non-function(c.l, id-name, l, true), errors)
                      link(fun-to-lam(bind), acc)
                    end
                  | else =>
                    if A.is-a-arrow(c.ann) or A.is-a-arrow-argnames(c.ann) block:
                      errors := link(C.contract-non-function(c.l, id-name, bind.value.l, false), errors)
                      link(bind, acc)
                    else:
                      link(rebuild-bind(bind, A.s-bind(l, shadows, id, c.ann), bind.value), acc)
                    end
                end
              end
            else:
              errors := link(C.contract-redefined(c.l, id-name, l), errors)
              link(fun-to-lam(bind), acc)
            end
        end
      | else => link(bind, acc)
    end
  end
  contracts-sd.each-key-now(lam(c-name):
      c = contracts-sd.get-value-now(c-name)
      errors := link(C.contract-unused(c.l, c-name), errors)
    end)
  ans
end

fun bind-wrap(bg, expr) -> A.Expr:
  cases(List) bg.binds block:
    | empty =>
      for each(c from bg.contracts):
        errors := link(C.contract-unused(c.l, c.name.toname()), errors)
      end
      expr
    | else =>
      cases(BindingGroup) bg:
        | let-binds(contracts, rev-binds) =>
          A.s-let-expr(rev-binds.first.l, weave-contracts(contracts, rev-binds), expr, false)
        | letrec-binds(contracts, rev-binds) =>
          A.s-letrec(rev-binds.first.l, weave-contracts(contracts, rev-binds), expr, false)
        | type-let-binds(rev-binds) =>
          A.s-type-let-expr(rev-binds.first.l, rev-binds.reverse(), expr, false)
      end
  end
end

fun add-letrec-bind(bg :: BindingGroup, lrb :: A.LetrecBind, stmts :: List<A.Expr>) -> A.Expr:
  add-letrec-binds(bg, [list: lrb], stmts)
end

fun add-letrec-binds(bg :: BindingGroup, lrbs :: List<A.LetrecBind>, stmts :: List<A.Expr>) -> A.Expr:
  cases(BindingGroup) bg:
    | letrec-binds(contracts, binds) =>
      desugar-scope-block(stmts, letrec-binds(contracts, lrbs + binds))
    | else =>
      bind-wrap(bg, desugar-scope-block(stmts, letrec-binds(empty, lrbs)))
  end
end

fun simplify-let-bind(rebuild, l, bind, expr, lbs :: List<A.LetBind>) -> List<A.LetBind>:
  cases(A.Bind) bind:
    | s-bind(_,_,_,_) => rebuild(l, bind, expr) ^ link(_, lbs)
    | s-tuple-bind(lb, fields, as-name) =>
      {bound-expr; binding} = cases(Option) as-name:
        | none =>
          name = names.make-atom("tup")
          ann = A.a-tuple(lb, for map(f from fields):
              cases(A.Bind) f:
                | s-bind(_, _, _, a) => a
                | s-tuple-bind(_, _, _) => A.a-blank
              end
            end)
          {A.s-id(lb, name); rebuild(lb, A.s-bind(lb, false, name, ann), expr)}
        | some(b) =>
          binding = cases(A.Ann) b.ann:
            | a-blank =>
              # just create a tuple of the correct arity; leave the field annotations to be checked later
              ann = A.a-tuple(lb, for map(_ from fields): A.a-blank end)
              A.s-bind(b.l, b.shadows, b.id, ann)
            | else => b
          end
          {A.s-id(b.l, b.id); rebuild(l, binding, expr)}
      end
      for lists.fold_n(n from 0, shadow lbs from binding ^ link(_, lbs), f from fields):
        simplify-let-bind(rebuild, f.l, f, A.s-tuple-get(f.l, bound-expr, n, f.l), lbs)
      end
  end
end

fun add-let-binds(bg :: BindingGroup, lbs :: List<A.LetBind>, stmts :: List<A.Expr>) -> A.Expr:
  simplified-lbs = for fold(acc from empty, lb from lbs):
    cases(A.LetBind) lb:
      | s-let-bind(l, b, value) => simplify-let-bind(A.s-let-bind, l, b, value, acc)
      | s-var-bind(l, b, value) => simplify-let-bind(A.s-var-bind, l, b, value, acc)
    end
  end
  cases(BindingGroup) bg:
    | let-binds(contracts, binds) =>
      desugar-scope-block(stmts, let-binds(contracts, simplified-lbs + binds))
    | else =>
      bind-wrap(bg, desugar-scope-block(stmts, let-binds(empty, simplified-lbs)))
  end
end

fun add-let-bind(bg :: BindingGroup, lb :: A.LetBind, stmts :: List<A.Expr>) -> A.Expr:
  add-let-binds(bg, [list: lb], stmts)
end

fun add-type-let-bind(bg :: BindingGroup, tlb :: A.TypeLetBind, stmts :: List<A.Expr>) -> A.Expr:
  cases(BindingGroup) bg:
    | type-let-binds(binds) =>
      desugar-scope-block(stmts, type-let-binds(link(tlb, binds)))
    | else =>
      bind-wrap(bg, desugar-scope-block(stmts, type-let-binds(link(tlb, empty))))
  end
end

fun add-contracts(bg :: BindingGroup, cs :: List<Contract>, stmts :: List<A.Expr>) -> A.Expr:
  # The type of the next statement determines which binding group this contract belongs in
  cases(List) stmts block:
    | empty =>
      # NOTE(Ben): would rather raise an informative error than a "no cases matched" error,
      # if somehow this invariant is violated
      raise("Impossible: well-formedness prohibits contracts being last in block (at " + to-string(cs.first.l) + ")")
    | link(first, rest) =>
      # Construct the appropriate binding group (containing just the initial contract)
      # depending on the next binding: mimics the cases logic in desugar-scope-block, but defers
      # all actual processing to that function
      if A.is-s-rec(first) or A.is-s-fun(first) or A.is-s-data-expr(first) or A.is-s-check(first):
        if is-letrec-binds(bg): # keep the current binding group going
          desugar-scope-block(stmts, letrec-binds(cs + bg.contracts, bg.binds))
        else:
          bind-wrap(bg, desugar-scope-block(stmts, letrec-binds(cs, empty)))
        end
      else:
        if is-let-binds(bg): # keep the current binding group going
          desugar-scope-block(stmts, let-binds(cs + bg.contracts, bg.binds))
        else:
          bind-wrap(bg, desugar-scope-block(stmts, let-binds(cs, empty)))
        end
      end
  end
end


fun desugar-scope-block(stmts :: List<A.Expr>, binding-group :: BindingGroup) -> A.Expr:
  doc: ```
       Treating stmts as a block, resolve scope.
       There should be no blocks left after this stage of the compiler pipeline.
       ```
  cases(List) stmts:
    | empty => raise("Should not get an empty block in desugar-scope-block")
    | link(f, rest-stmts) =>
      cases(A.Expr) f:
        | s-type(l, name, params, ann) =>
          add-type-let-bind(binding-group, A.s-type-bind(l, name, params, ann), rest-stmts)
        | s-contract(l, name, ann) =>
          {contracts; shadow rest-stmts} = L.take-while(A.is-s-contract, rest-stmts)
          add-contracts(binding-group, link(f, contracts), rest-stmts)
        | s-let(l, bind, expr, _) =>
          add-let-bind(binding-group, A.s-let-bind(l, bind, expr), rest-stmts)
        | s-var(l, bind, expr) =>
          add-let-bind(binding-group, A.s-var-bind(l, bind, expr), rest-stmts)
        | s-rec(l, bind, expr) =>
          add-letrec-bind(binding-group, A.s-letrec-bind(l, bind, expr), rest-stmts)
        | s-fun(l, name, params, args, ann, doc, body, _check-loc, _check, blocky) =>
          add-letrec-bind(binding-group, A.s-letrec-bind(
              l,
              A.s-bind(l, false, A.s-name(l, name), A.a-blank),
              # NOTE(Ben): deliberately keeping this as an s-fun;
              # it'll get turned into an s-lam in weave-contracts
              f
              ), rest-stmts)
        | s-data-expr(l, name, namet, params, mixins, variants, shared, _check-loc, _check) =>
          fun b(loc, id :: String): A.s-bind(loc, false, A.s-name(loc, id), A.a-blank) end
          fun bn(loc, n :: A.Name): A.s-bind(loc, false, n, A.a-blank) end
          fun variant-binds(data-blob-id, v):
            vname = v.name
            checker-name = A.make-checker-name(vname)
            get-part = A.s-dot(v.l, data-blob-id, _)
            [list:
              A.s-letrec-bind(v.l, b(v.l, vname), get-part(vname)),
              A.s-letrec-bind(v.l, b(v.l, checker-name), get-part(checker-name))
            ]
          end
          blob-id = names.make-atom(name)
          data-expr = A.s-data-expr(l, name, namet, params, mixins, variants, shared, _check-loc, _check)
          bind-data = A.s-letrec-bind(l, bn(l, blob-id), data-expr)
          bind-data-pred = A.s-letrec-bind(l, b(l, A.make-checker-name(name)), A.s-dot(l, A.s-id-letrec(l, blob-id, true), name))
          all-binds = for fold(acc from [list: bind-data-pred, bind-data], v from variants):
            variant-binds(A.s-id-letrec(v.l, blob-id, true), v) + acc
          end
          add-letrec-binds(binding-group, all-binds, rest-stmts)
        | s-check(l, name, body, keyword) =>
          fun b(loc): A.s-bind(loc, false, A.s-underscore(l), A.a-blank) end
          add-letrec-binds(binding-group, [list: A.s-letrec-bind(l, b(l), A.s-check(l, name, body, keyword))], rest-stmts)
        | else =>
          cases(List) rest-stmts:
            | empty => bind-wrap(binding-group, f)
            | link(_, _) =>
              rest-stmt = desugar-scope-block(rest-stmts, let-binds(empty, empty))
              shadow rest-stmts = cases(A.Expr) rest-stmt:
                | s-block(_, shadow stmts) => link(f, stmts)
                | else => [list: f, rest-stmt]
              end
              bind-wrap(binding-group, A.s-block(f.l, rest-stmts))
          end
      end
  end
where:
  dsb = desugar-scope-block(_, let-binds(empty, empty))
  p = lam(str): PP.surface-parse(str, "test").block end
  d = A.dummy-loc
  b = lam(s): A.s-bind(d, false, A.s-name(d, s), A.a-blank) end
  id = lam(s): A.s-id(d, A.s-name(d, s)) end
  bk = lam(e): A.s-block(d, [list: e]) end
  bs = lam(str):
    dsb(p(str).stmts).visit(A.dummy-loc-visitor)
  end
  n = none
  thunk = lam(e): A.s-lam(d, "", [list: ], [list: ], A.a-blank, "", bk(e), n, n, false) end


  compare1 =
    A.s-let-expr(d, [list: A.s-let-bind(d, b("x"), A.s-num(d, 15)),
      A.s-let-bind(d, b("y"), A.s-num(d, 10))],
    id("y"), false)
  dsb(p("x = 15 y = 10 y").stmts).visit(A.dummy-loc-visitor)
    is compare1

  dsb(p("x = 55 var y = 10 y").stmts).visit(A.dummy-loc-visitor)
    is A.s-let-expr(d, [list: A.s-let-bind(d, b("x"), A.s-num(d, 55)),
      A.s-var-bind(d, b("y"), A.s-num(d, 10))], id("y"), false)

  bs("x = 7 print(2) var y = 10 y") is
  A.s-let-expr(d, [list:A.s-let-bind(d, b("x"), A.s-num(d, 7))],
    A.s-block(d, [list:
        A.s-app(d, id("print"), [list:A.s-num(d, 2)]),
        A.s-let-expr(d, [list:A.s-var-bind(d, b("y"), A.s-num(d, 10))],
          id("y"), false)
      ]), false)

  prog = bs("fun f(): 4 end fun g(): 5 end f()")
  prog is A.s-letrec(d, [list:
      A.s-letrec-bind(d, b("f"), thunk(A.s-num(d, 4))),
      A.s-letrec-bind(d, b("g"), thunk(A.s-num(d, 5)))
    ],
    A.s-app(d, id("f"), [list: ]), false)

  p-s = lam(e): A.s-app(d, id("print"), [list: e]) end
  pretty = lam(e): e.tosource().pretty(80).join-str("\n") end

  prog2 = bs("print(1) fun f(): 4 end fun g(): 5 end fun h(): 6 end x = 3 print(x)")
  prog2 is A.s-block(d,
    [list: p-s(A.s-num(d, 1)),
      A.s-letrec(d, [list:
          A.s-letrec-bind(d, b("f"), thunk(A.s-num(d, 4))),
          A.s-letrec-bind(d, b("g"), thunk(A.s-num(d, 5))),
          A.s-letrec-bind(d, b("h"), thunk(A.s-num(d, 6)))
        ],
        A.s-let-expr(d, [list: A.s-let-bind(d, b("x"), A.s-num(d, 3))], p-s(id("x")), false),
        false)])

  dsb([list: prog2]) is prog2
  for each2(p1 from dsb(prog2.stmts).stmts, p2 from prog2.stmts):
    p1.visit(A.dummy-loc-visitor) is p2
  end

  prog3 = bs("print(x) x := 3 print(x)")
  prog3 is A.s-block(d,
    [list:
      p-s(id("x")),
      A.s-assign(d, A.s-name(d, "x"), A.s-num(d, 3)),
      p-s(id("x"))
    ])
  
  prog4 = bs("var x = 10 fun f(): 4 end f()")
  prog4
    is A.s-let-expr(d, [list:
      A.s-var-bind(d, b("x"), A.s-num(d, 10))
    ],
    A.s-letrec(d, [list:
        A.s-letrec-bind(d, b("f"), thunk(A.s-num(d, 4)))
      ],
      A.s-app(d, id("f"), [list: ]), false),
    false
    )

  #prog5 = bs("data List: empty | link(f, r) end empty")
  #prog5.stmts.length() is 1
  #the-let = prog5.stmts
  #the-let satisfies A.is-s-let-expr
  #the-let.binds.length() is 6 # ListB, emptyB, linkB, List, is-empty, is-link
  #the-let.binds.take(3).map(_.value) satisfies lists.all(lam(e): A.is-s-app(e) and (e._fun.id == "brander") end, _)
  #the-let.binds.drop(3).map(_.value) satisfies lists.all(lam(e): A.is-s-dot(e) and (e.field == "test") end, _)
  #the-letrec = the-let.body
  #the-letrec satisfies A.is-s-letrec
  #the-letrec.binds.length() is 4 # emptyDict, linkDict, empty, link

end

fun rebuild-fun(rebuild, visitor, l, name, params, args, ann, doc, body, _check-loc, _check, blocky):
  v-params = params.map(_.visit(visitor))
  v-ann = ann.visit(visitor)
  v-body = body.visit(visitor)
  v-check = visitor.option(_check)
  placeholder = A.s-str(l, "placeholder")
  {new-binds; new-body} = for fold(acc from {empty; v-body}, a from args):
    {new-binds; new-body} = acc
    lbs = simplify-let-bind(A.s-let-bind, a.l, a.visit(visitor), placeholder, empty).reverse()
    arg-bind = lbs.first
    shadow new-binds = arg-bind.b ^ link(_, new-binds)
    cases(List) lbs.rest:
      | empty => {new-binds; new-body}
      | link(_, _) => {new-binds; A.s-let-expr(a.l, lbs.rest, new-body, false)}
    end
  end  
  rebuild(l, name, v-params, new-binds.reverse(), v-ann, doc, new-body, _check-loc, v-check, blocky)
end

desugar-scope-visitor = A.default-map-visitor.{
  method s-block(self, l, stmts):
    desugar-scope-block(stmts.map(_.visit(self)), let-binds(empty, empty))
  end,
  method s-let-expr(self, l, binds, body, blocky):
    v-body = body.visit(self)
    new-binds = for fold(new-binds from empty, b from binds):
      simplify-let-bind(A.s-let-bind, b.l, b.b.visit(self), b.value.visit(self), new-binds)
    end
    A.s-let-expr(l, new-binds.reverse(), v-body, blocky)
  end,
  method s-for(self, l, iterator, bindings, ann, body, blocky):
    v-iterator = iterator.visit(self)
    v-ann = ann.visit(self)
    v-body = body.visit(self)
    {new-binds; new-body} = for fold(acc from {empty; v-body}, b from bindings):
      {new-binds; new-body} = acc
      lbs = simplify-let-bind(A.s-let-bind, b.l, b.bind.visit(self), b.value.visit(self), empty).reverse()
      arg-bind = lbs.first
      shadow new-binds = A.s-for-bind(b.l, arg-bind.b, arg-bind.value) ^ link(_, new-binds)
      cases(List) lbs.rest:
        | empty => {new-binds; new-body}
        | link(_, _) => {new-binds; A.s-let-expr(b.l, lbs.rest, new-body, false)}
      end
    end
    A.s-for(l, v-iterator, new-binds.reverse(), v-ann, new-body, blocky)
  end,
  method s-cases-branch(self, l, pat-loc, name, args, body):
    v-body = body.visit(self)
    {new-binds; new-body} = for fold(acc from {empty; v-body}, b from args):
      {new-binds; new-body} = acc
      lbs = simplify-let-bind(A.s-let-bind, b.l, b.bind.visit(self), A.s-str(b.l, "placeholder"), empty).reverse()
      arg-bind = lbs.first
      shadow new-binds = A.s-cases-bind(b.l, b.field-type, arg-bind.b) ^ link(_, new-binds)
      cases(List) lbs.rest:
        | empty => {new-binds; new-body}
        | link(_, _) => {new-binds; A.s-let-expr(b.l, lbs.rest, new-body, false)}
      end
    end
    A.s-cases-branch(l, pat-loc, name, new-binds.reverse(), new-body)
  end,
  method s-fun(self, l, name, params, args, ann, doc, body, _check-loc, _check, blocky):
    rebuild-fun(A.s-fun, self, l, name, params, args, ann, doc, body, _check-loc, _check, blocky)
  end,
  method s-lam(self, l, name, params, args, ann, doc, body, _check-loc, _check, blocky):
    rebuild-fun(A.s-lam, self, l, name, params, args, ann, doc, body, _check-loc, _check, blocky)
  end,
  method s-method(self, l, name, params, args, ann, doc, body, _check-loc, _check, blocky):
    rebuild-fun(A.s-method, self, l, name, params, args, ann, doc, body, _check-loc, _check, blocky)
  end,
  method s-method-field(self, l, name, params, args, ann, doc, body, _check-loc, _check, blocky):
    rebuild-fun(A.s-method-field, self, l, name, params, args, ann, doc, body, _check-loc, _check, blocky)
  end
}

fun desugar-scope(prog :: A.Program, env :: C.CompileEnvironment) -> C.ScopeResolution:
  doc: ```
       Remove x = e, var x = e, tuple bindings, and fun f(): e end
       and turn them into explicit let and letrec expressions.
       Do this recursively through the whole program.
       Preconditions on prog:
         - well-formed
       Postconditions on prog:
         - contains no s-provide in headers
         - contains no s-let, s-var, s-data, s-tuple-bind
       ```
  cases(A.Program) prog block:
    | s-program(l, _provide-raw, provide-types-raw, provide-modules-raw, imports-raw, body) =>
      imports = imports-raw.map(lam(i): expand-import(i, env) end)
      str = A.s-str(l, _)
      resolved-provides = resolve-provide(_provide-raw, body)
      resolved-type-provides = resolve-type-provide(provide-types-raw, body)
      # TODO: Need to resolve provide-types here
      with-imports = cases(A.Expr) body:
        | s-block(l2, stmts) =>
          A.s-block(l2, desugar-toplevel-types(stmts))
        | else => A.s-block(l, desugar-toplevel-types([list: body]))
      end
      fun transform-toplevel-last(l2, last):
        A.s-module(l2, last, empty, empty, empty, A.s-app(l2, A.s-dot(l2, U.checkers(l2), "results"), empty))
      end
      with-provides = cases(A.Expr) with-imports:
        | s-block(l2, stmts) =>
          last = stmts.last()
          cases(A.Expr) last:
            | s-type-let-expr(l3, binds, body2, blocky) =>
              inner-last = body2.stmts.last()
              A.s-block(l2,
                stmts.take(stmts.length() - 1) + [list:
                  A.s-type-let-expr(l3, binds,
                    A.s-block(body2.l, body2.stmts.take(body2.stmts.length() - 1)
                        + [list: transform-toplevel-last(l3, inner-last)]),
                    true)])
            | else =>
              A.s-block(l2, stmts.take(stmts.length() - 1) + [list: transform-toplevel-last(l2, last)])
          end
        | else => raise("Impossible")
      end
      
      errors := empty
      {initial-contracts; stmts} = L.take-while(A.is-s-contract, with-provides.stmts)
      all-imported-names = [SD.mutable-string-dict: ]
      for each(im from imports):
        for each(v from im.values):
          all-imported-names.set-now(v.toname(), im.import-type)
        end
      end
      remaining = for filter(c from initial-contracts):
        c-name = c.name.toname()
        cases(Option) all-imported-names.get-now(c-name) block:
          | none => true # keep this; it may be needed below
          | some(im-type) =>
            errors := link(C.contract-on-import(c.l, c-name, im-type), errors)
            false
        end
      end
      recombined = A.s-block(with-provides.l, remaining + stmts)
      visited = recombined.visit(desugar-scope-visitor)

      resulting-program = A.s-program(l, resolved-provides, resolved-type-provides, resolve-module-provide(provide-modules-raw, imports), imports, visited)

      C.resolved-scope(resulting-program, errors)

  end

where:
  d = A.dummy-loc
  b = lam(s): A.s-bind(d, false, A.s-name(d, s), A.a-blank) end
  id = lam(s): A.s-id(d, A.s-name(d, s)) end
  checks = A.s-app(d, A.s-dot(d, U.checkers(d), "results"), [list: ])
  str = A.s-str(d, _)
  ds = lam(prog): desugar-scope(prog, C.standard-builtins).ast.visit(A.dummy-loc-visitor) end
  compare1 = A.s-program(d, A.s-provide-none(d), A.s-provide-types-none(d), A.s-provide-modules-none(d), [list: ],
        A.s-let-expr(d, [list:
            A.s-let-bind(d, b("x"), A.s-num(d, 10))
          ],
          A.s-module(d, id("nothing"), empty, empty, empty, checks), false)
      )
  # NOTE(joe): Explicit nothing here because we expect to have
  # had append-nothing-if-necessary called
  ds(PP.surface-parse("provide x end x = 10 nothing", "test")) #is compare1
end


fun get-origin-loc(o):
  cases(C.BindOrigin) o:
    | bo-local(l) => l
    | bo-module(_, uri) => S.builtin(uri)
  end
end

# TODO (Philip): Use this or something like it in resolve-names
# FIXME: Maybe not needed?
data BoundName<a>:
  | bn-value(name-key :: a) with:
    method key(self :: BoundName<String>):
      self.name-key + "#value"
    end,
    method other-names(self):
      # TODO: Remove
      [list: bn-type(self.name-key), bn-module(self.name-key)]
    end,
    method map(self, f):
      bn-value(f(self.name-key))
    end,
    method rename(self, s):
      bn-value(s)
    end
  | bn-type(name-key :: a) with:
    method key(self :: BoundName<String>):
      self.name-key + "#type"
    end,
    method other-names(self):
      # TODO: Remove
      [list: bn-value(self.name-key), bn-module(self.name-key)]
    end,
    method map(self, f):
      bn-type(f(self.name-key))
    end,
    method rename(self, s):
      bn-type(s)
    end
  | bn-module(name-key :: a) with:
    method key(self :: BoundName<String>):
      self.name-key + "#module"
    end,
    method other-names(self):
      # TODO: Remove
      [list: bn-value(self.name-key), bn-type(self.name-key)]
    end,
    method map-name(self, f):
      bn-module(f(self.name-key))
    end,
    method rename(self, s):
      bn-module(s)
    end
end

data NamespacedBinding:
  | nb-value(bind :: C.ValueBind) with:
    method is-compatible-with(self, other :: NamespacedBinding):
      is-nb-value(other)
    end,
    method get-compatability-error(self, loc, other :: NamespacedBinding):
      cases(NamespacedBinding) other:
        | nb-value(_) => none
        | nb-type(b) => some(C.value-id-used-as-type(loc, b.atom))
        | nb-module(b) => some(C.value-id-used-as-module(loc, b.atom))
      end
    end,
    method info-str(self):
      "<Value Binding: " + torepr(self.bind) + ">"
    end
  | nb-type(bind :: C.TypeBind) with:
    method is-compatible-with(self, other :: NamespacedBinding):
      is-nb-type(other)
    end,
    method get-compatability-error(self, loc, other :: NamespacedBinding):
      cases(NamespacedBinding) other:
        | nb-type(_) => none
        | nb-value(b) => some(C.type-id-used-as-value(loc, b.atom))
        | nb-module(b) => some(C.type-id-used-as-module(loc, b.atom))
      end
    end,
    method info-str(self):
      "<Type Binding: " + torepr(self.bind) + ">"
    end
  | nb-module(bind :: C.ModuleBind) with:
    method is-compatible-with(self, other :: NamespacedBinding):
      is-nb-module(other)
    end,
    method get-compatability-error(self, loc, other :: NamespacedBinding):
      cases(NamespacedBinding) other:
        | nb-module(_) => none
        | nb-value(b) => some(C.module-id-used-as-value(loc, b.atom))
        | nb-type(b) => some(C.module-id-used-as-type(loc, b.atom))
      end
    end,
    method info-str(self):
      "<Module Binding: " + torepr(self.bind) + ">"
    end
sharing:
  method to-value-bind(self):
    if is-nb-value(self):
      some(self.bind)
    else:
      none
    end
  end,
  method to-type-bind(self):
    if is-nb-type(self):
      some(self.bind)
    else:
      none
    end
  end,
  method to-module-bind(self):
    if is-nb-module(self):
      some(self.bind)
    else:
      none
    end
  end
end

fun key-to-name(k :: String) -> BoundName:
  length = k.length()
  ask:
    | (length > 6) and (string-substring(k, length - 6, length) == "#value") then:
      bn-value(string-substring(k, length - 6, length))
    | (length > 5) and (string-substring(k, length - 5, length) == "#type") then:
      bn-type(string-substring(k, length - 5, length))
    | (length > 7) and (string-substring(k, length - 7, length) == "#module") then:
      bn-module(string-substring(k, length - 7, length))
    | otherwise: raise("Key does not correspond to bound name: " + k)
  end
end

data BoundNames:
  | bound-name-set(dict :: SD.MutableStringDict) with:
    method has-name-now(self, name :: BoundName):
      self.dict.has-key-now(name.key())
    end,
    method has-any-name-now(self, name :: String):
      self.dict.has-key-now(bn-value(name).key())
      or self.dict.has-key-now(bn-type(name).key())
      or self.dict.has-key-now(bn-module(name).key())
    end,
    method get-name-now(self, name :: BoundName):
      self.dict.get-now(name.key())
    end,
    method get-any-name-now(self, name :: String):
      self.dict.get-now(bn-value(name).key())
        .or-else(self.dict.get-now(bn-type(name).key()))
        .or-else(self.dict.get-now(bn-module(name).key()))
    end,
    method get-name-value-now(self, name :: BoundName):
      self.dict.get-value-now(name.key())
    end,
    method set-name-now(self, name :: BoundName, val) block:
      each(self.dict.remove-now, name.other-names().map(_.key()))
      self.dict.set-now(name.key(), val)
    end
end

data FrozenBoundNames:
  | frozen-bound-name-set(dict :: SD.StringDict) with:
    method has-name(self, name :: BoundName):
      self.dict.has-key(name.key())
    end,
    method has-any-name(self, name :: String):
      self.dict.has-key(bn-value(name).key())
      or self.dict.has-key(bn-type(name).key())
      or self.dict.has-key(bn-module(name).key())
    end,
    method get-name(self, name :: BoundName):
      self.dict.get(name.key())
    end,
    method get-any-name(self, name :: String):
      self.dict.get(bn-value(name).key())
        .or-else(self.dict.get(bn-type(name).key()))
        .or-else(self.dict.get(bn-module(name).key()))
    end,
    method get-name-value(self, name :: BoundName):
      self.dict.get-value(name.key())
    end,
    method set-name(self, name :: BoundName, val):
      frozen-bound-name-set(
        fold(_.remove, self.dict, name.other-names().map(_.key()))
          .set(name.key(), val))
    end,
    method handle-id(self, l, id):
      cases(A.Name) id:
        | s-atom(_, _) => id
        | s-underscore(_) => id
        | s-name(l2, s) =>
          if self.has-name(bn-value(s)):
            bn-value(self.get-name-value(bn-value(s)))
          else if self.has-name(bn-module(s)):
            bn-module(self.get-name-value(bn-module(s)))
          else if self.has-name(bn-type(s)):
            bn-type(self.get-name-value(bn-type(s)))
          else:
            names.s-global(s)
          end
        | else => raise("Wasn't expecting a non-s-name in resolve-names id: " + torepr(id))
      end
    end
end

fun split-bindings-now(bindings :: SD.MutableStringDict<NamespacedBinding>) -> {values :: SD.MutableStringDict<C.ValueBind>, types :: SD.MutableStringDict<C.TypeBind>, modules :: SD.MutableStringDict<C.ModuleBind>} block:
  doc: ```Splits the given binding dictionary into bindings in different namespaces.```
  ret = {
    values: SD.make-mutable-string-dict(),
    types: SD.make-mutable-string-dict(),
    modules: SD.make-mutable-string-dict()
  }
  for each(key from bindings.keys-list-now()):
    cases(NamespacedBinding) bindings.get-value-now(key):
      | nb-value(bind) => ret.values.set-now(key, bind)
      | nb-type(bind) => ret.types.set-now(key, bind)
      | nb-module(bind) => ret.modules.set-now(key, bind)
    end
  end
  ret
end

fun split-bindings(bindings :: SD.StringDict<NamespacedBinding>) -> {values :: SD.StringDict<C.ValueBind>, types :: SD.StringDict<C.TypeBind>, modules :: SD.StringDict<C.ModuleBind>} block:
  doc: ```Splits the given binding dictionary into bindings in different namespaces.```
  values = SD.make-mutable-string-dict()
  types = SD.make-mutable-string-dict()
  modules = SD.make-mutable-string-dict()
  for each(key from bindings.keys-list()):
    cases(NamespacedBinding) bindings.get-value(key):
      | nb-value(bind) => values.set-now(key, bind)
      | nb-type(bind) => types.set-now(key, bind)
      | nb-module(bind) => modules.set-now(key, bind)
    end
  end
  {
    values: values.freeze(),
    types: types.freeze(),
    modules: modules.freeze()
  }
end

fun safe-set-now(bindings :: SD.MutableStringDict, key :: String, bind :: NamespacedBinding, callback) block:
  cases(Option) bindings.get-now(key):
    | none => nothing
    | some(v) =>
      when not(v.is-compatible-with(bind)):
        if A.is-s-name(bind.bind.atom):
          v.get-compatibility-error(bind.bind.atom.l, bind).and-then(callback)
        else:
          # This should only happen if globals clash somehow
          raise("Incompatible bindings (did well-formedness fail?): " + v.info-str() + ", " + bind.info-str())
        end
      end
  end
  bindings.set-now(key, bind)
end

fun safe-set(bindings :: SD.StringDict, key :: String, bind :: NamespacedBinding, callback) block:
  cases(Option) bindings.get(key):
    | none => nothing
    | some(v) =>
      when not(bind.is-compatible-with(v)):
        if A.is-s-name(bind.bind.atom):
          v.get-compatibility-error(bind.bind.atom.l, bind).and-then(callback)
        else:
          # This should only happen if globals clash somehow
          raise("Incompatible bindings (did well-formedness fail?): " + v.info-str() + ", " + bind.info-str())
        end
      end
  end
  bindings.set(key, bind)
end

fun has-value-binding-now(bindings :: SD.MutableStringDict, key :: String):
  # nbind will be some(..) if in dict
  nbind = bindings.get-now(key)
  # vbind will be some(..) if in dict AND value binding
  vbind = nbind.and-then(_.to-value-bind())
  option.is-some(vbind)
end

fun has-type-binding-now(bindings :: SD.MutableStringDict, key :: String):
  # nbind will be some(..) if in dict
  nbind = bindings.get-now(key)
  # tbind will be some(..) if in dict AND type binding
  tbind = nbind.and-then(_.to-type-bind())
  option.is-some(tbind)
end

fun has-module-binding-now(bindings :: SD.MutableStringDict, key :: String):
  # nbind will be some(..) if in dict
  nbind = bindings.get-now(key)
  # mbind will be some(..) if in dict AND module binding
  mbind = nbind.and-then(_.to-module-bind())
  option.is-some(mbind)
end

fun resolve-names(p :: A.Program, initial-env :: C.CompileEnvironment) block:
  doc: ```
       Turn all s-names into s-atom or s-global
       Requires:
        1. desugar-scope
       Preconditions on p:
        -  Contains no s-block, s-let, s-var, s-data, s-rec
       Postconditions on p (in addition to preconditions):
        -  Contains no s-name in names
       ```
  var name-errors = [list: ]
  bound-names = bound-name-set(SD.make-mutable-string-dict())
  bindings = SD.make-mutable-string-dict()
  datatypes = SD.make-mutable-string-dict()
  module-bindings = SD.make-mutable-string-dict()
  provided-values = SD.make-mutable-string-dict()
  provided-types = SD.make-mutable-string-dict()
  provided-modules = SD.make-mutable-string-dict()

  fun add-name-error(e):
    name-errors := link(e, name-errors)
  end

  fun wrap-binding(b) -> NamespacedBinding:
    # This is admittedly a little gross, but it keeps the main visitor's
    # code significantly cleaner.
    ask:
      | C.is-ValueBind(b) then: nb-value(b)
      | C.is-TypeBind(b) then: nb-type(b)
      | C.is-ModuleBind(b) then: nb-module(b)
      | is-NamespacedBinding(b) then: b
      | otherwise: raise("Invalid binding: " + torepr(b))
    end
  end

  fun same-nb-type(nb1 :: NamespacedBinding, nb2 :: NamespacedBinding) -> Boolean:
    ask:
      | is-nb-value(nb1) and is-nb-value(nb2) then: true
      | is-nb-type(nb1) and is-nb-type(nb2) then: true
      | is-nb-module(nb1) and is-nb-module(nb2) then: true
      | otherwise: false
    end
  end

  fun make-anon-import-for(l, s, env, shadow bindings, b) block:
    atom = names.make-atom(s)
    safe-set-now(bindings, atom.key(), wrap-binding(b(atom)), add-name-error)
    { atom: atom, env: env }
  end
  fun make-atom-for(name, is-shadowing, env, shadow bindings, make-binding):
    shadow make-binding = lam(x): wrap-binding(make-binding(x)) end
    cases(A.Name) name block:
      | s-name(l, s) =>
        atom = names.make-atom(s)
        binding = make-binding(atom)

        # To count as a shadow error here, a name must be in the same namespace
        # subset (value/type/module), and be in the environment already, and
        # shadow-checking must be off

        when env.has-key(s) and
             not(is-shadowing) and
             same-nb-type(env.get-value(s), binding):

          old-loc = get-origin-loc(env.get-value(s).bind.origin)
          name-errors := link(C.shadow-id(s, l, old-loc), name-errors)

        end

        safe-set-now(bindings, atom.key(), binding, add-name-error)
        { atom: atom, env: env.set(s, binding) }
      | s-underscore(l) =>
        atom = names.make-atom("$underscore")
        safe-set-now(bindings, atom.key(), make-binding(atom), add-name-error)
        { atom: atom, env: env }
      # NOTE(joe): an s-atom is pre-resolved to all its uses, so no need to add
      # it or do any more work.
      | s-atom(_, _) =>
        binding = make-binding(name)
        # THIS LINE DOES NOTHING??
        # env.set(name.key(), binding)
        safe-set-now(bindings, name.key(), binding, add-name-error)
        { atom: name, env: env }
      | else => raise("Unexpected atom type: " + torepr(name))
    end
  end

  fun scope-env-from-env(initial :: C.CompileEnvironment) block:
    acc = SD.make-mutable-string-dict()
    for each(name from initial.globals.values.keys-list()):
      mod-info = initial.mods.get-value(initial.globals.values.get-value(name))
      val-info = mod-info.values.get(name)
      # TODO(joe): I am a little confused about how many times we are asserting
      # that something is bound here, in bindings vs. in the environment
      cases(Option) val-info block:
        | none =>
          # TODO(joe): hack hack hack
          when not(name == "_"):
            raise("The value is a global that doesn't exist in any module: " + name)
          end
          # b = global-bind(S.builtin(mod-info.from-uri), names.s-global(name), none)
          # bindings.set-now(names.s-global(name).key(), b)
          # acc.set-now(name, b)
        | some(shadow val-info) =>
          cases(C.ValueExport) val-info block:
            | v-var(t) =>
              b = nb-value(C.value-bind(C.bo-module(none, mod-info.from-uri), C.vb-var, names.s-global(name), A.a-blank, none))
              bindings.set-now(names.s-global(name).key(), b)
              acc.set-now(name, b)
            | else =>
              # TODO(joe): Good place to add _location_ to valueexport to report errs better
              b = nb-value(C.value-bind(C.bo-module(none, mod-info.from-uri), C.vb-let, names.s-global(name), A.a-blank, none))
              bindings.set-now(names.s-global(name).key(), b)
              acc.set-now(name, b)
          end
      end
    end
    for each(name from initial.globals.types.keys-list()) block:
      mod-info = initial.mods.get-value(initial.globals.types.get-value(name))
      tbind = nb-type(C.type-bind(C.bo-module(none, mod-info.from-uri), C.tb-type-let, names.s-type-global(name), none))
      acc.set-now(name, tbind)
    end
    for each(name from initial.globals.modules.keys-list()) block:
      mod-info = initial.mods.get-value(initial.globals.modules.get-value(name))
      mbind = nb-module(C.module-bind(C.bo-module(none, mod-info.from-uri), C.mb-resolved(mod-info), names.s-global(name)))
      acc.set-now(name, mbind)
    end
    acc.freeze()
  end


  fun resolve-letrec-binds(visitor, binds):
    {env; atoms} = for fold(acc from { visitor.env; empty }, b from binds):
      {env; atoms} = acc
      # TODO(joe): I think that b.b.ann.visit below could be wrong if
      # a letrec'd ID is used in a refinement within the same letrec,
      # so state may be necessary here
      atom-env = make-atom-for(b.b.id, b.b.shadows, env, bindings,
        C.value-bind(C.bo-local(b.l), C.vb-letrec, _, b.b.ann.visit(visitor), none))
      { atom-env.env; link(atom-env.atom, atoms) }
    end
    new-visitor = visitor.{env: env}
    visit-binds = for map2(b from binds, a from atoms.reverse()):
      cases(A.LetrecBind) b block:
        | s-letrec-bind(l2, bind, expr) =>
          new-bind = A.s-bind(l2, false, a, bind.ann.visit(visitor.{env: env}))
          visit-expr = expr.visit(new-visitor)
          A.s-letrec-bind(l2, new-bind, visit-expr)
      end
    end
    {
      visit-binds;
      new-visitor
    }
  end
  fun handle-id(env, l, id):
    cases(A.Name) id:
      | s-name(l2, s) =>
        if env.has-key(s):
          # TODO: extract nb-value
          bind = env.get-value(s)
          cases(Option) bind.to-value-bind() block:
            | none =>
              err = cases(NamespacedBinding) bind:
                | nb-value(_) => raise("Should be impossible")
                | nb-type(_) => C.type-id-used-as-value(l, id)
                | nb-module(_) => C.module-id-used-as-value(l, id)
              end
              name-errors := link(err, name-errors)
              bind.bind.atom
            | some(vb) => vb.atom
          end
        else:
          names.s-global(s)
        end
      | s-atom(_, _) => id
      | s-underscore(_) => id
      | else => raise("Wasn't expecting a non-s-name in resolve-names id: " + torepr(id))
    end
  end
  fun handle-ann(l, env, id):
    cases(A.Name) id:
      | s-name(_, s) =>
        if env.has-key(s):
          bind = env.get-value(s)
          cases(Option) bind.to-type-bind() block:
            | none =>
              err = cases(NamespacedBinding) bind:
                | nb-type(_) => raise("Should be impossible")
                | nb-value(_) => C.value-id-used-as-type(l, id)
                | nb-module(_) => C.module-id-used-as-type(l, id)
              end
              name-errors := link(err, name-errors)
              A.a-name(l, bind.bind.atom)
            | some(tb) =>
              name = tb.atom
              cases(C.TypeBinder) tb.binder block:
                | tb-type-let => A.a-name(l, name)
                | tb-type-var => A.a-type-var(l, name)
                | tb-module(_, _) => raise("Should be impossible")
              end
          end
        else:
          A.a-name(l, names.s-type-global(s))
        end
      | else => A.a-name(l, id)
    end
  end

  fun handle-column-binds(column-binds :: A.ColumnBinds, visitor):
    env-and-binds = for fold(acc from { env: visitor.env, cbs: [list: ] }, cb from column-binds.binds):
        atom-env = make-atom-for(cb.id, cb.shadows, acc.env, bindings,
          C.value-bind(C.bo-local(cb.l), C.vb-let, _, cb.ann.visit(visitor), none))
        new-cb = A.s-bind(cb.l, cb.shadows, atom-env.atom, cb.ann.visit(visitor.{env: acc.env}))
        { env: atom-env.env, cbs: link(new-cb, acc.cbs) }
      end
    { column-binds: A.s-column-binds(column-binds.l, env-and-binds.cbs, column-binds.table.visit(visitor)),
               env: env-and-binds.env }
  end

  # FIXME(Philip): It's great that we have a module-bindings dictionary,
  #   but it needs to play nicely with scoping rules; i.e.
  #   the following program should work (since it's well-formed):
  #
  #       import lists as L
  #       shadow L = 1
  #       L + 1
  #
  names-visitor = A.default-map-visitor.{
    env: scope-env-from-env(initial-env),
    method s-module(self, l, answer, _, _, _, checks) block: # provided-vals, provided-types
      var defined-vals = empty
      var defined-types = empty
      var defined-modules = empty
      fun get-defined-value(key, vb):
        atom = vb.atom
        pname = if provided-values.has-key-now(key): some(key) else: none end
        cases(C.ValueBinder) vb.binder block:
          | vb-let => A.s-defined-value(key, pname, A.s-id(l, atom))
          | vb-letrec => A.s-defined-value(key, pname, A.s-id-letrec(l, atom, true))
          | vb-var => A.s-defined-var(key, pname, atom)
          | vb-module(_, _) => raise("get-defined-value: Should be impossible (?) " + torepr(vb))
        end
      end
      fun get-defined-type(key, tb):
        atom = tb.atom
        pname = if provided-types.has-key-now(key): some(key) else: none end
        A.s-defined-type(key, pname, A.a-name(l, atom))
      end
      fun get-defined-module(key, mb):
        atom = mb.atom
        pname = if provided-modules.has-key-now(key): some(key) else: none end
        cases(C.ModuleBinder) mb.binder:
          | mb-module(dep, uri) => A.s-defined-module(key, pname, atom, dep.key())
          | mb-resolved(_) => raise("get-defined-module: Should be impossible " + torepr(mb))
        end
      end
      for each(k from self.env.keys-list()):
        cases(NamespacedBinding) self.env.get-value(k):
          | nb-value(vb) =>
            when C.is-bo-local(vb.origin):
              defined-vals := get-defined-value(k, vb) ^ link(_, defined-vals)
            end
          | nb-type(tb) =>
            when C.is-bo-local(tb.origin):
              defined-types := get-defined-type(k, tb) ^ link(_, defined-types)
            end
          | nb-module(mb) =>
            when C.is-bo-local(mb.origin):
              defined-modules := get-defined-module(k, mb) ^ link(_, defined-modules)
            end
        end
      end
      A.s-module(l, answer.visit(self), defined-vals, defined-types, defined-modules, checks.visit(self))
    end,
    method s-program(self, l, _provide, _provide-types, _provide-modules, imports, body) block:
      {imp-e; imp-imps} = for fold(acc from { self.env; empty }, i from imports):
        {imp-e; imp-imps} = acc
        cases(A.Import) i block:
          | s-import-complete(l2, vnames, tnames, file, name-vals, name-types) =>
            dep = U.import-to-dep(file)
            mod-info = initial-env.mods.get-value(dep.key())
            module-bindings.set-now(dep.key(), i)
#            when not(A.is-s-underscore(name-vals)):
#              print("Making atom import for: " + to-repr(i) + "\n")
#            end
            atom-env =
              if A.is-s-underscore(name-vals):
                make-anon-import-for(name-vals.l, "$import", imp-e, bindings,
                  C.module-bind(C.bo-local(name-vals.l), C.mb-module(dep, mod-info.from-uri), _))
              else:
                make-atom-for(name-vals, false, imp-e, bindings,
                  C.module-bind(C.bo-local(name-vals.l), C.mb-module(dep, mod-info.from-uri), _))
              end
            # TODO (Philip): How does this play with s-module-dot?
            {e; vn} = for fold(nv-v from {atom-env.env; empty}, v from vnames):
              {e; vn} = nv-v
              maybe-value-export = mod-info.values.get(v.toname())
              v-atom-env = cases(Option) maybe-value-export block:
                | some(value-export) =>
                  cases(C.ValueExport) value-export block:
                    | v-var(t) =>
                      make-atom-for(v, false, e, bindings,
                        C.value-bind(C.bo-module(some(file), mod-info.from-uri), C.vb-var, _, A.a-any(l2), none))
                    | else =>
                      make-atom-for(v, false, e, bindings,
                        C.value-bind(C.bo-module(some(file), mod-info.from-uri), C.vb-let, _, A.a-any(l2), none))
                  end
                | none =>
                  # NOTE(joe): This seems odd – just trusting a binding from another module that
                  # we don't know about statically?
                  # (Philip): Conjecture: This print message will show when importing a module which has exported a module
                  print("Trusting scary binding for " + torepr(v) + "\n")
                  make-atom-for(v, false, e, bindings,
                    C.value-bind(C.bo-module(some(file), mod-info.from-uri), C.vb-let, _, A.a-any(l2), none))
              end
              { v-atom-env.env; link(v-atom-env.atom, vn) }
            end
            {te; tn} = for fold(nv-t from {e; empty}, t from tnames):
              {te; tn} = nv-t
              t-atom-env = make-atom-for(t, false, te, bindings,
                C.type-bind(C.bo-module(some(file), mod-info.from-uri), C.tb-type-let, _, none))
              { t-atom-env.env; link(t-atom-env.atom, tn) }
            end
            new-header = A.s-import-complete(l2,
              vn,
              tn,
              file,
              atom-env.atom,
              atom-env.atom) # atom-env-t.atom
            { te; link(new-header, imp-imps) }
          | else => raise("Should only have s-import-complete when checking scope")
        end
      end

      for each(field from p._provide.block.fields):
        provided-values.set-now(field.name, none)
      end

      for each(ann from p.provided-types.ann):
        provided-types.set-now(ann.name, none)
      end

      for each(field from p.provided-modules.block.fields):
        provided-modules.set-now(field.name, none)
      end
      
      visit-body = body.visit(self.{
          env: imp-e
        })
      var vals = nothing
      var typs = nothing
      var mods = nothing
      visit-body.visit(A.default-iter-visitor.{
        method s-module(_, _, _, dv, dt, dm, _) block:
          vals := dv
          typs := dt
          mods := dm
          true
        end
      })
      provides-dict = cases(A.Provide) _provide block:
          # TODO(Philip): Need to incorporate modules into this somehow
          #         +---> Might be okay, actually. We can assume that this
          #               dict will contain any exported modules; additionally,
          #               we can probably make it a well-formedness error to
          #               explicitly place a module binding in here. (if we
          #               really want, we could add 'provide-modules' syntax as well)
          #               [Note that modules being provided primarily happens
          #                internally; it's not as if people are doing it.]
        | s-provide(_, obj) =>
          when not(A.is-s-obj(obj)): raise("Provides didn't look like an object" + torepr(_provide)) end
          for fold(pd from [SD.string-dict:], f from obj.fields):
            pd.set(f.name, true)
          end
        | else => raise("Should have been resolved: " + torepr(_provide))
      end
      provide-types-dict = cases(A.ProvideTypes) _provide-types block:
        | s-provide-types(_, fields) =>
          for fold(pd from [SD.string-dict:], f from fields):
            pd.set(f.name, true)
          end
        | else => raise("Should have been resolved: " + torepr(_provide-types))
      end
      get-dv-key = lam(dv):
        cases(A.DefinedValue) dv:
          | s-defined-value(n, _, v) => v.id.key()
          | s-defined-var(n, _, id) => id.key()
        end
      end
      # FIXME: vb-module stuff should be obsolete now
      val-defs = for lists.filter-map(dv from vals) block:
        v-binding = bindings.get-value-now(get-dv-key(dv)).bind
        if provides-dict.has-key(dv.name):
          cases(C.ValueBinder) v-binding.binder:
            | vb-module(dep, _) =>
              raise("Should be impossible")
            | else =>
              some(A.p-value(l, v-binding.atom, v-binding.ann))
          end
        else:
          none
        end
      end
      # some(E.right(A.p-module(l, v-binding.atom, module-bindings.get-value-now(dep.key()))))
      # Note that there is no 'provide-modules' syntax to process
      module-defs = for lists.filter-map(dm from mods) block:
        m-binding = bindings.get-value-now(dm.id.key()).bind
        if provides-dict.has-key(dm.name):
          some(A.p-module(l, m-binding.atom, module-bindings.get-value-now(dm.mod)))
        else:
          none
        end
      end
      alias-defs = for lists.filter-map(td from typs):
        t-binding = bindings.get-value-now(td.typ.id.key()).bind
        if provide-types-dict.has-key(td.name):
          some(A.p-alias(l, t-binding.atom, t-binding.atom, none))
        else:
          none
        end
      end
      data-defs = for lists.filter-map(ddk from datatypes.keys-list-now()):
        dd = datatypes.get-value-now(ddk) 
        if provide-types-dict.has-key(dd.name):
          some(A.p-data(dd.l, dd.namet, none))
        else:
          none
        end
      end
      one-true-provide = A.s-provide-complete(
        l,
        val-defs,
        alias-defs,
        data-defs,
        module-defs
      )

      A.s-program(l, one-true-provide, A.s-provide-types-none(l), A.s-provide-modules-none(l), imp-imps.reverse(), visit-body)
    end,
    method s-type-let-expr(self, l, binds, body, blocky):
      {e; bs} = for fold(acc from { self.env; empty }, b from binds):
        {e; bs} = acc
        cases(A.TypeLetBind) b block:
          | s-type-bind(l2, name, params, ann) =>
            shadow acc = { env: e }
            new-types = for fold(shadow acc from {env: acc.env, atoms: empty}, param from params):
              atom-env = make-atom-for(param, false, acc.env, bindings,
                C.type-bind(C.bo-local(l2), C.tb-type-var, _, none))
              { env: atom-env.env, atoms: link(atom-env.atom, acc.atoms) }
            end
            atom-env = make-atom-for(name, false, acc.env, bindings,
              C.type-bind(C.bo-local(l2), C.tb-type-let, _, none))
            new-bind = A.s-type-bind(l2, atom-env.atom, new-types.atoms.reverse(), ann.visit(self.{env: new-types.env}))
            { atom-env.env; link(new-bind, bs) }
          | s-newtype-bind(l2, name, tname) =>
            atom-env-t = make-atom-for(name, false, e, bindings,
              C.type-bind(C.bo-local(l2), C.tb-type-let, _, none))
            atom-env = make-atom-for(tname, false, atom-env-t.env, bindings,
              C.value-bind(C.bo-local(l2), C.vb-let, _, A.a-blank, none))
            new-bind = A.s-newtype-bind(l2, atom-env-t.atom, atom-env.atom)
            { atom-env.env; link(new-bind, bs) }
        end
      end
      visit-body = body.visit(self.{env: e})
      A.s-type-let-expr(l, bs.reverse(), visit-body, blocky)
    end,
    method s-let-expr(self, l, binds, body, blocky):
      {e; bs; atoms} = for fold(acc from { self.env; empty; empty}, b from binds):
        {e; bs; atoms} = acc
        cases(A.LetBind) b block:
          | s-let-bind(l2, bind, expr) =>
            visited-ann = bind.ann.visit(self.{env: e})
            atom-env = make-atom-for(bind.id, bind.shadows, e, bindings,
              C.value-bind(C.bo-local(l2), C.vb-let, _, visited-ann, none))
            visit-expr = expr.visit(self.{env: e})
            new-bind = A.s-let-bind(l2, A.s-bind(l2, bind.shadows, atom-env.atom, visited-ann), visit-expr)
            {
              atom-env.env;
              link(new-bind, bs);
              link(atom-env.atom, atoms)
               }
          | s-var-bind(l2, bind, expr) =>
            visited-ann = bind.ann.visit(self.{env: e})
            atom-env = make-atom-for(bind.id, bind.shadows, e, bindings,
              C.value-bind(C.bo-local(l2), C.vb-var, _, visited-ann, none))
            visit-expr = expr.visit(self.{env: e})
            new-bind = A.s-var-bind(l2, A.s-bind(l2, bind.shadows, atom-env.atom, visited-ann), visit-expr)
            {
              atom-env.env;
              link(new-bind, bs);
              link(atom-env.atom, atoms)
            }
        end
      end
      visit-binds = bs.reverse()
      visit-body = body.visit(self.{env: e})
      A.s-let-expr(l, visit-binds, visit-body, blocky)
    end,
    method s-letrec(self, l, binds, body, blocky):
      {new-binds; new-visitor} = resolve-letrec-binds(self, binds)
      visit-body = body.visit(new-visitor)
      A.s-letrec(l, new-binds, visit-body, blocky)
    end,
    method s-for(self, l, iter, binds, ann, body, blocky):
      {env; fbs} = for fold(acc from { self.env; empty }, fb from binds):
        {env; fbs} = acc
        cases(A.ForBind) fb block:
          | s-for-bind(l2, bind, val) => 
            atom-env = make-atom-for(bind.id, bind.shadows, env, bindings,
              C.value-bind(C.bo-local(l2), C.vb-let, _, bind.ann.visit(self), none))
            new-bind = A.s-bind(bind.l, bind.shadows, atom-env.atom, bind.ann.visit(self.{env: env}))
            visit-val = val.visit(self)
            new-fb = A.s-for-bind(l2, new-bind, visit-val)
            { atom-env.env; link(new-fb, fbs) }
        end
      end
      A.s-for(l, iter.visit(self), fbs.reverse(), ann.visit(self), body.visit(self.{env: env}), blocky)
    end,
    method s-cases-branch(self, l, pat-loc, name, args, body):
      {env; atoms} = for fold(acc from { self.env; empty }, a from args.map(_.bind)):
        {env; atoms} = acc
        atom-env = make-atom-for(a.id, a.shadows, env, bindings,
          C.value-bind(C.bo-local(a.l), C.vb-let, _, a.ann.visit(self), none))
        { atom-env.env; link(atom-env.atom, atoms) }
      end
      new-args = for map2(a from args, at from atoms.reverse()):
        cases(A.CasesBind) a:
          | s-cases-bind(l2, typ, binding) =>
            cases(A.Bind) binding:
              | s-bind(l3, shadows, id, ann) =>
                A.s-cases-bind(l2, typ, A.s-bind(l3, false, at, ann.visit(self.{env: env})))
            end
        end
      end
      new-body = body.visit(self.{env: env})
      A.s-cases-branch(l, pat-loc, name, new-args, new-body)
    end,
    # s-singleton-cases-branch introduces no new bindings
    method s-data-expr(self, l, name, namet, params, mixins, variants, shared-members, _check-loc, _check) block:
      {env; atoms} = for fold(acc from { self.env; empty }, param from params):
        {env; atoms} = acc
        atom-env = make-atom-for(param, false, env, bindings,
          C.type-bind(C.bo-local(l), C.tb-type-var, _, none))
        { atom-env.env; link(atom-env.atom, atoms) }
      end
      with-params = self.{env: env}
      result = A.s-data-expr(l, name, namet, atoms.reverse(),
        mixins.map(_.visit(with-params)), variants.map(_.visit(with-params)),
        shared-members.map(_.visit(with-params)), _check-loc, with-params.option(_check))
      datatypes.set-now(namet.key(), result)
      result
    end,
    method s-lam(self, l, name, params, args, ann, doc, body, _check-loc, _check, blocky) block:
     {ty-env; ty-atoms} = for fold(acc from {self.env; empty }, param from params):
        {env; atoms} = acc
        atom-env = make-atom-for(param, false, env, bindings,
          C.type-bind(C.bo-local(l), C.tb-type-var, _, none))
        { atom-env.env; link(atom-env.atom, atoms) }
      end
      with-params = self.{env: ty-env}
      {env; atoms} = for fold(acc from { with-params.env; empty }, a from args):
        {env; atoms} = acc
        atom-env = make-atom-for(a.id, a.shadows, env, bindings,
          C.value-bind(C.bo-local(a.l), C.vb-let, _, a.ann.visit(with-params), none))
        { atom-env.env; link(atom-env.atom, atoms) }
      end
      new-args = for map2(a from args, at from atoms.reverse()):
        cases(A.Bind) a:
          | s-bind(l2, shadows, id, ann2) => A.s-bind(l2, false, at, ann2.visit(with-params))
        end
      end
      with-params-and-args = with-params.{env: env}
      new-body = body.visit(with-params-and-args)
      saved-name-errors = name-errors
      new-check = with-params.option(_check) # Maybe should be self?  Are any type params visible here?
      # Restore the errors to what they were. (_check has already been desugared,
      # so the programmer will see those errors, not the ones from here.)
      name-errors := saved-name-errors
      A.s-lam(l, name, ty-atoms.reverse(), new-args, ann.visit(with-params), doc, new-body, _check-loc, new-check, blocky)
    end,
    method s-method(self, l, name, params, args, ann, doc, body, _check-loc, _check, blocky):
      {ty-env; ty-atoms} = for fold(acc from {self.env; empty }, param from params):
        {env; atoms} = acc
        atom-env = make-atom-for(param, false, env, bindings,
          C.type-bind(C.bo-local(param.l), C.tb-type-var, _, none))
        { atom-env.env; link(atom-env.atom, atoms) }
      end
      with-params = self.{env: ty-env}
      {env; atoms} = for fold(acc from { ty-env; empty }, a from args):
        {env; atoms} = acc
        atom-env = make-atom-for(a.id, a.shadows, env, bindings,
          C.value-bind(C.bo-local(a.l), C.vb-let, _, a.ann.visit(with-params), none))
        { atom-env.env; link(atom-env.atom, atoms) }
      end
      new-args = for map2(a from args, at from atoms.reverse()):
        cases(A.Bind) a:
          | s-bind(l2, shadows, id, ann2) => A.s-bind(l2, shadows, at, ann2.visit(with-params))
        end
      end
      new-body = body.visit(with-params.{env: env})
      new-check = with-params.option(_check)
      A.s-method(l, name, ty-atoms.reverse(), new-args, ann.visit(with-params), doc, new-body, _check-loc, new-check, blocky)
    end,
    method s-method-field(self, l, name, params, args, ann, doc, body, _check-loc, _check, blocky):
      {ty-env; ty-atoms} = for fold(acc from {self.env; empty }, param from params):
        {env; atoms} = acc
        atom-env = make-atom-for(param, false, env, bindings,
          C.type-bind(C.bo-local(l), C.tb-type-var, _, none))
        { atom-env.env; link(atom-env.atom, atoms) }
      end
      with-params = self.{env: ty-env}
      {env; atoms} = for fold(acc from { with-params.env; empty }, a from args):
        {env; atoms} = acc
        atom-env = make-atom-for(a.id, a.shadows, env, bindings,
          C.value-bind(C.bo-local(a.l), C.vb-let, _, a.ann.visit(with-params), none))
        { atom-env.env; link(atom-env.atom, atoms) }
      end
      new-args = for map2(a from args, at from atoms.reverse()):
        cases(A.Bind) a:
          | s-bind(l2, shadows, id, ann2) => A.s-bind(l2, shadows, at, ann2.visit(with-params))
        end
      end
      new-body = body.visit(with-params.{env: env})
      new-check = with-params.option(_check)
      A.s-method-field(l, name, ty-atoms.reverse(), new-args, ann.visit(with-params), doc, new-body, _check-loc, new-check, blocky)    end,
    method s-assign(self, l, id, expr):
      cases(A.Name) id:
        | s-name(l2, s) =>
          if self.env.has-key(s):
            bind = self.env.get-value(s)
            cases(Option) bind.to-value-bind() block:
              | none =>
                err = cases(NamespacedBinding) bind:
                  | nb-value(_) => raise("Should be impossible")
                  | nb-type(_) => C.type-id-used-as-value(l2, id)
                  | nb-module(_) => C.module-id-used-as-value(l2, id)
                end
                name-errors := link(err, name-errors)
                A.s-assign(l, bind.bind.atom, expr.visit(self))
              | some(vbind) =>
                A.s-assign(l, vbind.atom, expr.visit(self))
            end
            # This used to examine bind in more detail, and raise an error if it wasn't a var-bind
            # but that's better suited for a later pass
          else:
            A.s-assign(l, id, expr.visit(self)) # TODO: Should this be a s-global after all?
          end
        | s-underscore(_) =>
          A.s-assign(l, id, expr.visit(self))
        | else => raise("Wasn't expecting a non-s-name in resolve-names for assignment: " + torepr(id))
      end
    end,
    # NOTE(joe): Since there's no syntactic difference between _uses_ of letrec-,
    # let-, and var-bound names, this case disambiguates based on known binding
    # information
    method s-id(self, l, id):
      cases(A.Name) id:
        | s-name(l2, s) =>
          cases(Option) self.env.get(s) block:
            | none =>
              A.s-id(l2, names.s-global(s))
            | some(bind) =>
              cases (Option) bind.to-value-bind() block:
                | none =>
                  err = cases(NamespacedBinding) bind:
                    | nb-value(_) => raise("Should be impossible")
                    | nb-type(_) => C.type-id-used-as-value(l2, id)
                    | nb-module(_) => C.module-id-used-as-value(l2, id)
                  end
                  name-errors := link(err, name-errors)
                  A.s-id(l2, bind.bind.atom)
                | some(vb) =>
                  cases (C.ValueBinder) vb.binder:
                    | vb-let => A.s-id(l2, vb.atom)
                    | vb-letrec => A.s-id-letrec(l2, vb.atom, false)
                    | vb-var => A.s-id-var(l2, vb.atom)
                    | vb-module(_, _) => raise("s-id: should be impossible: " + torepr(bind))
                  end
              end
          end
        | s-atom(_, _) => A.s-id(l, id)
        | s-underscore(_) => A.s-id(l, id)
        | else => raise("Wasn't expecting a non-s-name in resolve-names id: " + torepr(id))
      end
    end,
    method s-id-letrec(self, l, id, _): A.s-id-letrec(l, handle-id(self.env, l, id), false) end,
    method s-id-var(self, l, id): A.s-id-var(l, handle-id(self.env, l, id)) end,
    method s-variant-member(self, l, typ, bind):
      new-bind = cases(A.Bind) bind:
        | s-bind(l2, shadows, name, ann) =>
          atom-env = make-atom-for(name, true, self.env, bindings,
            C.value-bind(C.bo-local(l2), C.vb-let, _, ann.visit(self), none))
          A.s-bind(l2, shadows, atom-env.atom, ann.visit(self))
      end
      A.s-variant-member(l, typ, new-bind)
    end,
    method s-bind(self, l, shadows, id, ann):
      cases(A.Name) id:
        | s-underscore(_) => A.s-bind(l, shadows, id, ann)
        | else => 
          raise("Should not reach non-underscore bindings in resolve-names: " + id.key() + " at " + torepr(l))
      end
    end,
    method s-dot(self, l, obj, field):
      cases(A.Expr) obj block:
        | s-dot(_, _, _) =>
          obj-visited = obj.visit(self)
          cases(A.Expr) obj-visited block:
            | s-dot(_, _, _) =>
              A.s-dot(l, obj-visited, field)
            | s-module-dot(l2, mod-ref, path) =>
              when path.length() <> 1:
                raise("Bug: resolve-scope path length != 1 (path was " + torepr(path) + ")")
              end
              # This is intended to flatten references like A.B.c into a single
              # module-dot expression of the form <uri-of-B>.c .
              # If something is unbound, then the s-module-dot will stop folding at that point
              cases(A.ModuleReference) mod-ref:
                | s-mref-by-name(n) => raise("Bug: ModuleReference was by-name")
                | s-mref-by-uri(name, uri) =>
                  dep = initial-env.uri-map.get(uri)
                  provs = dep.and-then(initial-env.mods.get).or-else(none)
                  cases(Option) provs:
                    | none => raise("Bug: reference to unknown module: " + uri)
                    | some(shadow p) =>
                      # Attempt to look up the module
                      cases(Option) p.modules.get(path.first):
                          # We don't worry about nonexistent fields here
                        | none => A.s-dot(l, obj-visited, field)
                        | some(mod-export) =>
                          new-provs = cases(C.ModuleExport) mod-export:
                            | m-resolved(res) => res
                            | m-uri(new-uri) =>
                              cases(Option) initial-env.uri-map.get(new-uri).and-then(initial-env.mods.get).or-else(none):
                                | none => raise("Bug: Reference to an unknown module " + mod-export)
                                | some(np) => np
                              end
                          end
                          A.s-module-dot(l, A.s-mref-by-uri(obj-visited, new-provs.from-uri), [list: field])
                      end
                  end
              end
          end
        | s-id(l2, name) =>
          # TODO: Well-formedness error on nonexistent module field
          maybe-module = cases(A.Name) name block:
            | s-name(l3, s) =>
              cases(Option) self.env.get(s) block:
                | none => none
                | some(bind) =>
                  bind.to-module-bind()
              end
            | else => none
          end
          cases(Option) maybe-module:
            | none => A.s-dot(l, obj.visit(self), field)
            | some(mb) =>
              mod-uri = cases(C.ModuleBinder) mb.binder:
                | mb-module(_, uri) => uri
                | mb-resolved(shadow p) => p.from-uri
              end
              A.s-module-dot(l, A.s-mref-by-uri(A.s-id(l2, mb.atom), mod-uri), [list: field])
          end
        | else => A.s-dot(l, obj.visit(self), field)
      end
    end,
    method a-blank(self): A.a-blank end,
    method a-any(self, l): A.a-any(l) end,
    method a-name(self, l, id): handle-ann(l, self.env, id) end,
    method a-arrow(self, l, args, ret, parens): A.a-arrow(l, args.map(_.visit(self)), ret.visit(self), parens) end,
    method a-arrow-argnames(self, l, args, ret, parens):
      A.a-arrow-argnames(l, args.map(_.visit(self)), ret.visit(self), parens)
    end,
    method a-method(self, l, args, ret): A.a-method(l, args.map(_.visit(self)), ret.visit(self)) end,
    method a-record(self, l, fields): A.a-record(l, fields.map(_.visit(self))) end,
    method a-app(self, l, ann, args): A.a-app(l, ann.visit(self), args.map(_.visit(self))) end,
    method a-pred(self, l, ann, exp): A.a-pred(l, ann.visit(self), exp.visit(self)) end,
    method a-dot(self, l, obj, field) block:
      cases(A.ModuleReference) obj:
        | s-mref-by-uri(_, _) => raise("s-mref-by-uri shouldn't exist before resolve-scope")
        | s-mref-by-name(name) =>
          cases(A.Name) name block:
            | s-name(nameloc, s) =>
              cases(Option) self.env.get(s) block:
                | none =>
                  name-errors := link(C.unbound-id(A.s-id(nameloc, name)), name-errors)
                  A.a-dot(l, obj, field)
                | some(bind) =>
                  cases(Option) bind.to-module-bind() block:
                    | none =>
                      err = cases(NamespacedBinding) bind:
                        | nb-module(_) => raise("Should be impossible")
                          # FIXME: Maybe a better error could go here:
                        | nb-value(_) => C.value-id-used-as-type(nameloc, bind.bind.atom)
                        | nb-type(_) => C.type-id-used-in-dot-lookup(nameloc, bind.bind.atom)
                      end
                      name-errors := link(err, name-errors)
                      A.a-dot(l, A.s-mref-by-name(bind.bind.atom), field)
                    | some(mb) =>
                      A.a-dot(l, A.s-mref-by-uri(A.s-id(l, mb.atom), mb.binder.get-uri()), field)
                  end
              end
            | else =>
              name-errors := link(C.underscore-as-ann(name.l), name-errors)
              A.a-dot(l, obj.visit(self), field)
          end
      end
    end,
    method a-field(self, l, name, ann): A.a-field(l, name, ann.visit(self)) end,
    method s-table-select(self, l, columns, table):
      A.s-table-select(l, columns.map(_.visit(self)), table.visit(self))
    end,
    method s-table-extend(self, l, column-binds, extensions):
      column-binds-and-env = handle-column-binds(column-binds, self)
      A.s-table-extend(l, column-binds-and-env.column-binds,
        extensions.map(_.visit(self.{env: column-binds-and-env.env})))
    end,
    method s-table-update(self, l, column-binds, updates):
      column-binds-and-env = handle-column-binds(column-binds, self)
      A.s-table-update(l, column-binds-and-env.column-binds,
        updates.map(_.visit(self.{env: column-binds-and-env.env})))
    end,
    method s-table-filter(self, l, column-binds, pred):
      column-binds-and-env = handle-column-binds(column-binds, self)
      A.s-table-filter(l, column-binds-and-env.column-binds,
        pred.visit(self.{env: column-binds-and-env.env}))
    end,
    method s-table-order(self, l, table, ordering):
      A.s-table-order(l, table.visit(self), ordering)
    end
  }
  visited = p.visit(names-visitor)
  split-binds = split-bindings-now(bindings)
  # mod-binds = SD.make-mutable-string-dict()
  # val-binds = SD.make-mutable-string-dict()
  # for each(val from bindings.keys-list-now()):
  #   v-binding = bindings.get-value-now(val)
  #   cases(C.ValueBinder) v-binding.binder block:
  #     | vb-module(dep, uri) =>
  #       # print("\nModule binding: " + to-repr({val; v-binding}) + "...")
  #       cases (Option) initial-env.mods.get(dep.key()):
  #         | none => raise("Bug: Reference to an unknown module: " + to-repr(val) + " " + to-repr(v-binding))
  #         | some(provides) =>
  #           mod-binds.set-now(val, provides)
  #       end
  #     | else =>
  #       # print("Value binding: " + to-repr({val; v-binding}) + "\n")
  #       val-binds.set-now(val, v-binding)
  #   end
  # end
  C.resolved-names(visited, name-errors, split-binds.values, split-binds.types, datatypes, split-binds.modules)
end

fun check-unbound-ids-bad-assignments(ast :: A.Program, resolved :: C.NameResolution, initial-env :: C.CompileEnvironment) block:
  var shadow errors = [list: ] # THE MUTABLE LIST OF ERRORS
  bindings = resolved.bindings
  type-bindings = resolved.type-bindings
  modules = resolved.modules
  fun add-error(err): errors := err ^ link(_, errors) end
  fun handle-id(id, loc):
    if A.is-s-underscore(id) block:
      add-error(C.underscore-as-expr(loc))
      false
    else if A.is-s-global(id) and initial-env.globals.values.has-key(id.toname()):
      false
    else if bindings.has-key-now(id.key()):
      false
    else:
      # print-error("Cannot find binding for " + id.key() + " at " + loc.format(true) + " in \n")
      # print-error("Bindings: " + torepr(bindings.keys-list-now()) + "\n")
      # print-error("Global values: " + torepr(initial-env.globals.values.keys-list()) + "\n")
      true
    end
  end

  fun handle-module-id(id, loc, check-underscore) block:
    if A.is-s-underscore(id) block:
      when check-underscore:
        add-error(C.underscore-as-expr(loc))
      end
      false
    else if A.is-s-global(id) and initial-env.globals.modules.has-key(id.toname()):
      false
    else if modules.has-key-now(id.key()):
      false
    else:
      true
    end
  end

  fun handle-value-or-module-id(id, loc):
    if not(handle-id(id, loc)):
      false
    else:
      handle-module-id(id, loc, false)
    end
  end

  fun handle-type-id(id, loc):
    if A.is-s-underscore(id) block:
      add-error(C.underscore-as-ann(id.l))
      false
    else if A.is-s-type-global(id) and initial-env.globals.types.has-key(id.toname()):
      false
    else if type-bindings.has-key-now(id.key()):
      false
    else:
      true
    end
  end

  fun handle-module-type-id(id, loc, check-underscore):
    if A.is-s-underscore(id) block:
      when check-underscore:
        add-error(C.underscore-as-ann(id.l))
      end
      false
    else if A.is-s-global(id) and initial-env.globals.modules.has-key(id.toname()):
      false
    else if modules.has-key-now(id.key()):
      false
    else:
      true
    end
  end

  fun handle-type-or-module-id(id, loc):
    if not(handle-type-id(id, loc)):
      false
    else:
      handle-module-type-id(id, loc, false)
    end
  end
    
  ast.visit(A.default-iter-visitor.{
      method s-id(self, loc, id) block:
        when handle-id(id, loc):
          add-error(C.unbound-id(A.s-id(loc, id)))
        end
        true
      end,
      method s-module-dot(self, l, base, path) block:
        fun value-to-module(vb :: C.ValueBinder) -> Option<C.Provides>:
          cases(C.ValueBinder) vb:
            | vb-module(dep, _) =>
              mod-info = initial-env.mods.get(dep.key())
              cases(Option) mod-info:
                | none =>
                  raise("Bug: Reference to an unknown module. " + to-repr(base) + " " + to-repr(vb.binder) )
                | some(info) => some(info)
              end
            | else => none
          end
        end
        fun module-to-provides(mb :: C.ModuleBinder) -> C.Provides:
          cases(C.ModuleBinder) mb:
            | mb-module(dep, _) =>
              mod-info = initial-env.mods.get(dep.key())
              cases(Option) mod-info:
                | none =>
                  raise("Bug: Reference to an unknown module. " + to-repr(base) + " " + to-repr(mb.binder) )
                | some(info) => some(info)
              end
            | mb-resolved(provides) => provides
          end
        end
        fun traverse-path(cur-mod, path-elt) -> Option<E.Either<C.ValueExport, C.Provides>> block:
          #print("Traversing uri " + torepr(cur-mod.from-uri) + " to find binding for " + torepr(path-elt) + "\n")
          cases(Option) cur-mod.values.get(path-elt) block:
            | some(val) =>
              #print("found value binding: " + torepr(val) + "\n")
              # Maybe we should print a warning here, since
              # I don't think this should really ever resolve
              # to a module...
              some(E.left(val))
            | none =>
              cases(Option) cur-mod.modules.get(path-elt):
                | none => none
                | some(mod-export) =>
                  cases(C.ModuleExport) mod-export:
                    | m-resolved(res) => some(E.right(res))
                    | m-uri(uri) =>
                      # FIXME: mods contains by Dependency key, not URI 
                      cases(Option) initial-env.mods.get(uri):
                        | none =>
                          raise("Bug: Reference to an unknown module. " + to-repr(path-elt) + " " + to-repr(mod-export) )
                        | some(prov) =>
                          some(E.right(prov))
                      end
                  end
              end
          end
        end
        
        base-mod = cases(A.ModuleReference) base block:
          | s-mref-by-name(n) =>
            when handle-value-or-module-id(n, l):
              add-error(C.unbound-id(A.s-id(l, n)))
            end
            cases(Option) modules.get-now(n.key()):
              | none =>
                none
              | some(mb) =>
                cases(C.ModuleBinder) mb.binder:
                  | mb-module(dep, _) => initial-env.mods.get(dep.key())
                  | mb-resolved(res) => some(res)
                end
            end
          | s-mref-by-uri(n, uri) =>
            cases(A.Expr) n:
              | s-id(_, name) =>
                when handle-value-or-module-id(name, l):
                  add-error(C.unbound-id(A.s-id(l, name)))
                end
              | else => nothing # s-dots are statically known to be fine
            end
            initial-env.uri-map.get(uri).and-then(initial-env.mods.get).or-else(none)
        end
        cases(Option) base-mod block:
          | none =>
            raise("Unbound module: " + base.tosource().pretty(80))
          | some(info) =>
            #print("Looking up path: " + torepr(path) + " in module: " + torepr(info) + "\n")
            cases(Option) info.values.get(path.first) block:
              | some(_) => true
              | none =>
                add-error(C.wf-err("Name " + path.first + " not found in module bound to " + base.tosource().pretty(80).join-str("\n"), l))
                true
            end
        end
        #cases(Option) modules.get-now(base.name.key()):
        #  | none => true
        #  | some(mb) =>
        #    cases(C.ModuleBinder) mb.binder block:
        #      | mb-module(dep, _) =>
        #        mod-info = initial-env.mods.get(dep.key())
        #        cases(Option) mod-info block:
        #          | none =>
        #            raise("Bug: Reference to an unknown module. " + to-repr(base) + " " + to-repr(mb.binder) )
        #          | some(info) =>
        #            for L.fold-while(acc from {info; [list: torepr(base)]}, path-elt from path):
        #              {cur-info; path-so-far} = acc
        #              cases(Option) traverse-path(cur-info, path-elt):
        #                | none =>
        #                  E.right(add-error(C.wf-err("Name " + path-elt + " not found in module bound to " + to-repr(path-so-far.reverse().join-str(".")), l)))
        #                | some(v) =>
        #                  cases(E.Either) v:
        #                    | left(val) =>
        #                      # Found a value. Finish.
        #                      # Note: Ideally, we would want to
        #                      # use some type information to check
        #                      # that the remaining path is indeed
        #                      # a sequence of fields on whatever value
        #                      # we've found at this point. However,
        #                      # this information is not available at
        #                      # this point.
        #                      E.right(nothing)
        #                    | right(nested-mod) =>
        #                      E.left({nested-mod; link(path-elt, path-so-far)})
        #                  end
        #              end
        #            end
        #        end
        #    end
        #end
      end,
      method s-id-var(self, loc, id) block:
        when handle-id(id, loc):
          add-error(C.unbound-id(A.s-id-var(loc, id)))
        end
        true
      end,
      method s-id-letrec(self, loc, id, safe) block:
        when handle-id(id, loc):
          add-error(C.unbound-id(A.s-id-letrec(loc, id, safe)))
        end
        true
      end,
      method s-assign(self, loc, id, value) block:
        id-k = id.key()
        if bindings.has-key-now(id-k):
          binding = bindings.get-value-now(id-k)
          when not(C.is-vb-var(binding.binder)) block:
            #print("The resolution was: " + torepr(resolved))
            #print("\n\nThe environment was: " + torepr(initial-env))
            var-loc = get-origin-loc(binding.origin)
            add-error(C.bad-assignment(A.s-assign(loc, id, value), var-loc))
          end
        else:
          add-error(C.unbound-var(id.toname(), loc))
        end
        value.visit(self)
      end,
      method a-name(self, loc, id) block:
        when handle-type-id(id, loc):
          add-error(C.unbound-type-id(A.a-name(loc, id)))
        end
        true
      end,
      method a-dot(self, l, name, field) block:
        fun module-should-exist(m, mb) block:
          cases(Option) m:
            | some(_) => nothing
            | none => raise("Bug: Reference to an unknown module: " + torepr(name) + " " + mb)
          end
          m
        end
        base-mod = cases(A.ModuleReference) name block:
          | s-mref-by-name(n) =>
            when handle-type-or-module-id(n, l):
              add-error(C.unbound-type-id(A.a-name(l, n)))
            end
            cases(Option) modules.get-now(n.key()):
              | none => none
              | some(mb) =>
                cases(C.ModuleBinder) mb.binder:
                  | mb-module(dep) => module-should-exist(initial-env.mods.get(dep.key()), torepr(mb.binder))
                  | mb-resolved(res) => some(res)
                end
            end
          | s-mref-by-uri(n, uri) =>
            cases(A.Expr) n:
              | s-id(shadow l, _name) =>
                when handle-type-or-module-id(_name, l):
                  add-error(C.unbound-type-id(A.a-name(l, _name)))
                end
              | else => nothing # statically known to be bound
            end
            mod = initial-env.uri-map.get(uri).and-then(initial-env.mods.get).or-else(none)
            module-should-exist(mod, uri)
        end
        cases(Option) base-mod:
          | none => nothing
          | some(info) =>
            cases(Option) info.aliases.get(field) block:
              | none => add-error(C.wf-err("Type named " + field + " not found in module bound to " + to-repr(name.name), l))
              | some(_) => nothing
            end
        end
        true
      end
    })
  errors
where:
  p = PP.surface-parse(_, "test")
  px = p("x")
  resolved = C.resolved-names(px, empty, [SD.mutable-string-dict:], [SD.mutable-string-dict:], [SD.mutable-string-dict:], [SD.mutable-string-dict:])
  unbound1 = check-unbound-ids-bad-assignments(px, resolved, C.standard-builtins)
  unbound1.length() is 1
end
