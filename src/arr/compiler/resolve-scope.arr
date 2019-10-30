#lang pyret

provide *
provide-types *
import ast as A
import srcloc as S
import parse-pyret as PP
import string-dict as SD
import lists as L
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
  fun params-match(funargs :: List<A.Name>, cargs :: List<A.Name>):
    if is-empty(funargs) and is-empty(cargs): true
    else if is-empty(funargs) or is-empty(cargs): false
    else:
      (funargs.first.toname() == cargs.first.toname()) and params-match(funargs.rest, cargs.rest)
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

  fun is-blank-contract(a):
    if A.is-a-blank(a): true
    else if A.is-a-tuple(a):
      a.fields.all({(elt): is-blank-contract(elt)})
    else: false
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
                    if not(args.all({(a): is-blank-contract(a.ann)}) and A.is-a-blank(ret)) block:
                      errors := link(C.contract-redefined(c.l, id-name, l-fun), errors)
                      link(fun-to-lam(bind), acc)
                    else if A.is-a-arrow(c.ann):
                      ok-params =
                        if is-link(params) and not(params-match(c.params, params)) block:
                          errors := link(C.contract-inconsistent-params(c.l, id-name, l-fun), errors)
                          false
                        else: true
                        end
                      ok-args =
                        if c.ann.args.length() <> args.length() block:
                          errors := link(C.contract-inconsistent-names(c.l, id-name, l-fun), errors)
                          false
                        else: true
                        end
                      if ok-params and ok-args:
                        new-args = for map2(a from args, shadow ann from c.ann.args):
                          A.s-bind(a.l, a.shadows, a.id, ann)
                        end
                        link(
                          rebuild-bind(bind,
                            bind.b,
                            A.s-lam(l, name, c.params, new-args, c.ann.ret, doc, body, _check-loc, _check, blocky)),
                          acc)
                      else:
                        link(fun-to-lam(bind), acc)
                      end
                    else if A.is-a-arrow-argnames(c.ann):
                      ok-params =
                        if is-link(params) and not(params-match(c.params, params)) block:
                          errors := link(C.contract-inconsistent-params(c.l, id-name, l-fun), errors)
                          false
                        else: true
                        end
                      ok-args =
                        if not(names-match(args, c.ann.args)) block:
                          errors := link(C.contract-inconsistent-names(c.l, id-name, l), errors)
                          false
                        else: true
                        end
                      if ok-params and ok-args:
                        new-args = for map2(a from args, shadow ann from c.ann.args):
                          A.s-bind(a.l, a.shadows, a.id, ann.ann)
                        end
                        link(
                          rebuild-bind(bind,
                            bind.b,
                            A.s-lam(l, name, c.params, new-args, c.ann.ret, doc, body, _check-loc, _check, blocky)),
                          acc)
                      else:
                        link(fun-to-lam(bind), acc)
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
        | s-contract(l, name, params, ann) =>
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
    | s-program(l, _provide-raw, provide-types-raw, provides, imports-raw, body) =>
      str = A.s-str(l, _)

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

      recombined = A.s-block(with-provides.l, #| remaining + |# with-provides.stmts)
      visited = recombined.visit(desugar-scope-visitor)
      C.resolved-scope(A.s-program(l, _provide-raw, provide-types-raw, provides, imports-raw, visited), errors)
  end

end


fun get-origin-loc(o):
  cases(C.BindOrigin) o:
    | bind-origin(_, definition-bind-site, _, _, _) => definition-bind-site
  end
end

fun get-local-loc(o):
  cases(C.BindOrigin) o:
    | bind-origin(local-bind-site, _, _, _, _) => local-bind-site
  end
end


fun uri-from(start :: String, path :: List<A.Name>, compile-env):
  cases(List) path:
    | empty => some(start)
    | link(f, r) =>
      mod-info = compile-env.provides-by-uri-value(start)
      cases(Option) mod-info.modules.get(f.toname()):
        | none => raise("Cannot find a a provided module named  " + to-repr(f) + " on module " + start)
        | some(uri) => uri-from(uri, r, compile-env)
      end
  end
end

fun maybe-uri-for-path(full-path :: List<A.Name>, compile-env, mod-env):
  cases(List) full-path:
    | empty => none
    | link(f, r) =>
      cases(Option) mod-env.get(f.toname()):
        | none => raise("Cannot find a binding for module named " + to-repr(f))
        | some(mod-bind) => uri-from(mod-bind.uri, r, compile-env)
      end
  end
end

fun path-uri(pre-path, path, compile-env, mod-env):
  maybe-uri-for-path(pre-path + path.take(path.length() - 1), compile-env, mod-env)
end

fun resolve-names(p :: A.Program, initial-env :: C.CompileEnvironment):
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

  # Maps from keys to ModuleBinds
  module-bindings = SD.make-mutable-string-dict()

  # Maps from keys to ValueBinds
  bindings = SD.make-mutable-string-dict()

  # Maps from keys to TypeBinds
  type-bindings = SD.make-mutable-string-dict()

  # Maps from keys to data expressions
  datatypes = SD.make-mutable-string-dict()

  fun make-anon-import-for(l, s, env, shadow bindings, b) block:
    atom = names.make-atom(s)
    bindings.set-now(atom.key(), b(atom))
    { atom: atom, env: env }
  end
  fun make-atom-for(name, is-shadowing, env, shadow bindings, make-binding):
    cases(A.Name) name block:
      | s-name(l, s) =>
        when env.has-key(s) and not(is-shadowing):
          old-loc = get-origin-loc(env.get-value(s).origin)
          local-loc = get-local-loc(env.get-value(s).origin)
          import-loc-opt =
            if (local-loc == p.l) or (local-loc == A.dummy-loc) block:
              none
            else:
              some(local-loc)
            end
          #|
          spy "import-loc-opt":
            name,
            use-loc: l,
            orig-loc: old-loc,
            local-loc
          end
          |#
          name-errors := link(C.shadow-id(s, l, old-loc, import-loc-opt), name-errors)
        end
        # when env.has-key(s):
        #   spy "make-atom-for":
        #     s,
        #     use-loc: l,
        #     is-shadowing,
        #     origin: env.get-value(s)
        #   end
        # end
        atom = names.make-atom(s)
        binding = make-binding(atom)
        bindings.set-now(atom.key(), binding)
        { atom: atom, env: env.set(s, binding) }
      | s-underscore(l) =>
        atom = names.make-atom("$underscore")
        bindings.set-now(atom.key(), make-binding(atom))
        { atom: atom, env: env }
      # NOTE(joe): an s-atom is pre-resolved to all its uses, so no need to add
      # it or do any more work.
      | s-atom(_, _) =>
        binding = make-binding(name)
        bindings.set-now(name.key(), binding)
        { atom: name, env: env }
      | else => raise("Unexpected atom type: " + torepr(name))
    end
  end

  fun make-import-atom-for(name :: A.Name, from-uri, env, shadow bindings, make-binding):
    if A.is-s-name(name):
      cases(Option) env.get(name.toname()) block:
        | none =>
          make-atom-for(name, false, env, bindings, make-binding)
        | some(b) =>
          # If they are from the same URI, can import the same name multiple
          # times. If not, then they count as shadowing one another (e.g. two
          # values named list coming from two different libs)
          shadowing = b.origin.uri-of-definition == from-uri
          make-atom-for(name, shadowing, env, bindings, make-binding)
      end
    else:
      make-atom-for(name, false, env, bindings, make-binding)
    end
  end

  fun scope-env-from-env(initial :: C.CompileEnvironment) block:
    acc = SD.make-mutable-string-dict()
    for SD.each-key(name from initial.globals.values) block:
      # TODO: Below we should use the origin-name from globals to find the right original value
      origin = initial.globals.values.get-value(name)
      uri-of-definition = origin.uri-of-definition
      val-info = initial.value-by-origin(origin)
      cases(Option) val-info block:
        | none => raise("The value is a global that doesn't exist in any module: " + name)
        | some(shadow val-info) =>
          cases(C.ValueExport) val-info block:
            | v-var(_, t) =>
              b = C.value-bind(C.bo-global(some(origin), uri-of-definition, origin.original-name), C.vb-var, names.s-global(name), A.a-blank)
              bindings.set-now(names.s-global(name).key(), b)
              acc.set-now(name, b)
            | else =>
              # TODO(joe): Good place to add _location_ to valueexport to report errs better
              b = C.value-bind(C.bo-global(some(origin), uri-of-definition, origin.original-name), C.vb-let, names.s-global(name), A.a-blank)
              bindings.set-now(names.s-global(name).key(), b)
              acc.set-now(name, b)
          end
      end
    end
    acc.freeze()
  end

  fun type-env-from-env(initial :: C.CompileEnvironment) block:
    acc = SD.make-mutable-string-dict()
    for SD.each-key(name from initial.globals.types) block:
      origin = initial.globals.types.get-value(name)
      type-info = initial.type-by-origin-value(origin)
      b = C.type-bind(C.bo-global(some(origin), origin.uri-of-definition, origin.original-name), C.tb-type-let, names.s-type-global(name), none)
      type-bindings.set-now(names.s-type-global(name).key(), b)
      acc.set-now(name, b)
    end
    acc.freeze()
  end

  fun module-env-from-env(initial :: C.CompileEnvironment) block:
    acc = SD.make-mutable-string-dict()
    for SD.each-key(name from initial.globals.modules) block:
      origin = initial.globals.modules.get-value(name)
      mod-info = initial.provides-by-origin-value(origin)
      when not(mod-info.modules.has-key(name)):
        spy: mod-info end
      end
      b = C.module-bind(C.bo-global(some(origin), origin.uri-of-definition, origin.original-name), names.s-module-global(name), mod-info.modules.get-value(name))
      module-bindings.set-now(names.s-module-global(name).key(), b)
      acc.set-now(name, b)
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
        C.value-bind(C.bo-local(b.l, b.b.id), C.vb-letrec, _, b.b.ann.visit(visitor)))
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
          env.get-value(s).atom
        else:
          names.s-global(s)
        end
      | s-atom(_, _) => id
      | s-underscore(_) => id
      | else => raise("Wasn't expecting a non-s-name in resolve-names id: " + torepr(id))
    end
  end
  fun handle-ann(l, type-env, id):
    cases(A.Name) id:
      | s-name(_, s) =>
        if type-env.has-key(s):
          vb = type-env.get-value(s)
          name = vb.atom
          cases(C.TypeBinder) vb.binder block:
            | tb-type-let => A.a-name(l, name)
            | tb-type-var => A.a-type-var(l, name)
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
          C.value-bind(C.bo-local(cb.l, cb.id), C.vb-let, _, cb.ann.visit(visitor)))
        new-cb = A.s-bind(cb.l, cb.shadows, atom-env.atom, cb.ann.visit(visitor.{env: acc.env}))
        { env: atom-env.env, cbs: link(new-cb, acc.cbs) }
      end
    { column-binds: A.s-column-binds(column-binds.l, env-and-binds.cbs, column-binds.table.visit(visitor)),
               env: env-and-binds.env }
  end

  var include-counter = 0
  fun include-name() block:
    include-counter := include-counter + 1
    "$included-" + to-string(include-counter)
  end

  fun add-value-name(l, imp-loc, env, vname, as-name, mod-info):
    maybe-value-export = mod-info.values.get(vname.toname())
    cases(Option) maybe-value-export block:
      | none =>
        name-errors := link(C.name-not-provided(l, imp-loc, vname, "value"), name-errors)
        env
      | some(value-export) =>
        vbinder = cases(C.ValueExport) value-export block:
          | v-var(_, t) => C.vb-var
          | else => C.vb-let
        end
        atom-env = make-import-atom-for(as-name, value-export.origin.uri-of-definition, env, bindings,
          C.value-bind(C.bo-module(as-name.l, value-export.origin.definition-bind-site, value-export.origin.uri-of-definition, value-export.origin.original-name), vbinder, _, A.a-any(vname.l)))
        atom-env.env
    end
  end

  fun add-type-name(l, imp-loc, type-env, tname, as-name, mod-info):
    maybe-type-export = mod-info.aliases.get(tname.toname())
    cases(Option) maybe-type-export block:
      | none =>
        name-errors := link(C.name-not-provided(l, imp-loc, tname, "type"), name-errors)
        type-env
      | some(t) =>
        { orig-name; uri-of-typ; loc-of-typ } = cases(T.Type) t:
          | t-name(module-name, id, loc, _) =>
            cases(T.NameOrigin) module-name:
              | local => { id; mod-info.from-uri; loc }
              | module-uri(shadow uri) => { id; uri; loc }
              | dependency(d) => { id; mod-info.from-uri; loc }
            end
          | else => { tname; mod-info.from-uri; t.l }
        end
        atom-env = make-import-atom-for(as-name, uri-of-typ, type-env, type-bindings,
          C.type-bind(C.bo-module(as-name.l, loc-of-typ, uri-of-typ, orig-name), C.tb-type-let, _, none))
        atom-env.env
    end
  end

  fun add-module-name(l, imp-loc, module-env, mname, as-name, mod-info):
    maybe-module-export = mod-info.modules.get(mname.toname())
    cases(Option) maybe-module-export block:
      | none =>
        name-errors := link(C.name-not-provided(l, imp-loc, mname, "module"), name-errors)
        module-env
      | some(uri) =>
        atom-env = make-import-atom-for(as-name, mod-info.from-uri, module-env, module-bindings,
          C.module-bind(C.bo-module(as-name.l, S.builtin(uri), mod-info.from-uri, mname), _, uri))
        atom-env.env
    end
  end

  fun star-names(shadow names, hidings):
    for filter(n from names):
      not(lists.member(hidings.map(_.toname()), n))
    end
  end

  fun add-spec(imp-loc, {imp-e; imp-te; imp-me; imp-imps} as acc, mod-info, spec):
    fun add-name-spec(name-spec, dict, which-env, adder):
      cases(A.NameSpec) name-spec block:
        | s-star(l, hidings) =>          
          imported-names = star-names(dict.keys-list(), hidings)
          for fold(shadow which-env from which-env, n from imported-names):
            adder(l, imp-loc, which-env, A.s-name(l, n), A.s-name(l, n), mod-info)
          end
        | s-module-ref(l, path, as-name) =>
          maybe-uri = uri-from(mod-info.from-uri, path.take(path.length() - 1), initial-env)
          shadow mod-info = cases(Option) maybe-uri:
            | none => raise("Could not find module " + to-repr(path))
            | some(p-uri) => initial-env.provides-by-uri-value(p-uri)
          end
          shadow as-name = cases(Option) as-name:
            | none => path.last()
            | some(n) => n
          end
          adder(l, imp-loc, which-env, path.last(), as-name, mod-info)
      end
    end

    fun add-data-spec(envs, name-spec):
      cases(A.NameSpec) name-spec block:
        | s-star(l, _) =>
          datatype-names = mod-info.data-definitions.keys-list()
          for fold(shadow envs from envs, dname from datatype-names):
            add-data-spec(envs, A.s-module-ref(l, [list: A.s-name(l, dname)], none))
          end
        | s-module-ref(l, path, as-name) =>
          maybe-uri = uri-from(mod-info.from-uri, path.take(path.length() - 1), initial-env)
          shadow mod-info = cases(Option) maybe-uri:
            | none => raise("Could not find module " + to-repr(path))
            | some(p-uri) => initial-env.provides-by-uri-value(p-uri)
          end
          # NOTE(joe): the module-ref can't possibly have an as-name for data
          # based on how the grammar is defined; we simply re-use s-module-ref
          # rather than introduce a new variant.
          shadow as-name = cases(Option) as-name:
            | none => path.last()
            | some(n) => n
          end
          dname = path.last().toname()
          maybe-dt-export = initial-env.resolve-datatype-by-uri(mod-info.from-uri, dname)
          cases(Option) maybe-dt-export:
            | none => raise("Cannot find datatype name " + dname + " in " + mod-info.from-uri)
            | some(typ) =>
              {imp-e-dts; imp-te-dts} = envs
              shadow imp-e-dts = for fold(shadow imp-e-dts from imp-e-dts, v from typ.variants):
                constructor-ref = A.s-module-ref(l, path.take(path.length() - 1) + [list: A.s-name(l, v.name)], none)
                checker-ref = A.s-module-ref(l, path.take(path.length() - 1) + [list: A.s-name(l, A.make-checker-name(v.name))], none)
                env1 = add-name-spec(constructor-ref, mod-info.values, imp-e-dts, add-value-name)
                add-name-spec(checker-ref, mod-info.values, env1, add-value-name)
              end
              typ-alias-ref = A.s-module-ref(l, path.take(path.length() - 1) + [list: A.s-name(l, dname)], none)
              shadow imp-te-dts = add-name-spec(typ-alias-ref, mod-info.aliases, imp-te-dts, add-type-name)
              { imp-e-dts; imp-te-dts}
          end
      end
    end

    cases(A.IncludeSpec) spec:
      | s-include-name(l, name-spec) =>
        new-env = add-name-spec(name-spec, mod-info.values, imp-e, add-value-name)
        { new-env; imp-te; imp-me; imp-imps }
      | s-include-type(l, name-spec) =>
        new-type-env = add-name-spec(name-spec, mod-info.aliases, imp-te, add-type-name)
        { imp-e; new-type-env; imp-me; imp-imps }
      | s-include-module(l, name-spec) =>
        new-module-env = add-name-spec(name-spec, mod-info.modules, imp-me, add-module-name)
        { imp-e; imp-te; new-module-env; imp-imps }
      | s-include-data(l, name-spec, hidings) =>
        { imp-e-dts; imp-te-dts } = add-data-spec({imp-e; imp-te}, name-spec)
        { imp-e-dts; imp-te-dts; imp-me; imp-imps }
    end
  end

  fun add-import({imp-e; imp-te; imp-me; imp-imps} as acc, imp):
    cases(A.Import) imp block:
      | s-import(l, file, local-name) =>
        info-key = U.import-to-dep(file).key()
        mod-uri = initial-env.uri-by-dep-key(info-key)
        atom-env-m =
          if A.is-s-underscore(local-name):
            make-anon-import-for(local-name.l, "$underscore_import", imp-me, module-bindings,
              C.module-bind(C.bo-local(l, local-name), _, mod-uri))
          else:
            make-atom-for(local-name, false, imp-me, module-bindings,
              C.module-bind(C.bo-local(l, local-name), _, mod-uri))
          end
        new-header = A.s-import(l, file, atom-env-m.atom)
        { imp-e; imp-te; atom-env-m.env; link(new-header, imp-imps) }
      | s-import-fields(l, fields, file) =>
        synth-include-name = names.make-atom(include-name())
        updated = add-import(acc, A.s-import(l, file, synth-include-name))
        add-import(updated, A.s-include-from(l, [list: synth-include-name],
          fields.map(lam(f):
            A.s-include-name(l, A.s-module-ref(l, [list: f], none))
          end)))
      | s-include(l, file) =>
        synth-include-name = names.make-atom(include-name())
        updated = add-import(acc, A.s-import(l, file, synth-include-name))
        add-import(updated, A.s-include-from(l, [list: synth-include-name],
          [list:
            A.s-include-name(l, A.s-star(l, empty)),
            A.s-include-type(l, A.s-star(l, empty)),
            A.s-include-module(l, A.s-star(l, empty)),
            A.s-include-data(l, A.s-star(l, empty), [list:])
          ]))
      | s-include-from(l, name, specs) =>
        # NOTE(joe): This few lines is a funky little pattern. It may be worth
        # extracting for generic use for values & types as well. The reason
        # it's necessary is that it's useful to use atoms to avoid putting
        # "real" names into the namespace. If this is a more general thing we
        # do across different pre-resolve-scope desugarings, then this pattern
        # becomes handy.
        module-info = if A.is-s-atom(name.first):
          module-bindings.get-now(name.first.key())
        else:
          imp-me.get(name.first.toname())
        end
        { first-mod-uri; atom } = cases(Option) module-info:
          | none => raise("Could not find import: " + name.first.toname())
          | some(mod-bind) => { mod-bind.uri; mod-bind.atom }
        end
        dotted-uri = cases(Option) uri-from(first-mod-uri, name.drop(1), initial-env):
          | none => raise("Could not find module " + to-repr(name))
          | some(uri) => uri
        end
        mod-info = initial-env.provides-by-uri-value(dotted-uri)
        {specs-e; specs-te; specs-me; _ } = for fold(shadow acc from acc, s from specs):
          add-spec(l, acc, mod-info, s)
        end
        {specs-e; specs-te; specs-me; link(A.s-include-from(l, [list: atom], specs), imp-imps)}
    end
  end

  fun resolve-import-names(self, imports):
    for fold(acc from { self.env; self.type-env; self.module-env; empty }, i from imports):
      add-import(acc, i)
    end
  end

  var final-visitor = nothing


  names-visitor = A.default-map-visitor.{
    env: scope-env-from-env(initial-env),
    type-env: type-env-from-env(initial-env),
    module-env: module-env-from-env(initial-env),
    method s-module(self, l, answer, _, _, _, checks) block:


#|
      ```
      include from T: * end <-- defined here?
      include from T: a, b, c end <-- defined here?
      ```
 |#

      non-globals =
        for filter(k from self.env.keys-list()):
          vb = self.env.get-value(k)
          vb.origin.new-definition
        end
      defined-vals = for map(key from non-globals): 
        vb = self.env.get-value(key)
        atom = vb.atom
        cases(C.ValueBinder) vb.binder block:
          | vb-let => A.s-defined-value(key, A.s-id(l, atom))
          | vb-letrec => A.s-defined-value(key, A.s-id-letrec(l, atom, true))
          | vb-var => A.s-defined-var(key, atom)
        end
      end

      non-global-types =
        for filter(k from self.type-env.keys-list()):
          tb = self.type-env.get-value(k)
          tb.origin.new-definition
        end
      defined-types = for map(key from non-global-types):
        atom = self.type-env.get-value(key).atom
        A.s-defined-type(key, A.a-name(l, atom))
      end

      non-global-modules = 
        for filter(k from self.module-env.keys-list()):
          mb = self.module-env.get-value(k)
          mb.origin.new-definition
        end
      defined-modules = for map(key from non-global-modules):
        bind = self.module-env.get-value(key)
        A.s-defined-module(key, bind.atom, bind.uri)
      end

      final-visitor := self
      A.s-module(l, answer.visit(self), defined-modules, defined-vals, defined-types, checks.visit(self))
    end,
    method s-program(self, l, _provide, _provide-types, provides, imports, body) block:
      {imp-e; imp-te; imp-me; imp-imps} = resolve-import-names(self, imports)

      visit-body = body.visit(self.{env: imp-e, type-env: imp-te, module-env: imp-me})

      provide-vals-specs = cases(A.Provide) _provide:
        | s-provide(shadow l, obj) =>
          specs = for map(f from obj.fields) block:
            when not(A.is-s-id(f.value)):
              raise("The rhs of an object provide was not an id: " + to-repr(f))
            end
            A.s-provide-name(f.l, A.s-module-ref(f.l, [list: f.value.id], some(A.s-name(f.l, f.name))))
          end + [list: A.s-provide-data(l, A.s-star(l, [list:]), [list:])]
          A.s-provide-block(l, empty, specs)
        | s-provide-all(shadow l) =>
          A.s-provide-block(l, empty, [list:
            A.s-provide-name(l, A.s-star(l, [list:])),
            A.s-provide-data(l, A.s-star(l, [list:]), [list:])])
        | s-provide-none(shadow l) =>
          A.s-provide-block(l, empty, [list:])
      end

      provide-types-specs = cases(A.ProvideTypes) _provide-types:
        | s-provide-types(shadow l, anns) =>
          A.s-provide-block(l, empty, for map(a from anns) block:
            when not(A.is-a-name(a.ann)): raise("Cannot use a non-name as a provided type") end
            A.s-provide-type(l, A.s-module-ref(a.ann.l, [list: a.ann.id], some(A.s-name(a.l, a.name))))
          end +
          [list: A.s-provide-data(l, A.s-star(l, [list:]), [list:])])
        | s-provide-types-none(shadow l) =>
          A.s-provide-block(l, empty, [list:])
        | s-provide-types-all(shadow l) =>
          A.s-provide-block(l, empty, [list:
            A.s-provide-data(l, A.s-star(l, [list:]), [list:]),
            A.s-provide-type(l, A.s-star(l, [list:]))])
      end
      
      all-provides = [list: provide-vals-specs, provide-types-specs] + provides

      # Each of these dictionaries maps from plain names to atoms, for example
      #   link => atom("link", 42)
      # the goal is to create a single s-provide-block with all the necessary
      # names and atoms that will be exposed. The atoms will be used by code
      # generation and by the type-checker/cross-module scope resolution to
      # pick out information about the binding (e.g. flatness, etc)
      # The actual values in the dictionaries are triples of
      #
      #  {Srcloc; Option<URI>; Atom}
      #
      # The location is the location of the provide clause, the URI is none if
      # the name is provided from this module, and some if it is re-provided
      # from another module
      provided-modules = [SD.mutable-string-dict:]
      provided-values = [SD.mutable-string-dict:]
      provided-types = [SD.mutable-string-dict:]
      provided-datatypes = [SD.mutable-string-dict:]

      fun is-hidden(hidden :: List<A.Name>, maybe-hidden-name :: String):
        for lists.any(h from hidden):
          h.toname() == maybe-hidden-name
        end
      end

      fun maybe-add(hidden, which-dict, maybe-add-name :: String, to-add):
        when not(is-hidden(hidden, maybe-add-name)):
          which-dict.set-now(maybe-add-name, to-add)
        end
      end



      fun expand-name-spec(which-dict, which-bindings, which-env, get-provided-bindings, spec, pre-path):
        cases(A.NameSpec) spec:
          | s-star(shadow l, hidden) =>
            remote-reference-uri = maybe-uri-for-path(pre-path, initial-env, final-visitor.module-env)
            cases(Option) remote-reference-uri:
              | none =>
                for each(shadow k from which-env.keys-list()):
                  bind = which-env.get-value(k)
                  when(bind.origin.new-definition and not(is-hidden(hidden, bind.atom.toname()))):
                    maybe-add(hidden, which-dict, bind.atom.toname(), {l; none; bind.atom})
                  end
                end
              | some(uri) =>
                bindings-from-module = get-provided-bindings(initial-env.provides-by-uri-value(uri))
                for each(shadow k from bindings-from-module.keys-list()):
                  # NOTE(joe): This is where we would do something like
                  # "prefix-out" by doing `prefix + k` below. The k that's the
                  # key in set-now is the name it's provided as, and the k in
                  # the s-name is the name to look for in the original module
                  which-dict.set-now(k, {l; remote-reference-uri; A.s-name(l, k) })
                end
            end
          | s-module-ref(shadow l, path, as-name) =>
            remote-reference-uri = path-uri(pre-path, path, initial-env, final-visitor.module-env)
            {maybe-uri; atom} = cases(Option) remote-reference-uri:
              | none =>
                b = which-env.get(path.first.toname())
                cases(Option) b block:
                  | some(shadow b) =>
                    if b.origin.new-definition:
                      { none; b.atom }
                    else:
                      { some(b.origin.uri-of-definition); b.origin.original-name }
                    end
                  | none =>
                    name-errors := link(C.unbound-id(A.s-id(l, A.s-name(l, path.last().toname()))), name-errors)
                    { none; A.s-name(l, path.last().toname()) }
                end
              | some(uri) =>
                { some(uri); A.s-name(l, path.last().toname()) }
            end
            cases(Option) as-name:
              | none => which-dict.set-now(atom.toname(), {l; maybe-uri; atom})
              | some(n) => which-dict.set-now(n.toname(), {l; maybe-uri; atom})
            end
        end
      end
      fun expand-data-spec(val-env, type-env, spec, pre-path, hidden):
        cases(A.NameSpec) spec:
          | s-star(shadow l, _) => # NOTE(joe): Assumption is that this s-star's hiding is always empty for s-provide-data
            remote-reference-uri = maybe-uri-for-path(pre-path, initial-env, final-visitor.module-env)
            # TODO(joe): need to condition on pre-path being empty/referring to
            # another module as above in expand-name-spec
            cases(Option) remote-reference-uri:
              | none => 
                for each(k from datatypes.keys-list-now()):
                  data-expr = datatypes.get-value-now(k)
                  expand-data-spec(val-env, type-env, A.s-module-ref(l, [list: A.s-name(l, data-expr.name)], none), pre-path, hidden)
                end
              | some(remote-uri) =>
                datatyps-from-module = initial-env.provides-by-uri-value(remote-uri).data-definitions
                for each(k from datatyps-from-module.keys-list()):
                  data-name = cases(C.DataExport) datatyps-from-module.get-value(k):
                    | d-alias(_, name) => name
                    | d-type(_, typ) => typ.name
                  end
                  expand-data-spec(val-env, type-env, A.s-module-ref(l, [list: A.s-name(l, data-name)], none), pre-path, hidden)
                end
            end
          | s-module-ref(shadow l, path, as-name) =>
            maybe-uri = path-uri(pre-path, path, initial-env, final-visitor.module-env)
            cases(Option) maybe-uri block:
              | none => # path must be a single element if there's no URI of a remote module
                        # e.g. provide: D end   NOT    provide: M.D end
                data-expr = datatypes.get-value-now(path.first.toname())
                maybe-add(hidden, provided-datatypes, data-expr.name, {l; none; data-expr.namet})
                data-checker-name = A.make-checker-name(data-expr.name)
                data-checker-vb = val-env.get-value(data-checker-name)
                maybe-add(hidden, provided-values, data-checker-name, {l; none; data-checker-vb.atom})
                data-alias-tb = type-env.get-value(data-expr.name)
                maybe-add(hidden, provided-types, data-expr.name, {l; none; data-alias-tb.atom})
                for each(v from data-expr.variants) block:
                  variant-vb = val-env.get-value(v.name)
                  checker-name = A.make-checker-name(v.name)
                  variant-checker-vb = val-env.get-value(checker-name)
                  maybe-add(hidden, provided-values, v.name, {l; none; variant-vb.atom})
                  maybe-add(hidden, provided-values, checker-name, {l; none; variant-checker-vb.atom})
                end
                
              | some(uri) =>
                datatype-name = path.last().toname()
                providing-module = initial-env.provides-by-uri-value(uri)
                maybe-datatype = initial-env.resolve-datatype-by-uri(uri, datatype-name)
                { datatype-uri; datatype } = cases(Option) maybe-datatype:
                  | none => 
                    cases(Option) providing-module.aliases.get(datatype-name):
                      | none => raise("Name " + datatype-name + " not defined as a type or datatype on " + uri)
                      | some(t) =>
                        cases(T.Type) t block:
                          | t-name(module-name, id, _, _) =>
                            when(not(T.is-module-uri(module-name))): raise("Expected a remote reference: " + to-repr(module-name)) end

                            remote-datatype = initial-env.provides-by-uri-value(module-name.uri).data-definitions.get(datatype-name)
                            cases(Option) remote-datatype:
                              | some(rd) => { module-name.uri; rd }
                              | none =>
                                raise("Cannot re-provide datatype " + datatype-name + " because it isn't a datatype in " + uri)
                            end
                        end
                    end
                  | some(datatype) => { uri; datatype }
                end
                fun add-value-if-defined(name):
                  when(providing-module.values.has-key(name)):
                    maybe-add(hidden, provided-values, name, {l; some(datatype-uri); A.s-name(l, name)})
                  end
                end
                maybe-add(hidden, provided-datatypes, datatype-name, {l; some(uri); A.s-name(l, datatype-name)})
                add-value-if-defined(A.make-checker-name(datatype-name))
                when(providing-module.aliases.has-key(datatype-name)):
                  maybe-add(hidden, provided-types, datatype-name, {l; some(datatype-uri); A.s-name(l, datatype-name)})
                end
                for each(v from datatype.variants) block:
                  add-value-if-defined(v.name)
                  add-value-if-defined(A.make-checker-name(v.name))
                end
            end
        end
      end
      fun expand(provide-spec, path):
        cases(A.ProvideSpec) provide-spec:
          | s-provide-name(shadow l, name-spec) =>
            expand-name-spec(provided-values, bindings, final-visitor.env, _.values, name-spec, path)
          | s-provide-type(shadow l, name-spec) =>
            expand-name-spec(provided-types, type-bindings, final-visitor.type-env, _.aliases, name-spec, path)
          | s-provide-module(shadow l, name-spec) =>
            expand-name-spec(provided-modules, module-bindings, final-visitor.module-env, _.modules, name-spec, path)
          | s-provide-data(shadow l, name-spec, hidden) =>
            expand-data-spec(final-visitor.env, final-visitor.type-env, name-spec, path, hidden)
          | else => nothing
        end
      end

      for each(pb from all-provides):
        for each(provide-spec from pb.specs):
          expand(provide-spec, pb.path)
        end
      end

      fun make-provide-spec({shadow l; maybe-uri; atom}, k, maker):
        name-spec = cases(Option) maybe-uri:
          | none => A.s-local-ref(l, atom, A.s-name(l, k))
          | some(uri) => A.s-remote-ref(l, uri, atom, A.s-name(l, k))
        end
        maker(name-spec)
      end

      for each(k from datatypes.keys-list-now()):
        dt = datatypes.get-value-now(k)
        provided-datatypes.set-now(k, {dt.l; none; dt.namet})
      end

      final-val-provides = for map(k from provided-values.keys-list-now()):
        make-provide-spec(provided-values.get-value-now(k), k, A.s-provide-name(l, _))
      end
      final-type-provides = for map(k from provided-types.keys-list-now()):
        make-provide-spec(provided-types.get-value-now(k), k, A.s-provide-type(l, _))
      end
      final-module-provides = for map(k from provided-modules.keys-list-now()):
        make-provide-spec(provided-modules.get-value-now(k), k, A.s-provide-module(l, _))
      end
      final-datatype-provides = for map(k from provided-datatypes.keys-list-now()):
        make-provide-spec(provided-datatypes.get-value-now(k), k, A.s-provide-data(l, _, empty))
      end

      one-true-provide = [list: A.s-provide-block(l, empty, final-val-provides + final-type-provides + final-module-provides + final-datatype-provides)]

      A.s-program(l, A.s-provide-none(l), A.s-provide-types-none(l), one-true-provide, imp-imps.reverse(), visit-body)
    end,
    method s-type-let-expr(self, l, binds, body, blocky):
      {e; te; bs} = for fold(acc from { self.env; self.type-env; empty }, b from binds):
        {e; te; bs} = acc
        cases(A.TypeLetBind) b block:
          | s-type-bind(l2, name, params, ann) =>
            shadow acc = { env: e, te: te }
            new-types = for fold(shadow acc from {env: acc.te, atoms: empty}, param from params):
              atom-env = make-atom-for(param, false, acc.env, type-bindings,
                C.type-bind(C.bo-local(l2, param), C.tb-type-var, _, none))
              { env: atom-env.env, atoms: link(atom-env.atom, acc.atoms) }
            end
            atom-env = make-atom-for(name, false, acc.te, type-bindings,
              C.type-bind(C.bo-local(l2, name), C.tb-type-let, _, none))
            new-bind = A.s-type-bind(l2, atom-env.atom, new-types.atoms.reverse(), ann.visit(self.{env: e, type-env: new-types.env}))
            { e; atom-env.env; link(new-bind, bs) }
          | s-newtype-bind(l2, name, tname) =>
            atom-env-t = make-atom-for(name, false, te, type-bindings,
              C.type-bind(C.bo-local(l2, name), C.tb-type-let, _, none))
            atom-env = make-atom-for(tname, false, e, bindings,
              C.value-bind(C.bo-local(l2, tname), C.vb-let, _, A.a-blank))
            new-bind = A.s-newtype-bind(l2, atom-env-t.atom, atom-env.atom)
            { atom-env.env; atom-env-t.env; link(new-bind, bs) }
        end
      end
      visit-body = body.visit(self.{env: e, type-env: te})
      A.s-type-let-expr(l, bs.reverse(), visit-body, blocky)
    end,
    method s-let-expr(self, l, binds, body, blocky):
      {e; bs; atoms} = for fold(acc from { self.env; empty; empty}, b from binds):
        {e; bs; atoms} = acc
        cases(A.LetBind) b block:
          | s-let-bind(l2, bind, expr) =>
            visited-ann = bind.ann.visit(self.{env: e})
            atom-env = make-atom-for(bind.id, bind.shadows, e, bindings,
              C.value-bind(C.bo-local(l2, bind.id), C.vb-let, _, visited-ann))
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
              C.value-bind(C.bo-local(l2, bind.id), C.vb-var, _, visited-ann))
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
              C.value-bind(C.bo-local(l2, bind.id), C.vb-let, _, bind.ann.visit(self)))
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
          C.value-bind(C.bo-local(a.l, a.id), C.vb-let, _, a.ann.visit(self)))
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
      {env; atoms} = for fold(acc from { self.type-env; empty }, param from params):
        {env; atoms} = acc
        atom-env = make-atom-for(param, false, env, type-bindings,
          C.type-bind(C.bo-local(l, param), C.tb-type-var, _, none))
        { atom-env.env; link(atom-env.atom, atoms) }
      end
      with-params = self.{type-env: env}
      result = A.s-data-expr(l, name, namet, atoms.reverse(),
        mixins.map(_.visit(with-params)), variants.map(_.visit(with-params)),
        shared-members.map(_.visit(with-params)), _check-loc, with-params.option(_check))
      datatypes.set-now(name, result)
      result
    end,
    method s-lam(self, l, name, params, args, ann, doc, body, _check-loc, _check, blocky) block:
     {ty-env; ty-atoms} = for fold(acc from {self.type-env; empty }, param from params):
        {env; atoms} = acc
        atom-env = make-atom-for(param, false, env, type-bindings,
          C.type-bind(C.bo-local(l, param), C.tb-type-var, _, none))
        { atom-env.env; link(atom-env.atom, atoms) }
      end
      with-params = self.{type-env: ty-env}
      {env; atoms} = for fold(acc from { with-params.env; empty }, a from args):
        {env; atoms} = acc
        atom-env = make-atom-for(a.id, a.shadows, env, bindings,
          C.value-bind(C.bo-local(a.l, a.id), C.vb-let, _, a.ann.visit(with-params)))
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
      {ty-env; ty-atoms} = for fold(acc from {self.type-env; empty }, param from params):
        {env; atoms} = acc
        atom-env = make-atom-for(param, false, env, type-bindings,
          C.type-bind(C.bo-local(param.l, param), C.tb-type-var, _, none))
        { atom-env.env; link(atom-env.atom, atoms) }
      end
      with-params = self.{type-env: ty-env}
      {env; atoms} = for fold(acc from { with-params.env; empty }, a from args):
        {env; atoms} = acc
        atom-env = make-atom-for(a.id, a.shadows, env, bindings,
          C.value-bind(C.bo-local(a.l, a.id), C.vb-let, _, a.ann.visit(with-params)))
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
      {ty-env; ty-atoms} = for fold(acc from {self.type-env; empty }, param from params):
        {env; atoms} = acc
        atom-env = make-atom-for(param, false, env, type-bindings,
          C.type-bind(C.bo-local(l, param), C.tb-type-var, _, none))
        { atom-env.env; link(atom-env.atom, atoms) }
      end
      with-params = self.{type-env: ty-env}
      {env; atoms} = for fold(acc from { with-params.env; empty }, a from args):
        {env; atoms} = acc
        atom-env = make-atom-for(a.id, a.shadows, env, bindings,
          C.value-bind(C.bo-local(a.l, a.id), C.vb-let, _, a.ann.visit(with-params)))
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
            A.s-assign(l, bind.atom, expr.visit(self))
            # This used to examine bind in more detail, and raise an error if it wasn't a var-bind
            # but that's better suited for a later pass
          else:
            A.s-assign(l, id, expr.visit(self))
          end
        | s-underscore(_) =>
          A.s-assign(l, id, expr.visit(self))
        | else => raise("Wasn't expecting a non-s-name in resolve-names for assignment: " + torepr(id))
      end
    end,
    method s-dot(self, l, obj, name):
      cases(A.Expr) obj:
        | s-id(l2, id) =>
          cases(A.Name) id block:
            | s-name(_, s) => 
              # NOTE(joe): This gives an ordering to names. If somehow we end up with
              # import foo as C
              #
              # C = 5
              # C.x
              #
              # and we _don't_ count it as a shadowing error, then the above
              # would be field-not-found
              if not(self.env.has-key(s)) and self.module-env.has-key(s):
                mod-bind = self.module-env.get-value(s)
                cases(Option) initial-env.value-by-uri(mod-bind.uri, name) block:
                  | none =>
                    name-errors := link(C.wf-err-split("The module " + s + " (" + mod-bind.uri + ") has no provided member " + name, [list: l, l2]), name-errors)
                    A.s-id-modref(l, mod-bind.atom, mod-bind.uri, name)
                  | some(ve) =>
                    cases(C.ValueExport) ve:
                      | v-var(_, t) => A.s-id-var-modref(l, mod-bind.atom, mod-bind.uri, name)
                      | else => A.s-id-modref(l, mod-bind.atom, mod-bind.uri, name)
                    end
                end
              else:
                A.s-dot(l, obj.visit(self), name)
              end
            | else => A.s-dot(l, obj.visit(self), name)
          end
        | else => A.s-dot(l, obj.visit(self), name)
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
              when self.type-env.has-key(s) block:
                name-errors := link(C.type-id-used-as-value(id, self.type-env.get-value(s).origin), name-errors)
              end
              A.s-id(l2, names.s-global(s))
            | some(vb) =>
              cases (C.ValueBinder) vb.binder:
                | vb-let => A.s-id(l2, vb.atom)
                | vb-letrec => A.s-id-letrec(l2, vb.atom, false)
                | vb-var => A.s-id-var(l2, vb.atom)
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
            C.value-bind(C.bo-local(l2, name), C.vb-let, _, ann.visit(self)))
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
    method a-blank(self): A.a-blank end,
    method a-any(self, l): A.a-any(l) end,
    method a-name(self, l, id): handle-ann(l, self.type-env, id) end,
    method a-arrow(self, l, args, ret, parens): A.a-arrow(l, args.map(_.visit(self)), ret.visit(self), parens) end,
    method a-arrow-argnames(self, l, args, ret, parens):
      A.a-arrow-argnames(l, args.map(_.visit(self)), ret.visit(self), parens)
    end,
    method a-method(self, l, args, ret): A.a-method(l, args.map(_.visit(self)), ret.visit(self)) end,
    method a-record(self, l, fields): A.a-record(l, fields.map(_.visit(self))) end,
    method a-app(self, l, ann, args): A.a-app(l, ann.visit(self), args.map(_.visit(self))) end,
    method a-pred(self, l, ann, exp): A.a-pred(l, ann.visit(self), exp.visit(self)) end,
    method a-dot(self, l, obj, field) block:
      cases(A.Name) obj block:
        | s-name(nameloc, s) =>
          cases(Option) self.module-env.get(s):
            | none => A.a-dot(l, obj, field) # NOTE(joe): Should this be error?
            | some(mb) =>
              A.a-dot(l, mb.atom, field)
          end
        | else =>
          name-errors := link(C.underscore-as-ann(obj.l), name-errors)
          A.a-dot(l, obj.visit(self), field)
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
    end,
  }
  C.resolved-names(p.visit(names-visitor), name-errors, C.computed-env(module-bindings, bindings, type-bindings, datatypes, final-visitor.module-env, final-visitor.env, final-visitor.type-env))
end


fun check-unbound-ids-bad-assignments(ast :: A.Program, resolved :: C.NameResolution, initial-env :: C.CompileEnvironment) block:
  var shadow errors = [list: ] # THE MUTABLE LIST OF ERRORS
  bindings = resolved.env.bindings
  type-bindings = resolved.env.type-bindings
  module-bindings = resolved.env.module-bindings
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
  ast.visit(A.default-iter-visitor.{
      method s-id(self, loc, id) block:
        when handle-id(id, loc):
          add-error(C.unbound-id(A.s-id(loc, id)))
        end
        true
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
            var-loc = get-origin-loc(binding.origin)
            add-error(C.bad-assignment(A.s-assign(loc, id, value), var-loc))
          end
        else:
          add-error(C.unbound-var(id.toname(), loc))
        end
        value.visit(self)
      end,
      method a-name(self, loc, id) block:
        if A.is-s-underscore(id) block:
          add-error(C.underscore-as-ann(id.l))
        else if A.is-s-type-global(id) and initial-env.globals.types.has-key(id.toname()):
          nothing
        else if type-bindings.has-key-now(id.key()):
          nothing
        else:
          add-error(C.unbound-type-id(A.a-name(loc, id)))
          nothing
        end
        true
      end,
      method a-dot(self, loc, name, field) block:
        if A.is-s-underscore(name) block:
          add-error(C.underscore-as-ann(name.l))
        else if A.is-s-type-global(name) and initial-env.globals.types.has-key(name.toname()):
          nothing
        else if module-bindings.has-key-now(name.key()):
          nothing
        else:
          add-error(C.unbound-type-id(A.a-name(loc, name)))
          nothing
        end
        true
      end
    })
  errors
where:
  p = PP.surface-parse(_, "test")
  px = p("x")
  resolved = C.resolved-names(px, empty, C.computed-env([SD.mutable-string-dict:], [SD.mutable-string-dict:], [SD.mutable-string-dict:], [SD.mutable-string-dict:], [SD.string-dict:], [SD.string-dict:], [SD.string-dict:]))
  unbound1 = check-unbound-ids-bad-assignments(px, resolved, C.no-builtins)
  unbound1.length() is 1
end
