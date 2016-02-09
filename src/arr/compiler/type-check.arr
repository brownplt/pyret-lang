provide *
provide-types *
import ast as A
import valueskeleton as VS
import string-dict as SD
import srcloc as SL
import either as E
import "compiler/ast-util.arr" as AU
import "compiler/type-structs.arr" as TS
import "compiler/type-check-structs.arr" as TCS
import "compiler/compile-structs.arr" as C
import "compiler/list-aux.arr" as LA

type Name                 = A.Name

type Loc                  = SL.Srcloc

type Type                 = TS.Type
t-name                    = TS.t-name
t-var                     = TS.t-var
t-arrow                   = TS.t-arrow
t-top                     = TS.t-top
t-bot                     = TS.t-bot
t-app                     = TS.t-app
t-record                  = TS.t-record
t-forall                  = TS.t-forall
t-ref                     = TS.t-ref
t-data                    = TS.t-data
t-data-construct          = TS.t-data-construct
t-data-single-construct   = TS.t-data-single-construct
t-existential             = TS.t-existential

is-t-record               = TS.is-t-record
is-t-name                 = TS.is-t-name
is-t-data                 = TS.is-t-data

type TypeMember           = TS.TypeMember
t-member                  = TS.t-member

type TypeVariant          = TS.TypeVariant
t-variant                 = TS.t-variant
t-singleton-variant       = TS.t-singleton-variant

type ModuleType           = TS.ModuleType
t-module                  = TS.t-module

type DataType             = TS.DataType
t-datatype                = TS.t-datatype

type Pair                 = TS.Pair
pair                      = TS.pair

type Either               = E.Either
left                      = E.left
right                     = E.right

t-number                  = TS.t-number
t-string                  = TS.t-string
t-boolean                 = TS.t-boolean
t-array                   = TS.t-array
t-srcloc                  = TS.t-srcloc

type Variance             = TS.Variance
constant                  = TS.constant
bivariant                 = TS.bivariant
invariant                 = TS.invariant
covariant                 = TS.covariant
contravariant             = TS.contravariant

type TCInfo               = TCS.TCInfo
tc-info                   = TCS.tc-info

type Context              = TCS.Context
typing-context            = TCS.typing-context

type SynthesisResult      = TCS.SynthesisResult
type CheckingResult       = TCS.CheckingResult
type FoldResult           = TCS.FoldResult

synthesis-result          = TCS.synthesis-result
synthesis-binding-result  = TCS.synthesis-binding-result
synthesis-err             = TCS.synthesis-err
checking-result           = TCS.checking-result
checking-err              = TCS.checking-err
fold-result               = TCS.fold-result
fold-errors               = TCS.fold-errors

bind                      = TCS.bind
map-bind                  = TCS.map-bind
check-bind                = TCS.check-bind
synth-bind                = TCS.synth-bind
fold-bind                 = TCS.fold-bind

fold-synthesis            = TCS.fold-synthesis
fold-checking             = TCS.fold-checking
collapse-fold-list        = TCS.collapse-fold-list
map-result                = TCS.map-result

all2-strict               = LA.all2-strict
map2-strict               = LA.map2-strict
fold2-strict              = LA.fold2-strict

t-num-binop = t-arrow([list: t-number, t-number], t-number)
t-num-cmp   = t-arrow([list: t-number, t-number], t-boolean)
t-str-binop = t-arrow([list: t-string, t-string], t-string)
t-str-cmp   = t-arrow([list: t-string, t-string], t-boolean)
t-method-binop = lam(field-name :: String):
  t-forall(
    [list:
      t-var(A.s-atom("B", 1)),
      t-var(A.s-atom("C", 1))
    ],
    t-arrow(
      [list:
        t-record([list:
          t-member(field-name, t-arrow([list: t-var(A.s-atom("B", 1))], t-var(A.s-atom("C", 1))))
        ]),
        t-var(A.s-atom("B",1))
      ],
      t-var(A.s-atom("C", 1))
    )
  )
end

# Math operators
t-plus-method   = t-method-binop("_plus")
t-minus-method  = t-method-binop("_minus")
t-divide-method = t-method-binop("_divide")
t-times-method  = t-method-binop("_times")

# Comparison operators
t-lt-method     = t-method-binop("_lessthan")
t-lte-method    = t-method-binop("_lessequal")
t-gt-method     = t-method-binop("_greaterthan")
t-gte-method    = t-method-binop("_greaterequal")

shadow fold2 = lam(f, base, l1, l2):
                 if l1.length() <> l2.length():
                   raise("Lists are not equal in length!")
                 else:
                   fold2(f, base, l1, l2)
                 end
               end

shadow map2 = lam(f, l1, l2):
                 if l1.length() <> l2.length():
                   raise("Lists are not equal in length!")
                 else:
                   map2(f, l1, l2)
                 end
               end

fun split<X, Y>(ps :: List<Pair<X,Y>>) -> Pair<List<X>, List<Y>>:
  fun step(p, curr):
    pair(link(p.left, curr.left), link(p.right, curr.right))
  end
  ps.foldr(step, pair(empty, empty))
end

fun new-existential():
  t-existential(A.global-names.make-atom("exists"))
end

fun import-to-string(i :: A.ImportType, c :: C.CompileEnvironment) -> String:
  c.mods.get-value(AU.import-to-dep(i).key()).from-uri
end

fun provides-as-dict(provides):
  for fold(d from SD.make-string-dict(), p from provides.fields):
    d.set(p.field-name, p.typ)
  end
end

# TODO(MATT): properly update info
fun type-check(program :: A.Program, compile-env :: C.CompileEnvironment, modules) -> C.CompileResult<A.Program>:
  info = TCS.empty-tc-info("default")
  globvs = compile-env.globals.values
  globts = compile-env.globals.types
  for each(g from globvs.keys-list()):
    info.typs.set-now(A.s-global(g).key(), globvs.get-value(g))
  end
  for each(g from globts.keys-list()):
    info.aliases.set-now(A.s-global(g).key(), globts.get-value(g))
  end
  for each(k from modules.keys-list-now()):
    when not(info.modules.has-key-now(k)):
      mod = modules.get-value-now(k).provides
      key = mod.from-uri
      val-provides = t-record(
        for map(v from mod.values.keys-list()): TS.t-member(v, mod.values.get-value(v)) end
      )
      module-type = TS.t-module(
          key,
          val-provides,
          mod.data-definitions,
          mod.aliases)
      info.modules.set-now(mod.from-uri, module-type)
      #for each(d from mod.data-definitions.keys-list()):
      #  info.data-exprs.set-now(d, mod.data-definitions.get-value(d))
      #end
    end
  end
  cases(A.Program) program:
    | s-program(l, _provide, provided-types, imports, body) =>
      for each(_import from imports):
        cases(A.Import) _import:
          | s-import(_, file, name) =>
            raise("NYI")
          | s-import-complete(_, vals, types, file, vname, tname) =>
            key = import-to-string(file, compile-env)
            info.mod-names.set-now(tname.key(), key)
            when not(info.modules.has-key-now(key)):
              mod = compile-env.mods.get-value(AU.import-to-dep(file).key())
              val-provides = t-record(
                for map(v from mod.values.keys-list()): TS.t-member(v, mod.values.get-value(v)) end
              )
              module-type = TS.t-module(
                  key,
                  val-provides,
                  mod.data-definitions,
                  mod.aliases)

              info.modules.set-now(key, module-type)
              for each(d from mod.data-definitions.keys-list()):
                info.data-exprs.set-now(d, mod.data-definitions.get-value(d))
              end
            end
            thismod = info.modules.get-value-now(key)
            info.typs.set-now(vname.key(), thismod.provides)
            info.aliases.set-now(tname.key(), TS.t-top)
            for each(a from types):
              info.aliases.set-now(a.key(), thismod.aliases.get-value(a.toname()))
            end
            # TODO(joe): This is kinda gross, skipping the name binding based on
            # built-in vs non-built-in module for now, until builtins can accurately
            # report their types programmatically
            #when not(A.is-s-const-import(file)):
              mod = compile-env.mods.get-value(AU.import-to-dep(file).key())
              for each(v from vals):
                cases(Option) mod.values.get(v.toname()):
                  | none =>
                    cases(Option) provides-as-dict(thismod.provides).get(v.toname()):
                      | none => nothing # still skipping complete misses for now
                      | some(typ) => info.typs.set-now(v.key(), typ)
                    end
                  | some(typ) => info.typs.set-now(v.key(), typ)
                end
              end
            #end
          | else => raise("typechecker received incomplete import")
        end
      end
      each(print, body.tosource().pretty(72))

      tc-result = checking(body, A.dummy-loc, t-top, typing-context([list: ], info))
      cases(CheckingResult) tc-result:
        | checking-result(new-body, context) =>
          each(print, new-body.tosource().pretty(72))
          C.ok(TCS.typed(A.s-program(l, _provide, provided-types, imports, new-body), context.info))
        | checking-err(err-list) =>
          C.err(err-list)
      end
    | else => raise("Attempt to type-check non-program: " + torepr(program))
  end
end

fun checking(e, expect-loc, expect-typ, context):
  result = _checking(e, expect-loc, expect-typ, context)
  print("")
  print("checking:")
  each(print, e.tosource().pretty(72))
  print("has type: " + tostring(expect-typ))
  print("result:")
  print(result)
end

fun _checking(e :: A.Expr, expect-loc :: A.Loc, expect-typ :: Type, context :: Context) -> CheckingResult:
  cases(A.Expr) e:
    | s-module(l, answer, defined-values, defined-types, provided-values, provided-types, checks) =>
      checking(answer, expect-loc, expect-typ, context)
        .map(A.s-module(l, _, defined-values, defined-types, provided-values, provided-types, checks))
    | s-type-let-expr(l, binds, body) =>
      for check-bind(ctxt from handle-type-let-binds(binds, context)):
        checking(body, expect-loc, expect-typ, ctxt)
          .map(A.s-type-let-expr(l, binds, _))
      end
    | s-let-expr(l, binds, body) =>
      binds-result = fold-synthesis(synthesis-let-bind, context, binds)
      binds-result.check-bind(lam(result-pair):
        checking(body, expect-loc, expect-typ, result-pair.left)
          .map(A.s-let-expr(l, result-pair.right, _))
        end)
    | s-letrec(l, binds, body) =>
      collected = collect-bindings(binds.map(lam(binding): binding.b end), context)
      fold-context = collected.bind(lam(self): fold-result(self.add-types(context)) end)

      binds-result = for bind(unfold-ctxt from fold-context):
        fold-synthesis(lam(binding, ctxt):
          cases(A.LetrecBind) binding:
            | s-letrec-bind(l2, b, value) =>
              recreate = A.s-letrec-bind(l2, _, _)
              synthesis-binding(b, value, recreate, ctxt)
          end
        end, unfold-ctxt, binds)
      end

      binds-result.check-bind(lam(result-pair):
        checking(body, expect-loc, expect-typ, result-pair.left)
          .map(A.s-letrec(l, result-pair.right, _))
        end)
    | s-hint-exp(l, hints, exp) =>
      raise("checking for s-hint-exp not implemented")
    | s-instantiate(l, expr, params) =>
      raise("checking for s-instantiate not implemented")
    | s-block(l, stmts) =>
      fun gen(curr, base):
        pair(link(pair(curr, base.right), base.left), t-top)
      end
      paired-stmts = stmts.foldr(gen, pair(empty, expect-typ)).left
      result = fold-checking(lam(stmt-typ-pair, ctxt):
        checking(stmt-typ-pair.left, expect-loc, stmt-typ-pair.right, ctxt)
      end, context, paired-stmts)
      result.check-bind(lam(result-pair):
        checking-result(A.s-block(l, result-pair.right), result-pair.left)
      end)
    | s-user-block(l, body) =>
      raise("checking for s-user-block not implemented")
    | s-fun(l, name, params, args, ann, doc, body, _check) =>
      raise("checking for s-fun not implemented")
    | s-type(l, name, ann) =>
      raise("checking for s-type not implemented")
    | s-newtype(l, name, namet) =>
      raise("checking for s-newtype not implemented")
    | s-var(l, name, value) =>
      raise("checking for s-var not implemented")
    | s-rec(l, name, value) =>
      raise("checking for s-rec not implemented")
    | s-let(l, name, value, keyword-val) =>
      raise("checking for s-let not implemented")
    | s-ref(l, ann) =>
      raise("checking for s-ref not implemented")
    | s-contract(l, name, ann) =>
      raise("checking for s-contract not implemented")
    | s-when(l, test, block) =>
      raise("checking for s-when not implemented")
    | s-assign(l, id, value) =>
      raise("checking for s-assign not implemented")
    | s-if-pipe(l, branches) =>
      raise("checking for s-if-pipe not implemented")
    | s-if-pipe-else(l, branches, _else) =>
      raise("checking for s-if-pipe-else not implemented")
    | s-if(l, branches) =>
      raise("checking for s-if not implemented")
    | s-if-else(l, branches, _else) =>
      raise("checking for s-if-else not implemented")
    | s-cases(l, typ, val, branches) =>
      checking-cases(l, typ, val, branches, none, expect-loc, expect-typ, context)
    | s-cases-else(l, typ, val, branches, _else) =>
      checking-cases(l, typ, val, branches, some(_else), expect-loc, expect-typ, context)
    | s-op(loc, op, l, r) =>
      raise("checking for s-op not implemented")
    | s-check-test(loc, op, refinement, l, r) =>
      raise("checking for s-check-test not implemented")
    | s-check-expr(l, expr, ann) =>
      raise("checking for s-check-expr not implemented")
    | s-paren(l, expr) =>
      raise("checking for s-paren not implemented")
    | s-lam(l, params, args, ann, doc, body, _check) =>
      raise("checking for s-lam not implemented")
    | s-method(l, params, args, ann, doc, body, _check) =>
      raise("checking for s-method not implemented")
    | s-extend(l, supe, fields) =>
      raise("checking for s-extend not implemented")
    | s-update(l, supe, fields) =>
      raise("checking for s-update not implemented")
    | s-obj(l, fields) =>
      raise("checking for s-obj not implemented")
    | s-array(l, values) =>
      raise("checking for s-array not implemented")
    | s-construct(l, modifier, constructor, values) =>
      raise("checking for s-construct not implemented")
    | s-app(l, _fun, args) =>
      check-synthesis(e, expect-typ, l, context)
    | s-prim-app(l, _fun, args) =>
      raise("checking for s-prim-app not implemented")
    | s-prim-val(l, name) =>
      raise("checking for s-prim-val not implemented")
    | s-id(l, id) =>
      check-synthesis(e, expect-typ, l, context)
    | s-id-var(l, id) =>
      raise("checking for s-id-var not implemented")
    | s-id-letrec(l, id, safe) =>
      check-synthesis(e, expect-typ, l, context)
    | s-undefined(l) =>
      raise("checking for s-undefined not implemented")
    | s-srcloc(l, loc) =>
      raise("checking for s-srcloc not implemented")
    | s-num(l, n) =>
      check-synthesis(e, expect-typ, l, context)
    | s-frac(l, num, den) =>
      raise("checking for s-frac not implemented")
    | s-bool(l, b) =>
      check-synthesis(e, expect-typ, l, context)
    | s-str(l, s) =>
      check-synthesis(e, expect-typ, l, context)
    | s-dot(l, obj, field) =>
      raise("checking for s-dot not implemented")
    | s-get-bang(l, obj, field) =>
      raise("checking for s-get-bang not implemented")
    | s-bracket(l, obj, field) =>
      raise("checking for s-bracket not implemented")
    | s-data(l, name, params, mixins, variants, shared-members, _check) =>
      raise("checking for s-data not implemented")
    | s-data-expr(l, name, namet, params, mixins, variants, shared-members, _check) =>
      raise("checking for s-data-expr not implemented")
    | s-for(l, iterator, bindings, ann, body) =>
      raise("checking for s-for not implemented")
    | s-check(l, name, body, keyword-check) =>
      raise("checking for s-check not implemented")
  end
end

fun synthesis(e, context):
  result = _synthesis(e, context)
  print("")
  print("synthesis on:")
  each(print, e.tosource().pretty(72))
  print("result:")
  print(result)
end

fun _synthesis(e :: A.Expr, context :: Context) -> SynthesisResult:
  cases(A.Expr) e:
    | s-module(l, answer, defined-values, defined-types, provided-values, provided-types, checks) =>
      synthesis(answer, context)
        .map-expr(A.s-module(l, _, defined-values, defined-types, provided-values, provided-types, checks))
    | s-type-let-expr(l, binds, body) =>
      raise("synthesis for s-type-let-expr not implemented")
    | s-let-expr(l, binds, body) =>
      raise("synthesis for s-let-expr not implemented")
    | s-letrec(l, binds, body) =>
      collected = collect-bindings(binds.map(lam(binding): binding.b end), context)
      fold-context = collected.bind(lam(self): fold-result(self.add-types(context)) end)

      binds-result = for bind(unfold-ctxt from fold-context):
        fold-synthesis(lam(binding, ctxt):
          cases(A.LetrecBind) binding:
            | s-letrec-bind(l2, b, value) =>
              recreate = A.s-letrec-bind(l2, _, _)
              synthesis-binding(b, value, recreate, ctxt)
          end
        end, unfold-ctxt, binds)
      end

      print(binds-result.synth-bind(lam(result-pair):
        synthesis(body, result-pair.left)
          .map-expr(A.s-letrec(l, result-pair.right, _))
      end))
    | s-hint-exp(l, hints, exp) =>
      raise("synthesis for s-hint-exp not implemented")
    | s-instantiate(l, expr, params) =>
      raise("synthesis for s-instantiate not implemented")
    | s-block(l, stmts) =>
      raise("synthesis for s-block not implemented")
    | s-user-block(l, body) =>
      raise("synthesis for s-user-block not implemented")
    | s-fun(l, name, params, args, ann, doc, body, _check) =>
      raise("synthesis for s-fun not implemented")
    | s-type(l, name, ann) =>
      raise("synthesis for s-type not implemented")
    | s-newtype(l, name, namet) =>
      raise("synthesis for s-newtype not implemented")
    | s-var(l, name, value) =>
      raise("synthesis for s-var not implemented")
    | s-rec(l, name, value) =>
      raise("synthesis for s-rec not implemented")
    | s-let(l, name, value, keyword-val) =>
      raise("synthesis for s-let not implemented")
    | s-ref(l, ann) =>
      raise("synthesis for s-ref not implemented")
    | s-contract(l, name, ann) =>
      raise("synthesis for s-contract not implemented")
    | s-when(l, test, block) =>
      raise("synthesis for s-when not implemented")
    | s-assign(l, id, value) =>
      raise("synthesis for s-assign not implemented")
    | s-if-pipe(l, branches) =>
      raise("synthesis for s-if-pipe not implemented")
    | s-if-pipe-else(l, branches, _else) =>
      raise("synthesis for s-if-pipe-else not implemented")
    | s-if(l, branches) =>
      raise("synthesis for s-if not implemented")
    | s-if-else(l, branches, _else) =>
      raise("synthesis for s-if-else not implemented")
    | s-cases(l, typ, val, branches) =>
      synthesis-cases(l, typ, val, branches, none, context)
    | s-cases-else(l, typ, val, branches, _else) =>
      synthesis-cases(l, typ, val, branches, some(_else), context)
    | s-op(loc, op, l, r) =>
      raise("synthesis for s-op not implemented")
    | s-check-test(loc, op, refinement, l, r) =>
      raise("synthesis for s-check-test not implemented")
    | s-check-expr(l, expr, ann) =>
      raise("synthesis for s-check-expr not implemented")
    | s-paren(l, expr) =>
      raise("synthesis for s-paren not implemented")
    | s-lam(l, params, args, ann, doc, body, _check) =>
      synthesis-fun(l, body, params, args, ann, A.s-lam(l, params, _, _, doc, _, _check), context)
    | s-method(l, params, args, ann, doc, body, _check) =>
      raise("synthesis for s-method not implemented")
    | s-extend(l, supe, fields) =>
      raise("synthesis for s-extend not implemented")
    | s-update(l, supe, fields) =>
      raise("synthesis for s-update not implemented")
    | s-obj(l, fields) =>
      raise("synthesis for s-obj not implemented")
    | s-array(l, values) =>
      raise("synthesis for s-array not implemented")
    | s-construct(l, modifier, constructor, values) =>
      raise("synthesis for s-construct not implemented")
    | s-app(l, _fun, args) =>
      synthesis-app-fun(l, _fun, args, context)
        .synth-bind(lam(new-fun, new-fun-loc, new-fun-typ, new-context):
          synthesis-spine(new-context.apply(new-fun-typ), A.s-app(l, _fun, _), args, l, new-context)
        end)
    | s-prim-app(l, _fun, args) =>
      raise("synthesis for s-prim-app not implemented")
    | s-prim-val(l, name) =>
      raise("synthesis for s-prim-val not implemented")
    | s-id(l, id) =>
      for synth-bind(id-typ from lookup-id(l, id, context)):
        synthesis-result(e, l, id-typ, context)
      end
    | s-id-var(l, id) =>
      raise("synthesis for s-id-var not implemented")
    | s-id-letrec(l, id, safe) =>
      for synth-bind(id-typ from lookup-id(l, id, context)):
        synthesis-result(e, l, id-typ, context)
      end
    | s-undefined(l) =>
      raise("synthesis for s-undefined not implemented")
    | s-srcloc(l, loc) =>
      raise("synthesis for s-srcloc not implemented")
    | s-num(l, n) =>
      synthesis-result(e, l, t-number, context)
    | s-frac(l, num, den) =>
      raise("synthesis for s-frac not implemented")
    | s-bool(l, b) =>
      synthesis-result(e, l, t-boolean, context)
    | s-str(l, s) =>
      synthesis-result(e, l, t-string, context)
    | s-dot(l, obj, field) =>
      synthesis(obj, context).bind(synthesis-field(l, _, _, _, field, A.s-dot, _))
    | s-get-bang(l, obj, field) =>
      raise("synthesis for s-get-bang not implemented")
    | s-bracket(l, obj, field) =>
      raise("synthesis for s-bracket not implemented")
    | s-data(l, name, params, mixins, variants, shared-members, _check) =>
      raise("synthesis for s-data not implemented")
    | s-data-expr(l, name, namet, params, mixins, variants, shared-members, _check) =>
      synthesis-datatype(l, name, namet, params, mixins, variants, shared-members, _check, context)
    | s-for(l, iterator, bindings, ann, body) =>
      raise("synthesis for s-for not implemented")
    | s-check(l, name, body, keyword-check) =>
      raise("synthesis for s-check not implemented")
  end.synth-bind(lam(ast, loc, typ, out-context):
    synthesis-result(ast, loc, out-context.apply(typ), out-context)
  end)
end

fun synthesis-spine(fun-type, recreate, args, app-loc, context):
  result = _synthesis-spine(fun-type, recreate, args, app-loc, context)
  print("")
  print("spine synthesis with type: " + tostring(fun-type))
  print("args:")
  each(lam(arg): each(print, arg.tosource().pretty(72)) end, args)
  print("result:")
  print(result)
end

fun _synthesis-spine(fun-type :: Type, recreate :: (List<A.Expr> -> A.Expr), args :: List<A.Expr>, app-loc :: Loc, context :: Context) -> SynthesisResult:
  cases(Type) fun-type:
    | t-arrow(arg-typs, ret-typ) =>
      result = fold2-strict(lam(acc, arg, arg-typ):
        for bind(exprs-and-context from acc):
          checking(arg, app-loc, arg-typ, exprs-and-context.right)
            .fold-bind(lam(new-arg, out-context):
              fold-result(pair(link(new-arg, exprs-and-context.left), out-context))
            end)
        end
      end, fold-result(pair(empty, context)), args, arg-typs)

      cases(Option<FoldResult<Pair<List<A.Expr>, Context>>>) result:
        | none => synthesis-err([list: C.incorrect-number-of-args(app-loc)])
        | some(folded) =>
          for synth-bind(exprs-and-context from folded):
            synthesis-result(recreate(exprs-and-context.left),
                             app-loc, ret-typ, exprs-and-context.right)
          end

      end
    | t-forall(introduces, onto) =>
      new-type = introduces.foldl(lam(type-var, new-type):
        onto.substitute(type-var, new-existential())
      end, onto)
      synthesis-spine(new-type, recreate, args, app-loc, context)
    | t-bot =>
      raise("synthesis-spine for t-bot not implemented yet")
    | t-existential(id) =>
      raise("synthesis-spine for t-existential not implemented yet")
    | t-data(params, variants, fields, refine) =>
      raise("synthesis-spine for t-data not implemented yet")
    | t-data-construct(params, arg-typs, ret-typ) =>
      if is-empty(params):
        result = fold2-strict(lam(acc, arg, arg-typ):
          for bind(exprs-and-context from acc):
            checking(arg, app-loc, arg-typ, exprs-and-context.right)
              .fold-bind(lam(new-arg, out-context):
                fold-result(pair(link(new-arg, exprs-and-context.left), out-context))
              end)
          end
        end, fold-result(pair(empty, context)), args, arg-typs)

        cases(Option<FoldResult<Pair<List<A.Expr>, Context>>>) result:
          | none => synthesis-err([list: C.incorrect-number-of-args(app-loc)])
          | some(folded) =>
            for synth-bind(exprs-and-context from folded):
              synthesis-result(recreate(exprs-and-context.left),
                              app-loc, ret-typ, exprs-and-context.right)
            end

        end
      else:
        raise("haven't dealt with params yet")
      end
    | t-data-single-construct(params, ret) =>
      raise("synthesis-spine for t-data-single-construct not implemented yet")
    | else =>
      synthesis-err([list: C.apply-non-function(app-loc, fun-type)])
  end
end

fun synthesis-let-bind(binding :: A.LetBind, context :: Context) -> SynthesisResult:
  cases(A.LetBind) binding:
    | s-let-bind(l, b, value) =>
      synthesis-binding(b, value, A.s-let-bind(l, _, _), context)
    | s-var-bind(l, b, value) =>
      synthesis-binding(b, value, A.s-var-bind(l, _, _), context)
  end
end

fun synthesis-binding(binding :: A.Bind, value :: A.Expr, recreate :: (A.Bind, A.Expr -> A.LetBind), context :: Context) -> SynthesisResult:
  fun process-value(expr, typ-loc, typ, out-context):
    synthesis-binding-result(recreate(binding, expr), typ, out-context)
  end
  for synth-bind(maybe-typ from to-type(binding.ann, context)):
    cases(Option<Type>) maybe-typ:
      | none =>
        synthesis(value, context)
      | some(typ) =>
        checking(value, binding.l, typ, context).synth-bind(synthesis-result(_, binding.l, typ, _))
    end.bind(process-value)
  end
end

fun to-type(in-ann :: A.Ann, context :: Context) -> FoldResult<Option<Type>>:
  cases(A.Ann) in-ann:
    | a-blank =>
      fold-result(none)
    | a-any =>
      fold-result(some(t-top))
    | a-name(l, id) =>
      cases(Option<Type>) context.info.aliases.get-now(id.key()):
        | some(typ) =>
          fold-result(some(typ))
        | none =>
          fold-result(some(t-name(none, id)))
      end
    | a-type-var(l, id) =>
      raise("a-type-var not implemented")
    | a-arrow(l, args, ret, use-parens) =>
      raise("a-arrow not implemented")
    | a-method(l, args, ret) =>
      raise("a-method not implemented")
    | a-record(l, fields) =>
      raise("a-record not implemented")
    | a-app(l, ann, args) =>
      raise("a-app not implemented")
    | a-pred(l, ann, exp) =>
      for bind(maybe-typ from to-type(ann, context)):
        cases(Option<Type>) maybe-typ:
          | some(typ) =>
            expect-typ = t-arrow([list: typ], t-boolean)
            # TODO(MATT): decide if I want to keep handling the errors this way
            cases(CheckingResult) checking(exp, l, expect-typ, context):
              | checking-err(errs) => errs.map(context.info.errors.insert)
              | else => nothing
            end
            fold-result(some(typ))
          | none => raise("figure out what to do here")
        end
      end
    | a-dot(l, obj, field) =>
      raise("a-dot not implemented")
    | a-checked(checked, residual) =>
      raise("a-checked should not be appearing before type checking!")
  end
end

# TODO(MATT): introduce errors here or satisfies-type?
fun check-synthesis(e :: A.Expr, expect-typ :: Type, expect-loc :: A.Loc, context :: Context) -> CheckingResult:
  synthesis(e, context).check-bind(lam(new-expr, new-loc, new-typ, new-context):
    for check-bind(ctxt from satisfies-type(new-context.apply(new-typ), new-context.apply(expect-typ), new-context)):
      checking-result(e, ctxt)
    end
  end)
end

fun satisfies-type(subtyp, supertyp, context):
  result = _satisfies-type(subtyp, supertyp, context)
  print("")
  print(tostring(subtyp) + " <: " + tostring(supertyp))
  print("result:")
  print(result)
end

# TODO(MATT): add tests back
# TODO(MATT): decide if fold-errors should be empty or not
# left <: right
fun _satisfies-type(subtyp :: Type, supertyp :: Type, context :: Context) -> FoldResult<Context>:
  shadow subtyp = resolve-alias(subtyp, context)
  shadow supertyp = resolve-alias(supertyp, context)
  cases(Type) supertyp:
    | t-existential(b-id) =>
      cases(Type) subtyp:
        # TODO(MATT): this doesn't quite cover it but we'll come back to this
        | t-existential(a-id) =>
          if a-id == b-id:
            fold-result(context)
          else:
            fold-errors(empty)
          end
        | else => instantiate-right(subtyp, supertyp, context)
      end
    | else =>
      cases(Type) subtyp:
        | t-name(a-mod, a-id) =>
          cases(Type) supertyp:
            | t-top => fold-result(context)
            | t-name(b-mod, b-id) =>
              if (a-mod == b-mod) and (a-id == b-id):
                fold-result(context)
              else:
                fold-errors(empty)
              end
            | t-bot => fold-errors(empty)
            | else =>
              raise("not yet handled")
          end
        | t-var(a-id) =>
          raise("satisfies-type for t-var not implemented")
        | t-arrow(a-args, a-ret) =>
          cases(Type) supertyp:
            | t-top => fold-result(context)
            | t-arrow(b-args, b-ret) =>
              # Order is important because contravariance!
              # TODO(MATT): deal with substitutions from discovered types
              result = fold2-strict(lam(fold-ctxt, b-arg, a-arg):
                for bind(ctxt from fold-ctxt):
                  satisfies-type(b-arg, a-arg, ctxt)
                end
              end, fold-result(context), b-args, a-args)
              cases(Option<Context>) result:
                | none => fold-errors(empty)
                | some(fold-ctxt) => fold-ctxt.bind(satisfies-type(a-ret, b-ret, _))
              end
            | else => fold-errors(empty)
          end
        | t-forall(a-introduces, a-onto) =>
          raise("satisfies-type for t-forall not implemented")
        | t-app(a-onto, a-args) =>
          raise("satisfies-type for t-app not implemented")
        | t-top =>
          raise("satisfies-type for t-top  not implemented")
        | t-bot =>
          fold-result(context)
        | t-record(fields) =>
          raise("satisfies-type for t-record not implemented")
        | t-ref(a-typ) =>
          raise("satisfies-type for t-ref not implemented")
        | t-data(params, variants, fields, refine) =>
          raise("satisfies-type for t-data not implemented")
        | t-data-construct(params, args, ret) =>
          raise("satisfies-type for t-data-construct not implemented")
        | t-data-single-construct(params, ret) =>
          raise("satisfies-type for t-data-single-construct not implemented")
        | t-existential(a-id) =>
          # TODO(MATT): this doesn't quite cover it but we'll come back to this
          cases(Type) supertyp:
            | t-existential(b-id) =>
              if a-id == b-id:
                fold-result(context)
              else:
                fold-errors(empty)
              end
            | else => instantiate-left(subtyp, supertyp, context)
          end
      end
  end
end

fun instantiate-right(subtyp :: Type, supertyp :: Type, context :: Context) -> FoldResult<Context>:
  cases(Type) supertyp:
    | t-existential(b-id) =>
      cases(Type) subtyp:
        | t-name(a-mod, a-id) => fold-result(context.assign-existential(supertyp, subtyp))
        | t-var(a-id) =>
          raise("instantiate-right for t-var not implemented yet")
        | t-arrow(a-args, a-ret) =>
          raise("instantiate-right for t-arrow not implemented yet")
        | t-forall(a-introduces, a-onto) =>
          raise("instantiate-right for t-forall not implemented yet")
        | t-app(a-onto, a-args) =>
          raise("instantiate-right for t-app not implemented yet")
        | t-top =>
          raise("instantiate-right for t-top not implemented yet")
        | t-bot =>
          raise("instantiate-right for t-bot not implemented yet")
        | t-record(fields) =>
          raise("instantiate-right for t-record not implemented yet")
        | t-ref(a-typ) =>
          raise("instantiate-right for t-ref not implemented yet")
        | t-data(params, variants, fields, refine) =>
          raise("instantiate-right for t-data not implemented")
        | t-data-construct(params, args, ret) =>
          raise("instantiate-right for t-data-construct not implemented")
        | t-data-single-construct(params, ret) =>
          raise("instantiate-right for t-data-single-construct not implemented")
        | t-existential(a-id) =>
          raise("instantiate-right for t-existential not implemented yet")
      end
    | else => raise("cannot instantiate non-existential")
  end
end

fun instantiate-left(subtyp :: Type, supertyp :: Type, context :: Context) -> FoldResult<Context>:
  cases(Type) subtyp:
    | t-existential(a-id) =>
      cases(Type) supertyp:
        | t-name(b-mod, b-id) => fold-result(context.assign-existential(subtyp, supertyp))
        | t-var(b-id) =>
          raise("instantiate-right for t-var not implemented yet")
        | t-arrow(b-args, b-ret) =>
          raise("instantiate-right for t-arrow not implemented yet")
        | t-forall(b-introduces, b-onto) =>
          raise("instantiate-right for t-forall not implemented yet")
        | t-app(b-onto, b-args) =>
          raise("instantiate-right for t-app not implemented yet")
        | t-top =>
          raise("instantiate-right for t-top not implemented yet")
        | t-bot =>
          raise("instantiate-right for t-bot not implemented yet")
        | t-record(fields) =>
          raise("instantiate-right for t-record not implemented yet")
        | t-ref(b-typ) =>
          raise("instantiate-right for t-ref not implemented yet")
        | t-data(params, variants, fields, refine) =>
          raise("instantiate-left for t-data not implemented")
        | t-data-construct(params, args, ret) =>
          raise("instantiate-left for t-data-construct not implemented")
        | t-data-single-construct(params, ret) =>
          raise("instantiate-left for t-data-single-construct not implemented")
        | t-existential(b-id) =>
          raise("instantiate-right for t-existential not implemented yet")
      end
    | else => raise("cannot instantiate non-existential")
  end
end

fun resolve-alias(t :: Type, context :: Context) -> Type:
  cases(Type) t:
    | t-name(a-mod, a-id) =>
      cases(Option) a-mod:
        | none =>
          cases(Option) context.info.aliases.get-now(a-id.key()):
            | none => t
            | some(aliased) =>
              resolve-alias(aliased, context)
          end
        | some(mod) =>
          if mod == "builtin":
            cases(Option) context.info.aliases.get-now(a-id.key()):
              | none => t
              | some(aliased) => aliased
            end
          else:
            cases(Option) context.info.modules.get-value-now(mod).aliases.get(a-id.toname()):
              | none => t
              | some(aliased) => aliased
                # resolve-alias(aliased, info)
            end
          end
      end
    | else => t
  end
end

fun lookup-id(blame-loc :: A.Loc, id, context :: Context) -> FoldResult<Type>:
  id-key = if is-string(id):
             id
           else if A.is-Name(id):
             id.key()
           else:
             raise("I don't know how to lookup your id! Received: " + torepr(id))
           end
  if context.has-var-key(id-key):
    fold-result(context.get-var-type(id-key))
  else:
    id-expr = if is-string(id):
                A.s-id(blame-loc, A.s-global(id))
              else if A.is-Name(id):
                A.s-id(blame-loc, id)
              else:
                A.s-id(blame-loc, A.s-global(tostring(id)))
              end
    fold-errors([list: C.unbound-id(id-expr)])
  end
end

fun synthesis-fun(l :: A.Loc, body :: A.Expr, params :: List<A.Name>, args :: List<A.Bind>, ret-ann :: A.Ann, recreate :: (List<A.Bind>, A.Ann, A.Expr -> A.Expr), context :: Context) -> SynthesisResult:
  type-var-context = params.foldl(lam(variable, ctxt): ctxt.add-type-var(variable) end, context)
  collected = collect-bindings(args, type-var-context)

  for synth-bind(coll from collected):
    # TODO(MATT): is this keeping too much of the context?
    fun process(new-body :: A.Expr, ret-typ :: Type, out-context :: Context) -> SynthesisResult:
      new-fun = recreate(args, ret-ann, new-body)
      new-typ = if is-empty(params):
        t-arrow(args.foldr(lam(arg, lst):
          link(coll.types.get-value(arg.id.key()), lst)
        end, empty), ret-typ)
      else:
        raise("I have to handle parametric functions")
      end
      synthesis-result(new-fun, l, new-typ, out-context)
    end

    for synth-bind(maybe-typ from to-type(ret-ann, type-var-context)):
      cases(Option<Type>) maybe-typ:
        | some(typ) => checking(body, l, typ, coll.add-types(type-var-context)).synth-bind(process(_, typ, _))
        | none =>
          new-exists = new-existential()
          checking(body, l, new-exists, coll.add-types(type-var-context))
            .synth-bind(process(_, new-exists, _))
      end
    end
  end
end

# TODO(MATT): look more closely at this
fun synthesis-app-fun(app-loc :: Loc, _fun :: A.Expr, args :: List<A.Expr>, context :: Context) -> SynthesisResult:
  cases(A.Expr) _fun:
    | s-id(fun-loc, id) =>
      result = synthesis-result(_fun, _, _)
      fun pick2(num-typ :: Type, rec-typ :: Type):
        cases(List<A.Expr>) args:
          | empty      =>
            synthesis-err([list: C.incorrect-number-of-args(app-loc)])
          | link(f, r) =>
            synthesis(f, context).bind(
              lam(_, l, f-typ):
                ask:
                  | f-typ == t-number  then: result(l, num-typ)
                  | is-t-record(f-typ) then: result(l, rec-typ)
                  | otherwise: synthesis-err([list:
                      C.incorrect-type(tostring(f-typ), l, "Number or an object with the field " + id.toname(), app-loc)])
                end
              end)
        end
      end
      fun pick3(num-typ :: Type, str-typ :: Type, rec-typ :: Type):
        cases(List<A.Expr>) args:
          | empty      =>
            synthesis-err([list: C.incorrect-number-of-args(app-loc)])
          | link(f, r) =>
            synthesis(f, context).bind(
              lam(_, l, f-typ):
                ask:
                  | f-typ == t-number  then: result(l, num-typ)
                  | f-typ == t-string  then: result(l, str-typ)
                  | is-t-record(f-typ) then: result(l, rec-typ)
                  | otherwise: synthesis-err([list:
                    C.incorrect-type(tostring(f-typ), l, "Number, String or an object with the field " + id.toname(), app-loc)])
                end
              end)
        end
      end
      ask:
        # Math operations
        | id == A.s-global("_plus")   then: pick3(t-num-binop, t-str-binop, t-plus-method)
        | id == A.s-global("_times")  then: pick2(t-num-binop, t-times-method)
        | id == A.s-global("_divide") then: pick2(t-num-binop, t-divide-method)
        | id == A.s-global("_minus")  then: pick2(t-num-binop, t-minus-method)
        # Comparison operations
        | id == A.s-global("_lessthan")     then: pick3(t-num-cmp, t-str-cmp, t-lt-method)
        | id == A.s-global("_lessequal")    then: pick3(t-num-cmp, t-str-cmp, t-lte-method)
        | id == A.s-global("_greaterthan")  then: pick3(t-num-cmp, t-str-cmp, t-gt-method)
        | id == A.s-global("_greaterequal") then: pick3(t-num-cmp, t-str-cmp, t-gte-method)
        | otherwise: synthesis(_fun, context)
      end
    | else =>
      synthesis(_fun, context)
  end
end

fun check-app(app-loc :: Loc, args :: List<A.Expr>, arrow-typ :: Type, expect-typ :: Type, context :: Context) -> FoldResult<Pair<Pair<List<A.Expr>, Context>, Type>>:
  cases(Type) arrow-typ:
    | t-arrow(arg-typs, ret-typ) =>
      result = fold2-strict(lam(acc, arg, arg-typ):
        for bind(exprs-and-context from acc):
          checking(arg, app-loc, arg-typ, exprs-and-context.right)
            .fold-bind(lam(new-arg, out-context):
              fold-result(pair(link(new-arg, exprs-and-context.left), out-context))
            end)
        end
      end, fold-result(pair(empty, context)), args, arg-typs)
      result.or-else(C.incorrect-number-of-args(app-loc))
        .bind(lam(res): fold-result(pair(res, ret-typ)) end)
    | t-forall(introduces, onto) =>
      raise("check-app not implemented for forall")
    | t-bot =>
      raise("check app for t-bot not implemented yet")
    | t-data(params, variants, fields, refine) =>
      raise("check-app for t-data not implemented")
    | t-data-construct(params, data-args, ret) =>
      raise("check-app for t-data-construct not implemented")
    | t-data-single-construct(params, ret) =>
      raise("check-app for t-data-single-construct not implemented")
    | else =>
      fold-errors([list: C.apply-non-function(app-loc, arrow-typ)])
  end
end

# left is type, right is existential type
fun collect-bindings(binds :: List<A.Bind>, context :: Context)
  -> FoldResult<{types :: SD.StringDict<Type>, add-types :: (Context -> Context)}>:
  result = binds.foldr(lam(binding, fold-results):
    for bind(results from fold-results):
      for bind(maybe-typ from to-type(binding.ann, context)):
        new-typ = cases(Option<Type>) maybe-typ:
          | some(typ) => typ
          | none => new-existential()
        end
        fold-result(results.set(binding.id.key(), new-typ))
      end
    end
  end, fold-result(SD.make-string-dict()))

  result.bind(lam(types):
    fold-result({
      types: types,
      add-types(self, ctxt):
        self.types.keys().fold(lam(fold-ctxt, id):
          fold-ctxt.add-term-var(id, self.types.get-value(id))
        end, ctxt)
      end})
    end)
end

fun handle-type-let-binds(bindings :: List<A.TypeLetBind>, context :: Context) -> FoldResult<Context>:
  bindings.foldr(lam(binding, fold-ctxt):
    for bind(ctxt from fold-ctxt):
      cases(A.TypeLetBind) binding:
        | s-type-bind(_, name, ann) =>
          raise("s-type-bind not implemented")
        | s-newtype-bind(l, name, namet) =>
          typ = t-name(none, namet)
          namet-key = namet.key()
          ctxt.info.branders.set-now(namet-key, typ)
          ctxt.info.aliases.set-now(name.key(), typ)
          new-context = ctxt.add-term-var(namet-key, t-record([list:
            t-member("test", t-arrow([list: t-top], t-boolean)),
            t-member("brand", t-arrow([list: t-top], typ))
          ]))
          fold-result(new-context)
      end
    end
  end, fold-result(context))
end

fun synthesis-datatype(l :: Loc, name :: String, namet :: A.Name, params :: List<A.Name>, mixins, variants :: List<A.Variant>, fields :: List<A.Member>, _check :: Option<A.Expr>, context :: Context) -> SynthesisResult:
  if context.info.branders.has-key-now(namet.key()):
    brander-typ = context.info.branders.get-value-now(namet.key())
    t-vars = params.map(t-var(_))

    for synth-bind(variants-result from map-result(to-type-variant(_, context), variants)):
      split-variants = split(variants-result)
      new-variants = split-variants.left
      variant-typs = split-variants.right

      # TODO(MATT): meet of the variants

      for synth-bind(fields-result from map-result(to-type-member(_, context), fields)):
        split-fields = split(fields-result)

        new-data-expr = A.s-data-expr(l, name, namet, params, mixins, new-variants, split-fields.left, _check)

        new-data-type = t-data(t-vars, variant-typs, split-fields.right, none)
        new-context = context.add-data-type(namet.key(), new-data-type)

        data-fields = link(t-member(name, t-arrow([list: t-top], t-boolean)),
          for map(variant-typ from variant-typs):
            t-member(variant-typ.name, mk-constructor-type(variant-typ, brander-typ, t-vars))
          end +
          for map(variant-typ from variant-typs):
            t-member("is-" + variant-typ.name, t-arrow([list: t-top], t-boolean))
          end)
        data-expr-typ = t-record(data-fields)
        synthesis-result(new-data-expr, l, data-expr-typ, new-context)
      end
    end
  else:
    raise("Cannot find brander name in brander dictionary!")
  end
end

fun to-type-variant(variant :: A.Variant, context :: Context) -> FoldResult<Pair<A.Variant, TypeVariant>>:
  cases(A.Variant) variant:
    | s-variant(l, constr-loc, name, members, with-members) =>
      # TODO(MATT): check out recursive methods
      for bind(result from map-result(to-type-member(_, context), with-members)):
        split-result = split(result)
        new-with-members = split-result.left
        with-type-members = split-result.right

        fun process-member(member):
          wrap = cases(A.VariantMemberType) member.member-type:
            | s-normal => lam(x): x end
            | s-mutable => t-ref
          end
          for bind(maybe-typ from to-type(member.bind.ann, context)):
            cases(Option<Type>) maybe-typ:
              | none => raise("do something here")
              | some(typ) => fold-result(t-member(member.bind.id.toname(), wrap(typ)))
            end
          end
        end

        for bind(type-members from map-result(process-member, members)):
          new-variant = A.s-variant(l, constr-loc, name, members, new-with-members)
          type-variant = t-variant(name, type-members, with-type-members)
          fold-result(pair(new-variant, type-variant))
        end
      end
    | s-singleton-variant(l, name, with-members) =>
      raise("to-type-variant for s-singleton-variant not implemented")
  end
end

fun to-type-member(field :: A.Member, context :: Context) -> FoldResult<Pair<A.Member, TypeMember>>:
  cases(A.Member) field:
    | s-data-field(l, name, value) =>
      raise("to-type member for s-data-field")
    | s-mutable-field(l, name, ann, value) =>
      raise("to-type member for s-mutable-field")
    | s-method(l, name, args, ann, doc, body, _check) =>
      raise("to-type member for s-method")
  end
end

fun mk-constructor-type(variant-typ :: TypeVariant, brander-typ :: Type, params :: List<Type>) -> Type:
  cases(TypeVariant) variant-typ:
    | t-variant(name, fields, _) =>
      t-data-construct(params, fields.map(_.typ), brander-typ)
    | t-singleton-variant(name, _) =>
      t-data-single-construct(params, brander-typ)
  end
end

fun synthesis-field(access-loc :: Loc, obj :: A.Expr, obj-typ-loc :: A.Loc, obj-typ :: Type, field-name :: String, recreate :: (A.Loc, A.Expr, String -> A.Expr), context :: Context) -> SynthesisResult:
  record-view(access-loc, obj-typ-loc, obj-typ,
  lam(l, maybe-obj-fields):
    cases(Option<List<TypeMember>>) maybe-obj-fields:
      | some(obj-fields) =>
        cases(Option<TypeMember>) TS.type-members-lookup(obj-fields, field-name):
          | some(tm) =>
            synthesis-result(recreate(l, obj, field-name), l, tm.typ, context)
          | none =>
            synthesis-err([list: C.object-missing-field(field-name, "{" + obj-fields.map(tostring).join-str(", ") + "}", l, access-loc)])
        end
      | none =>
        raise("not sure what to do here yet")
        # synthesis-result(recreate(l, obj, field-name), l, t-bot)
    end
  end, context)
end

fun record-view(access-loc :: Loc, obj-typ-loc :: A.Loc, obj-typ :: Type,
                handle :: (Loc, Option<List<TypeMember>> -> SynthesisResult),
                context :: Context) -> SynthesisResult:
  non-obj-err = synthesis-err([list: C.incorrect-type(tostring(obj-typ), obj-typ-loc, "an object type", access-loc)])
  cases(Type) obj-typ:
    | t-record(members) =>
      handle(obj-typ-loc, some(members))
    | t-data(_, variants, fields, refine) => # TODO(MATT): refine
      handle(obj-typ-loc, some(fields))
    | t-bot =>
      handle(obj-typ-loc, none) # TODO(MATT): keep this?
    | else => non-obj-err
  end
end

fun synthesis-cases(l :: A.Loc, ann :: A.Ann, val :: A.Expr, branches :: List<A.CasesBranch>, maybe-else :: Option<A.Expr>, context :: Context) -> SynthesisResult:
  handle-cases(l, ann, val, branches, maybe-else, A.dummy-loc, none, context, synth-bind, synthesis-err, synthesis-cases-has-else, synthesis-cases-no-else)
end

fun synthesis-cases-has-else(l :: A.Loc, ann :: A.Ann, new-val :: A.Expr, split-result :: Pair<List<A.CasesBranch>,List<Type>>, _else :: A.Expr, context :: Context) -> SynthesisResult:
  synthesis(_else, context).bind(
    lam(new-else, _, else-typ, out-context):
      branches-typ = meet-branch-typs(link(else-typ, split-result.right), out-context)
      new-cases = A.s-cases-else(l, ann, new-val, split-result.left, new-else)
      synthesis-result(new-cases, l, branches-typ, out-context)
    end)
end

fun synthesis-cases-no-else(l :: A.Loc, ann :: A.Ann, new-val :: A.Expr, split-result :: Pair<List<A.CasesBranch>,List<Type>>, context :: Context) -> SynthesisResult:
  branches-typ = meet-branch-typs(split-result.right, context)
  new-cases = A.s-cases(l, ann, new-val, split-result.left)
  synthesis-result(new-cases, l, branches-typ, context)
end

fun checking-cases(l :: A.Loc, ann :: A.Ann, val :: A.Expr, branches :: List<A.CasesBranch>, maybe-else :: Option<A.Expr>, expect-loc :: A.Loc, expect-typ :: Type, context :: Context) -> CheckingResult:
  handle-cases(l, ann, val, branches, maybe-else, expect-loc, some(expect-typ), context, check-bind, checking-err, checking-cases-has-else(expect-loc, expect-typ), checking-cases-no-else)
end

fun checking-cases-has-else(expect-loc :: A.Loc, expect-typ :: Type):
  lam(l :: A.Loc, ann :: A.Ann, new-val :: A.Expr, split-result :: Pair<List<A.CasesBranch>,List<Type>>, _else :: A.Expr, context :: Context) -> CheckingResult:
    checking(_else, expect-loc, expect-typ, context).bind(lam(new-else, out-context):
      new-cases = A.s-cases-ese(l, ann, new-val, split-result.left, new-else)
      checking-result(new-cases, out-context)
    end)
  end
end

fun checking-cases-no-else(l :: A.Loc, ann :: A.Ann, new-val :: A.Expr, split-result :: Pair<List<A.CasesBranch>,List<Type>>, context :: Context) -> CheckingResult:
  new-cases = A.s-cases(l, ann, new-val, split-result.left)
  checking-result(new-cases, context)
end

fun handle-cases<B>(l :: A.Loc, ann :: A.Ann, val :: A.Expr, branches :: List<A.CasesBranch>,
                    maybe-else :: Option<A.Expr>, expect-loc :: A.Loc, maybe-expect :: Option<Type>,
                    context :: Context, bind-direction, create-err :: (List<C.CompileError> -> B),
                    has-else, no-else) -> B:

  for bind-direction(maybe-typ from to-type(ann, context)):
    cases(Option<Type>) maybe-typ:
      | some(typ) =>
        cases(Option<Type>) context.get-data-type(typ):
          | some(data-type) =>
            bind-direction(lam(new-val, new-context):
              branch-tracker = track-branches(data-type)
              for bind-direction(result from map-result(handle-branch(data-type, l, _, expect-loc, maybe-expect, branch-tracker.remove, context), branches)):
                split-result = split(result)
                remaining-branches = branch-tracker.get().to-list()
                cases(Option<A.Expr>) maybe-else:
                  | some(_else) =>
                    if is-empty(remaining-branches):
                      create-err([list: C.unneccesary-else-branch(data-type.name, l)])
                    else:
                      has-else(l, ann, new-val, split-result, _else, context)
                    end
                  | none =>
                    if is-empty(remaining-branches):
                      no-else(l, ann, new-val, split-result, context)
                    else:
                      create-err([list: C.non-exhaustive-pattern(remaining-branches, data-type.name, l)])
                    end
                end
              end
            end, checking(val, l, typ, context))
          | none =>
            create-err([list: C.cant-match-on(tostring(typ), l)])
        end
      | none => raise("determine what to do here")
    end
  end
end

fun track-branches(data-type :: Type % (is-t-data)) ->
  { remove :: (String -> Set<String>), get :: (-> Set<String>) }:
  var unhandled-branches = data-type.variants.foldl(lam(b, s): s.add(b.name) end, [set:])
  {
    remove: lam(b-name :: String):
      unhandled-branches := unhandled-branches.remove(b-name)
    end,
    get: lam() -> Set<String>:
      unhandled-branches
    end
  }
end

# TODO(MATT): add refinement here?
fun handle-branch(data-type :: Type % (is-t-data), cases-loc :: A.Loc, branch :: A.CasesBranch,
                  expect-loc :: A.Loc, maybe-check :: Option<Type>,
                  remove :: (String -> Any), context :: Context
) -> FoldResult<Pair<A.CasesBranch, Type>>:
  fun handle-body(name :: String, body :: A.Expr, process, body-context :: Context):
    remove(name)
    cases(Option<Type>) maybe-check:
      | some(expect-typ) =>
        checking(body, expect-loc, expect-typ, body-context).fold-bind(process(_, expect-loc, expect-typ, _))
      | none =>
        synthesis(body, body-context).fold-bind(process)
    end
  end

  cases(Option<TypeVariant>) data-type.lookup-variant(branch.name):
    | some(tv) =>
      cases(TypeVariant) tv:
        | t-variant(_, fields, _) =>
          cases(A.CasesBranch) branch:
            | s-cases-branch(l, pat-loc, name, args, body) =>
              fun process(new-body, _, typ, out-context):
                new-branch = A.s-cases-branch(l, pat-loc, name, args, new-body)
                fold-result(pair(new-branch, typ))
              end
              body-context = fold2-strict(lam(ctxt, arg, arg-typ):
                ctxt.add-term-var(arg.bind.id.key(), arg-typ)
              end, context, args, fields.map(_.typ))

              cases(Option<Context>) body-context:
                | none => fold-errors([list: C.incorrect-number-of-bindings(name, l, args.length(), fields.length)])
                | some(shadow body-context) => handle-body(name, body, process, body-context)
              end
            | s-singleton-cases-branch(l, _, name, _) =>
              fold-errors([list: C.cases-singleton-mismatch(name, l, false)])
          end
        | t-singleton-variant(_, _) =>
          raise("have not reached this yet")
      end
    | none =>
      fold-errors([list: C.unneccesary-branch(branch.name, branch.l, data-type.name, cases-loc)])
  end
end

fun meet-branch-typs(branch-typs :: List<Type>, context :: Context) -> Type:
  branch-typs.foldl(least-upper-bound(_, _, context), t-bot)
end

# TODO(MATT): this
fun least-upper-bound(s :: Type, t :: Type, context :: Context) -> Type:
  cases(FoldResult<Context>) satisfies-type(s, t, context):
    | fold-result(ctxt) => t
    | fold-errors(_) =>
      cases(FoldResult<Context>) satisfies-type(t, s, context):
        | fold-result(ctxt) => s
        | fold-errors(_) => raise("lub not implemented for non subtypes yet")
      end
  end
end
