provide *
provide-types *
import ast as A
import valueskeleton as VS
import string-dict as SD
import srcloc as SL
import either as E
import "compiler/ast-util.arr" as AU
import "compiler/type-structs.arr" as TS
import "compiler/my-type-check-structs.arr" as TCS
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
t-existential             = TS.t-existential

is-t-record               = TS.is-t-record
is-t-name                 = TS.is-t-name

type TypeVariable         = TS.TypeVariable
t-variable                = TS.t-variable

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
      t-variable(A.dummy-loc, A.s-atom("B", 1), t-top, invariant),
      t-variable(A.dummy-loc, A.s-atom("C", 1), t-top, invariant)
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
        | checking-result(new-body, _) =>
          each(print, new-body.tosource().pretty(72))
          C.ok(TCS.typed(A.s-program(l, _provide, provided-types, imports, new-body), info))
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
      raise("checking for s-type-let-expr not implemented")
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
      raise("checking for s-cases not implemented")
    | s-cases-else(l, typ, val, branches, _else) =>
      raise("checking for s-cases-else not implemented")
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
      raise("synthesis for s-module not implemented")
    | s-type-let-expr(l, binds, body) =>
      raise("synthesis for s-type-let-expr not implemented")
    | s-let-expr(l, binds, body) =>
      raise("synthesis for s-let-expr not implemented")
    | s-letrec(l, binds, body) =>
      raise("synthesis for s-letrec not implemented")
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
      raise("synthesis for s-cases not implemented")
    | s-cases-else(l, typ, val, branches, _else) =>
      raise("synthesis for s-cases-else not implemented")
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
      raise("synthesis for s-dot not implemented")
    | s-get-bang(l, obj, field) =>
      raise("synthesis for s-get-bang not implemented")
    | s-bracket(l, obj, field) =>
      raise("synthesis for s-bracket not implemented")
    | s-data(l, name, params, mixins, variants, shared-members, _check) =>
      raise("synthesis for s-data not implemented")
    | s-data-expr(l, name, namet, params, mixins, variants, shared-members, _check) =>
      raise("synthesis for s-data-expr not implemented")
    | s-for(l, iterator, bindings, ann, body) =>
      raise("synthesis for s-for not implemented")
    | s-check(l, name, body, keyword-check) =>
      raise("synthesis for s-check not implemented")
  end
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
      raise("synthesis-spine for t-forall not implemented yet")
    | t-bot =>
      raise("synthesis-spine for t-bot not implemented yet")
    | t-existential(id) =>
      raise("synthesis-spine for t-existential not implemented yet")
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
          raise("satisfies-type for t-bot  not implemented")
        | t-record(fields) =>
          raise("satisfies-type for t-record not implemented")
        | t-ref(a-typ) =>
          raise("satisfies-type for t-ref not implemented")
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
