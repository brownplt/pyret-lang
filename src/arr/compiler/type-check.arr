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

type Loc = A.Loc
type Expr = A.Expr

type Context = TCS.Context
type TCInfo = TCS.TCInfo

type TypingResult = TCS.TypingResult
typing-result = TCS.typing-result
typing-error = TCS.typing-error

type FoldResult = TCS.FoldResult
fold-result = TCS.fold-result
fold-errors = TCS.fold-errors

type Pair = TS.Pair
pair = TS.pair

type ModuleType = TS.ModuleType
t-module = TS.t-module

type TypeMember = TS.TypeMember
t-member = TS.t-member

type TypeVariant = TS.TypeVariant
t-variant = TS.t-variant
t-singleton-variant = TS.t-singleton-variant

type Type = TS.Type
t-name = TS.t-name
t-var = TS.t-var
t-arrow = TS.t-arrow
t-app = TS.t-app
t-top = TS.t-top
t-bot = TS.t-bot
t-record = TS.t-record
t-forall = TS.t-forall
t-ref = TS.t-ref
t-existential = TS.t-existential
t-data = TS.t-data
t-data-refinement = TS.t-data-refinement

t-array-name = TS.t-array-name
t-number = TS.t-number
t-string = TS.t-string
t-boolean = TS.t-boolean
t-nothing = TS.t-nothing
t-srcloc = TS.t-srcloc
t-array = TS.t-array
t-option = TS.t-option

type-members-lookup = TS.type-members-lookup
is-t-existential = TS.is-t-existential
is-t-record = TS.is-t-record
is-t-data = TS.is-t-data

bind = TCS.bind
typing-bind = TCS.typing-bind
fold-bind = TCS.fold-bind
map-fold-result = TCS.map-fold-result
foldr-fold-result = TCS.foldr-fold-result
fold-typing = TCS.fold-typing
resolve-alias = TCS.resolve-alias

t-num-binop = lam(loc):
  t-arrow([list: t-number(loc), t-number(loc)], t-number(loc), loc)
end
t-num-cmp = lam(loc):
  t-arrow([list: t-number(loc), t-number(loc)], t-boolean(loc), loc)
end
t-str-binop = lam(loc):
  t-arrow([list: t-string(loc), t-string(loc)], t-string(loc), loc)
end
t-str-cmp = lam(loc):
  t-arrow([list: t-string(loc), t-string(loc)], t-boolean(loc), loc)
end
t-method-binop = lam(field-name :: String, loc):
  t-forall(
    [list:
      t-var(A.s-atom("B", 1), loc),
      t-var(A.s-atom("C", 1), loc)
    ],
    t-arrow(
      [list:
        t-record([list:
          t-member(field-name, t-arrow([list: t-var(A.s-atom("B", 1), loc)], t-var(A.s-atom("C", 1), loc), loc))
        ], loc),
        t-var(A.s-atom("B",1), loc)
      ],
      t-var(A.s-atom("C", 1), loc),
      loc
    ),
    loc
  )
end

# Math operators
t-plus-method   = lam(loc): t-method-binop("_plus", loc) end
t-minus-method  = lam(loc): t-method-binop("_minus", loc) end
t-divide-method = lam(loc): t-method-binop("_divide", loc) end
t-times-method  = lam(loc): t-method-binop("_times", loc) end

# Comparison operators
t-lt-method     = lam(loc): t-method-binop("_lessthan", loc) end
t-lte-method    = lam(loc): t-method-binop("_lessequal", loc) end
t-gt-method     = lam(loc): t-method-binop("_greaterthan", loc) end
t-gte-method    = lam(loc): t-method-binop("_greaterequal", loc) end

fun provides-as-dict(provides):
  for fold(d from SD.make-string-dict(), p from provides.fields):
    d.set(p.field-name, p.typ)
  end
end

fun option-bind<X, Y>(f :: (X -> Option<Y>), maybe-thing :: Option<X>) -> Option<Y>:
  cases(Option<X>) maybe-thing:
    | none => none
    | some(thing) => f(thing)
  end
end

fun split<X, Y>(ps :: List<Pair<X,Y>>) -> Pair<List<X>, List<Y>>:
  fun step(p, curr):
    pair(link(p.left, curr.left), link(p.right, curr.right))
  end
  ps.foldr(step, pair(empty, empty))
end

fun new-existential(l :: Loc):
  t-existential(A.global-names.make-atom("exists"), l)
end

fun import-to-string(i :: A.ImportType, c :: C.CompileEnvironment) -> String:
  c.mods.get-value(AU.import-to-dep(i).key()).from-uri
end

map2-strict = LA.map2-strict

fun fold2-strict<X, Y, R>(f :: (R, X, Y -> R), base :: R, l1 :: List<X>, l2 :: List<Y>) -> Option<R>:
  cases (List<X>) l1:
    | empty => cases (List<Y>) l2:
        | empty => some(base)
        | link(_, _) => none
      end
    | link(a, ar) => cases (List<Y>) l2:
        | empty => none
        | link(b, br) => cases(Option<R>) fold2-strict(f, base, ar, br):
            | none => none
            | some(r) => some(f(r, a, b))
          end
      end
  end
end

# I believe modules is always of type SD.MutableStringDict<Loadable> -Matt
fun type-check(program :: A.Program, compile-env :: C.CompileEnvironment, modules) -> C.CompileResult<A.Program>:
  context = TCS.empty-context()
  globvs = compile-env.globals.values
  globts = compile-env.globals.types
  for each(g from globvs.keys-list()):
    context.types.set-now(A.s-global(g).key(), globvs.get-value(g))
  end
  #for each(g from globts.keys-list()):
  #  context.aliases.set-now(A.s-type-global(g).key(), globts.get-value(g))
  #  # TODO(MATT): decide how to handle globals.types for the repl
  #  context.data-types.set-now(A.s-type-global(g).key(), globts.get-value(g))
  #end
  for each(k from modules.keys-list-now()):
    when not(context.modules.has-key-now(k)):
      mod = modules.get-value-now(k).provides
      key = mod.from-uri
      val-provides = t-record(
        for map(v from mod.values.keys-list()): t-member(v, mod.values.get-value(v)) end,
        program.l)
      module-type = t-module(
        key,
        val-provides,
        mod.data-definitions,
        mod.aliases)
      context.modules.set-now(key, module-type)
    end
  end
  cases(A.Program) program:
    | s-program(l, _provide, provided-types, imports, body) =>
      for each(_import from imports):
        cases(A.Import) _import:
          | s-import(_, file, name) =>
            raise("s-import not implemented")
          | s-import-complete(_, vals, types, file, vname, tname) =>
            key = import-to-string(file, compile-env)
            context.module-names.set-now(tname.key(), key)
            when not(context.modules.has-key-now(key)):
              mod = compile-env.mods.get-value(AU.import-to-dep(file).key())
              val-provides = t-record(
                for map(v from mod.values.keys-list()): t-member(v, mod.values.get-value(v)) end,
                l)
              module-type = t-module(
                key,
                val-provides,
                mod.data-definitions,
                mod.aliases)
              context.modules.set-now(key, module-type)
            end
            thismod = context.modules.get-value-now(key)
            context.types.set-now(vname.key(), thismod.provides)
            # TODO(MATT): decide if this is necessary
            context.aliases.set-now(tname.key(), t-top(l))
            # TODO(MATT): this might be where a circle is created with aliases
            #             determine what to do
            for each(a from types):
              context.aliases.set-now(a.key(), thismod.aliases.get-value(a.toname()))
              context.data-types.set-now(a.key(), thismod.types.get-value(a.toname()))
            end
            mod = compile-env.mods.get-value(AU.import-to-dep(file).key())
            for each(v from vals):
              cases(Option) mod.values.get(v.toname()):
                | none =>
                  # TODO(MATT): see if there is more efficient method
                  cases(Option) provides-as-dict(thismod.provides).get(v.toname()):
                    | none => nothing # skip misses for now
                    | some(typ) => context.types.set-now(v.key(), typ)
                  end
                | some(typ) => context.types.set-now(v.key(), typ)
              end
            end
          | else => raise("typechecker received incomplete import")
        end
      end
      #each(print, body.tosource().pretty(72))

      tc-result = checking(body, t-top(l), true, context)
      cases(TypingResult) tc-result:
        | typing-result(new-body, _, shadow context) =>
          #each(print, new-body.tosource().pretty(72))
          folded-info = gather-provides(_provide, context)
          cases(FoldResult<TCInfo>) folded-info:
            | fold-result(info, _) =>
              C.ok(TCS.typed(A.s-program(l, _provide, provided-types, imports, new-body), info))
            | fold-errors(errs) =>
              C.err(errs)
          end
        | typing-error(error-list) =>
          C.err(error-list)
      end
    | else => raise("Attempt to type-check non-program: " + torepr(program))
  end
end

fun checking(e, expect-typ, top-level, context):
  result = _checking(e, expect-typ, top-level, context)
  #print("")
  #print("checking:")
  #each(print, e.tosource().pretty(72))
  #print("has type: " + tostring(expect-typ))
  #print("result:")
  #print(result)
  result
end

fun _checking(e :: Expr, expect-type :: Type, top-level :: Boolean, context :: Context) -> TypingResult:
  cases(Type) expect-type:
    | t-existential(_, _) =>
      check-synthesis(e, expect-type, top-level, context)
    | else =>
      cases(Expr) e:
        | s-module(l, answer, defined-values, defined-types, provided-values, provided-types, checks) =>
          checking(answer, expect-type, false, context)
            .bind(lam(new-answer, _, shadow context):
              shadow context = cases(Expr) provided-values:
                | s-obj(_, fields) =>
                  map-fold-result(lam(field, shadow context):
                    cases(A.Member) field:
                      | s-data-field(_, name, value) =>
                        info = context.info
                        synthesis(value, false, context).fold-bind(lam(_, field-type, _):
                          shadow context = context.set-info(
                            TCS.tc-info(info.types.set(name, field-type),
                                        info.aliases,
                                        info.data-types))
                          fold-result(nothing, context)
                        end)
                    end
                  end, fields, context)
                | else => raise("provided-values isn't always an object")
              end
              context.typing-bind(lam(_, shadow context):
                shadow context = map-fold-result(lam(a-field, shadow context):
                  fun add-aliases(typ :: Type, shadow context :: Context) -> Context:
                    cases(Option<Type>) context.aliases.get-now(typ.key()):
                      | some(alias-typ) =>
                        info = context.info
                        shadow context = context.set-info(TCS.tc-info(info.types, info.aliases.set(typ.key(), alias-typ), info.data-types))
                        add-aliases(alias-typ, context)
                      | none => context
                    end
                  end

                  to-type(a-field.ann, context).bind(lam(maybe-type, _):
                    cases(Option<Type>) maybe-type:
                      | some(typ) =>
                        new-data-types = cases(Option<Type>) context.get-data-type(typ):
                          | some(data-type) =>
                            context.info.data-types.set(a-field.name, data-type)
                          | none =>
                            context.info.data-types
                        end
                        shadow context = add-aliases(typ, context)
                        info = context.info
                        fold-result(nothing, context.set-info(TCS.tc-info(info.types, info.aliases, new-data-types)))
                      | none => raise("providing a type that doesn't exist")
                    end
                  end)
                end, provided-types, context)

                context.typing-bind(lam(_, shadow context):
                  typing-result(A.s-module(l, new-answer, defined-values, defined-types, provided-values, provided-types, checks), expect-type, context)
                end)
              end)
            end)
        | s-type-let-expr(l, binds, body) =>
          handle-type-let-binds(binds, context).typing-bind(lam(_, shadow context):
            checking(body, expect-type, true, context)
              .map-expr(A.s-type-let-expr(l, binds, _))
          end)
        | s-let-expr(l, binds, body) =>
          fold-typing(synthesis-let-bind, binds, context).typing-bind(lam(rhs-result, shadow context):
            new-binds = map2(lam(binding, rhs):
              cases(A.LetBind) binding:
                | s-let-bind(let-l, let-b, _) =>
                  A.s-let-bind(let-l, let-b, rhs)
                | s-var-bind(let-l, let-b, _) =>
                  A.s-var-bind(let-l, let-b, rhs)
              end
            end, binds, rhs-result)
            checking(body, expect-type, top-level, context)
              .map-expr(A.s-let-expr(l, new-binds, _))
          end)
        | s-letrec(l, binds, body) =>
          fold-collected = collect-letrec-bindings(binds, top-level, context)
          fold-collected.typing-bind(lam(collected, _):
            shadow context = context.add-dict-to-bindings(collected)
            fold-rhs = fold-typing(lam(binding, shadow context):
              cases(A.LetrecBind) binding:
                | s-letrec-bind(l2, b, value) =>
                  checking(value, collected.get-value(b.id.key()), false, context)
              end
            end, binds, context)

            fold-rhs.typing-bind(lam(new-rhs, shadow context):
              new-binds = map2(lam(binding, rhs):
                cases(A.LetrecBind) binding:
                  | s-letrec-bind(l2, b, _) =>
                    A.s-letrec-bind(l2, b, rhs)
                end
              end, binds, new-rhs)
              checking(body, expect-type, top-level, context)
                .map-expr(A.s-letrec(l, new-binds, _))
            end)
          end)
        | s-hint-exp(l, hints, exp) =>
          raise("checking for s-hint-exp not implemented")
        | s-instantiate(l, expr, params) =>
          check-synthesis(e, expect-type, top-level, context)
        | s-block(l, stmts) =>
          fun gen(curr, base):
            pair(link(pair(curr, base.right), base.left), t-top(l))
          end
          paired-stmts = stmts.foldr(gen, pair(empty, expect-type)).left
          result = fold-typing(lam(stmt-type-pair, shadow context):
            checking(stmt-type-pair.left, stmt-type-pair.right, top-level, context)
          end, paired-stmts, context)
          result.typing-bind(lam(new-stmts, shadow context):
            typing-result(A.s-block(l, new-stmts), context.apply(expect-type), context)
          end)
        | s-user-block(l, body) =>
          raise("s-user-block should have already been desugared")
        | s-fun(l, name, params, args, ann, doc, body, _check) =>
          raise("s-fun should have already been desugared")
        | s-type(l, name, params, ann) =>
          raise("checking for s-type not implemented")
        | s-newtype(l, name, namet) =>
          raise("checking for s-newtype not implemented")
        | s-var(l, name, value) =>
          raise("s-var should have already been desugared")
        | s-rec(l, name, value) =>
          raise("checking for s-rec not implemented")
        | s-let(l, name, value, keyword-val) =>
          raise("s-let should have already been desugared")
        | s-ref(l, ann) =>
          raise("checking for s-ref not implemented")
        | s-contract(l, name, ann) =>
          raise("checking for s-contract not implemented")
        | s-when(l, test, block) =>
          raise("s-when should have already been desugared")
        | s-assign(l, id, value) =>
          lookup-id(l, id.key(), e, context).typing-bind(lam(id-type, _):
            cases(Type) id-type:
              | t-ref(arg-type, _) =>
                checking(value, arg-type, top-level, context)
              | else =>
                typing-error([list: C.incorrect-type(tostring(id-type), l, tostring(t-ref(id-type, l)), l)])
            end
          end)
        | s-if-pipe(l, branches) =>
          raise("s-if-pipe should have already been desugared")
        | s-if-pipe-else(l, branches, _else) =>
          raise("s-if-pipe-else should have already been desugared")
        | s-if(l, branches) =>
          raise("s-if should have already been desugared")
        | s-if-else(l, branches, _else) =>
          map-fold-result(lam(branch, shadow context):
            bool-result = checking(branch.test, t-boolean(branch.l), false, context)
            bool-result.fold-bind(lam(new-test, _, shadow context):
              body-result = checking(branch.body, expect-type, false, context)
              body-result.fold-bind(lam(new-body, _, shadow context):
                fold-result(A.s-if-branch(branch.l, new-test, new-body), context)
              end)
            end)
          end, branches, context).typing-bind(lam(new-branches, shadow context):
            checking(_else, expect-type, false, context)
              .map-expr(A.s-if-else(l, new-branches, _))
          end)
        | s-cases(l, typ, val, branches) =>
          checking-cases(l, typ, val, branches, none, expect-type, context)
        | s-cases-else(l, typ, val, branches, _else) =>
          checking-cases(l, typ, val, branches, some(_else), expect-type, context)
        | s-op(loc, op, l, r) =>
          raise("checking for s-op not implemented")
        | s-check-test(loc, op, refinement, l, r) =>
          typing-result(e, expect-type, context)
          raise("checking for s-check-test not implemented")
        | s-check-expr(l, expr, ann) =>
          raise("checking for s-check-expr not implemented")
        | s-paren(l, expr) =>
          raise("s-paren should have already been desugared")
        | s-lam(l, params, args, ann, doc, body, _check) =>
          check-fun(l, body, params, args, ann, expect-type, A.s-lam(l, params, _, _, doc, _, _check), context)
        | s-method(l, params, args, ann, doc, body, _check) =>
          raise("checking for s-method not implemented")
        | s-extend(l, supe, fields) =>
          raise("checking for s-extend not implemented")
        | s-update(l, obj, fields) =>
          check-synthesis(e, expect-type, top-level, context)
        | s-obj(l, fields) =>
          cases(Type) expect-type:
            | t-record(t-fields, t-l) =>
              map-fold-result(collect-member(_, true, _), fields, context).typing-bind(lam(field-types, shadow context):
                temp-obj-type = t-record(field-types, l)
                cases(Option<Context>) satisfies-type(temp-obj-type, expect-type, context):
                  | none =>
                    typing-error([list: C.incorrect-type(tostring(temp-obj-type), temp-obj-type.l, tostring(expect-type), expect-type.l)])
                  | some(shadow context) =>
                    new-obj-type = context.apply(temp-obj-type)
                    fields-and-types = map2(lam(field, field-type):
                      pair(field, field-type)
                    end, fields, new-obj-type.fields)
                    fold-new-field-types = map-fold-result(lam(field-and-type, shadow context):
                      to-type-member(field-and-type.left, field-and-type.right, new-obj-type, true, context)
                    end, fields-and-types, context)

                    fold-new-field-types.typing-bind(lam(_, shadow context):
                      typing-result(A.s-obj(l, fields), expect-type, context)
                    end)
                end
              end)
            | t-top(t-l) =>
              check-synthesis(e, expect-type, top-level, context)
            | else =>
              typing-error([list: C.incorrect-type(tostring(expect-type), expect-type.l, "an object type", l)])
          end
        | s-array(l, values) =>
          wrapped = cases(Type) expect-type:
            | t-app(rarray, args, tl) =>
              if TS.t-array-name == rarray:
                param-type = args.first
                for fold-typing(value from values, shadow context from context):
                  checking(value, param-type, false, context)
                end
              else:
                fold-errors([list: C.incorrect-type(tostring(TS.t-array-name), l, tostring(expect-type), expect-type.l)])
              end
            | t-top(tl) =>
              for fold-typing(value from values, shadow context from context):
                checking(value, t-top(tl), false, context)
              end
            | else =>
              fold-errors([list: C.incorrect-type("a raw array", l, tostring(expect-type), expect-type.l)])
          end

          wrapped.typing-bind(lam(new-values, shadow context):
            typing-result(A.s-array(l, new-values), expect-type, context)
          end)
        | s-construct(l, modifier, constructor, values) =>
          raise("checking for s-construct not implemented")
        | s-app(l, _fun, args) =>
          check-synthesis(e, expect-type, top-level, context)
        | s-prim-app(l, _fun, args) =>
          check-synthesis(e, expect-type, top-level, context)
        | s-prim-val(l, name) =>
          raise("checking for s-prim-val not implemented")
        | s-id(l, id) =>
          check-synthesis(e, expect-type, top-level, context)
        | s-id-var(l, id) =>
          check-synthesis(e, expect-type, top-level, context)
        | s-id-letrec(l, id, safe) =>
          check-synthesis(e, expect-type, top-level, context)
        | s-undefined(l) =>
          raise("checking for s-undefined not implemented")
        | s-srcloc(l, loc) =>
          check-synthesis(e, expect-type, top-level, context)
        | s-num(l, n) =>
          check-synthesis(e, expect-type, top-level, context)
        | s-frac(l, num, den) =>
          check-synthesis(e, expect-type, top-level, context)
        | s-bool(l, b) =>
          check-synthesis(e, expect-type, top-level, context)
        | s-str(l, s) =>
          check-synthesis(e, expect-type, top-level, context)
        | s-dot(l, obj, field) =>
          check-synthesis(e, expect-type, top-level, context)
        | s-get-bang(l, obj, field) =>
          check-synthesis(e, expect-type, top-level, context)
        | s-bracket(l, obj, field) =>
          raise("checking for s-bracket not implemented")
        | s-data(l, name, params, mixins, variants, shared-members, _check) =>
          raise("s-data should have already been desugared")
        | s-data-expr(l, name, namet, params, mixins, variants, shared-members, _check) =>
          check-synthesis(e, expect-type, top-level, context)
        | s-for(l, iterator, bindings, ann, body) =>
          raise("s-for should have already been desugared")
        | s-check(l, name, body, keyword-check) =>
          typing-result(e, expect-type, context)
      end
  end
end

fun synthesis(e, top-level, context):
  result = _synthesis(e, top-level, context)
  #print("")
  #print("synthesis on:")
  #each(print, e.tosource().pretty(72))
  #print("result:")
  #print(result)
  result
end

fun _synthesis(e :: Expr, top-level :: Boolean, context :: Context) -> TypingResult:
  cases(Expr) e:
    | s-module(l, answer, defined-values, defined-types, provided-values, provided-types, checks) =>
      synthesis(answer, false, context)
        .map-expr(A.s-module(l, _, defined-values, defined-types, provided-values, provided-types, checks))
        .map-type(_.set-loc(l))
    | s-type-let-expr(l, binds, body) =>
      handle-type-let-binds(binds, context).typing-bind(lam(_, shadow context):
        synthesis(body, false, context)
          .map-expr(A.s-type-let-expr(l, binds, _))
          .map-type(_.set-loc(l))
      end)
    | s-let-expr(l, binds, body) =>
      binds-result = fold-typing(synthesis-let-bind, binds, context)
      binds-result.typing-bind(lam(new-rhs, shadow context):
        new-binds = map2(lam(binding, rhs):
          cases(A.LetBind) binding:
            | s-let-bind(let-l, let-b, _) =>
              A.s-let-bind(let-l, let-b, rhs)
            | s-var-bind(let-l, let-b, _) =>
              A.s-var-bind(let-l, let-b, rhs)
          end
        end, binds, new-rhs)
        synthesis(body, false, context)
          .map-expr(A.s-let-expr(l, new-binds, _))
          .map-type(_.set-loc(l))
      end)
    | s-letrec(l, binds, body) =>
      fold-collected = collect-letrec-bindings(binds, top-level, context)

      fold-collected.typing-bind(lam(collected, _):
        shadow context = context.add-dict-to-bindings(collected)
        fold-rhs = fold-typing(lam(binding, shadow context):
          cases(A.LetrecBind) binding:
            | s-letrec-bind(l2, b, value) =>
              checking(value, collected.get-value(b.id.key()), false, context)
          end
        end, binds, context)

        fold-rhs.typing-bind(lam(new-rhs, shadow context):
          new-binds = map2(lam(binding, rhs):
            cases(A.LetrecBind) binding:
              | s-letrec-bind(l2, b, _) =>
                A.s-letrec-bind(l2, b, rhs)
            end
          end, binds, new-rhs)
          synthesis(body, top-level, context)
            .map-expr(A.s-letrec(l, new-binds, _))
            .map-type(_.set-loc(l))
        end)
      end)
    | s-hint-exp(l, hints, exp) =>
      raise("synthesis for s-hint-exp not implemented")
    | s-instantiate(l, expr, params) =>
      synthesis-instantiation(l, expr, params, top-level, context)
    | s-block(l, stmts) =>
      var typ = t-top(l)
      fold-typing(lam(stmt, shadow context):
        synthesis(stmt, top-level, context).bind(
          lam(stmt-expr, stmt-typ, shadow context):
            typ := stmt-typ
            typing-result(stmt-expr, stmt-typ, context)
          end)
      end, stmts, context).typing-bind(lam(new-stmts, shadow context):
        typing-result(A.s-block(l, new-stmts), typ.set-loc(l), context)
      end)
    | s-user-block(l, body) =>
      raise("s-user-block should have already been desugared")
    | s-fun(l, name, params, args, ann, doc, body, _check) =>
      raise("s-fun should have already been desugared")
    | s-type(l, name, params, ann) =>
      raise("synthesis for s-type not implemented")
    | s-newtype(l, name, namet) =>
      raise("synthesis for s-newtype not implemented")
    | s-var(l, name, value) =>
      raise("s-var should have already been desugared")
    | s-rec(l, name, value) =>
      raise("synthesis for s-rec not implemented")
    | s-let(l, name, value, keyword-val) =>
      raise("s-let should have already been desugared")
    | s-ref(l, ann) =>
      raise("synthesis for s-ref not implemented")
    | s-contract(l, name, ann) =>
      raise("synthesis for s-contract not implemented")
    | s-when(l, test, block) =>
      raise("s-when should have already been desugared")
    | s-assign(l, id, value) =>
      lookup-id(l, id.key(), e, context).typing-bind(lam(id-type, _):
        cases(Type) id-type:
          | t-ref(arg-type, tl) =>
            checking(value, arg-type, top-level, context).bind(lam(new-value, _, shadow context):
              typing-result(A.s-assign(l, id, new-value), arg-type.set-loc(l), context)
            end)
          | else =>
            typing-error([list: C.incorrect-type(tostring(id-type), l, tostring(t-ref(id-type, l)), l)])
        end
      end)
    | s-if-pipe(l, branches) =>
      raise("s-if-pipe should have already been desugared")
    | s-if-pipe-else(l, branches, _else) =>
      raise("s-if-pipe-else should have already been desugared")
    | s-if(l, branches) =>
      raise("s-if should have already been desugared")
    | s-if-else(l, branches, _else) =>
      map-fold-result(handle-if-branch(_, _), branches, context).typing-bind(lam(result, shadow context):
        synthesis(_else, false, context).bind(
          lam(new-else, else-type, shadow context):
            split-result = split(result)
            new-branches = split-result.left
            new-if-else = A.s-if-else(l, new-branches, new-else)

            meet-branch-types(link(else-type, split-result.right), l, context).typing-bind(lam(if-else-type, shadow context):
              typing-result(new-if-else, if-else-type.set-loc(l), context)
            end)
          end)
      end)
    | s-cases(l, typ, val, branches) =>
      synthesis-cases(l, typ, val, branches, none, context)
    | s-cases-else(l, typ, val, branches, _else) =>
      synthesis-cases(l, typ, val, branches, some(_else), context)
    | s-op(loc, op, l, r) =>
      raise("synthesis for s-op not implemented")
    | s-check-test(loc, op, refinement, l, r) =>
      typing-result(e, t-top(loc), context)
    | s-check-expr(l, expr, ann) =>
      raise("synthesis for s-check-expr not implemented")
    | s-paren(l, expr) =>
      raise("s-paren should have already been desugared")
    | s-lam(l, params, args, ann, doc, body, _check) =>
      synthesis-fun(l, body, params, args, ann, A.s-lam(l, params, _, _, doc, _, _check), top-level, context)
    | s-method(l, params, args, ann, doc, body, _check) =>
      raise("synthesis for s-method not implemented")
    | s-extend(l, supe, fields) =>
      raise("synthesis for s-extend not implemented")
    | s-update(l, obj, fields) =>
      synthesis(obj, top-level, context).bind(synthesis-update(l, _, _, fields, _))
    | s-obj(l, fields) =>
      map-fold-result(collect-member(_, false, _), fields, context).typing-bind(lam(field-types, shadow context):
        initial-obj-type = t-record(field-types, l)
        fields-and-types = map2(lam(field, field-type):
          pair(field, field-type)
        end, fields, field-types)
        fold-new-field-types = map-fold-result(lam(field-and-type, shadow context):
          to-type-member(field-and-type.left, field-and-type.right, initial-obj-type, false, context)
        end, fields-and-types, context)

        fold-new-field-types.typing-bind(lam(new-field-types, shadow context):
          typing-result(A.s-obj(l, fields), t-record(new-field-types, l), context)
        end)
      end)
    | s-array(l, values) =>
      fun process(value :: A.Expr, ctxt :: Context) -> FoldResult<Pair<A.Expr, Type>>:
        synthesis(value, false, ctxt).fold-bind(lam(expr, typ, shadow context):
          fold-result(pair(expr, typ), context)
        end)
      end

      map-fold-result(process, values).typing-bind(lam(result, shadow context):
        split-result = split(result)
        new-values = split-result.left
        value-types = split-result.right
        fold-array-type = meet-branch-types(value-types, l, context)
        fold-array-type.typing-bind(lam(array-type, shadow context):
          new-array = A.s-array(l, new-values)
          typing-result(new-array, t-array(array-type.set-loc(l), l), context)
        end)
      end)
    | s-construct(l, modifier, constructor, values) =>
      raise("synthesis for s-construct not implemented")
    | s-app(l, _fun, args) =>
      synthesis-app-fun(l, _fun, args, context)
        .typing-bind(lam(result-obj, shadow context):
          synthesis-spine(context.apply(result-obj.fun-type), A.s-app(l, _fun, _), args, l, context)
            .map-type(_.set-loc(l))
        end)
    | s-prim-app(l, _fun, args) =>
      lookup-id(l, _fun, e, context).typing-bind(lam(arrow-type, _):
        synthesis-spine(arrow-type, A.s-prim-app(l, _fun, _), args, l, context)
          .map-type(_.set-loc(l))
      end)
    | s-prim-val(l, name) =>
      raise("synthesis for s-prim-val not implemented")
    | s-id(l, id) =>
      lookup-id(l, id.key(), e, context).typing-bind(lam(id-type, _):
        typing-result(e, id-type, context)
      end)
    | s-id-var(l, id) =>
      lookup-id(l, id.key(), e, context).typing-bind(lam(id-type, _):
        cases(Type) id-type:
          | t-ref(arg-type, _) =>
            typing-result(e, arg-type.set-loc(l), context)
          | else =>
            typing-error([list: C.incorrect-type(tostring(id-type), id-type.l, tostring(t-ref(id-type, l)), l)])
        end
      end)
    | s-id-letrec(l, id, safe) =>
      lookup-id(l, id.key(), e, context).typing-bind(lam(id-type, _):
        typing-result(e, id-type, context)
      end)
    | s-undefined(l) =>
      raise("synthesis for s-undefined not implemented")
    | s-srcloc(l, loc) =>
      typing-result(e, t-srcloc(l), context)
    | s-num(l, n) =>
      typing-result(e, t-number(l), context)
    | s-frac(l, num, den) =>
      typing-result(e, t-number(l), context)
    | s-bool(l, b) =>
      typing-result(e, t-boolean(l), context)
    | s-str(l, s) =>
      typing-result(e, t-string(l), context)
    | s-dot(l, obj, field) =>
      synthesis(obj, top-level, context).bind(lam(new-ast, new-type, shadow context):
        synthesis-field(l, new-ast, new-type.l, new-type, field, A.s-dot, context)
      end)
    | s-get-bang(l, obj, field) =>
      synthesis(obj, top-level, context).bind(lam(new-ast, new-type, shadow context):
        synthesis-field(l, new-ast, new-type.l, new-type, field, A.s-get-bang, context)
      end).bind(lam(new-get-bang, field-type, shadow context):
        cases(Type) field-type:
          | t-ref(typ, _) =>
            typing-result(new-get-bang, typ.set-loc(l), context)
          | else =>
            typing-error([list: C.incorrect-type(tostring(field-type), field-type.l, "a ref type", l)])
        end
      end)
    | s-bracket(l, obj, field) =>
      raise("synthesis for s-bracket not implemented")
    | s-data(l, name, params, mixins, variants, shared-members, _check) =>
      raise("s-data should have already been desugared")
    | s-data-expr(l, name, namet, params, mixins, variants, shared-members, _check) =>
      synthesis-datatype(l, name, namet, params, mixins, variants, shared-members, _check, context)
    | s-for(l, iterator, bindings, ann, body) =>
      raise("s-for should have already been desugared")
    | s-check(l, name, body, keyword-check) =>
      typing-result(e, t-top(l), context)
  end.bind(lam(ast, new-type, shadow context):
    typing-result(ast, context.apply(new-type), context)
  end)
end

fun synthesis-spine(fun-type, recreate, args, app-loc, context):
  result = _synthesis-spine(fun-type, recreate, args, app-loc, context)
  #print("")
  #print("spine synthesis with type: " + tostring(fun-type))
  #print("args:")
  #each(lam(arg): each(print, arg.tosource().pretty(72)) end, args)
  #print("result:")
  #print(result)
  result
end

fun _synthesis-spine(fun-type :: Type, recreate :: (List<Expr> -> Expr), args :: List<Expr>, app-loc :: Loc, context :: Context) -> TypingResult:
  cases(Type) fun-type:
    | t-arrow(arg-types, ret-type, _) =>
      result = fold2-strict(lam(acc, arg, arg-type):
        acc.bind(lam(exprs, shadow context):
          checking(arg, arg-type, false, context)
            .fold-bind(lam(new-arg, _, shadow context):
              fold-result(link(new-arg, exprs), context)
            end)
        end)
      end, fold-result(empty, context), args, arg-types)

      cases(Option<FoldResult<List<Expr>>>) result:
        | none =>
          typing-error([list: C.incorrect-number-of-args(recreate(args), fun-type)])
        | some(folded) =>
          folded.typing-bind(lam(exprs, shadow context):
            typing-result(recreate(exprs), ret-type, context)
          end)
      end
    | t-forall(introduces, onto, l) =>
      new-type = introduces.foldr(lam(type-var, new-type):
        new-type.substitute(new-existential(l), type-var)
      end, onto)
      synthesis-spine(new-type, recreate, args, app-loc, context)
    | t-bot(l) =>
      result = fold-typing(lam(arg, shadow context):
        checking(arg, t-top(l), false, context)
      end, args, context)
      result.typing-bind(lam(new-args, shadow context):
        typing-result(recreate(new-args), t-bot(l), context)
      end)
    | t-existential(id, l) =>
      existential-args = args.map(lam(_): new-existential(l) end)
      existential-ret = new-existential(l)
      new-arrow = t-arrow(existential-args, existential-ret, l)
      shadow context = context.assign-existential(fun-type, new-arrow)

      result = fold2(lam(acc, arg, arg-type):
        acc.bind(lam(current-exprs, shadow context):
          checking(arg, arg-type, false, context)
            .fold-bind(lam(new-arg, _, shadow context):
              fold-result(link(new-arg, current-exprs), context)
            end)
        end)
      end, fold-result(empty, context), args, existential-args)

      result.typing-bind(lam(new-exprs, shadow context):
        typing-result(recreate(new-exprs), existential-ret, context)
      end)
    | else =>
      typing-error([list: C.apply-non-function(recreate(args), fun-type)])
  end.bind(lam(ast, typ, shadow context):
    typing-result(ast, context.apply(typ.set-loc(app-loc)), context)
  end)
end

# TODO(MATT): update context along the way
fun to-type(in-ann :: A.Ann, context :: Context) -> FoldResult<Option<Type>>:
  cases(A.Ann) in-ann:
    | a-blank =>
      fold-result(none, context)
    | a-any(l) =>
      fold-result(some(t-top(l)), context)
    | a-name(l, id) =>
      typ = cases(Option<Type>) context.aliases.get-now(id.key()):
        | some(typ) => some(typ)
        | none => context.data-types.get-now(id.key())
      end
      cases(Option<Type>) typ:
        | some(t) =>
          result-type = resolve-alias(t, context).set-loc(l)
          fold-result(some(result-type), context)
        | none =>
          fold-errors([list: C.unbound-type-id(in-ann)])
      end
    | a-type-var(l, id) =>
      fold-result(some(t-var(id, l)), context)
    | a-arrow(l, args, ret, _) =>
      fold-arg-typs = map-fold-result(lam(arg, _):
        to-type(arg, context).bind(lam(maybe-new-typ, _):
          cases(Option<Type>) maybe-new-typ:
            | none =>
              fold-errors([list: C.cant-typecheck("issue with arrow annotation", l)])
            | some(new-typ) =>
              fold-result(new-typ, context)
          end
        end)
      end, args, context)

      fold-arg-typs.bind(lam(arg-typs, _):
        to-type(ret, context).bind(lam(maybe-ret-typ, _):
          cases(Option<Type>) maybe-ret-typ:
            | none =>
              fold-errors([list: C.cant-typecheck("issue with arrow annotation", l)])
            | some(ret-typ) =>
              fold-result(some(t-arrow(arg-typs, ret-typ, l)), context)
          end
        end)
      end)
    | a-method(l, args, ret) =>
      fold-errors([list: C.cant-typecheck("a-method not yet implemented", l)])
    | a-record(l, fields) =>
      fields-result = map-fold-result(lam(field, _):
        to-type(field.ann, context).bind(lam(maybe-typ, _):
          cases(Option<Type>) maybe-typ:
            | none =>
              fold-errors([list: C.cant-typecheck("issue with record annotation", l)])
            | some(typ) =>
              fold-result(t-member(field.name, typ), context)
          end
        end)
      end, fields, context)

      fields-result.bind(lam(members, _):
        fold-result(some(t-record(members, l)), context)
      end)
    | a-app(l, ann, args) =>
      to-type(ann, context).bind(lam(maybe-typ, _):
        cases(Option<Type>) maybe-typ:
          | none =>
            fold-errors([list: C.cant-typecheck("issue with app annotation", l)])
          | some(typ) =>
            args-result = map-fold-result(lam(arg, _): to-type(arg, context) end, args, context)
            args-result.bind(lam(maybe-arg-typs, _):
              fold-arg-typs = map-fold-result(lam(maybe-arg-typ, _):
                cases(Option<Type>) maybe-arg-typ:
                  | none =>
                    fold-errors([list: C.cant-typecheck("issue with app annotation arguments", l)])
                  | some(arg-typ) =>
                    fold-result(arg-typ, context)
                end
              end, maybe-arg-typs, context)
              fold-arg-typs.bind(lam(arg-typs, _):
                fold-result(some(t-app(typ, arg-typs, l)), context)
              end)
            end)
        end
      end)
    | a-pred(l, ann, exp) =>
      to-type(ann, context).bind(lam(maybe-typ, _):
        cases(Option<Type>) maybe-typ:
          | some(typ) =>
            expect-type = t-arrow([list: typ], t-boolean(l), l)
            checking(exp, expect-type, false, context).fold-bind(lam(_, _, _):
              fold-result(some(typ), context)
            end)
          | none =>
            fold-errors([list: C.cant-typecheck("issue with pred annotation", l)])
        end
      end)
    | a-dot(l, obj, field) =>
      key = obj.key()
      cases(Option<String>) context.module-names.get-now(key):
        | none =>
          fold-errors([list: C.no-module(l, obj.toname())])
        | some(mod) =>
          t-mod = context.modules.get-value-now(mod)
          if t-mod.types.has-key(field):
            # TODO(MATT): s-global?
            fold-result(some(t-name(some(mod), A.s-type-global(field), l)), context)
          else if t-mod.aliases.has-key(field):
            # TODO(MATT): fully resolve alias?
            fold-result(some(t-mod.aliases.get-value(field)), context)
          else:
            fold-errors([list: C.unbound-type-id(in-ann)])
          end
      end
    | a-checked(checked, residual) =>
      fold-errors([list: C.cant-typecheck("a-checked should not be appearing before type checking", A.dummy-loc)])
  end
end

fun satisfies-type(subtype :: Type, supertype :: Type, context :: Context) -> Option<Context>:
  fun _satisfies-assuming(_subtype, _supertype, _context, assumptions :: Set<Pair<Type, Type>>):
    shadow subtype = _subtype
    shadow supertype = _supertype
    shadow context = _context
    if assumptions.member(pair(subtype, supertype)):
      some(context)
    else:
      shadow subtype = resolve-alias(subtype, context)
      shadow supertype = resolve-alias(supertype, context)
      cases(Type) supertype:
        | t-existential(b-id, _) =>
          cases(Type) subtype:
            | t-existential(a-id, _) =>
              if a-id == b-id:
                some(context)
              else:
                some(instantiate-left(subtype, supertype, context))
              end
            | else =>
              if subtype.free-variable(supertype):
                some(instantiate-right(subtype, supertype, context))
              else:
                none
              end
          end
        | t-forall(b-introduces, b-onto, _) =>
          satisfies-assuming(subtype, b-onto, context, assumptions)
        | else =>
          cases(Type) subtype:
            | t-name(a-mod, a-id, _) =>
              cases(Type) supertype:
                | t-top(_) => some(context)
                | t-name(b-mod, b-id, _) =>
                  if (a-mod == b-mod) and (a-id == b-id):
                    some(context)
                  else:
                    none
                  end
                | t-bot(_) => none
                | t-record(b-fields, _) =>
                  cases(Option<Type>) context.get-data-type(subtype):
                    | none => none
                    | some(data-type) =>
                      satisfies-assuming(data-type, supertype, context, assumptions)
                  end
                | t-data(b-name, b-variants, b-fields, _) =>
                  cases(Option<Type>) context.get-data-type(subtype):
                    | none => none
                    | some(data-type) =>
                      satisfies-assuming(data-type, subtype, context, assumptions)
                  end
                | else => none
              end
            | t-var(a-id, _) =>
              cases(Type) supertype:
                | t-top(_) => some(context)
                | t-var(b-id, _) =>
                  if a-id == b-id:
                    some(context)
                  else:
                    none
                  end
                | else => none
              end
            | t-arrow(a-args, a-ret, _) =>
              cases(Type) supertype:
                | t-top(_) => some(context)
                | t-arrow(b-args, b-ret, _) =>
                  result = fold2-strict(lam(maybe-context, b-arg, a-arg):
                    for option-bind(shadow context from maybe-context):
                      satisfies-assuming(context.apply(b-arg), context.apply(a-arg), context, assumptions)
                    end
                  end, some(context), b-args, a-args)
                  for option-bind(outer from result):
                    for option-bind(shadow context from outer):
                      satisfies-assuming(context.apply(a-ret), context.apply(b-ret), context, assumptions)
                    end
                  end
                | else => none
              end
            | t-forall(a-introduces, a-onto, a-l) =>
              new-onto = a-introduces.foldr(lam(type-var, new-onto):
                new-onto.substitute(new-existential(a-l), type-var)
              end, a-onto)
              satisfies-assuming(new-onto, supertype, context, assumptions)
            | t-app(a-onto, a-args, _) =>
              cases(Type) supertype:
                | t-top(_) => some(context)
                | t-forall(_, _, _) =>
                  satisfies-assuming(a-onto.introduce(a-args), supertype, context, assumptions)
                | else =>
                  for option-bind(data-type from context.get-data-type(a-onto)):
                    satisfies-assuming(data-type.introduce(a-args), supertype, context, assumptions)
                  end
              end
            | t-top(_) =>
              if TS.is-t-top(supertype):
                some(context)
              else:
                none
              end
            | t-bot(_) =>
              some(context)
            | t-record(a-fields, _) =>
              cases(Type) supertype:
                | t-top(_) => some(context)
                | t-record(b-fields, _) =>
                  satisfies-fields(a-fields, b-fields, context, assumptions)
                | else => none
              end
            | t-ref(a-typ, _) =>
              cases(Type) supertype:
                | t-top(_) => some(context)
                | t-ref(b-typ, _) =>
                  for option-bind(shadow context from satisfies-assuming(a-typ, b-typ, context, assumptions)):
                    satisfies-assuming(context.apply(b-typ), context.apply(a-typ), context, assumptions)
                  end
                | else => none
              end
            | t-existential(a-id, _) =>
              if supertype.free-variable(subtype):
                some(instantiate-left(subtype, supertype, context))
              else:
                none
              end
            | t-data(a-name, a-variants, a-fields, _) =>
              cases(Type) supertype:
                | t-data(b-name, b-variants, b-fields, _) =>
                  if a-name == b-name:
                    a-variants.foldr(lam(a-variant, maybe-context):
                      for option-bind(shadow context from maybe-context):
                        b-variant = supertype.lookup-variant(a-variant.name).value
                        cases(TypeVariant) a-variant:
                          | t-singleton-variant(_, _) =>
                            some(context)
                          | t-variant(_, a-var-fields, _) =>
                            b-var-fields = b-variant.fields
                            satisfies-fields(a-var-fields, b-var-fields, context, assumptions.add(pair(subtype, supertype)))
                        end
                      end
                    end, some(context))
                  else:
                    none
                  end
                | t-app(b-onto, b-args, _) =>
                  cases(Type) b-onto:
                    | t-forall(_, _, _) =>
                      satisfies-assuming(subtype, b-onto.introduce(b-args), context, assumptions)
                    | else =>
                      for option-bind(data-type from context.get-data-type(b-onto)):
                        satisfies-assuming(subtype, data-type.introduce(b-args), context, assumptions)
                      end
                  end
                | t-record(b-fields, _) =>
                  satisfies-fields(a-fields, b-fields, context, assumptions)
                | t-name(_, _, _) =>
                  cases(Option<Type>) context.get-data-type(supertype):
                    | none => none
                    | some(data-type) =>
                      satisfies-assuming(subtype, data-type, context, assumptions)
                  end
                | else => none
              end
            | t-data-refinement(a-data-type, _, _) =>
              # TODO(MATT): do more than just check the underlying data-type?
              satisfies-assuming(a-data-type, supertype, context, assumptions)
          end
      end
    end
  end

  fun satisfies-fields(a-fields :: List<TypeMember>, b-fields :: List<TypeMember>, _context :: Context, assumptions :: Set<Pair<Type, Type>>) -> Option<Context>:
    b-fields.foldr(lam(b-field, maybe-context):
      for option-bind(shadow context from maybe-context):
        for option-bind(a-field from a-fields.find(lam(a-field): a-field.field-name == b-field.field-name end)):
          satisfies-assuming(a-field.typ, b-field.typ, context, assumptions)
        end
      end
    end, some(_context))
  end

  fun satisfies-assuming(_subtype, _supertype, _context, assumptions):
    result = _satisfies-assuming(_subtype, _supertype, _context, assumptions)
    #print("")
    #print(tostring(_subtype) + " <: " + tostring(_supertype))
    #print("result:")
    #print(result)
    result
  end
  satisfies-assuming(subtype, supertype, context, [list-set: ])
end

fun instantiate-right(subtype, supertype, context):
  result = _instantiate-right(subtype, supertype, context)
  #print("")
  #print(tostring(subtype) + " <=: " + tostring(supertype))
  #print("result:")
  #print(result)
  result
end

fun _instantiate-right(subtype :: Type, supertype :: Type, context :: Context) -> Context:
  cases(Type) supertype:
    | t-existential(b-id, b-l) =>
      cases(Type) subtype:
        | t-name(a-mod, a-id, _) =>
          context.assign-existential(supertype, subtype)
        | t-var(a-id, _) =>
          context.assign-existential(supertype, subtype)
        | t-arrow(a-args, a-ret, a-l) =>
          args-and-existentials = a-args.map(lam(arg): pair(arg, new-existential(arg.l)) end)
          arg-existentials = split(args-and-existentials).right
          ret-existential = new-existential(a-ret.l)
          shadow context = context.assign-existential(supertype, t-arrow(arg-existentials, ret-existential, b-l))
          shadow context = args-and-existentials.foldr(lam(arg-and-exists, shadow context):
            instantiate-left(arg-and-exists.right, arg-and-exists.left, context)
          end, context)
          instantiate-right(context.apply(a-ret), ret-existential, context)
        | t-forall(a-introduces, a-onto, _) =>
          # TODO(MATT): examine implications
          context.assign-existential(supertype, subtype)
        | t-app(a-onto, a-args, _) =>
          context.assign-existential(supertype, subtype)
        | t-top(_) =>
          context.assign-existential(supertype, subtype)
        | t-bot(_) =>
          context.assign-existential(supertype, subtype)
        | t-record(fields, _) =>
          context.assign-existential(supertype, subtype)
        | t-ref(a-typ, _) =>
          context.assign-existential(supertype, subtype)
        | t-existential(a-id, _) =>
          context.assign-existential(supertype, subtype)
        | t-data(_, variants, fields, _) =>
          context.assign-existential(supertype, subtype)
        | t-data-refinement(_, _, _) =>
          context.assign-existential(supertype, subtype)
      end
    | else => raise("cannot instantiate non-existential")
  end
end

fun instantiate-left(subtype :: Type, supertype :: Type, context :: Context) -> Context:
  result = _instantiate-left(subtype, supertype, context)
  #print("")
  #print(tostring(subtype) + " :=< " + tostring(supertype))
  #print("result:")
  #print(result)
  result
end

fun _instantiate-left(subtype :: Type, supertype :: Type, context :: Context) -> Context:
  cases(Type) subtype:
    | t-existential(a-id, a-l) =>
      cases(Type) supertype:
        | t-name(b-mod, b-id, _) =>
          context.assign-existential(subtype, supertype)
        | t-var(b-id, _) =>
          context.assign-existential(subtype, supertype)
        | t-arrow(b-args, b-ret, b-l) =>
          args-and-existentials = b-args.map(lam(arg): pair(arg, new-existential(arg.l)) end)
          arg-existentials = split(args-and-existentials).right
          ret-existential = new-existential(b-ret.l)
          shadow context = context.assign-existential(subtype, t-arrow(arg-existentials, ret-existential, a-l))
          shadow context = args-and-existentials.foldr(lam(arg-and-exists, shadow context):
            instantiate-right(arg-and-exists.left, arg-and-exists.right, context)
          end, context)
          instantiate-left(ret-existential, context.apply(b-ret), context)
        | t-forall(b-introduces, b-onto, _) =>
          # TODO(MATT): examine consequences
          context.assign-existential(subtype, supertype)
        | t-app(b-onto, b-args, _) =>
          context.assign-existential(subtype, supertype)
        | t-top(_) =>
          context.assign-existential(subtype, supertype)
        | t-bot(_) =>
          context.assign-existential(subtype, supertype)
        | t-record(fields, _) =>
          context.assign-existential(subtype, supertype)
        | t-ref(b-type, _) =>
          context.assign-existential(subtype, supertype)
        | t-existential(b-id, _) =>
          context.assign-existential(supertype, subtype)
        | t-data(_, variants, fields, _) =>
          context.assign-existential(subtype, supertype)
        | t-data-refinement(_, _, _) =>
          context.assign-existential(subtype, supertype)
      end
    | else => raise("cannot instantiate non-existential")
  end
end

fun least-upper-bound(s, t, loc, context):
  result-type = _least-upper-bound(s, t, loc, context)
  #print("")
  #print(tostring(s) + " V " + tostring(t) + " = " + tostring(result-type))
  result-type
end

# TODO(MATT): t-ref?
fun _least-upper-bound(s :: Type, t :: Type, loc :: Loc, context :: Context) -> Type:
  cases(Option<Context>) satisfies-type(s, t, context):
    | some(new-context) =>
      new-context.apply(t.set-loc(loc))
    | none =>
      cases(Option<Context>) satisfies-type(t, s, context):
        | some(shadow context) =>
          context.apply(s.set-loc(loc))
        | none =>
          cases(Type) s:
            | t-arrow(s-args, s-ret, _) =>
              cases(Type) t:
                | t-arrow(t-args, t-ret, _) =>
                  maybe-glbs = fold2-strict(lam(glb-args, s-arg, t-arg):
                    glb = greatest-lower-bound(s-arg, t-arg, loc, context)
                    link(glb, glb-args)
                  end, empty, s-args, t-args)
                  cases(Option<List<Type>>) maybe-glbs:
                    | none => t-top(loc)
                    | some(m-args) =>
                      j-typ = least-upper-bound(s-ret, t-ret, loc, context)
                      t-arrow(m-args, j-typ, loc)
                  end
                | else => t-top(loc)
              end
            | t-app(s-onto, s-args, _) =>
              cases(Type) t:
                | t-app(t-onto, t-args, _) =>
                  # TODO(MATT): this is almost definitely broken
                  if resolve-alias(s-onto, context) == resolve-alias(t-onto, context):
                    maybe-new-args = map2-strict(lam(s-arg, t-arg):
                      least-upper-bound(s-arg, t-arg, loc, context)
                    end, s-args, t-args)
                    cases(Option<List<Type>>) maybe-new-args:
                      | none => t-top(loc)
                      | some(new-args) =>
                        t-app(s-onto, new-args, loc)
                    end
                  else:
                    t-top(loc)
                  end
                | else => t-top(loc)
              end
            | t-record(s-fields, _) =>
              cases(Type) t:
                | t-record(t-fields, _) =>
                  t-record(meet-fields(s-fields, t-fields, loc, context), loc)
                | else => t-top(loc)
              end
            | else => t-top(loc)
          end
      end
  end
end

fun greatest-lower-bound(s, t, loc, context):
  result-type = _greatest-lower-bound(s, t, loc, context)
  #print("")
  #print(tostring(s) + " V " + tostring(t) + " = " + tostring(result-type))
  result-type
end

fun _greatest-lower-bound(s :: Type, t :: Type, loc :: Loc, context :: Context) -> Type:
  cases(Option<Context>) satisfies-type(s, t, context):
    | some(shadow context) =>
      context.apply(s.set-loc(loc))
    | none =>
      cases(Option<Context>) satisfies-type(t, s, context):
        | some(shadow context) =>
          context.apply(t.set-loc(loc))
        | none =>
          cases(Type) s:
            | t-arrow(s-args, s-ret, _) =>
              cases(Type) t:
                | t-arrow(t-args, t-ret, _) =>
                  maybe-lubs = fold2-strict(lam(lub-args, s-arg, t-arg):
                    lub = least-upper-bound(s-arg, t-arg, loc, context)
                    link(lub, lub-args)
                  end, empty, s-args, t-args)
                  cases(Option<List<Type>>) maybe-lubs:
                    | none => t-top(loc)
                    | some(j-args) =>
                      m-typ = greatest-lower-bound(s-ret, t-ret, loc, context)
                      t-arrow(j-args, m-typ, loc)
                  end
                | else => t-bot(loc)
              end
            | t-app(s-onto, s-args, _) =>
              cases(Type) t:
                | t-app(t-onto, t-args, _) =>
                  # TODO(MATT): this is almost definitely broken
                  if resolve-alias(s-onto, context) == resolve-alias(t-onto, context):
                    maybe-new-args = map2-strict(lam(s-arg, t-arg):
                      greatest-lower-bound(s-arg, t-arg, loc, context)
                    end, s-args, t-args)
                    cases(Option<List<Type>>) maybe-new-args:
                      | none => t-bot(loc)
                      | some(new-args) =>
                        t-app(s-onto, new-args, loc)
                    end
                  else:
                    t-bot(loc)
                  end
                | else => t-bot(loc)
              end
            | t-record(s-fields, _) =>
              cases(Type) t:
                | t-record(t-fields, _) =>
                  t-record(join-fields(s-fields, t-fields, loc, context), loc)
                | else => t-bot(loc)
              end
            | else => t-bot(loc)
          end
      end
  end
end

fun meet-fields(a-fields :: List<TypeMember>, b-fields :: List<TypeMember>, loc :: Loc, context :: Context) -> List<TypeMember>:
  a-fields.foldr(lam(a-field, meet-list):
    field-name = a-field.field-name
    cases(Option<TypeMember>) type-members-lookup(b-fields, field-name):
      | some(b-field) =>
        lub = least-upper-bound(a-field.typ, b-field.typ, loc, context)
        link(t-member(field-name, lub), meet-list)
      | none => meet-list
    end
  end, empty)
end

fun join-fields(a-fields :: List<TypeMember>, b-fields :: List<TypeMember>, loc :: Loc, context :: Context) -> List<TypeMember>:
  initial-members = a-fields.foldr(lam(a-field, join-list):
    field-name = a-field.field-name
    cases(Option<TypeMember>) type-members-lookup(b-fields, field-name):
      | some(b-field) =>
        glb = greatest-lower-bound(a-field.typ, b-field.typ, loc, context)
        link(t-member(field-name, glb), join-list)
      | none =>
        link(a-field, join-list)
    end
  end, empty)

  b-fields.foldr(lam(b-field, join-list):
    field-name = b-field.field-name
    cases(Option<TypeMember>) type-members-lookup(join-list, field-name):
      | some(_) => join-list
      | none => link(b-field, join-list)
    end
  end, initial-members)
end

# TODO(MATT): better errors
# TODO(MATT): update context
fun meet-branch-types(branch-types :: List<Type>, loc :: Loc, context :: Context) -> FoldResult<Type>:
  meet-type = branch-types.foldr(lam(branch-type, current-type):
    least-upper-bound(current-type, branch-type, loc, context)
  end, t-bot(A.dummy-loc))
  cases(Type) meet-type:
    | t-top(l) => fold-errors([list: C.different-branch-types(loc, branch-types)])
    | else => fold-result(meet-type, context)
  end
end

fun handle-type-let-binds(bindings :: List<A.TypeLetBind>, context :: Context) -> FoldResult<Nothing>:
  map-fold-result(lam(binding, shadow context):
    cases(A.TypeLetBind) binding:
      | s-type-bind(l, name, params, ann) =>
        to-type(ann, context).bind(lam(maybe-typ, _):
          cases(Option<Type>) maybe-typ:
            | none => # TODO(MATT): is this correct?
              fold-errors([list: C.unbound-type-id(ann)])
            | some(typ) =>
              alias-type =
                if is-empty(params):
                  typ
                else:
                  forall = for map(param from params): t-var(param, l) end
                  t-forall(forall, typ, l)
                end
              context.aliases.set-now(name.key(), alias-type)
              fold-result(nothing, context)
          end
        end)
      | s-newtype-bind(l, name, namet) =>
        typ = t-name(none, namet, l)
        namet-key = namet.key()
        context.aliases.set-now(name.key(), typ)
        shadow context = context.add-binding(namet-key, t-record([list:
          t-member("test", t-arrow([list: t-top(l)], t-boolean(l), l)),
          t-member("brand", t-arrow([list: t-top(l)], typ, l))
        ], l))
        fold-result(nothing, context)
    end
  end, bindings, context)
end

fun collect-bindings(binds :: List<A.Bind>, context :: Context) -> FoldResult<SD.StringDict<Type>>:
  foldr-fold-result(lam(binding, _, dict):
    to-type(binding.ann, context).bind(lam(maybe-type, _):
      new-type = cases(Option<Type>) maybe-type:
        | some(typ) => typ.set-loc(binding.l)
        | none =>
          cases(A.Name) binding.id:
            | s-atom(base, _) =>
              if base == "$underscore":
                t-top(binding.l) # TODO(MATT): this probably should be t-forall instead but then we'll need to figure out skolemization
              else:
                new-existential(binding.l)
              end
            | else =>
              new-existential(binding.l)
          end
      end
      fold-result(dict.set(binding.id.key(), new-type), context)
    end)
  end, binds, context, SD.make-string-dict())
end

fun collect-letrec-bindings(binds :: List<A.LetrecBind>, top-level :: Boolean, context :: Context) -> FoldResult<SD.StringDict<Type>>:
  collect-bindings(binds.map(_.b), context).bind(lam(collected, _):
    shadow context = context.add-dict-to-bindings(collected)
    foldr-fold-result(lam(binding, _, collected-bindings):
      initial-type = collected-bindings.get-value(binding.b.id.key())
      if TS.is-t-existential(initial-type):
        cases(Expr) binding.value:
          | s-lam(lam-l, lam-params, lam-args, lam-ann, _, _, _) =>
            collect-bindings(lam-args, context).bind(lam(arg-coll, _):
              lam-to-type(arg-coll, lam-l, lam-params, lam-args, lam-ann, top-level, context).bind(lam(lam-type, _):
                fold-result(collected-bindings.set(binding.b.id.key(), lam-type), context)
              end)
            end)
          | else => fold-result(collected-bindings, context)
        end
      else:
        fold-result(collected-bindings, context)
      end
    end, binds, context, collected)
  end)
end

fun check-synthesis(e :: Expr, expect-type :: Type, top-level :: Boolean, context :: Context) -> TypingResult:
  synthesis(e, top-level, context).bind(lam(new-expr, new-type, shadow context):
    applied-new-type = context.apply(new-type)
    applied-expect-type = context.apply(expect-type)
    maybe-context = satisfies-type(applied-new-type, applied-expect-type, context)
    cases(Option<Context>) maybe-context:
      | some(shadow context) =>
        typing-result(new-expr, context.apply(applied-expect-type), context)
      | none =>
        # TODO(MATT): different error for existential
        typing-error([list: C.incorrect-type(tostring(applied-new-type), applied-new-type.l, tostring(applied-expect-type), applied-expect-type.l)])
    end
  end)
end

fun synthesis-datatype(l :: Loc, name :: String, namet :: A.Name, params :: List<A.Name>, mixins, variants :: List<A.Variant>, fields :: List<A.Member>, _check, context :: Context) -> TypingResult:
  brander-type = t-name(none, namet, l)
  t-vars = params.map(t-var(_, l))

  map-fold-result(collect-variants, variants, context).typing-bind(lam(initial-variants-and-types, shadow context):
    map-fold-result(collect-member(_, false, _), fields, context).typing-bind(lam(initial-shared-field-types, shadow context):
      split-result = split(initial-variants-and-types)
      initial-variants = split-result.left
      initial-variant-types = split-result.right

      temp-initial-data-type = t-data(name, initial-variant-types, initial-shared-field-types, l)
      initial-data-type =
        if is-empty(t-vars):
          temp-initial-data-type
        else:
          t-forall(t-vars, temp-initial-data-type, l)
        end

      context.data-types.set-now(namet.key(), initial-data-type)

      fold-new-variants = map-fold-result(lam(variant-and-type-variant, shadow context):
        variant = variant-and-type-variant.left
        type-variant = variant-and-type-variant.right
        current-data-type = t-data-refinement(if is-empty(t-vars): brander-type else: t-app(brander-type, t-vars, l) end, variant.name, l)

        paired-with-members = map2(lam(with-member, with-type-member):
          pair(with-member, with-type-member)
        end, variant.with-members, type-variant.with-fields)

        fold-new-with-fields = map-fold-result(lam(paired-with-member, shadow context):
          with-member = paired-with-member.left
          with-type-member = paired-with-member.right
          to-type-member(with-member, with-type-member, current-data-type, false, context).bind(lam(new-with-type-member, shadow context):
            fold-result(new-with-type-member, context)
          end)
        end, paired-with-members, context)

        fold-new-with-fields.bind(lam(new-with-fields, shadow context):
          cases(TypeVariant) type-variant:
            | t-variant(variant-name, variant-fields, _) =>
              fold-result(t-variant(variant-name, variant-fields, new-with-fields), context)
            | t-singleton-variant(variant-name, _) =>
              fold-result(t-singleton-variant(variant-name, new-with-fields), context)
          end
        end)
      end, initial-variants-and-types, context)

      fold-new-variants.typing-bind(lam(new-variant-types, shadow context):
        variant-type-fields = new-variant-types.map(lam(var-type):
          var-type.fields.append(var-type.with-fields)
        end)
        variants-meet = cases(List<TypeMember>) variant-type-fields:
          | empty => empty
          | link(first, rest) =>
            cases(List<TypeMember>) rest:
              | empty => first
              | link(_, _) =>
                rest.foldr(meet-fields(_, _, l, context), first)
            end
        end

        data-type-fields = initial-shared-field-types.append(variants-meet)
        # TODO(MATT): update each of these
        new-data-expr = A.s-data-expr(l, name, namet, params, mixins, initial-variants, fields, _check)

        temp-data-type = t-data(name, new-variant-types, data-type-fields, l)
        new-data-type =
          if is-empty(t-vars):
            temp-data-type
          else:
            t-forall(t-vars, temp-data-type, l)
          end
        context.data-types.set-now(namet.key(), new-data-type)

        data-fields = link(t-member(name, t-arrow([list: t-top(l)], t-boolean(l), l)),
          for map(variant-type from new-variant-types):
            t-member(variant-type.name, mk-constructor-type(variant-type, brander-type, t-vars, l))
          end +
          for map(variant-type from new-variant-types):
            t-member("is-" + variant-type.name, t-arrow([list: t-top(l)], t-boolean(l), l))
          end)

        data-expr-type = t-record(data-fields, l)
        typing-result(new-data-expr, data-expr-type, context)
      end)
    end)
  end)
end

# TODO(MATT): also return the "changed" A.Member?
# doesn't check data-fields that aren't methods or functions
# only checks data-fields that are functions when type-check-functions is true
fun to-type-member(member :: A.Member, type-member :: TypeMember, self-type :: Type, type-check-functions :: Boolean, context :: Context) -> FoldResult<TypeMember>:
  fun add-self-type(fun-type :: Type) -> Type:
    cases(Type) fun-type:
      | t-arrow(args, ret, l) =>
        t-arrow(link(self-type, args), ret, l)
      | t-forall(introduces, onto-arrow, l) =>
        cases(Type) onto-arrow:
        | t-arrow(args, ret, inner-l) =>
          t-forall(introduces, t-arrow(link(self-type, args), ret, inner-l), l)
        | else =>
          raise("method type is not a function (this shouldn't happen")
        end
      | else =>
        raise("method type is not a function (this shouldn't happen")
    end
  end

  fun remove-self-type(fun-type :: Type) -> Type:
    cases(Type) fun-type:
      | t-arrow(args, ret, l) =>
        t-arrow(args.rest, ret, l)
      | t-forall(introduces, onto-arrow, l) =>
        cases(Type) onto-arrow:
        | t-arrow(args, ret, inner-l) =>
          t-forall(introduces, t-arrow(args.rest, ret, inner-l), l)
        | else =>
          raise("method type is not a function (this shouldn't happen")
        end
      | else =>
        raise("method type is not a function (this shouldn't happen")
    end
  end

  cases(A.Member) member:
    | s-data-field(l, name, value) =>
      cases(Expr) value:
        | s-method(m-l, params, args, ann, doc, body, _check) =>
          new-type = add-self-type(type-member.typ)
          check-fun(m-l, body, params, args, ann, new-type, A.s-method(m-l, params, _, _, doc, _, _check), context)
            .fold-bind(lam(_, out-type, shadow context):
              fold-result(t-member(name, remove-self-type(context.apply(out-type))), context)
            end)
        | s-lam(l-l, params, args, ann, doc, body, _check) =>
          if type-check-functions:
            # TODO(MATT): false or true for top-level?
            checking(value, type-member.typ, false, context)
              .fold-bind(lam(new-ast, new-type, shadow context):
                fold-result(type-member(name, new-type), context)
              end)
          else:
            fold-result(type-member, context)
          end
        | else =>
          fold-result(type-member, context)
      end
    | s-mutable-field(l, name, ann, value) =>
      raise("Mutable fields not handled yet")
    | s-method-field(m-l, name, params, args, ann, doc, body, _check) =>
      new-type = add-self-type(type-member.typ)
      check-fun(m-l, body, params, args, ann, new-type, A.s-method(m-l, params, _, _, doc, _, _check), context)
        .fold-bind(lam(_, out-type, shadow context):
          fold-result(t-member(name, remove-self-type(context.apply(out-type))), context)
        end)
  end
end

# collect-functions: if true gather annotations from lambda terms
#                    else synthesize lambda terms
fun collect-member(member :: A.Member, collect-functions :: Boolean, context :: Context) -> FoldResult<TypeMember>:
  cases(A.Member) member:
    | s-data-field(l, name, value) =>
      folded-field-type = cases(Expr) value:
        | s-method(m-l, params, args, ann, _, _, _) =>
          cases(List<A.Bind>) args:
            | empty =>
              fold-errors([list: C.method-missing-self(value)])
            | link(self, rest) =>
              collect-bindings(rest, context).bind(lam(bindings, shadow context):
                lam-to-type(bindings, m-l, params, args.rest, ann, not(collect-functions), context)
              end)
          end
        | s-lam(l-l, params, args, ann, _, _, _) =>
          if collect-functions:
            collect-bindings(args, context).bind(lam(bindings, shadow context):
              lam-to-type(bindings, l-l, params, args.rest, ann, false, context)
            end)
          else:
            synthesis(value, true, context)
              .fold-bind(lam(_, value-type, shadow context):
                fold-result(value-type, context)
              end)
          end
        | else =>
          synthesis(value, false, context)
            .fold-bind(lam(_, value-type, shadow context):
              fold-result(value-type, context)
            end)
      end
      folded-field-type.bind(lam(field-type, shadow context):
        fold-result(t-member(name, field-type), context)
      end)
    | s-mutable-field(l, name, ann, value) =>
      raise("Mutable fields not handled yet")
    | s-method-field(l, name, params, args, ann, doc, body, _check) =>
      cases(List<A.Bind>) args:
        | empty =>
          fold-errors([list: C.method-missing-self(member)])
        | link(self, rest) =>
          collect-bindings(rest, context).bind(lam(bindings, shadow context):
            lam-to-type(bindings, l, params, args.rest, ann, not(collect-functions), context)
              .bind(lam(lam-type, shadow context): fold-result(t-member(name, lam-type), context) end)
          end)
      end
  end
end

fun collect-variants(variant :: A.Variant, context :: Context) -> FoldResult<Pair<A.Variant, TypeVariant>>:
  cases(A.Variant) variant:
    | s-variant(l, constr-loc, name, members, with-members) =>
      fun process-member(member, shadow context):
        wrap = cases(A.VariantMemberType) member.member-type:
          | s-normal => lam(x): x.set-loc(member.l) end
          | s-mutable => lam(x): t-ref(x.set-loc(member.l), member.l) end
        end
        to-type(member.bind.ann, context).bind(lam(maybe-typ, shadow context):
          cases(Option<Type>) maybe-typ:
            | none => fold-errors([list: C.cant-typecheck("No type annotation provided on member", l)])
            | some(typ) =>
              fold-result(t-member(member.bind.id.toname(), wrap(typ)), context)
          end
        end)
      end

      map-fold-result(process-member, members, context).bind(lam(type-members, shadow context):
        map-fold-result(collect-member(_, false, _), with-members, context).bind(lam(type-with-members, shadow context):
          new-variant = A.s-variant(l, constr-loc, name, members, with-members)
          type-variant = t-variant(name, type-members, type-with-members)
          fold-result(pair(new-variant, type-variant), context)
        end)
      end)
    | s-singleton-variant(l, name, with-members) =>
      map-fold-result(collect-member(_, false, _), with-members, context).bind(lam(type-with-members, shadow context):
        new-variant = A.s-singleton-variant(l, name, with-members)
        type-variant = t-singleton-variant(name, type-with-members)
        fold-result(pair(new-variant, type-variant), context)
      end)
  end
end

fun to-type-variant(variant :: A.Variant, context :: Context) -> FoldResult<Pair<A.Variant, TypeVariant>>:
  cases(A.Variant) variant:
    | s-variant(l, constr-loc, name, members, with-members) =>
      map-fold-result(to-type-member, with-members, context).bind(lam(result, shadow context):
        split-result = split(result)
        new-with-members = split-result.left
        with-type-members = split-result.right

        fun process-member(member, shadow context):
          wrap = cases(A.VariantMemberType) member.member-type:
            | s-normal => lam(x): x.set-loc(member.l) end
            | s-mutable => lam(x): t-ref(x.set-loc(member.l), member.l) end
          end
          to-type(member.bind.ann, context).bind(lam(maybe-typ, shadow context):
            cases(Option<Type>) maybe-typ:
              | none => fold-errors([list: C.cant-typecheck("No type annotation provided on member", l)])
              | some(typ) =>
                fold-result(t-member(member.bind.id.toname(), wrap(typ)), context)
            end
          end)
        end

        map-fold-result(process-member, members, context).bind(lam(type-members, shadow context):
          new-variant = A.s-variant(l, constr-loc, name, members, new-with-members)
          type-variant = t-variant(name, type-members, with-type-members)
          fold-result(pair(new-variant, type-variant), context)
        end)
      end)
    | s-singleton-variant(l, name, with-members) =>
      map-fold-result(to-type-member, with-members, context).bind(lam(result, shadow context):
        split-result = split(result)
        new-with-members = split-result.left
        with-type-members = split-result.right
        new-variant = A.s-singleton-variant(l, name, new-with-members)
        type-variant = t-singleton-variant(name, with-type-members)
        fold-result(pair(new-variant, type-variant), context)
      end)
  end
end

fun mk-constructor-type(variant-typ :: TypeVariant, brander-typ :: Type, params :: List<Type>, l :: Loc) -> Type:
  cases(TypeVariant) variant-typ:
    | t-variant(name, fields, _) =>
      field-types = fields.map(lam(field):
        cases(Type) field.typ:
          | t-ref(ref-typ, _) => ref-typ
          | else => field.typ
        end
      end)
      if is-empty(params):
        t-arrow(field-types, brander-typ, l)
      else:
        t-forall(params, t-arrow(field-types, t-app(brander-typ, params, l), l), l)
      end
    | t-singleton-variant(name, _) =>
      if is-empty(params):
        brander-typ
      else:
        t-forall(params, t-app(brander-typ, params, l), l)
      end
  end
end

fun synthesis-field(access-loc :: Loc, obj :: Expr, obj-type-loc :: Loc, obj-type :: Type, field-name :: String, recreate :: (Loc, Expr, String -> Expr), context :: Context) -> TypingResult:
  record-view(access-loc, obj-type-loc, obj-type,
  lam(l, maybe-obj-fields):
    cases(Option<List<TypeMember>>) maybe-obj-fields:
      | some(obj-fields) =>
        cases(Option<TypeMember>) type-members-lookup(obj-fields, field-name):
          | some(tm) =>
            typing-result(recreate(l, obj, field-name), tm.typ, context)
          | none =>
            typing-error([list: C.object-missing-field(field-name, "{" + obj-fields.map(tostring).join-str(", ") + "}", l, access-loc)])
        end
      | none =>
        # TODO(MATT): decide about this
        typing-result(recreate(l, obj, field-name), t-bot(l), context)
    end
  end, context)
end

fun record-view(access-loc :: Loc, obj-type-loc :: Loc, obj-type :: Type,
                handle :: (Loc, Option<List<TypeMember>> -> TypingResult),
                context :: Context) -> TypingResult:
  non-obj-err = typing-error([list: C.incorrect-type(tostring(obj-type), obj-type-loc, "an object type", access-loc)])
  cases(Type) obj-type:
    | t-record(members, _) =>
      handle(obj-type-loc, some(members))
    | t-data(_, variants, fields, _) =>
      handle(obj-type-loc, some(fields))
    | t-forall(introduces, onto, l) =>
      new-obj-type = introduces.foldr(lam(type-var, new-type):
        new-type.substitute(new-existential(l), type-var)
      end, onto)
      record-view(access-loc, obj-type-loc, new-obj-type, handle, context)
    | t-data-refinement(data-type, variant-name, _) =>
      cases(Option<Type>) context.get-data-type(data-type):
        | some(typ) =>
          cases(Type) typ:
            | t-data(_, variants, data-fields, _) =>
              variant = find(lam(variant): variant.name == variant-name end, variants).value
              cases(TypeVariant) variant:
                | t-variant(_, fields, with-fields) =>
                  handle(obj-type-loc, some(fields + with-fields + data-fields))
                | t-singleton-variant(_, with-fields) =>
                  handle(obj-type-loc, some(with-fields + data-fields))
              end
            | else => non-obj-err
          end
        | none => non-obj-err
      end
    | t-bot(_) =>
      handle(obj-type-loc, none)
    | else =>
      cases(Option<Type>) context.get-data-type(obj-type):
        | some(data-type) =>
          record-view(access-loc, obj-type-loc, data-type, handle, context)
        | none => non-obj-err
      end
  end
end

fun lookup-id(blame-loc :: A.Loc, id-key :: String, id-expr :: Expr, context :: Context) -> FoldResult<Type>:
  if context.binds.has-key(id-key):
    fold-result(context.binds.get-value(id-key), context)
  else if context.types.has-key-now(id-key):
    fold-result(context.types.get-value-now(id-key), context)
  else:
    fold-errors([list: C.unbound-id(id-expr)])
  end
end

fun lam-to-type(coll :: SD.StringDict<Type>, l :: Loc, params :: List<A.Name>, args :: List<A.Bind>, ret-ann :: A.Ann, top-level :: Boolean, context :: Context) -> FoldResult<Type>:
  to-type(ret-ann, context).bind(lam(maybe-type, shadow context):
    ret-type = cases(Option<Type>) maybe-type:
      | some(typ) => typ
      | none => new-existential(l)
    end
    fold-arg-types = map-fold-result(lam(arg, shadow context):
      arg-type = coll.get-value(arg.id.key())
      if top-level and is-t-existential(arg-type):
        fold-errors([list: C.toplevel-unann(arg)])
      else:
        fold-result(arg-type, context)
      end
    end, args, context)

    fold-arg-types.bind(lam(arg-types, shadow context):
      arrow-type = t-arrow(arg-types, ret-type, l)
      lam-type =
        if is-empty(params):
          arrow-type
        else:
          forall = for map(param from params): t-var(param, l) end
          t-forall(forall, arrow-type, l)
        end
      fold-result(lam-type, context)
    end)
  end)
end

# TODO(MATT): this really should get checked out
fun synthesis-fun(l :: Loc, body :: Expr, params :: List<A.Name>, args :: List<A.Bind>, ret-ann :: A.Ann, recreate :: (List<A.Bind>, A.Ann, A.Expr -> A.Expr), top-level :: Boolean, context :: Context) -> TypingResult:
  fun set-ret-type(lam-type :: Type, ret-type :: Type) -> Type:
    cases(Type) lam-type:
      | t-arrow(arg-types, _, a-l) =>
        t-arrow(arg-types, ret-type, a-l)
      | t-forall(introduces, onto, f-l) =>
        cases(Type) onto:
          | t-arrow(arg-types, _, a-l) =>
            t-forall(introduces, t-arrow(arg-types, ret-type, a-l), f-l)
          | else => raise("This shouldn't happen (non-function type lambda)")
        end
      | else => raise("This shouldn't happen (non-function type lambda)")
    end
  end

  collected = collect-bindings(args, context)
  collected.typing-bind(lam(coll, _):
    fold-lam-type = lam-to-type(coll, l, params, args, ret-ann, top-level, context)
    fold-lam-type.typing-bind(lam(lam-type, shadow context):
      fold-ret-type = cases(Type) lam-type:
        | t-arrow(_, ret-type, _) =>
          fold-result(ret-type, context)
        | t-forall(_, onto, _) =>
          cases(Type) onto:
            | t-arrow(_, ret-type, _) =>
              fold-result(ret-type, context)
            | else => raise("This shouldn't happen (non-function type lambda)")
          end
        | else => raise("This shouldn't happen (non-function type lambda)")
      end
      fold-ret-type.typing-bind(lam(ret-type, shadow context):
        checking(body, ret-type, false, context.add-dict-to-bindings(coll))
          .bind(lam(new-body, new-ret-type, shadow context):
            typing-result(recreate(args, ret-ann, new-body),
                          set-ret-type(lam-type, new-ret-type),
                          context)
          end)
      end)
    end)
  end)
end

fun gather-provides(_provide :: A.Provide, context :: Context) -> FoldResult<TCInfo>:
  cases(A.Provide) _provide:
    | s-provide-complete(_, values, aliases, data-definitions) =>
      fold-values-info = foldr-fold-result(lam(value, _, info):
        value-key = value.v.key()
        if info.types.has-key(value-key):
          fold-result(info, context)
        else:
          typ = context.binds.get-value(value-key)
          fold-result(TCS.tc-info(info.types.set(value-key, typ), info.aliases, info.data-types), context)
        end
      end, values, context, context.info)
      fold-values-info.bind(lam(values-info, _):
        fold-aliases-info = foldr-fold-result(lam(_alias, _, info):
          alias-key = _alias.in-name.key()
          if info.aliases.has-key(alias-key):
            fold-result(info, context)
          else:
            typ = context.aliases.get-value-now(alias-key)
            fold-result(TCS.tc-info(info.types, info.aliases.set(alias-key, typ), info.data-types), context)
          end
        end, aliases, context, values-info)
        fold-aliases-info.bind(lam(aliases-info, _):
          foldr-fold-result(lam(data-type, _, info):
            data-key = data-type.d.key()
            if info.data-types.has-key(data-key):
              fold-result(info, context)
            else:
              typ = context.data-types.get-value-now(data-key)
              fold-result(TCS.tc-info(info.types, info.aliases, info.data-types.set(data-key, typ)), context)
            end
          end, data-definitions, context, aliases-info)
        end)
      end)
    | else => raise("Haven't handled anything but s-provide-complete")
  end
end

fun synthesis-let-bind(binding :: A.LetBind, context :: Context) -> TypingResult:
  cases(A.LetBind) binding:
    | s-let-bind(l, b, value) =>
      to-type(b.ann, context).typing-bind(lam(maybe-type, shadow context):
        ann-type = cases(Option<Type>) maybe-type:
          | some(typ) => typ
          | none => new-existential(l)
        end
        checking(value, ann-type, false, context)
          .bind(lam(new-value, new-type, shadow context):
            typing-result(new-value, new-type, context.add-binding(b.id.key(), new-type))
          end)
      end)
    | s-var-bind(l, b, value) =>
      to-type(b.ann, context).typing-bind(lam(maybe-type, shadow context):
        ann-type = cases(Option<Type>) maybe-type:
          | some(typ) => typ
          | none => new-existential(l)
        end
        checking(value, ann-type, false, context)
          .bind(lam(new-value, new-type, shadow context):
            typing-result(new-value, t-ref(new-type, l), context.add-binding(b.id.key(), t-ref(new-type, l)))
          end)
      end)
  end
end

fun checking-cases(l :: Loc, ann :: A.Ann, val :: Expr, branches :: List<A.CasesBranch>, maybe-else :: Option<Expr>, expect-type :: Type, context :: Context) -> TypingResult:
  handle-cases(l, ann, val, branches, maybe-else, some(expect-type), context, checking-cases-has-else(expect-type), checking-cases-no-else(expect-type))
end

fun checking-cases-has-else(expect-type :: Type):
  lam(l :: Loc, ann :: A.Ann, new-val :: Expr, split-result :: Pair<List<A.CasesBranch>, List<Type>>, _else :: Expr, context :: Context) -> TypingResult:
    checking(_else, expect-type, false, context).bind(lam(new-else, new-type, shadow context):
      new-cases = A.s-cases-else(l, ann, new-val, split-result.left, new-else)
      typing-result(new-cases, new-type, context)
    end)
  end
end

fun checking-cases-no-else(expect-type :: Type):
  lam(l :: Loc, ann :: A.Ann, new-val :: Expr, split-result :: Pair<List<A.CasesBranch>, List<Type>>, context :: Context) -> TypingResult:
    new-cases = A.s-cases(l, ann, new-val, split-result.left)
    typing-result(new-cases, expect-type, context)
  end
end

fun synthesis-cases(l :: Loc, ann :: A.Ann, val :: Expr, branches :: List<A.CasesBranch>, maybe-else :: Option<A.Expr>, context :: Context) -> TypingResult:
  handle-cases(l, ann, val, branches, maybe-else, none, context, synthesis-cases-has-else, synthesis-cases-no-else)
    .map-type(_.set-loc(l))
end

fun synthesis-cases-has-else(l :: Loc, ann :: A.Ann, new-val :: A.Expr, split-result :: Pair<List<A.CasesBranch>, List<Type>>, _else :: A.Expr, context :: Context) -> TypingResult:
  synthesis(_else, false, context).bind(
    lam(new-else, else-type, shadow context):
      new-cases = A.s-cases-else(l, ann, new-val, split-result.left, new-else)
      meet-branch-types(link(else-type, split-result.right), l, context).typing-bind(lam(branches-type, shadow context):
        typing-result(new-cases, branches-type.set-loc(l), context)
      end)
    end)
end

fun synthesis-cases-no-else(l :: Loc, ann :: A.Ann, new-val :: Expr, split-result :: Pair<List<A.CasesBranch>, List<Type>>, context :: Context) -> TypingResult:
  new-cases = A.s-cases(l, ann, new-val, split-result.left)
  meet-branch-types(split-result.right, l, context).typing-bind(lam(branches-type, shadow context):
    typing-result(new-cases, branches-type.set-loc(l), context)
  end)
end

fun handle-cases(l :: Loc, ann :: A.Ann, val :: Expr, branches :: List<A.CasesBranch>, maybe-else :: Option<Expr>, maybe-expect :: Option<Type>, context :: Context, has-else, no-else) -> TypingResult:
  to-type(ann, context).typing-bind(lam(maybe-type, shadow context):
    cases(Option<Type>) maybe-type:
      | some(typ) =>
        cases(Option<Type>) context.get-data-type(typ):
          | some(data-type) =>
            synthesis(val, false, context).bind(lam(new-val, val-type, shadow context):
              body-data-type = cases(Type) data-type:
                | t-forall(introduces, onto-data-type, forall-l) =>
                  introduces.foldr(lam(type-var, new-type):
                    new-type.substitute(new-existential(forall-l), type-var)
                  end, onto-data-type)
                | else => data-type
              end
              cases(Option<Context>) satisfies-type(val-type, body-data-type, context):
                | some(shadow context) =>
                  shadow data-type = context.apply(body-data-type)
                  branch-tracker = track-branches(data-type)
                  temp-result = map-fold-result(lam(branch, shadow context):
                    branch-result = handle-branch(data-type, l, branch, maybe-expect, branch-tracker.remove, context)
                    branch-result.bind(lam(branch-type-pair, shadow context):
                      fold-result(branch-type-pair, context)
                    end)
                  end, branches, context)

                  temp-result.typing-bind(lam(result, shadow context):
                    split-result = split(result)
                    remaining-branches = branch-tracker.get().to-list()
                    cases(Option<A.Expr>) maybe-else:
                      | some(_else) =>
                        if is-empty(remaining-branches):
                          typing-error([list: C.unneccesary-else-branch(tostring(typ), l)])
                        else:
                          has-else(l, ann, new-val, split-result, _else, context)
                        end
                      | none =>
                        if is-empty(remaining-branches):
                          no-else(l, ann, new-val, split-result, context)
                        else:
                          # TODO(MATT): more appropriate error here
                          typing-error([list: C.non-exhaustive-pattern(remaining-branches, tostring(typ), l)])
                        end
                    end
                  end)
                | none =>
                  typing-error([list: C.incorrect-type(tostring(val-type), val-type.l, tostring(typ), typ.l)])
              end
            end)
          | none =>
            # TODO(MATT): this needs to be a much better error
            typing-error([list: C.cant-typecheck("No datatype provided for cases", l)])
        end
    end
  end)
end

fun track-branches(data-type :: Type % (is-t-data)) ->
  { remove :: (TypeVariant -> Set<TypeVariant>), get :: (-> Set<TypeVariant>) }:
  var unhandled-branches = data-type.variants.foldr(lam(b, s): s.add(b) end, [set:])
  {
    remove: lam(b :: TypeVariant):
      unhandled-branches := unhandled-branches.remove(b)
    end,
    get: lam() -> Set<TypeVariant>:
      unhandled-branches
    end
  }
end

fun handle-branch(data-type :: Type % (is-t-data), cases-loc :: A.Loc, branch :: A.CasesBranch, maybe-check :: Option<Type>, remove :: (TypeVariant -> Any), context :: Context) -> FoldResult<Pair<A.CasesBranch, Type>>:
  fun handle-body(variant :: TypeVariant, body :: A.Expr, process, shadow context :: Context):
    remove(variant)
    cases(Option<Type>) maybe-check:
      | some(expect-type) =>
        checking(body, expect-type, false, context)
          .fold-bind(process)
      | none =>
        synthesis(body, false, context)
          .fold-bind(process)
    end
  end

  cases(Option<TypeVariant>) data-type.lookup-variant(branch.name):
    | some(tv) =>
      cases(TypeVariant) tv:
        | t-variant(_, fields, _) =>
          cases(A.CasesBranch) branch:
            | s-cases-branch(l, pat-loc, name, args, body) =>
              fun process(new-body, typ, shadow context):
                new-branch = A.s-cases-branch(l, pat-loc, name, args, new-body)
                fold-result(pair(new-branch, typ), context)
              end

              shadow context = fold2-strict(lam(fold-context, arg, arg-typ):
                fold-context.bind(lam(_, shadow context):
                  to-type(arg.bind.ann, context).bind(lam(maybe-type, _):
                    cases(Option<Type>) maybe-type:
                      | some(typ) =>
                        cases(Option<Context>) satisfies-type(arg-typ, typ, context):
                          | some(shadow context) =>
                            fold-result(nothing, context.add-binding(arg.bind.id.key(), typ))
                          | none =>
                            # TODO(MATT): error message
                            raise("Branch does not satisfy type")
                        end
                      | none =>
                        fold-result(nothing, context.add-binding(arg.bind.id.key(), arg-typ))
                    end
                  end)
                end)
              end, fold-result(nothing, context), args, fields.map(_.typ))

              cases(Option<FoldResult<Nothing>>) context:
                | none =>
                  fold-errors([list: C.incorrect-number-of-bindings(branch, tv)])
                | some(folded-context) =>
                  folded-context.bind(lam(_, shadow context):
                    handle-body(tv, body, process, context)
                  end)
              end
            | s-singleton-cases-branch(l, _, name, _) =>
              fold-errors([list: C.cases-singleton-mismatch(name, l, false)])
          end
        | t-singleton-variant(_, _) =>
          cases(A.CasesBranch) branch:
            | s-cases-branch(l, _, name, _, _) =>
              fold-errors([list: C.cases-singleton-mismatch(name, l, true)])
            | s-singleton-cases-branch(l, pat-loc, name, body) =>
              fun process(new-body, typ, shadow context):
                new-branch = A.s-singleton-cases-branch(l, pat-loc, name, new-body)
                fold-result(pair(new-branch, typ), context)
              end
              handle-body(tv, body, process, context)
          end
      end
    | none =>
      fold-errors([list: C.unneccesary-branch(branch, data-type, cases-loc)])
  end
end

fun check-fun(fun-loc :: Loc, body :: Expr, params :: List<A.Name>, args :: List<A.Bind>, ret-ann :: A.Ann, expect-type :: Type, recreate :: (List<A.Bind>, A.Ann, Expr -> Expr), context :: Context) -> TypingResult:
  lam-bindings = collect-bindings(args, context)
  # TODO(MATT): checking when polymorphic lambda but non-polymorphic type

  cases(Type) expect-type:
    | t-arrow(expect-args, ret-type, _) =>
      lam-bindings.typing-bind(lam(temp-lam-binds, _):
        lam-binds = params.foldr(lam(param, lam-binds):
          new-exists = new-existential(fun-loc)
          lam-keys = lam-binds.keys()
          lam-keys.fold(lam(binds, key):
            binds.set(key, binds.get-value(key).substitute(new-exists, t-var(param, fun-loc)))
          end, lam-binds)
        end, temp-lam-binds)
        lam-arg-types = map(lam(arg): lam-binds.get-value(arg.id.key()) end, args)
        maybe-context = fold2-strict(lam(fold-context, arg, expect-arg):
          fold-context.bind(lam(_, shadow context):
            cases(Option<Context>) satisfies-type(expect-arg, arg, context):
              | some(shadow context) => fold-result(nothing, context)
              | none =>
                # TODO(MATT): error message
                raise("check-fun satisfies-type error")
            end
          end)
        end, fold-result(nothing, context.add-dict-to-bindings(lam-binds)), lam-arg-types, expect-args)
        cases(Option<FoldResult<Nothing>>) maybe-context:
          | none =>
            expected = "a function with " + tostring(expect-args.length()) + " arguments"
            found = "a function with " + tostring(args.length()) + " arguments"
            typing-error([list: C.incorrect-type(expected, fun-loc, found, expect-type.l)])
          | some(fold-ctxt) =>
            fold-ctxt.typing-bind(lam(_, shadow context):
              body-result = checking(body, ret-type, false, context)
              body-result.bind(lam(new-body, new-ret-type, shadow context):
                typing-result(recreate(args, ret-ann, new-body), expect-type, context)
              end)
            end)
        end
      end)
    | t-top(_) =>
      lam-bindings.typing-bind(lam(new-binds, _):
        body-result = checking(body, expect-type, false, context.add-dict-to-bindings(new-binds))
        body-result.bind(lam(new-body, new-type, shadow context):
          typing-result(recreate(args, ret-ann, new-body), expect-type, context)
        end)
      end)
    | t-forall(introduces, onto, l) =>
      check-fun(fun-loc, body, params, args, ret-ann, onto, recreate, context)
        .map-type(t-forall(introduces, _, l))
    | t-existential(id, _) =>
      check-synthesis(recreate(args, ret-ann, body), expect-type, false, context)
    | else =>
      typing-error([list: C.incorrect-type("a function", fun-loc, tostring(expect-type), expect-type.l)])
  end
end

fun synthesis-instantiation(l :: Loc, expr :: Expr, params :: List<A.Ann>, top-level :: Boolean, context :: Context) -> TypingResult:
  synthesis(expr, top-level, context).bind(lam(new-expr, tmp-type, shadow context):
    cases(Type) tmp-type:
      | t-forall(introduces, onto, _) =>
        map-fold-result(to-type, params, context).typing-bind(lam(new-maybe-types, shadow context):
          maybe-new-types = new-maybe-types.foldr(lam(maybe-type, new-types):
            for option-bind(typ from maybe-type):
              for option-bind(list-types from new-types):
                some(link(typ, list-types))
              end
            end
          end, some(empty))
          cases(Option<List<Type>>) maybe-new-types:
            | none => raise("Failure to determine types of forall")
            | some(new-types) =>
              cases(Option<Type>) remove-foralls(introduces, onto, new-types):
                | none => raise("Substituting types in forall failed")
                | some(new-type) =>
                  new-inst = A.s-instantiate(l, new-expr, params)
                  typing-result(new-inst, new-type.set-loc(l), context)
              end
          end
        end)
      | else =>
        raise("Instantiation not handled for " + tostring(tmp-type))
    end
  end)
end

fun remove-foralls(forall :: List<Type>, onto :: Type, replacements :: List<Type>) -> Option<Type>:
  for fold2-strict(curr from onto, variable from forall, replacement from replacements):
    curr.substitute(replacement, variable)
  end
end

fun handle-if-branch(branch :: A.IfBranch, context :: Context) -> FoldResult<Pair<A.IfBranch, Type>>:
  checking(branch.test, t-boolean(branch.l), false, context).fold-bind(
    lam(new-test, _, shadow context):
      synthesis(branch.body, false, context).fold-bind(
        lam(new-body, body-type, shadow context):
          new-branch = A.s-if-branch(branch.l, new-test, new-body)
          fold-result(pair(new-branch, body-type), context)
        end)
    end)
end

fun synthesis-update(update-loc :: Loc, obj :: Expr, obj-type :: Type, fields :: List<A.Member>, context :: Context) -> TypingResult:
  fun process-field(member, obj-fields, shadow context):
    cases(Option<TypeMember>) type-members-lookup(obj-fields, member.right.field-name):
      | some(btm) =>
        cases(Type) btm.typ:
          | t-ref(onto, l) =>
            cases(Option<Context>) satisfies-type(member.right.typ, onto, context):
              | some(shadow context) => fold-result(member.left, context)
              | none => fold-errors([list: C.incorrect-type(tostring(member.right.typ), update-loc, tostring(onto), obj-type.l)])
            end
          | else =>
            fold-errors([list: C.incorrect-type(tostring(btm.typ), obj-type.l, tostring(t-ref(btm.typ, update-loc)), update-loc)])
        end
      | none =>
        fold-errors([list: C.object-missing-field(member.right.field-name, "{" + obj-fields.map(tostring).join-str(", ") + "}", obj-type.l, update-loc)])
    end
  end

  fun process-fields(l, maybe-obj-fields, atms):
    cases(Option<List<TypeMember>>) maybe-obj-fields:
      | some(obj-fields) =>
        map-fold-result(process-field(_, obj-fields, _), atms, context).typing-bind(lam(new-fields, shadow context):
          typing-result(A.s-update(l, obj, new-fields), obj-type.set-loc(l), context)
        end)
      | none =>
        split-fields = split(atms)
        new-fields = split-fields.left
        typing-result(A.s-update(l, obj, new-fields), obj-type.set-loc(l), context)
    end
  end

  map-fold-result(collect-member(_, false, _), fields, context).typing-bind(lam(initial-new-members, shadow context):
    atms = map2(lam(atm, new-member):
      pair(atm, new-member)
    end, fields, initial-new-members)
    record-view(update-loc, obj-type.l, obj-type, process-fields(_, _, atms), context)
      .map-type(_.set-loc(update-loc))
  end)
end

fun synthesis-app-fun(app-loc :: Loc, _fun :: Expr, args :: List<Expr>, context :: Context) -> FoldResult<{fun-type :: Type, is-binop :: Boolean}>:
  cases(Expr) _fun:
    | s-id(fun-loc, id) =>
      binop-result = lam(new-type, shadow context):
        fold-result({fun-type: new-type.set-loc(app-loc), is-binop: true}, context)
      end
      result = lam(new-type, shadow context):
        fold-result({fun-type: new-type.set-loc(app-loc), is-binop: false}, context)
      end
      fun pick2(num-typ-f :: (A.Loc -> Type), rec-typ-f :: (A.Loc -> Type)):
        cases(List<A.Expr>) args:
          | empty =>
            fold-errors([list: C.incorrect-number-of-args(app-loc)])
          | link(f, r) =>
            synthesis(f, false, context).fold-bind(
              lam(_, f-typ, shadow context):
                ask:
                  | f-typ == t-number(A.dummy-loc) then: result(num-typ-f(fun-loc), context)
                  | is-t-record(f-typ) then: result(rec-typ-f(fun-loc), context)
                  | is-t-existential(f-typ) then: fold-errors([list:C.unable-to-infer(f-typ.l)])
                  | otherwise: fold-errors([list:
                      C.incorrect-type(tostring(f-typ), f-typ.l, "Number or an object with the field " + id.toname(), app-loc)])
                end
              end)
        end
      end
      fun pick3(num-typ-f :: (A.Loc -> Type), str-typ-f :: (A.Loc -> Type), rec-typ-f :: (A.Loc -> Type)):
        cases(List<A.Expr>) args:
          | empty =>
            fold-errors([list: C.incorrect-number-of-args(app-loc)])
          | link(f, r) =>
            synthesis(f, false, context).fold-bind(
              lam(_, f-typ, shadow context):
                ask:
                  | f-typ == t-number(A.dummy-loc) then: binop-result(num-typ-f(fun-loc), context)
                  | f-typ == t-string(A.dummy-loc) then: binop-result(str-typ-f(fun-loc), context)
                  | is-t-record(f-typ) then: binop-result(rec-typ-f(fun-loc), context)
                  | is-t-existential(f-typ) then: fold-errors([list:C.unable-to-infer(f-typ.l)])
                  | otherwise: fold-errors([list:
                    C.incorrect-type(tostring(f-typ), f-typ.l, "Number, String or an object with the field " + id.toname(), app-loc)])
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
        | otherwise: synthesis(_fun, false, context).fold-bind(lam(_, new-type, shadow context):
            fold-result({fun-type: new-type.set-loc(app-loc), is-binop: false}, context)
          end)
      end
    | else =>
      synthesis(_fun, false, context).fold-bind(lam(_, new-type, shadow context):
        fold-result({fun-type: new-type.set-loc(app-loc), is-binop: false}, context)
      end)
  end
end
