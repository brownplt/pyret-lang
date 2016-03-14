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
is-t-data = TS.is-t-data

bind = TCS.bind
typing-bind = TCS.typing-bind
fold-bind = TCS.fold-bind
map-fold-result = TCS.map-fold-result
foldr-fold-result = TCS.foldr-fold-result
fold-typing = TCS.fold-typing
resolve-alias = TCS.resolve-alias

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

fun fold2-strict<X, Y, R>(f :: (R, X, Y -> R), base :: R, l1 :: List<X>, l2 :: List<Y>) -> Option<R>:
  cases (List<X>) l1:
    | empty       => cases (List<Y>) l2:
        | empty       => some(base)
        | link(_, _)  => none
      end
    | link(a, ar) => cases (List<Y>) l2:
        | empty       => none
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
  for each(g from globts.keys-list()):
    context.aliases.set-now(A.s-global(g).key(), globts.get-value(g))
    # TODO(MATT): decide how to handle globals.types for the repl
    context.data-types.set-now(A.s-global(g).key(), globts.get-value(g))
  end
  for each(k from modules.keys-list-now()):
    when not(context.modules.has-key-now(k)):
      mod = modules.get-value-now(k).provides
      key = mod.from-uri
      val-provides = t-record(
        for map(v from mod.values.keys-list()): t-member(v, mod.values.get-value(v), program.l) end,
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
                for map(v from mod.values.keys-list()): t-member(v, mod.values.get-value(v), l) end,
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
      each(print, body.tosource().pretty(72))

      tc-result = checking(body, l, t-top(l), true, context)
      cases(TypingResult) tc-result:
        | typing-result(new-body, _, new-context) =>
          each(print, new-body.tosource().pretty(72))
          folded-info = gather-provides(_provide, new-context)
          cases(FoldResult<TCInfo>) folded-info:
            | fold-result(info) =>
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

fun checking(e, expect-loc, expect-typ, top-level, context):
  result = _checking(e, expect-loc, expect-typ, top-level, context)
  print("")
  print("checking:")
  each(print, e.tosource().pretty(72))
  print("has type: " + tostring(expect-typ))
  print("result:")
  print(result)
  result
end

fun _checking(e :: Expr, expect-loc :: Loc, expect-type :: Type, top-level :: Boolean, context :: Context) -> TypingResult:
  cases(Expr) e:
    | s-module(l, answer, defined-values, defined-types, provided-values, provided-types, checks) =>
      checking(answer, expect-loc, expect-type, false, context)
        .bind(lam(new-answer, _, out-context):
          provided-vals-context = cases(Expr) provided-values:
            | s-obj(_, fields) =>
              foldr-fold-result(lam(field, ctxt):
                cases(A.Member) field:
                  | s-data-field(_, name, value) =>
                    info = ctxt.info
                    synthesis(value, false, ctxt).fold-bind(lam(_, field-type, _):
                      fold-result(ctxt.set-info(
                        # TODO(MATT): is name correct here?
                        TCS.tc-info(info.types.set(name, field-type),
                                  info.aliases,
                                  info.data-types)))
                    end)
                  | else => raise("non data field in provided-values")
                end
              end, fields, out-context)
            | else => raise("provided-values isn't always an object")
          end
          for typing-bind(vals-ctxt from provided-vals-context):
            final-context = foldr-fold-result(lam(a-field, ctxt):
              fun add-aliases(typ :: Type, _context :: Context) -> Context:
                cases(Option<Type>) _context.aliases.get-now(typ.key()):
                  | some(alias-typ) =>
                    info = _context.info
                    new-context = _context.set-info(TCS.tc-info(info.types, info.aliases.set(typ.key(), alias-typ), info.data-types))
                    add-aliases(alias-typ, new-context)
                  | none => _context
                end
              end

              for bind(maybe-type from to-type(a-field.ann, ctxt)):
                cases(Option<Type>) maybe-type:
                  | some(typ) =>
                    new-context = add-aliases(typ, ctxt)
                    info = new-context.info
                    fold-result(new-context.set-info(TCS.tc-info(info.types, info.aliases, info.data-types.set(a-field.name, typ))))
                  | none => raise("providing a type that doesn't exist")
                end
              end
            end, provided-types, vals-ctxt)
            for typing-bind(final-ctxt from final-context):
              typing-result(A.s-module(l, new-answer, defined-values, defined-types, provided-values, provided-types, checks), expect-type, final-ctxt)
            end
          end
        end)
    | s-type-let-expr(l, binds, body) =>
      for typing-bind(ctxt from handle-type-let-binds(binds, context)):
        checking(body, expect-loc, expect-type, true, ctxt)
          .map-expr(A.s-type-let-expr(l, binds, _))
      end
    | s-let-expr(l, binds, body) =>
      context-and-rhs-result = fold-typing(synthesis-let-bind, binds, context)
      for typing-bind(context-and-rhs from context-and-rhs-result):
        new-binds = map2(lam(binding, rhs):
          cases(A.LetBind) binding:
            | s-let-bind(let-l, let-b, _) =>
              A.s-let-bind(let-l, let-b, rhs)
            | s-var-bind(let-l, let-b, _) =>
              A.s-var-bind(let-l, let-b, rhs)
          end
        end, binds, context-and-rhs.right)
        checking(body, expect-loc, expect-type, top-level, context-and-rhs.left)
          .map-expr(A.s-let-expr(l, new-binds, _))
      end
    | s-letrec(l, binds, body) =>
      initial-collected = collect-bindings(binds.map(_.b), context)
      fold-collected = for bind(collected from initial-collected):
        foldr-fold-result(lam(binding, collected-bindings):
          initial-type = collected-bindings.get-value(binding.b.id.key())
          if TS.is-t-existential(initial-type):
            cases(Expr) binding.value:
              | s-lam(lam-l, lam-params, lam-args, lam-ann, _, _, _) =>
                arg-collected = collect-bindings(lam-args, context)
                for bind(arg-coll from arg-collected):
                  fold-lam-type = lam-to-type(arg-coll, lam-l, lam-params, lam-args, lam-ann, top-level, context)
                  for bind(lam-type from fold-lam-type):
                    fold-result(collected-bindings.set(binding.b.id.key(), lam-type))
                  end
                end
              | else => fold-result(collected-bindings)
            end
          else:
            fold-result(collected-bindings)
          end
        end, binds, collected)
      end

      for typing-bind(collected from fold-collected):
        new-context = context.add-dict-to-bindings(collected)
        fold-context-and-rhs = fold-typing(lam(binding, ctxt):
          cases(A.LetrecBind) binding:
            | s-letrec-bind(l2, b, value) =>
              check-synthesis(value, collected.get-value(b.id.key()), l2, false, ctxt)
          end
        end, binds, new-context)

        for typing-bind(context-and-rhs from fold-context-and-rhs):
          new-binds = map2(lam(binding, rhs):
            cases(A.LetrecBind) binding:
              | s-letrec-bind(l2, b, _) =>
                A.s-letrec-bind(l2, b, rhs)
            end
          end, binds, context-and-rhs.right)
          checking(body, expect-loc, expect-type, top-level, context-and-rhs.left)
            .map-expr(A.s-letrec(l, new-binds, _))
        end
      end
    | s-hint-exp(l, hints, exp) =>
      raise("checking for s-hint-exp not implemented")
    | s-instantiate(l, expr, params) =>
      check-synthesis(e, expect-type, l, top-level, context)
    | s-block(l, stmts) =>
      fun gen(curr, base):
        pair(link(pair(curr, base.right), base.left), t-top(l))
      end
      paired-stmts = stmts.foldr(gen, pair(empty, expect-type)).left
      result = fold-typing(lam(stmt-type-pair, ctxt):
        checking(stmt-type-pair.left, expect-loc, stmt-type-pair.right, top-level, ctxt)
      end, paired-stmts, context)
      for typing-bind(result-pair from result):
        ctxt = result-pair.left
        typing-result(A.s-block(l, result-pair.right), ctxt.apply(expect-type), ctxt)
      end
    | s-user-block(l, body) =>
      raise("s-user-block should have already been desugared")
    | s-fun(l, name, params, args, ann, doc, body, _check) =>
      raise("s-fun should have already been desugared")
    | s-type(l, name, ann) =>
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
      for typing-bind(id-type from lookup-id(l, id.key(), context)):
        cases(Type) id-type:
          | t-ref(arg-type, _) =>
            checking(value, l, arg-type, top-level, context)
          | else =>
            typing-error([list: C.incorrect-type(tostring(id-type), l, tostring(t-ref(id-type, l)), l)])
        end
      end
    | s-if-pipe(l, branches) =>
      raise("s-if-pipe should have already been desugared")
    | s-if-pipe-else(l, branches, _else) =>
      raise("s-if-pipe-else should have already been desugared")
    | s-if(l, branches) =>
      raise("s-if should have already been desugared")
    | s-if-else(l, branches, _else) =>
      for map-fold-result(branch from branches):
        bool-result = checking(branch.test, branch.l, t-boolean(branch.l), false, context)
        bool-result.fold-bind(lam(new-test, _, new-context):
          body-result = checking(branch.body, expect-loc, expect-type, false, new-context)
          body-result.fold-bind(lam(new-body, _, _):
            fold-result(A.s-if-branch(branch.l, new-test, new-body))
          end)
        end)
      end.typing-bind(lam(new-branches):
        checking(_else, expect-loc, expect-type, false, context)
          .map-expr(A.s-if-else(l, new-branches, _))
      end)
    | s-cases(l, typ, val, branches) =>
      # TODO(MATT): substitution on typ?
      checking-cases(l, typ, val, branches, none, expect-loc, expect-type, context)
    | s-cases-else(l, typ, val, branches, _else) =>
      # TODO(MATT): substitution on typ?
      checking-cases(l, typ, val, branches, some(_else), expect-loc, expect-type, context)
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
      check-fun(l, body, params, args, ann, expect-loc, expect-type, A.s-lam(l, params, _, _, doc, _, _check), context)
    | s-method(l, params, args, ann, doc, body, _check) =>
      raise("checking for s-method not implemented")
    | s-extend(l, supe, fields) =>
      raise("checking for s-extend not implemented")
    | s-update(l, obj, fields) =>
      check-synthesis(e, expect-type, expect-loc, context)
    | s-obj(l, fields) =>
      check-synthesis(e, expect-type, expect-loc, context)
    | s-array(l, values) =>
      wrapped = cases(Type) expect-type:
        | t-app(rarray, args, tl) =>
          if TS.t-array-name == rarray:
            param-type = args.first
            for fold-typing(value from values, ctxt from context):
              checking(value, expect-loc, param-type, false, ctxt)
            end
          else:
            typing-error([list: C.incorrect-type(tostring(TS.t-array-name), l, tostring(expect-type), expect-loc)])
          end
        | t-top(tl) =>
          for fold-typing(value from values, ctxt from context):
            checking(value, expect-loc, t-top(tl), false, ctxt)
          end
        | else =>
          fold-errors([list: C.incorrect-type("a raw array", l, tostring(expect-type), expect-loc)])
      end

      for typing-bind(new-context-and-values from wrapped):
        typing-result(A.s-array(l, new-context-and-values.right), expect-type, new-context-and-values.left)
      end
    | s-construct(l, modifier, constructor, values) =>
      raise("checking for s-construct not implemented")
    | s-app(l, _fun, args) =>
      check-synthesis(e, expect-type, expect-loc, context)
    | s-prim-app(l, _fun, args) =>
      check-synthesis(e, expect-type, expect-loc, context)
    | s-prim-val(l, name) =>
      raise("checking for s-prim-val not implemented")
    | s-id(l, id) =>
      check-synthesis(e, expect-type, l, top-level, context)
    | s-id-var(l, id) =>
      check-synthesis(e, expect-type, l, top-level, context)
    | s-id-letrec(l, id, safe) =>
      check-synthesis(e, expect-type, l, top-level, context)
    | s-undefined(l) =>
      raise("checking for s-undefined not implemented")
    | s-srcloc(l, loc) =>
      check-synthesis(e, expect-type, l, top-level, context)
    | s-num(l, n) =>
      check-synthesis(e, expect-type, l, top-level, context)
    | s-frac(l, num, den) =>
      check-synthesis(e, expect-type, l, top-level, context)
    | s-bool(l, b) =>
      check-synthesis(e, expect-type, l, top-level, context)
    | s-str(l, s) =>
      check-synthesis(e, expect-type, l, top-level, context)
    | s-dot(l, obj, field) =>
      check-synthesis(e, expect-type, l, top-level, context)
    | s-get-bang(l, obj, field) =>
      check-synthesis(e, expect-type, l, top-level, context)
    | s-bracket(l, obj, field) =>
      raise("checking for s-bracket not implemented")
    | s-data(l, name, params, mixins, variants, shared-members, _check) =>
      raise("s-data should have already been desugared")
    | s-data-expr(l, name, namet, params, mixins, variants, shared-members, _check) =>
      check-synthesis(e, expect-type, l, top-level, context)
    | s-for(l, iterator, bindings, ann, body) =>
      raise("s-for should have already been desugared")
    | s-check(l, name, body, keyword-check) =>
      typing-result(e, expect-type, context)
  end
end

fun synthesis(e, top-level, context):
  result = _synthesis(e, top-level, context)
  print("")
  print("synthesis on:")
  each(print, e.tosource().pretty(72))
  print("result:")
  print(result)
  result
end

fun _synthesis(e :: Expr, top-level :: Boolean, context :: Context) -> TypingResult:
  cases(Expr) e:
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
      synthesis-fun(l, body, params, args, ann, A.s-lam(l, params, _, _, doc, _, _check), top-level, context)
    | s-method(l, params, args, ann, doc, body, _check) =>
      raise("synthesis for s-method not implemented")
    | s-extend(l, supe, fields) =>
      raise("synthesis for s-extend not implemented")
    | s-update(l, obj, fields) =>
      raise("synthesis for s-update not implemented")
    | s-obj(l, fields) =>
      raise("synthesis for s-obj not implemented")
    | s-array(l, values) =>
      raise("synthesis for s-array not implemented")
    | s-construct(l, modifier, constructor, values) =>
      raise("synthesis for s-construct not implemented")
    | s-app(l, _fun, args) =>
      raise("synthesis for s-app not implemented")
    | s-prim-app(l, _fun, args) =>
      raise("synthesis for s-prim-app not implemented")
    | s-prim-val(l, name) =>
      raise("synthesis for s-prim-val not implemented")
    | s-id(l, id) =>
      for typing-bind(id-type from lookup-id(l, id.key(), e, context)):
        typing-result(e, id-type, context)
      end
    | s-id-var(l, id) =>
      raise("synthesis for s-id-var not implemented")
    | s-id-letrec(l, id, safe) =>
      for typing-bind(id-type from lookup-id(l, id.key(), e, context)):
        typing-result(e, id-type, context)
      end
    | s-undefined(l) =>
      raise("synthesis for s-undefined not implemented")
    | s-srcloc(l, loc) =>
      raise("synthesis for s-srcloc not implemented")
    | s-num(l, n) =>
      raise("synthesis for s-num not implemented")
    | s-frac(l, num, den) =>
      raise("synthesis for s-frac not implemented")
    | s-bool(l, b) =>
      raise("synthesis for s-bool not implemented")
    | s-str(l, s) =>
      raise("synthesis for s-str not implemented")
    | s-dot(l, obj, field) =>
      synthesis(obj, top-level, context).bind(lam(new-ast, new-type, new-context):
        synthesis-field(l, new-ast, new-type.l, new-type, field, A.s-dot, new-context)
      end)
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
  end
end

fun to-type(in-ann :: A.Ann, context :: Context) -> FoldResult<Option<Type>>:
  cases(A.Ann) in-ann:
    | a-blank =>
      fold-result(none)
    | a-any => # TODO(MATT): a-any have a src-loc
      fold-result(some(t-top(A.dummy-loc)))
    | a-name(l, id) =>
      # cases(Option<Type>) context.aliases.get-now(id.key()):
      #   | some(typ) => fold-result(some(typ.set-loc(l)))
      #   | none => fold-result(some(t-name(none, id, l)))
      # end
      fold-result(some(t-name(none, id, l))) # TODO(MATT): does this work for name's from other modules?
    | a-type-var(l, id) =>
      fold-result(some(t-var(id, l)))
    | a-arrow(l, args, ret, _) =>
      fold-arg-typs = foldr-fold-result(lam(arg, arg-typs):
        for bind(maybe-new-typ from to-type(arg, context)):
          cases(Option<Type>) maybe-new-typ:
            | none =>
              fold-errors([list: C.cant-typecheck("issue with arrow annotation", l)])
            | some(new-typ) =>
              fold-result(link(new-typ, arg-typs))
          end
        end
      end, args, empty)

      for bind(arg-typs from fold-arg-typs):
        for bind(maybe-ret-typ from to-type(ret, context)):
          cases(Option<Type>) maybe-ret-typ:
            | none =>
              fold-errors([list: C.cant-typecheck("issue with arrow annotation", l)])
            | some(ret-typ) =>
              fold-result(t-arrow(arg-typs, ret-typ, l))
          end
        end
      end
    | a-method(l, args, ret) =>
      fold-errors([list: C.cant-typecheck("a-method not yet implemented", l)])
    | a-record(l, fields) =>
      fields-result = foldr-fold-result(lam(field, field-typs):
        for bind(maybe-typ from to-type(field.ann, context)):
          cases(Option<Type>) maybe-typ:
            | none =>
              fold-errors([list: C.cant-typecheck("issue with record annotation", l)])
            | some(typ) =>
              fold-result(link(t-member(field.name, typ, l), field-typs))
          end
        end
      end, fields, empty)

      for bind(members from fields-result):
        fold-result(t-record(members, l))
      end
    | a-app(l, ann, args) =>
      for bind(maybe-typ from to-type(ann, context)):
        cases(Option<Type>) maybe-typ:
          | none =>
            fold-errors([list: C.cant-typecheck("issue with app annotation", l)])
          | some(typ) =>
            for bind(maybe-arg-typs from map-fold-result(lam(arg): to-type(arg, context) end, args)):
              fold-arg-typs = foldr-fold-result(lam(maybe-arg-typ, arg-list):
                cases(Option<Type>) maybe-arg-typ:
                  | none =>
                    fold-errors([list: C.cant-typecheck("issue with app annotation arguments", l)])
                  | some(arg-typ) =>
                    fold-result(link(arg-typ, arg-list))
                end
              end, maybe-arg-typs, empty)
              for bind(arg-typs from fold-arg-typs):
                t-app(typ, arg-typs, l)
              end
            end
        end
      end
    | a-pred(l, ann, exp) =>
      for bind(maybe-typ from to-type(ann, context)):
        cases(Option<Type>) maybe-typ:
          | some(typ) =>
            expect-type = t-arrow([list: typ], t-boolean(l), l)
            checking(exp, l, expect-type, false, context).fold-bind(lam(_, _, _):
              fold-result(some(typ))
            end)
          | none =>
            fold-errors([list: C.cant-typecheck("issue with pred annotation", l)])
        end
      end
    | a-dot(l, obj, field) =>
      key = obj.key()
      cases(Option<String>) context.mod-names.get-now(key):
        | none =>
          fold-errors([list: C.no-module(l, obj.toname())])
        | some(mod) =>
          t-mod = context.modules.get-value-now(mod)
          if t-mod.types.has-key(field):
            # TODO(MATT): s-global?
            fold-result(some(t-name(some(mod), A.s-global(field), l)))
          else if t-mod.aliases.has-key(field):
            # TODO(MATT): fully resolve alias?
            fold-result(some(t-mod.aliases.get-value(field)))
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
      fold-result(context)
    else:
      shadow subtype = resolve-alias(subtype, context)
      shadow supertype = resolve-alias(supertype, context)
      cases(Type) supertype:
        | t-existential(b-id, _) =>
          cases(Type) subtype:
            | t-existential(a-id, _) =>
              if a-id == b-id:
                fold-result(context)
              else:
                some(instantiate-left(subtype, supertype, context))
              end
            | else => some(instantiate-right(subtype, supertype, context))
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
                    for option-bind(ctxt from maybe-context):
                      satisfies-assuming(ctxt.apply(b-arg), ctxt.apply(a-arg), ctxt, assumptions)
                    end
                  end, some(context), b-args, a-args)
                  for option-bind(outer from result):
                    for option-bind(ctxt from outer):
                      satisfies-assuming(ctxt.apply(a-ret), ctxt.apply(b-ret), ctxt, assumptions)
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
                  for option-bind(ctxt from satisfies-assuming(a-typ, b-typ, context, assumptions)):
                    satisfies-assuming(ctxt.apply(b-typ), ctxt.apply(a-typ), ctxt, assumptions)
                  end
                | else => none
              end
            | t-existential(a-id, _) =>
              some(instantiate-left(subtype, supertype, context))
            | t-data(a-name, a-variants, a-fields, _) =>
              cases(Type) supertype:
                | t-data(b-name, b-variants, b-fields, _) =>
                  if a-name == b-name:
                    a-variants.foldr(lam(a-variant, maybe-context):
                      for option-bind(ctxt from maybe-context):
                        b-variant = supertype.lookup-variant(a-variant.name).value
                        cases(TypeVariant) a-variant:
                          | t-singleton-variant(_, _, _) =>
                            some(ctxt)
                          | t-variant(_, a-var-fields, _, _) =>
                            b-var-fields = b-variant.fields
                            satisfies-fields(a-var-fields, b-var-fields, context, assumptions.add(pair(subtype, supertype)))
                        end
                      end
                    end, some(context))
                  else:
                    none
                  end
                | t-app(b-onto, b-args, _) =>
                  for option-bind(data-type from context.get-data-type(b-onto)):
                    satisfies-assuming(subtype, data-type.introduce(b-args), context, assumptions)
                  end
                | t-record(b-fields, _) =>
                  satisfies-fields(a-fields, b-fields, context, assumptions)
                | else => none
              end
          end
      end
    end
  end

  fun satisfies-fields(a-fields :: List<TypeMember>, b-fields :: List<TypeMember>, _context :: Context, assumptions :: Set<Pair<Type, Type>>) -> Option<Context>:
    b-fields.foldr(lam(b-field, maybe-context):
      for option-bind(ctxt from maybe-context):
        for option-bind(a-field from a-fields.find(lam(a-field): a-field.field-name == b-field.field-name end)):
          satisfies-assuming(a-field.typ, b-field.typ, ctxt, assumptions)
        end
      end
    end, some(_context))
  end

  fun satisfies-assuming(_subtype, _supertype, _context, assumptions):
    result = _satisfies-assuming(_subtype, _supertype, _context, assumptions)
    print("")
    print(tostring(_subtype) + " <: " + tostring(_supertype))
    print("result:")
    print(result)
    result
  end
  satisfies-assuming(subtype, supertype, context, [list-set: ])
end

fun instantiate-right(subtype, supertype, context):
  result = _instantiate-right(subtype, supertype, context)
  print("")
  print(tostring(subtype) + " <=: " + tostring(supertype))
  print("result:")
  print(result)
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
          new-context = context.assign-existential(supertype, t-arrow(arg-existentials, ret-existential, b-l))
          arg-context = args-and-existentials.foldr(lam(arg-and-exists, ctxt):
            instantiate-left(arg-and-exists.right, arg-and-exists.left, ctxt)
          end, new-context)
          instantiate-right(arg-context.apply(a-ret), ret-existential, arg-context)
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
        | t-data(variants, fields, _) =>
          context.assign-existential(supertype, subtype)
      end
    | else => raise("cannot instantiate non-existential")
  end
end

fun instantiate-left(subtype :: Type, supertype :: Type, context :: Context) -> Context:
  result = _instantiate-left(subtype, supertype, context)
  print("")
  print(tostring(subtype) + " :=< " + tostring(supertype))
  print("result:")
  print(result)
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
          new-context = context.assign-existential(subtype, t-arrow(arg-existentials, ret-existential, a-l))
          arg-context = args-and-existentials.foldr(lam(arg-and-exists, ctxt):
            instantiate-right(arg-and-exists.left, arg-and-exists.right, ctxt)
          end, new-context)
          instantiate-left(ret-existential, arg-context.apply(b-ret), arg-context)
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
        | t-data(variants, fields, _) =>
          context.assign-existential(subtype, supertype)
      end
    | else => raise("cannot instantiate non-existential")
  end
end

fun least-upper-bound(s, t, loc, context):
  result-type = _least-upper-bound(s, t, loc, context)
  print("")
  print(tostring(s) + " V " + tostring(t) + " = " + tostring(result-type))
  result-type
end

fun _least-upper-bound(s :: Type, t :: Type, loc :: Loc, context :: Context) -> Type:
  cases(Option<Context>) satisfies-type(s, t, context):
    | some(_) => t.set-loc(loc)
    | none =>
      cases(Option<Context>) satisfies-type(t, s, context):
        | some(_) => s.set-loc(loc)
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
              least-upper-bound(s-onto.introduce(s-args), t, loc, context)
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
  print("")
  print(tostring(s) + " V " + tostring(t) + " = " + tostring(result-type))
  result-type
end

fun _greatest-lower-bound(s :: Type, t :: Type, loc :: Loc, context :: Context) -> Type:
  cases(Option<Context>) satisfies-type(s, t, context):
    | some(_) => s.set-loc(loc)
    | none =>
      cases(Option<Context>) satisfies-type(t, s, context):
        | some(_) => t.set-loc(loc)
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
              greatest-lower-bound(s-onto.introduce(s-args), t, loc, context)
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
        cases(Type) lub:
          | t-top(_) => raise("pause here to consider")
          | else => link(t-member(field-name, lub, loc), meet-list)
        end
      | none => meet-list
    end
  end, empty)
end

fun join-fields(a-fields :: List<TypeMember>, b-fields :: List<TypeMember>, loc :: A.LOc, context :: Context) -> List<TypeMember>:
  initial-members = a-fields.foldr(lam(a-field, join-list):
    field-name = a-field.field-name
    cases(Option<TypeMember>) type-members-lookup(b-fields, field-name):
      | some(b-field) =>
        glb = greatest-lower-bound(a-field.typ, b-field.typ, loc, context)
        cases(Type) glb:
          | t-bot(_) => raise("also pause here to consider")
          | else => link(t-member(field-name, glb, loc), join-list)
        end
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

fun handle-type-let-binds(bindings :: List<A.TypeLetBind>, context) -> FoldResult<Context>:
  foldr-fold-result(lam(binding, ctxt):
    cases(A.TypeLetBind) binding:
      | s-type-bind(_, name, ann) =>
        for bind(maybe-typ from to-type(ann, ctxt)):
          cases(Option<Type>) maybe-typ:
            | none => # TODO(MATT): is this correct?
              fold-errors([list: C.unbound-type-id(ann)])
            | some(typ) =>
              ctxt.aliases.set-now(name.key(), typ)
              fold-result(ctxt)
          end
        end
      | s-newtype-bind(l, name, namet) =>
        typ = t-name(none, namet, l)
        namet-key = namet.key()
        ctxt.aliases.set-now(name.key(), typ)
        new-context = ctxt.add-binding(namet-key, t-record([list:
          t-member("test", t-arrow([list: t-top(l)], t-boolean(l), l), l),
          t-member("brand", t-arrow([list: t-top(l)], typ, l), l)
        ], l))
        fold-result(new-context)
    end
  end, bindings, context)
end

fun collect-bindings(binds :: List<A.Bind>, context :: Context) -> FoldResult<SD.StringDict<Type>>:
  foldr-fold-result(lam(binding, dict):
    for bind(maybe-type from to-type(binding.ann, context)):
      new-type = cases(Option<Type>) maybe-type:
        | some(typ) => typ.set-loc(binding.l)
        | none => new-existential(binding.l)
      end
      fold-result(dict.set(binding.id.key(), new-type))
    end
  end, binds, SD.make-string-dict())
end

fun check-synthesis(e :: Expr, expect-type :: Type, expect-loc :: Loc, top-level :: Boolean, context :: Context) -> TypingResult:
  synthesis(e, top-level, context).bind(lam(new-expr, new-type, new-context):
    applied-new-type = new-context.apply(new-type)
    applied-expect-type = new-context.apply(expect-type)
    maybe-context = satisfies-type(applied-new-type, applied-expect-type, new-context)
    cases(Option<Context>) maybe-context:
      | some(ctxt) =>
        typing-result(new-expr, ctxt.apply(applied-expect-type), ctxt)
      | none =>
        # TODO(MATT): different error for existential
        typing-error([list: C.incorrect-type(tostring(applied-new-type), applied-new-type.l, tostring(applied-expect-type), expect-loc)])
    end
  end)
end

fun synthesis-datatype(l :: Loc, name :: String, namet :: A.Name, params :: List<A.Name>, mixins, variants :: List<A.Variant>, fields :: List<A.Member>, _check, context :: Context) -> TypingResult:
  brander-type = t-name(none, namet, l)
  t-vars = params.map(t-var(_, l))

  for typing-bind(variants-result from map-fold-result(to-type-variant(_, context), variants)):
    split-variants = split(variants-result)
    new-variants = split-variants.left
    variant-types = split-variants.right

    for typing-bind(fields-result from map-fold-result(to-type-member(_, context), fields)):
      split-fields = split(fields-result)

      variant-type-fields = variant-types.map(lam(var-type):
        var-type.fields.append(var-type.with-fields)
      end)
      variants-meet = cases(List<TypeMember>) variant-type-fields:
        | empty => empty
        | link(first, rest) =>
          cases(List<TypeMember>) rest:
            | empty => first
            | link(_, _) =>
              rest.foldr(meet-fields(_, _, context), first)
          end
      end

      data-type-fields = split-fields.right.append(variants-meet)

      new-data-expr = A.s-data-expr(l, name, namet, params, mixins, new-variants, split-fields.left, _check)

      temp-data-type = t-data(name, variant-types, data-type-fields, l)
      new-data-type =
        if is-empty(t-vars):
          temp-data-type
        else:
          t-forall(t-vars, temp-data-type, l)
        end
      context.data-types.set-now(namet.key(), new-data-type)

      data-fields = link(t-member(name, t-arrow([list: t-top(l)], t-boolean(l), l), l),
        for map(variant-type from variant-types):
          t-member(variant-type.name, mk-constructor-type(variant-type, brander-type, t-vars), l)
        end +
        for map(variant-type from variant-types):
          t-member("is-" + variant-type.name, t-arrow([list: t-top(l)], t-boolean(l), l), l)
        end)

      data-expr-type = t-record(data-fields, l)
      typing-result(new-data-expr, data-expr-type, context)
    end
  end
end

fun to-type-variant(variant :: A.Variant, context :: Context) -> FoldResult<Pair<A.Variant, TypeVariant>>:
  cases(A.Variant) variant:
    | s-variant(l, constr-loc, name, members, with-members) =>
      for bind(result from map-fold-result(to-type-member(_, context), with-members)):
        split-result = split(result)
        new-with-members = split-result.left
        with-type-members = split-result.right

        fun process-member(member):
          wrap = cases(A.VariantMemberType) member.member-type:
            | s-normal => lam(x): x end
            | s-mutable => t-ref(_, member.l)
          end
          for bind(maybe-typ from to-type(member.bind.ann, context)):
            cases(Option<Type>) maybe-typ:
              | none => fold-errors([list: C.cant-typecheck("No type annotation provided on member", l)])
              | some(typ) => fold-result(t-member(member.bind.id.toname(), wrap(typ), l))
            end
          end
        end

        for bind(type-members from map-fold-result(process-member, members)):
          new-variant = A.s-variant(l, constr-loc, name, members, new-with-members)
          type-variant = t-variant(name, type-members, with-type-members, l)
          fold-result(pair(new-variant, type-variant))
        end
      end
    | s-singleton-variant(l, name, with-members) =>
      for bind(result from map-fold-result(to-type-member(_, context), with-members)):
        split-result = split(result)
        new-with-members = split-result.left
        with-type-members = split-result.right
        new-variant = A.s-singleton-variant(l, name, new-with-members)
        type-variant = t-singleton-variant(name, with-type-members, l)
        fold-result(pair(new-variant, type-variant))
      end
  end
end

fun to-type-member(field :: A.Member, context :: Context) -> FoldResult<Pair<A.Member, TypeMember>>:
  cases(A.Member) field:
    | s-data-field(l, name, value) =>
      if A.is-s-method(value): # TODO(cody): Type-check methods.
        raise("to-type member on method not done yet")
      else:
        synthesis(value, context).fold-bind(
        lam(new-value, value-loc, value-typ, out-context):
          fold-result(pair(A.s-data-field(l, name, new-value), t-member(name, value-typ, l)))
        end)
      end
    | s-mutable-field(l, name, ann, value) =>
      raise("to-type member for s-mutable-field")
    | s-method(l, name, args, ann, doc, body, _check) =>
      raise("to-type member for s-method")
  end
end

fun mk-constructor-type(variant-typ :: TypeVariant, brander-typ :: Type, params :: List<Type>) -> Type:
  cases(TypeVariant) variant-typ:
    | t-variant(name, fields, _, l) =>
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
    | t-singleton-variant(name, _, l) =>
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
    | t-data(variants, fields, _) =>
      handle(obj-type-loc, some(fields))
    | t-forall(introduces, onto, _) =>
      raise("record view of forall")
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
    fold-result(context.binds.get-value(id-key))
  else if context.types.has-key-now(id-key):
    fold-result(context.types.get-value-now(id-key))
  else:
    fold-errors([list: C.unbound-id(id-expr)])
  end
end

fun lam-to-type(coll :: SD.StringDict<Type>, l :: Loc, params :: List<A.Name>, args :: List<A.Bind>, ret-ann :: A.Ann, top-level :: Boolean, context :: Context) -> FoldResult<Type>:
  for bind(maybe-type from to-type(ret-ann, context)):
    ret-type = cases(Option<Type>) maybe-type:
      | some(typ) => typ
      | none => new-existential(l)
    end
    fold-arg-types = foldr-fold-result(lam(arg, lst):
      arg-type = coll.get-value(arg.id.key())
      if top-level and is-t-existential(arg-type):
        fold-errors([list: C.cant-typecheck("arguments on functions at the top-level must be annotated", l)]) # TODO(MATT): much better error message
      else:
        fold-result(link(arg-type, lst))
      end
    end, args, empty)

    for bind(arg-types from fold-arg-types):
      arrow-type = t-arrow(arg-types, ret-type, l)
      lam-type =
        if is-empty(params):
          arrow-type
        else:
          forall = for map(param from params): t-var(param, l) end
          t-forall(forall, arrow-type, l)
        end
      fold-result(lam-type)
    end
  end
end

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
  for typing-bind(coll from collected):
    fold-lam-type = lam-to-type(coll, l, params, args, ret-ann, top-level, context)
    for typing-bind(lam-type from fold-lam-type):
      fold-ret-type = cases(Type) lam-type:
        | t-arrow(_, ret-type, _) =>
          fold-result(ret-type)
        | t-forall(_, onto, _) =>
          cases(Type) onto:
            | t-arrow(_, ret-type, _) =>
              fold-result(ret-type)
            | else => raise("This shouldn't happen (non-function type lambda)")
          end
        | else => raise("This shouldn't happen (non-function type lambda)")
      end
      for typing-bind(ret-type from fold-ret-type):
        checking(body, l, ret-type, false, context.add-dict-to-bindings(coll))
          .bind(lam(new-body, new-ret-type, out-context):
            typing-result(recreate(args, ret-ann, new-body),
                          set-ret-type(lam-type, new-ret-type),
                          out-context)
          end)
      end
    end
  end
end

fun gather-provides(_provide :: A.Provide, context :: Context) -> FoldResult<TCInfo>:
  cases(A.Provide) _provide:
    | s-provide-complete(_, values, aliases, data-definitions) =>
      fold-values-info = foldr-fold-result(lam(value, info):
        value-key = value.v.key()
        if info.types.has-key(value-key):
          fold-result(info)
        else:
          typ = context.binds.get-value(value-key)
          fold-result(TCS.tc-info(info.types.set(value-key, typ), info.aliases, info.data-types))
        end
      end, values, context.info)
      for bind(values-info from fold-values-info):
        fold-aliases-info = foldr-fold-result(lam(_alias, info):
          alias-key = _alias.in-name.key()
          if info.aliases.has-key(alias-key):
            fold-result(info)
          else:
            typ = context.aliases.get-value-now(alias-key)
            fold-result(TCS.tc-info(info.types, info.aliases.set(alias-key, typ), info.data-types))
          end
        end, aliases, values-info)
        for bind(aliases-info from fold-aliases-info):
          foldr-fold-result(lam(data-type, info):
            data-key = data-type.d.key()
            if info.data-types.has-key(data-key):
              fold-result(info)
            else:
              typ = context.data-types.get-value-now(data-key)
              fold-result(TCS.tc-info(info.types, info.aliases, info.data-types.set(data-key, typ)))
            end
          end, data-definitions, aliases-info)
        end
      end
    | else => raise("Haven't handled anything but s-provide-complete")
  end
end

fun synthesis-let-bind(binding :: A.LetBind, context :: Context) -> TypingResult:
  cases(A.LetBind) binding:
    | s-let-bind(l, b, value) =>
      for typing-bind(maybe-type from to-type(b.ann, context)):
        ann-type = cases(Option<Type>) maybe-type:
          | some(typ) => typ
          | none => new-existential(l)
        end
        check-synthesis(value, ann-type, l, false, context)
          .bind(lam(new-value, new-type, new-context):
            typing-result(new-value, new-type, new-context.add-binding(b.id.key(), new-type))
          end)
      end
  end
end

fun checking-cases(l :: Loc, ann :: A.Ann, val :: Expr, branches :: List<A.CasesBranch>, maybe-else :: Option<Expr>, expect-loc :: Loc, expect-type :: Type, context :: Context) -> TypingResult:
  handle-cases(l, ann, val, branches, maybe-else, expect-loc, some(expect-type), context, checking-cases-has-else(expect-loc, expect-type), checking-cases-no-else(expect-type))
end

fun checking-cases-has-else(expect-loc :: Loc, expect-type :: Type):
  lam(l :: Loc, ann :: A.Ann, new-val :: Expr, split-result :: Pair<List<A.CasesBranch>, List<Type>>, _else :: Expr, context :: Context) -> TypingResult:
    checking(_else, expect-loc, expect-type, context).bind(lam(new-else, new-type, out-context):
      new-cases = A.s-cases-else(l, ann, new-val, split-result.left, new-else)
      typing-result(new-cases, new-type, out-context)
    end)
  end
end

fun checking-cases-no-else(expect-type :: Type):
  lam(l :: Loc, ann :: A.Ann, new-val :: Expr, split-result :: Pair<List<A.CasesBranch>, List<Type>>, context :: Context) -> TypingResult:
    new-cases = A.s-cases(l, ann, new-val, split-result.left)
    typing-result(new-cases, expect-type, context)
  end
end

fun handle-cases(l :: Loc, ann :: A.Ann, val :: Expr, branches :: List<A.CasesBranch>, maybe-else :: Option<Expr>, expect-loc :: Loc, maybe-expect :: Option<Type>, context :: Context, has-else, no-else) -> TypingResult:
  for typing-bind(maybe-type from to-type(ann, context)):
    cases(Option<Type>) maybe-type:
      | some(typ) =>
        # TODO(MATT): applied types and non-applied types
        cases(Option<Type>) context.get-data-type(typ):
          | some(data-type) =>
            checking(val, l, typ, false, context).bind(lam(new-val, _, new-context):
              branch-tracker = track-branches(data-type)

              temp-result = foldr-fold-result(lam(branch, branch-types-and-context):
                branch-result = handle-branch(data-type, l, branch, expect-loc, maybe-expect, branch-tracker.remove, branch-types-and-context.right)
                for bind(branch-type-and-new-context from branch-result):
                  branch-type-pair = branch-type-and-new-context.right
                  new-ctxt = branch-type-and-new-context.right
                  fold-result(pair(link(branch-type-pair, branch-types-and-context.left), new-ctxt))
                end
              end, fold-result(pair(empty, new-context)))

              for typing-bind(result from temp-result):
                split-result = split(result.left)
                ctxt = result.right
                remaining-branches = branch-tracker.get().to-list()
                cases(Option<A.Expr>) maybe-else:
                  | some(_else) =>
                    if is-empty(remaining-branches):
                      typing-error([list: C.unneccesary-else-branch(tostring(typ), l)])
                    else:
                      has-else(l, ann, new-val, split-result, _else, ctxt)
                    end
                  | none =>
                    if is-empty(remaining-branches):
                      no-else(l, ann, new-val, split-result, ctxt)
                    else:
                      # TODO(MATT): more appropriate error here
                      typing-error([list: C.non-exhaustive-pattern(remaining-branches, tostring(typ), l)])
                    end
                end
              end
            end)
          | none =>
            # TODO(MATT): this is bad and encompasses too much
            typing-error([list: C.cant-match-on(tostring(typ), l)])
        end
      | none => typing-error([list: C.cant-typecheck("No type provided for cases", l)])
    end
  end
end

fun track-branches(data-type :: Type % (is-t-data)) ->
  { remove :: (String -> Set<String>), get :: (-> Set<String>) }:
  var unhandled-branches = data-type.variants.foldr(lam(b, s): s.add(b.name) end, [set:])
  {
    remove: lam(b-name :: String):
      unhandled-branches := unhandled-branches.remove(b-name)
    end,
    get: lam() -> Set<String>:
      unhandled-branches
    end
  }
end

fun handle-branch(data-type :: Type % (is-t-data), cases-loc :: A.Loc, branch :: A.CasesBranch, expect-loc :: A.Loc, maybe-check :: Option<Type>, remove :: (String -> Any), context :: Context) -> FoldResult<Pair<Pair<A.CasesBranch, Type>>, Context>:
  fun handle-body(name :: String, body :: A.Expr, process, body-context :: Context):
    remove(name)
    cases(Option<Type>) maybe-check:
      | some(expect-type) =>
        checking(body, expect-loc, expect-type, false, body-context)
          .fold-bind(process)
      | none =>
        synthesis(body, false, body-context)
          .fold-bind(process)
    end
  end

  cases(Option<TypeVariant>) data-type.lookup-variant(branch.name):
    | some(tv) =>
      cases(TypeVariant) tv:
        | t-variant(_, fields, _, _) =>
          cases(A.CasesBranch) branch:
            | s-cases-branch(l, pat-loc, name, args, body) =>
              fun process(new-body, typ, out-context):
                new-branch = A.s-cases-branch(l, pat-loc, name, args, new-body)
                fold-result(pair(pair(new-branch, typ), out-context))
              end

              body-context = fold2-strict(lam(fold-ctxt, arg, arg-typ):
                for bind(ctxt from fold-ctxt):
                  for bind(maybe-type from to-type(arg.bind.ann, ctxt)):
                    cases(Option<Type>) maybe-type:
                      | some(typ) =>
                        cases(Option<Context>) satisfies-type(arg-typ, typ, ctxt):
                          | some(new-ctxt) =>
                            fold-result(new-ctxt.add-binding(arg.bind.id.key(), typ))
                          | none =>
                            # TODO(MATT): error message
                            raise("Branch does not satisfy type")
                        end
                      | none =>
                        fold-result(ctxt.add-binding(arg.bind.id.key(), arg-typ))
                    end
                  end
                end
              end, fold-result(context), args, fields.map(_.typ))

              cases(Option<FoldResult<Context>>) body-context:
                | none =>
                  fold-errors([list: C.incorrect-number-of-bindings(name, l, args.length(), fields.length())])
                | some(folded-context) =>
                  for bind(body-ctxt from folded-context):
                    handle-body(name, body, process, body-ctxt)
                  end
              end
            | s-singleton-cases-branch(l, _, name, _) =>
              fold-errors([list: C.cases-singleton-mismatch(name, l, false)])
          end
        | t-singleton-variant(_, _, _) =>
          cases(A.CasesBranch) branch:
            | s-cases-branch(l, _, name, _, _) =>
              fold-errors([list: C.cases-singleton-mismatch(name, l, true)])
            | s-singleton-cases-branch(l, pat-loc, name, body) =>
              fun process(new-body, typ, out-context):
                new-branch = A.s-singleton-cases-branch(l, pat-loc, name, new-body)
                fold-result(pair(pair(new-branch, typ), out-context))
              end
              handle-body(name, body, process, context)
          end
      end
    | none =>
      fold-errors([list: C.unneccesary-branch(branch.name, branch.l, tostring(data-type), cases-loc)])
  end
end

fun check-fun(fun-loc :: Loc, body :: Expr, params :: List<A.Name>, args :: List<A.Bind>, ret-ann :: A.Ann, expect-loc :: A.Loc, expect-type :: Type, recreate :: (List<A.Bind>, A.Ann, Expr -> Expr), context :: Context) -> TypingResult:
  lam-bindings = collect-bindings(args, context)
  # TODO(MATT): checking when polymorphic lambda but non-polymorphic type

  cases(Type) expect-type:
    | t-arrow(expect-args, ret-type, _) =>
      for typing-bind(lam-binds from lam-bindings):
        lam-arg-types = map(lam(arg): lam-binds.get-value(arg.id.key()) end, args)
        maybe-context = fold2-strict(lam(fold-context, arg, expect-arg):
          for bind(ctxt from fold-context):
            cases(Option<Context>) satisfies-type(expect-arg, arg, ctxt):
              | some(new-context) => fold-result(new-context)
              | none =>
                # TODO(MATT): error message
                raise("check-fun satisfies-type error")
            end
          end
        end, fold-result(context.add-dict-to-bindings(lam-binds)), lam-arg-types, expect-args)
        cases(Option<FoldResult<Context>>) maybe-context:
          | none =>
            expected = "a function with " + tostring(expect-args.length()) + " arguments"
            found = "a function with " + tostring(args.length()) + " arguments"
            typing-error([list: C.incorrect-type(expected, fun-loc, found, expect-loc)])
          | some(fold-ctxt) =>
            for typing-bind(ctxt from fold-ctxt):
              body-result = checking(body, expect-loc, ret-type, false, ctxt)
              body-result.bind(lam(new-body, new-type, out-context):
                typing-result(recreate(args, ret-ann, new-body), new-type, out-context)
              end)
            end
        end
      end
    | t-top(_) =>
      for typing-bind(new-binds from lam-bindings):
        body-result = checking(body, expect-loc, expect-type, false, context.add-dict-to-bindings(new-binds))
        body-result.bind(lam(new-body, new-type, out-context):
          typing-result(recreate(args, ret-ann, new-body), new-type, out-context)
        end)
      end
    | t-forall(introduces, onto, _) =>
      # TODO(MATT): determine if this works
      checking(recreate(args, ret-ann, body), fun-loc, onto, false, context)
    | t-existential(id, _) =>
      check-synthesis(recreate(args, ret-ann, body), expect-type, expect-loc, false, context)
    | else =>
      typing-error([list: C.incorrect-type("a function", fun-loc, tostring(expect-type), expect-loc)])
  end
end
