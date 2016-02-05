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
t-existential             = TS.t-existential

is-t-record               = TS.is-t-record
is-t-name                 = TS.is-t-name
is-t-data                 = TS.is-t-data

type TypeMember           = TS.TypeMember
t-member                  = TS.t-member

type-members-lookup       = TS.type-members-lookup

type TypeVariant          = TS.TypeVariant
t-variant                 = TS.t-variant
t-singleton-variant       = TS.t-singleton-variant

type ModuleType           = TS.ModuleType
t-module                  = TS.t-module

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

resolve-alias             = TCS.resolve-alias

all2-strict               = LA.all2-strict
map2-strict               = LA.map2-strict

t-num-binop = t-arrow([list: t-number(A.dummy-loc), t-number(A.dummy-loc)], t-number(A.dummy-loc), A.dummy-loc)
t-num-cmp   = t-arrow([list: t-number(A.dummy-loc), t-number(A.dummy-loc)], t-boolean(A.dummy-loc), A.dummy-loc)
t-str-binop = t-arrow([list: t-string(A.dummy-loc), t-string(A.dummy-loc)], t-string(A.dummy-loc), A.dummy-loc)
t-str-cmp   = t-arrow([list: t-string(A.dummy-loc), t-string(A.dummy-loc)], t-boolean(A.dummy-loc), A.dummy-loc)
t-method-binop = lam(field-name :: String):
  t-forall(
    [list:
      t-var(A.s-atom("B", 1), A.dummy-loc),
      t-var(A.s-atom("C", 1), A.dummy-loc)
    ],
    t-arrow(
      [list:
        t-record([list:
          t-member(field-name, t-arrow([list: t-var(A.s-atom("B", 1), A.dummy-loc)], t-var(A.s-atom("C", 1), A.dummy-loc), A.dummy-loc), A.dummy-loc)
        ], A.dummy-loc),
        t-var(A.s-atom("B",1), A.dummy-loc)
      ],
      t-var(A.s-atom("C", 1), A.dummy-loc),
      A.dummy-loc
    ),
    A.dummy-loc
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

fun split<X, Y>(ps :: List<Pair<X,Y>>) -> Pair<List<X>, List<Y>>:
  fun step(p, curr):
    pair(link(p.left, curr.left), link(p.right, curr.right))
  end
  ps.foldr(step, pair(empty, empty))
end

fun new-existential(l :: A.Loc):
  t-existential(A.global-names.make-atom("exists"), l)
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
        for map(v from mod.values.keys-list()): TS.t-member(v, mod.values.get-value(v), program.l) end,
        program.l
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
                for map(v from mod.values.keys-list()): TS.t-member(v, mod.values.get-value(v), l) end,
              l)
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
            info.aliases.set-now(tname.key(), TS.t-top(l))
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
      #each(print, body.tosource().pretty(72))

      tc-result = checking(body, A.dummy-loc, t-top(A.dummy-loc), typing-context([list: ], info))
      cases(CheckingResult) tc-result:
        | checking-result(new-body, context) =>
          #each(print, new-body.tosource().pretty(72))
          C.ok(TCS.typed(A.s-program(l, _provide, provided-types, imports, new-body), context.info))
        | checking-err(err-list) =>
          C.err(err-list)
      end
    | else => raise("Attempt to type-check non-program: " + torepr(program))
  end
end

fun checking(e, expect-loc, expect-typ, context):
  result = _checking(e, expect-loc, expect-typ, context)
  #print("")
  #print("checking:")
  #each(print, e.tosource().pretty(72))
  #print("has type: " + tostring(expect-typ))
  #print("result:")
  #print(result)
  result
end

fun _checking(e :: A.Expr, expect-loc :: A.Loc, expect-typ :: Type, context :: Context) -> CheckingResult:
  cases(A.Expr) e:
    | s-module(l, answer, defined-values, defined-types, provided-values, provided-types, checks) =>
      checking(answer, expect-loc, expect-typ, context)
        # TODO(MATT): error on underdetermined here?
        .bind(lam(new-answer, out-context):
          values-context = defined-values.foldr(lam(defined-value, fold-context):
            for bind(ctxt from fold-context):
              synthesis(defined-value.value, ctxt).fold-bind(lam(ast, _, typ, new-context):
                new-context.info.typs.set-now(ast.id.key(), typ)
                fold-result(new-context)
              end)
            end
          end, fold-result(out-context))

          for check-bind(result-context from values-context):
            checking-result(A.s-module(l, new-answer, defined-values, defined-types, provided-values, provided-types, checks), result-context)
          end
        end)
    | s-type-let-expr(l, binds, body) =>
      for check-bind(ctxt from handle-type-let-binds(binds, context)):
        checking(body, expect-loc, expect-typ, ctxt)
          .map(A.s-type-let-expr(l, binds, _))
      end
    | s-let-expr(l, binds, body) =>
      binds-result = fold-synthesis(synthesis-let-bind, binds, context)
      binds-result.check-bind(lam(result-pair):
        checking(body, expect-loc, expect-typ, result-pair.left)
          .map(A.s-let-expr(l, result-pair.right, _))
        end)
    | s-letrec(l, binds, body) =>
      initial-collected = collect-bindings(binds.map(lam(binding): binding.b end), context)
      collected = for bind(coll from initial-collected):
        binds.foldr(lam(binding, collected-bindings):
          for bind(coll-bindings from collected-bindings):
            cases(A.Expr) binding.value:
              | s-lam(lam-l, lam-params, lam-args, lam-ann, _, _, _) =>
                arg-collection = collect-bindings(lam-args, context)
                for bind(arg-coll from arg-collection):
                  for bind(maybe-typ from to-type(lam-ann, context)):
                    ret-typ = cases(Option<Type>) maybe-typ:
                      | some(typ) => typ
                      | none => new-existential(lam-l)
                    end
                    arrow-typ = t-arrow(lam-args.foldr(lam(arg, lst):
                      link(arg-coll.types.get-value(arg.id.key()), lst)
                    end, empty), ret-typ, lam-l)
                    lam-typ =
                      if is-empty(lam-params):
                        arrow-typ
                      else:
                        forall = for map(param from lam-params): t-var(param, lam-l) end
                        t-forall(forall, arrow-typ, lam-l)
                      end
                    fold-result(coll-bindings.{types: coll-bindings.types.set(binding.b.id.key(), lam-typ)})
                  end
                end
              | else => collected-bindings
            end
          end
        end, fold-result(coll))
      end
      fold-context = collected.bind(lam(self): fold-result(self.add-types(context)) end)

      binds-result = for bind(unfold-ctxt from fold-context):
        fold-synthesis(lam(binding, ctxt):
          cases(A.LetrecBind) binding:
            | s-letrec-bind(l2, b, value) =>
              recreate = A.s-letrec-bind(l2, _, _)
              synthesis-binding(b, value, recreate, lam(x): x end, ctxt)
          end
        end, binds, unfold-ctxt)
      end

      binds-result.check-bind(lam(result-pair):
        checking(body, expect-loc, expect-typ, result-pair.left)
          .map(A.s-letrec(l, result-pair.right, _))
        end)
    | s-hint-exp(l, hints, exp) =>
      raise("checking for s-hint-exp not implemented")
    | s-instantiate(l, expr, params) =>
      check-synthesis(e, expect-typ, l, context)
    | s-block(l, stmts) =>
      fun gen(curr, base):
        pair(link(pair(curr, base.right), base.left), t-top(l))
      end
      paired-stmts = stmts.foldr(gen, pair(empty, expect-typ)).left
      result = fold-checking(lam(stmt-typ-pair, ctxt):
        checking(stmt-typ-pair.left, expect-loc, stmt-typ-pair.right, ctxt)
      end, paired-stmts, context)
      result.check-bind(lam(result-pair):
        checking-result(A.s-block(l, result-pair.right), result-pair.left)
      end)
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
      for check-bind(id-typ from lookup-id(l, id, context)):
        cases(Type) id-typ:
          | t-ref(arg-typ, _) =>
            checking(value, l, arg-typ, context)
          | else =>
            checking-err([list: C.incorrect-type(tostring(id-typ), l, tostring(t-ref(id-typ, l)), l)])
        end
      end
    | s-if-pipe(l, branches) =>
      raise("s-if-pipe should have already been desugared")
    | s-if-pipe-else(l, branches, _else) =>
      raise("s-if-pipe-else should have already been desugared")
    | s-if(l, branches) =>
      raise("s-if should have already been desugared")
    | s-if-else(l, branches, _else) =>
      for map-result(branch from branches):
        checking(branch.test, branch.l, t-boolean(branch.l), context).fold-bind(lam(new-test, new-context):
          checking(branch.body, expect-loc, expect-typ, new-context).fold-bind(lam(new-body, new-ctxt):
            fold-result(A.s-if-branch(branch.l, new-test, new-body))
          end)
        end)
      end.check-bind(lam(new-branches):
        checking(_else, expect-loc, expect-typ, context).map(A.s-if-else(l, new-branches, _))
      end)
    | s-cases(l, typ, val, branches) =>
      checking-cases(l, typ, val, branches, none, expect-loc, expect-typ, context)
    | s-cases-else(l, typ, val, branches, _else) =>
      checking-cases(l, typ, val, branches, some(_else), expect-loc, expect-typ, context)
    | s-op(loc, op, l, r) =>
      raise("checking for s-op not implemented")
    | s-check-test(loc, op, refinement, l, r) =>
      checking-result(e, context)
    | s-check-expr(l, expr, ann) =>
      raise("checking for s-check-expr not implemented")
    | s-paren(l, expr) =>
      raise("s-paren should have already been desugared")
    | s-lam(l, params, args, ann, doc, body, _check) =>
      check-fun(l, body, params, args, ann, expect-loc, expect-typ, A.s-lam(l, params, _, _, doc, _, _check), context)
    | s-method(l, params, args, ann, doc, body, _check) =>
      raise("checking for s-method not implemented")
    | s-extend(l, supe, fields) =>
      raise("checking for s-extend not implemented")
    | s-update(l, obj, fields) =>
      check-synthesis(e, expect-typ, expect-loc, context)
    | s-obj(l, fields) =>
      check-synthesis(e, expect-typ, expect-loc, context)
    | s-array(l, values) =>
      wrapped = cases(Type) expect-typ:
        | t-app(rarray, args, tl) =>
          if TS.t-array-name == rarray:
            param-typ = args.first
            for fold-checking(value from values, ctxt from context):
              checking(value, expect-loc, param-typ, ctxt)
            end
          else:
            checking-err([list: C.incorrect-type(tostring(TS.t-array-name), l, tostring(expect-typ), expect-loc)])
          end
        | t-top(tl) =>
          for fold-checking(value from values, ctxt from context):
            checking(value, expect-loc, t-top(tl), ctxt)
          end
        | else =>
          fold-errors([list: C.incorrect-type("a raw array", l, tostring(expect-typ), expect-loc)])
      end
      for check-bind(new-context-and-values from wrapped):
        checking-result(A.s-array(l, new-context-and-values.right), new-context-and-values.left)
      end
    | s-construct(l, modifier, constructor, values) =>
      raise("checking for s-construct not implemented")
    | s-app(l, _fun, args) =>
      check-synthesis(e, expect-typ, l, context)
    | s-prim-app(l, _fun, args) =>
      check-synthesis(e, expect-typ, l, context)
    | s-prim-val(l, name) =>
      raise("checking for s-prim-val not implemented")
    | s-id(l, id) =>
      check-synthesis(e, expect-typ, l, context)
    | s-id-var(l, id) =>
      check-synthesis(e, expect-typ, l, context)
    | s-id-letrec(l, id, safe) =>
      check-synthesis(e, expect-typ, l, context)
    | s-undefined(l) =>
      raise("checking for s-undefined not implemented")
    | s-srcloc(l, loc) =>
      check-synthesis(e, expect-typ, l, context)
    | s-num(l, n) =>
      check-synthesis(e, expect-typ, l, context)
    | s-frac(l, num, den) =>
      check-synthesis(e, expect-typ, l, context)
    | s-bool(l, b) =>
      check-synthesis(e, expect-typ, l, context)
    | s-str(l, s) =>
      check-synthesis(e, expect-typ, l, context)
    | s-dot(l, obj, field) =>
      check-synthesis(e, expect-typ, l, context)
    | s-get-bang(l, obj, field) =>
      check-synthesis(e, expect-typ, l, context)
    | s-bracket(l, obj, field) =>
      raise("checking for s-bracket not implemented")
    | s-data(l, name, params, mixins, variants, shared-members, _check) =>
      raise("s-paren should have already been desugared")
    | s-data-expr(l, name, namet, params, mixins, variants, shared-members, _check) =>
      check-synthesis(e, expect-typ, l, context)
    | s-for(l, iterator, bindings, ann, body) =>
      raise("s-for should have already been desugared")
    | s-check(l, name, body, keyword-check) =>
      checking-result(e, context)
  end
end

fun synthesis(e, context):
  result = _synthesis(e, context)
  #print("")
  #print("synthesis on:")
  #each(print, e.tosource().pretty(72))
  #print("result:")
  #print(result)
  result
end

fun _synthesis(e :: A.Expr, context :: Context) -> SynthesisResult:
  cases(A.Expr) e:
    | s-module(l, answer, defined-values, defined-types, provided-values, provided-types, checks) =>
      synthesis(answer, context)
        .map-expr(A.s-module(l, _, defined-values, defined-types, provided-values, provided-types, checks))
        .map-typ(_.set-loc(l))
    | s-type-let-expr(l, binds, body) =>
      for synth-bind(_ from handle-type-let-binds(binds, context)):
        synthesis(body, context)
          .map-expr(A.s-type-let-expr(l, binds, _))
          .map-typ(_.set-loc(l))
      end
    | s-let-expr(l, binds, body) =>
      binds-result = fold-synthesis(synthesis-let-bind, binds, context)
      binds-result.synth-bind(lam(result-pair):
        synthesis(body, result-pair.left)
          .map-expr(A.s-let-expr(l, result-pair.right, _))
          .map-typ(_.set-loc(l))
      end)
    | s-letrec(l, binds, body) =>
      # TODO(MATT): annotation checking with recursive binds
      collected = collect-bindings(binds.map(lam(binding): binding.b end), context)
      fold-context = collected.bind(lam(self): fold-result(self.add-types(context)) end)

      binds-result = for bind(unfold-ctxt from fold-context):
        fold-synthesis(lam(binding, ctxt):
          cases(A.LetrecBind) binding:
            | s-letrec-bind(l2, b, value) =>
              recreate = A.s-letrec-bind(l2, _, _)
              synthesis-binding(b, value, recreate, lam(x): x end, ctxt)
                .map-typ(_.set-loc(l2))
          end
        end, binds, unfold-ctxt)
      end

      binds-result.synth-bind(lam(result-pair):
        synthesis(body, result-pair.left)
          .map-expr(A.s-letrec(l, result-pair.right, _))
          .map-typ(_.set-loc(l))
      end)
    | s-hint-exp(l, hints, exp) =>
      raise("synthesis for s-hint-exp not implemented")
    | s-instantiate(l, expr, params) =>
      synthesis-instantiation(l, expr, params, context)
    | s-block(l, stmts) =>
      var typ = t-top(l)
      var loc = l
      fold-synthesis(lam(stmt, ctxt):
        synthesis(stmt, ctxt).bind(
          lam(stmt-expr, stmt-loc, stmt-typ, out-context):
            typ := stmt-typ
            loc := stmt-loc
            synthesis-result(stmt-expr, stmt-loc, stmt-typ)
          end)
      end, stmts, context).synth-bind(lam(context-and-stmts):
        synthesis-result(A.s-block(l, context-and-stmts.right), loc, typ.set-loc(l), context-and-stmts.left)
      end)
    | s-user-block(l, body) =>
      raise("s-user-block should have already been desugared")
    | s-fun(l, name, params, args, ann, doc, body, _check) =>
      raise("s-fun should have already been desugared")
    | s-type(l, name, ann) =>
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
      for synth-bind(id-typ from lookup-id(l, id, context)):
        cases(Type) id-typ:
          | t-ref(arg-typ, tl) =>
            checking(value, l, arg-typ, context).synth-bind(lam(new-value, out-context):
              synthesis-result(A.s-assign(l, id, new-value), l, arg-typ.set-loc(l), out-context)
            end)
          | else =>
            synthesis-err([list: C.incorrect-type(tostring(id-typ), l, tostring(t-ref(id-typ, l)), l)])
        end
      end
    | s-if-pipe(l, branches) =>
      raise("s-if-pipe should have already been desugared")
    | s-if-pipe-else(l, branches, _else) =>
      raise("s-if-pipe-else should have already been desugared")
    | s-if(l, branches) =>
      raise("s-if should have already been desugared")
    | s-if-else(l, branches, _else) =>
      for synth-bind(result from map-result(handle-if-branch(_, context), branches)):
        synthesis(_else, context).bind(
          lam(new-else, _, else-typ, out-context):
            split-result = split(result)
            new-branches = split-result.left
            new-if-else  = A.s-if-else(l, new-branches, new-else)
            for synth-bind(if-else-typ from meet-branch-typs(link(else-typ, split-result.right), context)):
              synthesis-result(new-if-else, l, if-else-typ.set-loc(l), out-context)
            end
          end)
      end
    | s-cases(l, typ, val, branches) =>
      synthesis-cases(l, typ, val, branches, none, context)
    | s-cases-else(l, typ, val, branches, _else) =>
      synthesis-cases(l, typ, val, branches, some(_else), context)
    | s-op(loc, op, l, r) =>
      raise("synthesis for s-op not implemented")
    | s-check-test(loc, op, refinement, l, r) =>
      synthesis-result(e, l, t-top(loc), context)
    | s-check-expr(l, expr, ann) =>
      raise("synthesis for s-check-expr not implemented")
    | s-paren(l, expr) =>
      raise("s-paren should have already been desugared")
    | s-lam(l, params, args, ann, doc, body, _check) =>
      synthesis-fun(l, body, params, args, ann, A.s-lam(l, params, _, _, doc, _, _check), context)
    | s-method(l, params, args, ann, doc, body, _check) =>
      raise("synthesis for s-method not implemented")
    | s-extend(l, supe, fields) =>
      raise("synthesis for s-extend not implemented")
    | s-update(l, obj, fields) =>
      synthesis(obj, context).bind(synthesis-update(l, _, _, _, fields, _))
    | s-obj(l, fields) =>
      for synth-bind(result from map-result(to-type-member(_, context), fields)):
        split-fields = split(result)
        new-fields   = split-fields.left
        field-typs   = split-fields.right
        new-obj      = A.s-obj(l, new-fields)
        obj-typ      = t-record(field-typs, l)
        synthesis-result(new-obj, l, obj-typ, context)
      end
    | s-array(l, values) =>
      fun process(value :: A.Expr) -> FoldResult<Pair<A.Expr, Type>>:
        synthesis(value, context).fold-bind(lam(expr, _, typ, _):
          fold-result(pair(expr, typ))
        end)
      end
      for synth-bind(result from map-result(process, values)):
        split-result = split(result)
        new-values = split-result.left
        value-typs = split-result.right
        fold-array-typ = meet-branch-typs(value-typs, context)
        for synth-bind(array-typ from fold-array-typ):
          new-array = A.s-array(l, new-values)
          synthesis-result(new-array, l, t-array(array-typ.set-loc(l), l))
        end
      end
    | s-construct(l, modifier, constructor, values) =>
      raise("synthesis for s-construct not implemented")
    | s-app(l, _fun, args) =>
      synthesis-app-fun(l, _fun, args, context)
        .synth-bind(lam(new-fun, new-fun-loc, new-fun-typ, new-context):
          synthesis-spine(new-context.apply(new-fun-typ), A.s-app(l, _fun, _), args, l, new-context)
            .map-typ(_.set-loc(l))
        end)
    | s-prim-app(l, _fun, args) =>
      for synth-bind(arrow-typ from lookup-id(l, _fun, context)):
        synthesis-spine(arrow-typ, A.s-prim-app(l, _fun, _), args, l, context)
          .map-typ(_.set-loc(l))
      end
    | s-prim-val(l, name) =>
      raise("synthesis for s-prim-val not implemented")
    | s-id(l, id) =>
      for synth-bind(id-typ from lookup-id(l, id, context)):
        synthesis-result(e, l, id-typ, context)
      end
    | s-id-var(l, id) =>
      for synth-bind(id-typ from lookup-id(l, id, context)):
        cases(Type) id-typ:
          | t-ref(arg-typ, _) =>
            synthesis-result(e, l, arg-typ, context)
          | else =>
            synthesis-err([list: C.incorrect-type(tostring(id-typ), l, tostring(t-ref(id-typ, l)), l)])
        end
      end
    | s-id-letrec(l, id, safe) =>
      for synth-bind(id-typ from lookup-id(l, id, context)):
        synthesis-result(e, l, id-typ, context)
      end
    | s-undefined(l) =>
      raise("synthesis for s-undefined not implemented")
    | s-srcloc(l, loc) =>
      synthesis-result(e, l, t-srcloc(l), context)
    | s-num(l, n) =>
      synthesis-result(e, l, t-number(l), context)
    | s-frac(l, num, den) =>
      synthesis-result(e, l, t-number(l))
    | s-bool(l, b) =>
      synthesis-result(e, l, t-boolean(l), context)
    | s-str(l, s) =>
      synthesis-result(e, l, t-string(l), context)
    | s-dot(l, obj, field) =>
      synthesis(obj, context).bind(synthesis-field(l, _, _, _, field, A.s-dot, _))
    | s-get-bang(l, obj, field) =>
      synthesis(obj, context).bind(synthesis-field(l, _, _, _, field, A.s-get-bang, _)).bind(
      lam(new-get-bang, field-typ-loc, field-typ, out-context):
        cases(Type) field-typ:
          | t-ref(typ, _) =>
            synthesis-result(new-get-bang, field-typ-loc, typ.set-loc(l), out-context)
          | else =>
            synthesis-err([list: C.incorrect-type(tostring(field-typ), field-typ-loc, "a ref type", l)])
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
    | s-check(l, name, body, keyword-check) => synthesis-result(e, l, t-top(l), context)
  end.synth-bind(lam(ast, loc, typ, out-context):
    synthesis-result(ast, loc, out-context.apply(typ), out-context)
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

fun _synthesis-spine(fun-type :: Type, recreate :: (List<A.Expr> -> A.Expr), args :: List<A.Expr>, app-loc :: Loc, context :: Context) -> SynthesisResult:
  cases(Type) fun-type:
    | t-arrow(arg-typs, ret-typ, _) =>
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
    | t-forall(introduces, onto, l) =>
      new-type = introduces.foldr(lam(type-var, new-type):
        new-type.substitute(type-var, new-existential(l))
      end, onto)
      synthesis-spine(new-type, recreate, args, app-loc, context)
    | t-bot(l) =>
      result = fold-checking(lam(arg, ctxt):
        checking(arg, app-loc, t-top(l), context)
      end, args, context)
      for synth-bind(context-and-args from result):
        synthesis-result(recreate(context-and-args.right), app-loc, t-bot(l), context)
      end
    | t-existential(id, l) =>
      existential-args = args.map(lam(_): new-existential(l) end)
      existential-ret = new-existential(l)
      new-arrow = t-arrow(existential-args, existential-ret, l)
      folded-new-context = context.assign-existential(fun-type, new-arrow)

      for synth-bind(new-context from folded-new-context):
        result = fold2(lam(acc, arg, arg-typ):
          for bind(exprs-and-context from acc):
            checking(arg, app-loc, arg-typ, exprs-and-context.right)
              .fold-bind(lam(new-arg, out-context):
                fold-result(pair(link(new-arg, exprs-and-context.left), out-context))
              end)
          end
        end, fold-result(pair(empty, new-context)), args, existential-args)

        for synth-bind(exprs-and-context from result):
          synthesis-result(recreate(exprs-and-context.left),
                            app-loc, existential-ret, exprs-and-context.right)
        end
      end
    | else =>
      synthesis-err([list: C.apply-non-function(app-loc, fun-type)])
  end.synth-bind(lam(ast, loc, typ, out-context):
    synthesis-result(ast, loc, out-context.apply(typ.set-loc(app-loc)), out-context)
  end)
end

fun synthesis-let-bind(binding :: A.LetBind, context :: Context) -> SynthesisResult:
  cases(A.LetBind) binding:
    | s-let-bind(l, b, value) =>
      synthesis-binding(b, value, A.s-let-bind(l, _, _), lam(x): x end, context)
        .map-typ(_.set-loc(l))
    | s-var-bind(l, b, value) =>
      synthesis-binding(b, value, A.s-var-bind(l, _, _), lam(x): t-ref(x, l) end, context)
        .map-typ(_.set-loc(l))
  end
end

fun synthesis-binding(binding :: A.Bind, value :: A.Expr, recreate :: (A.Bind, A.Expr -> A.LetBind), wrap :: (Type -> Type), context :: Context) -> SynthesisResult:
  fun process-value(expr, typ-loc, typ, out-context):
    synthesis-binding-result(recreate(binding, expr), wrap(typ.set-loc(typ-loc)), out-context)
  end
  folded-type = if context.has-var-key(binding.id.key()):
    fold-result(some(context.get-var-type(binding.id.key())))
  else:
    to-type(binding.ann, context)
  end
  for synth-bind(maybe-typ from folded-type):
    cases(Option<Type>) maybe-typ:
      | none =>
        synthesis(value, context)
      | some(typ) =>
        checking(value, binding.l, typ, context).synth-bind(synthesis-result(_, binding.l, typ, _))
    end.bind(process-value)
  end
end

fun synthesis-update(update-loc :: Loc, obj :: A.Expr, obj-typ-loc :: A.Loc, obj-typ :: Type, fields :: List<A.Member>, context :: Context) -> SynthesisResult:
  fun process-field(member, obj-fields):
    cases(Option<TypeMember>) type-members-lookup(obj-fields, member.right.field-name):
      | some(btm) =>
        cases(Type) btm.typ:
          | t-ref(onto, l) =>
            cases(FoldResult<Context>) satisfies-type(member.right.typ, onto, context):
              | fold-result(_) => fold-result(member.left)
              | fold-errors(_) => fold-errors([list: C.incorrect-type(tostring(member.right.typ), update-loc, tostring(onto), obj-typ-loc)])
            end
          | else =>
            fold-errors([list: C.incorrect-type(tostring(btm.typ), obj-typ-loc, tostring(t-ref(btm.typ, update-loc)), update-loc)])
        end
      | none =>
        fold-errors([list: C.object-missing-field(member.right.field-name, "{" + obj-fields.map(tostring).join-str(", ") + "}", obj-typ-loc, update-loc)])
    end
  end

  fun process-fields(l, maybe-obj-fields, atms):
    cases(Option<List<TypeMember>>) maybe-obj-fields:
      | some(obj-fields) =>
        for synth-bind(new-fields from map-result(process-field(_, obj-fields), atms)):
          synthesis-result(A.s-update(l, obj, new-fields), obj-typ-loc, obj-typ, context)
            .map-typ(_.set-loc(l))
        end
      | none =>
        split-fields = split(atms)
        new-fields   = split-fields.left
        synthesis-result(A.s-update(l, obj, new-fields), obj-typ-loc, obj-typ.set-loc(l), context)
    end
  end

  for synth-bind(atms from map-result(to-type-member(_, context), fields)):
    record-view(update-loc, obj-typ-loc, obj-typ, process-fields(_, _, atms), context)
      .map-typ(_.set-loc(update-loc))
  end
end

# TODO(MATT): some of these should be errors
fun to-type(in-ann :: A.Ann, context :: Context) -> FoldResult<Option<Type>>:
  cases(A.Ann) in-ann:
    | a-blank =>
      fold-result(none)
    | a-any =>
      fold-result(some(t-top(A.dummy-loc)))
    | a-name(l, id) =>
      cases(Option<Type>) context.info.aliases.get-now(id.key()):
        | some(typ) => fold-result(some(typ))
        | none => fold-result(some(t-name(none, id, l)))
      end
    | a-type-var(l, id) =>
      fold-result(some(t-var(id, l)))
    | a-arrow(l, args, ret, use-parens) =>
      fold-arg-typs = args.foldr(lam(arg, fold-maybe-arg-typs):
        for bind(maybe-arg-typs from fold-maybe-arg-typs):
          cases(Option<List<Type>>) maybe-arg-typs:
            | none => fold-result(none)
            | some(arg-typs) =>
              for bind(maybe-new-typ from to-type(arg, context)):
                fold-result(maybe-new-typ.and-then(lam(new-typ):
                  link(new-typ, arg-typs)
                end))
              end
          end
        end
      end, fold-result(some(empty)))

      for bind(maybe-arg-typs from fold-arg-typs):
        cases(Option<List<Type>>) maybe-arg-typs:
          | none => fold-result(none)
          | some(arg-typs) =>
            for bind(maybe-ret-typ from to-type(ret, context)):
              fold-result(maybe-ret-typ.and-then(lam(ret-typ):
                t-arrow(arg-typs, ret-typ, l)
              end))
            end
        end
      end
    | a-method(l, args, ret) =>
      raise("a-method not implemented")
    | a-record(l, fields) =>
      fields-result = fields.foldr(lam(field, field-typs):
        for bind(maybe-typs from field-typs):
          cases(Option<List<Type>>) maybe-typs:
            | none => fold-result(none)
            | some(typs) =>
              for bind(maybe-typ from to-type(field.ann, context)):
                cases(Option<Type>) maybe-typ:
                  | none => fold-result(none)
                  | some(typ) =>
                    fold-result(some(link(t-member(field.name, typ, l), typs)))
                end
              end
          end
        end
      end, fold-result(some(empty)))
      for bind(maybe-members from fields-result):
        fold-result(maybe-members.and-then(lam(members):
          t-record(members, l)
        end))
      end
    | a-app(l, ann, args) => # TODO(MATT): check args?
      for bind(maybe-typ from to-type(ann, context)):
        cases(Option<Type>) maybe-typ:
          | none => fold-result(none)
          | some(typ) =>
            for bind(maybe-arg-typs from map-result(lam(arg): to-type(arg, context) end, args)):
              arg-typs = maybe-arg-typs.foldr(lam(maybe-arg-typ, maybe-arg-list):
                cases(Option<List<Type>>) maybe-arg-list:
                  | none => none
                  | some(arg-list) =>
                    cases(Option<Type>) maybe-arg-typ:
                      | none => none
                      | some(arg-typ) => some(link(arg-typ, arg-list))
                    end
                end
              end, some(empty))
              fold-result(arg-typs.and-then(lam(typs): t-app(typ, typs, l) end))
            end
        end
      end
    | a-pred(l, ann, exp) =>
      for bind(maybe-typ from to-type(ann, context)):
        cases(Option<Type>) maybe-typ:
          | some(typ) =>
            expect-typ = t-arrow([list: typ], t-boolean(l), l)
            # TODO(MATT): decide if I want to keep handling the errors this way
            cases(CheckingResult) checking(exp, l, expect-typ, context):
              | checking-err(errs) => errs.map(context.info.errors.insert)
              | else => nothing
            end
            fold-result(some(typ))
          | none => raise("Issue in a-pred annotation")
        end
      end
    | a-dot(l, obj, field) =>
      key = obj.key()
      cases(Option<String>) context.info.mod-names.get-now(key):
        | some(mod) =>
          t-mod = context.info.modules.get-value-now(mod)
          if t-mod.types.has-key(field):
            fold-result(some(t-name(some(mod), A.s-global(field), l)))
          else if t-mod.aliases.has-key(field):
            fold-result(some(t-mod.aliases.get-value(field)))
          else:
            fold-errors([list: C.unbound-type-id(in-ann)])
          end
        | none =>
          fold-errors([list: C.no-module(l, obj.toname())])
      end
    | a-checked(checked, residual) =>
      raise("a-checked should not be appearing before type checking!")
  end
end

fun check-synthesis(e :: A.Expr, expect-typ :: Type, expect-loc :: A.Loc, context :: Context) -> CheckingResult:
  synthesis(e, context).check-bind(lam(new-expr, new-loc, new-typ, new-context):
    for check-bind(ctxt from satisfies-type(new-context.apply(new-typ), new-context.apply(expect-typ), new-context)):
      checking-result(e, ctxt)
    end
  end)
end

fun satisfies-type(subtyp :: Type, supertyp :: Type, context :: Context) -> FoldResult<Context>:
  fun _satisfies-assuming(_subtyp, _supertyp, _context, assumptions :: Set<Pair<Type, Type>>):
    shadow subtyp = _subtyp
    shadow supertyp = _supertyp
    shadow context = _context
    if assumptions.member(pair(subtyp, supertyp)):
      fold-result(context)
    else:
      shadow subtyp = resolve-alias(subtyp, context)
      shadow supertyp = resolve-alias(supertyp, context)
      cases(Type) supertyp:
        | t-existential(b-id, _) =>
          cases(Type) subtyp:
            | t-existential(a-id, _) =>
              if a-id == b-id:
                fold-result(context)
              else:
                instantiate-left(subtyp, supertyp, context)
              end
            | else => instantiate-right(subtyp, supertyp, context)
          end
        | t-forall(b-introduces, b-onto, _) =>
          satisfies-assuming(subtyp, b-onto, context, assumptions)
        | else =>
          cases(Type) subtyp:
            | t-name(a-mod, a-id, _) =>
              cases(Type) supertyp:
                | t-top(_) => fold-result(context)
                | t-name(b-mod, b-id, _) =>
                  if (a-mod == b-mod) and (a-id == b-id):
                    fold-result(context)
                  else:
                    fold-errors([list: C.incorrect-type(tostring(subtyp), subtyp.l, tostring(supertyp), supertyp.l)])
                  end
                | t-bot(_) =>
                  fold-errors([list: C.incorrect-type(tostring(subtyp), subtyp.l, tostring(supertyp), supertyp.l)])
                | t-record(b-fields, _) =>
                  cases(Option<Type>) context.get-data-type(subtyp):
                    | none =>
                      fold-errors([list: C.incorrect-type(tostring(subtyp), subtyp.l, "A datatype", supertyp.l)])
                    | some(data-type) =>
                      satisfies-assuming(data-type, supertyp, context, assumptions)
                  end
                | else =>
                  fold-errors([list: C.incorrect-type(tostring(subtyp), subtyp.l, tostring(supertyp), supertyp.l)])
              end
            | t-var(a-id, _) =>
              cases(Type) supertyp:
                | t-top(_) => fold-result(context)
                | t-var(b-id, _) =>
                  if a-id == b-id:
                    fold-result(context)
                  else:
                    fold-errors([list: C.incorrect-type(tostring(subtyp), subtyp.l, tostring(supertyp), supertyp.l)])
                  end
                | else =>
                  fold-errors([list: C.incorrect-type(tostring(subtyp), subtyp.l, tostring(supertyp), supertyp.l)])
              end
            | t-arrow(a-args, a-ret, _) =>
              cases(Type) supertyp:
                | t-top(_) => fold-result(context)
                | t-arrow(b-args, b-ret, _) =>
                  # Order is important because contravariance!
                  # TODO(MATT): deal with substitutions from discovered types
                  result = fold2-strict(lam(fold-ctxt, b-arg, a-arg):
                    for bind(ctxt from fold-ctxt):
                      satisfies-assuming(b-arg, a-arg, ctxt, assumptions)
                    end
                  end, fold-result(context), b-args, a-args)
                  cases(Option<Context>) result:
                    | none =>
                      fold-errors([list: C.incorrect-type(tostring(subtyp), subtyp.l, tostring(supertyp), supertyp.l)])
                    | some(fold-ctxt) =>
                      for bind(ctxt from fold-ctxt):
                        satisfies-assuming(ctxt.apply(a-ret), ctxt.apply(b-ret), ctxt, assumptions)
                      end
                  end
                | else =>
                  fold-errors([list: C.incorrect-type(tostring(subtyp), subtyp.l, tostring(supertyp), supertyp.l)])
              end
            | t-forall(a-introduces, a-onto, a-l) =>
              new-onto = a-introduces.foldr(lam(type-var, new-onto):
                new-onto.substitute(type-var, new-existential(a-l))
              end, a-onto)
              satisfies-assuming(new-onto, supertyp, context, assumptions)
            | t-app(a-onto, a-args, _) =>
              cases(Type) supertyp:
                | t-top(_) => fold-result(context)
                | t-app(b-onto, b-args, _) =>
                  a-data-type = context.get-data-type(a-onto).and-then(lam(data-typ):
                    data-typ.introduce(a-args)
                  end)
                  b-data-type = context.get-data-type(b-onto).and-then(lam(data-typ):
                    data-typ.introduce(b-args)
                  end)
                  cases(Option<Type>) a-data-type:
                    | none =>
                      fold-errors([list: C.incorrect-type(tostring(subtyp), subtyp.l, tostring(supertyp), supertyp.l)])
                    | some(a-typ) =>
                      cases(Option<Type>) b-data-type:
                        | none =>
                          fold-errors([list: C.incorrect-type(tostring(subtyp), subtyp.l, tostring(supertyp), supertyp.l)])
                        | some(b-typ) =>
                          satisfies-assuming(a-typ, b-typ, context, assumptions.add(pair(subtyp, supertyp)))
                      end
                  end

                | else =>
                  fold-errors([list: C.incorrect-type(tostring(subtyp), subtyp.l, tostring(supertyp), supertyp.l)])
              end
            | t-top(_) =>
              if TS.is-t-top(supertyp):
                fold-result(context)
              else:
                fold-errors([list: C.incorrect-type(tostring(subtyp), subtyp.l, tostring(supertyp), supertyp.l)])
              end
            | t-bot(_) =>
              fold-result(context)
            | t-record(a-fields, _) =>
              cases(Type) supertyp:
                | t-top(_) => fold-result(context)
                | t-record(b-fields, _) =>
                  satisfies-fields(a-fields, b-fields, context, assumptions)
                | else =>
                  fold-errors([list: C.incorrect-type(tostring(subtyp), subtyp.l, tostring(supertyp), supertyp.l)])
              end
            | t-ref(a-typ, _) =>
              cases(Type) supertyp:
                | t-top(_) => fold-result(context)
                | t-ref(b-typ) =>
                  for bind(ctxt from satisfies-assuming(a-typ, b-typ, context, assumptions)):
                    satisfies-assuming(b-typ, a-typ, ctxt, assumptions)
                  end
                | else =>
                  fold-errors([list: C.incorrect-type(tostring(subtyp), subtyp.l, "A non-reference type", supertyp.l)])
              end
            | t-existential(a-id, _) =>
              instantiate-left(subtyp, supertyp, context)
            | t-data(a-params, a-variants, a-fields, _) =>
              cases(Type) supertyp:
                | t-data(b-params, b-variants, b-fields, _) =>
                  a-variants.foldr(lam(a-variant, fold-context):
                    for bind(ctxt from fold-context):
                      cases(Option<TypeVariant>) supertyp.lookup-variant(a-variant.name):
                        | none =>
                          fold-errors([list: C.incorrect-type(tostring(subtyp), subtyp.l, tostring(supertyp), supertyp.l)])
                        | some(b-variant) =>
                          cases(TypeVariant) a-variant:
                            | t-singleton-variant(_, _, _) =>
                              cases(TypeVariant) b-variant:
                                | t-singleton-variant(_, _, _) =>
                                  fold-result(ctxt)
                                | else =>
                                  fold-errors([list: C.incorrect-type(tostring(subtyp), subtyp.l, tostring(supertyp), supertyp.l)])
                              end
                            | t-variant(_, a-var-fields, _, _) =>
                              cases(TypeVariant) b-variant:
                                | t-variant(_, b-var-fields, _, _) =>
                                  satisfies-fields(a-var-fields, b-var-fields, ctxt, assumptions)
                                | else =>
                                  fold-errors([list: C.incorrect-type(tostring(subtyp), subtyp.l, tostring(supertyp), supertyp.l)])
                              end
                          end
                      end
                    end
                  end, fold-result(context))
                | t-record(b-fields, _) =>
                  satisfies-fields(a-fields, b-fields, context, assumptions)
                | else =>
                  fold-errors([list: C.incorrect-type(tostring(subtyp), subtyp.l, tostring(supertyp), supertyp.l)])
              end
          end
      end
    end
  end

  fun satisfies-fields(a-fields :: List<TypeMember>, b-fields :: List<TypeMember>, _context :: Context, assumptions :: Set<Pair<Type, Type>>) -> FoldResult<Context>:
    b-fields.foldr(lam(b-field, fold-context):
      for bind(ctxt from fold-context):
        cases(Option<TypeMember>) a-fields.find(lam(a-field): a-field.field-name == b-field.field-name end):
          | none =>
            fold-errors([list: C.cant-typecheck("could not find field on supertype")])
          | some(a-field) =>
            satisfies-assuming(a-field.typ, b-field.typ, ctxt, assumptions)
        end
      end
    end, fold-result(_context))
  end

  fun satisfies-assuming(_subtyp, _supertyp, _context, assumptions):
    result = _satisfies-assuming(_subtyp, _supertyp, _context, assumptions)
    #print("")
    #print(tostring(_subtyp) + " <: " + tostring(_supertyp))
    #print("result:")
    #print(result)
    result
  end
  satisfies-assuming(subtyp, supertyp, context, [list-set: ])
end


fun instantiate-right(subtyp, supertyp, context):
  result = _instantiate-right(subtyp, supertyp, context)
  #print("")
  #print(tostring(subtyp) + " <=: " + tostring(supertyp))
  #print("result:")
  #print(result)
  result
end

fun _instantiate-right(subtyp :: Type, supertyp :: Type, context :: Context) -> FoldResult<Context>:
  cases(Type) supertyp:
    | t-existential(b-id, b-l) =>
      cases(Type) subtyp:
        | t-name(a-mod, a-id, _) =>
          context.assign-existential(supertyp, subtyp)
        | t-var(a-id, _) =>
          context.assign-existential(supertyp, subtyp)
        | t-arrow(a-args, a-ret, a-l) =>
          args-and-existentials = a-args.map(lam(arg): pair(arg, new-existential(arg.l)) end)
          arg-existentials = split(args-and-existentials).right
          ret-existential = new-existential(a-ret.l)
          folded-new-context = context.assign-existential(supertyp, t-arrow(arg-existentials, ret-existential, b-l))
          for bind(new-context from folded-new-context):
            arg-context = args-and-existentials.foldr(lam(arg-and-exists, fold-ctxt):
              for bind(ctxt from fold-ctxt):
                instantiate-left(arg-and-exists.right, arg-and-exists.left, ctxt)
              end
            end, fold-result(new-context))
            for bind(arg-ctxt from arg-context):
              instantiate-right(arg-ctxt.apply(a-ret), ret-existential, arg-ctxt)
            end
          end
        | t-forall(a-introduces, a-onto, _) =>
          # TODO(MATT): examine implications
          context.assign-existential(supertyp, subtyp)
        | t-app(a-onto, a-args, _) =>
          context.assign-existential(supertyp, subtyp)
        | t-top(_) =>
          context.assign-existential(supertyp, subtyp)
        | t-bot(_) =>
          context.assign-existential(supertyp, subtyp)
        | t-record(fields, _) =>
          context.assign-existential(supertyp, subtyp)
        | t-ref(a-typ, _) =>
          context.assign-existential(supertyp, subtyp)
        | t-existential(a-id, _) =>
          context.assign-existential(subtyp, supertyp)
        | t-data(variants, fields, _) =>
          context.assign-existential(supertyp, subtyp)
      end
    | else => raise("cannot instantiate non-existential")
  end
end

fun instantiate-left(subtyp, supertyp, context):
  result = _instantiate-left(subtyp, supertyp, context)
  #print("")
  #print(tostring(subtyp) + " :=< " + tostring(supertyp))
  #print("result:")
  #print(result)
  result
end

fun _instantiate-left(subtyp :: Type, supertyp :: Type, context :: Context) -> FoldResult<Context>:
  cases(Type) subtyp:
    | t-existential(a-id, a-l) =>
      cases(Type) supertyp:
        | t-name(b-mod, b-id, _) =>
          context.assign-existential(subtyp, supertyp)
        | t-var(b-id, _) =>
          context.assign-existential(subtyp, supertyp)
        | t-arrow(b-args, b-ret, b-l) =>
          args-and-existentials = b-args.map(lam(arg): pair(arg, new-existential(arg.l)) end)
          arg-existentials = split(args-and-existentials).right
          ret-existential = new-existential(b-ret.l)
          folded-new-context = context.assign-existential(supertyp, t-arrow(arg-existentials, ret-existential, b-l))
          for bind(new-context from folded-new-context):
            arg-context = args-and-existentials.foldr(lam(arg-and-exists, fold-ctxt):
              for bind(ctxt from fold-ctxt):
                instantiate-right(arg-and-exists.left, arg-and-exists.right, ctxt)
              end
            end, fold-result(new-context))
            for bind(arg-ctxt from arg-context):
              instantiate-left(ret-existential, arg-ctxt.apply(b-ret), arg-ctxt)
            end
          end
        | t-forall(b-introduces, b-onto, _) =>
          # TODO(MATT): examine consequences
          context.assign-existential(subtyp, supertyp)
        | t-app(b-onto, b-args, _) =>
          context.assign-existential(subtyp, supertyp)
        | t-top(_) =>
          context.assign-existential(subtyp, supertyp)
        | t-bot(_) =>
          context.assign-existential(subtyp, supertyp)
        | t-record(fields, _) =>
          context.assign-existential(subtyp, supertyp)
        | t-ref(b-typ, _) =>
          context.assign-existential(subtyp, supertyp)
        | t-existential(b-id, _) =>
          context.assign-existential(supertyp, subtyp)
        | t-data(variants, fields, _) =>
          context.assign-existential(subtyp, supertyp)
      end
    | else => raise("cannot instantiate non-existential")
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
  collected = collect-bindings(args, context)

  for synth-bind(coll from collected):
    # TODO(MATT): is this keeping too much of the context?
    fun process(new-body :: A.Expr, ret-typ :: Type, out-context :: Context) -> SynthesisResult:
      new-fun = recreate(args, ret-ann, new-body)
      arrow-typ = t-arrow(args.foldr(lam(arg, lst):
          link(coll.types.get-value(arg.id.key()), lst)
        end, empty), ret-typ, l)
      new-typ =
        if is-empty(params):
          arrow-typ
        else:
          forall = for map(param from params): t-var(param, l) end
          t-forall(forall, arrow-typ, l)
        end
      synthesis-result(new-fun, l, new-typ.set-loc(l), out-context)
    end

    for synth-bind(maybe-typ from to-type(ret-ann, context)):
      cases(Option<Type>) maybe-typ:
        | some(typ) => checking(body, l, typ, coll.add-types(context)).synth-bind(process(_, typ, _))
        | none =>
          new-exists = new-existential(l)
          checking(body, l, new-exists, coll.add-types(context))
            .synth-bind(process(_, new-exists, _))
      end
    end
  end
end

# TODO(MATT): look more closely at this
fun synthesis-app-fun(app-loc :: Loc, _fun :: A.Expr, args :: List<A.Expr>, context :: Context) -> SynthesisResult:
  cases(A.Expr) _fun:
    | s-id(fun-loc, id) =>
      result = lam(new-l, new-typ, new-context):
        synthesis-result(_fun, new-l, new-typ.set-loc(app-loc), new-context)
      end
      fun pick2(num-typ :: Type, rec-typ :: Type):
        cases(List<A.Expr>) args:
          | empty      =>
            synthesis-err([list: C.incorrect-number-of-args(app-loc)])
          | link(f, r) =>
            synthesis(f, context).bind(
              lam(_, l, f-typ, out-context):
                ask:
                  | f-typ == t-number(A.dummy-loc) then: result(l, num-typ, out-context)
                  | is-t-record(f-typ) then: result(l, rec-typ, out-context)
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
              lam(_, l, f-typ, out-context):
                ask:
                  | f-typ == t-number(A.dummy-loc) then: result(l, num-typ, out-context)
                  | f-typ == t-string(A.dummy-loc) then: result(l, str-typ, out-context)
                  | is-t-record(f-typ) then: result(l, rec-typ, out-context)
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
        .map-typ(_.set-loc(app-loc))
  end
end

fun check-fun(fun-loc :: A.Loc, body :: A.Expr, params :: List<A.Name>, args :: List<A.Bind>, ret-ann :: A.Ann, expect-loc :: A.Loc, expect-typ :: Type, recreate :: (List<A.Bind>, A.Ann, A.Expr -> A.Expr), context :: Context) -> CheckingResult:
  tmp-lam-bindings = collect-bindings(args, context)
  lam-bindings = for bind(tmp-bindings from tmp-lam-bindings):
    if is-empty(params):
      fold-result(tmp-bindings)
    else:
      type-keys = tmp-bindings.types.keys()
      new-types = params.foldr(lam(param, types):
        type-keys.fold(lam(_types, key):
          _types.set(key, _types.get-value(key).substitute(t-var(param, fun-loc), new-existential(fun-loc)))
        end, types)
      end, tmp-bindings.types)
      fold-result({types: new-types,
                  add-types(self, ctxt):
                    self.types.keys().fold(lam(fold-ctxt, id):
                      fold-ctxt.add-term-var(id, self.types.get-value(id))
                    end, ctxt)
                  end})
    end
  end

  cases(Type) expect-typ:
    | t-arrow(expect-args, ret-typ, _) =>
      for check-bind(lam-binds from lam-bindings):
        lam-arg-typs = map(lam(arg): lam-binds.types.get-value(arg.id.key()) end, args)
        maybe-context = fold2-strict(lam(fold-context, arg, expect-arg):
          for bind(ctxt from fold-context):
            satisfies-type(expect-arg, arg, ctxt)
          end
        end, fold-result(lam-binds.add-types(context)), lam-arg-typs, expect-args)
        cases(Option<FoldResult<Context>>) maybe-context:
          | none =>
            expected = "a function with " + tostring(expect-args.length()) + " arguments"
            found = "a function with " + tostring(args.length()) + " arguments"
            checking-err([list: C.incorrect-type(expected, fun-loc, found, expect-loc)])
          | some(fold-ctxt) =>
            for check-bind(ctxt from fold-ctxt):
              body-result = checking(body, expect-loc, ret-typ, ctxt)
              body-result.check-bind(lam(new-body, out-context):
                checking-result(recreate(args, ret-ann, new-body), out-context)
              end)
            end
        end
      end
    | t-top(_) =>
      for check-bind(new-binds from lam-bindings):
        body-result = checking(body, expect-loc, expect-typ, new-binds.add-types(context))
        body-result.check-bind(lam(new-body, out-context):
          checking-result(recreate(args, ret-ann, new-body), out-context)
        end)
      end
    | t-forall(introduces, onto, _) =>
      # TODO(MATT): come up with a better way to handle this
      result = synthesis-fun(fun-loc, body, params, args, ret-ann, recreate, context)
      result.check-bind(lam(new-fun, new-loc, new-typ, out-context):
        for check-bind(ctxt from satisfies-type(out-context.apply(new-typ), out-context.apply(expect-typ), out-context)):
          checking-result(new-fun, ctxt)
        end
      end)
    | t-existential(id, _) =>
      check-synthesis(recreate(args, ret-ann, body), expect-typ, expect-loc, context)
    | else =>
      checking-err([list: C.incorrect-type("a function", fun-loc, tostring(expect-typ), expect-loc)])
  end
end

fun collect-bindings(binds :: List<A.Bind>, context :: Context)
  -> FoldResult<{types :: SD.StringDict<Type>, add-types :: (Context -> Context)}>:
  result = binds.foldr(lam(binding, fold-results):
    for bind(results from fold-results):
      for bind(maybe-typ from to-type(binding.ann, context)):
        new-typ = cases(Option<Type>) maybe-typ:
          | some(typ) => typ.set-loc(binding.l)
          | none => new-existential(binding.l)
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
          for bind(maybe-typ from to-type(ann, context)):
            cases(Option<Type>) maybe-typ:
              | none => fold-errors(empty)
              | some(typ) =>
                context.info.aliases.set-now(name.key(), typ)
                fold-result(context)
            end
          end
        | s-newtype-bind(l, name, namet) =>
          typ = t-name(none, namet, l)
          namet-key = namet.key()
          ctxt.info.branders.set-now(namet-key, typ)
          ctxt.info.aliases.set-now(name.key(), typ)
          new-context = ctxt.add-term-var(namet-key, t-record([list:
            t-member("test", t-arrow([list: t-top(l)], t-boolean(l), l), l),
            t-member("brand", t-arrow([list: t-top(l)], typ, l), l)
          ], l))
          fold-result(new-context)
      end
    end
  end, fold-result(context))
end

fun synthesis-datatype(l :: Loc, name :: String, namet :: A.Name, params :: List<A.Name>, mixins, variants :: List<A.Variant>, fields :: List<A.Member>, _check :: Option<A.Expr>, context :: Context) -> SynthesisResult:
  if context.info.branders.has-key-now(namet.key()):
    brander-typ = context.info.branders.get-value-now(namet.key())
    t-vars = params.map(lam(param): t-var(param, l) end)

    for synth-bind(variants-result from map-result(to-type-variant(_, context), variants)):
      split-variants = split(variants-result)
      new-variants = split-variants.left
      variant-typs = split-variants.right

      for synth-bind(fields-result from map-result(to-type-member(_, context), fields)):
        split-fields = split(fields-result)

        variant-type-fields = variant-typs.map(lam(var-typ):
          var-typ.fields.append(var-typ.with-fields)
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

        new-data-type = t-data(t-vars, variant-typs, data-type-fields, l)
        new-context = context.add-data-type(namet.key(), new-data-type)
        new-context.info.data-exprs.set-now(namet.key(), new-data-type)

        data-fields = link(t-member(name, t-arrow([list: t-top(l)], t-boolean(l), l), l),
          for map(variant-typ from variant-typs):
            t-member(variant-typ.name, mk-constructor-type(variant-typ, brander-typ, t-vars), l)
          end +
          for map(variant-typ from variant-typs):
            t-member("is-" + variant-typ.name, t-arrow([list: t-top(l)], t-boolean(l), l), l)
          end)
        data-expr-typ = t-record(data-fields, l)
        synthesis-result(new-data-expr, l, data-expr-typ.set-loc(l), new-context)
      end
    end
  else:
    raise("Cannot find brander name in brander dictionary!")
  end
end

fun to-type-variant(variant :: A.Variant, context :: Context) -> FoldResult<Pair<A.Variant, TypeVariant>>:
  cases(A.Variant) variant:
    | s-variant(l, constr-loc, name, members, with-members) =>
      for bind(result from map-result(to-type-member(_, context), with-members)):
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
              | none => raise("No type annotation provided on member")
              | some(typ) => fold-result(t-member(member.bind.id.toname(), wrap(typ), l))
            end
          end
        end

        for bind(type-members from map-result(process-member, members)):
          new-variant = A.s-variant(l, constr-loc, name, members, new-with-members)
          type-variant = t-variant(name, type-members, with-type-members, l)
          fold-result(pair(new-variant, type-variant))
        end
      end
    | s-singleton-variant(l, name, with-members) =>
      for bind(result from map-result(to-type-member(_, context), with-members)):
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
        # TODO(MATT): decide about this
        synthesis-result(recreate(l, obj, field-name), l, t-bot(l))
    end
  end, context)
end

fun record-view(access-loc :: Loc, obj-typ-loc :: A.Loc, obj-typ :: Type,
                handle :: (Loc, Option<List<TypeMember>> -> SynthesisResult),
                context :: Context) -> SynthesisResult:
  non-obj-err = synthesis-err([list: C.incorrect-type(tostring(obj-typ), obj-typ-loc, "an object type", access-loc)])
  cases(Type) obj-typ:
    | t-record(members, _) =>
      handle(obj-typ-loc, some(members))
    | t-data(variants, fields, _) =>
      handle(obj-typ-loc, some(fields))
    | t-bot(_) =>
      handle(obj-typ-loc, none)
    | else =>
      cases(Option<Type>) context.get-data-type(obj-typ):
        | some(data-typ) =>
          handle(obj-typ-loc, some(data-typ.fields))
        | none => non-obj-err
      end
  end
end

fun synthesis-cases(l :: A.Loc, ann :: A.Ann, val :: A.Expr, branches :: List<A.CasesBranch>, maybe-else :: Option<A.Expr>, context :: Context) -> SynthesisResult:
  handle-cases(l, ann, val, branches, maybe-else, l, none, context, synth-bind, synthesis-err, synthesis-cases-has-else, synthesis-cases-no-else)
    .map-typ(_.set-loc(l))
end

fun synthesis-cases-has-else(l :: A.Loc, ann :: A.Ann, new-val :: A.Expr, split-result :: Pair<List<A.CasesBranch>,List<Type>>, _else :: A.Expr, context :: Context) -> SynthesisResult:
  synthesis(_else, context).bind(
    lam(new-else, _, else-typ, out-context):
      new-cases = A.s-cases-else(l, ann, new-val, split-result.left, new-else)
      for synth-bind(branches-typ from meet-branch-typs(link(else-typ, split-result.right), out-context)):
        synthesis-result(new-cases, l, branches-typ.set-loc(l), out-context)
      end
    end)
end

fun synthesis-cases-no-else(l :: A.Loc, ann :: A.Ann, new-val :: A.Expr, split-result :: Pair<List<A.CasesBranch>,List<Type>>, context :: Context) -> SynthesisResult:
  new-cases = A.s-cases(l, ann, new-val, split-result.left)
  for synth-bind(branches-typ from meet-branch-typs(split-result.right, context)):
    synthesis-result(new-cases, l, branches-typ.set-loc(l), context)
  end
end

fun checking-cases(l :: A.Loc, ann :: A.Ann, val :: A.Expr, branches :: List<A.CasesBranch>, maybe-else :: Option<A.Expr>, expect-loc :: A.Loc, expect-typ :: Type, context :: Context) -> CheckingResult:
  handle-cases(l, ann, val, branches, maybe-else, expect-loc, some(expect-typ), context, check-bind, checking-err, checking-cases-has-else(expect-loc, expect-typ), checking-cases-no-else)
end

fun checking-cases-has-else(expect-loc :: A.Loc, expect-typ :: Type):
  lam(l :: A.Loc, ann :: A.Ann, new-val :: A.Expr, split-result :: Pair<List<A.CasesBranch>,List<Type>>, _else :: A.Expr, context :: Context) -> CheckingResult:
    checking(_else, expect-loc, expect-typ, context).bind(lam(new-else, out-context):
      new-cases = A.s-cases-else(l, ann, new-val, split-result.left, new-else)
      checking-result(new-cases, out-context)
    end)
  end
end

fun checking-cases-no-else(l :: A.Loc, ann :: A.Ann, new-val :: A.Expr, split-result :: Pair<List<A.CasesBranch>,List<Type>>, context :: Context) -> CheckingResult:
  new-cases = A.s-cases(l, ann, new-val, split-result.left)
  checking-result(new-cases, context)
end

fun handle-cases<B>(l :: A.Loc, ann :: A.Ann, val :: A.Expr, branches :: List<A.CasesBranch>, maybe-else :: Option<A.Expr>, expect-loc :: A.Loc, maybe-expect :: Option<Type>, context :: Context, bind-direction, create-err :: (List<C.CompileError> -> B), has-else, no-else) -> B:
  for bind-direction(maybe-typ from to-type(ann, context)):
    cases(Option<Type>) maybe-typ:
      | some(typ) =>
        cases(Option<Type>) context.get-data-type(typ):
          | some(data-type) =>
            bind-direction(lam(new-val, new-context):
              branch-tracker = track-branches(data-type)

              temp-result = branches.foldr(lam(branch, fold-branch-typs-and-context):
                for bind(branch-typs-and-context from fold-branch-typs-and-context):
                  branch-result = handle-branch(data-type, l, branch, expect-loc, maybe-expect, branch-tracker.remove, branch-typs-and-context.right)
                  for bind(branch-typ-and-new-context from branch-result):
                    branch-typ-pair = branch-typ-and-new-context.left
                    new-ctxt = branch-typ-and-new-context.right
                    fold-result(pair(link(branch-typ-pair, branch-typs-and-context.left), new-ctxt))
                  end
                end
              end, fold-result(pair(empty, new-context)))

              for bind-direction(result from temp-result):
                split-result = split(result.left)
                ctxt = result.right
                remaining-branches = branch-tracker.get().to-list()
                cases(Option<A.Expr>) maybe-else:
                  | some(_else) =>
                    if is-empty(remaining-branches):
                      create-err([list: C.unneccesary-else-branch(tostring(data-type), l)])
                    else:
                      has-else(l, ann, new-val, split-result, _else, ctxt)
                    end
                  | none =>
                    if is-empty(remaining-branches):
                      no-else(l, ann, new-val, split-result, ctxt)
                    else:
                      # TODO(MATT): more appropriate error here
                      create-err([list: C.non-exhaustive-pattern(remaining-branches, tostring(data-type), l)])
                    end
                end
              end
            end, checking(val, l, typ, context))
          | none =>
            create-err([list: C.cant-match-on(tostring(typ), l)])
        end
      | none => raise("No type provided for cases")
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

fun handle-branch(data-type :: Type % (is-t-data), cases-loc :: A.Loc, branch :: A.CasesBranch,
                  expect-loc :: A.Loc, maybe-check :: Option<Type>,
                  remove :: (String -> Any), context :: Context
) -> FoldResult<Pair<Pair<A.CasesBranch, Type>>, Context>:
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
        | t-variant(_, fields, _, _) =>
          cases(A.CasesBranch) branch:
            | s-cases-branch(l, pat-loc, name, args, body) =>
              fun process(new-body, _, typ, out-context):
                new-branch = A.s-cases-branch(l, pat-loc, name, args, new-body)
                fold-result(pair(pair(new-branch, typ), out-context))
              end
              body-context = fold2-strict(lam(fold-ctxt, arg, arg-typ):
                for bind(ctxt from fold-ctxt):
                  for bind(maybe-typ from to-type(arg.bind.ann, ctxt)):
                    cases(Option<Type>) maybe-typ:
                      | some(typ) =>
                        for bind(new-ctxt from satisfies-type(arg-typ, typ, ctxt)):
                          fold-result(new-ctxt.add-term-var(arg.bind.id.key(), typ))
                        end
                      | none => fold-result(ctxt.add-term-var(arg.bind.id.key(), arg-typ))
                    end
                  end
                end
              end, fold-result(context), args, fields.map(_.typ))

              cases(Option<FoldResult<Context>>) body-context:
                | none => fold-errors([list: C.incorrect-number-of-bindings(name, l, args.length(), fields.length())])
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
              fun process(new-body, _, typ, out-context):
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

fun handle-if-branch(branch :: A.IfBranch, context :: Context) -> FoldResult<Pair<A.IfBranch, Type>>:
  checking(branch.test, branch.l, t-boolean(branch.l), context).fold-bind(lam(new-test, new-context):
    synthesis(branch.body, new-context).fold-bind(
      lam(new-body, _, body-typ, out-context):
        new-branch = A.s-if-branch(branch.l, new-test, new-body)
        fold-result(pair(new-branch, body-typ))
      end)
  end)
end

fun synthesis-instantiation(l :: Loc, expr :: A.Expr, params :: List<A.Ann>, context :: Context) -> SynthesisResult:
  synthesis(expr, context).bind(lam(new-expr, tmp-typ-loc, tmp-typ, out-context):
    cases(Type) tmp-typ:
      | t-forall(introduces, onto, _) =>
        for synth-bind(new-maybe-typs from map-result(to-type(_, context), params)):
          maybe-new-typs = new-maybe-typs.foldr(lam(maybe-typ, new-typs):
            cases(Option<Type>) maybe-typ:
              | none => none
              | some(typ) =>
                cases(Option<List<Type>>) new-typs:
                  | none => none
                  | some(list-typs) => some(link(typ, list-typs))
                end
            end
          end, some(empty))
          cases(Option<List<Type>>) maybe-new-typs:
            | none => raise("Failure to determine types of forall")
            | some(new-typs) =>
              cases(Option<Type>) remove-foralls(l, introduces, onto, new-typs):
                | none => raise("Substituting types in forall failed")
                | some(new-typ) =>
                  new-inst = A.s-instantiate(l, new-expr, params)
                  synthesis-result(new-inst, l, new-typ.set-loc(l), out-context)
              end
          end
        end
      | else =>
        print("type: " + tostring(tmp-typ))
        raise("Instantiation not handled for this type")
    end
  end)
end

fun remove-foralls(l :: A.Loc, forall :: List<Type>, onto :: Type, replacements :: List<Type>) -> Option<Type>:
  for fold2-strict(curr from onto, variable from forall, replacement from replacements):
    curr.substitute(variable, replacement)
  end
end

fun meet-branch-typs(branch-typs :: List<Type>, context :: Context) -> FoldResult<Type>:
  branch-typs.foldr(lam(branch-typ, folded-typ):
    for bind(current-typ from folded-typ):
      least-upper-bound(current-typ, branch-typ, context)
    end
  end, fold-result(t-bot(A.dummy-loc)))
end

fun least-upper-bound(s, t, context):
  result-typ = _least-upper-bound(s, t, context)
  #print("")
  #print(tostring(s) + " V " + tostring(t) + " = " + tostring(result-typ))
  result-typ
end

fun _least-upper-bound(s :: Type, t :: Type, context :: Context) -> FoldResult<Type>:
  cases(FoldResult<Context>) satisfies-type(s, t, context):
    | fold-result(ctxt) => fold-result(t)
    | fold-errors(_) =>
      cases(FoldResult<Context>) satisfies-type(t, s, context):
        | fold-result(ctxt) => fold-result(s)
        | fold-errors(_) =>
          cases(Type) s:
            | t-arrow(s-args, s-ret, s-l) =>
              cases(Type) t:
                | t-arrow(t-args, t-ret, t-l) =>
                  glbs = fold2-strict(lam(glb-args, s-arg, t-arg):
                    for bind(lst from glb-args):
                      for bind(glb from greatest-lower-bound(s-arg, t-arg, context)):
                        fold-result(link(glb, lst))
                      end
                    end
                  end, fold-result(empty), s-args, t-args)

                  cases(Option<FoldResult<List<Type>>>) glbs:
                    | some(fold-args) =>
                      for bind(m-args from fold-args):
                        for bind(j-typ from least-upper-bound(s-ret, t-ret, context)):
                          fold-result(t-arrow(m-args, j-typ, t-l))
                        end
                      end
                    | none =>  # TODO(MATT): a proper error
                      fold-errors([list: C.incorrect-type(tostring(t), t.l, tostring(s), s.l)])
                  end
                | else => raise("lub not done yet")
              end
            | t-app(s-onto, s-args, s-l) => raise("lub for t-app")
            | t-record(s-fields, s-l) =>
              cases(Type) t:
                | t-record(t-fields, t-l) =>
                  t-record(meet-fields(s-fields, t-fields, context), t-l)
                | else =>
                  fold-errors([list: C.incorrect-type(tostring(t), t.l, tostring(s), s.l)])
              end
            | else =>
              fold-errors([list: C.incorrect-type(tostring(t), t.l, tostring(s), s.l)])
          end
      end
  end
end

fun greatest-lower-bound(s, t, context):
  result-typ = _greatest-lower-bound(s, t, context)
  #print("")
  #print(tostring(s) + " ^ " + tostring(t) + " = " + tostring(result-typ))
  result-typ
end

fun _greatest-lower-bound(s :: Type, t :: Type, context :: Context) -> FoldResult<Type>:
  cases(FoldResult<Context>) satisfies-type(s, t, context):
    | fold-result(ctxt) => fold-result(s)
    | fold-errors(_) =>
      cases(FoldResult<Context>) satisfies-type(t, s, context):
        | fold-result(ctxt) => fold-result(t)
        | fold-errors(_) =>
          cases(Type) s:
            | t-arrow(s-args, s-ret, s-l) =>
              cases(Type) t:
                | t-arrow(t-args, t-ret, t-l) =>
                  lubs = fold2-strict(lam(lub-args, s-arg, t-arg):
                    for bind(lst from lub-args):
                      for bind(lub from least-upper-bound(s-arg, t-arg, context)):
                        link(lub, lst)
                      end
                    end
                  end, fold-result(empty), s-args, t-args)

                  cases(Option<FoldResult<List<Type>>>) lubs:
                    | some(fold-args) =>
                      for bind(m-args from fold-args):
                        for bind(j-typ from greatest-lower-bound(s-ret, t-ret, context)):
                          fold-result(t-arrow(m-args, j-typ, t-l))
                        end
                      end
                    | none =>
                      fold-errors([list: C.incorrect-type(tostring(t), t.l, tostring(s), s.l)])
                  end
                | else => raise("glb not done yet")
              end
            | t-app(s-onto, s-args, s-l) => raise("glb for t-app")
            | t-record(s-fields, s-l) =>
              cases(Type) t:
                | t-record(t-fields, t-l) =>
                  fold-result(t-record(join-fields(s-fields, t-fields, context), t-l))
                | else => fold-result(t-bot(s-l)) # TODO(MATT): keep this?
              end
            | else =>
              fold-errors([list: C.incorrect-type(tostring(t), t.l, tostring(s), s.l)])
          end
      end
  end
end

fun meet-fields(a-fields :: List<TypeMember>, b-fields :: List<TypeMember>, context :: Context) -> List<TypeMember>:
  a-fields.foldr(lam(a-field, meet-list):
    field-name = a-field.field-name
    cases(Option<TypeMember>) type-members-lookup(b-fields, field-name):
      | some(b-field) =>
        lub = least-upper-bound(a-field.typ, b-field.typ, context)
        cases(FoldResult<Type>) lub:
          | fold-result(lub-typ) =>
            link(t-member(field-name, lub-typ, lub-typ.l), meet-list)
          | fold-errors(_) =>
            meet-list
        end
      | none => meet-list
    end
  end, empty)
end

fun join-fields(a-fields :: List<TypeMember>, b-fields :: List<TypeMember>, context :: Context) -> List<TypeMember>:
  initial-members = a-fields.foldr(lam(a-field, join-list):
    field-name = a-field.field-name
    cases(Option<TypeMember>) type-members-lookup(b-fields, field-name):
      | some(b-field) =>
        glb = greatest-lower-bound(a-field.typ, b-field.typ, context)
        cases(FoldResult<Type>) glb:
          | fold-result(glb-typ) =>
            link(t-member(field-name, glb-typ, glb-typ.l), join-list)
          | fold-errors(_) =>
            raise("I don't think this should happen")
        end
      | none =>
        link(a-field, join-list)
    end
  end, empty)

  b-fields.foldr(lam(b-field, join-list):
    field-name = b-field.field-name
    cases(Option<TypeMember>) type-members-lookup(join-list, field-name):
      | some(field) => join-list
      | none => link(b-field, join-list)
    end
  end, initial-members)
end
