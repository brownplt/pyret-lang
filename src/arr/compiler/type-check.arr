provide *
provide-types *

import type-logger as LOG

import error as ERR
import ast as A
import string-dict as SD
import file("ast-util.arr") as AU
import file("type-structs.arr") as TS
import file("type-check-structs.arr") as TCS
import file("compile-structs.arr") as C

type Type = TS.Type
type TypeMembers = TS.TypeMembers
type ConstraintSolution = TCS.ConstraintSolution
type Loc = A.Loc
type Expr = A.Expr
type Name = A.Name

local = TS.local
module-uri = TS.module-uri

t-name = TS.t-name
t-arrow = TS.t-arrow
t-app = TS.t-app
t-top = TS.t-top
t-bot = TS.t-bot
t-record = TS.t-record
t-tuple = TS.t-tuple
t-forall = TS.t-forall
t-ref = TS.t-ref
t-data-refinement = TS.t-data-refinement
t-var = TS.t-var
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

type DataType = TS.DataType

type TypeVariant = TS.TypeVariant
t-variant = TS.t-variant
t-singleton-variant = TS.t-singleton-variant

type ModuleType = TS.ModuleType
t-module = TS.t-module

is-t-app = TS.is-t-app
is-t-existential = TS.is-t-existential
is-t-forall = TS.is-t-forall
is-t-data-refinement = TS.is-t-data-refinement
is-t-top = TS.is-t-top

new-existential = TS.new-existential
new-type-var = TS.new-type-var

foldr2 = TS.foldr2

type Context = TCS.Context
type TCInfo = TCS.TCInfo

typing-context = TCS.typing-context

type TypingResult = TCS.TypingResult
typing-result = TCS.typing-result
typing-error = TCS.typing-error

type FoldResult = TCS.FoldResult
fold-result = TCS.fold-result
fold-errors = TCS.fold-errors

bind = TCS.bind
typing-bind = TCS.typing-bind
fold-bind = TCS.fold-bind
map-fold-result = TCS.map-fold-result
foldr-fold-result = TCS.foldr-fold-result
fold-typing = TCS.fold-typing
resolve-alias = TCS.resolve-alias
instantiate-data-type = TCS.instantiate-data-type
introduce-onto = TCS.introduce-onto
instantiate-object-type = TCS.instantiate-object-type
instantiate-forall = TCS.instantiate-forall

string-dict = SD.string-dict

is-s-check-test = A.is-s-check-test

################### Test Inference ####################

# an option containing the key of the function name,
# the arg-types (some of which are existentials),
# the return type (which may be an existential),
# and the existential that is the function's type
var test-inference-data :: Option<{name :: Name,
                                   arg-types :: List<Type>,
                                   ret-type :: Type,
                                   loc :: Loc,
                                   existential :: Type }> = none


#######################################################

fun option-bind<X, Y>(f :: (X -> Option<Y>), maybe-thing :: Option<X>) -> Option<Y>:
  cases(Option<X>) maybe-thing:
    | none => none
    | some(thing) => f(thing)
  end
end

fun split<X, Y>(ps :: List<{X;Y}>) -> {List<X>; List<Y>}:
  fun step(p, curr):
    {link(p.{0}, curr.{0}); link(p.{1}, curr.{1})}
  end
  ps.foldr(step, {empty; empty})
end

fun import-to-string(i :: A.ImportType, c :: C.CompileEnvironment) -> String:
  c.mods.get-value(AU.import-to-dep(i).key()).from-uri
end

# if a t-name refers to a polymorphic data type convert it to a t-app with existentials
fun add-existentials-to-data-name(typ :: Type, context :: Context) -> FoldResult<Type>:
  cases(Type) typ:
    | t-name(_, _, _, inferred) =>
      cases(Option<DataType>) context.get-data-type(typ):
        | none =>
          fold-errors([list: C.cant-typecheck("Expected a data type but got " + tostring(typ), typ.l)])
        | some(data-type) =>
          if is-empty(data-type.params):
            fold-result(typ, context)
          else:
            new-existentials = data-type.params.map(lam(a-var): new-existential(a-var.l, false) end)
            new-type = t-app(typ, new-existentials, typ.l, inferred)
            new-context = context.add-variable-set(list-to-tree-set(new-existentials))
            fold-result(new-type, new-context)
          end
      end
    | else => fold-result(typ, context)
  end
end

fun value-export-sd-to-type-sd(sd :: SD.StringDict<C.ValueExport>) -> SD.StringDict<Type>:
  tdict = for SD.fold-keys(tdict from SD.make-string-dict(), k from sd):
    tdict.set(k, sd.get-value(k).t)
  end
  tdict
end

# I believe modules is always of type SD.MutableStringDict<Loadable> -Matt
fun type-check(program :: A.Program, compile-env :: C.CompileEnvironment, modules) -> C.CompileResult<A.Program>:
  context = TCS.empty-context()
  globvs = compile-env.globals.values
  globts = compile-env.globals.types
  shadow context = globvs.fold-keys(lam(g, shadow context):
    if context.global-types.has-key(A.s-global(g).key()):
      context
    else:
      uri = globvs.get-value(g)
      # TODO(joe): type-check vars by making them refs
      context.set-global-types(context.global-types.set(A.s-global(g).key(), compile-env.mods.get-value(uri).values.get-value(g).t))
    end
  end, context)
  shadow context = globts.fold-keys(lam(g, shadow context):
    if context.aliases.has-key(A.s-type-global(g).key()):
      context
    else:
      uri = globts.get-value(g)
      cases(Option<C.Provides>) compile-env.mods.get(uri):
        | some(provs) =>
          t = cases(Option<Type>) provs.aliases.get(g):
            | none =>
              cases(Option<Type>) provs.data-definitions.get(g):
                | none => raise("Key " + g + " not found in " + torepr(provs))
                | some(v) => v
              end
            | some(v) => v
          end
          context.set-aliases(context.aliases.set(A.s-type-global(g).key(), t))
        | none =>
          raise("Could not find module " + torepr(uri) + " in " + torepr(compile-env.mods) + " in " + torepr(program.l))
      end
    end
  end, context)
  shadow context = modules.fold-keys-now(lam(k, shadow context):
    if context.modules.has-key(k):
      context
    else:
      mod = modules.get-value-now(k).provides
      key = mod.from-uri
      vals-types-dict = for SD.fold-keys(sd from [string-dict:], shadow k from mod.values):
        sd.set(k, mod.values.get-value(k).t)
      end
      val-provides = t-record(vals-types-dict, program.l, false)
      module-type = t-module(key,
                             val-provides,
                             mod.data-definitions,
                             mod.aliases)
      shadow context = context.set-modules(context.modules.set(key, module-type))
      mod.data-definitions.fold-keys(lam(d, shadow context):
        context.set-data-types(context.data-types.set(d, mod.data-definitions.get-value(d)))
      end, context)
    end
  end, context)

  cases(A.Program) program block:
    | s-program(l, _provide, provided-types, imports, body) =>
      shadow context = imports.foldl(lam(_import, shadow context):
        cases(A.Import) _import block:
          | s-import-complete(_, vals, types, file, vname, tname) =>
            key = import-to-string(file, compile-env)
            new-module-names = context.module-names.set(tname.key(), key)
            thismod = cases(Option) context.modules.get(key):
              | some(m) => m
              | none => raise(ERR.internal-error("Couldn't find " + key + " (needed for " + tname.key() + ") in context.modules:", [list: context.modules.keys-list-now()]))
            end
            thismod-provides = thismod.provides.fields
            new-global-types = context.global-types.set(vname.key(), thismod.provides)
            new-aliases = context.aliases.set(tname.key(), t-top(l, false))
            shadow new-aliases = types.foldl(lam(a, shadow new-aliases):
              cases(Option) thismod.aliases.get(a.toname()):
                | none => raise("Alias key " + a.toname() + " not found on " + torepr(thismod))
                | some(v) =>
                  new-aliases.set(a.key(), v)
              end
            end, new-aliases)
            shadow new-global-types = vals.foldl(lam(v, shadow new-global-types):
              cases(Option) thismod-provides.get(v.toname()):
                | none => raise("Value key " + v.toname() + " not found on " + torepr(thismod-provides))
                | some(typ) => new-global-types.set(v.key(), typ)
              end
            end, new-global-types)
            typing-context(new-global-types, new-aliases, context.data-types, context.modules, new-module-names, context.binds, context.constraints, context.info)
          | else => raise("type checker received incomplete import")
        end
      end, context)

      # print("\n\n")
      # each(lam(x) block:
      #   print(x)
      #   print("\n")
      # end, body.tosource().pretty(72))

      tc-result = checking(body, t-top(l, false), true, context)
      cases(TypingResult) tc-result:
        | typing-result(new-body, _, shadow context) =>
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
  end
end

fun checking(e, expect-typ, top-level, context) block:
  # print("\n\n")
  # print("checking:\n")
  # each(lam(x) block:
  #  print(x)
  #  print("\n")
  # end, e.tosource().pretty(72))
  # print("has type: " + tostring(expect-typ) + "\n    (")
  # result = _checking(e, expect-typ, top-level, context)
  # print("\n\nresult:\n")
  # print(result)
  # print("\n    )")
  # result
  _checking(e, expect-typ, top-level, context)
end

fun _checking(e :: Expr, expect-type :: Type, top-level :: Boolean, context :: Context) -> TypingResult:
  shadow context = context.add-level()
  shadow expect-type = resolve-alias(expect-type, context)
  cases(Type) expect-type:
    | t-app(onto, _, _, _) =>
      if is-t-app(onto) or is-t-forall(onto):
        introduce-onto(expect-type, context)
      else:
        fold-result(expect-type, context)
      end
    | else =>
      fold-result(expect-type, context)
  end.typing-bind(lam(shadow expect-type, shadow context):
    if is-t-existential(expect-type) or is-t-top(expect-type):
      check-synthesis(e, expect-type, top-level, context)
    else:
      cases(Expr) e:
        | s-module(l, answer, defined-values, defined-types, provided-values, provided-types, checks) =>
          checking(answer, expect-type, false, context)
            .bind(lam(new-answer, _, shadow context):
              cases(Expr) provided-values:
                | s-obj(_, fields) =>
                  foldr-fold-result(lam(field, shadow context, info):
                    cases(A.Member) field:
                      | s-data-field(data-l, name, value) =>
                        synthesis(value, false, context).fold-bind(lam(_, field-type, shadow context):
                          fold-result(TCS.tc-info(info.types.set(name, field-type.set-inferred(false)),
                                                  info.aliases,
                                                  info.data-types),
                                      context)
                        end)
                    end
                  end, fields, context, TCS.empty-info())
                | else => fold-errors([list: C.cant-typecheck("provided-values was not structured as an object.", l)])
              end.typing-bind(lam(info, shadow context):
                foldr-fold-result(lam(a-field, shadow context, shadow info):
                  fun add-aliases(typ :: Type, shadow info :: TCInfo, shadow context :: Context) -> TCInfo:
                    cases(Option<Type>) context.aliases.get(typ.key()):
                      | some(alias-typ) =>
                        shadow info = TCS.tc-info(info.types, info.aliases.set(typ.key(), alias-typ), info.data-types)
                        add-aliases(alias-typ, info, context)
                      | none =>
                        info
                    end
                  end

                  to-type(a-field.ann, context).bind(lam(maybe-type, shadow context):
                    cases(Option<Type>) maybe-type:
                      | some(typ) =>
                        new-data-types = cases(Option<DataType>) context.get-data-type(typ):
                          | some(data-type) => info.data-types.set(a-field.name, data-type)
                          | none =>
                            info.data-types
                        end
                        shadow info = add-aliases(typ, info, context)
                        fold-result(TCS.tc-info(info.types, info.aliases, new-data-types), context)
                      | none =>
                        fold-errors([list: C.cant-typecheck("provided type " + tostring(a-field.ann) + "did not resolve to a type.", l)])
                    end
                  end)
                end, provided-types, context, info).typing-bind(lam(shadow info, shadow context):
                  typing-result(A.s-module(l, new-answer, defined-values, defined-types, provided-values, provided-types, checks), expect-type, context.set-info(info))
                end)
              end)
            end)
        | s-template(l) =>
          typing-result(e, expect-type, context)
        | s-type-let-expr(l, binds, body, blocky) =>
          handle-type-let-binds(binds, context).typing-bind(lam(_, shadow context):
            checking(body, expect-type, true, context)
              .map-expr(A.s-type-let-expr(l, binds, _, blocky))
          end)
        | s-let-expr(l, binds, body, blocky) =>
          fun handler(shadow l, shadow binds, shadow body, shadow context):
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
                .map-expr(A.s-let-expr(l, new-binds, _, blocky))
                .bind(lam(new-expr, new-type, shadow context):
                  shadow context = binds.foldr(lam(binding, shadow context):
                    context.remove-binding(binding.b.id.key())
                  end, context)
                  typing-result(new-expr, new-type, context)
                end)
            end)
          end
          ignore-checker(l, binds, body, blocky, context, handler)
        | s-letrec(l, binds, body, blocky) =>
          handle-letrec-bindings(binds, top-level, context, lam(new-binds, shadow context):
            checking(body, expect-type, top-level, context)
              .map-expr(A.s-letrec(l, new-binds, _, blocky))
          end)
        | s-hint-exp(l, hints, exp) =>
          raise("checking for s-hint-exp not implemented")
        | s-instantiate(l, expr, params) =>
          check-synthesis(e, expect-type, top-level, context)
        | s-block(l, stmts) =>
          fun gen(curr, base):
            {link({curr; base.{1}}, base.{0}); t-top(l, false)}
          end
          paired-stmts = stmts.foldr(gen, {empty; expect-type}).{0}
          fold-typing(lam(stmt-type-pair, shadow context):
            checking(stmt-type-pair.{0}, stmt-type-pair.{1}, top-level, context)
          end, paired-stmts, context).typing-bind(lam(new-stmts, shadow context):
            typing-result(A.s-block(l, new-stmts), expect-type, context)
          end)
        | s-user-block(l, body) =>
          raise("s-user-block should have already been desugared")
        | s-fun(l, name, params, args, ann, doc, body, _check-loc, _check, blocky) =>
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
          lookup-id(l, id.key(), e, context).typing-bind(lam(id-type, shadow context):
            cases(Type) id-type:
              | t-ref(arg-type, _, _) =>
                checking(value, arg-type, top-level, context)
              | else =>
                typing-error([list: C.incorrect-type-expression(tostring(id-type), l, tostring(t-ref(id-type, l, false)), l, e)])
            end
          end)
        | s-if-pipe(l, branches) =>
          raise("s-if-pipe should have already been desugared")
        | s-if-pipe-else(l, branches, _else) =>
          raise("s-if-pipe-else should have already been desugared")
        | s-if(l, branches) =>
          raise("s-if should have already been desugared")
        | s-if-else(l, branches, _else, blocky) =>
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
              .map-expr(A.s-if-else(l, new-branches, _, blocky))
          end)
        | s-cases(l, typ, val, branches, b) =>
          checking-cases(l, typ, val, branches, none, expect-type, context)
        | s-cases-else(l, typ, val, branches, _else, b) =>
          checking-cases(l, typ, val, branches, some(_else), expect-type, context)
        | s-op(loc, op, l, r) =>
          raise("checking for s-op not implemented")
        | s-check-test(loc, op, refinement, l, r) =>
          if is-some(test-inference-data):
            collect-example(e, context).typing-bind(lam(_, shadow context):
              typing-result(e, expect-type, context)
            end)
          else:
            synthesis(l, false, context).bind(lam(_, left-type, shadow context):
              cases(Option<Expr>) r:
                | some(shadow r) =>
                  synthesis(r, false, context).bind(lam(_, right-type, shadow-context):
                    shadow context = context.add-constraint(left-type, right-type)
                    typing-result(e, expect-type, context)
                  end)
                | none =>
                  typing-result(e, expect-type, context)
              end
            end)
          end
        | s-check-expr(l, expr, ann) =>
          synthesis(expr, false, context) # XXX: this should probably use the annotation instead
        | s-paren(l, expr) =>
          raise("s-paren should have already been desugared")
        | s-lam(l, name, params, args, ann, doc, body, _check-loc, _check, b) =>
          check-fun(l, body, params, args, ann, expect-type, A.s-lam(l, name, params, _, _, doc, _, _check-loc, _check, b), context)
        | s-method(l, name, params, args, ann, doc, body, _check-loc, _check, b) =>
          raise("checking for s-method not implemented")
        | s-extend(l, supe, fields) =>
          check-synthesis(e, expect-type, top-level, context)
        | s-update(l, obj, fields) =>
          check-synthesis(e, expect-type, top-level, context)
        | s-tuple(l, elts) =>
          cases(Type) expect-type:
            | t-tuple(t-elts, t-l, _) =>
              if not(elts.length() == t-elts.length()):
                # TODO(MATT): better error
                typing-error([list: C.incorrect-type("a tuple type with length " + tostring(elts.length()), l, tostring(expect-type), expect-type.l)])
              else:
                result = foldr2(lam(acc, elt, elt-type):
                  acc.bind(lam(exprs, shadow context):
                    checking(elt, elt-type, false, context)
                      .fold-bind(lam(new-elt, _, shadow context):
                        fold-result(link(new-elt, exprs), context)
                      end)
                  end)
                end, fold-result(empty, context), elts.reverse(), t-elts.reverse())
                result.typing-bind(lam(exprs, shadow context):
                  typing-result(A.s-tuple(l, exprs), expect-type, context)
                end)
              end
            | else =>
              typing-error([list: C.incorrect-type(tostring(expect-type), expect-type.l, "a tuple type", l)])
          end
        | s-tuple-get(l, tup, index, index-loc) =>
          check-synthesis(e, expect-type, top-level, context)
        | s-obj(l, fields) =>
          instantiate-object-type(expect-type, context).typing-bind(lam(shadow expect-type, shadow context):
            cases(Type) expect-type:
              | t-record(t-fields, t-l, _) =>
                collect-members(fields, true, context).typing-bind(lam(field-types, shadow context):
                  temp-object-type = t-record(field-types, l, false)
                  shadow context = context.add-constraint(temp-object-type, expect-type)
                  fold-new-field-types = foldr-fold-result(lam(field, shadow context, member-types):
                    to-type-member(field, field-types.get-value(field.name), temp-object-type, true, context).bind(lam(field-type, shadow context):
                      fold-result(member-types.set(field.name, field-type), context)
                    end)
                  end, fields, context, [string-dict: ])
                  fold-new-field-types.typing-bind(lam(_, shadow context):
                    typing-result(A.s-obj(l, fields), expect-type, context)
                  end)
                end)
              | else =>
                typing-error([list: C.incorrect-type-expression(tostring(expect-type), expect-type.l, "an object type", l, e)])
            end
          end)
        | s-array(l, values) =>
          wrapped = cases(Type) expect-type:
            | t-app(rarray, args, tl, _) =>
              if TS.t-array-name == rarray:
                param-type = args.first
                for fold-typing(value from values, shadow context from context):
                  checking(value, param-type, false, context)
                end
              else:
                fold-errors([list: C.incorrect-type-expression(tostring(TS.t-array-name), l, tostring(expect-type), expect-type.l, e)])
              end
            | else =>
              fold-errors([list: C.incorrect-type-expression("a raw array", l, tostring(expect-type), expect-type.l, e)])
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
        | s-data(l, name, params, mixins, variants, shared-members, _check-loc, _check) =>
          raise("s-data should have already been desugared")
        | s-data-expr(l, name, namet, params, mixins, variants, shared-members, _check-loc, _check) =>
          raise("s-data-expr should have been handled by s-letrec")
        | s-for(l, iterator, bindings, ann, body) =>
          raise("s-for should have already been desugared")
        | s-check(l, name, body, keyword-check) =>
          typing-result(e, expect-type, context)
      end
    end.solve-bind()
  end)
end

fun synthesis(e, top-level, context) block:
  # print("\n\n")
  # print("synthesis on:\n")
  # each(lam(x) block:
  #  print(x)
  #  print("\n")
  # end, e.tosource().pretty(72))
  # print("    (")
  # result = _synthesis(e, top-level, context)
  # print("\n\nresult:\n")
  # print(result)
  # print("\n    )")
  # result
  _synthesis(e, top-level, context)
end

fun _synthesis(e :: Expr, top-level :: Boolean, context :: Context) -> TypingResult:
  shadow context = context.add-level()
  cases(Expr) e:
    | s-module(l, answer, defined-values, defined-types, provided-values, provided-types, checks) =>
      synthesis(answer, false, context)
        .map-expr(A.s-module(l, _, defined-values, defined-types, provided-values, provided-types, checks))
        .map-type(_.set-loc(l))
    | s-template(l) =>
      new-exists = new-existential(l, false)
      shadow context = context.add-variable(new-exists)
      typing-result(e, new-exists, context)
    | s-type-let-expr(l, binds, body, b) =>
      handle-type-let-binds(binds, context).typing-bind(lam(_, shadow context):
        synthesis(body, false, context)
          .map-expr(A.s-type-let-expr(l, binds, _, b))
          .map-type(_.set-loc(l))
      end)
    | s-let-expr(l, binds, body, b) =>
      fun handler(shadow l, shadow binds, shadow body, shadow context):
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
            .map-expr(A.s-let-expr(l, new-binds, _, b))
            .map-type(_.set-loc(l))
            .bind(lam(new-expr, new-type, shadow context):
              shadow context = binds.foldr(lam(binding, shadow context):
                context.remove-binding(binding.b.id.key())
              end, context)
              typing-result(new-expr, new-type, context)
            end)
        end)
      end
      ignore-checker(l, binds, body, b, context, handler)
    | s-letrec(l, binds, body, blocky) =>
      handle-letrec-bindings(binds, top-level, context, lam(new-binds, shadow context):
        synthesis(body, top-level, context)
          .map-expr(A.s-letrec(l, new-binds, _, blocky))
          .map-type(_.set-loc(l))
      end)
    | s-hint-exp(l, hints, exp) =>
      raise("synthesis for s-hint-exp not implemented")
    | s-instantiate(l, expr, params) =>
      synthesis-instantiation(l, expr, params, top-level, context)
    | s-block(l, stmts) =>
      var typ = t-top(l, false)
      fold-typing(lam(stmt, shadow context):
        synthesis(stmt, top-level, context).bind(
          lam(stmt-expr, stmt-typ, shadow context) block:
            typ := stmt-typ
            typing-result(stmt-expr, stmt-typ, context)
          end)
      end, stmts, context).typing-bind(lam(new-stmts, shadow context):
        typing-result(A.s-block(l, new-stmts), typ.set-loc(l), context)
      end)
    | s-user-block(l, body) =>
      raise("s-user-block should have already been desugared")
    | s-fun(l, name, params, args, ann, doc, body, _check-loc, _check, blocky) =>
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
      lookup-id(l, id.key(), e, context).typing-bind(lam(id-type, shadow context):
        cases(Type) id-type:
          | t-ref(arg-type, tl, _) =>
            checking(value, arg-type, top-level, context).bind(lam(new-value, _, shadow context):
              typing-result(A.s-assign(l, id, new-value), arg-type.set-loc(l), context)
            end)
          | else =>
            typing-error([list: C.incorrect-type-expression(tostring(id-type), l, tostring(t-ref(id-type, l, false)), l, e)])
        end
      end)
    | s-if-pipe(l, branches) =>
      raise("s-if-pipe should have already been desugared")
    | s-if-pipe-else(l, branches, _else) =>
      raise("s-if-pipe-else should have already been desugared")
    | s-if(l, branches) =>
      raise("s-if should have already been desugared")
    | s-if-else(l, branches, _else, b) =>
      map-fold-result(handle-if-branch, branches, context).typing-bind(lam(result, shadow context):
        synthesis(_else, false, context).bind(
          lam(new-else, else-type, shadow context):
            split-result = split(result)
            new-branches = split-result.{0}
            new-if-else = A.s-if-else(l, new-branches, new-else, b)
            meet-branch-types(link(else-type, split-result.{1}), l, context).typing-bind(lam(if-else-type, shadow context):
              typing-result(new-if-else, if-else-type.set-loc(l), context)
            end)
          end)
      end)
    | s-cases(l, typ, val, branches, b) =>
      synthesis-cases(l, typ, val, branches, none, context)
    | s-cases-else(l, typ, val, branches, _else, blocky) =>
      synthesis-cases(l, typ, val, branches, some(_else), context)
    | s-op(loc, op, l, r) =>
      raise("synthesis for s-op not implemented")
    | s-check-test(loc, op, refinement, l, r) =>
      if is-some(test-inference-data):
        collect-example(e, context).typing-bind(lam(_, shadow context):
          result-type = new-existential(loc, false)
          shadow context = context.add-variable(result-type)
          typing-result(e, result-type, context)
        end)
      else:
        synthesis(l, false, context).bind(lam(_, left-type, shadow context):
          cases(Option<Expr>) r:
            | some(shadow r) =>
              synthesis(r, false, context).bind(lam(_, right-type, shadow-context):
                shadow context = context.add-constraint(left-type, right-type)
                result-type = new-existential(loc, false)
                shadow context = context.add-variable(result-type)
                typing-result(e, result-type, context)
              end)
            | none =>
              result-type = new-existential(loc, false)
              shadow context = context.add-variable(result-type)
              typing-result(e, result-type, context)
          end
        end)
      end
    | s-check-expr(l, expr, ann) =>
      synthesis(expr, false, context) # XXX: this should probably use the annotation instead
    | s-paren(l, expr) =>
      raise("s-paren should have already been desugared")
    | s-lam(l, name, params, args, ann, doc, body, _check-loc, _check, b) =>
      synthesis-fun(l, body, params, args, ann, A.s-lam(l, name, params, _, _, doc, _, _check-loc, _check, b), top-level, context)
    | s-method(l, name, params, args, ann, doc, body, _check-loc, _check, b) =>
      raise("synthesis for s-method not implemented")
    | s-extend(l, supe, fields) =>
      synthesis(supe, top-level, context).bind(synthesis-extend(l, _, _, fields, _))
        .map-type(_.set-loc(l))
    | s-update(l, obj, fields) =>
      synthesis(obj, top-level, context).bind(synthesis-update(l, _, _, fields, _))
        .map-type(_.set-loc(l))
    | s-tuple(l, elts) =>
      result = map-fold-result(lam(elt, shadow context):
        synthesis(elt, false, context)
          .fold-bind(lam(_, elt-type, shadow context):
            fold-result(elt-type, context)
          end)
      end, elts, context)
      result.typing-bind(lam(typs, shadow context):
        typing-result(A.s-tuple(l, elts), t-tuple(typs, l, false), context)
      end)
    | s-tuple-get(l, tup, index, index-loc) =>
      synthesis(tup, top-level, context).bind(lam(new-ast, new-type, shadow context):
        synthesis-tuple-index(l, new-ast, new-type.l, new-type, index, A.s-tuple-get(_, _, _, index-loc), context)
      end)
    | s-obj(l, fields) =>
      collect-members(fields, false, context).typing-bind(lam(field-types, shadow context):
        initial-obj-type = t-record(field-types, l, false)
        fold-new-field-types = foldr-fold-result(lam(field, shadow context, new-field-types):
          to-type-member(field, field-types.get-value(field.name), initial-obj-type, false, context).bind(lam(new-field-type, shadow context):
            fold-result(new-field-types.set(field.name, new-field-type), context)
          end)
        end, fields, context, [string-dict: ])
        fold-new-field-types.typing-bind(lam(new-field-types, shadow context):
          typing-result(A.s-obj(l, fields), t-record(new-field-types, l, false), context)
        end)
      end)
    | s-array(l, values) =>
      fun process(value :: A.Expr, shadow context) -> FoldResult<{A.Expr; Type}>:
        synthesis(value, false, context).fold-bind(lam(expr, typ, shadow context):
          fold-result({expr; typ}, context)
        end)
      end

      map-fold-result(process, values, context).typing-bind(lam(result, shadow context):
        {new-values; value-types} = split(result)
        meet-branch-types(value-types, l, context).typing-bind(lam(array-type, shadow context):
          new-array = A.s-array(l, new-values)
          typing-result(new-array, t-array(array-type.set-loc(l), l), context)
        end)
      end)
    | s-construct(l, modifier, constructor, values) =>
      raise("synthesis for s-construct not implemented")
    | s-app(l, _fun, args) =>
      synthesis-app-fun(l, _fun, args, context)
        .typing-bind(lam(fun-type, shadow context):
          synthesis-spine(fun-type, A.s-app(l, _fun, _), args, l, context)
        end)
    | s-prim-app(l, _fun, args) =>
      lookup-id(l, _fun, e, context).typing-bind(lam(arrow-type, shadow context):
        synthesis-spine(arrow-type, A.s-prim-app(l, _fun, _), args, l, context)
          .map-type(_.set-loc(l))
      end)
    | s-prim-val(l, name) =>
      raise("synthesis for s-prim-val not implemented")
    | s-id(l, id) =>
      lookup-id(l, id.key(), e, context).typing-bind(lam(id-type, shadow context):
        typing-result(e, id-type, context)
      end)
    | s-id-var(l, id) =>
      lookup-id(l, id.key(), e, context).typing-bind(lam(id-type, shadow context):
        cases(Type) id-type:
          | t-ref(arg-type, _, _) =>
            typing-result(e, arg-type.set-loc(l), context)
          | else =>
            typing-error([list: C.incorrect-type-expression(tostring(id-type), id-type.l, tostring(t-ref(id-type, l, false)), l, e)])
        end
      end)
    | s-id-letrec(l, id, safe) =>
      lookup-id(l, id.key(), e, context).typing-bind(lam(id-type, shadow context):
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
        synthesis-field(l, new-ast, new-type, field, A.s-dot, context)
      end)
    | s-get-bang(l, obj, field) =>
      synthesis(obj, top-level, context).bind(lam(new-ast, new-type, shadow context):
        synthesis-field(l, new-ast, new-type, field, A.s-get-bang, context)
      end).bind(lam(new-get-bang, field-type, shadow context):
        cases(Type) field-type:
          | t-ref(typ, _, _) =>
            typing-result(new-get-bang, typ.set-loc(l), context)
          | else =>
            typing-error([list: C.incorrect-type-expression(tostring(field-type), field-type.l, "a ref type", l, e)])
        end
      end)
    | s-bracket(l, obj, field) =>
      raise("synthesis for s-bracket not implemented")
    | s-data(l, name, params, mixins, variants, shared-members, _check-loc, _check) =>
      raise("s-data should have already been desugared")
    | s-data-expr(l, name, namet, params, mixins, variants, shared-members, _check-loc, _check) =>
      raise("s-data-expr should have been handled by s-letrec")
    | s-for(l, iterator, bindings, ann, body, blocky) =>
      raise("s-for should have already been desugared")
    | s-check(l, name, body, keyword-check) =>
      result-type = new-existential(l, false)
      shadow context = context.add-variable(result-type)
      typing-result(e, result-type, context)
  end.solve-bind()
end

fun synthesis-spine(fun-type :: Type, recreate :: (List<Expr> -> Expr), args :: List<Expr>, app-loc :: Loc, context :: Context) -> TypingResult:
  shadow context = context.add-level()
  instantiate-forall(fun-type, context).typing-bind(lam(shadow fun-type, shadow context):
    cases(Type) fun-type:
      | t-arrow(arg-types, ret-type, _, _) =>
        if not(args.length() == arg-types.length()):
          typing-error([list: C.incorrect-number-of-args(recreate(args), fun-type)])
        else:
          foldr2(lam(acc, arg, arg-type):
            acc.bind(lam(exprs, shadow context):
              checking(arg, arg-type, false, context)
                .fold-bind(lam(new-arg, _, shadow context):
                  fold-result(link(new-arg, exprs), context)
                end)
            end)
          end, fold-result(empty, context), args, arg-types).typing-bind(lam(exprs, shadow context):
            typing-result(recreate(exprs), ret-type, context)
          end)
        end
      | t-existential(id, l, _) =>
        existential-args = args.map(lam(_): new-existential(l, false) end)
        existential-ret = new-existential(l, false)
        shadow context = context.add-variable-set(list-to-tree-set(link(existential-ret, existential-args)))
        new-arrow = t-arrow(existential-args, existential-ret, l, false)
        shadow context = context.add-constraint(fun-type, new-arrow)
        result = foldr2(lam(acc, arg, arg-type):
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
      | t-app(onto, type-args, _, _) =>
        introduce-onto(fun-type, context).typing-bind(lam(shadow onto, shadow context):
          synthesis-spine(onto, recreate, args, app-loc, context)
        end)
      | t-bot(l, inferred) =>
        fold-typing(lam(arg, shadow context):
          checking(arg, t-top(l, false), false, context)
        end, args, context).typing-bind(lam(new-args, shadow context):
          typing-result(recreate(new-args), t-bot(l, inferred), context)
        end)
      | else =>
        typing-error([list: C.apply-non-function(recreate(args), fun-type)])
    end.solve-bind().map-type(_.set-loc(app-loc))
  end)
end

fun check-synthesis(e :: Expr, expect-type :: Type, top-level :: Boolean, context :: Context) -> TypingResult:
  synthesis(e, top-level, context).bind(lam(new-expr, new-type, shadow context):
    # TODO(MATT): decide whether this should return new-type or expect-type
    typing-result(new-expr, new-type, context.add-constraint(new-type, expect-type))
  end)
end

fun lookup-id(blame-loc :: A.Loc, id-key :: String, id-expr :: Expr, context :: Context) -> FoldResult<Type>:
  if context.binds.has-key(id-key):
    fold-result(context.binds.get-value(id-key).set-loc(blame-loc), context)
  else if context.global-types.has-key(id-key):
    fold-result(context.global-types.get-value(id-key).set-loc(blame-loc), context)
  else:
    fold-errors([list: C.unbound-id(id-expr)])
  end
end

# TODO(MATT): this should require unifying of same-named methods
#             should it require unifying types of same-named members?
# Type checks data types
# Returns the list of all relevant letrec bindings
# Use the context returned from this function
fun handle-datatype(data-type-bind :: A.LetrecBind, bindings :: List<A.LetrecBind>,
context :: Context) -> FoldResult<List<A.LetrecBind>>:
  data-expr = data-type-bind.value
  cases(Expr) data-expr:
    | s-data-expr(l, name, namet, params, mixins, variants, fields, _check-loc, _check) =>
      shadow context = context.add-level()
      brander-type = t-name(local, namet, l, false)
      t-vars = params.map(t-var(_, l, false))
      map-fold-result(collect-variants, variants, context).bind(lam(initial-variant-types, shadow context):
        predicate-type = if is-empty(t-vars):
          t-arrow([list: brander-type], t-boolean(l), l, false)
        else:
          t-forall(t-vars, t-arrow([list: t-app(brander-type, t-vars, l, false)], t-boolean(l), l, false), l, false)
        end
        initial-data-fields = SD.make-string-dict()
          .set(name, predicate-type)
        data-fields = initial-variant-types.foldl(lam(variant-type, data-fields):
          data-fields
            .set(variant-type.name, mk-constructor-type(variant-type, brander-type, t-vars))
            .set("is-" + variant-type.name, predicate-type)
        end, initial-data-fields)
        shadow context = context.add-binding(data-type-bind.b.id.key(), t-record(data-fields, l, false))
        map-fold-result(lam(binding, shadow context):
          synthesis(binding.value, false, context).fold-bind(lam(new-value, result-type, shadow context):
            fold-result(A.s-letrec-bind(binding.l, binding.b, new-value), context.add-binding(binding.b.id.key(), result-type))
          end)
        end, bindings, context).bind(lam(new-bindings, shadow context):
          collect-members(fields, true, context).bind(lam(initial-shared-field-types, shadow context):
            initial-data-type = t-data(name, t-vars, initial-variant-types, initial-shared-field-types, l)
            shadow context = context.set-data-types(context.data-types.set(namet.key(), initial-data-type))
            shadow context = merge-common-fields(initial-variant-types, l, context)
            map-fold-result(lam(variant, shadow context):
              check-variant(variant, initial-data-type.get-variant-value(variant.name), brander-type, t-vars, context)
            end, variants, context).bind(lam(new-variant-types, shadow context):
              variant-type-fields = new-variant-types.map(lam(var-type):
                var-type.fields.foldr(lam({field-name; field-type}, all-fields):
                  all-fields.set(field-name, field-type)
                end, var-type.with-fields)
              end)
              variants-meet = cases(List<TypeMembers>) variant-type-fields:
                | empty => empty
                | link(first, rest) =>
                  cases(List<TypeMembers>) rest:
                    | empty => first
                    | link(_, _) =>
                      rest.foldr(meet-fields(_, _, l, context), first)
                  end
              end
              extended-shared-field-types = variants-meet.fold-keys(lam(key, extended-shared-field-types):
                extended-shared-field-types.set(key, variants-meet.get-value(key))
              end, initial-shared-field-types)
              shared-data-type = t-data(name, t-vars, new-variant-types, extended-shared-field-types, l)
              shadow context = context.set-data-types(context.data-types.set(namet.key(), shared-data-type))
              foldr-fold-result(lam(field, shadow context, new-shared-field-types):
                check-shared-field(field, initial-shared-field-types, brander-type, context).bind(lam(field-type, shadow context):
                  fold-result(new-shared-field-types.set(field.name, field-type), context)
                end)
              end, fields, context, SD.make-string-dict()).bind(lam(new-shared-field-types, shadow context):
                final-shared-field-types = variants-meet.fold-keys(lam(key, final-shared-field-types):
                  final-shared-field-types.set(key, variants-meet.get-value(key))
                end, new-shared-field-types)
                final-data-type = t-data(name, t-vars, new-variant-types, final-shared-field-types, l)
                context.solve-level().bind(lam(solution, shadow context):
                  solved-data-type = solution.apply-data-type(final-data-type)
                  shadow context = context.set-data-types(context.data-types.set(namet.key(), solved-data-type))
                  fold-result(link(data-type-bind, new-bindings), context)
                end)
              end)
            end)
          end)
        end)
      end)
    | else => raise("Expected an s-data-expr")
  end
end

# Checks with-members on a variant
fun check-variant(variant :: A.Variant, variant-type :: TS.TypeVariant, data-type :: Type, t-vars :: List<Type>, context :: Context) -> FoldResult<TypeVariant>:
  refined-type = t-data-refinement(if is-empty(t-vars): data-type else: t-app(data-type, t-vars, data-type.l, false) end, variant.name, data-type.l, false)

  foldr-fold-result(lam(member, shadow context, member-types):
    member-type = variant-type.with-fields.get-value(member.name)
    to-type-member(member, member-type, refined-type, true, context).bind(lam(checked-member-type, shadow context):
      fold-result(member-types.set(member.name, checked-member-type), context)
    end)
  end, variant.with-members, context, SD.make-string-dict()).bind(lam(member-types, shadow context):
    new-variant-type = cases(TypeVariant) variant-type:
      | t-variant(name, fields, _, l) => t-variant(name, fields, member-types, l)
      | t-singleton-variant(name, _, l) => t-singleton-variant(name, member-types, l)
    end
    fold-result(new-variant-type, context)
  end)
end

fun check-shared-field(field :: A.Member, field-types :: TypeMembers, data-type :: Type, context :: Context) -> FoldResult<Type>:
  field-type = field-types.get-value(field.name)
  to-type-member(field, field-type, data-type, true, context)
end

# doesn't check data-fields that aren't methods or functions
# only checks data-fields that are functions when type-check-functions is true
fun to-type-member(member :: A.Member, typ :: Type, self-type :: Type, type-check-functions :: Boolean, context :: Context) -> FoldResult<Type>:
  fun add-self-type(fun-type :: Type) -> Type:
    cases(Type) fun-type:
      | t-arrow(args, ret, l, inferred) =>
        t-arrow(link(self-type, args), ret, l, inferred)
      | t-forall(introduces, onto-arrow, l, inferred) =>
        cases(Type) onto-arrow:
        | t-arrow(args, ret, inner-l, inner-inferred) =>
          t-forall(introduces, t-arrow(link(self-type, args), ret, inner-l, inner-inferred), l, inferred)
        | else =>
          raise("method type is not a function (this shouldn't happen")
        end
      | else =>
        raise("method type is not a function (this shouldn't happen")
    end
  end

  fun remove-self-type(fun-type :: Type) -> Type:
    cases(Type) fun-type:
      | t-arrow(args, ret, l, inferred) =>
        t-arrow(args.rest, ret, l, inferred)
      | t-forall(introduces, onto-arrow, l, inferred) =>
        cases(Type) onto-arrow:
        | t-arrow(args, ret, inner-l, inner-inferred) =>
          t-forall(introduces, t-arrow(args.rest, ret, inner-l, inner-inferred), l, inferred)
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
        | s-method(m-l, m-name, params, args, ann, doc, body, _check-loc, _check, b) =>
          new-type = add-self-type(typ)
          check-fun(m-l, body, params, args, ann, new-type, A.s-method(m-l, m-name, params, _, _, doc, _, _check-loc, _check, b), context)
            .fold-bind(lam(_, out-type, shadow context):
              fold-result(remove-self-type(out-type), context)
            end)
        | s-lam(l-l, _, params, args, ann, doc, body, _check-loc, _check, b) =>
          if type-check-functions:
            checking(value, typ, false, context)
              .fold-bind(lam(new-ast, new-type, shadow context):
                fold-result(new-type, context)
              end)
          else:
            fold-result(typ, context)
          end
        | else =>
          fold-result(typ, context)
      end
    | s-method-field(m-l, name, params, args, ann, doc, body, _check-loc, _check, b) =>
      new-type = add-self-type(typ)
      check-fun(m-l, body, params, args, ann, new-type, A.s-method(m-l, name, params, _, _, doc, _, _check-loc, _check, b), context)
        .fold-bind(lam(_, out-type, shadow context):
          fold-result(remove-self-type(out-type), context)
        end)
    | s-mutable-field(l, name, ann, value) =>
      raise("Mutable fields not handled yet")
  end
end

fun collect-variants(variant :: A.Variant, context :: Context) -> FoldResult<TypeVariant>:
  cases(A.Variant) variant:
    | s-variant(l, constr-loc, name, members, with-members) =>
      fun process-member(member, shadow context):
        wrap = cases(A.VariantMemberType) member.member-type:
          | s-normal => lam(x): x.set-loc(member.l) end
          | s-mutable => lam(x): t-ref(x.set-loc(member.l), member.l, false) end
        end
        to-type(member.bind.ann, context).bind(lam(maybe-type, shadow context):
          cases(Option<Type>) maybe-type:
            | none => fold-errors([list: C.cant-typecheck("No type annotation provided on member", l)])
            | some(typ) =>
              fold-result(wrap(typ), context)
          end
        end)
      end

      foldr-fold-result(lam(member, shadow context, type-members):
        process-member(member, context)
          .bind(lam(member-type, shadow context):
            fold-result(link({member.bind.id.toname(); member-type}, type-members), context)
          end)
      end, members, context, empty).bind(lam(type-members, shadow context):
        collect-members(with-members, true, context).bind(lam(type-with-members, shadow context):
          type-variant = t-variant(name, type-members, type-with-members, l)
          fold-result(type-variant, context)
        end)
      end)
    | s-singleton-variant(l, name, with-members) =>
      collect-members(with-members, true, context).bind(lam(type-with-members, shadow context):
        type-variant = t-singleton-variant(name, type-with-members, l)
        fold-result(type-variant, context)
      end)
  end
end

fun mk-constructor-type(variant-typ :: TypeVariant, brander-typ :: Type, params :: List<Type>) -> Type:
  inner-type = if is-empty(params):
    brander-typ
  else:
    t-app(brander-typ, params, variant-typ.l, false)
  end
  refined-type = t-data-refinement(inner-type, variant-typ.name, variant-typ.l, false).set-loc(variant-typ.l)
  cases(TypeVariant) variant-typ:
    | t-variant(name, fields, _, l) =>
      field-types = fields.map(lam({field-name; field-type}):
        cases(Type) field-type:
          | t-ref(ref-typ, _, _) => ref-typ
          | else => field-type
        end
      end)
      if is-empty(params):
        t-arrow(field-types, refined-type, l, false)
      else:
        t-forall(params, t-arrow(field-types, refined-type, l, false), l, false)
      end
    | t-singleton-variant(name, _, l) =>
      if is-empty(params):
        refined-type
      else:
        t-forall(params, refined-type, l, false)
      end
  end
end

# collect-functions: if true gather annotations from lambda terms
#                    else synthesize lambda terms
fun collect-members(members :: List<A.Member>, collect-functions :: Boolean, context :: Context) -> FoldResult<TypeMembers>:
  foldr-fold-result(lam(member, shadow context, type-members):
    collect-member(member, collect-functions, context)
      .bind(lam(member-type, shadow context):
        fold-result(type-members.set(member.name, member-type), context)
      end)
  end, members, context, SD.make-string-dict())
end

fun collect-member(member :: A.Member, collect-functions :: Boolean, context :: Context) -> FoldResult<Type>:
  cases(A.Member) member:
    | s-data-field(l, name, value) =>
      cases(Expr) value:
        | s-method(m-l, _, params, args, ann, _, _, _, _, _) =>
          cases(List<A.Bind>) args:
            | empty =>
              fold-errors([list: C.method-missing-self(value)])
            | link(self, rest) =>
              collect-bindings(rest, context).bind(lam(bindings, shadow context):
                lam-to-type(bindings, m-l, params, args.rest, ann, not(collect-functions), context)
              end)
          end
        | s-lam(l-l, _, params, args, ann, _, _, _, _, _) =>
          if collect-functions:
            collect-bindings(args, context).bind(lam(bindings, shadow context):
              lam-to-type(bindings, l-l, params, args, ann, false, context)
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
    | s-method-field(l, name, params, args, ann, doc, body, _check-loc, _check) =>
      cases(List<A.Bind>) args:
        | empty =>
          fold-errors([list: C.method-missing-self(member)])
        | link(self, rest) =>
          collect-bindings(rest, context).bind(lam(bindings, shadow context):
            lam-to-type(bindings, l, params, args.rest, ann, not(collect-functions), context)
          end)
      end
    | s-mutable-field(l, name, ann, value) =>
      raise("Type checker does not handle mutable fields yet")
  end
end

fun checking-cases(l :: Loc, ann :: A.Ann, val :: Expr, branches :: List<A.CasesBranch>, maybe-else :: Option<Expr>, expect-type :: Type, context :: Context) -> TypingResult:
  handle-cases(l, ann, val, branches, maybe-else, some(expect-type), context, checking-cases-has-else(expect-type), checking-cases-no-else(expect-type))
end

fun checking-cases-has-else(expect-type :: Type):
  lam(l :: Loc, ann :: A.Ann, new-val :: Expr, split-result :: {List<A.CasesBranch>; List<Type>}, _else :: Expr, context :: Context) -> TypingResult:
    checking(_else, expect-type, false, context).bind(lam(new-else, new-type, shadow context):
      new-cases = A.s-cases-else(l, ann, new-val, split-result.{0}, new-else, false)
      typing-result(new-cases, new-type, context)
    end)
  end
end

fun checking-cases-no-else(expect-type :: Type):
  lam(l :: Loc, ann :: A.Ann, new-val :: Expr, split-result :: {List<A.CasesBranch>; List<Type>}, context :: Context) -> TypingResult:
    new-cases = A.s-cases(l, ann, new-val, split-result.{0}, false)
    typing-result(new-cases, expect-type, context)
  end
end

fun synthesis-cases(l :: Loc, ann :: A.Ann, val :: Expr, branches :: List<A.CasesBranch>, maybe-else :: Option<A.Expr>, context :: Context) -> TypingResult:
  handle-cases(l, ann, val, branches, maybe-else, none, context, synthesis-cases-has-else, synthesis-cases-no-else)
    .map-type(_.set-loc(l))
end

fun synthesis-cases-has-else(l :: Loc, ann :: A.Ann, new-val :: A.Expr, split-result :: {List<A.CasesBranch>; List<Type>}, _else :: A.Expr, context :: Context) -> TypingResult:
  synthesis(_else, false, context).bind(
    lam(new-else, else-type, shadow context):
      new-cases = A.s-cases-else(l, ann, new-val, split-result.{0}, new-else)
      meet-branch-types(link(else-type, split-result.{1}), l, context).typing-bind(lam(branches-type, shadow context):
        typing-result(new-cases, branches-type.set-loc(l), context)
      end)
    end)
end

fun synthesis-cases-no-else(l :: Loc, ann :: A.Ann, new-val :: Expr, split-result :: {List<A.CasesBranch>; List<Type>}, context :: Context) -> TypingResult:
  new-cases = A.s-cases(l, ann, new-val, split-result.{0}, false)
  meet-branch-types(split-result.{1}, l, context).typing-bind(lam(branches-type, shadow context):
    typing-result(new-cases, branches-type.set-loc(l), context)
  end)
end

fun handle-cases(l :: Loc, ann :: A.Ann, val :: Expr, branches :: List<A.CasesBranch>, maybe-else :: Option<Expr>, maybe-expect :: Option<Type>, context :: Context, has-else, no-else) -> TypingResult:
  to-type(ann, context).typing-bind(lam(maybe-type, shadow context):
    cases(Option<Type>) maybe-type:
      | some(typ) =>
        shadow context = context.add-level()
        add-existentials-to-data-name(typ, context).typing-bind(lam(cases-type, shadow context):
          synthesis(val, false, context).bind(lam(new-val, val-type, shadow context):
            shadow context = context.add-constraint(val-type, cases-type)
            typing-result(new-val, val-type, context)
          end).solve-bind().bind(lam(new-val, val-type, shadow context):
            instantiate-data-type(val-type, context).typing-bind(lam(data-type, shadow context):
              branch-tracker = track-branches(data-type)
              temp-result = map-fold-result(lam(branch, shadow context):
                maybe-key-to-update = cases(Expr) val:
                  | s-id(_, val-id) =>
                    some(val-id.key())
                  | s-id-var(_, val-id) =>
                    some(val-id.key())
                  | s-id-letrec(_, val-id, _) =>
                    some(val-id.key())
                  | else =>
                    none
                end
                shadow context = cases(Option<String>) maybe-key-to-update:
                  | some(key-to-update) =>
                    context.add-binding(key-to-update, t-data-refinement(val-type, branch.name, l, true))
                  | none =>
                    context
                end
                branch-result = handle-branch(data-type, l, branch, maybe-expect, branch-tracker.remove, context)
                branch-result.bind(lam(branch-type-pair, shadow context):
                  shadow context = cases(Option<String>) maybe-key-to-update:
                    | some(key-to-update) =>
                      context.add-binding(key-to-update, val-type)
                    | none =>
                      context
                  end
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
            end)
          end)
        end)
      | none =>
        typing-error([list: C.cant-typecheck("Could not resolve type on cases statement", l)])
    end
  end)
end

fun handle-branch(data-type :: DataType, cases-loc :: A.Loc, branch :: A.CasesBranch, maybe-check :: Option<Type>, remove :: (TypeVariant -> Any), context :: Context) -> FoldResult<{A.CasesBranch; Type}>:
  fun handle-body(variant :: TypeVariant, body :: A.Expr, process, shadow context :: Context) block:
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

  cases(Option<TypeVariant>) data-type.get-variant(branch.name):
    | some(tv) =>
      cases(TypeVariant) tv:
        | t-variant(_, fields, _, _) =>
          cases(A.CasesBranch) branch:
            | s-cases-branch(l, pat-loc, name, args, body) =>
              fun process(new-body, typ, shadow context):
                new-branch = A.s-cases-branch(l, pat-loc, name, args, new-body)
                fold-result({new-branch; typ}, context)
              end

              if not(args.length() == fields.length()):
                fold-errors([list: C.incorrect-number-of-bindings(branch, tv)])
              else:
                shadow context = context.add-level()
                foldr2(lam(fold-context, arg, {arg-name; arg-type}):
                  fold-context.bind(lam(_, shadow context):
                    to-type(arg.bind.ann, context).bind(lam(maybe-type, shadow context):
                      cases(Option<Type>) maybe-type:
                        | some(typ) =>
                          shadow context = context.add-constraint(arg-type, typ)
                          fold-result(nothing, context.add-binding(arg.bind.id.key(), typ))
                        | none =>
                          fold-result(nothing, context.add-binding(arg.bind.id.key(), arg-type))
                      end
                    end)
                  end)
                end, fold-result(nothing, context), args, fields).bind(lam(_, shadow context):
                  context.solve-level().bind(lam(solution, shadow context):
                    shadow context = context.substitute-in-binds(solution)
                    handle-body(tv, body, process, context)
                      .bind(lam(result, shadow context):
                        shadow context = args.foldr(lam(arg, shadow context):
                          context.remove-binding(arg.bind.id.key())
                        end, context)
                        fold-result(result, context)
                      end)
                  end)
                end)
              end
            | s-singleton-cases-branch(l, _, name, _) =>
              fold-errors([list: C.cases-singleton-mismatch(name, l, false)])
          end
        | t-singleton-variant(_, _, _) =>
          cases(A.CasesBranch) branch:
            | s-cases-branch(l, _, name, _, _) =>
              fold-errors([list: C.cases-singleton-mismatch(name, l, true)])
            | s-singleton-cases-branch(l, pat-loc, name, body) =>
              fun process(new-body, typ, shadow context):
                new-branch = A.s-singleton-cases-branch(l, pat-loc, name, new-body)
                fold-result({new-branch; typ}, context)
              end
              handle-body(tv, body, process, context)
          end
      end
    | none =>
      fold-errors([list: C.unneccesary-branch(branch, data-type, cases-loc)])
  end
end

fun track-branches(data-type :: DataType) ->
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

fun synthesis-field(access-loc :: Loc, obj :: Expr, obj-type :: Type, field-name :: String, recreate :: (Loc, Expr, String -> Expr), context :: Context) -> TypingResult:
  instantiate-object-type(obj-type, context).typing-bind(lam(shadow obj-type, shadow context):
    cases(Type) obj-type:
      | t-record(fields, _, _) =>
        cases(Option<Type>) fields.get(field-name):
          | some(field-typ) =>
            typing-result(recreate(access-loc, obj, field-name), field-typ, context)
          | none =>
            synthesized-type = new-existential(access-loc, false)
            shadow context = context.add-variable(synthesized-type)
                                    .add-field-constraint(obj-type, field-name, synthesized-type)
            typing-result(recreate(access-loc, obj, field-name), synthesized-type, context)
        end
      | t-existential(_, _, _) =>
        synthesized-type = new-existential(access-loc, false)
        shadow context = context.add-variable(synthesized-type)
                                .add-field-constraint(obj-type, field-name, synthesized-type)
        typing-result(recreate(access-loc, obj, field-name), synthesized-type, context)
      | else =>
        instantiate-data-type(obj-type, context).typing-bind(lam(data-type, shadow context):
          cases(Option<Type>) data-type.fields.get(field-name):
            | some(field-typ) =>
              typing-result(recreate(access-loc, obj, field-name), field-typ, context)
            | none =>
              typing-error([list: C.object-missing-field(field-name, tostring(obj-type), obj-type.l, access-loc)])
          end
        end)
    end
  end)
end

fun synthesis-app-fun(app-loc :: Loc, _fun :: Expr, args :: List<Expr>, context :: Context) -> FoldResult<Type>:
  fun choose-type(method-name :: String) -> FoldResult<Type>:
    # there should be two args here because its a binop
    obj-exists = new-existential(args.get(0).l, false)
    other-type = new-existential(args.get(1).l, false)
    ret-type = new-existential(app-loc, false)
    arrow-type = t-arrow([list: obj-exists, other-type], ret-type, app-loc, false)
    shadow context = context.add-variable(obj-exists).add-variable(other-type).add-variable(ret-type).add-field-constraint(obj-exists, method-name, t-arrow([list: other-type], ret-type, app-loc, false))
    fold-result(arrow-type, context)
  end
  cases(Expr) _fun:
    | s-id(fun-loc, id) =>
      ask:
        | id == A.s-global("_plus") then: choose-type("_plus")
        | id == A.s-global("_times") then: choose-type("_times")
        | id == A.s-global("_divide") then: choose-type("_divide")
        | id == A.s-global("_minus") then: choose-type("_minus")
        | id == A.s-global("_lessthan") then: choose-type("_lessthan")
        | id == A.s-global("_lessequal") then: choose-type("_lessequal")
        | id == A.s-global("_greaterthan") then: choose-type("_greaterthan")
        | id == A.s-global("_greaterequal") then: choose-type("_greaterequal")
        | otherwise:
          synthesis(_fun, false, context).fold-bind(lam(_, new-type, shadow context):
            fold-result(new-type, context)
          end)
      end
    | else =>
      synthesis(_fun, false, context).fold-bind(lam(_, new-type, shadow context):
        fold-result(new-type, context)
      end)
  end
end

fun handle-type-let-binds(bindings :: List<A.TypeLetBind>, context :: Context) -> FoldResult<Nothing>:
  map-fold-result(lam(binding, shadow context):
    cases(A.TypeLetBind) binding:
      | s-type-bind(l, name, params, ann) =>
        to-type(ann, context).bind(lam(maybe-typ, shadow context):
          cases(Option<Type>) maybe-typ:
            | none => # TODO(MATT): is this correct?
              fold-errors([list: C.unbound-type-id(ann)])
            | some(typ) =>
              alias-type =
                if is-empty(params):
                  typ
                else:
                  forall = for map(param from params): t-var(param, l, false) end
                  t-forall(forall, typ, l, false)
                end
              shadow context = context.set-aliases(context.aliases.set(name.key(), alias-type))
              fold-result(nothing, context)
          end
        end)
      | s-newtype-bind(l, name, namet) =>
        typ = t-name(local, namet, l, false)
        namet-key = namet.key()
        shadow context = context.set-aliases(context.aliases.set(name.key(), typ))
        shadow context = context.add-binding(namet-key, t-record([string-dict:
          "test", t-arrow([list: typ], t-boolean(l), l, false),
          "brand", t-arrow([list: t-top(l, false)], typ, l, false)], l, false))
        fold-result(nothing, context)
    end
  end, bindings, context)
end

# type checks letrec bindings
# types body with the provided function
fun handle-letrec-bindings(binds :: List<A.LetrecBind>, top-level :: Boolean, context :: Context, handle-body :: (List<A.LetrecBind>, Context -> TypingResult)) -> TypingResult:
  shadow context = context.add-level()
  collect-letrec-bindings(binds, top-level, context).typing-bind(lam(collected, shadow context):
    {bindings-to-type; collected-types} = collected.bindings
    data-bindings = collected.data-bindings
    shadow context = context.add-dict-to-bindings(collected-types)
    foldr-fold-result(lam(data-binding, shadow context, typed-bindings):
      handle-datatype(data-binding.{0}, data-binding.{1}, context).bind(lam(new-bindings, shadow context):
        fold-result(new-bindings.append(typed-bindings), context)
      end)
    end, data-bindings, context, empty).typing-bind(lam(new-data-binds, shadow context):
      fold-rhs = fold-typing(lam(binding, shadow context):
        cases(A.LetrecBind) binding:
          | s-letrec-bind(l2, b, value) =>
            expected-type = collected-types.get-value(b.id.key())
            cases(Option) context.constraints.example-types.get(expected-type.key()) block:
              | some({_; partial-type; _; _}) =>
                test-inference-data := some({name: b.id,
                                             arg-types: partial-type.arg-types,
                                             ret-type: partial-type.ret-type,
                                             loc: partial-type.loc,
                                             existential: expected-type})
                if A.is-s-lam(value) block:
                  check-block = value._check.value
                  result = checking(check-block, t-top(l2, false), false, context)
                  test-inference-data := none
                  result.bind(lam(_, result-type, shadow context):
                    typing-result(value, result-type, context)
                  end)
                else:
                  raise("the right hand side should be a lambda")
                end
              | none =>
                shadow context = context.add-level()
                free-vars = expected-type.free-variables()
                shadow context = context.add-variable-set(free-vars)
                checking(value, expected-type, false, context).bind(lam(new-ast, new-type, shadow context):
                  context.solve-level().typing-bind(lam(solution, shadow context):
                    shadow context = context.substitute-in-binds(solution)
                    shadow new-type = solution.generalize(solution.apply(new-type))
                    shadow context = context.add-binding(b.id.key(), new-type)
                    if A.is-s-lam(value):
                      cases(Option<Expr>) value._check:
                        | some(check-block) =>
                          checking(check-block, t-top(value._check-loc.value, false), false, context).bind(lam(_, _, shadow context):
                            typing-result(new-ast, new-type, context)
                          end)
                        | none =>
                          typing-result(new-ast, new-type, context)
                      end
                    else:
                      typing-result(new-ast, new-type, context)
                    end
                  end)
                end)
            end
        end
      end, bindings-to-type, context)
      fold-rhs.typing-bind(lam(new-rhs, shadow context):
        new-binds = map2(lam(binding, rhs):
          cases(A.LetrecBind) binding:
            | s-letrec-bind(l2, b, _) =>
              A.s-letrec-bind(l2, b, rhs)
          end
        end, bindings-to-type, new-rhs)
        all-new-binds = new-data-binds.append(new-binds)
        context.solve-level().typing-bind(lam(solution, shadow context):
          shadow context = context.substitute-in-binds(solution)
          handle-body(all-new-binds, context)
            .bind(lam(new-ast, new-type, shadow context):
              shadow context = binds.foldr(lam(binding, shadow context):
                context.remove-binding(binding.b.id.key())
              end, context)
              typing-result(new-ast, new-type, context)
            end)
        end)
      end)
    end)
  end)
end

# Separates out s-data-expr related bindings (which will always come first)
# Collects the annotated types of all other bindings
fun collect-letrec-bindings(binds :: List<A.LetrecBind>, top-level :: Boolean, context :: Context) -> FoldResult<{data-bindings :: List<{A.LetrecBind; List<A.LetrecBind>}>, bindings :: {List<A.LetrecBind>; SD.StringDict<Type>}}>:
  fun helper(shadow binds, shadow top-level, shadow context, data-bindings :: List<{A.LetrecBind; List<A.LetrecBind>}>, bindings :: {List<A.LetrecBind>; SD.StringDict<Type>}):
    cases(List<A.LetrecBind>) binds:
      | link(first-bind, rest-binds) =>
        first-value = first-bind.value
        cases(Expr) first-value:
          | s-data-expr(_, _, _, _, _, variants, _, _, _) =>
            num-data-binds = (2 * variants.length()) + 1
            split-list = split-at(num-data-binds, rest-binds)
            data-binds = split-list.prefix
            remaining-binds = split-list.suffix
            helper(remaining-binds, top-level, context, link({first-bind; data-binds}, data-bindings), bindings)
          | else =>
            collect-bindings([list: first-bind.b], context).bind(lam(collected, shadow context):
              shadow context = context.add-dict-to-bindings(collected)
              initial-type = collected.get-value(first-bind.b.id.key())
              if is-t-existential(initial-type):
                cases(Expr) first-bind.value:
                  | s-lam(lam-l, _, lam-params, lam-args, lam-ann, _, _, _, _check, _) =>
                    collect-bindings(lam-args, context).bind(lam(arg-coll, shadow context) block:
                      cases(Option<Expr>) _check:
                        | some(check-block) =>
                          lam-to-type(arg-coll, lam-l, lam-params, lam-args, lam-ann, false, context).bind(lam(lam-type, temp-context):
                            cases(Type) lam-type block:
                              | t-arrow(temp-args, temp-ret, temp-l, _) =>
                                if lam-type.free-variables().size() > 0:
                                  new-exists = new-existential(temp-l, true)
                                  shadow temp-context = temp-context.add-variable(new-exists)
                                  shadow temp-context = temp-context.add-example-variable(new-exists, temp-args, temp-ret, temp-l, checking(first-bind.value, _, top-level, _))
                                  fold-result(new-exists, temp-context)
                                else:
                                  lam-to-type(arg-coll, lam-l, lam-params, lam-args, lam-ann, top-level, context)
                                end
                              | else =>
                                lam-to-type(arg-coll, lam-l, lam-params, lam-args, lam-ann, top-level, context)
                            end
                          end)
                        | none =>
                          lam-to-type(arg-coll, lam-l, lam-params, lam-args, lam-ann, top-level, context)
                      end.bind(lam(lam-type, shadow context):
                        fold-result(collected.set(first-bind.b.id.key(), lam-type), context)
                      end)
                    end)
                  | else =>
                    fold-result(collected, context)
                end
              else:
                fold-result(collected, context)
              end.bind(lam(collected-bindings, shadow context):
                key = first-bind.b.id.key()
                helper(rest-binds, top-level, context, data-bindings, {bindings.{0}.append([list: first-bind]); bindings.{1}.set(key, collected-bindings.get-value(key))})
              end)
            end)
        end
      | empty =>
        fold-result({data-bindings: data-bindings, bindings: bindings}, context)
    end
  end
  helper(binds, top-level, context, empty, {empty; SD.make-string-dict()})
end

# Collects the annotated bindings and produces an existential for bindings that aren't annotated
# The existential is added to the current level's variables
fun collect-bindings(binds :: List<A.Bind>, context :: Context) -> FoldResult<SD.StringDict<Type>>:
  foldr-fold-result(lam(binding, shadow context, dict):
    to-type(binding.ann, context).bind(lam(maybe-type, shadow context):
      new-type = cases(Option<Type>) maybe-type:
        | some(typ) => typ.set-loc(binding.l)
        | none =>
          cases(A.Name) binding.id:
            | s-atom(base, _) =>
              if base == "$underscore":
                t-top(binding.l, false)
              else:
                new-existential(binding.l, true)
              end
            | else =>
              new-existential(binding.l, true)
          end
      end
      shadow context = context.add-variable(new-type)
      fold-result(dict.set(binding.id.key(), new-type), context)
    end)
  end, binds, context, SD.make-string-dict())
end

# adds any existentials generated to the current level's variables
fun lam-to-type(coll :: SD.StringDict<Type>, l :: Loc, params :: List<A.Name>, args :: List<A.Bind>, ret-ann :: A.Ann, top-level :: Boolean, context :: Context) -> FoldResult<Type>:
  to-type(ret-ann, context).bind(lam(maybe-type, shadow context):
    ret-type = cases(Option<Type>) maybe-type:
      | some(typ) => typ
      | none => new-existential(l, true)
    end
    shadow context = context.add-variable(ret-type)
    fold-arg-types = map-fold-result(lam(arg, shadow context):
      arg-type = coll.get-value(arg.id.key())
      if top-level and is-t-existential(arg-type):
        fold-errors([list: C.toplevel-unann(arg)])
      else:
        shadow context = context.add-variable(arg-type)
        fold-result(arg-type, context)
      end
    end, args, context)

    fold-arg-types.bind(lam(arg-types, shadow context):
      arrow-type = t-arrow(arg-types, ret-type, l, false)
      if is-empty(params):
        fold-result(arrow-type, context)
      else:
        if is-t-existential(ret-type):
          fold-errors([list: C.polymorphic-return-type-unann(l)])
        else:
          forall = for map(param from params): t-var(param, l, false) end
          fold-result(t-forall(forall, arrow-type, l, false), context)
        end
      end
    end)
  end)
end

# TODO(MATT): this should not generalize the arguments
fun synthesis-fun(l :: Loc, body :: Expr, params :: List<A.Name>, args :: List<A.Bind>, ret-ann :: A.Ann, recreate :: (List<A.Bind>, A.Ann, A.Expr -> A.Expr), top-level :: Boolean, context :: Context) -> TypingResult:
  fun set-ret-type(lam-type :: Type, ret-type :: Type) -> Type:
    cases(Type) lam-type:
      | t-arrow(arg-types, _, a-l, inferred) =>
        t-arrow(arg-types, ret-type, a-l, inferred)
      | t-forall(introduces, onto, f-l, inferred) =>
        cases(Type) onto:
          | t-arrow(arg-types, _, a-l, inner-inferred) =>
            t-forall(introduces, t-arrow(arg-types, ret-type, a-l, inner-inferred), f-l, inferred)
          | else => raise("This shouldn't happen (non-function type lambda)")
        end
      | else => raise("This shouldn't happen (non-function type lambda)")
    end
  end

  shadow context = context.add-level()
  collected = collect-bindings(args, context)
  collected.typing-bind(lam(coll, shadow context):
    fold-lam-type = lam-to-type(coll, l, params, args, ret-ann, top-level, context)
    fold-lam-type.typing-bind(lam(lam-type, shadow context):
      fold-ret-type = cases(Type) lam-type:
        | t-arrow(_, ret-type, _, _) =>
          fold-result(ret-type, context)
        | t-forall(_, onto, _, _) =>
          cases(Type) onto:
            | t-arrow(_, ret-type, _, _) =>
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
  end).solve-bind()
end

fun synthesis-let-bind(binding :: A.LetBind, context :: Context) -> TypingResult:
  shadow context = context.add-level()
  cases(A.LetBind) binding:
    | s-let-bind(l, b, value) =>
      to-type(b.ann, context).typing-bind(lam(maybe-type, shadow context):
        ann-type = cases(Option<Type>) maybe-type:
          | some(typ) => typ
          | none => new-existential(l, true)
        end
        shadow context = context.add-variable(ann-type)
        checking(value, ann-type, false, context)
          .bind(lam(new-value, new-type, shadow context):
            typing-result(new-value, new-type, context.add-binding(b.id.key(), new-type))
          end)
      end)
    | s-var-bind(l, b, value) =>
      to-type(b.ann, context).typing-bind(lam(maybe-type, shadow context):
        ann-type = cases(Option<Type>) maybe-type:
          | some(typ) => typ
          | none => new-existential(l, true)
        end
        shadow context = context.add-variable(ann-type)
        checking(value, ann-type, false, context)
          .bind(lam(new-value, new-type, shadow context):
            typing-result(new-value, t-ref(new-type, l, false), context.add-binding(b.id.key(), t-ref(new-type, l, false)))
          end)
      end)
  end.solve-bind()
end

fun synthesis-extend(update-loc :: Loc, obj :: Expr, obj-type :: Type, fields :: List<A.Member>, context :: Context) -> TypingResult:
  collect-members(fields, false, context).typing-bind(lam(new-members, shadow context):
    instantiate-object-type(obj-type, context).typing-bind(lam(shadow obj-type, shadow context):
      cases(Type) obj-type:
        | t-record(t-fields, _, inferred) =>
          final-fields = new-members.fold-keys(lam(key, final-fields):
            final-fields.set(key, new-members.get-value(key))
          end, t-fields)
          typing-result(A.s-extend(update-loc, obj, fields), t-record(final-fields, update-loc, inferred), context)
        | t-existential(_, l, _) =>
          typing-error([list: C.unable-to-infer(l)])
        | else =>
          typing-error([list: C.incorrect-type-expression(tostring(obj-type), obj-type.l, "an object type", update-loc, obj)])
      end
    end)
  end)
end

fun synthesis-update(update-loc :: Loc, obj :: Expr, obj-type :: Type, fields :: List<A.Member>, context :: Context) -> TypingResult:
  collect-members(fields, false, context).typing-bind(lam(new-members, shadow context):
    instantiate-object-type(obj-type, context).typing-bind(lam(shadow obj-type, shadow context):
      cases(Type) obj-type:
        | t-record(t-fields, _, inferred) =>
          foldr-fold-result(lam(key, shadow context, final-fields):
            cases(Option<Type>) t-fields.get(key):
              | none =>
                fold-errors([list: C.object-missing-field(key, tostring(obj-type), obj-type.l, update-loc)])
              | some(old-type) =>
                cases(Type) old-type:
                  | t-ref(onto, l, ref-inferred) =>
                    new-type = new-members.get-value(key)
                    fold-result(final-fields.set(key, t-ref(new-type, new-type.l, ref-inferred)), context)
                  | else =>
                    fold-errors([list: C.incorrect-type(tostring(old-type), old-type.l, tostring(t-ref(old-type, update-loc, false)), update-loc)])
                end
            end
          end, new-members.keys-list(), context, t-fields).typing-bind(lam(final-fields, shadow context):
            typing-result(A.s-update(update-loc, obj, fields), t-record(final-fields, update-loc, inferred), context)
          end)
        | t-existential(_, l, _) =>
          typing-error([list: C.unable-to-infer(l)])
        | else =>
          typing-error([list: C.incorrect-type-expression(tostring(obj-type), obj-type.l, "an object type", update-loc, obj)])
      end
    end)
  end)
end

# TODO(MATT): this should not generalize the arguments
fun check-fun(fun-loc :: Loc, body :: Expr, params :: List<A.Name>, args :: List<A.Bind>, ret-ann :: A.Ann, expect-type :: Type, recreate :: (List<A.Bind>, A.Ann, Expr -> Expr), context :: Context) -> TypingResult:
  shadow context = context.add-level()
  lam-bindings = collect-bindings(args, context)
  # TODO(MATT): checking when polymorphic lambda but non-polymorphic type

  cases(Type) expect-type:
    | t-arrow(expect-args, ret-type, _, _) =>
      lam-bindings.typing-bind(lam(temp-lam-binds, shadow context):
        if not(temp-lam-binds.count() == expect-args.length()):
          expected = "a function with " + tostring(expect-args.length()) + " arguments"
          found = "a function with " + tostring(args.length()) + " arguments"
          typing-error([list: C.incorrect-type(expected, fun-loc, found, expect-type.l)])
        else:
          shadow temp-lam-binds = foldr2(lam(lam-binds, arg, expect-arg-type):
            key = arg.id.key()
            bound-type = lam-binds.get-value(key)
            if is-t-existential(bound-type):
              lam-binds.set(key, expect-arg-type)
            else:
              lam-binds
            end
          end, temp-lam-binds, args, expect-args)
          {lam-binds; shadow context} = params.foldr(lam(param, {lam-binds; shadow context}):
            new-exists = new-existential(fun-loc, false)
            new-binds = lam-binds.fold-keys(lam(key, binds):
              binds.set(key, binds.get-value(key).substitute(new-exists, t-var(param, fun-loc, false)))
            end, lam-binds)
            {new-binds; context.add-variable(new-exists)}
          end, {temp-lam-binds; context})
          lam-arg-types = map(lam(arg): lam-binds.get-value(arg.id.key()) end, args)
          shadow context = foldr2(lam(shadow context, lam-arg-type, expect-arg-type):
            context.add-constraint(lam-arg-type, expect-arg-type)
          end, context.add-dict-to-bindings(lam-binds), lam-arg-types, expect-args)
          body-result = checking(body, ret-type, false, context)
          body-result.bind(lam(new-body, new-ret-type, shadow context):
            typing-result(recreate(args, ret-ann, new-body), expect-type, context)
          end)
        end
      end)
    | t-forall(introduces, onto, l, inferred) =>
      check-fun(fun-loc, body, params, args, ret-ann, onto, recreate, context)
        .map-type(t-forall(introduces, _, l, inferred))
    | t-existential(id, _, _) =>
      check-synthesis(recreate(args, ret-ann, body), expect-type, false, context)
    | t-app(onto, type-args, _, _) =>
      fold-onto = introduce-onto(expect-type, context)
      fold-onto.typing-bind(lam(shadow onto, shadow context):
        check-fun(fun-loc, body, params, args, ret-ann, onto, recreate, context)
      end)
    | t-top(l, _) =>
      lam-bindings.typing-bind(lam(new-binds, shadow context):
        body-result = checking(body, expect-type, false, context.add-dict-to-bindings(new-binds))
        body-result.bind(lam(new-body, new-type, shadow context):
          typing-result(recreate(args, ret-ann, new-body), expect-type, context)
        end)
      end)
    | else =>
      typing-error([list: C.incorrect-type(tostring(expect-type), expect-type.l, "a function", fun-loc)])
  end.solve-bind()
end

# TODO(MATT): this might be totally broken
# generalization can flip the order of variables
fun synthesis-instantiation(l :: Loc, expr :: Expr, params :: List<A.Ann>, top-level :: Boolean, context :: Context) -> TypingResult:
  synthesis(expr, top-level, context).bind(lam(new-expr, tmp-type, shadow context):
    shadow tmp-type = if TCS.is-constraint-system(context.constraints):
      tmp-solution = TCS.constraint-solution(context.constraints.variables, [string-dict: ])
      tmp-solution.generalize(tmp-type)
    else:
      tmp-type
    end
    cases(Type) tmp-type:
      | t-forall(introduces, onto, _, _) =>
        map-fold-result(to-type, params, context).typing-bind(lam(new-maybe-types, shadow context):
          maybe-new-types = new-maybe-types.foldr(lam(maybe-type, new-types):
            for option-bind(typ from maybe-type):
              for option-bind(list-types from new-types):
                some(link(typ, list-types))
              end
            end
          end, some(empty))
          cases(Option<List<Type>>) maybe-new-types:
            | none => typing-error([list: C.cant-typecheck("Failure to determine types of forall", l)])
            | some(new-types) =>
              if not(new-types.length() == introduces.length()):
                typing-error([list: C.cant-typecheck("Expected " + tostring(introduces.length()) + " type arguments, but got " + tostring(new-types.length()) + " arguments.", l)])
              else:
                new-type = foldr2(lam(curr, variable, replacement):
                  curr.substitute(replacement, variable)
                end, onto, introduces, new-types)
                new-inst = A.s-instantiate(l, new-expr, params)
                typing-result(new-inst, new-type.set-loc(l), context)
              end
          end
        end)
      | t-existential(_, exists-l, _) =>
        typing-error([list: C.unable-to-infer(exists-l)])
      | else => typing-error([list: C.incorrect-type(tostring(tmp-type), tmp-type.l, "a polymorphic type", l)])
    end
  end)
end

fun handle-if-branch(branch :: A.IfBranch, context :: Context) -> FoldResult<{A.IfBranch; Type}>:
  checking(branch.test, t-boolean(branch.l), false, context).fold-bind(
    lam(new-test, _, shadow context):
      synthesis(branch.body, false, context).fold-bind(
        lam(new-body, body-type, shadow context):
          new-branch = A.s-if-branch(branch.l, new-test, new-body)
          fold-result({new-branch; body-type}, context)
        end)
    end)
end

fun synthesis-tuple-index(access-loc :: Loc, tup :: Expr, tup-type-loc :: Loc, tup-type :: Type, index :: Number, recreate :: (Loc, Expr, Number -> Expr), context :: Context) -> TypingResult:
  non-tup-err = typing-error([list: C.incorrect-type(tostring(tup-type), tup-type-loc, "a tuple type", access-loc)])
  tuple-view(access-loc, tup-type-loc, tup-type,
  lam(l, maybe-tup-members):
    cases(Option<List<Type>>) maybe-tup-members:
      | some(tup-members) =>
        if index >= tup-members.length():
          typing-error([list: C.tuple-too-small(index, tup-members.length(), "{" + tup-members.map(tostring).join-str("; ") + "}", l, access-loc)])
        else:
          typing-result(recreate(l, tup, index), tup-members.get(index), context)
        end
      | none =>
        non-tup-err
        # TODO(MATT): decide about this
    end
  end, context)
end

fun tuple-view(access-loc :: Loc, tup-type-loc :: Loc, tup-type :: Type,
                handle :: (Loc, Option<List<Type>> -> TypingResult),
                context :: Context) -> TypingResult:
  non-tup-err = typing-error([list: C.incorrect-type(tostring(tup-type), tup-type-loc, "a tuple type", access-loc)])
  cases(Type) tup-type:
    | t-tuple(fields, _, _) =>
      handle(tup-type-loc, some(fields))
    | t-forall(introduces, onto, l, _) =>
      new-existentials = introduces.map(lam(a-var): new-existential(a-var.l, false) end)
      new-tup-type = foldr2(lam(new-onto, a-var, a-exists):
        new-onto.substitute(a-exists, a-var)
      end, onto, introduces, new-existentials)
      shadow context = context.add-variable-set(list-to-tree-set(new-existentials))
      tuple-view(access-loc, tup-type-loc, new-tup-type, handle, context)
    | t-existential(_, exists-l, _) =>
      typing-error([list: C.unable-to-infer(exists-l)])
    | else => non-tup-err
  end
end

fun meet-branch-types(branch-types :: List<Type>, loc :: Loc, context :: Context) -> FoldResult<Type> block:
  new-exists = new-existential(loc, false)
  shadow context = context.add-level().add-variable(new-exists)
  shadow context = branch-types.foldr(lam(branch-type, shadow context):
    context.add-constraint(new-exists, branch-type)
  end, context)
  context.solve-level().bind(lam(solution, shadow context):
    meet-type = solution.generalize(solution.apply(new-exists))
    fold-result(meet-type, context)
  end)
end

# Adds constraints between methods with the same name across all variants
fun merge-common-fields(variants :: List<TypeVariant>, data-loc :: Loc, context :: Context) -> Context:
  fun get-in-all(field-name :: String, members :: List<TypeMembers>) -> Option<{field-name :: String, types :: List<Type>}>:
    members.foldl(lam(member, maybe-field-types):
      for option-bind(field-types from maybe-field-types):
        for option-bind(member-field-type from member.get(field-name)):
          some({field-name: field-name, types: link(member-field-type, field-types.types)})
        end
      end
    end, some({field-name: field-name, types: empty}))
  end

  fields-to-merge = cases (List<TypeMembers>) variants:
    | empty => empty
    | link(first, rest) =>
      with-fields = variants.map(lam(variant): variant.with-fields end)
      first.with-fields.keys-list().map(lam(field-name):
        get-in-all(field-name, with-fields)
      end).filter(is-some).map(_.value)
  end
  fields-to-merge.foldr(lam(field-and-types, shadow context):
    merge-existential = new-existential(data-loc, false)
    shadow context = context.add-variable(merge-existential)
    field-and-types.types.foldr(lam(field-type, shadow context):
      context.add-constraint(merge-existential, field-type)
    end, context)
  end, context)
end

fun meet-fields(a-fields :: TypeMembers, b-fields :: TypeMembers, loc :: Loc, context :: Context) -> TypeMembers:
  fun introduce(typ :: Type, temp-context :: Context) -> {Type; Context}:
    cases(Type) typ:
      | t-forall(introduces, onto, _, _) =>
        new-existentials = introduces.map(lam(a-var): new-existential(a-var.l, false) end)
        new-onto = foldr2(lam(new-onto, a-var, a-exists):
          new-onto.substitute(a-exists, a-var)
        end, onto, introduces, new-existentials)
        {new-onto; temp-context.add-variable-set(list-to-tree-set(new-existentials))}
      | else => {typ; temp-context}
    end
  end

  a-fields.fold-keys(lam(a-field-name, meet-members):
    cases(Option<Type>) b-fields.get(a-field-name):
      | none => meet-members
      | some(b-type) =>
        a-type = a-fields.get-value(a-field-name)
        temp-existential = new-existential(loc, false)
        temp-context = context.add-level().add-variable(temp-existential)
        {shadow a-type; shadow temp-context} = introduce(a-type, temp-context)
        {shadow b-type; shadow temp-context} = introduce(b-type, temp-context)
        shadow temp-context = temp-context.add-constraint(temp-existential, a-type).add-constraint(temp-existential, b-type)
        fold-solution = temp-context.solve-level()
        cases(FoldResult) fold-solution:
          | fold-errors(_) => meet-members
          | fold-result(solution, shadow context) =>
            meet-type = solution.generalize(solution.apply(temp-existential))
            meet-members.set(a-field-name, meet-type)
        end
    end
  end, SD.make-string-dict())
end

fun gather-provides(_provide :: A.Provide, context :: Context) -> FoldResult<TCInfo>:
  cases(A.Provide) _provide:
    | s-provide-complete(_, values, aliases, data-definitions) =>
      initial-info = TCS.tc-info([string-dict: ], context.info.aliases, context.info.data-types)
      fold-values-info = foldr-fold-result(lam(value, shadow context, info):
        value-key = value.v.key()
        if info.types.has-key(value-key):
          fold-result(info, context)
        else:
          typ = context.info.types.get-value(value-key).set-inferred(false)
          fold-result(TCS.tc-info(info.types.set(value-key, typ), info.aliases, info.data-types), context)
        end
      end, values, context, initial-info)
      fold-values-info.bind(lam(values-info, shadow context):
        fold-aliases-info = foldr-fold-result(lam(_alias, shadow context, info):
          alias-key = _alias.in-name.key()
          if info.aliases.has-key(alias-key):
            fold-result(info, context)
          else:
            typ = context.aliases.get-value(alias-key)
            fold-result(TCS.tc-info(info.types, info.aliases.set(alias-key, typ), info.data-types), context)
          end
        end, aliases, context, values-info)
        fold-aliases-info.bind(lam(aliases-info, shadow context):
          foldr-fold-result(lam(data-type, shadow context, info):
            data-key = data-type.d.key()
            if info.data-types.has-key(data-key):
              fold-result(info, context)
            else:
              typ = context.data-types.get-value(data-key)
              fold-result(TCS.tc-info(info.types, info.aliases, info.data-types.set(data-key, typ)), context)
            end
          end, data-definitions, context, aliases-info)
        end)
      end)
    | else => raise("Haven't handled anything but s-provide-complete")
  end
end

fun to-type(in-ann :: A.Ann, context :: Context) -> FoldResult<Option<Type>>:
  cases(A.Ann) in-ann:
    | a-blank =>
      fold-result(none, context)
    | a-any(l) =>
      fold-result(some(t-top(l, false)), context)
    | a-name(l, id) =>
      typ = context.aliases.get(id.key())
      cases(Option<Type>) typ:
        | some(t) =>
          result-type = resolve-alias(t, context).set-loc(l)
          fold-result(some(result-type), context)
        | none => fold-errors([list: C.unbound-type-id(in-ann)])
      end
    | a-type-var(l, id) =>
      fold-result(some(t-var(id, l, false)), context)
    | a-arrow(l, args, ret, _) =>
      fold-arg-typs = map-fold-result(lam(arg, shadow context):
        to-type(arg, context).bind(lam(maybe-new-typ, shadow context):
          cases(Option<Type>) maybe-new-typ:
            | none =>
              fold-errors([list: C.cant-typecheck("no annotation provided on " + tostring(arg), l)])
            | some(new-typ) =>
              fold-result(new-typ, context)
          end
        end)
      end, args, context)

      fold-arg-typs.bind(lam(arg-typs, shadow context):
        to-type(ret, context).bind(lam(maybe-ret-typ, shadow context):
          cases(Option<Type>) maybe-ret-typ:
            | none =>
              fold-errors([list: C.cant-typecheck("no annotation provided on " + tostring(ret), l)])
            | some(ret-typ) =>
              fold-result(some(t-arrow(arg-typs, ret-typ, l, false)), context)
          end
        end)
      end)
    | a-method(l, args, ret, _) =>
      fold-errors([list: C.cant-typecheck("a-method not yet implemented", l)])
    | a-record(l, fields) =>
      fields-result = foldr-fold-result(lam(field, shadow context, fields-dict):
        to-type(field.ann, context).bind(lam(maybe-typ, shadow context):
          cases(Option<Type>) maybe-typ:
            | none =>
              fold-errors([list: C.cant-typecheck("no annotation provided on " + tostring(field), l)])
            | some(typ) =>
              fold-result(fields-dict.set(field.name, typ), context)
          end
        end)
      end, fields, context, SD.make-string-dict())

      fields-result.bind(lam(members, shadow context):
        fold-result(some(t-record(members, l, false)), context)
      end)
    | a-tuple(l, elts) =>
      fold-elt-typs = map-fold-result(lam(elt, shadow context):
        to-type(elt, context).bind(lam(maybe-new-typ, shadow context):
          cases(Option<Type>) maybe-new-typ:
            | none =>
              new-exists = new-existential(l, true)
              shadow context = context.add-variable(new-exists)
              fold-result(new-exists, context)
            | some(new-typ) =>
              fold-result(new-typ, context)
          end
        end)
      end, elts, context)
      fold-elt-typs.bind(lam(new-elts, shadow context):
        fold-result(some(t-tuple(new-elts, l, false)), context)
      end)
    | a-app(l, ann, args) =>
      to-type(ann, context).bind(lam(maybe-typ, shadow context):
        cases(Option<Type>) maybe-typ:
          | none =>
            fold-errors([list: C.cant-typecheck("no annotation provided on " + tostring(ann), l)])
          | some(typ) =>
            args-result = map-fold-result(lam(arg, shadow context): to-type(arg, context) end, args, context)
            args-result.bind(lam(maybe-arg-types, shadow context):
              fold-arg-typs = map-fold-result(lam(maybe-arg-typ, shadow context):
                cases(Option<Type>) maybe-arg-typ:
                  | none =>
                    fold-errors([list: C.cant-typecheck("no annotation provided on app argument", l)])
                  | some(arg-typ) =>
                    fold-result(arg-typ, context)
                end
              end, maybe-arg-types, context)
              fold-arg-typs.bind(lam(arg-typs, shadow context):
                fold-result(some(t-app(typ, arg-typs, l, false)), context)
              end)
            end)
        end
      end)
    | a-pred(l, ann, exp) =>
      to-type(ann, context).bind(lam(maybe-typ, shadow context):
        cases(Option<Type>) maybe-typ:
          | some(typ) =>
            expect-type = t-arrow([list: typ], t-boolean(l), l, false)
            checking(exp, expect-type, false, context).fold-bind(lam(_, _, shadow context):
              fold-result(some(typ), context)
            end)
          | none =>
            fold-errors([list: C.cant-typecheck("missing annotation on " + tostring(ann), l)])
        end
      end)
    | a-dot(l, obj, field) =>
      key = obj.key()
      origin = context.module-names.get(key)
      cases(Option) origin:
        | none =>
          fold-errors([list: C.no-module(l, obj.toname())])
        | some(mod) =>
          t-mod = context.modules.get-value(mod)
          if t-mod.aliases.has-key(field) block:
            typ = resolve-alias(t-mod.aliases.get-value(field), context)
            fold-result(some(typ), context)
          else:
            fold-errors([list: C.unbound-type-id(in-ann)])
          end
      end
    | a-checked(checked, residual) =>
      fold-errors([list: C.cant-type-check("a-checked should not be appearing before type checking", A.dummy-loc)])
  end
end

# ignores the desugared checker output
fun ignore-checker(l :: Loc, binds :: List<A.LetBind>, body :: Expr, blocky, context :: Context, handler :: (Loc, List<A.LetBind>, Expr, Context -> TypingResult)) -> TypingResult:
  if binds.length() == 1:
    binding = binds.get(0)
    cases(A.Name) binding.b.id:
      | s-atom(base, _) =>
        if string-length(base) >= 19:
          name = string-substring(base, 0, 19)
          if name == "result-after-checks":
            cases(Expr) body:
              | s-block(_, stmts) =>
                maybe-module = stmts.last()
                if A.is-s-module(maybe-module):
                  shadow context = context.add-binding(binding.b.id.key(), t-top(l, false))
                  checking(maybe-module, t-top(l, false), true, context)
                    .bind(lam(new-module, new-type, shadow context):
                      shadow context = context.remove-binding(binding.b.id.key())
                      typing-result(A.s-let-expr(l, binds, body, blocky), new-type, context)
                    end)
                else:
                  handler(l, binds, body, context)
                end
              | else =>
                handler(l, binds, body, context)
            end
          else:
            handler(l, binds, body, context)
          end
        else:
          handler(l, binds, body, context)
        end
      | else =>
        handler(l, binds, body, context)
    end
  else:
    handler(l, binds, body, context)
  end
end

################### Test Inference ####################

fun collect-example(e :: Expr%(is-s-check-test), context :: Context) -> FoldResult<Nothing>:
  cases(Option) test-inference-data:
    | none => fold-result(nothing, context)
    | some(inference-data) =>
      cases(A.Expr) e:
        | s-check-test(l, op, refinement, lhs, rhs) =>
          cases(A.CheckOp) op:
            | s-op-is(_) =>
              cases(Option<A.Expr>) refinement:
                | some(_) => fold-result(nothing, context)
                | none =>
                  cases(A.Expr) lhs:
                    | s-app(_, _fun, args) =>
                      maybe-id = cases(A.Expr) _fun:
                        | s-id(_, id) => some(id)
                        | s-id-var(_, id) => some(id)
                        | s-id-letrec(_, id, _) => some(id)
                        | else => none
                      end
                      cases(Option<Name>) maybe-id block:
                        | none => fold-result(nothing, context)
                        | some(id) =>
                          if inference-data.name == id block:
                            fold-arg-types = foldr2(lam(fold-result-args, arg, expect-arg):
                              fold-result-args.bind(lam(result-args, shadow context):
                                if is-t-existential(expect-arg):
                                  synthesis(arg, false, context).fold-bind(lam(_, result-type, shadow context):
                                    fold-result(link(result-type, result-args), context)
                                  end)
                                else:
                                  checking(arg, expect-arg, false, context).fold-bind(lam(_, _, shadow context):
                                    fold-result(link(expect-arg, result-args), context)
                                  end)
                                end
                              end)
                            end, fold-result(empty, context), args, inference-data.arg-types)
                            fold-arg-types.bind(lam(arg-types, shadow context):
                              expect-ret-type = inference-data.ret-type
                              if is-t-existential(expect-ret-type):
                                synthesis(rhs.value, false, context).fold-bind(lam(_, result-type, shadow context):
                                  fold-result(result-type, context)
                                end)
                              else:
                                checking(rhs.value, expect-ret-type, false, context).fold-bind(lam(_, _, shadow context):
                                  fold-result(expect-ret-type, context)
                                end)
                              end.bind(lam(ret-type, shadow context):
                                shadow context = context.add-example-type(inference-data.existential, t-arrow(arg-types, ret-type, inference-data.loc, true))
                                fold-result(nothing, context)
                              end)
                            end)
                          else:
                            fold-result(nothing, context)
                          end
                      end
                    | else => fold-result(nothing, context)
                  end
              end
            | else => fold-result(nothing, context)
          end
        | else => fold-result(nothing, context)
      end
  end
end

#######################################################
