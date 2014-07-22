#lang pyret

provide *
provide-types *
import ast as A
import string-dict as SD
import srcloc as SL
import "compiler/compile-structs.arr" as C
import "compiler/type-structs.arr" as TS
import "compiler/type-check-structs.arr" as TCS
import "compiler/type-constraints.arr" as TC

type Loc                  = SL.Srcloc

type Type                 = TS.Type
t-name                    = TS.t-name
t-var                     = TS.t-var
t-arrow                   = TS.t-arrow
t-top                     = TS.t-top
t-bot                     = TS.t-bot
t-app                     = TS.t-app
t-record                  = TS.t-record

type TypeVariable         = TS.TypeVariable
t-variable                = TS.t-variable

type TypeMember           = TS.TypeMember
t-member                  = TS.t-member

type TypeVariant          = TS.TypeVariant
t-variant                 = TS.t-variant
t-singleton-variant       = TS.t-singleton-variant

type DataType             = TS.DataType
t-datatype                = TS.t-datatype

type Pair                 = TS.Pair
pair                      = TS.pair


t-number                  = TS.t-number
t-string                  = TS.t-string
t-boolean                 = TS.t-boolean
t-srcloc                  = TS.t-srcloc

least-upper-bound         = TS.least-upper-bound
greatest-lower-bound      = TS.greatest-lower-bound

type TypeConstraint       = TC.TypeConstraint
type TypeConstraints      = TC.TypeConstraints
generate-constraints      = TC.generate-constraints
empty-type-constraints    = TC.empty-type-constraints

type SynthesisResult      = TCS.SynthesisResult
type CheckingResult       = TCS.CheckingResult
type FoldResult           = TCS.FoldResult
type CheckingMapResult    = TCS.CheckingMapResult

synthesis-result          = TCS.synthesis-result
synthesis-if-result       = TCS.synthesis-if-result
synthesis-binding-result  = TCS.synthesis-binding-result
synthesis-err             = TCS.synthesis-err
checking-result           = TCS.checking-result
checking-err              = TCS.checking-err
fold-result               = TCS.fold-result
fold-errors               = TCS.fold-errors
checking-map              = TCS.checking-map
checking-map-errors       = TCS.checking-map-errors

bind                      = TCS.bind
map-bind                  = TCS.map-bind
check-bind                = TCS.check-bind
synth-bind                = TCS.synth-bind
fold-bind                 = TCS.fold-bind

map-synthesis             = TCS.map-synthesis
foldl2-result             = TCS.foldl2-result
foldr-result              = TCS.foldr-result
map2-checking             = TCS.map2-checking
map-checking              = TCS.map-checking
map-result                = TCS.map-result

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



fun <B> identity(b :: B) -> B:
  b
end

fun <B,D> split(ps :: List<Pair<A,B>>) -> Pair<List<A>,List<B>>:
  fun step(p, curr):
    pair(link(p.left, curr.left), link(p.right, curr.right))
  end
  ps.foldr(step, pair(empty, empty))
end

data TCInfo:
  | tc-info(typs       :: SD.StringDict<Type>,
            aliases    :: SD.StringDict<Type>,
            data-exprs :: SD.StringDict<DataType>,
            branders   :: SD.StringDict<Type>,
            errors     :: { insert :: (C.CompileError -> List<C.CompileError>),
                            get    :: (-> List<C.CompileError>)})
end

fun to-type-member(field :: A.Member, info :: TCInfo) -> FoldResult<Pair<A.Member,TypeMember>>:
  cases(A.Member) field:
    | s-data-field(l, name, value) =>
      if A.is-s-method(value) or A.is-s-lam(value): # TODO(cody): Type-check methods and lambdas.
        fold-result(pair(A.s-data-field(l, name, value), t-member(name, t-bot)))
      else:
        synthesis(value, info).fold-bind(
        lam(new-value, value-typ):
          fold-result(pair(A.s-data-field(l, name, new-value), t-member(name, value-typ)))
        end)
      end
    | s-mutable-field(l, name, ann, value) =>
      raise("NYI(to-type-member): s-mutable-field")
    | s-once-field(l, name, ann, value) =>
      raise("NYI(to-type-member): s-once-field")
    | s-method(l, name, args, ann, doc, body, _check) =>
      raise("NYI(to-type-member): s-method")
  end
end

fun to-type-variant(variant :: A.Variant, info :: TCInfo) -> FoldResult<Pair<A.Variant,TypeVariant>>:
  cases(A.Variant) variant:
    | s-variant(l, constr-loc, name, members, with-members) =>
      map-result(to-type-member(_, info), with-members).bind(
      lam(result):
        split-result      = split(result)
        new-with-members  = split-result.left
        with-type-members = split-result.right
        type-members = for map(member from members):
          t-member(member.bind.id.toname(), to-type-std(member.bind.ann, info))
        end
        new-variant = A.s-variant(l, constr-loc, name, members, new-with-members)
        type-variant = t-variant(constr-loc, name, type-members, with-type-members)
        fold-result(pair(new-variant, type-variant))
      end)
    | s-singleton-variant(l, name, with-members) =>
      map-result(to-type-member(_, info), with-members).bind(
      lam(result):
        split-result      = split(result)
        new-with-members  = split-result.left
        with-type-members = split-result.right
        new-variant       = A.s-singleton-variant(l, name, new-with-members)
        type-variant      = t-singleton-variant(l, name, with-type-members)
        fold-result(pair(new-variant, type-variant))
      end)
  end
end

fun record-view(access-loc :: Loc, obj :: A.Expr, obj-typ :: Type,
                handle :: (A.Expr, Loc, List<TypeMember> -> SynthesisResult),
                info :: TCInfo
) -> SynthesisResult:
  non-obj-err = synthesis-err([list: C.incorrect-type(obj-typ.tostring(), obj-typ.toloc(), "an object type", access-loc)])
  cases(Type) obj-typ:
    | t-record(l, members) =>
      handle(obj, l, members)
    | t-name(l, module-name, id) =>
      key = obj-typ.tostring()
      if info.data-exprs.has-key(key):
        handle(obj, l, info.data-exprs.get(key).fields)
      else:
        non-obj-err
      end
    | t-app(l, onto, args) =>
      raise("NYI(record-view): " + torepr(obj-typ))
    | else =>
      non-obj-err
  end
end

fun synthesis-field(access-loc :: Loc, obj :: A.Expr, obj-typ :: Type, field-name :: String, info :: TCInfo) -> SynthesisResult:
  record-view(access-loc, obj, obj-typ,
  lam(new-obj, l, obj-fields):
    cases(Option<TypeMember>) TS.type-members-lookup(obj-fields, field-name):
      | some(tm) =>
        synthesis-result(A.s-dot(l, new-obj, field-name), tm.typ)
      | none =>
        synthesis-err([list: C.object-missing-field(field-name, "{" + obj-fields.map(_.tostring()).join-str(", ") + "}", l, access-loc)])
    end
  end, info)
end

fun mk-variant-constructor(variant :: TypeVariant, creates :: Type, params :: List<TypeVariable>) -> Type:
  cases(TypeVariant) variant:
    | t-variant(l, _, fields, _) =>
      t-arrow(l, params, fields.map(_.typ), creates)
    | t-singleton-variant(l, _, _) =>
      creates
  end
end

fun synthesis-datatype(l :: Loc, name :: String, namet :: A.Name, params :: List<A.Name>, mixins, variants :: List<A.Variant>, fields :: List<A.Member>, _check :: Option<A.Expr>, info :: TCInfo) -> SynthesisResult:
  for synth-bind(variants-result from map-result(to-type-variant(_, info), variants)):
    for synth-bind(fields-result from map-result(to-type-member, fields)):
      if info.branders.has-key(namet.key()):
        t-vars = for map(param from params):
          t-variable(param.l, param.key(), t-top)
        end

        # TODO(cody): Handle mixins and _check
        # TODO(cody): Join common fields in variants for type-datatype
        split-variants = split(variants-result)
        split-fields   = split(fields-result)

        variant-typs   = split-variants.right

        variants-meet  = cases(List<TypeMember>) variant-typs:
          | empty => TS.empty-type-members
          | link(f, r) =>
            for fold(curr from TS.type-variant-fields(f), tv from r):
              TS.meet-fields(curr, TS.type-variant-fields(tv))
            end
        end

        # Save processed datatype
        brander-typ    = info.branders.get(namet.key())
        type-datatype  = t-datatype(t-vars, variant-typs, variants-meet + split-fields.right)
        info.data-exprs.set(brander-typ.tostring(), type-datatype)

        new-data-expr  = A.s-data-expr(l, name, namet, params, mixins, variants, split-fields.left, _check)
        brand-test-typ = t-arrow(_, empty, [list: t-top], t-boolean)
        data-fields    = link(t-member(name, brand-test-typ(l)),
        for map(variant from variant-typs):
          t-member(variant.name, mk-variant-constructor(variant, brander-typ, t-vars))
        end +
        for map(variant from variant-typs):
          t-member("is-" + variant.name, brand-test-typ(variant.l))
        end)
        data-expr-typ  = t-record(l, data-fields)

        # Return result of synthesis
        synthesis-result(new-data-expr, data-expr-typ)
      else:
        raise("Cannot find brander name in brander dictionary!")
      end
    end
  end
end

fun to-type(in-ann :: A.Ann, info :: TCInfo) -> Option<Type>:
  cases(A.Ann) in-ann:
    | a-blank =>
      none
    | a-any =>
      some(t-top)
    | a-name(l, id) =>
      some(t-name(l, none, id.key()))
    | a-type-var(l, id) =>
      some(t-var(id.key()))
    | a-arrow(l, args, ret, use-parens) =>
      arg-typs = args.map(lam(x): to-type(x, info).or-else(t-bot) end)
      ret-typ  = to-type(ret, info).or-else(t-top)
      forall   = empty
      some(t-arrow(l, forall, arg-typs, ret-typ))
    | a-method(l, args, ret) =>
      raise("a-method not yet handled:" + torepr(in-ann))
    | a-record(l, fields) =>
      new-fields = for map(field from fields):
        t-member(field.name, to-type-std(field.ann, info))
      end
      some(t-record(l, new-fields))
    | a-app(l, ann, args) =>
      raise("a-app not yet handled:" + torepr(in-ann))
    | a-pred(l, ann, exp) =>
      raise("a-pred not yet handled:" + torepr(in-ann))
    | a-dot(l, obj, field) =>
      some(t-name(l, some(obj.key()), field))
    | a-checked(checked, residual) =>
      raise("a-checked should not be appearing before type checking!")
  end
end

fun to-type-std(in-ann :: A.Ann, info :: TCInfo) -> Type:
  to-type(in-ann, info).or-else(t-top)
end

fun ensure-satisfies(typ :: Type, ann :: A.Ann, info :: TCInfo) -> Type:
  cases(Option<Type>) to-type(ann, info):
    | none =>
      typ
    | some(expect-typ) =>
      check-and-log(typ, expect-typ, expect-typ, info)
  end
end

fun handle-type-let-binds(bindings :: List<A.TypeLetBind>, info :: TCInfo):
  for map(binding from bindings):
    cases(A.TypeLetBind) binding:
      | s-type-bind(_, name, ann) =>
        info.aliases.set(name.key(), to-type-std(ann, info))
      | s-newtype-bind(l, name, namet) =>
        info.branders.set(namet.key(), t-name(l, none, name.key()))
    end
  end
end


fun synthesis-fun(
  l :: A.Loc, body :: A.Expr, params :: List<A.Name>, args :: List<A.Bind>, ret-ann :: A.Ann,
  recreate :: (List<A.Bind>, A.Ann, A.Expr -> A.Expr), info :: TCInfo
) -> SynthesisResult:
  forall   = for map(param from params):
               t-variable(A.dummy-loc, param.key(), t-top)
             end
  arg-typs = args.foldr(lam(arg :: A.Bind, base :: List<Type>):
                          arg-typ = to-type-std(arg.ann, info)
                          info.typs.set(arg.id.key(), arg-typ)
                          link(arg-typ, base)
                        end, empty)
  synthesis(body, info).bind(
  lam(new-body, new-body-typ):
    ret-typ  = ensure-satisfies(new-body-typ, ret-ann, info)
    new-fun  = recreate(args, ret-ann, new-body)
    fun-typ  = t-arrow(l, forall, arg-typs, ret-typ)
    synthesis-result(new-fun, fun-typ)
  end)
end

fun lookup-id(id, info :: TCInfo) -> Type:
  id-key = if is-string(id):
             id
           else if A.is-Name(id):
             id.key()
           else:
             raise("I don't know how to lookup your id! Received: " + torepr(id))
           end
  if info.typs.has-key(id-key):
    info.typs.get(id-key)
  else:
    raise("Identifier not found in environment! Tried to find: " + id-key)
  end
end

fun remove-foralls(l :: Loc, forall :: List<TypeVariable>, args :: List<Type>, ret :: Type, replacements :: List<Type>, info :: TCInfo) -> Type:
  n = for fold2(curr from pair(args, ret), variable from forall, typ from replacements):
        to-replace  = t-var(variable.id)
        replacement = typ
        upper       = variable.upper-bound
        new-args    = curr.left.map(_.substitute(to-replace, replacement))
        new-ret     = curr.right.substitute(to-replace, replacement)
        check-and-log(typ, upper, pair(new-args, new-ret), info)
      end
  t-arrow(l, empty, n.left, n.right)
end

fun synthesis-instantiation(l :: Loc, expr :: A.Expr, params :: List<A.Ann>, info :: TCInfo) -> SynthesisResult:
  synthesis(expr, info).bind(
  lam(new-expr, tmp-typ):
    cases(Type) tmp-typ:
      | t-arrow(l2, forall, args, ret) =>
        new-typs = params.map(to-type-std(_, info))
        new-typs-length = new-typs.length()
        forall-length   = forall.length()
        if new-typs-length == forall-length:
          new-inst = A.s-instantiate(l, new-expr, params)
          new-typ  = remove-foralls(l2, forall, args, ret, new-typs, info)
          synthesis-result(new-inst, new-typ)
        else:
          synthesis-err([list: C.bad-type-instantiation(forall-length, new-typs-length, l)])
        end
      | else =>
        synthesis-err([list: C.incorrect-type(tmp-typ.tostring(), tmp-typ.toloc(), "a function", l)])
    end
  end)
end

fun synthesis(e :: A.Expr, info :: TCInfo) -> SynthesisResult:
  cases(A.Expr) e:
    | s-module(l, answer, provides, types, checks) =>
      raise("s-module not yet handled")
    | s-type-let-expr(l, binds, body) =>
      handle-type-let-binds(binds, info)
      synthesis(body, info)
        .map-expr(A.s-type-let-expr(l, binds, _))
    | s-let-expr(l, bindings, body) =>
      action = synthesis-let-bind(_, info)
      for synth-bind(new-bindings from map-synthesis(action, bindings)):
        synthesis(body, info)
          .map-expr(A.s-let-expr(l, new-bindings, _))
      end
    | s-letrec(l, binds, body) =>
      raise("s-letrec not yet handled")
    | s-hint-exp(l, hints, exp) =>
      raise("s-hint-exp not yet handled")
    | s-instantiate(l, expr, params) =>
      synthesis-instantiation(l, expr, params, info)
    | s-block(l, stmts) =>
      var typ = t-top
      fun process-result(stmt-typ):
        typ := stmt-typ
      end
      fun build-ret-pair(new-stmts):
        synthesis-result(A.s-block(l, new-stmts), typ)
      end
      for map-synthesis(stmt from stmts):
        synthesis(stmt, info)
          .map-typ(process-result)
      end.synth-bind(build-ret-pair)
    | s-type(l, name, ann) =>
      raise("s-type not yet handled")
    | s-newtype(l, name, namet) =>
      raise("s-newtype not yet handled: " + torepr(e))
    | s-graph(l, bindings) =>
      raise("s-graph not yet handled")
    | s-contract(l, name, ann) =>
      raise("s-contract not yet handled")
    | s-assign(l, id, value) =>
      raise("s-assign not yet handled")
    | s-if-else(l, branches, _else) =>
      for fold(base from synthesis-if-result(empty, t-bot), branch from branches):
        base.bind(lam(new-branches, meet-typ):
          for synth-bind(new-test from checking(branch.test, t-boolean, info)):
            synthesis(branch.body, info).bind(
            lam(new-body, new-body-typ):
              new-branch = A.s-if-branch(branch.l, new-test, new-body)
              meet       = least-upper-bound(new-body-typ, meet-typ)
              synthesis-if-result(link(new-branch, new-branches), meet)
            end)
          end
        end)
      end.bind(lam(new-branches, meet-typ):
        synthesis(_else, info).bind(
        lam(new-else, new-else-typ):
          if-else-typ  = least-upper-bound(meet-typ, new-else-typ)
          new-if-else  = A.s-if-else(l, new-branches, new-else)
          synthesis-result(new-if-else, if-else-typ)
        end)
      end)
    | s-cases(l, typ, val, branches) =>
      raise("s-cases not yet handled")
    | s-cases-else(l, typ, val, branches, _else) =>
      raise("s-cases-else not yet handled")
    | s-try(l, body, id, _except) =>
      raise("s-try not yet handled")
    | s-op(l, op, left, right) =>
      raise("s-op not yet handled")
    | s-lam(l,
          params, # Type parameters
          args, # Value parameters
          ann, # return type
          doc, body, _check) =>
      synthesis-fun(l, body, params, args, ann, A.s-lam(l, params, _, _, doc, _, _check), info)
    | s-method(l,
        args, # Value parameters
        ann, # return type
        doc, body, _check) =>
      synthesis-fun(l, body, empty, args, ann, A.s-method(l, _, _, doc, _, _check), info)
    | s-extend(l, supe, fields) =>
      raise("s-extend not yet handled")
    | s-update(l, supe, fields) =>
      raise("s-update not yet handled")
    | s-obj(l, fields) =>
      for synth-bind(result from map-result(to-type-member(_, info), fields)):
        split-fields = split(result)
        new-fields   = split-fields.left
        field-typs   = split-fields.right
        new-obj      = A.s-obj(l, new-fields)
        obj-typ      = t-record(l, field-typs)
        synthesis-result(new-obj, obj-typ)
      end
    | s-array(l, values) =>
      raise("s-array not yet handled")
    | s-construct(l, modifier, constructor, values) =>
      raise("s-construct not yet handled")
    | s-confirm(l, expr, typ) =>
      raise("s-confirm not yet handled")
    | s-bless(l, expr, typ) =>
      raise("s-bless not yet handled")
    | s-app(l, _fun, args) =>
      synthesis(_fun, info).bind(
      lam(new-fun, arrow-typ):
        result = check-app(l, args, arrow-typ, t-top, info)
        for synth-bind(new-args from result.left):
          ast = A.s-app(l, new-fun, new-args)
          synthesis-result(ast, result.right)
        end
      end)
    | s-prim-app(l, _fun, args) =>
      arrow-typ = lookup-id(_fun, info)
      result = check-app(l, args, arrow-typ, t-top, info)
      for synth-bind(new-args from result.left):
        ast = A.s-prim-app(l, _fun, new-args)
        synthesis-result(ast, result.right)
      end
    | s-prim-val(l, name) =>
      raise("s-prim-val not yet handled")
    | s-id(l, id) =>
      synthesis-result(e, lookup-id(id, info))
    | s-id-var(l, id) =>
      synthesis-result(e, lookup-id(id, info))
    | s-id-letrec(l, id, safe) =>
      synthesis-result(e, lookup-id(id, info))
    | s-undefined(l) =>
      raise("s-undefined not yet handled")
    | s-srcloc(l, loc) =>
      raise("s-srcloc not yet handled")
    | s-num(l, n) =>
      synthesis-result(e, t-number)
    | s-frac(l, num, den) =>
      synthesis-result(e, t-number)
    | s-bool(l, b) =>
      synthesis-result(e, t-boolean)
    | s-str(l, s) =>
      synthesis-result(e, t-string)
    | s-dot(l, obj, field-name) =>
      synthesis(obj, info).bind(synthesis-field(l, _, _, field-name, info))
    | s-get-bang(l, obj, field) =>
      raise("s-get-bang not yet handled")
    | s-bracket(l, obj, field) =>
      raise("s-bracket not yet handled")
    | s-data-expr(l,
        name,
        namet,
        params, # type params
        mixins, variants, shared-members, _check) =>
      synthesis-datatype(l, name, namet, params, mixins, variants, shared-members, _check, info)
    | s-data(_, _, _, _, _, _, _)   => raise("s-data should have been desugared")
    | s-check(_, _, _, _)           => raise("s-check should have been desugared")
    | s-check-test(_, _, _, _)      => raise("s-check-test should have been desugared")
    | s-let(_, _, _, _)             => raise("s-let should have already been desugared")
    | s-var(_, _, _)                => raise("s-var should have already been desugared")
    | s-user-block(_, _)            => raise("s-user-block should have already been desugared")
    | s-if-pipe(_, _)               => raise("s-if-pipe should have already been desugared")
    | s-if-pipe-else(_, _, _)       => raise("s-if-pipe-else should have already been desugared")
    | s-if(_, _)                    => raise("s-if should have already been desugared")
    | s-fun(_, _, _, _, _, _, _, _) => raise("s-fun should have already been desugared")
    | s-when(_, _, _)               => raise("s-when should have already been desugared")
    | s-for(_, _, _, _, _)          => raise("s-for should have already been desugared")
    | s-paren(_, _)                 => raise("s-paren should have already been desugared")
  end

end

fun synthesis-binding(binding :: A.Bind, value :: A.Expr, recreate :: (A.Bind, A.Expr -> A.LetBind), info :: TCInfo) -> SynthesisResult:
  fun process-value(expr, typ):
    info.typs.set(binding.id.key(), typ)
    synthesis-binding-result(recreate(binding, expr), typ)
  end
  cases(Option<Type>) to-type(binding.ann, info):
    | none =>
      synthesis(value, info)
    | some(t) =>
      checking(value, t, info)
        .synth-bind(synthesis-result(_, t))
  end.bind(process-value)
end

fun synthesis-let-bind(binding :: A.LetBind, info :: TCInfo) -> SynthesisResult:
  cases(A.LetBind) binding:
    | s-let-bind(l, b, value) =>
      synthesis-binding(b, value, A.s-let-bind(l, _, _), info)
    | s-var-bind(l, b, value) =>
      raise("s-var-bind not yet handled")
  end
end

fun check-fun(body :: A.Expr, params :: List<A.Name>, args :: List<A.Bind>, ret-ann :: A.Ann, expect-typ :: Type, recreate :: (List<A.Bind>, A.Ann, A.Expr -> A.Expr), info :: TCInfo) -> CheckingResult:
  forall   = for map(param from params):
               t-variable(A.dummy-loc, param.key(), t-top)
             end
  arg-typs = cases(Type) expect-typ:
               | t-arrow(_, _, expect-args, _) =>
                 for map2(arg from args, expect-arg from expect-args):
                   arg-typ = to-type(arg.ann, info).or-else(expect-arg)
                   info.typs.set(arg.id.key(), arg-typ)
                   arg-typ
                 end
               | else =>
                 for map(arg from args):
                   arg-typ = to-type-std(arg.ann, info)
                   info.typs.set(arg.id.key(), arg-typ)
                   arg-typ
                 end
             end
  ret-typ  = cases(Type) expect-typ:
               | t-arrow(_, _, _, expect-ret) =>
                 to-type(ret-ann, info).or-else(expect-ret)
               | else =>
                 to-type-std(ret-ann, info)
             end
  arrow-typ = t-arrow(A.dummy-loc, forall, arg-typs, ret-typ)
  for bind(new-body from checking(body, ret-typ, info)):
    new-fun = recreate(args, ret-ann, new-body)
    check-and-return(arrow-typ, expect-typ, new-fun, info)
  end
end

fun check-app(app-loc :: Loc, args :: List<A.Expr>, arrow-typ :: Type, expect-typ :: Type, info :: TCInfo) -> Pair<CheckingMapResult,Type>:
  cases(Type) arrow-typ:
    | t-arrow(_, forall, arg-typs, ret-typ) =>
      bad-args    = C.incorrect-number-of-args(app-loc)
      args-map2   = map2-checking(bad-args)
      args-foldl2 = foldl2-result(bad-args)
      if is-empty(forall):
        new-args = for args-map2(arg from args, arg-typ from arg-typs):
                     checking(arg, arg-typ, info)
                   end
        pair(new-args, ret-typ)
      else:
        unknowns-list = for map(x from forall):
                          t-var(x.id)
                        end
        unknowns-set  = sets.list-to-tree-set(unknowns-list)
        binds = SD.immutable-string-dict()
        t-var-constraints = for fold2(current from empty-type-constraints,
                                     unknown from unknowns-list, x from forall):
                              generate-constraints(unknown, x.upper-bound, binds, [set: ], unknowns-set).meet(current)
                            end
        wrapped = for args-foldl2(curr from fold-result(empty-type-constraints),
                                  arg from args, arg-typ from arg-typs):
                    synthesis(arg, info).fold-bind(
                    lam(_, synthesis-typ):
                      result = generate-constraints(synthesis-typ, arg-typ, binds, [set: ], unknowns-set)
                      fold-result(curr.meet(result))
                    end)
                  end
        cases(FoldResult<TypeConstraints>) wrapped:
          | fold-result(args-constraints) =>
            ret-constraints  = generate-constraints(ret-typ, expect-typ, binds, [set: ], unknowns-set)
            constraints = t-var-constraints.meet(args-constraints.meet(ret-constraints))
            substitutions = for map(unknown from unknowns-list):
                              pair(unknown, constraints.substitute(unknown, ret-typ, binds))
                            end
            new-arg-typs  = for fold(curr from arg-typs, substitution from substitutions):
                              for map(arg-typ from curr):
                                arg-typ.substitute(substitution.left, substitution.right)
                              end
                            end
            new-ret-typ   = for fold(curr from ret-typ, substitution from substitutions):
                              curr.substitute(substitution.left, substitution.right)
                            end
            new-args      = for args-map2(arg from args, arg-typ from new-arg-typs):
                              checking(arg, arg-typ, info)
                            end
            pair(new-args, new-ret-typ)
          | fold-errors(errors) =>
            pair(checking-map-errors(errors), t-top)
        end
      end
    | t-bot =>
      new-args = for map-checking(arg from args):
        checking(arg, t-top, info)
      end
      pair(new-args, t-bot)
    | else =>
      pair(checking-map-errors([list: C.apply-non-function(app-loc)]), t-top)
  end
end

fun <V> check-and-log(typ :: Type, expect-typ :: Type, value :: V, info :: TCInfo) -> V:
  when not(typ.satisfies-type(expect-typ)):
    info.errors.insert(C.incorrect-type(typ.tostring(), typ.toloc(), expect-typ.tostring(), expect-typ.toloc()))
  end
  value
end

fun check-and-return(typ :: Type, expect-typ :: Type, value :: A.Expr, info :: TCInfo) -> CheckingResult:
  if typ.satisfies-type(expect-typ):
    checking-result(value)
  else:
    checking-err([list: C.incorrect-type(typ.tostring(), typ.toloc(), expect-typ.tostring(), expect-typ.toloc())])
  end
end

fun checking(e :: A.Expr, expect-typ :: Type, info :: TCInfo) -> CheckingResult:
  cases(A.Expr) e:
    | s-module(l, answer, provides, types, checks) =>
      checking(answer, expect-typ, info)
        .map(A.s-module(l, _, provides, types, checks))
    | s-type-let-expr(l, binds, body) =>
      handle-type-let-binds(binds, info)
      checking(body, expect-typ, info)
        .map(A.s-type-let-expr(l, binds, _))
    | s-let-expr(l, bindings, body) =>
      action = synthesis-let-bind(_, info)
      for check-bind(new-bindings from map-synthesis(action, bindings)):
        checking(body, expect-typ, info)
          .map(A.s-let-expr(l, new-bindings, _))
      end
    | s-letrec(l, bindings, body) =>
      # TODO(cody): This needs to be thought out more...
      for each(binding from bindings):
        # Collect initial annotations. If we don't have any, make them t-bot
        info.typs.set(binding.b.id.key(), to-type(binding.b.ann, info).or-else(t-bot))
      end
      fun traverse(curr-bindings :: List<A.LetrecBind>) -> FoldResult<List<A.Expr>>:
        for map-synthesis(binding from curr-bindings):
          cases(A.LetrecBind) binding:
            | s-letrec-bind(l2, b, value) =>
              recreate = A.s-letrec-bind(l2, _, _)
              synthesis-binding(b, value, recreate, info)
          end
        end
      end
      for check-bind(tmp-bindings from traverse(bindings)): # Traverse once to determine each one's correct type.
        for check-bind(new-bindings from traverse(tmp-bindings)): # Traverse again to check recursive references.
          checking(body, expect-typ, info)
            .map(A.s-letrec(l, new-bindings, _))
        end
      end
    | s-hint-exp(l, hints, exp) =>
      raise("s-hint-exp not yet handled")
    | s-instantiate(l, expr, params) =>
      synthesis-instantiation(l, expr, params, info).check-bind(
      lam(result, result-typ):
        check-and-return(result-typ, expect-typ, result, info)
      end)
    | s-block(l, stmts) =>
      fun mk-thunk(stmt, next, required):
        lam():
          for map-bind(ast from checking(stmt, required, info)):
            next().prepend(ast)
          end
        end
      end
      fun gen(curr, base):
        pair(mk-thunk(curr, base.left, base.right), t-top)
      end
      fun just-empty():
        checking-map(empty)
      end
      thunk = stmts.foldr(gen, pair(just-empty, expect-typ)).left
      for check-bind(new-stmts from thunk()):
        checking-result(A.s-block(l, new-stmts))
      end
    | s-type(l, name, ann) =>
      type-alias   = name.key()
      type-aliased = to-type-std(ann, info)
      info.aliases.set(type-alias, type-aliased)
      e
    | s-newtype(l, name, namet) =>
      raise("s-newtype not yet handled")
    | s-graph(l, bindings) =>
      raise("s-graph not yet handled")
    | s-contract(l, name, ann) =>
      raise("s-contract not yet handled")
    | s-assign(l, id, value) =>
      raise("s-assign not yet handled")
    | s-if-else(l, branches, _else) =>
      for map-result(branch from branches):
        for fold-bind(new-test from checking(branch.test, t-boolean, info)):
          for fold-bind(new-body from checking(branch.body, expect-typ, info)):
            fold-result(A.s-if-branch(branch.l, new-test, new-body))
          end
        end
      end.check-bind(
      lam(new-branches):
        checking(_else, expect-typ, info).map(A.s-if-else(l, new-branches, _))
      end)
    | s-cases(l, typ, val, branches) =>
      raise("s-cases not yet handled")
    | s-cases-else(l, typ, val, branches, _else) =>
      raise("s-cases-else not yet handled")
    | s-try(l, body, id, _except) =>
      raise("s-try not yet handled")
    | s-op(l, op, left, right) =>
      raise("s-op not yet handled")
    | s-lam(l,
          params, # Type parameters
          args, # Value parameters
          ann, # return type
          doc, body, _check) =>
      check-fun(body, params, args, ann, expect-typ, A.s-lam(l, params, _, _, doc, _, _check), info)
    | s-method(l,
        args, # Value parameters
        ann, # return type
        doc, body, _check) =>
      check-fun(body, empty, args, ann, expect-typ, A.s-method(l, _, _, doc, _, _check), info)
    | s-extend(l, supe, fields) =>
      raise("s-extend not yet handled")
    | s-update(l, supe, fields) =>
      raise("s-update not yet handled")
    | s-obj(l, fields) =>
      for check-bind(result from map-result(to-type-member(_, info), fields)):
        split-fields = split(result)
        new-fields   = split-fields.left
        field-typs   = split-fields.right
        new-obj      = A.s-obj(l, new-fields)
        obj-typ      = t-record(l, field-typs)
        check-and-return(obj-typ, expect-typ, new-obj, info)
      end
    | s-array(l, values) =>
      raise("s-array not yet handled")
    | s-construct(l, modifier, constructor, values) =>
      raise("s-construct not yet handled")
    | s-confirm(l, expr, typ) =>
      raise("s-confirm not yet handled")
    | s-bless(l, expr, typ) =>
      raise("s-bless not yet handled")
    | s-app(l, _fun, args) =>
      synthesis(_fun, info).check-bind(
      lam(new-fun, new-fun-typ):
        result = check-app(l, args, new-fun-typ, expect-typ, info)
        for check-bind(new-args from result.left):
          ast = A.s-app(l, new-fun, new-args)
          check-and-return(result.right, expect-typ, ast, info)
        end
      end)
    | s-prim-app(l, _fun, args) =>
      arrow-typ = lookup-id(_fun, info)
      result = check-app(l, args, arrow-typ, expect-typ, info)
      for check-bind(new-args from result.left):
        ast = A.s-prim-app(l, _fun, new-args)
        check-and-return(result.right, expect-typ, ast, info)
      end
    | s-prim-val(l, name) =>
      raise("s-prim-val not yet handled")
    | s-id(l, id) =>
      check-and-return(lookup-id(id, info), expect-typ, e, info)
    | s-id-var(l, id) =>
      check-and-return(lookup-id(id, info), expect-typ, e, info)
    | s-id-letrec(l, id, safe) =>
      check-and-return(lookup-id(id, info), expect-typ, e, info)
    | s-undefined(l) =>
      raise("s-undefined not yet handled")
    | s-srcloc(l, loc) =>
      check-and-return(t-srcloc, expect-typ, e, info)
    | s-num(l, n) =>
      check-and-return(t-number, expect-typ, e, info)
    | s-frac(l, num, den) =>
      check-and-return(t-number, expect-typ, e, info)
    | s-bool(l, b) =>
      check-and-return(t-boolean, expect-typ, e, info)
    | s-str(l, s) =>
      check-and-return(t-string, expect-typ, e, info)
    | s-dot(l, obj, field-name) =>
      synthesis(obj, info).bind(synthesis-field(l, _, _, field-name, info)).check-bind(
      lam(new-s-dot, s-dot-typ):
        check-and-return(s-dot-typ, expect-typ, new-s-dot, info)
      end)
    | s-get-bang(l, obj, field) =>
      raise("s-get-bang not yet handled")
    | s-bracket(l, obj, field) =>
      raise("s-bracket not yet handled")
    | s-data-expr(l,
        name,
        namet,
        params, # type params
        mixins, variants, shared-members, _check) =>
      synthesis-datatype(l, name, namet, params, mixins, variants, shared-members, _check, info).check-bind(
      lam(new-s-data-expr, s-data-expr-typ):
        check-and-return(s-data-expr-typ, expect-typ, new-s-data-expr, info)
      end)
    | s-data(_, _, _, _, _, _, _)   => raise("s-data should have been desugared")
    | s-check(_, _, _, _)           => raise("s-check should have been desugared")
    | s-check-test(l, _, _, _)      => raise("s-check-test should have been desugared")
    | s-let(_, _, _, _)             => raise("s-let should have already been desugared")
    | s-var(_, _, _)                => raise("s-var should have already been desugared")
    | s-user-block(_, _)            => raise("s-user-block should have already been desugared")
    | s-if-pipe(_, _)               => raise("s-if-pipe should have already been desugared")
    | s-if-pipe-else(_, _, _)       => raise("s-if-pipe-else should have already been desugared")
    | s-if(_, _)                    => raise("s-if should have already been desugared")
    | s-fun(_, _, _, _, _, _, _, _) => raise("s-fun should have already been desugared")
    | s-when(_, _, _)               => raise("s-when should have already been desugared")
    | s-for(_, _, _, _, _)          => raise("s-for should have already been desugared")
    | s-paren(_, _)                 => raise("s-paren should have already been desugared")
  end
end


default-typs = SD.string-dict()
default-typs.set(A.s-global("nothing").key(), t-name(A.dummy-loc, none, "tglobal#Nothing"))
default-typs.set("isBoolean", t-arrow(A.dummy-loc, empty, [list: t-top], t-boolean))
default-typs.set(A.s-global("torepr").key(), t-arrow(A.dummy-loc, empty, [list: t-top], t-string))
default-typs.set("throwNonBooleanCondition",
                 t-arrow(A.dummy-loc, empty, [list: t-srcloc,
                                                    t-string,
                                                    t-top], t-bot))
default-typs.set("throwNoBranchesMatched",
                 t-arrow(A.dummy-loc, empty, [list: t-srcloc,
                                                    t-string], t-bot))
default-typs.set("equiv", t-arrow(A.dummy-loc, empty, [list: t-top, t-top], t-boolean))
default-typs.set("hasField", t-arrow(A.dummy-loc, empty, [list: t-record(A.dummy-loc, empty), t-string], t-boolean))
default-typs.set(A.s-global("_times").key(), t-arrow(A.dummy-loc, empty, [list: t-number, t-number], t-number))
default-typs.set(A.s-global("_minus").key(), t-arrow(A.dummy-loc, empty, [list: t-number, t-number], t-number))
default-typs.set(A.s-global("_divide").key(), t-arrow(A.dummy-loc, empty, [list: t-number, t-number], t-number))
default-typs.set(A.s-global("_plus").key(), t-arrow(A.dummy-loc, empty, [list: t-number, t-number], t-number))


fun type-check(program :: A.Program, compile-env :: C.CompileEnvironment) -> C.CompileResult<A.Program>:
  errors = block:
    var err-list = empty
    {
      insert: lam(err :: C.CompileError):
        err-list := link(err, err-list)
      end,
      get: lam():
        err-list
      end
    }
  end

  cases(A.Program) program:
    | s-program(l, _provide, provided-types, imports, body) =>
      info = tc-info(default-typs, SD.string-dict(), SD.string-dict(), SD.string-dict(), errors)
      cases(CheckingResult) checking(body, t-top, info):
        | checking-result(new-body) =>
          C.ok(A.s-program(l, _provide, provided-types, imports, new-body))
        | checking-err(err-list) =>
          C.err(err-list + errors.get())
      end
    | else => raise("Attempt to type-check non-program: " + torepr(program))
  end
end
