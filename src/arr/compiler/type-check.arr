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
import "compiler/list-aux.arr" as LA

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

type Variance             = TS.Variance
constant                  = TS.constant
bivariant                 = TS.bivariant
invariant                 = TS.invariant
covariant                 = TS.covariant
contravariant             = TS.contravariant

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

type TypeConstraint       = TC.TypeConstraint
type TypeConstraints      = TC.TypeConstraints
generate-constraints      = TC.generate-constraints
empty-type-constraints    = TC.empty-type-constraints
satisfies-type            = TC.satisfies-type
least-upper-bound         = TC.least-upper-bound
greatest-lower-bound      = TC.greatest-lower-bound

type TCInfo               = TCS.TCInfo
tc-info                   = TCS.tc-info

type Bindings             = TCS.Bindings
empty-bindings            = TCS.empty-bindings

type SynthesisResult      = TCS.SynthesisResult
type CheckingResult       = TCS.CheckingResult
type FoldResult           = TCS.FoldResult
type CheckingMapResult    = TCS.CheckingMapResult

synthesis-result          = TCS.synthesis-result
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
foldr2-result             = TCS.foldr2-result
map2-result               = TCS.map2-result
foldr-result              = TCS.foldr-result
map2-checking             = TCS.map2-checking
map-checking              = TCS.map-checking
map-result                = TCS.map-result

all2-strict               = LA.all2-strict
map2-strict               = LA.map2-strict
fold2-strict              = LA.fold2-strict

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

fun mk-arrow(l :: A.Loc, forall :: List<TypeVariable>, args :: List<Type>, ret :: Type) -> Type:
  if is-empty(forall):
    t-arrow(l, args, ret)
  else:
    f-pairs = for map(f from forall):
      new-id = gensym(f.id)
      pair(f.id, t-variable(f.l, new-id, f.upper-bound, f.variance))
    end
    new-forall = f-pairs.map(_.right)
    new-args = for fold(curr from args, f-pair from f-pairs):
      curr.map(_.substitute(t-var(f-pair.left), t-var(f-pair.right.id)))
    end
    new-ret = for fold(curr from ret, f-pair from f-pairs):
      curr.substitute(t-var(f-pair.left), t-var(f-pair.right.id))
    end
    t-forall(new-forall, t-arrow(l, new-args, new-ret))
  end
end

fun to-type-member(field :: A.Member, info :: TCInfo) -> FoldResult<Pair<A.Member,TypeMember>>:
  cases(A.Member) field:
    | s-data-field(l, name, value) =>
      if A.is-s-method(value) or A.is-s-lam(value): # TODO(cody): Type-check methods and lambdas.
        fold-result(pair(A.s-data-field(l, name, value), t-member(name, t-top)))
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
      for bind(result from map-result(to-type-member(_, info), with-members)):
        split-result      = split(result)
        new-with-members  = split-result.left
        with-type-members = split-result.right
        fun process-member(member):
          to-type-std(member.bind.ann, info)
            .map(t-member(member.bind.id.toname(), _))
        end
        for bind(type-members from map-result(process-member, members)):
          new-variant = A.s-variant(l, constr-loc, name, members, new-with-members)
          type-variant = t-variant(constr-loc, name, type-members, with-type-members)
          fold-result(pair(new-variant, type-variant))
        end
      end
    | s-singleton-variant(l, name, with-members) =>
      for bind(result from map-result(to-type-member(_, info), with-members)):
        split-result      = split(result)
        new-with-members  = split-result.left
        with-type-members = split-result.right
        new-variant       = A.s-singleton-variant(l, name, new-with-members)
        type-variant      = t-singleton-variant(l, name, with-type-members)
        fold-result(pair(new-variant, type-variant))
      end
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
      cases(Option<DataType>) TCS.get-data-type(obj-typ, info):
        | some(data-type) =>
          handle(obj, l, data-type.fields)
        | none =>
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
      mk-arrow(l, params, fields.map(_.typ), creates)
    | t-singleton-variant(l, _, _) =>
      creates
  end
end

fun synthesis-datatype(l :: Loc, name :: String, namet :: A.Name, params :: List<A.Name>, mixins, variants :: List<A.Variant>, fields :: List<A.Member>, _check :: Option<A.Expr>, info :: TCInfo) -> SynthesisResult:
  for synth-bind(variants-result from map-result(to-type-variant(_, info), variants)):
    for synth-bind(fields-result from map-result(to-type-member(_, info), fields)):
      if info.branders.has-key(namet.key()):
        t-vars = for map(param from params):
          t-variable(param.l, param.key(), t-top, invariant)
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
              TC.meet-fields(curr, TS.type-variant-fields(tv), info)
            end
        end

        # Save processed datatype
        brander-typ    = info.branders.get(namet.key())
        type-datatype  = t-datatype(name, t-vars, variant-typs, variants-meet + split-fields.right)
        info.data-exprs.set(brander-typ.tostring(), type-datatype)

        new-data-expr  = A.s-data-expr(l, name, namet, params, mixins, variants, split-fields.left, _check)
        brand-test-typ = mk-arrow(_, empty, [list: t-top], t-boolean)
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

fun to-type(in-ann :: A.Ann, info :: TCInfo) -> FoldResult<Option<Type>>:
  cases(A.Ann) in-ann:
    | a-blank =>
      fold-result(none)
    | a-any =>
      fold-result(some(t-top))
    | a-name(l, id) =>
      fold-result(some(t-name(l, none, id.key())))
    | a-type-var(l, id) =>
      fold-result(some(t-var(id.key())))
    | a-arrow(l, args, ret, use-parens) =>
      fun arg-to-type(x):
        to-type(x, info).map(_.or-else(t-bot))
      end
      for bind(arg-typs from map-result(arg-to-type, args)):
        for bind(ret-typ from to-type(ret, info).map(_.or-else(t-top))):
          forall = empty
          fold-result(some(mk-arrow(l, forall, arg-typs, ret-typ)))
        end
      end
    | a-method(l, args, ret) =>
      raise("a-method not yet handled:" + torepr(in-ann))
    | a-record(l, fields) =>
      fun field-to-type(field):
        to-type-std(field.ann, info).map(t-member(field.name, _))
      end
      for bind(new-fields from map-result(field-to-type, fields)):
        fold-result(some(t-record(l, new-fields)))
      end
    | a-app(l, ann, args) =>
      raise("a-app not yet handled:" + torepr(in-ann))
    | a-pred(l, ann, exp) =>
      for bind(typ from to-type-std(ann, info)):
        expect-typ = mk-arrow(l, empty, [list: typ], t-boolean)
        cases(CheckingResult) checking(exp, expect-typ, info):
          | checking-err(errs) => errs.map(info.errors.insert)
          | else => nothing
        end
        fold-result(some(typ))
      end
    | a-dot(l, obj, field) =>
      fold-result(some(t-name(l, some(obj.key()), field)))
    | a-checked(checked, residual) =>
      raise("a-checked should not be appearing before type checking!")
  end
end

fun to-type-std(in-ann :: A.Ann, info :: TCInfo) -> FoldResult<Type>:
  to-type(in-ann, info).map(_.or-else(t-top))
end

fun handle-type-let-binds(bindings :: List<A.TypeLetBind>, info :: TCInfo):
  for map-result(binding from bindings):
    cases(A.TypeLetBind) binding:
      | s-type-bind(_, name, ann) =>
        for bind(typ from to-type-std(ann, info)):
          info.aliases.set(name.key(), typ)
          fold-result(typ)
        end
      | s-newtype-bind(l, name, namet) =>
        typ = t-name(l, none, name.key())
        info.branders.set(namet.key(), typ)
        fold-result(typ)
    end
  end
end

fun process-binding(arg :: A.Bind, default-typ :: Type, info :: TCInfo):
  for bind(arg-typ from to-type(arg.ann, info).map(_.or-else(default-typ))):
    info.typs.set(arg.id.key(), arg-typ)
    fold-result(arg-typ)
  end
end

fun process-letrec-binding(lrb :: A.LetrecBind, default-typ :: Type, info :: TCInfo):
  process-binding(lrb.b, default-typ, info)
end


fun synthesis-fun(
  l :: A.Loc, body :: A.Expr, params :: List<A.Name>, args :: List<A.Bind>, ret-ann :: A.Ann,
  recreate :: (List<A.Bind>, A.Ann, A.Expr -> A.Expr), info :: TCInfo
) -> SynthesisResult:
  new-info = params.foldl(lam(param, tmp-info): TCS.add-binding(param.key(), t-top, tmp-info);, info)
  for synth-bind(arg-typs from map-result(process-binding(_, t-top, new-info), args)):
    fun process(new-body :: A.Expr, ret-typ :: Type) -> SynthesisResult:
      tmp-arrow = t-arrow(l, arg-typs, ret-typ)
      forall = for map(param from params):
        id = param.key()
        t-variable(A.dummy-loc, id, t-top, TC.determine-variance(tmp-arrow, id, new-info))
      end
      arrow-typ = mk-arrow(A.dummy-loc, forall, arg-typs, ret-typ)
      new-fun = recreate(args, ret-ann, new-body)
      synthesis-result(new-fun, arrow-typ)
    end
  
    for synth-bind(maybe-ret from to-type(ret-ann, new-info)):
      cases(Option<Type>) maybe-ret:
        | some(ret-typ) =>
          checking(body, ret-typ, new-info).synth-bind(process(_, ret-typ))
        | none =>
          synthesis(body, new-info).bind(process)
      end
    end
  end
end

fun bind-arg(info :: TCInfo, arg :: A.Bind, tm :: TypeMember) -> FoldResult<TCInfo>:
  for bind(maybe-declared from to-type(arg.ann, info)):
    typ = tm.typ
    cases(Option<Type>) maybe-declared:
      | some(declared-typ) =>
        if satisfies-type(typ, declared-typ, info):
          info.typs.set(arg.id.key(), declared-typ)
          fold-result(info)
        else:
          fold-errors([list: C.incorrect-type(declared-typ.tostring(), declared-typ.toloc(), typ.tostring(), typ.toloc())])
        end
      | none =>
        info.typs.set(arg.id.key(), typ)
        fold-result(info)
    end
  end
end


fun handle-if-branch(branch :: A.IfBranch, info :: TCInfo) -> FoldResult<Pair<A.IfBranch,Type>>:
  for fold-bind(new-test from checking(branch.test, t-boolean, info)):
    synthesis(branch.body, info).fold-bind(
      lam(new-body, body-typ):
        new-branch = A.s-if-branch(branch.l, new-test, new-body)
        fold-result(pair(new-branch, body-typ))
      end)
  end
end

fun handle-branch(data-type :: DataType, cases-loc :: A.Loc, branch :: A.CasesBranch,
                  maybe-check :: Option<Type>, remove :: (String -> Any),
                  info :: TCInfo
) -> FoldResult<Pair<A.CasesBranch, Type>>:
  fun handle-body(name :: String, body :: A.Expr, process, new-info :: TCInfo):
    remove(name)
    cases(Option<Type>) maybe-check:
      | some(expect-typ) =>
        checking(body, expect-typ, new-info).fold-bind(process(_, expect-typ))
      | none =>
        synthesis(body, new-info).fold-bind(process)
    end
  end
  cases(Option<TypeVariant>) data-type.lookup-variant(branch.name):
    | some(tv) =>
      cases(TypeVariant) tv:
        | t-variant(_, _, fields, _) =>
          cases(A.CasesBranch) branch:
            | s-cases-branch(l, pat-loc, name, args, body) =>
              fun process(new-body, typ):
                new-branch = A.s-cases-branch(l, pat-loc, name, args, new-body)
                fold-result(pair(new-branch, typ))
              end
              bind-args = foldl2-result(C.incorrect-number-of-bindings(name, l, args.length(), fields.length()))
              bind-args(bind-arg, fold-result(info), args, fields)
                .bind(handle-body(name, body, process, _))
            | s-singleton-cases-branch(l, _, name, _) =>
              fold-errors([list: C.cases-singleton-mismatch(name, l, false)])
          end
        | t-singleton-variant(_, _, _) =>
          cases(A.CasesBranch) branch:
            | s-cases-branch(l, _, name, _, _) =>
              fold-errors([list: C.cases-singleton-mismatch(name, l, true)])
            | s-singleton-cases-branch(l, pat-loc, name, body) =>
              fun process(new-body, typ):
                new-branch = A.s-singleton-cases-branch(l, pat-loc, name, new-body)
                fold-result(pair(new-branch, typ))
              end
              handle-body(name, body, process, info)
          end
      end
    | none =>
      fold-errors([list: C.unneccesary-branch(branch.name, branch.l, data-type.name, cases-loc)])
  end
end

fun meet-branch-typs(branch-typs :: List<Type>, info :: TCInfo) -> Type:
  branch-typs.foldl(least-upper-bound(_, _, info), t-bot)
end

fun track-branches(data-type :: DataType) ->
  { remove :: (String -> Set<String>), get :: (-> Set<String>) }:
  var unhandled-branches = data-type.variants.foldl(lam(b, s): s.add(b.name);, [set:])
  {
    remove: lam(b-name :: String):
      unhandled-branches := unhandled-branches.remove(b-name)
    end,
    get: lam() -> Set<String>:
      unhandled-branches
    end
  }
end

fun <B> handle-cases(l :: A.Loc, ann :: A.Ann, val :: A.Expr, branches :: List<A.CasesBranch>,
                     maybe-else :: Option<A.Expr>, maybe-expect :: Option<Type>,
                     info :: TCInfo, bind-direction, create-err :: (List<C.CompileError> -> B),
                     has-else, no-else) -> B:
  for bind-direction(typ from to-type-std(ann, info)):
    cases(Option<DataType>) TCS.get-data-type(typ, info):
      | some(data-type) =>
        for bind-direction(new-val from checking(val, typ, info)):
          branch-tracker = track-branches(data-type)
          for bind-direction(result from map-result(handle-branch(data-type, l, _, maybe-expect, branch-tracker.remove, info), branches)):
            split-result = split(result)
            remaining-branches = branch-tracker.get().to-list()
            cases(Option<A.Expr>) maybe-else:
              | some(_else) =>
                if is-empty(remaining-branches):
                  create-err([list: C.unneccesary-else-branch(data-type.name, l)])
                else:
                  has-else(l, ann, new-val, split-result, _else, info)
                end
              | none =>
                if is-empty(remaining-branches):
                  no-else(l, ann, new-val, split-result, info)
                else:
                  create-err([list: C.non-exhaustive-pattern(remaining-branches, data-type.name, l)])
                end
            end
          end
        end
      | none =>
        create-err([list: C.cant-match-on(typ.tostring(), l)])
    end
  end
end

fun synthesis-cases-has-else(l :: A.Loc, ann :: A.Ann, new-val :: A.Expr, split-result :: Pair<List<A.CasesBranch>,List<Type>>, _else :: A.Expr, info :: TCInfo) -> SynthesisResult:
  synthesis(_else, info).bind(
    lam(new-else, else-typ):
      branches-typ = meet-branch-typs(link(else-typ, split-result.right), info)
      new-cases = A.s-cases-else(l, ann, new-val, split-result.left, new-else)
      synthesis-result(new-cases, branches-typ)
    end)
end

fun synthesis-cases-no-else(l :: A.Loc, ann :: A.Ann, new-val :: A.Expr, split-result :: Pair<List<A.CasesBranch>,List<Type>>, info :: TCInfo) -> SynthesisResult:
  branches-typ = meet-branch-typs(split-result.right, info)
  new-cases = A.s-cases(l, ann, new-val, split-result.left)
  synthesis-result(new-cases, branches-typ)
end

fun checking-cases-has-else(expect-typ :: Type):
  lam(l :: A.Loc, ann :: A.Ann, new-val :: A.Expr, split-result :: Pair<List<A.CasesBranch>,List<Type>>, _else :: A.Expr, info :: TCInfo) -> CheckingResult:
    for bind(new-else from checking(_else, expect-typ, info)):
      new-cases = A.s-cases-else(l, ann, new-val, split-result.left, new-else)
      checking-result(new-cases)
    end
  end
end

fun checking-cases-no-else(l :: A.Loc, ann :: A.Ann, new-val :: A.Expr, split-result :: Pair<List<A.CasesBranch>,List<Type>>, info :: TCInfo) -> CheckingResult:
  new-cases = A.s-cases(l, ann, new-val, split-result.left)
  checking-result(new-cases)
end

fun synthesis-cases(l :: A.Loc, ann :: A.Ann, val :: A.Expr, branches :: List<A.CasesBranch>, maybe-else :: Option<A.Expr>, info :: TCInfo) -> SynthesisResult:
  handle-cases(l, ann, val, branches, maybe-else, none, info, synth-bind, synthesis-err, synthesis-cases-has-else, synthesis-cases-no-else)
end

fun checking-cases(l :: A.Loc, ann :: A.Ann, val :: A.Expr, branches :: List<A.CasesBranch>, maybe-else :: Option<A.Expr>, expect-typ :: Type, info :: TCInfo) -> CheckingResult:
  handle-cases(l, ann, val, branches, maybe-else, some(expect-typ), info, check-bind, checking-err, checking-cases-has-else(expect-typ), checking-cases-no-else)
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

fun remove-foralls(forall :: List<TypeVariable>, onto :: Type, replacements :: List<Type>, info :: TCInfo) -> Option<Type>:
  for fold2-strict(curr from onto, variable from forall, replacement from replacements):
    to-replace  = t-var(variable.id)
    upper       = variable.upper-bound
    new-curr    = curr.substitute(to-replace, replacement)
    check-and-log(replacement, upper, new-curr, info)
  end
end

fun synthesis-instantiation(l :: Loc, expr :: A.Expr, params :: List<A.Ann>, info :: TCInfo) -> SynthesisResult:
  synthesis(expr, info).bind(
  lam(new-expr, tmp-typ):
    cases(Type) tmp-typ:
      | t-forall(introduces, onto) =>
        for synth-bind(new-typs from map-result(to-type-std(_, info), params)):
          cases(Option<Type>) remove-foralls(introduces, onto, new-typs, info):
            | some(new-typ) =>
              new-inst = A.s-instantiate(l, new-expr, params)
              synthesis-result(new-inst, new-typ)
            | none =>
              nt-l = new-typs.length()
              i-l   = introduces.length()
              synthesis-err([list: C.bad-type-instantiation(i-l, nt-l, l)])
          end
        end
      | t-bot =>
        for synth-bind(new-typs from map-result(to-type-std(_, info), params)):
          new-inst = A.s-instantiate(l, new-expr, params)
          synthesis-result(new-inst, t-bot)
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
      for synth-bind(_ from handle-type-let-binds(binds, info)):
        synthesis(body, info)
          .map-expr(A.s-type-let-expr(l, binds, _))
      end
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
      for synth-bind(result from map-result(handle-if-branch(_, info), branches)):
        synthesis(_else, info).bind(
          lam(new-else, else-typ):
            split-result = split(result)
            new-branches = split-result.left
            if-else-typ  = meet-branch-typs(link(else-typ, split-result.right), info)
            new-if-else  = A.s-if-else(l, new-branches, new-else)
            synthesis-result(new-if-else, if-else-typ)
          end)
      end
    | s-cases(l, typ, val, branches) =>
      synthesis-cases(l, typ, val, branches, none, info)
    | s-cases-else(l, typ, val, branches, _else) =>
      synthesis-cases(l, typ, val, branches, some(_else), info)
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
  for synth-bind(maybe-typ from to-type(binding.ann, info)):
    cases(Option<Type>) maybe-typ:
      | none =>
        synthesis(value, info)
      | some(t) =>
        checking(value, t, info)
          .synth-bind(synthesis-result(_, t))
    end.bind(process-value)
  end
end

fun synthesis-let-bind(binding :: A.LetBind, info :: TCInfo) -> SynthesisResult:
  cases(A.LetBind) binding:
    | s-let-bind(l, b, value) =>
      synthesis-binding(b, value, A.s-let-bind(l, _, _), info)
    | s-var-bind(l, b, value) =>
      raise("s-var-bind not yet handled")
  end
end

fun check-fun(fun-loc :: A.Loc, body :: A.Expr, params :: List<A.Name>, args :: List<A.Bind>, ret-ann :: A.Ann, expect-typ :: Type, recreate :: (List<A.Bind>, A.Ann, A.Expr -> A.Expr), info :: TCInfo) -> CheckingResult:
  new-info = params.foldl(TCS.add-binding(_, t-top, _), info)
  maybe-arg-typs =
  cases(Type) expect-typ:
    | t-arrow(l, expect-args, _) =>
      expected = "a function with " + tostring(expect-args.length())
      found    = "a function with " + tostring(args.length())
      set-args = map2-result(C.incorrect-type(expected, fun-loc, found, l))
      set-args(process-binding(_, _, new-info), args, expect-args)
    | else =>
      map-result(process-binding(_, t-top, new-info), args)
  end
  for check-bind(arg-typs from maybe-arg-typs):
    for check-bind(maybe-ret from to-type(ret-ann, new-info)):
      fun process(new-body :: A.Expr, ret-typ :: Type) -> CheckingResult:
        tmp-arrow = t-arrow(fun-loc, arg-typs, ret-typ)
        forall = for map(param from params):
          id = param.key()
          t-variable(A.dummy-loc, id, t-top, TC.determine-variance(tmp-arrow, id, new-info))
        end
        arrow-typ = mk-arrow(A.dummy-loc, forall, arg-typs, ret-typ)
        new-fun = recreate(args, ret-ann, new-body)
        check-and-return(arrow-typ, expect-typ, new-fun, new-info)
      end
      cases(Option<Type>) maybe-ret:
        | some(ret-typ) =>
          checking(body, ret-typ, new-info).bind(process(_, ret-typ))
        | none =>
          cases(Type) expect-typ:
            | t-arrow(_, _, ret-typ) =>
              checking(body, ret-typ, new-info).bind(process(_, ret-typ))
            | else =>
              synthesis(body, new-info).check-bind(process)
          end
      end
    end
  end
end

fun check-app(app-loc :: Loc, args :: List<A.Expr>, arrow-typ :: Type, expect-typ :: Type, info :: TCInfo) -> Pair<CheckingMapResult,Type>:
  bad-args    = C.incorrect-number-of-args(app-loc)
  args-map2   = map2-checking(bad-args)
  args-foldl2 = foldl2-result(bad-args)
  cases(Type) arrow-typ:
    | t-arrow(_, arg-typs, ret-typ) =>
      new-args = for args-map2(arg from args, arg-typ from arg-typs):
        checking(arg, arg-typ, info)
      end
      pair(new-args, ret-typ)
    | t-forall(introduces, onto) =>
      cases(Type) onto:
        | t-arrow(_, arg-typs, ret-typ) =>
          fun process(arg :: A.Expr) -> FoldResult<Type>:
            synthesis(arg, info).fold-bind(lam(_, typ): fold-result(typ);)
          end
          wrapped = map-result(process, args)
                      .bind(TC.arrow-constraints(app-loc, introduces, arg-typs, ret-typ, _, expect-typ, info))
          cases(FoldResult<Option<TC.Substitutions>>) wrapped:
            | fold-result(substitutions) =>
              new-arg-typs = for fold(curr from arg-typs, substitution from substitutions):
                for map(arg-typ from curr):
                  arg-typ.substitute(substitution.left, substitution.right)
                end
              end
              new-ret-typ = for fold(curr from ret-typ, substitution from substitutions):
                curr.substitute(substitution.left, substitution.right)
              end
              new-args = for args-map2(arg from args, arg-typ from new-arg-typs):
                checking(arg, arg-typ, info)
              end
              pair(new-args, new-ret-typ)
            | fold-errors(errors) =>
              pair(checking-map-errors(errors), t-top)
          end
        | else =>
          pair(checking-map-errors([list: C.apply-non-function(app-loc)]), t-top)
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
  when not(satisfies-type(typ, expect-typ, info)):
    info.errors.insert(C.incorrect-type(typ.tostring(), typ.toloc(), expect-typ.tostring(), expect-typ.toloc()))
  end
  value
end

fun check-and-return(typ :: Type, expect-typ :: Type, value :: A.Expr, info :: TCInfo) -> CheckingResult:
  if satisfies-type(typ, expect-typ, info):
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
      for check-bind(_ from handle-type-let-binds(binds, info)):
        checking(body, expect-typ, info)
          .map(A.s-type-let-expr(l, binds, _))
      end
    | s-let-expr(l, bindings, body) =>
      action = synthesis-let-bind(_, info)
      for check-bind(new-bindings from map-synthesis(action, bindings)):
        checking(body, expect-typ, info)
          .map(A.s-let-expr(l, new-bindings, _))
      end
    | s-letrec(l, bindings, body) =>
      # TODO(cody): This needs to be thought out more...
      # Collect initial annotations. If we don't have any, make them t-bot
      fun traverse(curr-bindings :: List<A.LetrecBind>) -> FoldResult<List<A.Expr>>:
        for map-synthesis(binding from curr-bindings):
          cases(A.LetrecBind) binding:
            | s-letrec-bind(l2, b, value) =>
              recreate = A.s-letrec-bind(l2, _, _)
              synthesis-binding(b, value, recreate, info)
          end
        end
      end
      for check-bind(_ from map-result(process-letrec-binding(_, t-bot, info), bindings)):
        for check-bind(tmp-bindings from traverse(bindings)): # Traverse once to determine each one's correct type.
          for check-bind(new-bindings from traverse(tmp-bindings)): # Traverse again to check recursive references.
            checking(body, expect-typ, info)
              .map(A.s-letrec(l, new-bindings, _))
          end
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
      for check-bind(type-aliased from to-type-std(ann, info)):
        type-alias = name.key()
        info.aliases.set(type-alias, type-aliased)
        checking-result(e)
      end
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
      checking-cases(l, typ, val, branches, none, expect-typ, info)
    | s-cases-else(l, typ, val, branches, _else) =>
      checking-cases(l, typ, val, branches, some(_else), expect-typ, info)
    | s-try(l, body, id, _except) =>
      raise("s-try not yet handled")
    | s-op(l, op, left, right) =>
      raise("s-op not yet handled")
    | s-lam(l,
          params, # Type parameters
          args, # Value parameters
          ann, # return type
          doc, body, _check) =>
      check-fun(l, body, params, args, ann, expect-typ, A.s-lam(l, params, _, _, doc, _, _check), info)
    | s-method(l,
        args, # Value parameters
        ann, # return type
        doc, body, _check) =>
      check-fun(l, body, empty, args, ann, expect-typ, A.s-method(l, _, _, doc, _, _check), info)
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



fun type-check(program :: A.Program, compile-env :: C.CompileEnvironment) -> C.CompileResult<A.Program>:
  cases(A.Program) program:
    | s-program(l, _provide, provided-types, imports, body) =>
      info = TCS.empty-tc-info()
      tc-result = checking(body, t-top, info)
      side-errs = info.errors.get()
      cases(CheckingResult) tc-result:
        | checking-result(new-body) =>
          if is-empty(side-errs):
            C.ok(A.s-program(l, _provide, provided-types, imports, new-body))
          else:
            C.err(side-errs)
          end
        | checking-err(err-list) =>
          C.err(err-list + side-errs)
      end
    | else => raise("Attempt to type-check non-program: " + torepr(program))
  end
end
