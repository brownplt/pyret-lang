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

is-t-record               = TS.is-t-record

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

fun <B> identity(b :: B) -> B:
  b
end

fun <B,D> split(ps :: List<Pair<A,B>>) -> Pair<List<A>,List<B>>:
  fun step(p, curr):
    pair(link(p.left, curr.left), link(p.right, curr.right))
  end
  ps.foldr(step, pair(empty, empty))
end

fun rename-name(name :: Name) -> Name:
  cases(Name) name:
    | s-atom(base, serial) =>
      A.s-atom(gensym(base), serial)
    | else =>
      raise("NYI(rename-name): " + torepr(name))
  end
end

fun mk-arrow(l :: A.Loc, forall :: List<TypeVariable>, args :: List<Type>, ret :: Type) -> Type:
  if is-empty(forall):
    t-arrow(args, ret)
  else:
    f-pairs = for map(f from forall):
      new-id = rename-name(f.id)
      pair(f.id, t-variable(f.l, new-id, f.upper-bound, f.variance))
    end
    new-forall = f-pairs.map(_.right)
    new-args = for fold(curr from args, f-pair from f-pairs):
      curr.map(_.substitute(t-var(f-pair.left), t-var(f-pair.right.id)))
    end
    new-ret = for fold(curr from ret, f-pair from f-pairs):
      curr.substitute(t-var(f-pair.left), t-var(f-pair.right.id))
    end
    t-forall(new-forall, t-arrow(new-args, new-ret))
  end
end

fun to-type-member(field :: A.Member, info :: TCInfo) -> FoldResult<Pair<A.Member,TypeMember>>:
  cases(A.Member) field:
    | s-data-field(l, name, value) =>
      if A.is-s-method(value) or A.is-s-lam(value): # TODO(cody): Type-check methods and lambdas.
        fold-result(pair(A.s-data-field(l, name, value), t-member(name, t-top)))
      else:
        synthesis(value, info).fold-bind(
        lam(new-value, value-loc, value-typ):
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

fun record-view(access-loc :: Loc, obj :: A.Expr, obj-typ-loc :: A.Loc, obj-typ :: Type,
                handle :: (A.Expr, Loc, List<TypeMember> -> SynthesisResult),
                info :: TCInfo
) -> SynthesisResult:
  non-obj-err = synthesis-err([list: C.incorrect-type(tostring(obj-typ), obj-typ-loc, "an object type", access-loc)])
  cases(Type) obj-typ:
    | t-record(members) =>
      handle(obj, obj-typ-loc, members)
    | t-bot =>
      raise("NYI(record-view): Cannot currently handle record views of the bottom type")
    | else =>
      cases(Option<DataType>) TCS.get-data-type(obj-typ, info):
        | some(data-type) =>
          handle(obj, obj-typ-loc, data-type.fields)
        | none =>
          non-obj-err
      end
  end
end

fun synthesis-field(access-loc :: Loc, obj :: A.Expr, obj-typ-loc :: A.Loc, obj-typ :: Type, field-name :: String, info :: TCInfo) -> SynthesisResult:
  record-view(access-loc, obj, obj-typ-loc, obj-typ,
  lam(new-obj, l, obj-fields):
    cases(Option<TypeMember>) TS.type-members-lookup(obj-fields, field-name):
      | some(tm) =>
        synthesis-result(A.s-dot(l, new-obj, field-name), l, tm.typ)
      | none =>
        synthesis-err([list: C.object-missing-field(field-name, "{" + obj-fields.map(tostring).join-str(", ") + "}", l, access-loc)])
    end
  end, info)
end

fun mk-variant-constructor(variant :: TypeVariant, brander-typ :: Type, params :: List<TypeVariable>) -> Type:
  if is-empty(params):
    cases(TypeVariant) variant:
      | t-variant(l, _, fields, _) =>
        mk-arrow(l, params, fields.map(_.typ), brander-typ)
      | t-singleton-variant(l, _, _) =>
        brander-typ
    end
  else:
    creates = t-app(brander-typ, params.map(lam(x): t-var(x.id);))
    cases(TypeVariant) variant:
      | t-variant(l, _, fields, _) =>
        mk-arrow(l, params, fields.map(_.typ), creates)
      | t-singleton-variant(l, _, _) =>
        t-forall(params, creates)
    end
  end
end

fun synthesis-datatype(l :: Loc, name :: String, namet :: A.Name, params :: List<A.Name>, mixins, variants :: List<A.Variant>, fields :: List<A.Member>, _check :: Option<A.Expr>, info :: TCInfo) -> SynthesisResult:
  if info.branders.has-key(namet.key()):
    brander-typ    = info.branders.get(namet.key())
    tmp-t-vars = for map(param from params):
      t-variable(l, param, t-top, bivariant)
    end

    fun save-datatype(variant-typs :: List<TypeVariant>,
                      t-vars :: List<TypeVariable>,
                      datatype-fields :: TS.TypeMembers
    ) -> DataType:
      tmp-datatype = t-datatype(name, t-vars, variant-typs, datatype-fields)
      info.data-exprs.set(brander-typ.key(), tmp-datatype)
      tmp-datatype
    end

    # Save datatype before processing variants
    save-datatype(empty, tmp-t-vars, TS.empty-type-members)

    for synth-bind(variants-result from map-result(to-type-variant(_, info), variants)):
      split-variants = split(variants-result)
      variant-typs   = split-variants.right
      new-variants   = split-variants.left

      t-vars = for map(tmp-t-var from tmp-t-vars):
        variance = for fold(base from constant, variant-typ from variant-typs):
          for fold(curr from base, field from variant-typ.fields):
            TC.determine-variance(field.typ, tmp-t-var.id, info).join(curr)
          end
        end
        t-variable(tmp-t-var.l, tmp-t-var.id, tmp-t-var.upper-bound, variance)
      end

      variants-meet  = cases(List<TypeMember>) variant-typs.map(TS.type-variant-fields):
        | empty => TS.empty-type-members
        | link(f, r) => r.foldl(TC.meet-fields(_, _, info), f)
      end

      # Save datatype with new variant info before processing shared fields
      save-datatype(variant-typs, t-vars, variants-meet)

      for synth-bind(fields-result from map-result(to-type-member(_, info), fields)):
        # TODO(cody): Handle mixins and _check
        split-fields   = split(fields-result)

        # Save the final processed datatype
        # TODO(cody): If there are any common fields between the two
        # TypeMembers, then the lub should be calculated for that field
        save-datatype(variant-typs, t-vars, variants-meet + split-fields.right)

        new-data-expr  = A.s-data-expr(l, name, namet, params, mixins, new-variants, split-fields.left, _check)
        brand-test-typ = mk-arrow(_, empty, [list: t-top], t-boolean)
        data-fields    = link(t-member(name, brand-test-typ(l)),
        for map(variant from variant-typs):
          t-member(variant.name, mk-variant-constructor(variant, brander-typ, t-vars))
        end +
        for map(variant from variant-typs):
          t-member("is-" + variant.name, brand-test-typ(variant.l))
        end)
        data-expr-typ  = t-record(data-fields)

        # Return result of synthesis
        synthesis-result(new-data-expr, l, data-expr-typ)
      end
    end
  else:
    raise("Cannot find brander name in brander dictionary!")
  end
end

fun instantiate(l :: A.Loc, base-typ :: Type, data-type :: DataType, args :: List<Type>, info :: TCInfo) -> FoldResult<Option<Type>>:
  bound = for map2-strict(param from data-type.params, arg from args):
    check-and-log(l, arg, param.l, param.upper-bound, arg, info)
  end
  cases(Option<List<Type>>) bound:
    | some(lst) =>
      fold-result(some(t-app(base-typ, lst)))
    | none =>
      fold-errors([list: C.bad-type-instantiation(data-type.params.length(), args.length(), l)])
  end
end

fun ann-loc(in-ann :: A.Ann, default :: A.Loc) -> A.Loc:
  cases(A.Ann) in-ann:
    | a-blank                => default
    | a-any                  => default
    | a-name(l, _)           => l
    | a-type-var(l, _)       => l
    | a-arrow(l, _, _, _)    => l
    | a-method(l, _, _)      => l
    | a-record(l, _)         => l
    | a-app(l, _, _)         => l
    | a-pred(l, _, _)        => l
    | a-dot(l, _, _)         => l
    | a-checked(_, residual) => ann-loc(residual, default)
  end
end

fun to-type(in-ann :: A.Ann, info :: TCInfo) -> FoldResult<Option<Type>>:
  cases(A.Ann) in-ann:
    | a-blank =>
      fold-result(none)
    | a-any =>
      fold-result(some(t-top))
    | a-name(l, id) =>
      fold-result(some(t-name(none, id)))
    | a-type-var(l, id) =>
      fold-result(some(t-var(id)))
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
        fold-result(some(t-record(new-fields)))
      end
    | a-app(l, ann, args) =>
      for bind(base-typ from to-type-std(ann, info)):
        cases(Option<DataType>) TCS.get-data-type(base-typ, info):
          | some(data-type) =>
            map-result(to-type-std(_, info), args)
              .bind(instantiate(l, base-typ, data-type, _, info))
          | none =>
            fold-errors([list: C.given-parameters(tostring(base-typ), l)])
        end
      end
    | a-pred(l, ann, exp) =>
      for bind(typ from to-type-std(ann, info)):
        expect-typ = mk-arrow(l, empty, [list: typ], t-boolean)
        cases(CheckingResult) checking(exp, l, expect-typ, info):
          | checking-err(errs) => errs.map(info.errors.insert)
          | else => nothing
        end
        fold-result(some(typ))
      end
    | a-dot(l, obj, field) =>
      fold-result(some(t-name(some(obj), A.s-global(field))))
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
        typ = t-name(none, name)
        namet-key = namet.key()
        info.branders.set(namet-key, typ)
        info.typs.set(namet-key, t-record([list:
          t-member("test", t-arrow([list: t-top], t-boolean)),
          t-member("brand", t-arrow([list: t-top], typ))
        ]))
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
  new-info = params.foldl(TCS.add-binding(_, t-top, _), info)
  for synth-bind(arg-typs from map-result(process-binding(_, t-top, new-info), args)):
    fun process(new-body :: A.Expr, _ :: A.Loc, ret-typ :: Type) -> SynthesisResult:
      tmp-arrow = t-arrow(arg-typs, ret-typ)
      forall = for map(param from params):
        t-variable(A.dummy-loc, param, t-top, TC.determine-variance(tmp-arrow, param, new-info))
      end
      arrow-typ = mk-arrow(l, forall, arg-typs, ret-typ)
      new-fun = recreate(args, ret-ann, new-body)
      synthesis-result(new-fun, l, arrow-typ)
    end
  
    for synth-bind(maybe-ret from to-type(ret-ann, new-info)):
      cases(Option<Type>) maybe-ret:
        | some(ret-typ) =>
          ret-loc = ann-loc(ret-ann, l)
          checking(body, ret-loc, ret-typ, new-info).synth-bind(process(_, ret-loc, ret-typ))
        | none =>
          synthesis(body, new-info).bind(process)
      end
    end
  end
end

fun bind-arg(info :: TCInfo, arg :: A.Bind, tm-loc :: A.Loc, tm :: TypeMember) -> FoldResult<TCInfo>:
  for bind(maybe-declared from to-type(arg.ann, info)):
    typ = tm.typ
    cases(Option<Type>) maybe-declared:
      | some(declared-typ) =>
        declared-loc = ann-loc(arg.ann, A.dummy-loc)
        if satisfies-type(typ, declared-typ, info):
          info.typs.set(arg.id.key(), declared-typ)
          fold-result(info)
        else:
          fold-errors([list: C.incorrect-type(tostring(declared-typ), declared-loc, tostring(typ), tm-loc)])
        end
      | none =>
        info.typs.set(arg.id.key(), typ)
        fold-result(info)
    end
  end
end


fun handle-if-branch(branch :: A.IfBranch, info :: TCInfo) -> FoldResult<Pair<A.IfBranch,Type>>:
  for fold-bind(new-test from checking(branch.test, branch.l, t-boolean, info)):
    synthesis(branch.body, info).fold-bind(
      lam(new-body, _, body-typ):
        new-branch = A.s-if-branch(branch.l, new-test, new-body)
        fold-result(pair(new-branch, body-typ))
      end)
  end
end

fun handle-branch(data-type :: DataType, cases-loc :: A.Loc, branch :: A.CasesBranch,
                  expect-loc :: A.Loc, maybe-check :: Option<Type>,
                  remove :: (String -> Any), info :: TCInfo
) -> FoldResult<Pair<A.CasesBranch, Type>>:
  fun handle-body(name :: String, body :: A.Expr, process, new-info :: TCInfo):
    remove(name)
    cases(Option<Type>) maybe-check:
      | some(expect-typ) =>
        checking(body, expect-loc, expect-typ, new-info).fold-bind(process(_, expect-loc, expect-typ))
      | none =>
        synthesis(body, new-info).fold-bind(process)
    end
  end
  cases(Option<TypeVariant>) data-type.lookup-variant(branch.name):
    | some(tv) =>
      cases(TypeVariant) tv:
        | t-variant(variant-loc, _, fields, _) =>
          cases(A.CasesBranch) branch:
            | s-cases-branch(l, pat-loc, name, args, body) =>
              fun process(new-body, _, typ):
                new-branch = A.s-cases-branch(l, pat-loc, name, args, new-body)
                fold-result(pair(new-branch, typ))
              end
              bind-args = foldl2-result(C.incorrect-number-of-bindings(name, l, args.length(), fields.length()))
              bind-args(bind-arg(_, _, variant-loc, _), fold-result(info), args.map(_.bind), fields)
                .bind(handle-body(name, body, process, _))
            | s-singleton-cases-branch(l, _, name, _) =>
              fold-errors([list: C.cases-singleton-mismatch(name, l, false)])
          end
        | t-singleton-variant(_, _, _) =>
          cases(A.CasesBranch) branch:
            | s-cases-branch(l, _, name, _, _) =>
              fold-errors([list: C.cases-singleton-mismatch(name, l, true)])
            | s-singleton-cases-branch(l, pat-loc, name, body) =>
              fun process(new-body, _, typ):
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
                     maybe-else :: Option<A.Expr>, expect-loc :: A.Loc, maybe-expect :: Option<Type>,
                     info :: TCInfo, bind-direction, create-err :: (List<C.CompileError> -> B),
                     has-else, no-else) -> B:
  for bind-direction(typ from to-type-std(ann, info)):
    cases(Option<DataType>) TCS.get-data-type(typ, info):
      | some(data-type) =>
        for bind-direction(new-val from checking(val, l, typ, info)):
          branch-tracker = track-branches(data-type)
          for bind-direction(result from map-result(handle-branch(data-type, l, _, expect-loc, maybe-expect, branch-tracker.remove, info), branches)):
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
        create-err([list: C.cant-match-on(tostring(typ), l)])
    end
  end
end

fun synthesis-cases-has-else(l :: A.Loc, ann :: A.Ann, new-val :: A.Expr, split-result :: Pair<List<A.CasesBranch>,List<Type>>, _else :: A.Expr, info :: TCInfo) -> SynthesisResult:
  synthesis(_else, info).bind(
    lam(new-else, _, else-typ):
      branches-typ = meet-branch-typs(link(else-typ, split-result.right), info)
      new-cases = A.s-cases-else(l, ann, new-val, split-result.left, new-else)
      synthesis-result(new-cases, l, branches-typ)
    end)
end

fun synthesis-cases-no-else(l :: A.Loc, ann :: A.Ann, new-val :: A.Expr, split-result :: Pair<List<A.CasesBranch>,List<Type>>, info :: TCInfo) -> SynthesisResult:
  branches-typ = meet-branch-typs(split-result.right, info)
  new-cases = A.s-cases(l, ann, new-val, split-result.left)
  synthesis-result(new-cases, l, branches-typ)
end

fun checking-cases-has-else(expect-loc :: A.Loc, expect-typ :: Type):
  lam(l :: A.Loc, ann :: A.Ann, new-val :: A.Expr, split-result :: Pair<List<A.CasesBranch>,List<Type>>, _else :: A.Expr, info :: TCInfo) -> CheckingResult:
    for bind(new-else from checking(_else, expect-loc, expect-typ, info)):
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
  handle-cases(l, ann, val, branches, maybe-else, A.dummy-loc, none, info, synth-bind, synthesis-err, synthesis-cases-has-else, synthesis-cases-no-else)
end

fun checking-cases(l :: A.Loc, ann :: A.Ann, val :: A.Expr, branches :: List<A.CasesBranch>, maybe-else :: Option<A.Expr>, expect-loc :: A.Loc, expect-typ :: Type, info :: TCInfo) -> CheckingResult:
  handle-cases(l, ann, val, branches, maybe-else, expect-loc, some(expect-typ), info, check-bind, checking-err, checking-cases-has-else(expect-loc, expect-typ), checking-cases-no-else)
end



fun lookup-id(blame-loc :: A.Loc, id, info :: TCInfo) -> FoldResult<Type>:
  id-key = if is-string(id):
             id
           else if A.is-Name(id):
             id.key()
           else:
             raise("I don't know how to lookup your id! Received: " + torepr(id))
           end
  if info.typs.has-key(id-key):
    fold-result(info.typs.get(id-key))
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

fun remove-foralls(l :: A.Loc, forall :: List<TypeVariable>, onto :: Type, replacements :: List<Type>, info :: TCInfo) -> Option<Type>:
  for fold2-strict(curr from onto, variable from forall, replacement from replacements):
    to-replace  = t-var(variable.id)
    upper       = variable.upper-bound
    new-curr    = curr.substitute(to-replace, replacement)
    check-and-log(l, replacement, variable.l, upper, new-curr, info)
  end
end

fun synthesis-instantiation(l :: Loc, expr :: A.Expr, params :: List<A.Ann>, info :: TCInfo) -> SynthesisResult:
  synthesis(expr, info).bind(
  lam(new-expr, tmp-typ-loc, tmp-typ):
    cases(Type) tmp-typ:
      | t-forall(introduces, onto) =>
        for synth-bind(new-typs from map-result(to-type-std(_, info), params)):
          cases(Option<Type>) remove-foralls(l, introduces, onto, new-typs, info):
            | some(new-typ) =>
              new-inst = A.s-instantiate(l, new-expr, params)
              synthesis-result(new-inst, l, new-typ)
            | none =>
              nt-l = new-typs.length()
              i-l   = introduces.length()
              synthesis-err([list: C.bad-type-instantiation(i-l, nt-l, l)])
          end
        end
      | t-bot =>
        for synth-bind(new-typs from map-result(to-type-std(_, info), params)):
          new-inst = A.s-instantiate(l, new-expr, params)
          synthesis-result(new-inst, l, t-bot)
        end
      | else =>
        synthesis-err([list: C.incorrect-type(tostring(tmp-typ), tmp-typ-loc, "a function", l)])
    end
  end)
end

fun synthesis(e :: A.Expr, info :: TCInfo) -> SynthesisResult:
  cases(A.Expr) e:
    | s-module(l, answer, provides, types, checks) =>
      synthesis(answer, info)
        .map-expr(A.s-module(l, _, provides, types, checks))
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
    | s-letrec(l, bindings, body) =>
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
      for synth-bind(_ from map-result(process-letrec-binding(_, t-bot, info), bindings)):
        for synth-bind(tmp-bindings from traverse(bindings)): # Traverse once to determine each one's correct type.
          for synth-bind(new-bindings from traverse(tmp-bindings)): # Traverse again to check recursive references.
            synthesis(body, info)
              .map-expr(A.s-letrec(l, new-bindings, _))
          end
        end
      end
    | s-hint-exp(l, hints, exp) =>
      raise("s-hint-exp not yet handled")
    | s-instantiate(l, expr, params) =>
      synthesis-instantiation(l, expr, params, info)
    | s-block(l, stmts) =>
      var typ = t-top
      var loc = A.dummy-loc
      fun build-ret-pair(new-stmts):
        synthesis-result(A.s-block(l, new-stmts), loc, typ)
      end
      for map-synthesis(stmt from stmts):
        synthesis(stmt, info).bind(
          lam(stmt-expr, stmt-loc, stmt-typ):
            typ := stmt-typ
            loc := stmt-loc
            synthesis-result(stmt-expr, stmt-loc, stmt-typ)
          end)
      end.synth-bind(build-ret-pair)
    | s-type(l, name, ann) =>
      synthesis-err([list: C.unsupported("Synthesizing type aliases is currently unsupported by the type checker", l)])
    | s-newtype(l, name, namet) =>
      synthesis-err([list: C.unsupported("newtype is currently unsupported by the type checker", l)])
    | s-graph(l, bindings) =>
      synthesis-err([list: C.unsupported("Graph is currently unsupported by the type checker", l)])
    | s-contract(l, name, ann) =>
      synthesis-err([list: C.unsupported("s-contract is currently unsupported by the type checker", l)])
    | s-assign(l, id, value) =>
      synthesis-err([list: C.unsupported("Variables and assignment are currently unsupported by the type checker", l)])
    | s-if-else(l, branches, _else) =>
      for synth-bind(result from map-result(handle-if-branch(_, info), branches)):
        synthesis(_else, info).bind(
          lam(new-else, _, else-typ):
            split-result = split(result)
            new-branches = split-result.left
            if-else-typ  = meet-branch-typs(link(else-typ, split-result.right), info)
            new-if-else  = A.s-if-else(l, new-branches, new-else)
            synthesis-result(new-if-else, l, if-else-typ)
          end)
      end
    | s-cases(l, typ, val, branches) =>
      synthesis-cases(l, typ, val, branches, none, info)
    | s-cases-else(l, typ, val, branches, _else) =>
      synthesis-cases(l, typ, val, branches, some(_else), info)
    | s-try(l, body, id, _except) =>
      synthesis-err([list: C.unsupported("try is currently unsupported by the type checker", l)])
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
      synthesis-err([list: C.unsupported("Object extension is currently unsupported by the type checker", l)])
    | s-update(l, supe, fields) =>
      synthesis-err([list: C.unsupported("Updating object fields is currently unsupported by the type checker", l)])
    | s-obj(l, fields) =>
      for synth-bind(result from map-result(to-type-member(_, info), fields)):
        split-fields = split(result)
        new-fields   = split-fields.left
        field-typs   = split-fields.right
        new-obj      = A.s-obj(l, new-fields)
        obj-typ      = t-record(field-typs)
        synthesis-result(new-obj, l, obj-typ)
      end
    | s-array(l, values) =>
      synthesis-err([list: C.unsupported("Arrays are currently unsupported by the type checker", l)])
    | s-construct(l, modifier, constructor, values) =>
      synthesis-err([list: C.unsupported("s-construct is currently unsupported by the type checker", l)])
    | s-app(l, _fun, args) =>
      synthesis-app-fun(l, _fun, args, info).bind(
      lam(new-fun, arrow-typ-loc, arrow-typ):
        result = check-app(l, args, arrow-typ, t-top, info)
        for synth-bind(new-args from result.left):
          ast = A.s-app(l, new-fun, new-args)
          synthesis-result(ast, l, result.right)
        end
      end)
    | s-prim-app(l, _fun, args) =>
      for synth-bind(arrow-typ from lookup-id(l, _fun, info)):
        result = check-app(l, args, arrow-typ, t-top, info)
        for synth-bind(new-args from result.left):
          ast = A.s-prim-app(l, _fun, new-args)
          synthesis-result(ast, l, result.right)
        end
      end
    | s-prim-val(l, name) =>
      raise("s-prim-val not yet handled")
    | s-id(l, id) =>
      for synth-bind(id-typ from lookup-id(l, id, info)):
        synthesis-result(e, l, id-typ)
      end
    | s-id-var(l, id) =>
      for synth-bind(id-typ from lookup-id(l, id, info)):
        synthesis-result(e, l, id-typ)
      end
    | s-id-letrec(l, id, safe) =>
      for synth-bind(id-typ from lookup-id(l, id, info)):
        synthesis-result(e, l, id-typ)
      end
    | s-undefined(l) =>
      raise("s-undefined not yet handled")
    | s-srcloc(l, loc) =>
      synthesis-result(e, l, t-srcloc)
    | s-num(l, n) =>
      synthesis-result(e, l, t-number)
    | s-frac(l, num, den) =>
      synthesis-result(e, l, t-number)
    | s-bool(l, b) =>
      synthesis-result(e, l, t-boolean)
    | s-str(l, s) =>
      synthesis-result(e, l, t-string)
    | s-dot(l, obj, field-name) =>
      synthesis(obj, info).bind(synthesis-field(l, _, _, _, field-name, info))
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
  fun process-value(expr, typ-loc, typ):
    info.typs.set(binding.id.key(), typ)
    synthesis-binding-result(recreate(binding, expr), typ)
  end
  for synth-bind(maybe-typ from to-type(binding.ann, info)):
    cases(Option<Type>) maybe-typ:
      | none =>
        synthesis(value, info)
      | some(t) =>
        checking(value, binding.l, t, info)
          .synth-bind(synthesis-result(_, binding.l, t))
    end.bind(process-value)
  end
end

fun synthesis-let-bind(binding :: A.LetBind, info :: TCInfo) -> SynthesisResult:
  cases(A.LetBind) binding:
    | s-let-bind(l, b, value) =>
      synthesis-binding(b, value, A.s-let-bind(l, _, _), info)
    | s-var-bind(l, b, value) =>
      synthesis-err([list: C.unsupported("Variables and assignment are currently unsupported by the type-checker.", l)])
  end
end

fun check-fun(fun-loc :: A.Loc, body :: A.Expr, params :: List<A.Name>, args :: List<A.Bind>, ret-ann :: A.Ann, expect-loc :: A.Loc, expect-typ :: Type, recreate :: (List<A.Bind>, A.Ann, A.Expr -> A.Expr), info :: TCInfo) -> CheckingResult:
  new-info = params.foldl(TCS.add-binding(_, t-top, _), info)
  maybe-arg-typs =
  cases(Type) expect-typ:
    | t-arrow(expect-args, _) =>
      expected = "a function with " + tostring(expect-args.length()) + " arguments"
      found    = "a function with " + tostring(args.length()) + " arguments"
      set-args = map2-result(C.incorrect-type(expected, fun-loc, found, expect-loc))
      set-args(process-binding(_, _, new-info), args, expect-args)
    | else =>
      map-result(process-binding(_, t-top, new-info), args)
  end
  for check-bind(arg-typs from maybe-arg-typs):
    for check-bind(maybe-ret from to-type(ret-ann, new-info)):
      fun process(new-body :: A.Expr, ret-loc :: A.Loc, ret-typ :: Type) -> CheckingResult:
        tmp-arrow = t-arrow(arg-typs, ret-typ)
        forall = for map(param from params):
          t-variable(fun-loc, param, t-top, TC.determine-variance(tmp-arrow, param, new-info))
        end
        arrow-typ = mk-arrow(fun-loc, forall, arg-typs, ret-typ)
        new-fun = recreate(args, ret-ann, new-body)
        check-and-return(fun-loc, arrow-typ, expect-loc, expect-typ, new-fun, new-info)
      end
      cases(Option<Type>) maybe-ret:
        | some(ret-typ) =>
          ret-loc = ann-loc(ret-ann, fun-loc)
          checking(body, ret-loc, ret-typ, new-info).bind(process(_, ret-loc, ret-typ))
        | none =>
          cases(Type) expect-typ:
            | t-arrow(_, ret-typ) =>
              cases(List<A.Name>) params:
                | empty =>
                  checking(body, expect-loc, ret-typ, new-info).bind(process(_, expect-loc, ret-typ))
                | link(_, _) =>
                  # If the programmer has not written a return type and this is
                  # a polymorphic function, then the return type may have the
                  # type of one of the type variables. In this case, we should
                  # just synthesize the return type. For an example, see:
                  #   tests/type-check/good/lam-forall-check.arr
                  synthesis(body, new-info).check-bind(process)
              end
            | else =>
              synthesis(body, new-info).check-bind(process)
          end
      end
    end
  end
end

fun synthesis-app-fun(app-loc :: Loc, _fun :: A.Expr, args :: List<A.Expr>, info :: TCInfo) -> SynthesisResult:
  cases(A.Expr) _fun:
    | s-id(fun-loc, id) =>
      result = synthesis-result(_fun, _, _)
      fun pick2(num-typ :: Type, rec-typ :: Type):
        cases(List<A.Expr>) args:
          | empty      =>
            synthesis-err([list: raise("fixme")])
          | link(f, r) =>
            synthesis(f, info).bind(
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
            synthesis-err([list: raise("fixme")])
          | link(f, r) =>
            synthesis(f, info).bind(
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
        | otherwise: synthesis(_fun, info)
      end
    | else =>
      synthesis(_fun, info)
  end
end

fun check-app(app-loc :: Loc, args :: List<A.Expr>, arrow-typ :: Type, expect-typ :: Type, info :: TCInfo) -> Pair<CheckingMapResult,Type>:
  bad-args    = C.incorrect-number-of-args(app-loc)
  args-map2   = map2-checking(bad-args)
  args-foldl2 = foldl2-result(bad-args)
  cases(Type) arrow-typ:
    | t-arrow(arg-typs, ret-typ) =>
      new-args = for args-map2(arg from args, arg-typ from arg-typs):
        checking(arg, app-loc, arg-typ, info)
      end
      pair(new-args, ret-typ)
    | t-forall(introduces, onto) =>
      cases(Type) onto:
        | t-arrow(arg-typs, ret-typ) =>
          fun process(arg :: A.Expr) -> FoldResult<Type>:
            synthesis(arg, info).fold-bind(lam(_, _, typ): fold-result(typ);)
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
                checking(arg, app-loc, arg-typ, info)
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
        checking(arg, app-loc, t-top, info)
      end
      pair(new-args, t-bot)
    | else =>
      pair(checking-map-errors([list: C.apply-non-function(app-loc)]), t-top)
  end
end

fun <V> check-and-log(typ-loc :: A.Loc, typ :: Type, expect-loc :: A.Loc, expect-typ :: Type, value :: V, info :: TCInfo) -> V:
  when not(satisfies-type(typ, expect-typ, info)):
    info.errors.insert(C.incorrect-type(tostring(typ), typ-loc, tostring(expect-typ), expect-loc))
  end
  value
end

fun check-and-return(typ-loc :: A.Loc, typ :: Type, expect-loc :: A.Loc, expect-typ :: Type, value :: A.Expr, info :: TCInfo) -> CheckingResult:
  if satisfies-type(typ, expect-typ, info):
    checking-result(value)
  else:
    checking-err([list: C.incorrect-type(tostring(typ), typ-loc, tostring(expect-typ), expect-loc)])
  end
end

fun checking(e :: A.Expr, expect-loc :: A.Loc, expect-typ :: Type, info :: TCInfo) -> CheckingResult:
  cases(A.Expr) e:
    | s-module(l, answer, provides, types, checks) =>
      checking(answer, expect-loc, expect-typ, info)
        .map(A.s-module(l, _, provides, types, checks))
    | s-type-let-expr(l, binds, body) =>
      for check-bind(_ from handle-type-let-binds(binds, info)):
        checking(body, expect-loc, expect-typ, info)
          .map(A.s-type-let-expr(l, binds, _))
      end
    | s-let-expr(l, bindings, body) =>
      action = synthesis-let-bind(_, info)
      for check-bind(new-bindings from map-synthesis(action, bindings)):
        checking(body, expect-loc, expect-typ, info)
          .map(A.s-let-expr(l, new-bindings, _))
      end
    | s-letrec(l, bindings, body) =>
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
            checking(body, expect-loc, expect-typ, info)
              .map(A.s-letrec(l, new-bindings, _))
          end
        end
      end
    | s-hint-exp(l, hints, exp) =>
      raise("s-hint-exp not yet handled")
    | s-instantiate(l, expr, params) =>
      synthesis-instantiation(l, expr, params, info).check-bind(
      lam(result, result-loc, result-typ):
        check-and-return(l, result-typ, expect-loc, expect-typ, result, info)
      end)
    | s-block(l, stmts) =>
      fun mk-thunk(stmt, next, required):
        lam():
          # We can use expect-loc every time, since it will always be either
          # Any or expect-typ
          for map-bind(ast from checking(stmt, expect-loc, required, info)):
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
      checking-err([list: C.unsupported("newtype is currently unsupported by the type checker", l)])
    | s-graph(l, bindings) =>
      checking-err([list: C.unsupported("Graph is currently unsupported by the type checker", l)])
    | s-contract(l, name, ann) =>
      checking-err([list: C.unsupported("Contract is currently unsupported by the type checker", l)])
    | s-assign(l, id, value) =>
      checking-err([list: C.unsupported("Assignment is currently unsupported by the type checker", l)])
    | s-if-else(l, branches, _else) =>
      for map-result(branch from branches):
        for fold-bind(new-test from checking(branch.test, branch.l, t-boolean, info)):
          for fold-bind(new-body from checking(branch.body, expect-loc, expect-typ, info)):
            fold-result(A.s-if-branch(branch.l, new-test, new-body))
          end
        end
      end.check-bind(
      lam(new-branches):
        checking(_else, expect-loc, expect-typ, info).map(A.s-if-else(l, new-branches, _))
      end)
    | s-cases(l, typ, val, branches) =>
      checking-cases(l, typ, val, branches, none, expect-loc, expect-typ, info)
    | s-cases-else(l, typ, val, branches, _else) =>
      checking-cases(l, typ, val, branches, some(_else), expect-loc, expect-typ, info)
    | s-try(l, body, id, _except) =>
      checking-err([list: C.unsupported("try is currently unsupported by the type checker", l)])
    | s-op(l, op, left, right) =>
      raise("s-op not yet handled")
    | s-lam(l,
          params, # Type parameters
          args, # Value parameters
          ann, # return type
          doc, body, _check) =>
      check-fun(l, body, params, args, ann, expect-loc, expect-typ, A.s-lam(l, params, _, _, doc, _, _check), info)
    | s-method(l,
        args, # Value parameters
        ann, # return type
        doc, body, _check) =>
      check-fun(l, body, empty, args, ann, expect-loc, expect-typ, A.s-method(l, _, _, doc, _, _check), info)
    | s-extend(l, supe, fields) =>
      checking-err([list: C.unsupported("Object extension is currently unsupported by the type checker", l)])
    | s-update(l, supe, fields) =>
      checking-err([list: C.unsupported("Object updates are currently unsupported by the type checker", l)])
    | s-obj(l, fields) =>
      for check-bind(result from map-result(to-type-member(_, info), fields)):
        split-fields = split(result)
        new-fields   = split-fields.left
        field-typs   = split-fields.right
        new-obj      = A.s-obj(l, new-fields)
        obj-typ      = t-record(field-typs)
        check-and-return(l, obj-typ, expect-loc, expect-typ, new-obj, info)
      end
    | s-array(l, values) =>
      checking-err([list: C.unsupported("s-array is currently unsupported by the type checker", l)])
    | s-construct(l, modifier, constructor, values) =>
      checking-err([list: C.unsupported("s-construct is unsupported by the type checker", l)])
    | s-app(l, _fun, args) =>
      synthesis-app-fun(l, _fun, args, info).check-bind(
      lam(new-fun, new-fun-loc, new-fun-typ):
        result = check-app(l, args, new-fun-typ, expect-typ, info)
        for check-bind(new-args from result.left):
          ast = A.s-app(l, new-fun, new-args)
          check-and-return(l, result.right, expect-loc, expect-typ, ast, info)
        end
      end)
    | s-prim-app(l, _fun, args) =>
      for check-bind(arrow-typ from lookup-id(l, _fun, info)):
        result = check-app(l, args, arrow-typ, expect-typ, info)
        for check-bind(new-args from result.left):
          ast = A.s-prim-app(l, _fun, new-args)
          check-and-return(l, result.right, expect-loc, expect-typ, ast, info)
        end
      end
    | s-prim-val(l, name) =>
      raise("s-prim-val not yet handled")
    | s-id(l, id) =>
      for check-bind(id-typ from lookup-id(l, id, info)):
        check-and-return(l, id-typ, expect-loc, expect-typ, e, info)
      end
    | s-id-var(l, id) =>
      for check-bind(id-typ from lookup-id(l, id, info)):
        check-and-return(l, id-typ, expect-loc, expect-typ, e, info)
      end
    | s-id-letrec(l, id, safe) =>
      for check-bind(id-typ from lookup-id(l, id, info)):
        check-and-return(l, id-typ, expect-loc, expect-typ, e, info)
      end
    | s-undefined(l) =>
      raise("s-undefined not yet handled")
    | s-srcloc(l, loc) =>
      check-and-return(l, t-srcloc, expect-loc, expect-typ, e, info)
    | s-num(l, n) =>
      check-and-return(l, t-number, expect-loc, expect-typ, e, info)
    | s-frac(l, num, den) =>
      check-and-return(l, t-number, expect-loc, expect-typ, e, info)
    | s-bool(l, b) =>
      check-and-return(l, t-boolean, expect-loc, expect-typ, e, info)
    | s-str(l, s) =>
      check-and-return(l, t-string, expect-loc, expect-typ, e, info)
    | s-dot(l, obj, field-name) =>
      synthesis(obj, info).bind(synthesis-field(l, _, _, _, field-name, info)).check-bind(
      lam(new-s-dot, s-dot-loc, s-dot-typ):
        check-and-return(s-dot-loc, s-dot-typ, expect-loc, expect-typ, new-s-dot, info)
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
      lam(new-s-data-expr, s-data-expr-loc, s-data-expr-typ):
        check-and-return(s-data-expr-loc, s-data-expr-typ, expect-loc, expect-typ, new-s-data-expr, info)
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
      if is-empty(imports):
        info = TCS.empty-tc-info()
        tc-result = checking(body, A.dummy-loc, t-top, info)
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
      else:
        C.err([list: C.unsupported("The type-checker does not currently support imports.", l)])
      end
    | else => raise("Attempt to type-check non-program: " + torepr(program))
  end
end
