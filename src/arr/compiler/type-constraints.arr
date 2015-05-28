provide { generate-constraints   : generate-constraints,
          arrow-constraints      : arrow-constraints,
          empty-type-constraints : empty-type-constraints,
          satisfies-type         : satisfies-type,
          determine-variance     : determine-variance,
          least-upper-bound      : least-upper-bound,
          greatest-lower-bound   : greatest-lower-bound,
          meet-fields            : meet-fields } end

provide-types { TypeConstraint  : TypeConstraint,
                TypeConstraints : TypeConstraints,
                Substitutions   : Substitutions }

import ast as A
import string-dict as SD
import srcloc as SL
import valueskeleton as VS
import "compiler/type-structs.arr" as TS
import "compiler/type-check-structs.arr" as TCS
import "compiler/list-aux.arr" as LA
import "compiler/compile-structs.arr" as C

all2-strict  = LA.all2-strict
map2-strict  = LA.map2-strict
fold2-strict = LA.fold2-strict

type Name            = A.Name

dict-to-string       = TS.dict-to-string
mut-dict-to-string   = TS.mut-dict-to-string

type Pair            = TS.Pair
pair                 = TS.pair

type Type            = TS.Type
t-name               = TS.t-name
t-var                = TS.t-var
t-arrow              = TS.t-arrow
t-top                = TS.t-top
t-bot                = TS.t-bot
t-app                = TS.t-app
t-record             = TS.t-record
t-forall             = TS.t-forall
t-ref                = TS.t-ref

t-number             = TS.t-number
t-string             = TS.t-string
t-boolean            = TS.t-boolean
t-srcloc             = TS.t-srcloc

type TypeMember      = TS.TypeMember
type TypeMembers     = TS.TypeMembers
empty-type-members   = TS.empty-type-members
t-member             = TS.t-member
type-members-lookup  = TS.type-members-lookup

type Variance        = TS.Variance
constant             = TS.constant
bivariant            = TS.bivariant
invariant            = TS.invariant
covariant            = TS.covariant
contravariant        = TS.contravariant

type TypeVariable    = TS.TypeVariable
t-variable           = TS.t-variable
is-t-top             = TS.is-t-top
is-t-bot             = TS.is-t-bot
is-t-var             = TS.is-t-var

type DataType        = TS.DataType

type TCInfo          = TCS.TCInfo
tc-info              = TCS.tc-info

type Bindings        = TCS.Bindings
empty-bindings       = TCS.empty-bindings

type FoldResult      = TCS.FoldResult
foldl2-result        = TCS.foldl2-result
foldl3-result        = TCS.foldl3-result
map-result           = TCS.map-result
fold-result          = TCS.fold-result
fold-errors          = TCS.fold-errors
bind                 = TCS.bind

type KeyEliminator   = (Type, SD.StringDict<Type>, Set<String> -> Type)
type DirectionInfo   = Pair<Type, KeyEliminator>

type Substitutions   = List<Pair<Type,Type>>

foo-name  = A.s-type-global("Foo")

example-name = A.s-atom("A", 1)
example-a = t-var(example-name)
example-b = t-var(A.s-atom("B", 2))
example-c = t-arrow([list: example-b, example-a], example-b)
example-d = t-arrow([list: example-b, example-b], example-a)
example-e = t-arrow([list: example-a], example-a)
example-f = t-arrow([list: example-e], example-b)
example-g = t-arrow([list: example-b], example-e)
example-h = t-arrow([list: example-e], example-e)
example-i = t-arrow([list: example-b], example-d)
example-j = t-arrow([list: example-b], example-b)
example-k = t-arrow([list: example-c], example-b)
example-l = t-arrow([list: example-b], example-c)
example-m = t-arrow([list: example-d], example-b)
example-n = t-name(none, foo-name)
example-o = t-top
example-p = t-bot
example-q = t-ref(t-number)
example-r = t-ref(example-a)

all-examples = [list: example-a, example-b, example-c, example-d, example-e,
                      example-f, example-g, example-h, example-i, example-j,
                      example-k, example-l, example-m, example-n, example-o,
                      example-p, example-q, example-r]

test-to-remove = [set: example-a]
test-info      = TCS.add-binding(example-a.id, t-top, TCS.empty-tc-info("test"))
example-a-promoted = t-top
example-a-demoted  = t-bot
example-b-promoted = example-b
example-b-demoted  = example-b
example-c-promoted = t-arrow([list: example-b-demoted, example-a-demoted], example-b-promoted)
example-c-demoted  = t-arrow([list: example-b-promoted, example-a-promoted], example-b-demoted)
example-d-promoted = t-arrow([list: example-b-demoted, example-b-demoted], example-a-promoted)
example-d-demoted  = t-arrow([list: example-b-promoted, example-b-promoted], example-a-demoted)
example-e-promoted = t-arrow([list: example-a-demoted], example-a-promoted)
example-e-demoted  = t-arrow([list: example-a-promoted], example-a-demoted)
example-f-promoted = t-arrow([list: example-e-demoted], example-b-promoted)
example-f-demoted  = t-arrow([list: example-e-promoted], example-b-demoted)
example-g-promoted = t-arrow([list: example-b-demoted], example-e-promoted)
example-g-demoted  = t-arrow([list: example-b-promoted], example-e-demoted)
example-h-promoted = t-arrow([list: example-e-demoted], example-e-promoted)
example-h-demoted  = t-arrow([list: example-e-promoted], example-e-demoted)
example-i-promoted = t-arrow([list: example-b-demoted], example-d-promoted)
example-i-demoted  = t-arrow([list: example-b-promoted], example-d-demoted)
example-j-promoted = t-arrow([list: example-b-demoted], example-b-promoted)
example-j-demoted  = t-arrow([list: example-b-promoted], example-b-demoted)
example-k-promoted = t-arrow([list: example-c-demoted], example-b-promoted)
example-k-demoted  = t-arrow([list: example-c-promoted], example-b-demoted)
example-l-promoted = t-arrow([list: example-b-demoted], example-c-promoted)
example-l-demoted  = t-arrow([list: example-b-promoted], example-c-demoted)
example-m-promoted = t-arrow([list: example-d-demoted], example-b-promoted)
example-m-demoted  = t-arrow([list: example-d-promoted], example-b-demoted)
example-n-promoted = t-name(none, foo-name)
example-n-demoted  = t-name(none, foo-name)
example-o-promoted = t-top
example-o-demoted  = t-top
example-p-promoted = t-bot
example-p-demoted  = t-bot
example-q-promoted = t-ref(t-number)
example-q-demoted  = t-ref(t-number)
example-r-promoted = t-top
example-r-demoted  = t-bot

fun create-substitutions(blame-loc :: A.Loc,
                         constraints :: TypeConstraints,
                         unknowns :: List<Type % (is-t-var)>,
                         r :: Type,
                         info :: TCInfo) -> FoldResult<Substitutions>:
  for map-result(unknown from unknowns):
    constraints
      .substitute(blame-loc, unknown, r, info)
      .map(pair(unknown, _))
  end
end

fun apply-substitutions(maybe-subs :: FoldResult<Substitutions>, typ :: Type):
  for bind(substitutions from maybe-subs):
    fold-result(for fold(curr from typ, substitution from substitutions):
      curr.substitute(substitution.left, substitution.right)
    end)
  end
end

fun arrow-constraints(blame-loc :: A.Loc, a-forall :: List<TypeVariable>,
                      a-args :: List<Type>, a-ret :: Type,
                      b-args :: List<Type>, b-ret :: Type,
                      info :: TCInfo) -> FoldResult<Substitutions>:
  unknowns-list = for map(x from a-forall):
    t-var(x.id)
  end
  unknowns-set = sets.list-to-list-set(unknowns-list)
  ret-constraints = generate-constraints(blame-loc, a-ret, b-ret, [set: ], unknowns-set, info)
  t-var-constraints = for fold2(wrapped from ret-constraints,
                                unknown from unknowns-list, x from a-forall):
    for bind(current from wrapped):
      generate-constraints(blame-loc, unknown, x.upper-bound, [set: ], unknowns-set, info)
        .map(_.meet(current, info))
    end
  end
  handle-args = foldl2-result(C.incorrect-number-of-args(blame-loc))
  for handle-args(curr from t-var-constraints,
                  b-arg from b-args, a-arg from a-args):
    generate-constraints(blame-loc, b-arg, a-arg, [set: ], unknowns-set, info)
      .map(_.meet(curr, info))
  end.bind(create-substitutions(blame-loc, _, unknowns-list, a-ret, info))
end

fun satisfies-type(here :: Type, there :: Type, info :: TCInfo) -> Boolean:
  cases(Type) here:
    | t-name(a-mod, a-id) =>
      cases(Type) there:
        | t-top => true
        | t-name(b-mod, b-id) =>
          (a-mod == b-mod) and (a-id == b-id)
        | t-record(there-fields) =>
          cases(Option<Type>) TCS.get-data-type(here, info):
            | some(data-type) =>
              fields-satisfy(data-type.fields, there-fields, info)
            | none =>
              false
          end
        | else => false
      end
    | t-var(a-id) =>
      cases(Type) there:
        | t-top => true
        | t-var(b-id) => a-id == b-id
        | else => false
      end
    | t-arrow(a-args, a-ret) =>
      cases(Type) there:
        | t-top => true
        | t-arrow(b-args, b-ret) =>
          # Order is important because contravariance!
          all2-strict(satisfies-type(_, _, info), b-args, a-args)
            and satisfies-type(a-ret, b-ret, info)
        | else => false
      end
    | t-forall(a-introduces, a-onto) =>
      fun process(maybe-subs :: FoldResult<Substitutions>, s :: Type, t :: Type):
        cases(FoldResult<Type>) apply-substitutions(maybe-subs, s):
          | fold-result(new-s) =>
            satisfies-type(new-s, t, info)
          | fold-errors(_) =>
            false
        end
      end
      cases(Type) a-onto:
        | t-arrow(a-args, a-ret) =>
          cases(Type) there:
            | t-arrow(b-args, b-ret) =>
              arrow-constraints(A.dummy-loc, a-introduces, a-args, a-ret, b-args, b-ret, info)
                ^ process(_, a-onto, there)
            | t-forall(b-introduces, b-onto) =>
              cases(Type) b-onto:
                | t-arrow(b-args, b-ret) =>
                  arrow-constraints(A.dummy-loc, a-introduces, a-args, a-ret, b-args, b-ret, info)
                    ^ process(_, a-onto, b-onto)
                | else =>
                  false # TODO(cody): Revisit this for more thought
              end
            | t-top => true
            | else => false
          end
        | else =>
          unknowns-list    = for map(x from a-introduces):
            t-var(x.id)
          end
          unknowns-set     = sets.list-to-list-set(unknowns-list)
          blame-loc = A.dummy-loc
          cases(Type) there:
            | t-forall(b-introduces, b-onto) =>
              onto-constraints = generate-constraints(blame-loc, a-onto, b-onto, [set: ], unknowns-set, info)
              for fold2(wrapped from onto-constraints,
                        unknown from unknowns-list, x from a-introduces):
                for bind(current from wrapped):
                  generate-constraints(blame-loc, unknown, x.upper-bound, [set: ], unknowns-set, info)
                    .map(_.meet(current, info))
                end
              end
                .bind(create-substitutions(blame-loc, _, unknowns-list, a-onto, info))
                ^ process(_, a-onto, b-onto)
            | t-top => true
            | else =>
              onto-constraints = generate-constraints(blame-loc, a-onto, there, [set: ], unknowns-set, info)
              for fold2(wrapped from onto-constraints,
                        unknown from unknowns-list, x from a-introduces):
                for bind(current from wrapped):
                  generate-constraints(blame-loc, unknown, x.upper-bound, [set: ], unknowns-set, info)
                    .map(_.meet(current, info))
                end
              end
                .bind(create-substitutions(blame-loc, _, unknowns-list, a-onto, info))
                ^ process(_, a-onto, there)
          end
      end
    | t-app(a-onto, a-args) =>
      cases(Type) there:
        | t-top => true
        | t-app(b-onto, b-args) =>
          (a-onto == b-onto) and
          cases(Option<DataType>) TCS.get-data-type(a-onto, info):
            | some(data-type) =>
              params-length = data-type.params.length()
              a-args-length = a-args.length()
              b-args-length = b-args.length()
              (params-length == a-args-length) and
              (a-args-length == b-args-length) and
              for fold3(base from true, param from data-type.params, a-arg from a-args, b-arg from b-args):
                base and cases(Variance) param.variance:
                  | constant      =>
                    raise("Internal type-checking error: Please send this program to the developers.")
                  | bivariant     =>
                    satisfies-type(a-arg, b-arg, info) or satisfies-type(b-arg, a-arg, info)
                  | covariant     =>
                    satisfies-type(a-arg, b-arg, info)
                  | contravariant =>
                    satisfies-type(b-arg, a-arg, info)
                  | invariant     =>
                    satisfies-type(a-arg, b-arg, info) and satisfies-type(b-arg, a-arg, info)
                end
              end
            | none =>
              false
          end
        | t-record(there-fields) =>
          cases(Option<Type>) TCS.get-data-type(here, info):
            | some(data-type) =>
              fields-satisfy(data-type.fields, there-fields, info)
            | none =>
              false
          end
        | else => false
      end
    | t-top => is-t-top(there)
    | t-bot => true
    | t-record(fields) =>
      cases(Type) there:
        | t-top => true
        | t-record(there-fields) =>
          fields-satisfy(fields, there-fields, info)
        | else => false
      end
    | t-ref(a-typ) =>
      cases(Type) there:
        | t-top => true
        | t-ref(b-typ) =>
          satisfies-type(a-typ, b-typ, info) and satisfies-type(b-typ, a-typ, info)
        | else => false
      end
  end
where:
  info = TCS.empty-tc-info("test")
  a1 = A.s-atom(gensym("A"), 3)
  a1-t = t-var(a1)
  a2 = A.s-atom(gensym("A"), 4)
  a2-t = t-var(a2)
  b1 = A.s-atom(gensym("B"), 5)
  b1-t = t-var(b1)
  forall-a = t-forall([list: t-variable(A.dummy-loc, a1, t-top, covariant)],
                      t-arrow([list: a1-t, a1-t], a1-t))
  forall-ab = t-forall([list: t-variable(A.dummy-loc, a2, t-top, invariant),
                              t-variable(A.dummy-loc, b1, t-top, contravariant)],
                       t-arrow([list: a2-t, b1-t], a2-t))
  num-fun = t-arrow([list: t-number, t-number], t-number)
  satisfies-type(forall-ab, forall-a, info) is true
  satisfies-type(forall-a, forall-ab, info) is false
  satisfies-type(forall-ab, num-fun, info) is true
  satisfies-type(forall-a, num-fun, info) is true
  for map(example from all-examples):
    example satisfies satisfies-type(_, t-top, info)
  end
  list-name = A.s-type-global("List")
  info.data-exprs.set-now(list-name.key(),
    TS.t-datatype(list-name.key(),
                 [list: t-variable(A.dummy-loc, A.s-atom("C", 6), t-top, covariant)],
                 empty, empty))
  t-list = lam(x): t-app(t-name(none, list-name), [list: x]);
  a = t-forall([list: t-variable(A.dummy-loc, a1, t-top, covariant)], t-list(a1-t))
  b = t-forall([list: t-variable(A.dummy-loc, b1, t-top, covariant)], t-list(b1-t))
  c = t-list(t-top)
  d = t-list(t-number)
  satisfies-type(a, b, info) is true
  satisfies-type(a, c, info) is true
  satisfies-type(b, c, info) is true
  satisfies-type(b, a, info) is true
  satisfies-type(c, a, info) is false
  satisfies-type(c, b, info) is false
  satisfies-type(a, a, info) is true
  satisfies-type(b, b, info) is true
  satisfies-type(c, c, info) is true

  satisfies-type(d, b, info) is false
  satisfies-type(d, c, info) is true
  satisfies-type(b, d, info) is true
  satisfies-type(d, a, info) is false
  satisfies-type(c, d, info) is false
  satisfies-type(a, d, info) is true
  satisfies-type(d, d, info) is true

  satisfies-type(t-ref(t-number), t-ref(t-number), info) is true
  satisfies-type(t-ref(t-number), t-ref(t-string), info) is false
  satisfies-type(t-ref(t-number), t-ref(t-top), info)    is false
  satisfies-type(t-ref(t-bot), t-ref(t-number), info)    is false
end

fun fields-satisfy(a-fields :: TypeMembers, b-fields :: TypeMembers, info :: TCInfo) -> Boolean:
  fun shares-name(here :: TypeMember):
    lam(there :: TypeMember):
      here.field-name == there.field-name
    end
  end
  for fold(good from true, b-field from b-fields):
    pred = shares-name(b-field)
    good and
    cases(Option<TypeMember>) a-fields.find(pred):
      | some(a-field) =>
        satisfies-type(a-field.typ, b-field.typ, info)
      | none     =>
        false
    end
  end
end

fun meet-fields(a-fields :: TypeMembers, b-fields :: TypeMembers, info :: TCInfo) -> TypeMembers:
  for fold(curr from empty, a-field from a-fields):
    field-name = a-field.field-name
    cases(Option<TypeMember>) type-members-lookup(b-fields, field-name):
      | some(b-field) =>
        link(t-member(field-name, least-upper-bound(a-field.typ, b-field.typ, info)), curr)
      | none =>
        curr
    end
  end
end

fun join-fields(a-fields :: TypeMembers, b-fields :: TypeMembers, info :: TCInfo) -> TypeMembers:
  for fold(curr from empty, a-field from a-fields):
    field-name = a-field.field-name
    cases(Option<TypeMember>) type-members-lookup(b-fields, field-name):
      | some(b-field) =>
        link(t-member(field-name, greatest-lower-bound(a-field.typ, b-field.typ, info)), curr)
      | none =>
        link(a-field, curr)
        curr
    end
  end
end

fun least-upper-bound(s :: Type, t :: Type, info :: TCInfo) -> Type:
  if satisfies-type(s, t, info):
    t
  else if satisfies-type(t, s, info):
    s
  else:
    cases(Type) s:
      | t-arrow(s-args, s-ret) =>
        cases(Type) t:
          | t-arrow(t-args, t-ret) =>
            cases (Option<List<Type>>) map2-strict(greatest-lower-bound(_, _, info), s-args, t-args):
              | some(m-args) =>
                j-typ  = least-upper-bound(s-ret, t-ret, info)
                t-arrow(m-args, j-typ)
              | else => t-top
            end
          | else => t-top
        end
      | t-app(s-onto, s-args) =>
        cases(Type) t:
          | t-app(t-onto, t-args) =>
            if (s-onto == t-onto) and (s-args == t-args):
              t-app(s-onto, s-args)
            else:
              t-top
            end
          | else => t-top
        end
      | t-record(s-fields) =>
        cases(Type) t:
          | t-record(t-fields) =>
            t-record(meet-fields(s-fields, t-fields, info))
          | else => t-top
        end
      | else => t-top
    end
  end
end

fun greatest-lower-bound(s :: Type, t :: Type, info :: TCInfo) -> Type:
  if satisfies-type(s, t, info):
    s
  else if satisfies-type(t, s, info):
    t
  else: cases(Type) s:
      | t-arrow(s-args, s-ret) => cases(Type) t:
          | t-arrow(t-args, t-ret) =>
            cases (Option<List<Type>>) map2-strict(least-upper-bound(_, _, info), s-args, t-args):
              | some(m-args) =>
                j-typ  = greatest-lower-bound(s-ret, t-ret, info)
                t-arrow(m-args, j-typ)
              | else => t-bot
            end
          | else => t-bot
        end
      | t-app(s-onto, s-args) => cases(Type) t:
          | t-app(t-onto, t-args) =>
            if (s-onto == t-onto) and (s-args == t-args):
              t-app(s-onto, s-args)
            else:
              t-bot
            end
          | else => t-bot
        end
      | t-record(s-fields) => cases(Type) t:
          | t-record(t-fields) =>
            t-record(join-fields(s-fields, t-fields, info))
          | else => t-bot
        end
      | else => t-bot
    end
  end
end


fun union<B>(a :: Set<B>, b :: Set<B>) -> Set<B>:
  a.union(b)
end

fun free-vars(t :: Type, binds :: Bindings) -> Set<Type>:
  fun add-free-var(typ :: Type):
    if binds.has-key(typ.key()):
      [set: ]
    else:
      [set: typ]
    end
  end
  cases(Type) t:
    | t-name(module-name, id) =>
      cases(Option<String>) module-name:
        | none =>
          [set: ]
        | some(_) =>
          [set: ]
      end
    | t-var(id) =>
      add-free-var(t)
    | t-arrow(args, ret) =>
      args
        .map(free-vars(_, binds))
        .foldl(union, free-vars(ret, binds))
    | t-forall(introduces, onto) =>
      new-binds = for fold(base from binds, tv from introduces):
        base.set(tv.id.key(), tv.upper-bound)
      end
      free-vars(onto, new-binds)
    | t-app(onto, args) =>
      args
        .map(free-vars(_, binds))
        .foldl(union, free-vars(onto, binds))
    | t-record(fields) =>
      fields
        .map(lam(field): free-vars(field.typ, binds);)
        .foldl(union, [set: ])
    | t-ref(typ) =>
      free-vars(typ, binds)
    | t-top =>
      [set: ]
    | t-bot =>
      [set: ]
    | else => raise("NYI(free-vars): " + torepr(t))
  end
where:
  free-vars(example-a, empty-bindings) is [set: example-a]
  free-vars(example-b, empty-bindings) is [set: example-b]
  free-vars(example-c, empty-bindings) is [set: example-a, example-b]
  free-vars(example-d, empty-bindings) is [set: example-a, example-b]
  free-vars(example-e, empty-bindings) is [set: example-a]
  free-vars(example-f, empty-bindings) is [set: example-a, example-b]
  free-vars(example-g, empty-bindings) is [set: example-a, example-b]
  free-vars(example-h, empty-bindings) is [set: example-a]
  free-vars(example-i, empty-bindings) is [set: example-a, example-b]
  free-vars(example-j, empty-bindings) is [set: example-b]
  free-vars(example-k, empty-bindings) is [set: example-a, example-b]
  free-vars(example-l, empty-bindings) is [set: example-a, example-b]
  free-vars(example-m, empty-bindings) is [set: example-a, example-b]
  free-vars(example-n, empty-bindings) is [set: ]
  free-vars(example-o, empty-bindings) is [set: ]
  free-vars(example-p, empty-bindings) is [set: ]
  free-vars(example-q, empty-bindings) is [set: ]
  free-vars(example-r, empty-bindings) is [set: example-a]
end

fun eliminate-variables(typ :: Type, to-remove :: Set<Type>,
                        _to :: DirectionInfo, _from :: DirectionInfo, info :: TCInfo
) -> Type:
  here  = eliminate-variables(_, _, _to, _from, _)
  there = eliminate-variables(_, _, _from, _to, _)
  to-typ = _to.left
  to-typ-move = _to.right
  if to-remove.member(typ):
    to-typ-move(typ, to-remove, info)
  else:
    cases(Type) typ:
      | t-name(_, _) =>
        typ
      | t-var(_) =>
        typ
      | t-arrow(args, ret) =>
        new-args = args.map(there(_, to-remove, info))
        new-ret  = here(ret, to-remove, info)
        t-arrow(new-args, new-ret)
      | t-forall(introduces, onto) =>
        bounded-free = for fold(base from sets.empty-list-set, tv from introduces):
          free = free-vars(tv.upper-bound, info.binds)
          base.union(free)
        end
        intersection = bounded-free.intersect(to-remove)
        set-is-empty = is-empty(intersection.to-list())
        if set-is-empty:
          new-info = introduces.foldl(TCS.add-type-variable, info)
          new-onto = here(onto, to-remove, new-info)
          t-forall(introduces, onto)
        else:
          to-typ
        end
      | t-app(onto, args) =>
        new-onto = here(onto, to-remove, info)
        fun process-args(params :: List<TypeVariable>, typs :: List<Type>, new-args :: List<Type>) -> Type:
          cases(List<TypeVariable>) params:
            | link(param, params-rest) =>
              cases(List<Type>) typs:
                | link(arg-typ, typs-rest) =>
                  fun process(t):
                    process-args(params-rest, typs-rest, link(t, new-args))
                  end
                  cases(Variance) param.variance:
                    | constant =>
                      raise("Internal type-checking error: Please send this program to the developers")
                    | covariant =>
                      process(here(arg-typ, to-remove, info))
                    | contravariant =>
                      process(there(arg-typ, to-remove, info))
                    | invariant =>
                      arg-typ-free = free-vars(arg-typ, empty-bindings)
                      intersection = arg-typ-free.intersect(to-remove)
                      set-is-empty = is-empty(intersection.to-list())
                      if set-is-empty:
                        process(arg-typ)
                      else:
                        to-typ
                      end
                  end
                | empty =>
                  to-typ
              end
            | empty =>
              cases(List<Type>) typs:
                | link(_, _) =>
                  to-typ
                | empty =>
                  t-app(new-onto, new-args.reverse())
              end
          end
        end
        cases(Option<DataType>) TCS.get-data-type(onto, info):
          | some(data-type) =>
            process-args(data-type.params, args, empty)
          | none =>
            to-typ
        end
      | t-record(fields) =>
        new-fields = for map(field from fields):
          t-member(field.field-name, here(field.typ, to-remove, info))
        end
        t-record(new-fields)
      | t-ref(arg-typ) =>
        arg-typ-free = free-vars(arg-typ, empty-bindings)
        intersection = arg-typ-free.intersect(to-remove)
        cases(List<Type>) intersection.to-list():
          | empty => t-ref(arg-typ)
          | link(_, _) => to-typ
        end
      | t-top =>
        t-top
      | t-bot =>
        t-bot
    end
  end
end

fun move-up(typ :: Type, to-remove :: Set<Type>, info :: TCInfo) -> Type:
  key = typ.key()
  if info.binds.has-key(key):
    least-supertype(info.binds.get-value(key), to-remove, info)
  else:
    raise("Couldn't find the key " + key + " in binds dictionary, so variable can't be eliminated! Existing keys: " + torepr(info.binds.keys().to-list()))
  end
end

fun move-down(_ :: Type, to-remove :: Set<Type>, info :: TCInfo) -> Type:
  t-bot
end

fun least-supertype(typ :: Type, to-remove :: Set<Type>, info :: TCInfo) -> Type:
  eliminate-variables(typ, to-remove, pair(t-top, move-up), pair(t-bot, move-down), info)
where:
  least-supertype(example-a, test-to-remove, test-info) is example-a-promoted
  least-supertype(example-b, test-to-remove, test-info) is example-b-promoted
  least-supertype(example-c, test-to-remove, test-info) is example-c-promoted
  least-supertype(example-d, test-to-remove, test-info) is example-d-promoted
  least-supertype(example-e, test-to-remove, test-info) is example-e-promoted
  least-supertype(example-f, test-to-remove, test-info) is example-f-promoted
  least-supertype(example-g, test-to-remove, test-info) is example-g-promoted
  least-supertype(example-h, test-to-remove, test-info) is example-h-promoted
  least-supertype(example-i, test-to-remove, test-info) is example-i-promoted
  least-supertype(example-j, test-to-remove, test-info) is example-j-promoted
  least-supertype(example-k, test-to-remove, test-info) is example-k-promoted
  least-supertype(example-l, test-to-remove, test-info) is example-l-promoted
  least-supertype(example-m, test-to-remove, test-info) is example-m-promoted
  least-supertype(example-n, test-to-remove, test-info) is example-n-promoted
  least-supertype(example-o, test-to-remove, test-info) is example-o-promoted
  least-supertype(example-p, test-to-remove, test-info) is example-p-promoted
  least-supertype(example-q, test-to-remove, test-info) is example-q-promoted
  least-supertype(example-r, test-to-remove, test-info) is example-r-promoted
end

fun greatest-subtype(typ :: Type, to-remove :: Set<Type>, info :: TCInfo) -> Type:
  eliminate-variables(typ, to-remove, pair(t-bot, move-down), pair(t-top, move-up), info)
where:
  greatest-subtype(example-a, test-to-remove, test-info) is example-a-demoted
  greatest-subtype(example-b, test-to-remove, test-info) is example-b-demoted
  greatest-subtype(example-c, test-to-remove, test-info) is example-c-demoted
  greatest-subtype(example-d, test-to-remove, test-info) is example-d-demoted
  greatest-subtype(example-e, test-to-remove, test-info) is example-e-demoted
  greatest-subtype(example-f, test-to-remove, test-info) is example-f-demoted
  greatest-subtype(example-g, test-to-remove, test-info) is example-g-demoted
  greatest-subtype(example-h, test-to-remove, test-info) is example-h-demoted
  greatest-subtype(example-i, test-to-remove, test-info) is example-i-demoted
  greatest-subtype(example-j, test-to-remove, test-info) is example-j-demoted
  greatest-subtype(example-k, test-to-remove, test-info) is example-k-demoted
  greatest-subtype(example-l, test-to-remove, test-info) is example-l-demoted
  greatest-subtype(example-m, test-to-remove, test-info) is example-m-demoted
  greatest-subtype(example-n, test-to-remove, test-info) is example-n-demoted
  greatest-subtype(example-o, test-to-remove, test-info) is example-o-demoted
  greatest-subtype(example-p, test-to-remove, test-info) is example-p-demoted
  greatest-subtype(example-q, test-to-remove, test-info) is example-q-demoted
  greatest-subtype(example-r, test-to-remove, test-info) is example-r-demoted
end

data TypeConstraint:
  | Equality(t :: Type) with:
    max(self) -> Type: self.t end,
    min(self) -> Type: self.t end,
    is-rigid(self, info :: TCInfo) -> Boolean: is-rigid-under(self.t, info.binds) end,
    is-tight(self, info :: TCInfo) -> Boolean: true end
  | Bounds(s :: Type, t :: Type) with:
    max(self) -> Type: self.t end,
    min(self) -> Type: self.s end,
    is-rigid(self, info :: TCInfo) -> Boolean:
      (self.s == self.t) and is-rigid-under(self.s, info.binds)
    end,
    is-tight(self, info :: TCInfo) -> Boolean:
      satisfies-type(self.s, self.t, info) and satisfies-type(self.t, self.s, info)
    end
sharing:
  meet(self, other :: TypeConstraint, info :: TCInfo) -> Option<TypeConstraint>:
    undefined = none
    cases(TypeConstraint) self:
      | Equality(s) =>
        cases(TypeConstraint) other:
          | Equality(t) =>
            if s == t:
              some(self)
            else:
              undefined
            end
          | Bounds(u, v) =>
            if satisfies-type(u, s, info) and satisfies-type(s, v, info):
              some(self)
            else:
              undefined
            end
        end
      | Bounds(s, t) =>
        cases(TypeConstraint) other:
          | Equality(u) =>
            if satisfies-type(s, u, info) and satisfies-type(u, t, info):
              some(other)
            else:
              undefined
            end
          | Bounds(u, v) =>
            j = least-upper-bound(s, u, info)
            m = greatest-lower-bound(t, v, info)
            if satisfies-type(j, m, info):
              some(Bounds(j, m))
            else:
              undefined
            end
        end
    end
  end
end

fun determine-variance(typ, var-id :: Name, info :: TCInfo) -> Variance:
  cases(Type) typ:
    | t-name(module-name, id) =>
      constant
    | t-var(id) =>
      if id.key() == var-id.key():
        covariant
      else:
        constant
      end
    | t-arrow(args, ret) =>
      for fold(base from constant, arg from args):
        base.join(determine-variance(arg, var-id, info))
      end.flip().join(determine-variance(ret, var-id, info))
    | t-app(onto, args) =>
      cases(Option<DataType>) TCS.get-data-type(onto, info):
        | some(data-type) =>
          result = for fold2-strict(base from constant, param from data-type.params, arg from args):
            cases(Variance) param.variance:
              | constant      =>
                if arg == t-var(var-id):
                  constant
                else:
                  determine-variance(arg, var-id, info)
                end
              | bivariant     =>
                if arg == t-var(var-id):
                  bivariant
                else:
                  determine-variance(arg, var-id, info)
                end
              | covariant     =>
                determine-variance(arg, var-id, info)
              | contravariant =>
                determine-variance(arg, var-id, info).flip()
              | invariant     =>
                cases(Variance) determine-variance(arg, var-id, info):
                  | constant => constant
                  | else => invariant
                end
            end.join(base)
          end
          cases(Option<Variance>) result:
            | some(v) => v
            | none => raise("Internal type-checking error: Please send this program to the developers.")
          end
        | none =>
          raise("internal type-checking error. This shouldn't ever happen. " + tostring(typ) + " isn't actually a data type! Available data types are: " + mut-dict-to-string(info.data-exprs))
      end
    | t-top =>
      constant
    | t-bot =>
      constant
    | t-record(fields) =>
      for fold(base from constant, tm from fields):
        base.join(determine-variance(tm.typ, var-id, info))
      end
    | t-forall(introduces, onto) =>
      # TODO(cody): Rename all introduces in onto to avoid conflict.
      determine-variance(onto, var-id, info)
    | t-ref(_) =>
      invariant
  end
where:
  info = TCS.empty-tc-info("test")
  determine-variance(example-a, example-name, info) is covariant
  determine-variance(example-b, example-name, info) is constant
  determine-variance(example-c, example-name, info) is contravariant
  determine-variance(example-d, example-name, info) is covariant
  determine-variance(example-e, example-name, info) is invariant
  determine-variance(example-f, example-name, info) is invariant
  determine-variance(example-g, example-name, info) is invariant
  determine-variance(example-h, example-name, info) is invariant
  determine-variance(example-i, example-name, info) is covariant
  determine-variance(example-j, example-name, info) is constant
  determine-variance(example-k, example-name, info) is covariant
  determine-variance(example-l, example-name, info) is contravariant
  determine-variance(example-m, example-name, info) is contravariant
  determine-variance(example-n, example-name, info) is constant
  determine-variance(example-o, example-name, info) is constant
  determine-variance(example-p, example-name, info) is constant
  determine-variance(example-q, example-name, info) is invariant
  determine-variance(example-q, example-name, info) is invariant
end

fun is-bottom-variable(x :: Type, binds :: Bindings) -> Boolean:
  key = x.key()
  binds.has-key(key) and
  let bound = binds.get-value(key):
    is-t-bot(bound) or is-bottom-variable(bound, binds)
  end
end

fun is-rigid(v :: Variance) -> Boolean:
  TS.is-invariant(v) # This should be fine for now.
end

fun is-rigid-under(r :: Type, binds :: Bindings) -> Boolean:
  is-t-top(r) or
  (is-t-bot(r) and for fold(curr from true, key from binds.keys().to-list()):
                     curr and not(is-t-bot(binds.get-value(key)))
                   end) or
  not(is-bottom-variable(r, binds)) or
  cases(Type) r:
    | t-arrow(arg-typs, ret) =>
      # TODO(cody): Implement this
      raise("t-arrow not yet handled in is-rigid-under")
    | else =>
      raise("This shouldn't happen!")
  end
end

data TypeConstraints:
  | type-constraints(dict :: SD.StringDict<Option<TypeConstraint>>)
sharing:
  _insert(self, typ-str :: String, constraint :: TypeConstraint, info :: TCInfo) -> TypeConstraints:
    new-constraint = if self.dict.has-key(typ-str):
                       cases (Option<TypeConstraint>) (self.dict.get-value(typ-str)):
                         | none => none
                         | some(tc) => tc.meet(constraint, info)
                       end
                     else:
                       some(constraint)
                     end
    type-constraints(self.dict.set(typ-str, new-constraint))
  end,
  insert(self, typ :: Type, constraint :: TypeConstraint, info :: TCInfo) -> TypeConstraints:
    typ-str = typ.key()
    self._insert(typ-str, constraint, info)
  end,
  get(self, typ :: Type) -> Option<TypeConstraint>:
    typ-str = typ.key()
    if self.dict.has-key(typ-str):
      self.dict.get-value(typ-str)
    else:
      some(Bounds(t-bot, t-top))
    end
  end,
  meet(self, other :: TypeConstraints, info :: TCInfo) -> TypeConstraints:
    keys = other.dict.keys().to-list()
    for fold(curr from self, key from keys):
      cases(Option<TypeConstraint>) other.dict.get-value(key):
        | some(t) =>
          curr._insert(key, t, info)
        | none =>
          type-constraints(curr.dict.set(key, none))
      end
    end
  end,
  substitute(self, blame-loc :: A.Loc, x :: Type % (is-t-var), r :: Type, info :: TCInfo) -> FoldResult<Type>:
    cases(Option<TypeConstraint>) self.get(x):
      | some(constraint) =>
        variance = determine-variance(r, x.id, info)
        if TS.is-constant(variance) or TS.is-covariant(variance):
          fold-result(constraint.min())
        else if TS.is-bivariant(variance) or TS.is-contravariant(variance):
          fold-result(constraint.max())
        else if TS.is-invariant(variance) and constraint.is-tight(info):
          fold-result(constraint.min())
        else if is-rigid(variance) and constraint.is-rigid(info):
          fold-result(constraint.min())
        else:
          fold-errors([list: C.unable-to-instantiate(blame-loc)])
        end
      | none    =>
        fold-errors([list: C.unable-to-instantiate(blame-loc)])
    end
  end,
  _output(self):
    VS.vs-constr("type-constraints", [list: VS.vs-value(dict-to-string(self.dict))])
  end
end

rec empty-type-constraints = type-constraints(SD.make-string-dict())

fun handle-matching(s-introduces :: List<TypeVariable>, t-introduces :: List<TypeVariable>, to-remove :: Set<Type>, unknowns :: Set<Type>, info :: TCInfo):
  # TODO(cody): Check that foralls are the same
  tmp-binds = for fold(curr from SD.make-string-dict(), y from s-introduces):
    curr.set(y.id.key(), y.upper-bound)
  end
  ks-ds = for fold(curr from pair(tmp-binds, empty-type-constraints), t-f from t-introduces):
    key    = t-f.id
    s-f-b  = if curr.left.has-key(key):
               curr.left.get-value(key)
             else:
               t-f.upper-bound
             end
    result = matching(s-f-b, t-f.upper-bound, info.binds, to-remove, unknowns)
    new-ks = curr.left.set(key, result.left)
    new-ds = result.right.meet(curr.right, info)
    pair(new-ks, new-ds)
  end
  introduced      = ks-ds.left
  for-constraints = ks-ds.right
  new-info        = for fold(curr from info, y from introduced.keys().to-list()):
    TCS.add-binding-string(y, introduced.get-value(y), curr)
  end
  new-to-remove   = for fold(curr from to-remove, f from s-introduces + t-introduces):
    curr.add(t-var(f.id))
  end
  {
    introduced: introduced,
    constraints: for-constraints,
    info: new-info,
    to-remove: new-to-remove
  }
end

fun generate-constraints(blame-loc :: A.Loc, s :: Type, t :: Type, to-remove :: Set<Type>, unknowns :: Set<Type>, info :: TCInfo) -> FoldResult<TypeConstraints>:
  binds   = info.binds
  s-free  = free-vars(s, binds)
  s-str   = s.key()
  t-free  = free-vars(t, binds)
  initial = empty-type-constraints
  if is-t-top(t):
    fold-result(initial)
  else if is-t-bot(s):
    fold-result(initial)
  else if unknowns.member(s) and is-empty(t-free.intersect(unknowns).to-list()):
    r = greatest-subtype(t, to-remove, info)
    fold-result(initial.insert(s, Bounds(t-bot, r), info))
  else if unknowns.member(t) and is-empty(s-free.intersect(unknowns).to-list()):
    r = least-supertype(s, to-remove, info)
    fold-result(initial.insert(t, Bounds(r, t-top), info))
  else if s == t:
    fold-result(initial)
  else if binds.has-key(s-str):
    generate-constraints(blame-loc, binds.get-value(s-str), t, to-remove, unknowns, info)
  else:
    cases(Type) s:
      | t-arrow(s-args, s-ret) =>
        cases(Type) t:
          | t-arrow(t-args, t-ret) =>
            ret-constraints = generate-constraints(blame-loc, s-ret, t-ret, to-remove, unknowns, info)
            args-fold = foldl2-result(C.incorrect-type(tostring(s), blame-loc, tostring(t), blame-loc))
            for args-fold(curr from ret-constraints,
                          s-arg from s-args, t-arg from t-args):
              generate-constraints(blame-loc, t-arg, s-arg, to-remove, unknowns, info)
                .map(_.meet(curr, info))
            end
          | t-forall(t-introduces, t-onto) =>
            matched = handle-matching(empty, t-introduces, to-remove, unknowns, info)
            generate-constraints(blame-loc, s, t-onto, matched.to-remove, unknowns, matched.info)
              .map(_.meet(matched.constraints, info))
          | else =>
            fold-errors([list: C.unable-to-instantiate(blame-loc)])
        end
      | t-forall(s-introduces, s-onto) =>
        cases(Type) t:
          | t-forall(t-introduces, t-onto) =>
            matched = handle-matching(s-introduces, t-introduces, to-remove, unknowns, info)
            generate-constraints(blame-loc, s-onto, t-onto, matched.to-remove, unknowns, matched.info)
              .map(_.meet(matched.constraints, info))
          | else =>
            matched = handle-matching(s-introduces, empty, to-remove, unknowns, info)
            generate-constraints(blame-loc, s-onto, t, matched.to-remove, unknowns, matched.info)
              .map(_.meet(matched.constraints, info))
        end
      | t-app(s-onto, s-args) =>
        cases(Type) t:
          | t-app(t-onto, t-args) =>
            onto-constraints = generate-constraints(blame-loc, s-onto, t-onto, to-remove, unknowns, info)
            cases(Option<DataType>) TCS.get-data-type(s-onto, info):
              | some(data-type) =>
                args-fold = foldl3-result(C.incorrect-type(tostring(s), blame-loc, tostring(t), blame-loc))
                for args-fold(curr from onto-constraints,
                              param from data-type.params,
                              s-arg from s-args, t-arg from t-args):
                  result-a = generate-constraints(blame-loc, s-arg, t-arg, to-remove, unknowns, info)
                  result-b = generate-constraints(blame-loc, t-arg, s-arg, to-remove, unknowns, info)
                  cases(Variance) param.variance:
                    | constant =>
                      raise("Internal type-checking error: Please send this program to the developers.")
                    | bivariant     => result-a
                    | covariant     => result-a
                    | contravariant => result-b
                    | invariant     =>
                      for bind(unwrapped from result-a):
                        result-b.map(_.meet(unwrapped, info))
                      end
                  end.map(_.meet(curr, info))
                end
              | none =>
                fold-errors([list: C.unable-to-instantiate(blame-loc)])
            end
          | else =>
            fold-errors([list: C.unable-to-instantiate(blame-loc)])
        end
      | t-ref(s-typ) =>
        cases(Type) t:
          | t-ref(t-typ) =>
            result-a = generate-constraints(blame-loc, s-typ, t-typ, to-remove, unknowns, info)
            result-b = generate-constraints(blame-loc, t-typ, s-typ, to-remove, unknowns, info)
            for bind(unwrapped from result-a):
              result-b.map(_.meet(unwrapped, info))
            end
          | else =>
            fold-errors([list: C.unable-to-instantiate(blame-loc)])
        end
      | else =>
        fold-errors([list: C.unable-to-instantiate(blame-loc)])
    end
  end
end

fun matching(s :: Type, t :: Type, binds :: Bindings, to-remove :: Set<Type>, unknowns :: Set<Type>) -> Pair<Type,TypeConstraints>:
  ru-union = to-remove.union(unknowns)
  if is-t-top(s) and is-t-top(t):
    pair(t-top, empty-type-constraints)
  else if is-t-bot(s) and is-t-bot(t):
    pair(t-bot, empty-type-constraints)
  else if unknowns.member(s) and is-empty(free-vars(t, binds).intersect(ru-union).to-list()):
    pair(t, empty-type-constraints.insert(s, Equality(t)))
  else if unknowns.member(t) and is-empty(free-vars(s, binds).intersect(ru-union).to-list()):
    pair(s, empty-type-constraints.insert(t, Equality(s)))
  else if (s == t) and not(unknowns.member(s)):
    pair(s, empty-type-constraints)
  else:
    cases(Type) s:
      | t-arrow(s-arg-typs, s-ret) =>
        cases(Type) t:
          | t-arrow(t-arg-typs, t-ret) =>
            # TODO(cody): Implement this section
            raise("Not yet implemented")
          | else =>
            raise("The matching relationship should match everything")
        end
      | t-forall(s-introduces, s-onto) =>
        cases(Type) t:
          | t-forall(t-introduces, t-onto) =>
            # TODO(cody): Implement this section
            raise("Not yet implemented")
          | else =>
            raise("The matching relationship should match everything")
        end
      | t-ref(s-typ) =>
        cases(Type) t:
          | t-ref(t-typ) =>
            # TODO(cody): Implement this section
            raise("Not yet implemented")
          | else =>
            raise("The matching relationship should match everything")
        end
      | else =>
        raise("The matching relationship should match everything")
    end
  end
end
