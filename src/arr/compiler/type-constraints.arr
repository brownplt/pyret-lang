provide { generate-constraints   : generate-constraints,
          arrow-constraints      : arrow-constraints,
          empty-type-constraints : empty-type-constraints,
          satisfies-type         : satisfies-type,
          least-upper-bound      : least-upper-bound,
          greatest-lower-bound   : greatest-lower-bound,
          meet-fields            : meet-fields } end

provide-types { TypeConstraint  : TypeConstraint,
                TypeConstraints : TypeConstraints,
                Substitutions   : Substitutions }

import ast as A
import string-dict as SD
import "compiler/type-structs.arr" as TS
import "compiler/type-check-structs.arr" as TCS
import "compiler/list-aux.arr" as LA
import "compiler/compile-structs.arr" as C

all2-strict  = LA.all2-strict
map2-strict  = LA.map2-strict
fold2-strict = LA.fold2-strict

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

t-number             = TS.t-number
t-string             = TS.t-string
t-boolean            = TS.t-boolean
t-srcloc             = TS.t-srcloc

type TypeMember      = TS.TypeMember
type TypeMembers     = TS.TypeMembers
empty-type-members   = TS.empty-type-members
t-member             = TS.t-member
type-members-lookup  = TS.type-members-lookup

type TypeVariable    = TS.TypeVariable
t-variable           = TS.t-variable
is-t-top             = TS.is-t-top
is-t-bot             = TS.is-t-bot
is-t-var             = TS.is-t-var

type TCInfo          = TCS.TCInfo
tc-info              = TCS.tc-info

type Bindings        = TCS.Bindings
empty-bindings       = TCS.empty-bindings

type FoldResult      = TCS.FoldResult
foldl2-result        = TCS.foldl2-result
map-result           = TCS.map-result
fold-result          = TCS.fold-result
fold-errors          = TCS.fold-errors
bind                 = TCS.bind

type KeyEliminator   = (Type, SD.StringDict<Type>, Set<String> -> Type)
type DirectionInfo   = Pair<Type, KeyEliminator>

type Substitutions   = List<Pair<Type,Type>>

example-t-var = t-var("A")
example-a = t-var("A")
example-b = t-var("B")
example-c = t-arrow(A.dummy-loc, [list: example-b, example-a], example-b)
example-d = t-arrow(A.dummy-loc, [list: example-b, example-b], example-a)
example-e = t-arrow(A.dummy-loc, [list: example-a], example-a)
example-f = t-arrow(A.dummy-loc, [list: example-e], example-b)
example-g = t-arrow(A.dummy-loc, [list: example-b], example-e)
example-h = t-arrow(A.dummy-loc, [list: example-e], example-e)
example-i = t-arrow(A.dummy-loc, [list: example-b], example-d)
example-j = t-arrow(A.dummy-loc, [list: example-b], example-b)
example-k = t-arrow(A.dummy-loc, [list: example-c], example-b)
example-l = t-arrow(A.dummy-loc, [list: example-b], example-c)
example-m = t-arrow(A.dummy-loc, [list: example-d], example-b)
example-n = t-name(A.dummy-loc, none, "Foo")
example-o = t-top
example-p = t-bot

test-to-remove = [set: example-a]
test-binds     = SD.immutable-string-dict().set(example-a.tostring(), t-top)
example-a-promoted = t-top
example-a-demoted  = t-bot
example-b-promoted = example-b
example-b-demoted  = example-b
example-c-promoted = t-arrow(A.dummy-loc, [list: example-b-demoted, example-a-demoted], example-b-promoted)
example-c-demoted  = t-arrow(A.dummy-loc, [list: example-b-promoted, example-a-promoted], example-b-demoted)
example-d-promoted = t-arrow(A.dummy-loc, [list: example-b-demoted, example-b-demoted], example-a-promoted)
example-d-demoted  = t-arrow(A.dummy-loc, [list: example-b-promoted, example-b-promoted], example-a-demoted)
example-e-promoted = t-arrow(A.dummy-loc, [list: example-a-demoted], example-a-promoted)
example-e-demoted  = t-arrow(A.dummy-loc, [list: example-a-promoted], example-a-demoted)
example-f-promoted = t-arrow(A.dummy-loc, [list: example-e-demoted], example-b-promoted)
example-f-demoted  = t-arrow(A.dummy-loc, [list: example-e-promoted], example-b-demoted)
example-g-promoted = t-arrow(A.dummy-loc, [list: example-b-demoted], example-e-promoted)
example-g-demoted  = t-arrow(A.dummy-loc, [list: example-b-promoted], example-e-demoted)
example-h-promoted = t-arrow(A.dummy-loc, [list: example-e-demoted], example-e-promoted)
example-h-demoted  = t-arrow(A.dummy-loc, [list: example-e-promoted], example-e-demoted)
example-i-promoted = t-arrow(A.dummy-loc, [list: example-b-demoted], example-d-promoted)
example-i-demoted  = t-arrow(A.dummy-loc, [list: example-b-promoted], example-d-demoted)
example-j-promoted = t-arrow(A.dummy-loc, [list: example-b-demoted], example-b-promoted)
example-j-demoted  = t-arrow(A.dummy-loc, [list: example-b-promoted], example-b-demoted)
example-k-promoted = t-arrow(A.dummy-loc, [list: example-c-demoted], example-b-promoted)
example-k-demoted  = t-arrow(A.dummy-loc, [list: example-c-promoted], example-b-demoted)
example-l-promoted = t-arrow(A.dummy-loc, [list: example-b-demoted], example-c-promoted)
example-l-demoted  = t-arrow(A.dummy-loc, [list: example-b-promoted], example-c-demoted)
example-m-promoted = t-arrow(A.dummy-loc, [list: example-d-demoted], example-b-promoted)
example-m-demoted  = t-arrow(A.dummy-loc, [list: example-d-promoted], example-b-demoted)
example-n-promoted = t-name(A.dummy-loc, none, "Foo")
example-n-demoted  = t-name(A.dummy-loc, none, "Foo")
example-o-promoted = t-top
example-o-demoted  = t-top
example-p-promoted = t-bot
example-p-demoted  = t-bot


fun dict-to-string(dict :: SD.StringDict) -> String:
  "{"
    + for map(key from dict.keys()):
        key + " => " + torepr(dict.get(key))
      end.join-str(", ")
    + "}"
end

fun create-substitutions(constraints :: TypeConstraints,
                         unknowns :: List<Type % (is-t-var)>,
                         r :: Type,
                         info :: TCInfo) -> FoldResult<Substitutions>:
  for map-result(unknown from unknowns):
    constraints
      .substitute(unknown, r, info)
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
  ret-constraints = generate-constraints(a-ret, b-ret, [set: ], unknowns-set, info)
  t-var-constraints = for fold2(wrapped from ret-constraints,
                                unknown from unknowns-list, x from a-forall):
    for bind(current from wrapped):
      generate-constraints(unknown, x.upper-bound, [set: ], unknowns-set, info)
        .map(_.meet(current, info))
    end
  end
  handle-args = foldl2-result(C.incorrect-number-of-args(blame-loc))
  for handle-args(curr from t-var-constraints,
                  b-arg from b-args, a-arg from a-args):
    generate-constraints(b-arg, a-arg, [set: ], unknowns-set, info)
      .map(_.meet(curr, info))
  end.bind(create-substitutions(_, unknowns-list, a-ret, info))
end

fun satisfies-type(here :: Type, there :: Type, info :: TCInfo) -> Boolean:
  cases(Type) here:
    | t-name(_, a-mod, a-id) =>
      cases(Type) there:
        | t-top => true
        | t-name(_, b-mod, b-id) =>
          (a-mod == b-mod) and (a-id == b-id)
        | t-record(_, there-fields) =>
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
    | t-arrow(_, a-args, a-ret) =>
      cases(Type) there:
        | t-top => true
        | t-arrow(_, b-args, b-ret) =>
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
        | t-arrow(_, a-args, a-ret) =>
          cases(Type) there:
            | t-arrow(_, b-args, b-ret) =>
              arrow-constraints(A.dummy-loc, a-introduces, a-args, a-ret, b-args, b-ret, info)
                ^ process(_, a-onto, there)
            | t-forall(b-introduces, b-onto) =>
              cases(Type) b-onto:
                | t-arrow(_, b-args, b-ret) =>
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
          cases(Type) there:
            | t-forall(b-introduces, b-onto) =>
              onto-constraints = generate-constraints(a-onto, b-onto, [set: ], unknowns-set, info)
              for fold2(wrapped from onto-constraints,
                        unknown from unknowns-list, x from a-introduces):
                for bind(current from wrapped):
                  generate-constraints(unknown, x.upper-bound, [set: ], unknowns-set, info)
                    .map(_.meet(current, info))
                end
              end
                .bind(create-substitutions(_, unknowns-list, a-onto, info))
                ^ process(_, a-onto, b-onto)
            | t-top => true
            | else =>
              onto-constraints = generate-constraints(a-onto, there, [set: ], unknowns-set, info)
              for fold2(wrapped from onto-constraints,
                        unknown from unknowns-list, x from a-introduces):
                for bind(current from wrapped):
                  generate-constraints(unknown, x.upper-bound, [set: ], unknowns-set, info)
                    .map(_.meet(current, info))
                end
              end
                .bind(create-substitutions(_, unknowns-list, a-onto, info))
                ^ process(_, a-onto, there)
          end
      end
    | t-app(_, a-onto, a-args) =>
      cases(Type) there:
        | t-top => true
        | t-app(_, b-onto, b-args) =>
          a-onto._equal(b-onto) and all2-strict(lam(a, b): a.equal(b);, a-args, b-args)
        | t-record(_, there-fields) =>
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
    | t-record(_, fields) =>
      cases(Type) there:
        | t-top => true
        | t-record(_, there-fields) =>
          fields-satisfy(fields, there-fields, info)
        | else => false
      end
  end
where:
  info = TCS.empty-tc-info()
  a1 = t-var("A1")
  forall-a = t-forall([list: t-variable(A.dummy-loc, "A1", t-top)],
                      t-arrow(A.dummy-loc, [list: a1, a1], t-var("A1")))
  forall-ab = t-forall([list: t-variable(A.dummy-loc, "A2", t-top),
                              t-variable(A.dummy-loc, "B1", t-top)],
                       t-arrow(A.dummy-loc, [list: t-var("A2"), t-var("B1")], t-var("A2")))
  satisfies-type(forall-ab, forall-a, info) is true
  satisfies-type(forall-a, forall-ab, info) is false
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
      | t-arrow(_, s-args, s-ret) =>
        cases(Type) t:
          | t-arrow(_, t-args, t-ret) =>
            cases (Option<List<Type>>) map2-strict(greatest-lower-bound(_, _, info), s-args, t-args):
              | some(m-args) =>
                j-typ  = least-upper-bound(s-ret, t-ret, info)
                t-arrow(A.dummy-loc, m-args, j-typ)
              | else => t-top
            end
          | else => t-top
        end
      | t-app(_, s-onto, s-args) =>
        cases(Type) t:
          | t-app(_, t-onto, t-args) =>
            if (s-onto == t-onto) and (s-args == t-args):
              t-app(A.dummy-loc, s-onto, s-args)
            else:
              t-top
            end
          | else => t-top
        end
      | t-record(_, s-fields) =>
        cases(Type) t:
          | t-record(_, t-fields) =>
            t-record(A.dummy-loc, meet-fields(s-fields, t-fields, info))
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
      | t-arrow(s-l, s-args, s-ret) => cases(Type) t:
          | t-arrow(_, t-args, t-ret) =>
            cases (Option<List<Type>>) map2-strict(least-upper-bound(_, _, info), s-args, t-args):
              | some(m-args) =>
                j-typ  = greatest-lower-bound(s-ret, t-ret, info)
                t-arrow(A.dummy-loc, m-args, j-typ)
              | else => t-bot
            end
          | else => t-bot
        end
      | t-app(_, s-onto, s-args) => cases(Type) t:
          | t-app(_, t-onto, t-args) =>
            if (s-onto == t-onto) and (s-args == t-args):
              t-app(A.dummy-loc, s-onto, s-args)
            else:
              t-bot
            end
          | else => t-bot
        end
      | t-record(_, s-fields) => cases(Type) t:
          | t-record(_, t-fields) =>
            t-record(A.dummy-loc, join-fields(s-fields, t-fields, info))
          | else => t-bot
        end
      | else => t-bot
    end
  end
end


fun <B> union(a :: Set<B>, b :: Set<B>) -> Set<B>:
  a.union(b)
end

fun free-vars(t :: Type, binds :: Bindings) -> Set<Type>:
  fun add-free-var(typ :: Type):
    if binds.has-key(typ.tostring()):
      [set: ]
    else:
      [set: typ]
    end
  end
  cases(Type) t:
    | t-name(l, module-name, id) =>
      cases(Option<String>) module-name:
        | none =>
          [set: ]
        | some(_) =>
          [set: ]
      end
    | t-var(id) =>
      add-free-var(t)
    | t-arrow(l, args, ret) =>
      args
        .map(free-vars(_, binds))
        .foldl(union, free-vars(ret, binds))
    | t-forall(introduces, onto) =>
      new-binds = for fold(base from binds, tv from introduces):
        base.set(tv.id, tv.upper-bound)
      end
      free-vars(onto, new-binds)
    | t-app(l, onto, args) =>
      args
        .map(free-vars(_, binds))
        .foldl(union, free-vars(onto, binds))
    | t-record(_, fields) =>
      fields
        .map(lam(field): free-vars(field.typ, binds);)
        .foldl(union, [set: ])
    | t-top =>
      [set: ]
    | t-bot =>
      [set: ]
    | else => raise("NYI(free-vars): " + torepr(t))
  end
where:
  free-vars(example-a, empty-bindings) satisfies [set: example-a]._equals
  free-vars(example-b, empty-bindings) satisfies [set: example-b]._equals
  free-vars(example-c, empty-bindings) satisfies [set: example-a, example-b]._equals
  free-vars(example-d, empty-bindings) satisfies [set: example-a, example-b]._equals
  free-vars(example-e, empty-bindings) satisfies [set: example-a]._equals
  free-vars(example-f, empty-bindings) satisfies [set: example-a, example-b]._equals
  free-vars(example-g, empty-bindings) satisfies [set: example-a, example-b]._equals
  free-vars(example-h, empty-bindings) satisfies [set: example-a]._equals
  free-vars(example-i, empty-bindings) satisfies [set: example-a, example-b]._equals
  free-vars(example-j, empty-bindings) satisfies [set: example-b]._equals
  free-vars(example-k, empty-bindings) satisfies [set: example-a, example-b]._equals
  free-vars(example-l, empty-bindings) satisfies [set: example-a, example-b]._equals
  free-vars(example-m, empty-bindings) satisfies [set: example-a, example-b]._equals
  free-vars(example-n, empty-bindings) satisfies [set: ]._equals
  free-vars(example-o, empty-bindings) satisfies [set: ]._equals
  free-vars(example-p, empty-bindings) satisfies [set: ]._equals
end

fun eliminate-variables(typ :: Type, binds :: Bindings, to-remove :: Set<Type>,
                        _to :: DirectionInfo, _from :: DirectionInfo) -> Type:
  here  = eliminate-variables(_, _, _, _to, _from)
  there = eliminate-variables(_, _, _, _from, _to)
  to-typ = _to.left
  to-typ-move = _to.right
  if to-remove.member(typ):
    to-typ-move(typ, binds, to-remove)
  else:
    cases(Type) typ:
      | t-name(_, _, _) =>
        typ
      | t-var(_) =>
        typ
      | t-arrow(l, args, ret) =>
        new-args = args.map(there(_, binds, to-remove))
        new-ret  = here(ret, binds, to-remove)
        t-arrow(l, new-args, new-ret)
      | t-forall(introduces, onto) =>
        bounded-free = for fold(base from sets.empty-list-set, tv from introduces):
          free = free-vars(tv.upper-bound, binds)
          base.union(free)
        end
        intersection = bounded-free.intersect(to-remove)
        set-is-empty = is-empty(intersection.to-list())
        if set-is-empty:
          new-binds  = for fold(base from binds, x from introduces):
            base.set(x.id, x.upper-bound)
          end
          new-onto   = here(onto, new-binds, to-remove)
          t-forall(introduces, onto)
        else:
          to-typ
        end
      | t-top =>
        t-top
      | t-bot =>
        t-bot
    end
  end
end

fun move-up(typ :: Type, binds :: Bindings, to-remove :: Set<Type>) -> Type:
  key = typ.tostring()
  if binds.has-key(key):
    least-supertype(binds.get(key), binds, to-remove)
  else:
    raise("Couldn't find the key " + key + " in binds dictionary, so variable can't be eliminated! Existing keys: " + torepr(binds.keys()))
  end
end

fun move-down(_ :: Type, binds :: Bindings, to-remove :: Set<Type>) -> Type:
  t-bot
end

fun least-supertype(typ :: Type, binds :: Bindings, to-remove :: Set<Type>) -> Type:
  eliminate-variables(typ, binds, to-remove, pair(t-top, move-up), pair(t-bot, move-down))
where:
  least-supertype(example-a, test-binds, test-to-remove) is example-a-promoted
  least-supertype(example-b, test-binds, test-to-remove) is example-b-promoted
  least-supertype(example-c, test-binds, test-to-remove) is example-c-promoted
  least-supertype(example-d, test-binds, test-to-remove) is example-d-promoted
  least-supertype(example-e, test-binds, test-to-remove) is example-e-promoted
  least-supertype(example-f, test-binds, test-to-remove) is example-f-promoted
  least-supertype(example-g, test-binds, test-to-remove) is example-g-promoted
  least-supertype(example-h, test-binds, test-to-remove) is example-h-promoted
  least-supertype(example-i, test-binds, test-to-remove) is example-i-promoted
  least-supertype(example-j, test-binds, test-to-remove) is example-j-promoted
  least-supertype(example-k, test-binds, test-to-remove) is example-k-promoted
  least-supertype(example-l, test-binds, test-to-remove) is example-l-promoted
  least-supertype(example-m, test-binds, test-to-remove) is example-m-promoted
  least-supertype(example-n, test-binds, test-to-remove) is example-n-promoted
  least-supertype(example-o, test-binds, test-to-remove) is example-o-promoted
  least-supertype(example-p, test-binds, test-to-remove) is example-p-promoted
end

fun greatest-subtype(typ :: Type, binds :: Bindings, to-remove :: Set<Type>) -> Type:
  eliminate-variables(typ, binds, to-remove, pair(t-bot, move-down), pair(t-top, move-up))
where:
  greatest-subtype(example-a, test-binds, test-to-remove) is example-a-demoted
  greatest-subtype(example-b, test-binds, test-to-remove) is example-b-demoted
  greatest-subtype(example-c, test-binds, test-to-remove) is example-c-demoted
  greatest-subtype(example-d, test-binds, test-to-remove) is example-d-demoted
  greatest-subtype(example-e, test-binds, test-to-remove) is example-e-demoted
  greatest-subtype(example-f, test-binds, test-to-remove) is example-f-demoted
  greatest-subtype(example-g, test-binds, test-to-remove) is example-g-demoted
  greatest-subtype(example-h, test-binds, test-to-remove) is example-h-demoted
  greatest-subtype(example-i, test-binds, test-to-remove) is example-i-demoted
  greatest-subtype(example-j, test-binds, test-to-remove) is example-j-demoted
  greatest-subtype(example-k, test-binds, test-to-remove) is example-k-demoted
  greatest-subtype(example-l, test-binds, test-to-remove) is example-l-demoted
  greatest-subtype(example-m, test-binds, test-to-remove) is example-m-demoted
  greatest-subtype(example-n, test-binds, test-to-remove) is example-n-demoted
  greatest-subtype(example-o, test-binds, test-to-remove) is example-o-demoted
  greatest-subtype(example-p, test-binds, test-to-remove) is example-p-demoted
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


fun is-constant(x :: Type % (is-t-var), r :: Type) -> Boolean:
  if x == r:
    false
  else:
    cases(Type) r:
      | t-arrow(l, args, ret) =>
        args-okay = for fold(base from true, arg from args):
                      base and is-constant(x, arg)
                    end
        args-okay and is-constant(x, ret)
      | else =>
        true
    end
  end
where:
  is-constant(example-a, example-a) is false
  is-constant(example-a, example-b) is true
  is-constant(example-a, example-c) is false
  is-constant(example-a, example-d) is false
  is-constant(example-a, example-e) is false
  is-constant(example-a, example-f) is false
  is-constant(example-a, example-g) is false
  is-constant(example-a, example-h) is false
  is-constant(example-a, example-i) is false
  is-constant(example-a, example-j) is true
  is-constant(example-a, example-k) is false
  is-constant(example-a, example-l) is false
  is-constant(example-a, example-m) is false
  is-constant(example-a, example-n) is true
  is-constant(example-a, example-o) is true
  is-constant(example-a, example-p) is true
end

fun is-covariant(x :: Type % (is-t-var), r :: Type) -> Boolean:
  if x == r:
    true
  else:
    cases(Type) r:
      | t-arrow(l, args, ret) =>
        var arg-is-contravariant = false
        args-okay = for fold(base from true, arg from args):
                      if is-constant(x, arg):
                        base
                      else if is-contravariant(x, arg):
                        arg-is-contravariant := true
                        base
                      else:
                        false
                      end
                    end
        args-okay and (is-covariant(x, ret) or (arg-is-contravariant and is-constant(x, ret)))
      | else =>
        false
    end
  end
where:
  is-covariant(example-a, example-a) is true
  is-covariant(example-a, example-b) is false
  is-covariant(example-a, example-c) is false
  is-covariant(example-a, example-d) is true
  is-covariant(example-a, example-e) is false
  is-covariant(example-a, example-f) is false
  is-covariant(example-a, example-g) is false
  is-covariant(example-a, example-h) is false
  is-covariant(example-a, example-i) is true
  is-covariant(example-a, example-j) is false
  is-covariant(example-a, example-k) is true
  is-covariant(example-a, example-l) is false
  is-covariant(example-a, example-m) is false
  is-covariant(example-a, example-n) is false
  is-covariant(example-a, example-o) is false
  is-covariant(example-a, example-p) is false
end

fun is-contravariant(x :: Type % (is-t-var), r :: Type) -> Boolean:
  cases(Type) r:
    | t-arrow(l, args, ret) =>
      var arg-is-covariant = false
      args-okay = for fold(base from true, arg from args):
                    if is-constant(x, arg):
                      base
                    else if is-covariant(x, arg):
                      arg-is-covariant := true
                      base
                    else:
                      false
                    end
                  end
      args-okay and (is-contravariant(x, ret) or (arg-is-covariant and is-constant(x, ret)))
    | else =>
      false
  end
where:
  is-contravariant(example-a, example-a) is false
  is-contravariant(example-a, example-b) is false
  is-contravariant(example-a, example-c) is true
  is-contravariant(example-a, example-d) is false
  is-contravariant(example-a, example-e) is false
  is-contravariant(example-a, example-f) is false
  is-contravariant(example-a, example-g) is false
  is-contravariant(example-a, example-h) is false
  is-contravariant(example-a, example-i) is false
  is-contravariant(example-a, example-j) is false
  is-contravariant(example-a, example-k) is false
  is-contravariant(example-a, example-l) is true
  is-contravariant(example-a, example-m) is true
  is-contravariant(example-a, example-n) is false
  is-contravariant(example-a, example-o) is false
  is-contravariant(example-a, example-p) is false
end

fun is-invariant(x :: Type % (is-t-var), r :: Type) -> Boolean:
  cases(Type) r:
    | t-arrow(l, args, ret) =>
      var arg-is-invariant = false
      var arg-is-covariant = false
      for each(arg from args):
        if is-covariant(x, arg):
          arg-is-covariant := true
        else if is-invariant(x, arg):
          arg-is-invariant := true
        else:
          nothing
        end
      end
      (arg-is-covariant and is-covariant(x, ret)) or arg-is-invariant or is-invariant(x, ret)
    | else =>
      false
  end
where:
  is-invariant(example-a, example-a) is false
  is-invariant(example-a, example-b) is false
  is-invariant(example-a, example-c) is false
  is-invariant(example-a, example-d) is false
  is-invariant(example-a, example-e) is true
  is-invariant(example-a, example-f) is true
  is-invariant(example-a, example-g) is true
  is-invariant(example-a, example-h) is true
  is-invariant(example-a, example-i) is false
  is-invariant(example-a, example-j) is false
  is-invariant(example-a, example-k) is false
  is-invariant(example-a, example-l) is false
  is-invariant(example-a, example-m) is false
  is-invariant(example-a, example-n) is false
  is-invariant(example-a, example-o) is false
  is-invariant(example-a, example-p) is false
end


fun is-bottom-variable(x :: Type, binds :: Bindings) -> Boolean:
  key = x.tostring()
  binds.has-key(key) and
  let bound = binds.get(key):
    is-t-bot(bound) or is-bottom-variable(bound, binds)
  end
end

fun is-rigid(x :: Type % (is-t-var), r :: Type) -> Boolean:
  is-invariant(x, r) # This should be fine for now.
end

fun is-rigid-under(r :: Type, binds :: Bindings) -> Boolean:
  is-t-top(r) or
  (is-t-bot(r) and for fold(curr from true, key from binds.keys()):
                     curr and not(is-t-bot(binds.get(key)))
                   end) or
  not(is-bottom-variable(r, binds)) or
  cases(Type) r:
    | t-arrow(_, arg-typs, ret) =>
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
                       self.dict.get(typ-str).and-then(_.meet(constraint, info))
                     else:
                       some(constraint)
                     end
    type-constraints(self.dict.set(typ-str, new-constraint))
  end,
  insert(self, typ :: Type, constraint :: TypeConstraint, info :: TCInfo) -> TypeConstraints:
    typ-str = typ.tostring()
    self._insert(typ-str, constraint, info)
  end,
  get(self, typ :: Type) -> Option<TypeConstraint>:
    typ-str = typ.tostring()
    if self.dict.has-key(typ-str):
      self.dict.get(typ-str)
    else:
      some(Bounds(t-bot, t-top))
    end
  end,
  meet(self, other :: TypeConstraints, info :: TCInfo) -> TypeConstraints:
    keys = other.dict.keys()
    for fold(curr from self, key from keys):
      cases(Option<TypeConstraint>) other.dict.get(key):
        | some(t) =>
          curr._insert(key, t, info)
        | none =>
          type-constraints(curr.dict.set(key, none))
      end
    end
  end,
  substitute(self, x :: Type % (is-t-var), r :: Type, info :: TCInfo) -> FoldResult<Type>:
    blame-loc = A.dummy-loc
    cases(Option<TypeConstraint>) self.get(x):
      | some(constraint) =>
        if is-constant(x, r) or is-covariant(x, r):
          fold-result(constraint.min())
        else if is-contravariant(x, r):
          fold-result(constraint.max())
        else if is-invariant(x, r) and constraint.is-tight(info):
          fold-result(constraint.min())
        else if is-rigid(x, r) and constraint.is-rigid(info):
          fold-result(constraint.min())
        else:
          fold-errors([list: C.unable-to-instantiate(blame-loc)])
        end
      | none    =>
        fold-errors([list: C.unable-to-instantiate(blame-loc)])
    end
  end,
  tostring(self) -> String:
    "type-constraints("
      + dict-to-string(self.dict)
      + ")"
  end
end

# empty-type-constraints = type-constraints(SD.immutable-string-dict())
# TODO(cody): Uncomment above and remove definitions in below functions below once `rec' keyword exists

fun handle-matching(s-introduces :: List<TypeVariable>, t-introduces :: List<TypeVariable>, to-remove :: Set<Type>, unknowns :: Set<Type>, info :: TCInfo):
  empty-type-constraints = type-constraints(SD.immutable-string-dict())
  # TODO(cody): Check that foralls are the same
  tmp-binds = for fold(curr from SD.immutable-string-dict(), y from s-introduces):
    curr.set(y.id, y.upper-bound)
  end
  ks-ds = for fold(curr from pair(tmp-binds, empty-type-constraints), t-f from t-introduces):
    key    = t-f.id
    s-f-b  = if curr.left.has-key(key):
               curr.left.get(key)
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
  new-info        = for fold(curr from info, y from introduced.keys()):
    TCS.add-binding(y, introduced.get(y), curr)
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

fun generate-constraints(s :: Type, t :: Type, to-remove :: Set<Type>, unknowns :: Set<Type>, info :: TCInfo) -> FoldResult<TypeConstraints>:
  empty-type-constraints = type-constraints(SD.immutable-string-dict())
  binds   = info.binds
  s-free  = free-vars(s, binds)
  s-str   = s.tostring()
  t-free  = free-vars(t, binds)
  initial = empty-type-constraints
  if is-t-top(t):
    fold-result(initial)
  else if is-t-bot(s):
    fold-result(initial)
  else if unknowns.member(s) and is-empty(t-free.intersect(unknowns).to-list()):
    r = greatest-subtype(t, binds, to-remove)
    fold-result(initial.insert(s, Bounds(t-bot, r), info))
  else if unknowns.member(t) and is-empty(s-free.intersect(unknowns).to-list()):
    r = least-supertype(s, binds, to-remove)
    fold-result(initial.insert(t, Bounds(r, t-top), info))
  else if s._equal(t):
    fold-result(initial)
  else if binds.has-key(s-str):
    generate-constraints(binds.get(s-str), t, to-remove, unknowns, info)
  else:
    cases(Type) s:
      | t-arrow(s-l, s-args, s-ret) =>
        cases(Type) t:
          | t-arrow(t-l, t-args, t-ret) =>
            ret-constraints = generate-constraints(s-ret, t-ret, to-remove, unknowns, info)
            args-fold = foldl2-result(C.incorrect-type(s.tostring(), s.toloc(), t.tostring(), t.toloc()))
            for args-fold(curr from ret-constraints,
                          s-arg from s-args, t-arg from t-args):
              generate-constraints(t-arg, s-arg, to-remove, unknowns, info)
                .map(_.meet(curr, info))
            end
          | t-forall(t-introduces, t-onto) =>
            matched = handle-matching(empty, t-introduces, to-remove, unknowns, info)
            generate-constraints(s, t-onto, matched.to-remove, unknowns, matched.info)
              .map(_.meet(matched.constraints, info))
          | else =>
            fold-errors([list: ]) # TODO(cody): Make CompileError for this
        end
      | t-forall(s-introduces, s-onto) =>
        cases(Type) t:
          | t-forall(t-introduces, t-onto) =>
            matched = handle-matching(s-introduces, t-introduces, to-remove, unknowns, info)
            generate-constraints(s-onto, t-onto, matched.to-remove, unknowns, matched.info)
              .map(_.meet(matched.constraints, info))
          | else =>
            matched = handle-matching(s-introduces, empty, to-remove, unknowns, info)
            generate-constraints(s-onto, t, matched.to-remove, unknowns, matched.info)
              .map(_.meet(matched.constraints, info))
        end
      | else =>
        fold-errors([list: ]) # TODO(cody): Make CompileError for this
    end
  end
end

fun matching(s :: Type, t :: Type, binds :: Bindings, to-remove :: Set<Type>, unknowns :: Set<Type>) -> Pair<Type,TypeConstraints>:
  empty-type-constraints = type-constraints(SD.immutable-string-dict())
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
      | t-arrow(s-l, s-arg-typs, s-ret) =>
        cases(Type) t:
          | t-arrow(t-l, t-arg-typs, t-ret) =>
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
      | else =>
        raise("The matching relationship should match everything")
    end
  end
end

empty-type-constraints = type-constraints(SD.immutable-string-dict())
