provide { generate-constraints   : generate-constraints,
          empty-type-constraints : empty-type-constraints,
          satisfies-type         : satisfies-type,
          least-upper-bound      : least-upper-bound,
          greatest-lower-bound   : greatest-lower-bound,
          meet-fields            : meet-fields } end

provide-types { TypeConstraint  : TypeConstraint,
                TypeConstraints : TypeConstraints }

import ast as A
import string-dict as SD
import "compiler/type-structs.arr" as TS
import "compiler/type-check-structs.arr" as TCS
import "compiler/list-aux.arr" as LA

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

t-number             = TS.t-number
t-string             = TS.t-string
t-boolean            = TS.t-boolean
t-srcloc             = TS.t-srcloc

type TypeMember      = TS.TypeMember
type TypeMembers     = TS.TypeMembers
empty-type-members   = TS.empty-type-members
t-member             = TS.t-member
type-members-lookup  = TS.type-members-lookup

t-variable           = TS.t-variable
is-t-top             = TS.is-t-top
is-t-bot             = TS.is-t-bot
is-t-var             = TS.is-t-var

type TCInfo          = TCS.TCInfo
tc-info              = TCS.tc-info

type Bindings        = TCS.Bindings
empty-bindings       = TCS.empty-bindings

type KeyEliminator   = (Type, SD.StringDict<Type>, Set<String> -> Type)
type DirectionInfo   = Pair<Type, KeyEliminator>

example-t-var = t-var("A")
example-a = t-var("A")
example-b = t-var("B")
example-c = t-arrow(A.dummy-loc, [list:], [list: example-b, example-a], example-b)
example-d = t-arrow(A.dummy-loc, [list:], [list: example-b, example-b], example-a)
example-e = t-arrow(A.dummy-loc, [list:], [list: example-a], example-a)
example-f = t-arrow(A.dummy-loc, [list:], [list: example-e], example-b)
example-g = t-arrow(A.dummy-loc, [list:], [list: example-b], example-e)
example-h = t-arrow(A.dummy-loc, [list:], [list: example-e], example-e)
example-i = t-arrow(A.dummy-loc, [list:], [list: example-b], example-d)
example-j = t-arrow(A.dummy-loc, [list:], [list: example-b], example-b)
example-k = t-arrow(A.dummy-loc, [list:], [list: example-c], example-b)
example-l = t-arrow(A.dummy-loc, [list:], [list: example-b], example-c)
example-m = t-arrow(A.dummy-loc, [list:], [list: example-d], example-b)
example-n = t-name(A.dummy-loc, none, "Foo")
example-o = t-top
example-p = t-bot

test-to-remove = [set: example-a]
test-binds     = SD.immutable-string-dict().set(example-a.tostring(), t-top)
example-a-promoted = t-top
example-a-demoted  = t-bot
example-b-promoted = example-b
example-b-demoted  = example-b
example-c-promoted = t-arrow(A.dummy-loc, [list:], [list: example-b-demoted, example-a-demoted], example-b-promoted)
example-c-demoted  = t-arrow(A.dummy-loc, [list:], [list: example-b-promoted, example-a-promoted], example-b-demoted)
example-d-promoted = t-arrow(A.dummy-loc, [list:], [list: example-b-demoted, example-b-demoted], example-a-promoted)
example-d-demoted  = t-arrow(A.dummy-loc, [list:], [list: example-b-promoted, example-b-promoted], example-a-demoted)
example-e-promoted = t-arrow(A.dummy-loc, [list:], [list: example-a-demoted], example-a-promoted)
example-e-demoted  = t-arrow(A.dummy-loc, [list:], [list: example-a-promoted], example-a-demoted)
example-f-promoted = t-arrow(A.dummy-loc, [list:], [list: example-e-demoted], example-b-promoted)
example-f-demoted  = t-arrow(A.dummy-loc, [list:], [list: example-e-promoted], example-b-demoted)
example-g-promoted = t-arrow(A.dummy-loc, [list:], [list: example-b-demoted], example-e-promoted)
example-g-demoted  = t-arrow(A.dummy-loc, [list:], [list: example-b-promoted], example-e-demoted)
example-h-promoted = t-arrow(A.dummy-loc, [list:], [list: example-e-demoted], example-e-promoted)
example-h-demoted  = t-arrow(A.dummy-loc, [list:], [list: example-e-promoted], example-e-demoted)
example-i-promoted = t-arrow(A.dummy-loc, [list:], [list: example-b-demoted], example-d-promoted)
example-i-demoted  = t-arrow(A.dummy-loc, [list:], [list: example-b-promoted], example-d-demoted)
example-j-promoted = t-arrow(A.dummy-loc, [list:], [list: example-b-demoted], example-b-promoted)
example-j-demoted  = t-arrow(A.dummy-loc, [list:], [list: example-b-promoted], example-b-demoted)
example-k-promoted = t-arrow(A.dummy-loc, [list:], [list: example-c-demoted], example-b-promoted)
example-k-demoted  = t-arrow(A.dummy-loc, [list:], [list: example-c-promoted], example-b-demoted)
example-l-promoted = t-arrow(A.dummy-loc, [list:], [list: example-b-demoted], example-c-promoted)
example-l-demoted  = t-arrow(A.dummy-loc, [list:], [list: example-b-promoted], example-c-demoted)
example-m-promoted = t-arrow(A.dummy-loc, [list:], [list: example-d-demoted], example-b-promoted)
example-m-demoted  = t-arrow(A.dummy-loc, [list:], [list: example-d-promoted], example-b-demoted)
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
    | t-arrow(_, a-forall, a-args, a-ret) =>
      cases(Type) there:
        | t-top => true
        | t-arrow(_, b-forall, b-args, b-ret) =>
          all2-strict(_ == _, a-forall, b-forall)
                # order is important because contravariance!
            and all2-strict(satisfies-type(_, _, info), b-args, a-args)
            and satisfies-type(a-ret, b-ret, info)
        | else => false
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
      | some(tm) =>
        satisfies-type(tm.typ, b-field.typ, info)
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
      | t-arrow(_, s-forall, s-args, s-ret) =>
        cases(Type) t:
          | t-arrow(_, t-forall, t-args, t-ret) =>
            if s-forall == t-forall:
              cases (Option<List<Type>>) map2-strict(greatest-lower-bound(_, _, info), s-args, t-args):
                | some(m-args) =>
                  j-typ  = least-upper-bound(s-ret, t-ret, info)
                  t-arrow(A.dummy-loc, s-forall, m-args, j-typ)
                | else => t-top
              end
            else:
              t-top
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
      | t-arrow(s-l, s-forall, s-args, s-ret) => cases(Type) t:
          | t-arrow(_, t-forall, t-args, t-ret) =>
            if s-forall == t-forall:
              cases (Option<List<Type>>) map2-strict(least-upper-bound(_, _, info), s-args, t-args):
                | some(m-args) =>
                  j-typ  = greatest-lower-bound(s-ret, t-ret, info)
                  t-arrow(A.dummy-loc, s-forall, m-args, j-typ)
                | else => t-bot
              end
            else:
              t-bot
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
    | t-arrow(l, forall, args, ret) =>
      new-binds = for fold(base from binds, typ from forall):
        binds.set(typ.id, typ.upper-bound)
      end
      args
        .map(free-vars(_, new-binds))
        .foldl(union, free-vars(ret, new-binds))
    | t-app(l, onto, args) =>
      args
        .map(free-vars(_, binds))
        .foldl(union, free-vars(onto, binds))
    | t-top =>
      [set: ]
    | t-bot =>
      [set: ]
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
      | t-arrow(l, forall, args, ret) =>
        bounded-free = for fold(base from sets.empty-list-set, x from forall):
                         free = free-vars(x.upper-bound, binds)
                         base.union(free)
                       end
        intersection = bounded-free.intersect(to-remove)
        set-is-empty = is-empty(intersection.to-list())
        if set-is-empty:
          new-binds  = for fold(base from binds, x from forall):
                         base.set(x.id, x.upper-bound)
                       end
          new-args = args.map(there(_, new-binds, to-remove))
          new-ret  = here(ret, new-binds, to-remove)
          t-arrow(l, forall, new-args, new-ret)
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
            some(Bounds(j, m))
        end
    end
  end
end


fun is-constant(x :: Type % (is-t-var), r :: Type) -> Boolean:
  if x == r:
    false
  else:
    cases(Type) r:
      | t-arrow(l, forall, args, ret) =>
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
      | t-arrow(l, forall, args, ret) =>
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
    | t-arrow(l, forall, args, ret) =>
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
    | t-arrow(l, forall, args, ret) =>
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
    | t-arrow(_, forall, arg-typs, ret) =>
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
          # TODO(cody): Don't blow up
          raise("Cannot meet two TypeConstraint sets!")
      end
    end
  end,
  substitute(self, x :: Type % (is-t-var), r :: Type, info :: TCInfo) -> Type:
    constraint = cases(Option<TypeConstraint>) self.get(x):
                   | some(c) => c
                   | none    =>
                     # TODO(cody): Don't blow up
                     raise("Can't satisfy constraints!")
                 end
    if is-constant(x, r) or is-covariant(x, r):
      constraint.min()
    else if is-contravariant(x, r):
      constraint.max()
    else if is-invariant(x, r) and constraint.is-tight(info):
      constraint.min()
    else if is-rigid(x, r) and constraint.is-rigid(info):
      constraint.min()
    else:
      raise("Substitution is undefined! x = " + torepr(x) + ", r = " + torepr(r) + ", constraint = " + torepr(constraint))
    end
  end,
  tostring(self) -> String:
    "type-constraints("
      + dict-to-string(self.dict)
      + ")"
  end
end

empty-type-constraints = type-constraints(SD.immutable-string-dict())

fun generate-constraints(s :: Type, t :: Type, to-remove :: Set<Type>, unknowns :: Set<Type>, info :: TCInfo) -> TypeConstraints:
  binds = info.binds
  s-free  = free-vars(s, binds)
  s-str   = s.tostring()
  t-free  = free-vars(t, binds)
  initial = empty-type-constraints
  if is-t-top(t):
    initial
  else if is-t-bot(s):
    initial
  else if unknowns.member(s) and is-empty(t-free.intersect(unknowns).to-list()):
    r = greatest-subtype(t, binds, to-remove)
    initial.insert(s, Bounds(t-bot, r), info)
  else if unknowns.member(t) and is-empty(s-free.intersect(unknowns).to-list()):
    r = least-supertype(s, binds, to-remove)
    initial.insert(t, Bounds(r, t-top), info)
  else if s._equal(t):
    initial
  else if binds.has-key(s-str):
    generate-constraints(binds.get(s-str), t, to-remove, unknowns, info)
  else:
    cases(Type) s:
      | t-arrow(s-l, s-forall, s-args, s-ret) =>
        cases(Type) t:
          | t-arrow(t-l, t-forall, t-args, t-ret) =>
            # TODO(cody): Check that foralls are the same
            tmp-binds       = for fold(curr from SD.immutable-string-dict(), y from s-forall):
                                curr.set(y.id, y.upper-bound)
                              end
            ks-ds = for fold(curr from pair(tmp-binds, empty-type-constraints), t-f from t-forall):
                      key    = t-f.id
                      s-f-b  = if curr.left.has-key(key):
                                 curr.left.get(key)
                               else:
                                 t-f.upper-bound
                               end
                      result = matching(s-f-b, t-f.upper-bound, binds, to-remove, unknowns)
                      new-ks = curr.left.set(key, result.left)
                      new-ds = result.right.meet(curr.right, info)
                      pair(new-ks, new-ds)
                    end
            introduced      = ks-ds.left
            for-constraints = ks-ds.right
            new-info        = for fold(curr from info, y from introduced.keys()):
                                TCS.add-binding(y, introduced.get(y), curr)
                              end
            new-to-remove   = for fold(curr from to-remove, f from s-forall + t-forall):
                                curr.add(t-var(f.id))
                              end
            arg-constraints = for fold2(curr from empty-type-constraints,
                                        s-arg from s-args, t-arg from t-args):
                                result = generate-constraints(t-arg, s-arg, new-to-remove, unknowns, new-info)
                                curr.meet(result, info)
                              end
            ret-constraints = generate-constraints(s-ret, t-ret, new-to-remove, unknowns, new-info)
            for-constraints.meet(arg-constraints.meet(ret-constraints, info), info)
          | else =>
            raise("What happens here?")
        end
      | else =>
        raise("What happens here?")
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
      | t-arrow(s-l, s-forall, s-arg-typs, s-ret) =>
        cases(Type) t:
          | t-arrow(t-l, t-forall, t-arg-typs, t-ret) =>
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

