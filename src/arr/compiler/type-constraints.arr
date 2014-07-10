provide { generate-constraints   : generate-constraints,
          empty-type-constraints : empty-type-constraints } end

provide-types { TypeConstraint  : TypeConstraint,
                TypeConstraints : TypeConstraints }

import ast as A
import string-dict as SD
import "compiler/type-structs.arr" as TS

type Type            = TS.Type
type Pair            = TS.Pair
pair                 = TS.pair
t-name               = TS.t-name
t-var                = TS.t-var
t-arrow              = TS.t-arrow
t-top                = TS.t-top
t-bot                = TS.t-bot
t-number             = TS.t-number
t-string             = TS.t-string
t-boolean            = TS.t-boolean
t-srcloc             = TS.t-srcloc
t-variable           = TS.t-variable
is-t-top             = TS.is-t-top
is-t-bot             = TS.is-t-bot
is-t-var             = TS.is-t-var
least-upper-bound    = TS.least-upper-bound
greatest-lower-bound = TS.greatest-lower-bound

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

fun dict-to-string(dict :: SD.StringDict) -> String:
  "{"
    + for map(key from dict.keys()):
        key + " => " + torepr(dict.get(key))
      end.join-str(", ")
    + "}"
end

fun free-vars(t :: Type) -> Set<Type>:
  cases(Type) t:
    | t-name(l, module-name, id) =>
      cases(Option<String>) module-name:
        | none =>
          [set: ]
        | some(_) =>
          [set: ]
      end
    | t-var(id) =>
      [set: t]
    | t-arrow(l, forall, args, ret) =>
      free = for fold(base from free-vars(ret), arg from args):
               base.union(free-vars(arg))
             end
      for fold(base from free, typ from forall):
        base.remove(typ.id)
      end
    | t-top =>
      [set: ]
    | t-bot =>
      [set: ]
  end
where:
  free-vars(example-a) satisfies [set: example-a]._equals
  free-vars(example-b) satisfies [set: example-b]._equals
  free-vars(example-c) satisfies [set: example-a, example-b]._equals
  free-vars(example-d) satisfies [set: example-a, example-b]._equals
  free-vars(example-e) satisfies [set: example-a]._equals
  free-vars(example-f) satisfies [set: example-a, example-b]._equals
  free-vars(example-g) satisfies [set: example-a, example-b]._equals
  free-vars(example-h) satisfies [set: example-a]._equals
  free-vars(example-i) satisfies [set: example-a, example-b]._equals
  free-vars(example-j) satisfies [set: example-b]._equals
  free-vars(example-k) satisfies [set: example-a, example-b]._equals
  free-vars(example-l) satisfies [set: example-a, example-b]._equals
  free-vars(example-m) satisfies [set: example-a, example-b]._equals
  free-vars(example-n) satisfies [set: ]._equals
  free-vars(example-o) satisfies [set: ]._equals
  free-vars(example-p) satisfies [set: ]._equals
end

type Bindings      = SD.StringDict<Type>
type KeyEliminator = (Type, SD.StringDict<Type>, Set<String> -> Type)
type DirectionInfo = Pair<Type, KeyEliminator>

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
                         free = free-vars(x.upper-bound)
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
  result = eliminate-variables(typ, binds, to-remove, pair(t-top, move-up), pair(t-bot, move-down))
  result
end

fun greatest-subtype(typ :: Type, binds :: Bindings, to-remove :: Set<Type>) -> Type:
  result = eliminate-variables(typ, binds, to-remove, pair(t-bot, move-down), pair(t-top, move-up))
  result
end

data TypeConstraint:
  | Equality(t :: Type) with:
    max(self) -> Type: self.t end,
    min(self) -> Type: self.t end,
    is-rigid(self, binds :: Bindings) -> Boolean: is-rigid-under(self.t, binds) end,
    is-tight(self) -> Boolean: true end
  | Bounds(s :: Type, t :: Type) with:
    max(self) -> Type: self.t end,
    min(self) -> Type: self.s end,
    is-rigid(self, binds :: Bindings) -> Boolean:
      (self.s == self.t) and is-rigid-under(self.s, binds)
    end,
    is-tight(self) -> Boolean:
      self.s.satisfies-type(self.t) and self.t.satisfies-type(self.s)
    end
sharing:
  meet(self, other :: TypeConstraint) -> Option<TypeConstraint>:
    undefined = none
    cases(TypeConstraint) self:
      | Equality(s) =>
        cases(TypeConstraint) other:
          | Equality(t) =>
            if s._equals(t):
              some(self)
            else:
              undefined
            end
          | Bounds(u, v) =>
            if u.satisfies-type(s) and s.satisfies-type(v):
              some(self)
            else:
              undefined
            end
        end
      | Bounds(s, t) =>
        cases(TypeConstraint) other:
          | Equality(u) =>
            if s.satisfies-type(u) and u.satisfies-type(t):
              some(other)
            else:
              undefined
            end
          | Bounds(u, v) =>
            j = least-upper-bound(s, u)
            m = greatest-lower-bound(t, v)
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
        for fold(base from true, arg from args):
          base and is-constant(x, arg)
        end and is-constant(x, ret)
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
        for fold(base from true, arg from args):
          if is-constant(x, arg):
            base
          else if is-contravariant(x, arg):
            arg-is-contravariant := true
            base
          else:
            false
          end
        end and (is-covariant(x, ret) or (arg-is-contravariant and is-constant(x, ret)))
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
      for fold(base from true, arg from args):
        if is-constant(x, arg):
          base
        else if is-covariant(x, arg):
          arg-is-covariant := true
          base
        else:
          false
        end
      end and (is-contravariant(x, ret) or (arg-is-covariant and is-constant(x, ret)))
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
  block:
    bound = binds.get(key)
    is-t-bot(bound) or
    is-bottom-variable(bound, binds)
  end
end

fun is-rigid(x :: Type % (is-t-var), r :: Type) -> Boolean:
  cases(Type) r:
    | t-arrow(l, forall, args, ret) =>
      for fold(base from false, arg from args):
        base or is-invariant(x, arg)
      end or is-invariant(x, ret)
    | else =>
      false
  end
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
  _insert(self, typ-str :: String, constraint :: TypeConstraint) -> TypeConstraints:
    new-constraint = if self.dict.has-key(typ-str):
                       self.dict.get(typ-str).and-then(_.meet(constraint))
                     else:
                       some(constraint)
                     end
    type-constraints(self.dict.set(typ-str, new-constraint))
  end,
  insert(self, typ :: Type, constraint :: TypeConstraint) -> TypeConstraints:
    typ-str = typ.tostring()
    self._insert(typ-str, constraint)
  end,
  get(self, typ :: Type) -> Option<TypeConstraint>:
    typ-str = typ.tostring()
    if self.dict.has-key(typ-str):
      self.dict.get(typ-str)
    else:
      some(Bounds(t-bot, t-top))
    end
  end,
  meet(self, other :: TypeConstraints) -> TypeConstraints:
    keys = other.dict.keys()
    for fold(curr from self, key from keys):
      cases(Option<TypeConstraint>) other.dict.get(key):
        | some(t) =>
          curr._insert(key, t)
        | none =>
          # TODO(cody): Don't blow up
          raise("Cannot meet two TypeConstraint sets!")
      end
    end
  end,
  substitute(self, x :: Type % (is-t-var), r :: Type, binds :: Bindings) -> Type:
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
    else if is-invariant(x, r) and constraint.is-tight():
      constraint.min()
    else if is-rigid(x, r) and constraint.is-rigid(binds):
      constraint.min()
    else:
      raise("Substitution is undefined!")
    end
  end,
  tostring(self) -> String:
    "type-constraints("
      + dict-to-string(self.dict)
      + ")"
  end
end

empty-type-constraints = type-constraints(SD.immutable-string-dict())

fun generate-constraints(s :: Type, t :: Type, binds :: Bindings, to-remove :: Set<Type>, unknowns :: Set<Type>) -> TypeConstraints:
  s-free  = free-vars(s)
  s-str   = s.tostring()
  t-free  = free-vars(t)
  initial = empty-type-constraints
  if is-t-top(t):
    initial
  else if is-t-bot(s):
    initial
  else if unknowns.member(s) and is-empty(t-free.intersect(unknowns).to-list()):
    r = greatest-subtype(t, binds, to-remove)
    initial.insert(s, Bounds(t-bot, r))
  else if unknowns.member(t) and is-empty(s-free.intersect(unknowns).to-list()):
    r = least-supertype(s, binds, to-remove)
    initial.insert(t, Bounds(r, t-top))
  else if s == t:
    initial
  else if binds.has-key(s-str):
    generate-constraints(binds.get(s-str), t, binds, to-remove, unknowns)
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
                      new-ds = result.right.meet(curr.right)
                      pair(new-ks, new-ds)
                    end
            introduced      = ks-ds.left
            for-constraints = ks-ds.right
            new-binds       = for fold(curr from binds, y from introduced.keys()):
                                curr.set(y, introduced.get(y))
                              end
            new-to-remove   = for fold(curr from to-remove, f from s-forall + t-forall):
                                curr.add(t-var(f.id))
                              end
            arg-constraints = for fold2(curr from empty-type-constraints,
                                        s-arg from s-args, t-arg from t-args):
                                result = generate-constraints(t-arg, s-arg, new-binds, new-to-remove, unknowns)
                                curr.meet(result)
                              end
            ret-constraints = generate-constraints(s-ret, t-ret, new-binds, new-to-remove, unknowns)
            for-constraints.meet(arg-constraints.meet(ret-constraints))
          | else =>
            raise("What happens here?")
        end
      | else =>
        raise("What happens here?")
    end
  end
end

fun matching(s :: Type, t :: Type, binds :: Bindings, to-remove :: Set<Type>, unknowns :: Set<Type>) -> Pair<Type,TypeConstraints>:
  union = to-remove.union(unknowns)
  if is-t-top(s) and is-t-top(t):
    pair(t-top, empty-type-constraints)
  else if is-t-bot(s) and is-t-bot(t):
    pair(t-bot, empty-type-constraints)
  else if unknowns.member(s) and is-empty(free-vars(t).intersect(union).to-list()):
    pair(t, empty-type-constraints.insert(s, Equality(t)))
  else if unknowns.member(t) and is-empty(free-vars(s).intersect(union).to-list()):
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

