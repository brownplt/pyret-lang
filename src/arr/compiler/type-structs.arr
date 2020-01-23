provide *
provide-types *

import string-dict as SD
import file("ast.arr") as A
import lists as LISTS
import valueskeleton as VS
import equality as E

type Loc = A.Loc
type Name = A.Name
type StringDict = SD.StringDict

string-dict = SD.string-dict

all = LISTS.all
fold_n = LISTS.fold_n
fun sd-all(f, sd):
  for SD.fold-keys(acc from true, key from sd):
    acc and f(key)
  end
end

fun foldr2<X, Y, R>(f :: (R, X, Y -> R), base :: R, l1 :: List<X>, l2 :: List<Y>) -> R:
  cases(List<X>) l1:
    | empty =>
      base
    | link(a, ar) =>
      cases(List<Y>) l2:
        | empty => base
        | link(b, br) =>
          f(foldr2(f, base, ar, br), a, b)
      end
  end
end

fun interleave(lst, item):
  if is-empty(lst): lst
  else if is-empty(lst.rest): lst
  else: link(lst.first, link(item, interleave(lst.rest, item)))
  end
end

data ModuleType:
  | t-module(name :: String, provides :: Type, types :: StringDict<DataType>, aliases :: StringDict<Type>)
sharing:
  method _output(self):
    VS.vs-constr("t-module",
      [list:
        VS.vs-value(torepr(self.name)),
        VS.vs-value(self.provides),
        VS.vs-value(self.types),
        VS.vs-value(self.aliases)])
  end
end

type TypeMembers = StringDict<Type>

fun type-member-map(members :: TypeMembers, f :: (String, Type -> Type)) -> TypeMembers:
  members.fold-keys(lam(key, new-members):
    new-members.set(key, f(key, members.get-value(key)))
  end, SD.make-string-dict())
end

fun type-member-output(field-name :: String, typ :: Type):
  field-name + " :: " + typ.to-string()
end

fun variant-field-get-value(fields :: List<{String; Type}>, name :: String) -> {String; Type}:
  cases(Option<{String; Type}>) variant-field-get(fields, name):
    | some(result) => result
    | none => raise("Could not find field with name " + name + " in " + tostring(fields))
  end
end

fun variant-field-get(fields :: List<{String; Type}>, name :: String) -> Option<{String; Type}>:
  cases(List) fields:
    | empty => none
    | link({field-name; field-type}, rest) =>
      if field-name == name:
        some({field-name; field-type})
      else:
        variant-field-get(rest, name)
      end
  end
end

data TypeVariant:
  | t-variant(name        :: String,
              fields      :: List<{String; Type}>,
              with-fields :: TypeMembers,
              l           :: Loc)
  | t-singleton-variant(name        :: String,
                        with-fields :: TypeMembers,
                        l           :: Loc) with:
    fields: empty
sharing:
  method substitute(self, new-type :: Type, type-var :: Type):
    fun fields-substitute(fields):
      type-member-map(fields,
                      {(_, field-type): field-type.substitute(new-type, type-var)})
    end
    cases(TypeVariant) self:
      | t-variant(name, fields, with-fields, l) =>
        new-fields = fields.map(lam({field-name; typ}): {field-name; typ.substitute(new-type, type-var)} end)
        new-with-fields = fields-substitute(with-fields)
        t-variant(name, new-fields, new-with-fields, l)
      | t-singleton-variant(name, with-fields, l) =>
        new-with-fields = fields-substitute(with-fields)
        t-singleton-variant(name, new-with-fields, l)
    end
  end
end

data NameOrigin:
  | local
  | module-uri(uri :: String)
  | dependency(dep :: String) # Dependency in key form
end

fun name-comp(no :: NameOrigin):
  cases(NameOrigin) no:
    | local => ""
    | module-uri(uri) => uri
    | dependency(d) => d
  end
end

fun dep-error(no :: NameOrigin):
  raise("Should not get dependency in typechecker: " + torepr(no))
end

data DataType:
  | t-data(name :: String, params :: List<Type>, variants :: List<TypeVariant>, fields :: TypeMembers, l :: Loc)
sharing:
  method get-variant(self, variant-name :: String) -> Option<TypeVariant>:
    self.variants.find(lam(tv): tv.name == variant-name end)
  end,
  method get-variant-value(self, variant-name :: String) -> TypeVariant:
    cases(Option<TypeVariant>) self.get-variant(variant-name):
      | none => raise("data type " + self.name + " did not have variant: " + variant-name)
      | some(var-type) => var-type
    end
  end,
  method substitute(self, new-type :: Type, type-var :: Type):
    t-data(self.name,
           self.params,
           self.variants.map(_.substitute(new-type, type-var)),
           type-member-map(self.fields, {(_, field-type): field-type.substitute(new-type, type-var)}),
           self.l)
  end
end

data Type:
  | t-name(module-name :: NameOrigin, id :: Name, l :: Loc, inferred :: Boolean)
  | t-arrow(args :: List<Type>, ret :: Type, l :: Loc, inferred :: Boolean, existentials :: Set<Type % (is-t-existential)>)
  | t-app(onto :: Type, args :: List<Type>, l :: Loc, inferred :: Boolean, existentials :: Set<Type % (is-t-existential)>)
  | t-top(l :: Loc, inferred :: Boolean)
  | t-bot(l :: Loc, inferred :: Boolean)
  | t-record(fields :: TypeMembers, l :: Loc, inferred :: Boolean, existentials :: Set<Type % (is-t-existential)>)
  | t-tuple(elts :: List<Type>, l :: Loc, inferred :: Boolean, existentials :: Set<Type % (is-t-existential)>)
  | t-forall(introduces :: List<Type>, onto :: Type, l :: Loc, inferred :: Boolean, existentials :: Set<Type % (is-t-existential)>)
  | t-ref(typ :: Type, l :: Loc, inferred :: Boolean, existentials :: Set<Type % (is-t-existential)>)
  | t-data-refinement(data-type :: Type, variant-name :: String, l :: Loc, inferred :: Boolean, existentials :: Set<Type % (is-t-existential)>)
  | t-var(id :: Name, l :: Loc, inferred :: Boolean)
  | t-existential(id :: Name, l :: Loc, inferred :: Boolean)
sharing:
  method substitute(self, new-type :: Type, type-var :: Type):
    cases(Type) self:
      | t-name(_, _, _, _) => self
      | t-arrow(args, ret, l, inferred, _) =>
        new-args = args.map(_.substitute(new-type, type-var))
        new-ret = ret.substitute(new-type, type-var)
        new-existentials = existentials-from-list(new-args)
          .union(existentials-from-type(new-ret))
        t-arrow(new-args, new-ret, l, inferred, new-existentials)
      | t-app(onto, args, l, inferred, _) =>
        new-onto = onto.substitute(new-type, type-var)
        new-args = args.map(_.substitute(new-type, type-var))
        new-existentials = existentials-from-type(new-onto)
          .union(existentials-from-list(new-args))
        t-app(new-onto, new-args, l, inferred, new-existentials)
      | t-top(_, _) => self
      | t-bot(_, _) => self
      | t-record(fields, l, inferred, _) =>
        new-fields = type-member-map(fields, {(_, field-type): field-type.substitute(new-type, type-var)})
        new-existentials = existentials-from-string-dict(new-fields)
        t-record(new-fields, l, inferred, new-existentials)
      | t-tuple(elts, l, inferred, _) =>
        new-elts = elts.map(_.substitute(new-type, type-var))
        new-existentials = existentials-from-list(new-elts)
        t-tuple(new-elts, l, inferred, new-existentials)
      | t-forall(introduces, onto, l, inferred, _) =>
        # doesn't need to be capture avoiding thanks to resolve-names
        new-onto = onto.substitute(new-type, type-var)
        new-existentials = existentials-from-list(introduces)
          .union(existentials-from-type(new-onto))
        t-forall(introduces, new-onto, l, inferred, new-existentials)
      | t-ref(typ, l, inferred, _) =>
        new-typ = typ.substitute(new-type, type-var)
        new-existentials = existentials-from-type(new-typ)
        t-ref(new-typ, l, inferred, new-existentials)
      | t-data-refinement(data-type, variant-name, l, inferred, _) =>
        new-data-type = data-type.substitute(new-type, type-var)
        new-existentials = existentials-from-type(new-data-type)
        t-data-refinement(new-data-type,
                          variant-name,
                          l,
                          inferred,
                          new-existentials)
      | t-var(id, l, _) =>
        cases(Type) type-var:
          | t-var(var-id, _, _) =>
            if id == var-id:
              new-type.set-loc(l)
            else:
              self
            end
          | else => self
        end
      | t-existential(id, l, inferred) =>
        cases(Type) type-var:
          | t-existential(var-id, _, _) =>
            if id == var-id:
              # inferred existentials keep their locations
              # this is along the lines of inferred argument types etc
              # uninferred existentials are used to equate different pieces of code
              # they should not keep their location
              if inferred:
                new-type.set-loc(l)
              else:
                new-type
              end
            else:
              self
            end
          | else => self
        end
    end
  end,
  method free-variables(self) -> Set<Type>:
    cases(Type) self:
      | t-name(module-name, id, _, _) =>
        empty-list-set
      | t-arrow(args, ret, _, _, _) =>
        args.foldl(lam(arg, free): free.union(arg.free-variables()) end, ret.free-variables())
      | t-app(onto, args, _, _, _) =>
        args.foldl(lam(arg, free): free.union(arg.free-variables()) end, onto.free-variables())
      | t-top(_, _) =>
        empty-list-set
      | t-bot(_, _) =>
        empty-list-set
      | t-record(fields, _, _, _) =>
        fields.fold-keys(lam(key, free): free.union(fields.get-value(key).free-variables()) end, empty-list-set)
      | t-tuple(elts, _, _, _) =>
        elts.foldl(lam(elt, free): free.union(elt.free-variables()) end, empty-list-set)
      | t-forall(_, onto, _, _, _) =>
        onto.free-variables()
      | t-ref(typ, _, _, _) =>
        typ.free-variables()
      | t-data-refinement(data-type, _, _, _, _) =>
        data-type.free-variables()
      | t-var(a-id, _, _) =>
        empty-list-set
      | t-existential(a-id, _, _) =>
        [list-set: self]
    end
  end,
  method has-variable-free(self, var-type :: Type) -> Boolean:
    cases(Type) self:
      | t-name(module-name, id, _, _) =>
        true
      | t-arrow(args, ret, _, _) =>
        all(_.has-variable-free(var-type), args) and
        ret.has-variable-free(var-type)
      | t-app(onto, args, _, _, _) =>
        onto.has-variable-free(var-type) and
        all(_.has-variable-free(var-type), args)
      | t-top(_, _) =>
        true
      | t-bot(_, _) =>
        true
      | t-record(fields, _, _, _) =>
        sd-all(lam(key): fields.get-value(key).has-variable-free(var-type) end, fields)
      | t-tuple(elts, _, _, _) =>
        all(_.has-variable-free(var-type), elts)
      | t-forall(_, onto, _, _, _) =>
        # TODO(MATT): can we really ignore the introduces?
        onto.has-variable-free(var-type)
      | t-ref(typ, _, _, _) =>
        typ.has-variable-free(var-type)
      | t-data-refinement(data-type, _, _, _, _) =>
        data-type.has-variable-free(var-type)
      | t-var(a-id, _, _) =>
        cases(Type) var-type:
          | t-var(b-id, _, _) => if a-id == b-id: false else: true end
          | else => true
        end
      | t-existential(a-id, _, _) =>
        cases(Type) var-type:
          | t-existential(b-id, _, _) => if a-id == b-id: false else: true end
          | else => true
        end
    end
  end,
  method key(self) -> String:
    cases(Type) self:
      | t-name(module-name, id, _, _) =>
        cases(NameOrigin) module-name:
          | local => id.key()
          | module-uri(m) => m + "." + id.key()
          | dependency(_) => dep-error(module-name)
        end
      | t-arrow(args, ret, _, _, _) =>
        "("
          + args.map(_.key()).join-str(", ")
          + " -> " + ret.key() + ")"
      | t-app(onto, args, _, _, _) =>
        onto.key() + "<" + args.map(_.key()).join-str(", ") + ">"
      | t-top(_, _) =>
        "Any"
      | t-bot(_, _) =>
        "Bot"
      | t-record(fields, _, _, _) =>
        "{" + fields.map-keys(lam(key): key + " :: " + fields.get-value(key).key() end).join-str(", ") + "}"
      | t-tuple(elts, _, _, _) =>
        "{"
          + for map(elt from elts):
              elt.key()
            end.join-str("; ")
          + "}"
      | t-forall(introduces, onto, l, _, _) =>
        "<" + introduces.map(_.key()).join-str(", ") + ">"
          + onto.key()
      | t-ref(typ, _, _, _) =>
        "ref " + typ.key()
      | t-data-refinement(data-type, variant-name, l, _, _) =>
        "("
          + data-type.key()
          + " %is-" + variant-name
          + ")"
      | t-var(id, _, _) =>
        id.key()
      | t-existential(id, _, _) =>
        id.key()
    end
  end,
  method set-inferred(self, inferred :: Boolean):
    cases(Type) self:
      | t-name(module-name, id, loc, _) =>
        t-name(module-name, id, loc, inferred)
      | t-arrow(args, ret, loc, _, existentials) =>
        t-arrow(args, ret, loc, inferred, existentials)
      | t-app(onto, args, loc, _, existentials) =>
        t-app(onto, args, loc, inferred, existentials)
      | t-top(loc, _) =>
        t-top(loc, inferred)
      | t-bot(loc, _) =>
        t-bot(loc, inferred)
      | t-record(fields, loc, _, existentials) =>
        t-record(fields, loc, inferred, existentials)
      | t-tuple(elts, loc, _, existentials) =>
        t-tuple(elts, loc, inferred, existentials)
      | t-forall(introduces, onto, loc, _, existentials) =>
        t-forall(introduces, onto, loc, inferred, existentials)
      | t-ref(typ, loc, _, existentials) =>
        t-ref(typ, loc, inferred, existentials)
      | t-data-refinement(data-type, variant-name, loc, _, existentials) =>
        t-data-refinement(data-type, variant-name, loc, inferred, existentials)
      | t-var(id, loc, _) =>
        t-var(id, loc, inferred)
      | t-existential(id, loc, _) =>
        t-existential(id, loc, inferred)
    end
  end,
  method set-loc(self, loc :: Loc):
    sl = _.set-loc(loc)
    cases(Type) self:
      | t-name(module-name, id, _, inferred) =>
        t-name(module-name, id, loc, inferred)
      | t-arrow(args, ret, _, inferred, existentials) =>
        t-arrow(args.map(sl), sl(ret), loc, inferred, existentials)
      | t-app(onto, args, _, inferred, existentials) =>
        t-app(sl(onto), args.map(sl), loc, inferred, existentials)
      | t-top(_, inferred) =>
        t-top(loc, inferred)
      | t-bot(_, inferred) =>
        t-bot(loc, inferred)
      | t-record(fields, _, inferred, existentials) =>
        t-record(type-member-map(fields, {(_, field-type): sl(field-type)}), loc, inferred, existentials)
      | t-tuple(elts, _, inferred, existentials) =>
        t-tuple(elts.map(sl), loc, inferred, existentials)
      | t-forall(introduces, onto, _, inferred, existentials) =>
        t-forall(introduces.map(sl), sl(onto), loc, inferred, existentials)
      | t-ref(typ, _, inferred, existentials) =>
        t-ref(sl(typ), loc, inferred, existentials)
      | t-data-refinement(data-type, variant-name, _, inferred, existentials) =>
        t-data-refinement(sl(data-type), variant-name, loc, inferred, existentials)
      | t-var(id, _, inferred) =>
        t-var(id, loc, inferred)
      | t-existential(id, _, inferred) =>
        t-existential(id, loc, inferred)
    end
  end,
  method _equals(self, other :: Type, _) -> E.EqualityResult:
    cases(Type) self:
      | t-name(a-module-name, a-id, _, _) =>
        cases(Type) other:
          | t-name(b-module-name, b-id, _, _) =>
            ask:
              | not(a-module-name == b-module-name) then: E.NotEqual("Module names", self, other)
              | not(a-id == b-id) then: E.NotEqual("IDs", self, other)
              | otherwise: E.Equal
            end
          | else => E.NotEqual("Different types", self, other)
        end
      | t-arrow(a-args, a-ret, _, _, _) =>
        cases(Type) other:
          | t-arrow(b-args, b-ret, _, _, _) =>
            ask:
              | not(a-args == b-args) then: E.NotEqual("Args", self, other)
              | not(a-ret == b-ret) then: E.NotEqual("Return types", self, other)
              | otherwise: E.Equal
            end
          | else => E.NotEqual("Different types", self, other)
        end
      | t-app(a-onto, a-args, _, _, _) =>
        cases(Type) other:
          | t-app(b-onto, b-args, _, _, _) =>
            ask:
              | not(a-onto == b-onto) then: E.NotEqual("Ontos", self, other)
              | not(a-args == b-args) then: E.NotEqual("Args", self, other)
              | otherwise: E.Equal
            end
          | else => E.NotEqual("Different types", self, other)
        end
      | t-top(_, _) =>
        cases(Type) other:
          | t-top(_, _) => E.Equal
          | else => E.NotEqual("Different types", self, other)
        end
      | t-bot(_, _) =>
        cases(Type) other:
          | t-bot(_, _) => E.Equal
          | else => E.NotEqual("Different types", self, other)
        end
      | t-record(a-fields, _, _, _) =>
        cases(Type) other:
          | t-record(b-fields, _, _, _) =>
            ask:
              | not(a-fields == b-fields) then: E.NotEqual("Fields", self, other)
              | otherwise: E.Equal
            end
          | else => E.NotEqual("Different types", self, other)
        end
      | t-tuple(a-elts, _, _, _) =>
        cases(Type) other:
          | t-tuple(b-elts, _, _, _) =>
            ask:
              | not(a-elts == b-elts) then: E.NotEqual("Elements", self, other)
              | otherwise: E.Equal
            end
          | else => E.NotEqual("Different types", self, other)
        end
      | t-forall(a-introduces, a-onto, _, _, _) =>
        cases(Type) other:
          | t-forall(b-introduces, b-onto, _, _, _) =>
            if (a-introduces.length() == b-introduces.length()):
              shadow b-onto = foldr2(lam(shadow b-onto, a-var, b-var):
                b-onto.substitute(a-var, b-var)
              end, b-onto, a-introduces, b-introduces)
              if a-onto == b-onto:
                E.Equal
              else:
                E.NotEqual("Ontos", self, other)
              end
            else:
              E.NotEqual("Introduces", self, other)
            end
          | else => E.NotEqual("Different types", self, other)
        end
      | t-ref(a-typ, _, _, _) =>
        cases(Type) other:
          | t-ref(b-typ, _, _, _) =>
            ask:
              | not(a-typ == b-typ) then: E.NotEqual("Ref types", self, other)
              | otherwise: E.Equal
            end
          | else => E.NotEqual("Different types", self, other)
        end
      | t-data-refinement(a-data-type, a-variant-name, _, _, _) =>
        cases(Type) other:
          | t-data-refinement(b-data-type, b-variant-name, _, _, _) =>
            ask:
              | not(a-data-type == b-data-type) then: E.NotEqual("Data types", self, other)
              | not(a-variant-name == b-variant-name) then: E.NotEqual("Variant names", self, other)
              | otherwise: E.Equal
            end
          | else => E.NotEqual("Different types", self, other)
        end
      | t-var(a-id, _, _) =>
        cases(Type) other:
          | t-var(b-id, _, _) =>
            ask:
              | not(a-id == b-id) then: E.NotEqual("IDs", self, other)
              | otherwise: E.Equal
            end
          | else => E.NotEqual("Different types", self, other)
        end
      | t-existential(a-id, _, _) =>
        cases(Type) other:
          | t-existential(b-id, _, _) =>
            ask:
              | not(a-id == b-id) then: E.NotEqual("IDs", self, other)
              | otherwise: E.Equal
            end
          | else => E.NotEqual("Different types", self, other)
        end
    end
  end,
  method _lessthan(self, other):
    self.key() < other.key()
  end,
  method to-string(self):
    var current-letter = "A"
    fun helper(typ, free-vars-mapping, tyvar-mapping) -> String:
      h = helper(_, free-vars-mapping, tyvar-mapping)
      cases(Type) typ:
        | t-name(module-name, id, _, _) =>
          id.toname()
        | t-arrow(args, ret, _, _, _) =>
          "("
            + args.map(h).join-str(", ")
            + " -> " + h(ret)
            + ")"
        | t-app(onto, args, _, _, _) =>
          h(onto) + "<"
            + args.map(h).join-str(", ")
            + ">"
        | t-top(_, _) =>
          "Any"
        | t-bot(_, _) =>
          "Bot"
        | t-record(fields, _, _, _) =>
          "{"
            + fields.map-keys(lam(key): type-member-output(key, fields.get-value(key)) end).join-str(", ")
            + "}"
        | t-tuple(elts, _, _, _) =>
          "{"
            + elts.map(h).join-str("; ")
            + "}"
        | t-forall(introduces, onto, _, _, _) =>
          "forall "
            + introduces.map(h).join-str(", ")
            + " . " + h(onto)
        | t-ref(ref-typ, _, _, _) =>
          "ref " + h(ref-typ)
        | t-data-refinement(data-type, variant-name, _, _, _) =>
          "("
            + h(data-type)
            + " % is-" + variant-name
            + ")"
        | t-var(id, _, _) =>
          cases(Name) id:
            | s-atom(base, _) =>
              if base == "%tyvar":
                cases(Option<String>) tyvar-mapping.get-now(typ.key()) block:
                  | some(name) => name
                  | none =>
                    letter = current-letter
                    tyvar-mapping.set-now(typ.key(), current-letter)
                    current-letter := string-from-code-point(string-to-code-point(letter) + 1)
                    letter
                end
              else:
                id.toname()
              end
            | else =>
              id.toname()
          end
        | t-existential(id, _, _) =>
          "?-" + free-vars-mapping.get-value(typ.key())
          #id.key()
      end
    end
    free-vars-list = self.free-variables().to-list()
    free-vars-mapping = fold_n(lam(position, mapping, free-var):
      mapping.set(free-var.key(), tostring(position))
    end, 1, SD.make-string-dict(), free-vars-list)
    helper(self, free-vars-mapping, SD.make-mutable-string-dict())
  end,
  #|
  method _output(self):
    VS.vs-str(self.to-string())
  end
  |#
end

# Returns the `Type`'s `.existentials` field if it has one. Otherwise, returns the empty set.
fun existentials-from-type(typ :: Type) -> Set<Type % (is-t-existential)>:
  cases(Type) typ:
    | t-name(_, _, _, _) => empty-tree-set
    | t-arrow(_, _, _, _, existentials) => existentials
    | t-app(_, _, _, _, existentials) => existentials
    | t-top(_, _) => empty-tree-set
    | t-bot(_, _) => empty-tree-set
    | t-record(_, _, _, existentials) => existentials
    | t-tuple(_, _, _, existentials) => existentials
    | t-forall(_, _, _, _, existentials) => existentials
    | t-ref(_, _, _, existentials) => existentials
    | t-data-refinement(_, _, _, _, existentials) => existentials
    | t-var(_, _, _) => empty-tree-set
    | t-existential(_, _, _) => empty-tree-set
  end
end

fun existentials-from-list(lst :: List<Type>) -> Set<Type % (is-t-existential)>:
  lst.foldl(lam(x :: Type, acc :: Set<Type % (is-t-existential)>):
      acc.union(existentials-from-type(x))
    end,
    empty-tree-set)
end

fun existentials-from-string-dict(sd :: TypeMembers) -> Set<Type % (is-t-existential)>:
  sd.fold-keys(lam(key :: String, acc :: Set<Type % (is-t-existential)>):
      cases(Option) sd.get(key):
        | some(typ) => acc.union(existentials-from-type(typ))
        | none => raise("No value for key")
      end
    end,
    empty-tree-set)
end

# Constructors for `Type`s which have an `.existentials` field [so we don't have to
# manually specify it when constructing new `Type`s].
shadow t-arrow = lam(
    args :: List<Type>,
    ret :: Type,
    l :: Loc,
    inferred :: Boolean) -> Type % (is-t-arrow):
  existentials = existentials-from-list(args).union(existentials-from-type(ret))
  t-arrow(args, ret, l, inferred, existentials)
end
shadow t-app = lam(
    onto :: Type,
    args :: List<Type>,
    l :: Loc,
    inferred :: Boolean) -> Type % (is-t-app):
  existentials = existentials-from-type(onto).union(existentials-from-list(args))
  t-app(onto, args, l, inferred, existentials)
end
shadow t-record = lam(
    fields :: TypeMembers,
    l :: Loc,
    inferred :: Boolean) -> Type % (is-t-record):
  existentials = existentials-from-string-dict(fields)
  t-record(fields, l, inferred, existentials)
end
shadow t-tuple = lam(
    elts :: List<Type>,
    l :: Loc,
    inferred :: Boolean) -> Type % (is-t-tuple):
  existentials = existentials-from-list(elts)
  t-tuple(elts, l, inferred, existentials)
end
shadow t-forall = lam(
    introduces :: List<Type>,
    onto :: Type,
    l :: Loc,
    inferred :: Boolean) -> Type % (is-t-forall):
  existentials = existentials-from-list(introduces).union(existentials-from-type(onto))
  t-forall(introduces, onto, l, inferred, existentials)
end
shadow t-ref = lam(
    typ :: Type,
    l :: Loc,
    inferred :: Boolean) -> Type % (is-t-ref):
  existentials = existentials-from-type(typ)
  t-ref(typ, l, inferred, existentials)
end
shadow t-data-refinement = lam(
    data-type :: Type,
    variant-name :: String,
    l :: Loc,
    inferred :: Boolean) -> Type % (is-t-data-refinement):
  existentials = existentials-from-type(data-type)
  t-data-refinement(data-type, variant-name, l, inferred, existentials)
end

check:
  a-name = t-name(local, A.s-name(A.dummy-loc, "a"), A.dummy-loc, false, [tree-set: ])
  b-name = t-name(local, A.s-name(A.dummy-loc, "b"), A.dummy-loc, false, [tree-set: ])
  a-name.to-string() is "a"
  t-arrow([list: a-name, b-name], a-name, A.dummy-loc, false, [tree-set: ]).to-string() is "(a, b -> a)"
  t-top(A.dummy-loc, false).to-string() is "Any"
  t-bot(A.dummy-loc, false).to-string() is "Bot"
  t-record([string-dict: "a", a-name, "b", a-name], A.dummy-loc, false, [tree-set: ]).to-string() is "{a :: a, b :: a}"
  t-tuple([list: a-name, b-name], A.dummy-loc, false, [tree-set: ]).to-string() is "{a; b}"
  t-forall([list: a-name, b-name], b-name, A.dummy-loc, false, [tree-set: ]).to-string() is "forall a, b . b"
  t-ref(a-name, A.dummy-loc, false, [tree-set: ]).to-string() is "ref a"
  t-data-refinement(a-name, "a", A.dummy-loc, false, [tree-set: ]).to-string() is "(a % is-a)"
  t-var(A.s-atom("%tyvar", 0), A.dummy-loc, false).to-string() is "A"
  t-var(A.s-name(A.dummy-loc, "a"), A.dummy-loc, false).to-string() is "a"
  t-existential(A.s-name(A.dummy-loc, "a"), A.dummy-loc, false).to-string() is "?-1"
end

fun new-existential(l :: Loc, inferred :: Boolean):
  t-existential(A.global-names.make-atom("%exists"), l, inferred)
end

fun new-type-var(l :: Loc):
  t-var(A.global-names.make-atom("%tyvar"), l, false)
end

# TODO(MATT): which of these should be kept
builtin-uri = module-uri("builtin://global")

t-array-name = t-name(builtin-uri, A.s-type-global("RawArray"), A.dummy-loc, false)

t-number  = lam(l): t-name(builtin-uri, A.s-type-global("Number"), l, false) end
t-string  = lam(l): t-name(builtin-uri, A.s-type-global("String"), l, false) end
t-boolean = lam(l): t-name(builtin-uri, A.s-type-global("Boolean"), l, false) end
t-nothing = lam(l): t-name(builtin-uri, A.s-type-global("Nothing"), l, false) end
t-srcloc  = lam(l): t-name(builtin-uri, A.s-type-global("Loc"), l, false) end
t-array   = lam(v, l): t-app(t-array-name.set-loc(l), [list: v], l, false) end
t-option  = lam(v, l): t-app(t-name(module-uri("builtin://option"), A.s-type-global("Option"), l, false), [list: v], l, false) end
t-table = lam(l): t-name(builtin-uri, A.s-type-global("Table"), l, false) end
