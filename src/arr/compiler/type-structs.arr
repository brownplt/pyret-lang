provide *
provide-types *

import string-dict as SD
import ast as A
import lists as LISTS
import valueskeleton as VS
import equality as E

type Loc = A.Loc
type Name = A.Name
type StringDict = SD.StringDict

string-dict = SD.string-dict

all = LISTS.all
fold_n = LISTS.fold_n

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
  keys = members.keys-list()
  keys.foldr(lam(key, new-members):
    new-members.set(key, f(key, members.get-value(key)))
  end, SD.make-string-dict())
end

fun type-member-output(field-name :: String, typ :: Type):
  VS.vs-seq([list: VS.vs-str(field-name), VS.vs-str(" :: "), VS.vs-value(typ)])
end

data TypeVariant:
  | t-variant(name        :: String,
              fields      :: TypeMembers,
              with-fields :: TypeMembers,
              l           :: Loc)
  | t-singleton-variant(name        :: String,
                        with-fields :: TypeMembers,
                        l           :: Loc) with:
    fields: [string-dict: ]
sharing:
  method substitute(self, new-type :: Type, type-var :: Type):
    fun fields-substitute(fields):
      type-member-map(fields,
                      {(_, field-type): field-type.substitute(new-type, type-var)})
    end
    cases(TypeVariant) self:
      | t-variant(name, fields, with-fields, l) =>
        new-fields = fields-substitute(fields)
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
  | t-arrow(args :: List<Type>, ret :: Type, l :: Loc, inferred :: Boolean)
  | t-app(onto :: Type, args :: List<Type>, l :: Loc, inferred :: Boolean)
  | t-top(l :: Loc, inferred :: Boolean)
  | t-bot(l :: Loc, inferred :: Boolean)
  | t-record(fields :: TypeMembers, l :: Loc, inferred :: Boolean)
  | t-tuple(elts :: List<Type>, l :: Loc, inferred :: Boolean)
  | t-forall(introduces :: List<Type>, onto :: Type, l :: Loc, inferred :: Boolean)
  | t-ref(typ :: Type, l :: Loc, inferred :: Boolean)
  | t-data-refinement(data-type :: Type, variant-name :: String, l :: Loc, inferred :: Boolean)
  | t-var(id :: Name, l :: Loc, inferred :: Boolean)
  | t-existential(id :: Name, l :: Loc, inferred :: Boolean)
sharing:
  method substitute(self, new-type :: Type, type-var :: Type):
    cases(Type) self:
      | t-name(_, _, _, _) => self
      | t-arrow(args, ret, l, inferred) =>
        new-args = args.map(_.substitute(new-type, type-var))
        new-ret = ret.substitute(new-type, type-var)
        t-arrow(new-args, new-ret, l, inferred)
      | t-app(onto, args, l, inferred) =>
        new-onto = onto.substitute(new-type, type-var)
        new-args = args.map(_.substitute(new-type, type-var))
        t-app(new-onto, new-args, l, inferred)
      | t-top(_, _) => self
      | t-bot(_, _) => self
      | t-record(fields, l, inferred) =>
        new-fields = type-member-map(fields, {(_, field-type): field-type.substitute(new-type, type-var)})
        t-record(new-fields, l, inferred)
      | t-tuple(elts, l, inferred) =>
        t-tuple(elts.map(_.substitute(new-type, type-var)), l, inferred)
      | t-forall(introduces, onto, l, inferred) =>
        # doesn't need to be capture avoiding thanks to resolve-names
        new-onto = onto.substitute(new-type, type-var)
        t-forall(introduces, new-onto, l, inferred)
      | t-ref(typ, l, inferred) =>
        t-ref(typ.substitute(new-type, type-var, l), inferred)
      | t-data-refinement(data-type, variant-name, l, inferred) =>
        t-data-refinement(data-type.substitute(new-type, type-var),
                          variant-name,
                          l,
                          inferred)
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
      | t-arrow(args, ret, _, _) =>
        args.foldl(lam(arg, free): free.union(arg.free-variables()) end, ret.free-variables())
      | t-app(onto, args, _, _) =>
        args.foldl(lam(arg, free): free.union(arg.free-variables()) end, onto.free-variables())
      | t-top(_, _) =>
        empty-list-set
      | t-bot(_, _) =>
        empty-list-set
      | t-record(fields, _, _) =>
        fields.keys-list().foldl(lam(key, free): free.union(fields.get-value(key).free-variables()) end, empty-list-set)
      | t-tuple(elts, _, _) =>
        elts.foldl(lam(elt, free): free.union(elt.free-variables()) end, empty-list-set)
      | t-forall(_, onto, _, _) =>
        onto.free-variables()
      | t-ref(typ, _, _) =>
        typ.free-variables()
      | t-data-refinement(data-type, _, _, _) =>
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
      | t-app(onto, args, _, _) =>
        onto.has-variable-free(var-type) and
        all(_.has-variable-free(var-type), args)
      | t-top(_, _) =>
        true
      | t-bot(_, _) =>
        true
      | t-record(fields, _, _) =>
        keys = fields.keys-list()
        all(lam(key): fields.get-value(key).has-variable-free(var-type) end, keys)
      | t-tuple(elts, _, _) =>
        all(_.has-variable-free(var-type), elts)
      | t-forall(_, onto, _, _) =>
        # TODO(MATT): can we really ignore the introduces?
        onto.has-variable-free(var-type)
      | t-ref(typ, _, _) =>
        typ.has-variable-free(var-type)
      | t-data-refinement(data-type, _, _, _) =>
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
      | t-arrow(args, ret, _, _) =>
        "("
          + args.map(_.key()).join-str(", ")
          + " -> " + ret.key() + ")"
      | t-app(onto, args, _, _) =>
        onto.key() + "<" + args.map(_.key()).join-str(", ") + ">"
      | t-top(_, _) =>
        "Any"
      | t-bot(_, _) =>
        "Bot"
      | t-record(fields, _, _) =>
        "{" + fields.keys-list().map(lam(key): key + " :: " + fields.get-value(key).key() end).join-str(", ") + "}"
      | t-tuple(elts, _, _) =>
        "{"
          + for map(elt from elts):
              elt.key()
            end.join-str("; ")
          + "}"
      | t-forall(introduces, onto, l, _) =>
        "<" + introduces.map(_.key()).join-str(", ") + ">"
          + onto.key()
      | t-ref(typ, _, _) =>
        "ref " + typ.key()
      | t-data-refinement(data-type, variant-name, l, _) =>
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
      | t-arrow(args, ret, loc, _) =>
        t-arrow(args, ret, loc, inferred)
      | t-app(onto, args, loc, _) =>
        t-app(onto, args, loc, inferred)
      | t-top(loc, _) =>
        t-top(loc, inferred)
      | t-bot(loc, _) =>
        t-bot(loc, inferred)
      | t-record(fields, loc, _) =>
        t-record(fields, loc, inferred)
      | t-tuple(elts, loc, _) =>
        t-tuple(elts, loc, inferred)
      | t-forall(introduces, onto, loc, _) =>
        t-forall(introduces, onto, loc, inferred)
      | t-ref(typ, loc, _) =>
        t-ref(typ, loc, inferred)
      | t-data-refinement(data-type, variant-name, loc, _) =>
        t-data-refinement(data-type, variant-name, loc, inferred)
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
      | t-arrow(args, ret, _, inferred) =>
        t-arrow(args.map(sl), sl(ret), loc, inferred)
      | t-app(onto, args, _, inferred) =>
        t-app(sl(onto), args.map(sl), loc, inferred)
      | t-top(_, inferred) =>
        t-top(loc, inferred)
      | t-bot(_, inferred) =>
        t-bot(loc, inferred)
      | t-record(fields, _, inferred) =>
        t-record(type-member-map(fields, {(_, field-type): sl(field-type)}), loc, inferred)
      | t-tuple(elts, _, inferred) =>
        t-tuple(elts.map(sl), loc, inferred)
      | t-forall(introduces, onto, _, inferred) =>
        t-forall(introduces.map(sl), sl(onto), loc, inferred)
      | t-ref(typ, _, inferred) =>
        t-ref(sl(typ), loc, inferred)
      | t-data-refinement(data-type, variant-name, _, inferred) =>
        t-data-refinement(sl(data-type), variant-name, loc, inferred)
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
      | t-arrow(a-args, a-ret, _, _) =>
        cases(Type) other:
          | t-arrow(b-args, b-ret, _, _) =>
            ask:
              | not(a-args == b-args) then: E.NotEqual("Args", self, other)
              | not(a-ret == b-ret) then: E.NotEqual("Return types", self, other)
              | otherwise: E.Equal
            end
          | else => E.NotEqual("Different types", self, other)
        end
      | t-app(a-onto, a-args, _, _) =>
        cases(Type) other:
          | t-app(b-onto, b-args, _, _) =>
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
      | t-record(a-fields, _, _) =>
        cases(Type) other:
          | t-record(b-fields, _, _) =>
            ask:
              | not(a-fields == b-fields) then: E.NotEqual("Fields", self, other)
              | otherwise: E.Equal
            end
          | else => E.NotEqual("Different types", self, other)
        end
      | t-tuple(a-elts, _, _) =>
        cases(Type) other:
          | t-tuple(b-elts, _, _) =>
            ask:
              | not(a-elts == b-elts) then: E.NotEqual("Elements", self, other)
              | otherwise: E.Equal
            end
          | else => E.NotEqual("Different types", self, other)
        end
      | t-forall(a-introduces, a-onto, _, _) =>
        cases(Type) other:
          | t-forall(b-introduces, b-onto, _, _) =>
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
      | t-ref(a-typ, _, _) =>
        cases(Type) other:
          | t-ref(b-typ, _, _) =>
            ask:
              | not(a-typ == b-typ) then: E.NotEqual("Ref types", self, other)
              | otherwise: E.Equal
            end
          | else => E.NotEqual("Different types", self, other)
        end
      | t-data-refinement(a-data-type, a-variant-name, _, _) =>
        cases(Type) other:
          | t-data-refinement(b-data-type, b-variant-name, _, _) =>
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
  method _output(self):
    var current-letter = "A"
    fun helper(typ, free-vars-mapping, tyvar-mapping):
      h = helper(_, free-vars-mapping, tyvar-mapping)
      cases(Type) typ:
        | t-name(module-name, id, _, _) =>
          VS.vs-str(id.toname())
        | t-arrow(args, ret, _, _) =>
          VS.vs-seq([list: VS.vs-str("(")]
            + interleave(args.map(h), VS.vs-str(", "))
            + [list: VS.vs-str(" -> "), h(ret), VS.vs-str(")")])
        | t-app(onto, args, _, _) =>
          VS.vs-seq([list: h(onto), VS.vs-str("<")]
            + interleave(args.map(h), VS.vs-str(", "))
            + [list: VS.vs-str(">")])
        | t-top(_, _) =>
          VS.vs-str("Any")
        | t-bot(_, _) =>
          VS.vs-str("Bot")
        | t-record(fields, _, _) =>
          VS.vs-seq([list: VS.vs-str("{")]
            + interleave(fields.keys-list().map(lam(key): type-member-output(key, fields.get-value(key)) end), VS.vs-str(", "))
            + [list: VS.vs-str("}")])
        | t-tuple(elts, _, _) =>
          VS.vs-seq([list: VS.vs-str("{")]
            + interleave(elts.map(h), VS.vs-str("; "))
            + [list: VS.vs-str("}")])
        | t-forall(introduces, onto, _, _) =>
          VS.vs-seq([list: VS.vs-str("forall ")]
            + interleave(introduces.map(h), VS.vs-str(", "))
            + [list: VS.vs-str(" . "), h(onto)])
        | t-ref(ref-typ, _, _) =>
          VS.vs-seq([list: VS.vs-str("ref "), h(ref-typ)])
        | t-data-refinement(data-type, variant-name, _, _) =>
          VS.vs-seq([list: VS.vs-str("("),
                          h(data-type),
                          VS.vs-str(" % is-" + variant-name + ")")])
        | t-var(id, _, _) =>
          cases(Name) id:
            | s-atom(base, _) =>
              if base == "%tyvar":
                cases(Option<String>) tyvar-mapping.get-now(typ.key()) block:
                  | some(name) => VS.vs-str(name)
                  | none =>
                    letter = current-letter
                    tyvar-mapping.set-now(typ.key(), current-letter)
                    current-letter := string-from-code-point(string-to-code-point(letter) + 1)
                    VS.vs-str(letter)
                end
              else:
                VS.vs-str(id.toname())
              end
            | else =>
              VS.vs-str(id.toname())
          end
        | t-existential(id, _, _) =>
          VS.vs-str("?-" + free-vars-mapping.get-value(typ.key()))
      end
    end
    free-vars-list = self.free-variables().to-list()
    free-vars-mapping = fold_n(lam(position, mapping, free-var):
      mapping.set(free-var.key(), tostring(position))
    end, 1, SD.make-string-dict(), free-vars-list)
    helper(self, free-vars-mapping, SD.make-mutable-string-dict())
  end
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
