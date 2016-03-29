provide *
provide-types *

import ast as A
import string-dict as SD
import lists as LISTS
import "compiler/list-aux.arr" as LA
import equality as E
import valueskeleton as VS

type Name = A.Name

all = LISTS.all

all2-strict  = LA.all2-strict

fun interleave(lst, item):
  if is-empty(lst): lst
  else if is-empty(lst.rest): lst
  else: link(lst.first, link(item, interleave(lst.rest, item)))
  end
end

fun compare-lists(xs, ys):
  all2-strict(lam(x, y): x == y end, xs, ys)
end

# BEGIN MAYBE KEEP

fun type-members-lookup(type-members :: List<TypeMember>, field-name :: String) -> Option<TypeMember>:
  fun same-field(tm):
    tm.field-name == field-name
  end
  type-members.find(same-field)
end

data Pair<L,R>:
  | pair(left :: L, right :: R)
sharing:
  on-left(self, f :: (L -> L)) -> Pair<L,R>:
    pair(f(self.left), self.right)
  end,
  on-right(self, f :: (R -> R)) -> Pair<L,R>:
    pair(self.left, f(self.right))
  end
end

# END MAYBE KEEP

data ModuleType:
  | t-module(name :: String, provides :: Type, types :: SD.StringDict<Type>, aliases :: SD.StringDict<Type>)
sharing:
  _output(self):
    VS.vs-constr("t-module",
      [list:
        VS.vs-value(torepr(self.name)),
        VS.vs-value(self.provides),
        VS.vs-value(self.types),
        VS.vs-value(self.aliases)])
  end
end

data TypeMember:
  | t-member(field-name :: String, typ :: Type, l :: A.Loc)
sharing:
  _output(self):
    VS.vs-seq([list: VS.vs-str(self.field-name), VS.vs-str(" : "), VS.vs-value(self.typ)])
  end,
  key(self) -> String:
    self.field-name + " : " + self.typ.key()
  end,
  substitute(self, new-type :: Type, old-type :: Type):
    t-member(self.field-name, self.typ.substitute(new-type, old-type), self.l)
  end,
  set-loc(self, loc :: A.Loc):
    t-member(self.field-name, self.typ, loc)
  end,
  free-variable(self, var-type :: Type) -> Boolean:
    self.typ.free-variable(var-type)
  end,
  _equals(self, other :: TypeMember, _) -> E.EqualityResult:
    bool-result =
      (self.field-name == other.field-name) and
      (self.typ == other.typ)
    E.from-boolean(bool-result)
  end
end

type TypeMembers = List<TypeMember>

data TypeVariant:
  | t-variant(name        :: String,
              fields      :: List<TypeMember>,
              with-fields :: List<TypeMember>,
              l           :: A.Loc)
  | t-singleton-variant(name        :: String,
                        with-fields :: List<TypeMember>,
                        l           :: A.Loc) with:
    fields: empty
sharing:
  substitute(self, new-type :: Type, old-type :: Type):
    cases(TypeVariant) self:
      | t-variant(name, fields, with-fields, l) =>
        new-fields = fields.map(_.substitute(new-type, old-type))
        new-with-fields = with-fields.map(_.substitute(new-type, old-type))
        t-variant(name, new-fields, new-with-fields, l)
      | t-singleton-variant(name, with-fields, l) =>
        new-with-fields = with-fields.map(_.substitute(new-type, old-type))
        t-singleton-variant(name, new-with-fields, l)
    end
  end,
  set-loc(self, loc :: A.Loc):
    cases(TypeVariant) self:
      | t-variant(name, fields, with-fields, _) =>
        t-variant(name, fields, with-fields, loc)
      | t-singleton-variant(name, with-fields, _) =>
        t-singleton-variant(name, with-fields, loc)
    end
  end,
  free-variable(self, var-type :: Type) -> Boolean:
    cases(TypeVariant) self:
      | t-variant(_, fields, with-fields, _) =>
        all(_.free-variable(var-type), fields) and
        all(_.free-variable(var-type), with-fields)
      | t-singleton-variant(_, with-fields, _) =>
        all(_.free-variable(var-type), with-fields)
    end
  end,
  _equals(self, other :: TypeVariant, _) -> E.EqualityResult:
    bool-result =
      cases(TypeVariant) self:
        | t-variant(a-name, a-fields, a-with-fields, _) =>
          cases(TypeVariant) other:
            | t-variant(b-name, b-fields, b-with-fields, _) =>
              (a-name == b-name) and
              compare-lists(a-fields, b-fields) and
              compare-lists(a-with-fields, b-with-fields)
            | else => false
          end
        | t-singleton-variant(a-name, a-with-fields, _) =>
          cases(TypeVariant) other:
            | t-singleton-variant(b-name, b-with-fields, _) =>
              (a-name == b-name) and
              compare-lists(a-with-fields, b-with-fields)
            | else => false
          end
      end
    E.from-boolean(bool-result)
  end
end

data Type:
  | t-name(module-name :: Option<String>, id :: Name, l :: A.Loc)
  | t-var(id :: Name, l :: A.Loc)
  | t-arrow(args :: List<Type>, ret :: Type, l :: A.Loc)
  | t-app(onto :: Type, args :: List<Type>, l :: A.Loc)
  | t-top(l :: A.Loc)
  | t-bot(l :: A.Loc)
  | t-record(fields :: List<TypeMember>, l :: A.Loc)
  | t-forall(introduces :: List<Type>, onto :: Type, l :: A.Loc)
  | t-ref(typ :: Type, l :: A.Loc)
  | t-existential(id :: Name, l :: A.Loc)
  | t-data(name :: String, variants :: List<TypeVariant>, fields :: List<TypeMember>, l :: A.Loc)
sharing:
  _output(self):
    cases(Type) self:
      | t-name(module-name, id, _) => VS.vs-value(id.toname())
      | t-var(id, _) => VS.vs-value(id.toname())
      | t-arrow(args, ret, _) =>
        VS.vs-seq([list: VS.vs-str("(")]
          + interleave(args.map(VS.vs-value), VS.vs-str(", "))
          + [list: VS.vs-str(" -> "), VS.vs-value(ret), VS.vs-str(")")])
      | t-app(onto, args, _) =>
        VS.vs-seq([list: VS.vs-value(onto), VS.vs-str("<")]
          + interleave(args.map(VS.vs-value), VS.vs-str(", "))
          + [list: VS.vs-str(">")])
      | t-top(_) => VS.vs-str("Any")
      | t-bot(_) => VS.vs-str("Bot")
      | t-record(fields, _) =>
        VS.vs-seq([list: VS.vs-str("{")]
          + interleave(fields.map(VS.vs-value), VS.vs-value(", "))
          + [list: VS.vs-str("}")])
      | t-forall(introduces, onto, _) =>
        VS.vs-seq([list: VS.vs-str("forall ")]
          + interleave(introduces.map(VS.vs-value), VS.vs-str(", "))
          + [list: VS.vs-str(" . "), VS.vs-value(onto)])
      | t-ref(typ, _) =>
        VS.vs-seq([list: VS.vs-str("ref "), VS.vs-value(typ)])
      | t-existential(id, _) => VS.vs-str(id.key())
      | t-data(name, variants, fields, _) => VS.vs-str(name)
    end
  end,
  key(self) -> String:
    cases(Type) self:
      | t-name(module-name, id, _) =>
        cases(Option<String>) module-name:
          | none => id.key()
          | some(m) => m + "." + id.key()
        end
      | t-var(id, _) => id.key()
      | t-arrow(args, ret, _) =>
        "("
          + args.map(_.key()).join-str(", ")
          + " -> " + ret.key() + ")"
      | t-app(onto, args, _) =>
        onto.key() + "<" + args.map(_.key()).join-str(", ") + ">"
      | t-top(_) => "Top"
      | t-bot(_) => "Bot"
      | t-record(fields, _) =>
        "{"
          + for map(field from fields):
              field.key()
            end.join-str(", ")
          + "}"
      | t-forall(introduces, onto, _) =>
        "<" + introduces.map(_.key()).join-str(", ") + ">"
          + onto.key()
      | t-ref(typ, _) =>
        "ref " + typ.key()
      | t-existential(id, _) => id.key()
      | t-data(name, variants, fields, _) => name
    end
  end,
  substitute(self, new-type :: Type, old-type :: Type):
    if self == old-type:
      new-type
    else:
      cases(Type) self:
        | t-arrow(args, ret, l) =>
          new-args = args.map(_.substitute(new-type, old-type))
          new-ret = ret.substitute(new-type, old-type)
          t-arrow(new-args, new-ret, l)
        | t-app(onto, args, l) =>
          new-onto = onto.substitute(new-type, old-type)
          new-args = args.map(_.substitute(new-type, old-type))
          t-app(new-onto, new-args, l)
        | t-record(fields, l) =>
          t-record(fields.map(_.substitute(new-type, old-type)), l)
        | t-forall(introduces, onto, l) =>
          # doesn't need to be capture avoiding due to resolve-names
          new-onto = onto.substitute(new-type, old-type)
          t-forall(introduces, new-onto, l)
        | t-ref(typ, l) =>
          t-ref(typ.substitute(new-type, old-type), l)
        | t-data(name, variants, fields, l) =>
          t-data(name,
                 variants.map(_.substitute(new-type, old-type)),
                 fields.map(_.substitute(new-type, old-type)),
                 l)
        | else => self
      end
    end
  end,
  introduce(self, args :: List<Type>) -> Type:
    cases(Type) self:
      | t-forall(introduces, onto, l) =>
        fold2(lam(new-type, param-type, arg-type):
          new-type.substitute(arg-type, param-type)
        end, onto, self.introduces, args)
      | else => self
    end
  end,
  free-variable(self, var-type :: Type) -> Boolean:
    if self == var-type:
      false
    else:
      cases(Type) self:
        | t-name(_, _, _) =>
          true
        | t-var(_, _) =>
          true
        | t-arrow(args, ret, _) =>
          all(_.free-variable(var-type), args) and
          ret.free-variable(var-type)
        | t-app(onto, args, _) =>
          onto.free-variable(var-type) and
          all(_.free-variable(var-type), args)
        | t-top(_) =>
          true
        | t-bot(_) =>
          true
        | t-record(fields, _) =>
          all(_.free-variable(var-type), fields)
        | t-forall(_, onto, _) =>
          onto.free-variable(var-type)
        | t-ref(typ, _) =>
          typ.free-variable(var-type)
        | t-existential(_, _) =>
          true
        | t-data(_, variants, fields, _) =>
          all(_.free-variable(var-type), variants) and
          all(_.free-variable(var-type), fields)
      end
    end
  end,
  lookup-variant(self, name :: String) -> Option<TypeVariant>:
    cases(Type) self:
      | t-data(_, variants, _, _) =>
        variants.find(lam(tv): tv.name == name end)
      | else => none
    end
  end,
  set-loc(self, loc :: A.Loc):
    cases(Type) self:
      | t-name(module-name, id, _) =>
        t-name(module-name, id, loc)
      | t-var(id, _) =>
        t-var(id, loc)
      | t-arrow(args, ret, _) =>
        t-arrow(args, ret, loc)
      | t-app(onto, args, _) =>
        t-app(onto, args, loc)
      | t-top(_) =>
        t-top(loc)
      | t-bot(_) =>
        t-bot(loc)
      | t-record(fields, _) =>
        t-record(fields, loc)
      | t-forall(introduces, onto, _) =>
        t-forall(introduces, onto, loc)
      | t-ref(typ, _) =>
        t-ref(typ, loc)
      | t-existential(id, _) =>
        t-existential(id, loc)
      | t-data(name, variants, fields, _) =>
        t-data(name, variants, fields, loc)
    end
  end,
  _equals(self, other :: Type, _) -> E.EqualityResult:
    bool-result =
      cases(Type) self:
        | t-name(a-module-name, a-id, _) =>
          cases(Type) other:
            | t-name(b-module-name, b-id, _) =>
              (a-module-name == b-module-name) and
              (a-id == b-id)
            | else => false
          end
        | t-var(a-id, _) =>
          cases(Type) other:
            | t-var(b-id, _) => a-id == b-id
            | else => false
          end
        | t-arrow(a-args, a-ret, _) =>
          cases(Type) other:
            | t-arrow(b-args, b-ret, _) =>
              compare-lists(a-args, b-args) and
              (a-ret == b-ret)
            | else => false
          end
        | t-app(a-onto, a-args, _) =>
          cases(Type) other:
            | t-app(b-onto, b-args, _) =>
              (a-onto == b-onto) and
              compare-lists(a-args, b-args)
            | else => false
          end
        | t-top(_) =>
          cases(Type) other:
            | t-top(_) => true
            | else => false
          end
        | t-bot(_) =>
          cases(Type) other:
            | t-bot(_) => true
            | else => false
          end
        | t-record(a-fields, _) =>
          cases(Type) other:
            | t-record(b-fields, _) =>
              compare-lists(a-fields, b-fields)
            | else => false
          end
        | t-forall(a-introduces, a-onto, _) =>
          cases(Type) other:
            | t-forall(b-introduces, b-onto, _) =>
              compare-lists(a-introduces, b-introduces) and
              (a-onto == b-onto)
            | else => false
          end
        | t-ref(a-typ, _) =>
          cases(Type) other:
            | t-ref(b-typ, _) =>
              a-typ == b-typ
            | else => false
          end
        | t-existential(a-id, _) =>
          cases(Type) other:
            | t-existential(b-id, _) =>
              a-id == b-id
            | else => false
          end
        | t-data(a-name, a-variants, a-fields, _) =>
          cases(Type) other:
            | t-data(b-name, b-variants, b-fields, _) =>
              compare-lists(a-variants, b-variants) and
              compare-lists(a-fields, b-fields)
            | else => false
          end
      end
    E.from-boolean(bool-result)
  end
end

# TODO(MATT): which of these should be kept
builtin-uri = some("builtin")

t-array-name = t-name(none, A.s-type-global("RawArray"), A.dummy-loc)

t-number  = lam(l): t-name(builtin-uri, A.s-type-global("Number"), l) end
t-string  = lam(l): t-name(builtin-uri, A.s-type-global("String"), l) end
t-boolean = lam(l): t-name(builtin-uri, A.s-type-global("Boolean"), l) end
t-nothing = lam(l): t-name(builtin-uri, A.s-type-global("Nothing"), l) end
t-srcloc  = lam(l): t-name(builtin-uri, A.s-type-global("Loc"), l) end
t-array   = lam(v, l): t-app(t-array-name, [list: v], l) end
t-option  = lam(v, l): t-app(t-name(some("pyret-builtin://option"), A.s-type-global("Option"), l), [list: v], l) end
