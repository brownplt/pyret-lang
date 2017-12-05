#|
Assume that all AST nodes are annotated, this file generates visitors
|#

provide *
provide-types *

import ast as A
import ast-visitors as AV
import parse-pyret as SP
import file as F
import string-dict as D

dummy = A.dummy-loc
success-code = 0
failure-code = 1

data SimplifiedVariant:
  | simplified-variant(
      type-name :: String,
      name :: String,
      members :: List<A.Bind>)
  | simplified-singleton-variant(
      type-name :: String,
      name :: String,
      members :: List<A.Bind>%(is-empty))
end

data Tag:
  | t-str
  | t-num
  | t-bool
  | t-loc
  | t-list(t :: Tag)
  | t-option(t :: Tag)
  | t-not-recognized
end

data VisitableArgType:
  | arg-not-visitable with:
    method is-visitable(self): false end
  | arg-visitable with:
    method is-visitable(self): true end
  | arg-list(t :: VisitableArgType) with:
    method is-visitable(self): self.t.is-visitable() end
  | arg-option(t :: VisitableArgType) with:
    method is-visitable(self): self.t.is-visitable() end
end

fun read(in-file :: String) -> A.Program:
  SP.surface-parse(F.input-file(in-file).read-file(), in-file)
end

fun shared-member-has-visit(shared-members :: List<A.Member>) -> Boolean:
  for lists.any(member from shared-members):
    cases (A.Member) member:
      | s-method-field(_, name, _, _, _, _, _, _, _, _) => name == 'visit'
      | else => false
    end
  end
end

fun simplify-variant(type-name :: String) -> (A.Variant -> SimplifiedVariant):
  doc: ```
       Assume that AST variants are not "fancy"--no ref fields, etc. This
       function extracts the core information and convert it to a simplified
       variant
       ```
  lam(v :: A.Variant):
    cases (A.Variant) v.visit(AV.dummy-loc-visitor):
      | s-variant(_, _, name, members, _) =>
        simplified-variant(type-name, name, members.map(_.bind))
      | s-singleton-variant(_, name, _) =>
        simplified-singleton-variant(type-name, name, empty)
    end
  end
end

fun collect-ast(p :: A.Program) -> {List<SimplifiedVariant>; List<String>} block:
  var collected-variants = empty
  var collected-data-definitions = empty

  p.visit(AV.default-iter-visitor.{
    method s-data(self, l :: A.Loc, name :: String, params :: List<A.Name>, mixins :: List<A.Expr>, variants :: List<A.Variant>, shared-members :: List<A.Member>, _check-loc :: Option<A.Loc>, _check :: Option<A.Expr>) block:
      when shared-member-has-visit(shared-members) block:
        # appending the other way around is more efficient, but would mess up the order
        collected-variants := collected-variants + variants.map(simplify-variant(name))
        collected-data-definitions := link(name, collected-data-definitions)
      end
      true
    end
  })
  {collected-variants; collected-data-definitions}
end

fun make-name(s :: String) -> A.Name:
  A.s-name(dummy, s)
end

fun make-bind(s :: String) -> A.Bind:
  A.s-bind(dummy, false, make-name(s), A.a-blank)
end

fun make-id(s :: String) -> A.Expr:
  A.s-id(dummy, make-name(s))
end

fun make-visit-self(id :: A.Expr) -> A.Expr:
  A.s-app(dummy, A.s-dot(dummy, id, "visit"), [list: make-id("self")])
end

fun make-method-call(obj :: A.Expr, meth-name :: String, args :: List<A.Expr>) -> A.Expr:
  A.s-app(dummy, A.s-dot(dummy, obj, meth-name), args)
end

fun make-complex-visit(id :: A.Expr, meth :: String) -> A.Expr:
  make-method-call(
    make-id('self'),
    meth,
    [list: id]
  )
end

fun is-loc(ann :: A.Ann) -> Boolean block:
  cases (A.Ann) ann:
    | a-name(_, name) =>
      cases (A.Name) name:
        | s-name(_, id) => id == "Loc"
        | else => false
      end
    | else => false
  end
end

fun strip-annotation(b :: A.Bind) -> A.Bind:
  cases (A.Bind) b:
    | s-bind(l :: A.Loc, shadows :: Boolean, id :: A.Name, ann :: A.Ann) =>
      A.s-bind(l, shadows, id, A.a-blank)
  end
end

fun bind-to-id(b :: A.Bind) -> A.Expr:
  A.s-id(dummy, b.id)
end

fun get-tag(ann :: A.Ann) -> Tag:
  cases (A.Ann) ann:
    | a-blank => t-not-recognized
    | a-name(_, name) =>
      cases (A.Name) name:
        | s-name(_, s) =>
          ask:
            | s == 'String' then: t-str
            | s == 'Number' then: t-num
            | s == 'NumInteger' then: t-num
            | s == 'Boolean' then: t-bool
            | s == 'Loc' then: t-loc
            | s == 'List' then: t-list(t-not-recognized)
            | s == 'Option' then: t-option(t-not-recognized)
            | otherwise: t-not-recognized
          end
        | else => raise("impossible (I think) " + tostring(name))
      end
    | a-app(_, shadow ann, args) =>
      cases (Tag) get-tag(ann):
        | t-list(_) => t-list(get-tag(args.get(0)))
        | t-option(_) => t-option(get-tag(args.get(0)))
        | else => t-not-recognized
      end
    | a-pred(_, shadow ann, _) => get-tag(ann)
  end
end

fun get-arg-type(
  ann :: A.Ann,
  collected-data-definitions :: List<String>
) -> VisitableArgType:
  shadow get-arg-type = get-arg-type(_, collected-data-definitions)

  fun name-arg-type(name :: A.Name) -> VisitableArgType:
    cases (A.Name) name:
      | s-name(_, s) =>
        if collected-data-definitions.member(s):
          arg-visitable
        else:
          arg-not-visitable
        end
      | else => raise("impossible (I think) " + tostring(name))
    end
  end
  cases (A.Ann) ann:
    | a-blank => raise("You probably want to annotate")
    | a-name(_, name) => name-arg-type(name)
    | a-app(_, shadow ann, args) =>
      opt = cases (Tag) get-tag(ann):
        | t-list(_) => some(arg-list)
        | t-option(_) => some(arg-option)
        | else => none
      end
      cases (Option) opt:
        | none => arg-not-visitable
        | some(v) => v(get-arg-type(args.get(0)))
      end
    | a-pred(_, shadow ann, _) => get-arg-type(ann)
    | else => arg-not-visitable
  end
end

fun visitor-maker(
  collected-variants :: List<SimplifiedVariant>,
  name :: String,
  transformer :: (SimplifiedVariant -> A.Expr),
  is-strip-annotation :: Boolean,
  preamble :: String
) -> A.Expr:
  doc: ```
       Assume no variant member has `self` as its name, this makes a visitor
       ```
  methods = for map(variant from collected-variants):
    A.s-method-field(
      dummy,
      variant.name,
      empty,
      link(
        make-bind("self"),
        if is-strip-annotation:
          variant.members.map(strip-annotation)
        else:
          variant.members
        end),
      A.a-blank,
      "",
      A.s-block(dummy, [list: transformer(variant)]),
      none,
      none,
      false)
  end
  method-preamble = SP.surface-parse(preamble, '').block.stmts.get(0).fields
  A.s-let(dummy, make-bind(name), A.s-obj(dummy, method-preamble + methods), false)
end
