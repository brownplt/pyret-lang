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

data ArgType:
  | arg-not-visitable
  | arg-visitable
  | arg-list
  | arg-option
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

fun is-only-serializable(shared-members :: List<A.Member>) -> Boolean:
  for lists.any(member from shared-members):
    cases (A.Member) member:
      | s-data-field(l, name, value) =>
        (name == 'only-serializable') and A.is-s-bool(value) and value.b
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

fun collect-ast(p :: A.Program) -> {List<SimplifiedVariant>; D.StringDict<Boolean>} block:
  var collected-variants = empty
  var collected-data-definitions = [D.mutable-string-dict: ]

  p.visit(AV.default-iter-visitor.{
    method s-data(self, l :: A.Loc, name :: String, params :: List<A.Name>, mixins :: List<A.Expr>, variants :: List<A.Variant>, shared-members :: List<A.Member>, _check-loc :: Option<A.Loc>, _check :: Option<A.Expr>) block:
      when shared-member-has-visit(shared-members) block:
        collected-variants := collected-variants + variants.map(simplify-variant(name))
        collected-data-definitions.set-now(name, is-only-serializable(shared-members))
      end
      true
    end
  })
  {collected-variants; collected-data-definitions.freeze()}
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
    id,
    meth,
    [list: make-visit-self(A.s-id(dummy, A.s-underscore(dummy)))])
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

fun get-arg-type(
  ann-top :: A.Ann,
  collected-data-definitions :: D.StringDict<Boolean>,
  include-only-serializable :: Boolean
) -> ArgType:
  fun name-arg-type(name :: A.Name) -> ArgType:
    cases (A.Name) name:
      | s-name(_, s) =>
        cases (Option) collected-data-definitions.get(s):
          | none => arg-not-visitable
          | some(v) =>
            if include-only-serializable or not(v):
              arg-visitable
            else:
              arg-not-visitable
            end
        end
      | else => raise("impossible (I think) " + tostring(name))
    end
  end
  cases (A.Ann) ann-top:
    | a-blank => raise("You probably want to annotate")
    | a-name(_, name) => name-arg-type(name)
    | a-app(_, ann, args) =>
      arg-type = cases (A.Ann) ann:
        | a-name(_, name) =>
          # assume this name is of type Name%(is-s-name)
          cases (A.Name) name:
            | s-name(_, s) =>
              ask:
                | s == "List" then: arg-list
                | s == "Option" then: arg-option
                | otherwise: arg-not-visitable
              end
            | else => raise("impossible (I think) " + tostring(ann))
          end
        | else => raise("impossible (I think) " + tostring(ann))
      end
      cases (ArgType) arg-type:
        | arg-not-visitable => arg-not-visitable
        | arg-visitable => raise("impossible")
        | else =>
          # this is either List or Option, so args has exactly one element
          cases (A.Ann) args.get(0):
            | a-name(_, name) =>
              cases (ArgType) name-arg-type(name):
                | arg-not-visitable => arg-not-visitable
                | arg-visitable => arg-type
                | else => raise("impossible")
              end
            | else => raise("impossible (I think) " + tostring(args.get(0)))
          end
      end
    | a-pred(_, ann, _) =>
      get-arg-type(ann, collected-data-definitions, include-only-serializable)
    | else => arg-not-visitable
  end
end

fun visitor-maker(
  collected-variants :: List<SimplifiedVariant>,
  name :: String,
  transformer :: (SimplifiedVariant -> A.Expr),
  is-strip-annotation :: Boolean
) -> A.Expr:
  doc: ```
       Assume no variant member has `self` as its name, this makes a visitor
       ```
  methods = for map(variant from collected-variants):
    A.s-method-field(
      dummy,
      variant.name,
      empty, # TODO(Oak): not sure what this is
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
  A.s-let(dummy, make-bind(name), A.s-obj(dummy, methods), false)
end
