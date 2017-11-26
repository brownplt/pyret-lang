#|
Assume that all AST nodes are annotated, this file generates visitors
|#

import ast as A
import parse-pyret as SP
import file as F

trove-dir = "src/arr/trove"
in-name = trove-dir + "/ast.arr"
out-name = trove-dir + "/ast-visitors.arr"
dummy = A.dummy-loc

p = SP.surface-parse(F.input-file(in-name).read-file(), in-name)

fun shared-member-has-visit(shared-members :: List<A.Member>) -> Boolean:
  for lists.any(member from shared-members):
    cases (A.Member) member:
      | s-method-field(
          l :: A.Loc,
          name :: String,
          params :: List<A.Name>,
          args :: List<A.Bind>,
          ann :: A.Ann,
          doc :: String,
          body :: A.Expr,
          _check-loc :: Option<A.Loc>,
          _check :: Option<A.Expr>,
          blocky :: Boolean) =>
        name == 'visit'
    end
  end
end

data SimplifiedVariant:
  | simplified-variant(name :: String, members :: List<A.Bind>)
  | simplified-singleton-variant(name :: String, members :: List<A.Bind>%(is-empty))
end

fun simplify-variant(v :: A.Variant) -> SimplifiedVariant:
  doc: ```
       Assume that AST variants are not "fancy"--no ref fields, etc. This
       function extracts the core information and convert it to a simplified
       variant
       ```
  cases (A.Variant) v.visit(A.dummy-loc-visitor):
    | s-variant(_, _, name, members, _) => simplified-variant(name, members.map(_.bind))
    | s-singleton-variant(_, name, _) => simplified-singleton-variant(name, empty)
  end
end

var collected-variants = empty
var collected-data-definitions = empty

p.visit(A.default-iter-visitor.{
  method s-data(self, l :: A.Loc, name :: String, params :: List<A.Name>, mixins :: List<A.Expr>, variants :: List<A.Variant>, shared-members :: List<A.Member>, _check-loc :: Option<A.Loc>, _check :: Option<A.Expr>) block:
    when shared-member-has-visit(shared-members) block:
      collected-variants := collected-variants + variants.map(simplify-variant)
      collected-data-definitions := link(name, collected-data-definitions)
    end
    true
  end
})

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

fun strip-annotation(b :: A.Bind) -> A.Bind:
  cases (A.Bind) b:
    | s-bind(l :: A.Loc, shadows :: Boolean, id :: A.Name, ann :: A.Ann) =>
      A.s-bind(l, shadows, id, A.a-blank)
  end
end

fun visitor-maker(name :: String, transformer):
  doc: ```
       Assume no variant member has `self` as its name, this makes a visitor
       ```
  methods = for map(variant from collected-variants):
    A.s-method-field(
      dummy,
      variant.name,
      empty, # TODO(Oak): not sure what this is
      link(make-bind("self"), variant.members.map(strip-annotation)), # strip annotation for performance
      A.a-blank,
      "",
      A.s-block(dummy, [list: transformer(variant)]),
      none,
      none,
      false)
  end
  A.s-let(dummy, make-bind(name), A.s-obj(dummy, methods), false)
end

fun bind-to-id(b :: A.Bind) -> A.Expr:
  A.s-id(dummy, b.id)
end

data ArgType:
  | arg-not-visitable
  | arg-visitable
  | arg-list
  | arg-option
end

fun get-arg-type(b :: A.Bind) -> ArgType:
  cases (A.Ann) b.ann:
    | a-blank => raise("You probably want to annotate " + tostring(b))
    | a-name(_, name) =>
      # assume this name is of type Name%(is-s-name)
      cases (A.Name) name:
        | s-name(_, s) =>
          if collected-data-definitions.member(s):
            arg-visitable
          else:
            arg-not-visitable
          end
        | else => raise("impossible (I think) " + tostring(name))
      end
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
              # assume this name is of type Name%(is-s-name)
              cases (A.Name) name:
                | s-name(_, s) =>
                  if collected-data-definitions.member(s):
                    arg-type
                  else:
                    arg-not-visitable
                  end
                | else => raise("impossible (I think) " + tostring(name))
              end
            | else => raise("impossible (I think) " + tostring(args.get(0)))
          end
      end
    | else => arg-not-visitable
  end
end

fun default-map-visitor-transform(variant :: SimplifiedVariant) -> A.Expr:
  cases (SimplifiedVariant) variant:
    | simplified-variant(name, members) =>
      shadow members = members.map(lam(b :: A.Bind) -> A.Expr:
        id = bind-to-id(b)
        cases (ArgType) get-arg-type(b):
          | arg-not-visitable => id
          | arg-visitable => make-visit-self(id)
          | arg-list => make-complex-visit(id, "map")
          | arg-option => make-complex-visit(id, "and-then")
        end
      end)
      A.s-app(dummy, make-id(name), members)
    | simplified-singleton-variant(name, _) => make-id(name)
  end
end

fun default-iter-visitor-transform(variant :: SimplifiedVariant) -> A.Expr:
  cases (SimplifiedVariant) variant:
    | simplified-variant(name, members) =>
      shadow members = for lists.filter-map(b from members):
        id = bind-to-id(b)
        cases (ArgType) get-arg-type(b):
          | arg-not-visitable => none
          | arg-visitable => some(make-visit-self(id))
          | arg-list => some(make-complex-visit(id, "all"))
          | arg-option =>
            make-complex-visit(id, "and-then")
              ^ make-method-call(_, "or-else", [list: A.s-bool(dummy, true)])
              ^ some
        end
      end
      cases (List) members:
        | empty => A.s-bool(dummy, true)
        | link(f, r) =>
          # left recursion
          for fold(prev from f, e from r):
            A.s-op(dummy, dummy, "opand", prev, e)
          end
      end
    | simplified-singleton-variant(name, _) => A.s-bool(dummy, true)
  end
end

fun dummy-loc-visitor-transform(variant :: SimplifiedVariant) -> A.Expr:
  cases (SimplifiedVariant) variant:
    | simplified-variant(name, members) =>
      shadow members = members.map(lam(b :: A.Bind) -> A.Expr:
        id = bind-to-id(b)
        cases (ArgType) get-arg-type(b):
          | arg-not-visitable => id
          | arg-visitable => make-visit-self(id)
          | arg-list => make-complex-visit(id, "map")
          | arg-option => make-complex-visit(id, "and-then")
        end
      end)
      shadow members = cases (List) members:
        | empty => empty
        | link(_, rest) => link(make-id("dummy-loc"), rest)
      end
      A.s-app(dummy, make-id(name), members)
    | simplified-singleton-variant(name, _) => make-id(name)
  end
end

body = [list:
  visitor-maker("default-map-visitor", default-map-visitor-transform),
  visitor-maker("default-iter-visitor", default-iter-visitor-transform),
  visitor-maker("dummy-loc-visitor", dummy-loc-visitor-transform)]

out-program =
  A.s-program(dummy,
    A.s-provide-all(dummy),
    A.s-provide-types-none(dummy),
    [list:
      A.s-include(dummy, A.s-const-import(dummy, "ast")),
      A.s-import(dummy, A.s-const-import(dummy, "global"), A.s-underscore(dummy)),
      A.s-import(dummy, A.s-const-import(dummy, "base"), A.s-underscore(dummy))],
    A.s-block(dummy, body))

as-string = out-program.tosource().pretty(80).join-str("\n")

F.output-file(out-name, false).display(as-string)
