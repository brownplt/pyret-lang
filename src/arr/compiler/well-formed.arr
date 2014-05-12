#lang pyret

provide {
  check-well-formed: check-well-formed
} end
import ast as A
import "compiler/compile-structs.arr" as C
import format as F

# TODO: Make this a mutable field when we have them...
var errors = []
var in-check-block = false
var PARAM-current-where-everywhere = false # TODO: What does this mean? (used by ensure-empty-block)

reserved-names = [
  "function",
  "break",
  "return",
  "do",
  "yield",
  "throw",
  "continue",
  "while",
  "class",
  "interface",
  "type",
  "generator",
  "alias",
  "extends",
  "implements",
  "module",
  "package",
  "namespace",
  "use",
  "public",
  "private",
  "protected",
  "static",
  "const",
  "enum",
  "super",
  "export",
  "new",
  "try",
  "finally",
  "debug",
  "spy",
  "switch",
  "this",
  "match",
  "case",
  "with"
]


fun add-error(err):
  errors := err ^ link(errors)
  nothing
end
fun wf-error(msg, loc):
  add-error(C.wf-err(msg, loc))
end
fun wf-error2(msg, loc1, loc2):
  add-error(C.wf-err-split(msg, [loc1, loc2]))
end
fun duplicate-id(id, loc1, loc2):
  add-error(C.duplicate-id(id, loc1, loc2))
end
fun reserved-name(loc, id):
  add-error(C.reserved-name(loc, id))
end

fun wrap-visit-check(self, target):
  cur-in-check = in-check-block
  in-check-block := true
  ret = self.option(target)
  in-check-block := cur-in-check
  ret
end


fun ensure-empty-block(loc, typ, block :: A.is-s-block):
  if not(PARAM-current-where-everywhere):
    if block.stmts.length() == 0: nothing
    else:
      wf-error("where: blocks only allowed on named function declarations and data, not on " + tostring(typ), loc)
    end
  else:
    nothing
  end
end

fun ensure-unique-cases(_cases :: List<A.CasesBranch>):
  cases(List) _cases:
    | empty => nothing
    | link(f, rest) =>
      cases(A.CasesBranch) f:
        | s-cases-branch(l, name, args, body) =>
          cases(Option) list.find(fun(b): b.name == name end, rest):
            | some(found) => wf-error2("Duplicate case for " + name, found.l, l)
            | none => ensure-unique-cases(rest)
          end
      end
  end
end

fun ensure-unique-ids(bindings :: List<A.Bind>):
  cases(List) bindings:
    | empty => nothing
    | link(f, rest) =>
      cases(A.Bind) f:
        | s-bind(l, shadows, id, ann) =>
          if A.is-s-underscore(id): nothing
          else:
            cases(Option) list.find(fun(b): b.id == id end, rest):
              | some(found) => wf-error2("Found duplicate id " + tostring(id) + " in list of bindings", l, found.l)
              | none => ensure-unique-ids(rest)
            end
          end
      end
  end
end

# NOTE: This is almost exactly the same function as above, but gives a
# different error This is replicating the old behavior; does it still
# make any sense?
fun ensure-unique-bindings(rev-bindings :: List<A.Bind>):
  cases(List) rev-bindings:
    | empty => nothing
    | link(f, rest) =>
      cases(A.Bind) f:
        | s-bind(l, shadows, id, ann) =>
          if A.is-s-underscore(id): nothing
          else if shadows: nothing
          else:
            cases(Option) list.find(fun(b): b.id == id end, rest):
              | some(found) => duplicate-id(tostring(id), l, found.l)
              | none => ensure-unique-bindings(rest)
            end
          end
      end
  end
end


fun ensure-unique-variant-ids(variants :: List): # A.DatatypeVariant or A.Variant
  cases(List) variants:
    | empty => nothing
    | link(f, rest) =>
      cases(Option) list.find(fun(b): b.name == f.name end, rest):
        | some(found) => wf-error2("Found duplicate id " + f.name + " in list of bindings", f.l, found.l)
        | none => ensure-unique-variant-ids(rest)
      end
  end
end


fun wf-last-stmt(stmt :: A.Expr):
  cases(A.Expr) stmt:
    | s-let(l, _, _, _) => wf-error("Cannot end a block in a let-binding", l)
    | s-var(l, _, _) => wf-error("Cannot end a block in a var-binding", l)
    | s-fun(l, _, _, _, _, _, _, _) => wf-error("Cannot end a block in a fun-binding", l)
    | s-data(l, _, _, _, _, _, _) => wf-error("Cannot end a block with a data definition", l)
    | s-datatype(l, _, _, _, _) => wf-error("Cannot end a block with a datatype definition", l)
    | s-graph(l, _) => wf-error("Cannot end a block with a graph definition", l)
    | else => nothing
  end
end

fun fields-to-binds(members :: List<A.Member>) -> List<A.Bind>:
  for map(mem from members):
    A.s-bind(mem.l, false, A.s-name(mem.l, mem.name.s), A.a-blank)
  end
end

fun opname(op): string-substring(op, 2, string-length(op)) end
fun reachable-ops(self, l, op, ast):
  cases(A.Expr) ast:
    | s-op(l2, op2, left2, right2) =>
      if (op == op2):
        reachable-ops(self, l, op, left2)
        reachable-ops(self, l, op, right2)
      else:
        wf-error2("Cannot mix binary operators of different types: `"
            + opname(op) + "` and `" + opname(op2)
            + "`.  Use parentheses to disambiguate.",
          l, l2)
      end
      true
    | else => ast.visit(self)
  end
end

fun check-well-formed(ast) -> C.CompileResult<A.Program, Any>:
  var cur-shared = []
  errors := []
  well-formed-visitor = A.default-iter-visitor.{
    s-program(self, l, _provide, imports, body):
      ok-body = cases(A.Expr) body:
        | s-block(l2, stmts) => list.all(_.visit(self), stmts)
        | else => body.visit(self)
      end
      ok-body and (_provide.visit(self)) and (list.all(_.visit(self), imports))
    end,
    s-op(self, l, op, left, right):
      reachable-ops(self, l, op, left) and reachable-ops(self, l, op, right)
    end,
    s-cases-branch(self, l, name, args, body):
      when (name == "_"):
        wf-error("Found a cases branch using _ rather than a constructor name; use 'else' instead", l)
      end
      ensure-unique-ids(args)
      list.all(_.visit(self), args) and body.visit(self)
    end,
    s-block(self, l, stmts):
      if is-empty(stmts):
        wf-error("Empty block", l)
        true
      else:
        wf-last-stmt(stmts.last())
        bind-stmts = stmts.filter(fun(s): A.is-s-var(s) or A.is-s-let(s) end).map(_.name)
        ensure-unique-bindings(bind-stmts.reverse())
        list.all(_.visit(self), stmts)
      end
    end,
    s-singleton-variant(self, l, name, with-members):
      ensure-unique-ids(fields-to-binds(with-members) + cur-shared)
      list.all(_.visit(self), with-members)
    end,
    s-bind(self, l, shadows, name, ann):
      when (reserved-names.member(tostring(name))):
        reserved-name(l, tostring(name))
      end
      true
    end,
    s-variant(self, l, constr-loc, name, binds, with-members):
      ensure-unique-ids(fields-to-binds(with-members) + binds.map(_.bind) + cur-shared)
      list.all(_.visit(self), binds) and list.all(_.visit(self), with-members)
    end,
    s-data(self, l, name, params, mixins, variants, shares, _check):
      ensure-unique-variant-ids(variants)
      the-cur-shared = cur-shared
      cur-shared := fields-to-binds(shares)
      ret = list.all(_.visit(self), mixins) and list.all(_.visit(self), variants) and list.all(_.visit(self), shares)
      cur-shared := the-cur-shared
      ret and wrap-visit-check(self, _check)
    end,
    s-datatype-variant(self, l, name, binds, constructor):
      ensure-unique-ids(fields-to-binds(binds))
      list.all(_.visit(self), binds) and constructor.visit(self)
    end,
    s-data-expr(self, l, name, params, mixins, variants, shared, _check):
      ensure-unique-variant-ids(variants)
      the-cur-shared = cur-shared
      cur-shared := fields-to-binds(shared)
      ret = list.all(_.visit(self), mixins) and list.all(_.visit(self), variants) and list.all(_.visit(self), shared)
      cur-shared := the-cur-shared
      ret and wrap-visit-check(self, _check)
    end,
    s-check-test(self, l, op, left, right):
      when not(in-check-block):
        if  (op == "opis"):
          wf-error("Cannot use `is` outside of a `check` or `where` block", l)
        else:
          wf-error("Cannot use a check-test form outside of a `check` or `where` block", l)
        end
      end
      left.visit(self) and right.visit(self)
    end,
    s-method-field(self, l, name, args, ann, doc, body, _check):
      when args.length() == 0:
        wf-error("Cannot have a method with zero arguments", l)
      end
      ensure-unique-ids(args)
      cases(Option) _check:
        | none => nothing
        | some(chk) => ensure-empty-block(l, "methods", chk)
      end
      list.all(_.visit(self), args) and ann.visit(self) and body.visit(self) and wrap-visit-check(self, _check)
    end,
    s-method(self, l, args, ann, doc, body, _check):
      when args.length() == 0:
        wf-error("Cannot have a method with zero arguments", l)
      end
      ensure-unique-ids(args)
      cases(Option) _check:
        | none => nothing
        | some(chk) => ensure-empty-block(l, "methods", chk)
      end
      list.all(_.visit(self), args) and ann.visit(self) and body.visit(self) and wrap-visit-check(self, _check)
    end,
    s-lam(self, l, params, args, ann, doc, body, _check):
      ensure-unique-ids(args)
      cases(Option) _check:
        | none => nothing
        | some(chk) => ensure-empty-block(l, "anonymous functions", chk)
      end
      list.all(_.visit(self), args) and ann.visit(self) and body.visit(self) and wrap-visit-check(self, _check)
    end,
    s-fun(self, s, name, params, args, ann, doc, body, _check):
      ensure-unique-ids(args)
      list.all(_.visit(self), args) and ann.visit(self) and body.visit(self) and wrap-visit-check(self, _check)
    end,
    s-check(self, l, name, body, keyword-check):
      wrap-visit-check(self, some(body))
    end,
    s-if(self, l, branches):
      when branches.length() == 1:
        wf-error("Cannot have an `if` with a single branch", l)
      end
      list.all(_.visit(self), branches)
    end,
    s-cases(self, l, typ, val, branches):
      ensure-unique-cases(branches)
      typ.visit(self) and val.visit(self) and list.all(_.visit(self), branches)
    end,
    s-cases-else(self, l, typ, val, branches, _else):
      ensure-unique-cases(branches)
      typ.visit(self) and val.visit(self) and list.all(_.visit(self), branches) and _else.visit(self)
    end,
    s-id(self, l, id):
      when (reserved-names.member(tostring(id))):
        reserved-name(l, tostring(id))
      end
      true
    end
  }

  in-check-block := false
  if ast.visit(well-formed-visitor) and (errors.length() == 0): C.ok(ast)
  else: C.err(errors)
  end
end
