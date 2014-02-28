#lang pyret

provide {
  check-well-formed: check-well-formed
} end
import ast as A
import "./compile-structs.arr" as C
import format as F

# TODO: Make this a mutable field when we have them...
var errors = []
var in-check-block = false
var PARAM-current-where-everywhere = false # TODO: What does this mean? (used by ensure-empty-block)


fun wf-error(msg, loc):
  e = C.wf-err(msg, loc)
  errors := e ^ link(errors)
  nothing
end
fun wf-error2(msg, loc1, loc2):
  e = C.wf-err-split(msg, [loc1, loc2])
  errors := e ^ link(errors)
  nothing
end
fun duplicate-id(id, loc1, loc2):
  e = C.duplicate-id(id, loc1, loc2)
  errors := e ^ link(errors)
  nothing
end

fun wrap-visit-check(self, target):
  cur-in-check = in-check-block
  in-check-block := true
  ret = self.option(target)
  in-check-block := cur-in-check
  ret
end


fun ensure-empty-block(loc, type, block :: A.is-s_block):
  if not PARAM-current-where-everywhere:
    if block.stmts.length() == 0: nothing
    else:
      wf-error("where: blocks only allowed on named function declarations and data, not on " + tostring(type), loc)
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
        | s_cases_branch(l, name, args, body) =>
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
        | s_bind(l, shadows, id, ann) =>
          if id == "_": nothing # TODO: Fix when we have real underscores
          else:
            cases(Option) list.find(fun(b): b.id == id end, rest):
              | some(found) => wf-error2("Found duplicate id " + id + " in list of bindings", l, found.l)
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
        | s_bind(l, shadows, id, ann) =>
          if id == A.s_underscore: nothing
          else if shadows: nothing
          else:
            cases(Option) list.find(fun(b): b.id == id end, rest):
              | some(found) => duplicate-id(id, l, found.l)
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
    | s_let(l, _, _) => wf-error("Cannot end a block in a let-binding", l)
    | s_var(l, _, _) => wf-error("Cannot end a block in a var-binding", l)
    | s_fun(l, _, _, _, _, _, _, _) => wf-error("Cannot end a block in a fun-binding", l)
    | s_data(l, _, _, _, _, _, _) => wf-error("Cannot end a block with a data definition", l)
    | s_datatype(l, _, _, _, _) => wf-error("Cannot end a block with a datatype definition", l)
    | s_graph(l, _) => wf-error("Cannot end a block with a graph definition", l)
    | else => nothing
  end
end

fun fields-to-binds(members :: List<A.Member>) -> List<A.Bind>:
  for map(mem from members):
    A.s_bind(mem.l, false, A.s_name(mem.name.s), A.a_blank)
  end
end

fun opname(op): op.substring(2, op.length()) end
fun reachable-ops(self, l, op, ast):
  cases(A.Expr) ast:
    | s_not(l2, _) =>
      wf-error2("Cannot have nested bare `not` with a `"
          + opname(op)
          + "` operator.  Include parentheses around it.",
        l, l2)
      true
    | s_op(l2, op2, left2, right2) =>
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
  well-formed-visitor = A.default-iter-visitor.{
    s_op(self, l, op, left, right):
      reachable-ops(self, l, op, left) and reachable-ops(self, l, op, right)
    end,
    s_cases_branch(self, l, name, args, body):
      when (name == "_"):
        wf-error("Found a cases branch using _ rather than a constructor name; use 'else' instead", l)
      end
      ensure-unique-ids(args)
      list.all(_.visit(self), args) and body.visit(self)
    end,
    s_block(self, l, stmts):
      if is-empty(stmts): nothing
      else:
        wf-last-stmt(stmts.last())
        bind-stmts = stmts.filter(fun(s): A.is-s_var(s) or A.is-s_let(s) end).map(_.name)
        ensure-unique-bindings(bind-stmts.reverse())
        list.all(_.visit(self), stmts)
      end
    end,
    s_singleton_variant(self, l, name, with-members):
      ensure-unique-ids(fields-to-binds(with-members) + cur-shared)
      list.all(_.visit(self), with-members)
    end,
    s_variant(self, l, name, binds, with-members):
      ensure-unique-ids(fields-to-binds(with-members) + binds.map(_.bind) + cur-shared)
      list.all(_.visit(self), binds) and list.all(_.visit(self), with-members)
    end,
    s_data(self, l, name, params, mixins, variants, shares, _check):
      ensure-unique-variant-ids(variants)
      the-cur-shared = cur-shared
      cur-shared := fields-to-binds(shares)
      ret = list.all(_.visit(self), mixins) and list.all(_.visit(self), variants) and list.all(_.visit(self), shares)
      cur-shared := the-cur-shared
      ret and wrap-visit-check(self, _check)
    end,
    s_datatype_variant(self, l, name, binds, constructor):
      ensure-unique-ids(fields-to-binds(binds))
      list.all(_.visit(self), binds) and constructor.visit(self)
    end,
    s_data_expr(self, l, name, params, mixins, variants, shared, _check):
      ensure-unique-variant-ids(variants)
      the-cur-shared = cur-shared
      cur-shared := fields-to-binds(shared)
      ret = list.all(_.visit(self), mixins) and list.all(_.visit(self), variants) and list.all(_.visit(self), shared)
      cur-shared := the-cur-shared
      ret and wrap-visit-check(self, _check)
    end,
    s_check_test(self, l, op, left, right):
      when (not in-check-block):
        if  (op == "opis"):
          wf-error("Cannot use `is` outside of a `check` or `where` block", l)
        else:
          wf-error("Cannot use a check-test form outside of a `check` or `where` block", l)
        end
      end
      left.visit(self) and right.visit(self)
    end,
    s_method_field(self, l, name, args, ann, doc, body, _check):
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
    s_method(self, l, args, ann, doc, body, _check):
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
    s_lam(self, l, params, args, ann, doc, body, _check):
      ensure-unique-ids(args)
      cases(Option) _check:
        | none => nothing
        | some(chk) => ensure-empty-block(l, "anonymous functions", chk)
      end
      list.all(_.visit(self), args) and ann.visit(self) and body.visit(self) and wrap-visit-check(self, _check)
    end,
    s_fun(self, s, name, params, args, ann, doc, body, _check):
      ensure-unique-ids(args)
      list.all(_.visit(self), args) and ann.visit(self) and body.visit(self) and wrap-visit-check(self, _check)
    end,
    s_check(self, l, name, body):
      wrap-visit-check(self, some(body))
    end,
    s_if(self, l, branches):
      when branches.length() == 1:
        wf-error("Cannot have an `if` with a single branch", l)
      end
      list.all(_.visit(self), branches)
    end,
    s_cases(self, l, type, val, branches):
      ensure-unique-cases(branches)
      type.visit(self) and val.visit(self) and list.all(_.visit(self), branches)
    end,
    s_cases_else(self, l, type, val, branches, _else):
      ensure-unique-cases(branches)
      type.visit(self) and val.visit(self) and list.all(_.visit(self), branches) and _else.visit(self)
    end,
    s_id(self, l, id):
      when (id == "check") or (id == "where"):
        wf-error("Cannot use `" + id + "` as an identifier", l)
      end
      true
    end
  }

  in-check-block := false
  if ast.visit(well-formed-visitor) and (errors.length() == 0): C.ok(ast)
  else: C.err(errors)
  end
end
