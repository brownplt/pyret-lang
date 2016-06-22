#lang pyret

provide {
  check-well-formed: check-well-formed
} end
provide-types *

import ast as A
import srcloc as SL
import file("compile-structs.arr") as C
import format as F
import string-dict as SD

type Loc = SL.Srcloc

# TODO: Make this a mutable field when we have them...
var errors = empty
var in-check-block = false
var cur-shared = empty
var PARAM-current-where-everywhere = false # TODO: What does this mean? (used by ensure-empty-block)

is-s-let = A.is-s-let # ANNOYING WORKAROUND

reserved-names = [SD.string-dict: 
  "function", true,
  "break", true,
  "return", true,
  "do", true,
  "yield", true,
  "throw", true,
  "continue", true,
  "while", true,
  "class", true,
  "interface", true,
  "type", true,
  "generator", true,
  "alias", true,
  "extends", true,
  "implements", true,
  "module", true,
  "package", true,
  "namespace", true,
  "use", true,
  "public", true,
  "private", true,
  "protected", true,
  "static", true,
  "const", true,
  "enum", true,
  "super", true,
  "export", true,
  "new", true,
  "try", true,
  "finally", true,
  "debug", true,
  "spy", true,
  "switch", true,
  "this", true,
  "match", true,
  "case", true,
  "with", true,
  "__proto__", true
]


fun add-error(err) block:
  errors := err ^ link(_, errors)
  nothing
end
fun wf-error(msg, loc):
  add-error(C.wf-err(msg, loc))
end
fun wf-error2(msg, loc1, loc2):
  add-error(C.wf-err-split(msg, [list: loc1, loc2]))
end
fun duplicate-id(id, loc1, loc2):
  add-error(C.duplicate-id(id, loc1, loc2))
end
fun reserved-name(loc, id):
  add-error(C.reserved-name(loc, id))
end

fun wrap-visit-check(self, target) block:
  cur-in-check = in-check-block
  in-check-block := true
  ret = self.option(target)
  in-check-block := cur-in-check
  ret
end

is-s-block = A.is-s-block
fun ensure-empty-block(loc, typ, block :: A.Expr % (is-s-block)):
  if not(PARAM-current-where-everywhere):
    if block.stmts.length() == 0: nothing
    else:
      add-error(C.unwelcome-where(tostring(typ), loc))
    end
  else:
    nothing
  end
end

fun is-binder(expr):
  A.is-s-let(expr) or A.is-s-tuple-let(expr) or A.is-s-fun(expr) or A.is-s-var(expr) or A.is-s-rec(expr)
end

fun explicitly-blocky-block(block :: A.Expr % (is-s-block)) -> Boolean block:
  var seen-non-let = false
  var is-blocky = false
  var seen-template = false
  for each(expr from block.stmts):
    ask:
      | A.is-s-template(expr) then: seen-template := true
      | seen-non-let          then: is-blocky := true # even if expr is a binder, it's non-consecutive
      | not(is-binder(expr))  then: seen-non-let := true
      | otherwise: nothing
    end
  end
  # any template presence overrules blockiness presence
  is-blocky and not(seen-template)
end

fun wf-blocky-blocks(l :: Loc, blocks :: List<A.Expr % (is-s-block)>):
  explicitly-blocky-blocks = blocks.filter(explicitly-blocky-block)
  when not(is-empty(explicitly-blocky-blocks)):
    add-error(C.block-needed(l, explicitly-blocky-blocks.map(_.l)))
  end
end

fun ensure-unique-cases(_cases :: List<A.CasesBranch>):
  cases(List) _cases block:
    | empty => nothing
    | link(f, rest) =>
      cases(A.CasesBranch) f:
        | s-cases-branch(l, pat-loc, name, args, body) =>
          cases(Option) lists.find(lam(b): b.name == name end, rest):
            | some(found) => add-error(C.duplicate-branch(name, found.pat-loc, pat-loc))
            | none => nothing
          end
        | s-singleton-cases-branch(l, pat-loc, name, body) =>
          cases(Option) lists.find(lam(b): b.name == name end, rest):
            | some(found) => add-error(C.duplicate-branch(name, found.pat-loc, pat-loc))
            | none => nothing
          end
      end
      ensure-unique-cases(rest)
  end
end

fun ensure-unique-ids(bindings :: List<A.Bind>):
  cases(List) bindings block:
    | empty => nothing
    | link(f, rest) =>
      cases(A.Bind) f:
        | s-bind(l, shadows, id, ann) =>
          cases(A.Name) id:
            | s-underscore(_) => nothing
            | s-name(_, name) =>
              elt = lists.find(lam(b): A.is-s-name(b.id) and (b.id.s == name) end, rest)
              cases(Option) elt:
                | some(found) =>
                  add-error(C.duplicate-id(id.tosourcestring(), found.l, l))
                | none => nothing
              end
            | else =>
              elt = lists.find(lam(b): b.id == id end, rest)
              cases(Option) elt:
                | some(found) =>
                  add-error(C.duplicate-id(id.tosourcestring(), found.l, l))
                | none => nothing
              end
          end
      end
      ensure-unique-ids(rest)
  end
end

# NOTE: This is almost exactly the same function as above, but gives a
# different error This is replicating the old behavior; does it still
# make any sense?
fun ensure-unique-bindings(rev-bindings :: List<A.Bind>):
  cases(List) rev-bindings block:
    | empty => nothing
    | link(f, rest) =>
      cases(A.Bind) f:
        | s-bind(l, shadows, id, ann) =>
          if A.is-s-underscore(id): nothing
          else if shadows: nothing
          else:
            cases(Option) lists.find(lam(b): b.id == id end, rest):
              | some(found) => duplicate-id(id.tosourcestring(), l, found.l)
              | none => nothing
            end
          end
      end
      ensure-unique-bindings(rest)
  end
end

fun ensure-unique-fields(rev-fields):
  cases(List) rev-fields block:
    | empty => nothing
    | link(f, rest) =>
      cases(Option) lists.find(lam(f2): f2.name == f.name end, rest):
        | some(found) => add-error(C.duplicate-field(f.name, f.l, found.l))
        | none => nothing
      end
      ensure-unique-fields(rest)
  end
end

fun check-underscore-name(fields, kind-of-thing :: String) -> Boolean block:
  underscores = fields.filter(lam(f): f.name == "_" end)
  when not(is-empty(underscores)):
    add-error(C.underscore-as(underscores.first.l, kind-of-thing))
  end
  is-empty(underscores)
end

fun ensure-distinct-lines(loc :: Loc, prev-is-template :: Boolean, stmts :: List<A.Expr>):
  cases(List) stmts:
    | empty => nothing
    | link(first, rest) =>
      cases(Loc) loc:
        | builtin(_) => ensure-distinct-lines(first.l, A.is-s-template(first), rest)
        | srcloc(_, _, _, _, end-line1, _, _) =>
          cases(Loc) first.l block:
            | builtin(_) => ensure-distinct-lines(loc, prev-is-template, rest) # No need to preserve builtin() locs
            | srcloc(_, start-line2, _, _, _, _, _) =>
              when (end-line1 == start-line2):
                if A.is-s-template(first) and prev-is-template:
                  wf-error2("Found two adjacent template expressions on the same line: "
                      + "either remove one or separate them", loc, first.l)
                else if not(A.is-s-template(first)) and not(prev-is-template):
                  add-error(C.same-line(loc, first.l))
                else:
                  nothing
                end
              end
              ensure-distinct-lines(first.l, A.is-s-template(first), rest)
          end
      end
  end
end

fun ensure-unique-variant-ids(variants :: List<A.Variant>):
  cases(List) variants:
    | empty => nothing
    | link(f, rest) =>
      cases(Option) lists.find(lam(b): b.name == f.name end, rest):
        | some(found) => add-error(C.duplicate-variant(f.name, found.l, f.l))
        | none => ensure-unique-variant-ids(rest)
      end
  end
end


fun wf-last-stmt(stmt :: A.Expr):
  cases(A.Expr) stmt:
    | s-let(l, _, _, _) => wf-error("Cannot end a block in a let-binding", l)
    | s-var(l, _, _) => wf-error("Cannot end a block in a var-binding", l)
    | s-rec(l, _, _) => wf-error("Cannot end a block in a rec-binding", l)
    | s-fun(l, _, _, _, _, _, _, _, _) => wf-error("Cannot end a block in a fun-binding", l)
    | s-data(l, _, _, _, _, _, _) => wf-error("Cannot end a block with a data definition", l)
    | s-contract(l, _, _) => add-error(C.block-ending(l, "contract"))
    | else => nothing
  end
end

fun fields-to-binds(members :: List<A.Member>) -> List<A.Bind>:
  for map(mem from members):
    A.s-bind(mem.l, false, A.s-name(mem.l, mem.name), A.a-blank)
  end
end

fun opname(op): string-substring(op, 2, string-length(op)) end
fun reachable-ops(self, l, op-l, op, ast):
  cases(A.Expr) ast block:
    | s-op(l2, op-l2, op2, left2, right2) =>
      if (op == op2) block:
        reachable-ops(self, l, op-l, op, left2)
        reachable-ops(self, l, op-l, op, right2)
      else:
        add-error(C.mixed-binops(opname(op), op-l,  opname(op2), op-l2))
      end
      true
    | else => ast.visit(self)
  end
end

fun wf-block-stmts(visitor, l, stmts :: List%(is-link)) block:
  bind-stmts = stmts.filter(lam(s): A.is-s-var(s) or A.is-s-let(s) or A.is-s-rec(s) end).map(_.name)
  ensure-unique-bindings(bind-stmts.reverse())
  ensure-distinct-lines(A.dummy-loc, false, stmts)
  lists.all(_.visit(visitor), stmts)
end

fun wf-examples-body(visitor, body):
  for lists.all(b from body.stmts):
    if not(A.is-s-check-test(b)) block:
      add-error(C.non-example(b))
      false
    else:
      true
    end
  end
end


fun is-underscore(e):
  A.is-s-id(e) and A.is-s-underscore(e.id)
end

var last-visited-loc = nothing

well-formed-visitor = A.default-iter-visitor.{
  method s-program(self, l, _provide, _provide-types, imports, body):
    raise("Impossible")
  end,
  method s-special-import(self, l, kind, args) block:
    last-visited-loc := l
    if kind == "my-gdrive":
      if args.length() <> 1 block:
        wf-error("Imports with my-gdrive should have one argument, the name of the file", l)
        false
      else:
        true
      end
    else if kind == "shared-gdrive":
      if args.length() <> 2 block:
        wf-error("Imports with shared-gdrive should have two arguments, the name of the file and the file's id, which you can get from the share URL", l)
        false
      else:
        true
      end
    else if kind == "js-http":
      true
    else if kind == "gdrive-js":
      if args.length() <> 2:
        wf-error("Imports with gdrive-js should have two arguments, the name of the file and the file's id", l)
      else:
        true
      end
    else if kind == "gdrive-js":
      when args.length() <> 2:
        wf-error("Imports with gdrive-js should have two arguments, the name of the file and the file's id", l)
      end
    else:
      true
      #wf-error("Unsupported import type " + kind + ".  Did you mean my-gdrive or shared-gdrive?", l)
    end
  end,
  method s-data(self, l, name, params, mixins, variants, shares, _check) block:
    last-visited-loc := l
    add-error(C.non-toplevel("data declaration", l))
    true
  end,
  method s-data-expr(self, l, name, namet, params, mixins, variants, shared, _check) block:
    last-visited-loc := l
    add-error(C.non-toplevel("data declaration", l))
    true
  end,
  method s-type(self, l, name, ann) block:
    last-visited-loc := l
    add-error(C.non-toplevel("type alias", l))
    true
  end,
  method s-newtype(self, l, name, namet) block:
    last-visited-loc := l
    add-error(C.non-toplevel("newtype", l))
    true
  end,
  method s-let-expr(self, l, binds, body, blocky) block:
    last-visited-loc := l
    when not(blocky): 
      wf-blocky-blocks(l, [list: body])
    end
    lists.all(_.visit(self), binds) and body.visit(self)
  end,
  method s-letrec(self, l, binds, body, blocky) block:
    last-visited-loc := l
    when not(blocky): 
      wf-blocky-blocks(l, [list: body])
    end
    lists.all(_.visit(self), binds) and body.visit(self)
  end,
  method s-type-let-expr(self, l, binds, body, blocky) block:
    last-visited-loc := l
    add-error(C.non-toplevel("type alias", l))
    true
  end,
  method s-op(self, l, op-l, op, left, right) block:
    last-visited-loc := l
    reachable-ops(self, l, op-l, op, left) and reachable-ops(self, l, op-l, op, right)
  end,
  method s-cases-branch(self, l, pat-loc, name, args, body) block:
    last-visited-loc := l
    when (name == "_"):
      add-error(C.underscore-as-pattern(pat-loc))
    end
    ensure-unique-ids(args.map(_.bind))
    lists.all(_.visit(self), args) and body.visit(self)
  end,
  method s-singleton-cases-branch(self, l, pat-loc, name, body) block:
    last-visited-loc := l
    when (name == "_"):
      add-error(C.underscore-as-pattern(pat-loc))
    end
    body.visit(self)
  end,
  method s-var(self, l, bind, val) block:
    last-visited-loc := l
    when A.is-s-underscore(bind.id):
      add-error(C.pointless-var(l.at-start() + bind.l))
    end
    bind.visit(self) and val.visit(self)
  end,
  method s-rec(self, l, bind, val) block:
    last-visited-loc := l
    when A.is-s-underscore(bind.id):
      add-error(C.pointless-rec(l.at-start() + bind.l))
    end
    bind.visit(self) and val.visit(self)
  end,
  method s-var-bind(self, l, bind, val) block:
    last-visited-loc := l
    when A.is-s-underscore(bind.id):
      add-error(C.pointless-var(l.at-start() + bind.l))
    end
    bind.visit(self) and val.visit(self)
  end,
  method s-block(self, l, stmts):
    if is-empty(stmts) block:
      add-error(C.wf-empty-block(last-visited-loc))
      true
    else:
      wf-last-stmt(stmts.last())
      wf-block-stmts(self, l, stmts)
      true
    end
  end,
  method s-bind(self, l, shadows, name, ann) block:
    last-visited-loc := l
    when (reserved-names.has-key(name.tosourcestring())):
      reserved-name(l, name.tosourcestring())
    end
    when shadows and A.is-s-underscore(name):
      add-error(C.pointless-shadow(l))
    end
    name.visit(self) and ann.visit(self)
  end,
  method s-check-test(self, l, op, refinement, left, right) block:
    last-visited-loc := l
    when not(in-check-block):
      op-name = op.tosource().pretty(80).join-str("\n")
      wf-error("Cannot use `" + op-name + "` outside of a `check` or `where` block", l)
    end
    when is-some(refinement):
      cases(A.CheckOp) op:
        | s-op-is(_)            => nothing
        | s-op-is-not(_)        => nothing
        | s-op-satisfies(_)     =>
          wf-error("Cannot use refinement syntax `%(...)` with `satisfies`. "
              + "Consider changing the predicate instead.", l)
        | s-op-satisfies-not(_) =>
          wf-error("Cannot use refinement syntax `%(...)` with `violates`. "
              + "Consider changing the predicate instead.", l)
        | else               =>
          op-name = op.tosource().pretty(80).join-str("\n")
          wf-error("Cannot use refinement syntax `%(...)` with `" + op-name + "`.", l)
      end
    end
    left.visit(self) and self.option(right)
  end,
  method s-method-field(self, l, name, params, args, ann, doc, body, _check, blocky) block:
    last-visited-loc := l
    when reserved-names.has-key(name):
      reserved-name(l, name)
    end
    when args.length() == 0:
      add-error(C.no-arguments(A.s-method-field(l, name, params, args, ann, doc, body, _check, blocky)))
    end
    ensure-unique-ids(args)
    cases(Option) _check:
      | none => nothing
      | some(chk) => ensure-empty-block(l, "methods", chk)
    end
    when not(blocky): 
      wf-blocky-blocks(l, [list: body])
    end
    lists.all(_.visit(self), args) and ann.visit(self) and body.visit(self) and wrap-visit-check(self, _check)
  end,
  method s-data-field(self, l, name, value) block:
    last-visited-loc := l
    when reserved-names.has-key(name):
      reserved-name(l, name)
    end
    value.visit(self)
  end,
  method s-mutable-field(self, l, name, ann, value) block:
    last-visited-loc := l
    when reserved-names.has-key(name):
      reserved-name(l, name)
    end
    ann.visit(self) and value.visit(self)
  end,
  method s-method(self, l, params, args, ann, doc, body, _check, blocky) block:
    last-visited-loc := l
    when args.length() == 0:
      add-error(C.no-arguments(A.s-method(l, params, args, ann, doc, body, _check, blocky)))
    end
    ensure-unique-ids(args)
    cases(Option) _check:
      | none => nothing
      | some(chk) => ensure-empty-block(l, "methods", chk)
    end
    when not(blocky): 
      wf-blocky-blocks(l, [list: body])
    end
    lists.all(_.visit(self), args) and ann.visit(self) and body.visit(self) and wrap-visit-check(self, _check)
  end,
  method s-lam(self, l, params, args, ann, doc, body, _check, blocky) block:
    last-visited-loc := l
    ensure-unique-ids(args)
    cases(Option) _check:
      | none => nothing
      | some(chk) => ensure-empty-block(l, "anonymous functions", chk)
    end
    when not(blocky): 
      wf-blocky-blocks(l, [list: body])
    end
    lists.all(_.visit(self), params)
    and lists.all(_.visit(self), args) and ann.visit(self) and body.visit(self) and wrap-visit-check(self, _check)
  end,
  method s-fun(self, l, name, params, args, ann, doc, body, _check, blocky) block:
    last-visited-loc := l
    when reserved-names.has-key(name):
      reserved-name(l, name)
    end
    when not(blocky): 
      wf-blocky-blocks(l, [list: body])
    end
    ensure-unique-ids(args)
    lists.all(_.visit(self), params)
    and lists.all(_.visit(self), args) and ann.visit(self) and body.visit(self) and wrap-visit-check(self, _check)
  end,
  method s-obj(self, l, fields) block:
    last-visited-loc := l
    ensure-unique-fields(fields.reverse())
    check-underscore-name(fields, "a field name")
    lists.all(_.visit(self), fields)
  end,
  method s-tuple-get(self, l, tup, index):
    if (index < 0) block: 
      wf-error(" Index too small  ", l)
      false
    else if (index > 1000):
      wf-error(" Index over maximum allowed index  ", l)
      false
    else:
      true
     end
  end,
  method s-check(self, l, name, body, keyword-check) block:
    last-visited-loc := l
    if not(keyword-check) block:
      wrap-visit-check(self, some(body))
      wf-examples-body(self, body)
    else:
      wrap-visit-check(self, some(body))
    end
  end,
  method s-when(self, l, test, block, blocky) block:
    last-visited-loc := l
    when not(blocky):
      wf-blocky-blocks(l, [list: block])
    end
    test.visit(self) and block.visit(self)
  end,
  method s-if(self, l, branches, blocky) block:
    last-visited-loc := l
    when branches.length() == 1:
      add-error(C.single-branch-if(A.s-if(l, branches, blocky)))
    end
    when not(blocky):
      wf-blocky-blocks(l, branches.map(_.body))
    end
    lists.all(_.visit(self), branches)
  end,
  method s-if-else(self, l, branches, _else, blocky) block:
    last-visited-loc := l
    when not(blocky):
      wf-blocky-blocks(l, link(_else, branches.map(_.body)))
    end
    lists.all(_.visit(self), branches) and _else.visit(self)
  end,
  method s-if-pipe(self, l :: Loc, branches :: List<A.IfPipeBranch>, blocky :: Boolean) block:
    last-visited-loc := l
    when not(blocky):
      wf-blocky-blocks(l, branches.map(_.body))
    end
    lists.all(_.visit(self), branches)
  end,
  method s-if-pipe-else(self, l :: Loc, branches :: List<A.IfPipeBranch>, _else :: A.Expr, blocky :: Boolean) block:
    last-visited-loc := l
    when not(blocky):
      wf-blocky-blocks(l, link(_else, branches.map(_.body)))
    end
    lists.all(_.visit(self), branches) and _else.visit(self)
  end,
  method s-cases(self, l, typ, val, branches, blocky) block:
    last-visited-loc := l
    ensure-unique-cases(branches)
    when not(blocky):
      wf-blocky-blocks(l, branches.map(_.body))
    end
    typ.visit(self) and val.visit(self) and lists.all(_.visit(self), branches)
  end,
  method s-cases-else(self, l, typ, val, branches, _else, blocky) block:
    last-visited-loc := l
    ensure-unique-cases(branches)
    when not(blocky):
      wf-blocky-blocks(l, link(_else, branches.map(_.body)))
    end
    typ.visit(self) and val.visit(self) and lists.all(_.visit(self), branches) and _else.visit(self)
  end,
  method s-for(self, l, iterator, bindings, ann, body, blocky) block:
    when not(blocky):
      wf-blocky-blocks(l, [list: body])
    end
    iterator.visit(self) and lists.all(_.visit(self), bindings) and ann.visit(self) and body.visit(self)
  end,
  method s-frac(self, l, num, den) block:
    last-visited-loc := l
    when den == 0:
      add-error(C.zero-fraction(l, num))
    end
    true
  end,
  method s-id(self, l, id) block:
    last-visited-loc := l
    when (reserved-names.has-key(id.tosourcestring())):
      reserved-name(l, id.tosourcestring())
    end
    true
  end,
  method s-provide(self, l, expr):
    true
  end,
  method a-name(self, l, id) block:
    when A.is-s-underscore(id):
      add-error(C.underscore-as-ann(l))
    end
    true
  end
}

top-level-visitor = A.default-iter-visitor.{
  method s-program(self, l, _provide, _provide-types, imports, body):
    ok-body = cases(A.Expr) body:
      | s-block(l2, stmts) => wf-block-stmts(self, l2, stmts)
      | else => body.visit(self)
    end
    ok-body and (_provide.visit(self)) and _provide-types.visit(self) and (lists.all(_.visit(self), imports))
  end,
  method s-type(self, l, name, ann):
    ann.visit(well-formed-visitor)
  end,
  method s-newtype(self, l, name, namet):
    true
  end,
  method s-type-let-expr(self, l, binds, body, blocky) block:
    when not(blocky): 
      wf-blocky-blocks(l, [list: body])
    end
    lists.all(_.visit(self), binds) and body.visit(well-formed-visitor)
  end,
  method s-type-bind(self, l, name, ann):
    ann.visit(well-formed-visitor)
  end,
  method s-newtype-bind(self, l, name, namet):
    true
  end,
  method s-variant(self, l, constr-loc, name, binds, with-members) block:
    ids = fields-to-binds(with-members) + binds.map(_.bind)
    ensure-unique-ids(ids)
    underscores = binds.filter(lam(b): A.is-s-underscore(b.bind.id) end)
    when not(is-empty(underscores)):
      add-error(C.underscore-as(underscores.first.l, "a data variant name"))
    end
    check-underscore-name(with-members, "a field name")
    is-empty(underscores) and
      lists.all(_.visit(well-formed-visitor), binds) and lists.all(_.visit(well-formed-visitor), with-members)
  end,
  method s-singleton-variant(self, l, name, with-members) block:
    ensure-unique-ids(fields-to-binds(with-members))
    lists.all(_.visit(well-formed-visitor), with-members)
  end,
  method s-data(self, l, name, params, mixins, variants, shares, _check) block:
    ensure-unique-variant-ids(variants)
    check-underscore-name(variants, "a data variant name")
    check-underscore-name(shares, "a shared field name")
    check-underscore-name([list: {l: l, name: name}], "a datatype name")
    the-cur-shared = cur-shared
    cur-shared := fields-to-binds(shares)
    params-v = lists.all(_.visit(well-formed-visitor), params)
    mixins-v = lists.all(_.visit(well-formed-visitor), mixins)
    variants-v = lists.all(_.visit(self), variants)
    shares-v = lists.all(_.visit(well-formed-visitor), shares)
    cur-shared := the-cur-shared
    params-v and mixins-v and variants-v and shares-v and wrap-visit-check(well-formed-visitor, _check)
  end,
  method s-data-expr(self, l, name, namet, params, mixins, variants, shared, _check) block:
    ensure-unique-variant-ids(variants)
    underscores = variants.filter(lam(v): v.name == "_" end)
    when not(is-empty(underscores)):
      add-error(C.underscore-as(underscores.first.l, "a data variant name"))
    end
    the-cur-shared = cur-shared
    cur-shared := fields-to-binds(shared)
    ret = lists.all(_.visit(well-formed-visitor), params)
    and lists.all(_.visit(well-formed-visitor), mixins)
    and lists.all(_.visit(well-formed-visitor), variants)
    and lists.all(_.visit(well-formed-visitor), shared)
    cur-shared := the-cur-shared
    is-empty(underscores) and
      ret and wrap-visit-check(well-formed-visitor, _check)
  end,


  # Everything else delegates to the non-toplevel visitor
  method s-import(_, l, import-type, name):
    well-formed-visitor.s-import(l, import-type, name)
  end,
  method s-include(_, l, import-type, name):
    well-formed-visitor.s-include(l, import-type)
  end,
  method s-import-types(_, l, import-type, name, types):
    well-formed-visitor.s-import-types(l, import-type, name, types)
  end,
  method s-import-fields(_, l, fields, import-type):
    well-formed-visitor.s-import-fields(l, fields, import-type)
  end,
  method s-provide(_, l, expr):
    well-formed-visitor.s-provide(l, expr)
  end,
  method s-provide-types(_, l, anns):
    well-formed-visitor.s-provide-types(l, anns)
  end,
  method s-bind(_, l, shadows, name, ann):
    well-formed-visitor.s-bind(l, shadows, name, ann)
  end,
  method s-var-bind(_, l, bind, expr):
    well-formed-visitor.s-var-bind(l, bind, expr)
  end,
  method s-let-bind(_, l, bind, expr):
    well-formed-visitor.s-let-bind(l, bind, expr)
  end,
  method s-template(self, l):
    well-formed-visitor.s-template(l)
  end,
  method s-let-expr(_, l, binds, body, blocky):
    well-formed-visitor.s-let-expr(l, binds, body, blocky)
  end,
  method s-letrec-bind(_, l, bind, expr):
    well-formed-visitor.s-letrec-bind(l, bind, expr)
  end,
  method s-letrec(_, l, binds, body, blocky):
    well-formed-visitor.s-letrec(l, binds, body, blocky)
  end,
  method s-hint-exp(_, l :: Loc, hints :: List<A.Hint>, exp :: A.Expr):
    well-formed-visitor.s-hint-exp(l, hints, exp)
  end,
  method s-instantiate(_, l :: Loc, expr :: A.Expr, params :: List<A.Ann>):
    well-formed-visitor.s-instantiate(l, expr, params)
  end,
  method s-block(_, l, stmts):
    well-formed-visitor.s-block(l, stmts)
  end,
  method s-user-block(_, l :: Loc, body :: A.Expr):
    well-formed-visitor.s-user-block(l, body)
  end,
  method s-fun(_, l, name, params, args, ann, doc, body, _check, blocky):
    well-formed-visitor.s-fun(l, name, params, args, ann, doc, body, _check, blocky)
  end,
  method s-var(_, l :: Loc, name :: A.Bind, value :: A.Expr):
    well-formed-visitor.s-var(l, name, value)
  end,
  method s-rec(_, l :: Loc, name :: A.Bind, value :: A.Expr):
    well-formed-visitor.s-rec(l, name, value)
  end,
  method s-let(_, l :: Loc, name :: A.Bind, value :: A.Expr, keyword-val :: Boolean):
    well-formed-visitor.s-let(l, name, value, keyword-val)
  end,
  method s-ref(_, l :: Loc, ann :: A.Ann):
    well-formed-visitor.s-ref(l, ann)
  end,
  method s-when(_, l :: Loc, test :: A.Expr, block :: A.Expr, blocky):
    well-formed-visitor.s-when(l, test, block, blocky)
  end,
  method s-contract(_, l :: Loc, name :: A.Name, ann :: A.Ann):
    well-formed-visitor.s-contract(l, name, ann)
  end,
  method s-assign(_, l :: Loc, id :: A.Name, value :: A.Expr):
    well-formed-visitor.s-assign(l, id, value)
  end,
  method s-if-branch(_, l :: Loc, test :: A.Expr, body :: A.Expr):
    well-formed-visitor.s-if-branch(l, test, body)
  end,
  method s-if-pipe-branch(_, l :: Loc, test :: A.Expr, body :: A.Expr):
    well-formed-visitor.s-if-pipe-branch(l, test, body)
  end,
  method s-if(_, l :: Loc, branches :: List<A.IfBranch>, blocky :: Boolean):
    well-formed-visitor.s-if(l, branches, blocky)
  end,
  method s-if-else(_, l :: Loc, branches :: List<A.IfBranch>, _else :: A.Expr, blocky :: Boolean):
    well-formed-visitor.s-if-else(l, branches, _else, blocky)
  end,
  method s-if-pipe(_, l :: Loc, branches :: List<A.IfPipeBranch>, blocky :: Boolean):
    well-formed-visitor.s-if-pipe(l, branches, blocky)
  end,
  method s-if-pipe-else(_, l :: Loc, branches :: List<A.IfPipeBranch>, _else :: A.Expr, blocky :: Boolean):
    well-formed-visitor.s-if-pipe-else(l, branches, _else, blocky)
  end,
  method s-cases-branch(_, l :: Loc, pat-loc :: Loc, name :: String, args :: List<A.CasesBind>, body :: A.Expr):
    well-formed-visitor.s-cases-branch(l, pat-loc, name, args, body)
  end,
  method s-singleton-cases-branch(_, l :: Loc, pat-loc :: Loc, name :: String, body :: A.Expr):
    well-formed-visitor.s-singleton-cases-branch(l, pat-loc, name, body)
  end,
  method s-cases(_, l :: Loc, typ :: A.Ann, val :: A.Expr, branches :: List<A.CasesBranch>, blocky :: Boolean):
    well-formed-visitor.s-cases(l, typ, val, branches, blocky)
  end,
  method s-cases-else(_, l :: Loc, typ :: A.Ann, val :: A.Expr, branches :: List<A.CasesBranch>, _else :: A.Expr, blocky :: Boolean):
    well-formed-visitor.s-cases-else(l, typ, val, branches, _else, blocky)
  end,
  method s-op(_, l :: Loc, op-loc :: Loc, op :: String, left :: A.Expr, right :: A.Expr):
    well-formed-visitor.s-op(l, op-loc, op, left, right)
  end,
  method s-check-test(_, l :: Loc, op :: A.CheckOp, refinement :: Option<A.Expr>, left :: A.Expr, right :: Option<A.Expr>):
    well-formed-visitor.s-check-test(l, op, refinement, left, right)
  end,
  method s-paren(_, l :: Loc, expr :: A.Expr):
    well-formed-visitor.s-paren(l, expr)
  end,
  method s-lam(_, l :: Loc, params :: List<String>, args :: List<A.Bind>, ann :: A.Ann, doc :: String, body :: A.Expr, _check :: Option<A.Expr>, blocky):
    well-formed-visitor.s-lam(l, params, args, ann, doc, body, _check, blocky)
  end,
  method s-method(_, l :: Loc, params :: List<A.Name>, args :: List<A.Bind>, ann :: A.Ann, doc :: String, body :: A.Expr, _check :: Option<A.Expr>, blocky):
    well-formed-visitor.s-method(l, params, args, ann, doc, body, _check, blocky)
  end,
  method s-extend(_, l :: Loc, supe :: A.Expr, fields :: List<A.Member>):
    well-formed-visitor.s-extend(l, supe, fields)
  end,
  method s-update(_, l :: Loc, supe :: A.Expr, fields :: List<A.Member>):
    well-formed-visitor.s-update(l, supe, fields)
  end,
  method s-tuple-get(_, l :: Loc, tup, index):
    well-formed-visitor.s-tuple-get(l, tup, index)
  end,
  method s-obj(_, l :: Loc, fields :: List<A.Member>):
    well-formed-visitor.s-obj(l, fields)
  end,
  method s-array(_, l :: Loc, values :: List<A.Expr>):
    well-formed-visitor.s-array(l, values)
  end,
  method s-construct(_, l :: Loc, mod :: A.ConstructModifier, constructor :: A.Expr, values :: List<A.Expr>):
    well-formed-visitor.s-construct(l, mod, constructor, values)
  end,
  method s-app(_, l :: Loc, _fun :: A.Expr, args :: List<A.Expr>):
    well-formed-visitor.s-app(l, _fun, args)
  end,
  method s-prim-app(_, l :: Loc, _fun :: String, args :: List<A.Expr>):
    well-formed-visitor.s-prim-app(l, _fun, args)
  end,
  method s-frac(_, l :: Loc, num, den):
    well-formed-visitor.s-frac(l, num, den)
  end,
  method s-id(_, l :: Loc, id :: A.Name):
    well-formed-visitor.s-id(l, id)
  end,
  method s-id-var(_, l :: Loc, id :: A.Name):
    well-formed-visitor.s-id-var(l, id)
  end,
  method s-id-letrec(_, l :: Loc, id :: A.Name, safe :: Boolean):
    well-formed-visitor.s-id-letrec(l, id, safe)
  end,
  method s-dot(_, l :: Loc, obj :: A.Expr, field :: String):
    well-formed-visitor.s-dot(l, obj, field)
  end,
  method s-get-bang(_, l :: Loc, obj :: A.Expr, field :: String):
    well-formed-visitor.s-get-bang(l, obj, field)
  end,
  method s-bracket(_, l :: Loc, obj :: A.Expr, field :: A.Expr):
    well-formed-visitor.s-bracket(l, obj, field)
  end,
  method s-for(_, l :: Loc, iterator :: A.Expr, bindings :: List<A.ForBind>, ann :: A.Ann, body :: A.Expr, blocky :: Boolean):
    well-formed-visitor.s-for(l, iterator, bindings, ann, body, blocky)
  end,
  method s-check(_, l :: Loc, name :: Option<String>, body :: A.Expr, keyword-check :: Boolean):
    well-formed-visitor.s-check(l, name, body, keyword-check)
  end,
  method s-data-field(_, l :: Loc, name :: A.Expr, value :: A.Expr):
    well-formed-visitor.s-data-field(l, name, value)
  end,
  method s-mutable-field(_, l :: Loc, name :: A.Expr, ann :: A.Ann, value :: A.Expr):
    well-formed-visitor.s-mutable-field(l, name, ann, value)
  end,
  method s-method-field(_, l :: Loc, name :: A.Expr, params :: List<A.Name>, args :: List<A.Bind>, ann :: A.Ann, doc :: String, body :: A.Expr, _check :: Option<A.Expr>, blocky):
    well-formed-visitor.s-method-field(l, name, params, args, ann, doc, body, _check, blocky)
  end,
  method s-for-bind(_, l :: Loc, bind :: A.Bind, value :: A.Expr):
    well-formed-visitor.s-for-bind(l, bind, value)
  end,
  method s-variant-member(_, l :: Loc, member-type :: A.VariantMemberType, bind :: A.Bind):
    well-formed-visitor.s-variant-member(l, member-type, bind)
  end,
  method a-arrow(_, l, args, ret, use-parens):
    well-formed-visitor.a-arrow(l, args, ret, use-parens)
  end,
  method a-method(_, l, args, ret):
    well-formed-visitor.a-method(l, args, ret)
  end,
  method a-record(_, l, fields):
    well-formed-visitor.a-record(l, fields)
  end,
  method a-app(_, l, ann, args):
    well-formed-visitor.a-app(l, ann, args)
  end,
  method a-pred(_, l, ann, exp):
    well-formed-visitor.a-pred(l, ann, exp)
  end,
  method a-dot(_, l, obj, field):
    well-formed-visitor.a-dot(l, obj, field)
  end,
  method a-field(_, l, name, ann):
    well-formed-visitor.a-field(l, name, ann)
  end
}

fun check-well-formed(ast) -> C.CompileResult<A.Program, Any> block:
  cur-shared := empty
  errors := empty
  in-check-block := false
  ans =
    if ast.visit(top-level-visitor) and (errors.length() == 0): C.ok(ast)
    else: C.err(errors.reverse())
    end
  # cleanup
  cur-shared := empty
  errors := empty
  in-check-block := false
  ans
end
