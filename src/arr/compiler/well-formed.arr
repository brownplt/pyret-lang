#lang pyret

provide {
  check-well-formed: check-well-formed
} end
provide-types *

import ast as A
import srcloc as SL
import "compiler/compile-structs.arr" as C
import format as F

type Loc = SL.Srcloc

# TODO: Make this a mutable field when we have them...
var errors = empty
var in-check-block = false
var cur-shared = empty
var PARAM-current-where-everywhere = false # TODO: What does this mean? (used by ensure-empty-block)

is-s-let = A.is-s-let # ANNOYING WORKAROUND

reserved-names = [list: 
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
  "with",
  "__proto__"
]


fun add-error(err):
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

fun wrap-visit-check(self, target):
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
        | s-cases-branch(l, pat-loc, name, args, body) =>
          cases(Option) lists.find(lam(b): b.name == name end, rest):
            | some(found) => wf-error2("Duplicate case for " + name, found.l, pat-loc)
            | none => nothing
          end
        | s-singleton-cases-branch(l, pat-loc, name, body) =>
          cases(Option) lists.find(lam(b): b.name == name end, rest):
            | some(found) => wf-error2("Duplicate case for " + name, found.l, pat-loc)
            | none => nothing
          end
      end
      ensure-unique-cases(rest)
  end
end

fun ensure-unique-ids(bindings :: List<A.Bind>):
  cases(List) bindings:
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
                  wf-error2("Found duplicate id " + tostring(id) + " in list of bindings", l, found.l)
                | none => nothing
              end
            | else =>
              elt = lists.find(lam(b): b.id == id end, rest)
              cases(Option) elt:
                | some(found) =>
                  wf-error2("Found duplicate id " + tostring(id) + " in list of bindings", l, found.l)
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
  cases(List) rev-bindings:
    | empty => nothing
    | link(f, rest) =>
      cases(A.Bind) f:
        | s-bind(l, shadows, id, ann) =>
          if A.is-s-underscore(id): nothing
          else if shadows: nothing
          else:
            cases(Option) lists.find(lam(b): b.id == id end, rest):
              | some(found) => duplicate-id(tostring(id), l, found.l)
              | none => nothing
            end
          end
      end
      ensure-unique-bindings(rest)
  end
end

fun ensure-unique-fields(rev-fields):
  cases(List) rev-fields:
    | empty => nothing
    | link(f, rest) =>
      cases(Option) lists.find(lam(f2): f2.name == f.name end, rest):
        | some(found) => add-error(C.duplicate-field(f.name, f.l, found.l))
        | none => nothing
      end
      ensure-unique-fields(rest)
  end
end

fun check-underscore-name(fields, kind-of-thing :: String) -> Boolean:
  underscores = fields.filter(lam(f): f.name == "_" end)
  when not(is-empty(underscores)):
    wf-error("Cannot use underscore as a " + kind-of-thing, underscores.first.l)
  end
  is-empty(underscores)
end

fun ensure-distinct-lines(loc :: Loc, stmts :: List<A.Expr>):
  cases(List) stmts:
    | empty => nothing
    | link(first, rest) =>
      cases(Loc) loc:
        | builtin(_) => ensure-distinct-lines(first.l, rest)
        | srcloc(_, _, _, _, end-line1, _, _) =>
          cases(Loc) first.l:
            | builtin(_) => ensure-distinct-lines(loc, rest) # No need to preserve builtin() locs
            | srcloc(_, start-line2, _, _, _, _, _) =>
              when end-line1 == start-line2:
                wf-error2("Found two expressions on the same line", loc, first.l)
              end
              ensure-distinct-lines(first.l, rest)
          end
      end
  end
end

fun ensure-unique-variant-ids(variants :: List): # A.DatatypeVariant or A.Variant
  cases(List) variants:
    | empty => nothing
    | link(f, rest) =>
      cases(Option) lists.find(lam(b): b.name == f.name end, rest):
        | some(found) => wf-error2("Found duplicate id " + f.name + " in list of bindings", f.l, found.l)
        | none => ensure-unique-variant-ids(rest)
      end
  end
end


fun wf-last-stmt(stmt :: A.Expr):
  cases(A.Expr) stmt:
    | s-let(l, _, _, _) => wf-error("Cannot end a block in a let-binding", l)
    | s-var(l, _, _) => wf-error("Cannot end a block in a var-binding", l)
    | s-rec(l, _, _) => wf-error("Cannot end a block in a rec-binding", l)
    | s-fun(l, _, _, _, _, _, _, _) => wf-error("Cannot end a block in a fun-binding", l)
    | s-data(l, _, _, _, _, _, _) => wf-error("Cannot end a block with a data definition", l)
    | s-datatype(l, _, _, _, _) => wf-error("Cannot end a block with a datatype definition", l)
    | s-graph(l, _) => wf-error("Cannot end a block with a graph definition", l)
    | s-m-graph(l, _) => wf-error("Cannot end a block with a graph definition", l)
    | else => nothing
  end
end

fun fields-to-binds(members :: List<A.Member>) -> List<A.Bind>:
  for map(mem from members):
    A.s-bind(mem.l, false, A.s-name(mem.l, mem.name), A.a-blank)
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

fun wf-block-stmts(visitor, l, stmts :: List%(is-link)):
  bind-stmts = stmts.filter(lam(s): A.is-s-var(s) or A.is-s-let(s) or A.is-s-rec(s) end).map(_.name)
  ensure-unique-bindings(bind-stmts.reverse())
  ensure-distinct-lines(A.dummy-loc, stmts)
  lists.all(_.visit(visitor), stmts)
end


fun is-underscore(e):
  A.is-s-id(e) and A.is-s-underscore(e.id)
end

well-formed-visitor = A.default-iter-visitor.{
  s-program(self, l, _provide, _provide-types, imports, body):
    raise("Impossible")
  end,
  s-special-import(self, l, kind, args):
    if kind == "my-gdrive":
      when args.length() <> 1:
        wf-error("Imports with my-gdrive should have one argument, the name of the file", l)
      end
    else if kind == "shared-gdrive":
      when args.length() <> 2:
        wf-error("Imports with shared-gdrive should have two arguments, the name of the file and the file's id, which you can get from the share URL", l)
      end
    else:
      wf-error("Unsupported import type " + kind + ".  Did you mean my-gdrive or shared-gdrive?", l)
    end
    true
  end,
  s-data(self, l, name, params, mixins, variants, shares, _check):
    wf-error("Cannot define a data expression except at the top level of a file", l)
    false
  end,
  s-data-expr(self, l, name, namet, params, mixins, variants, shared, _check):
    wf-error("Cannot define a data expression except at the top level of a file", l)
    false
  end,
  s-type(self, l, name, ann):
    wf-error("Cannot define a type alias except at the top level of a file", l)
    false
  end,
  s-newtype(self, l, name, namet):
    wf-error("Cannot define a newtype except at the top level of a file", l)
    false
  end,
  s-type-let-expr(self, l, binds, body):
    wf-error("Cannot define newtypes or type aliases except at the top level of a file", l)
    false
  end,
  s-op(self, l, op, left, right):
    reachable-ops(self, l, op, left) and reachable-ops(self, l, op, right)
  end,
  s-cases-branch(self, l, pat-loc, name, args, body):
    when (name == "_"):
      wf-error("Found a cases branch using _ rather than a constructor name; use 'else' instead", pat-loc)
    end
    ensure-unique-ids(args.map(_.bind))
    lists.all(_.visit(self), args) and body.visit(self)
  end,
  s-singleton-cases-branch(self, l, pat-loc, name, body):
    when (name == "_"):
      wf-error("Found a cases branch using _ rather than a constructor name; use 'else' instead", pat-loc)
    end
    body.visit(self)
  end,
  s-var(self, l, bind, val):
    when A.is-s-underscore(bind.id):
      add-error(C.pointless-var(l.at-start() + bind.l))
    end
    bind.visit(self) and val.visit(self)
  end,
  s-rec(self, l, bind, val):
    when A.is-s-underscore(bind.id):
      add-error(C.pointless-rec(l.at-start() + bind.l))
    end
    bind.visit(self) and val.visit(self)
  end,
  s-var-bind(self, l, bind, val):
    when A.is-s-underscore(bind.id):
      add-error(C.pointless-var(l.at-start() + bind.l))
    end
    bind.visit(self) and val.visit(self)
  end,
  s-block(self, l, stmts):
    if is-empty(stmts):
      wf-error("Empty block", l)
      true
    else:
      wf-last-stmt(stmts.last())
      wf-block-stmts(self, l, stmts)
    end
  end,
  s-bind(self, l, shadows, name, ann):
    when (reserved-names.member(tostring(name))):
      reserved-name(l, tostring(name))
    end
    when shadows and A.is-s-underscore(name):
      add-error(C.pointless-shadow(l))
    end
    name.visit(self) and ann.visit(self)
  end,
  s-check-test(self, l, op, refinement, left, right):
    when not(in-check-block):
      op-name = op.tosource().pretty(80).join-str("\n")
      wf-error("Cannot use `" + op-name + "` outside of a `check` or `where` block", l)
    end
    when is-some(refinement):
      cases(A.CheckOp) op:
        | s-op-is            => nothing
        | s-op-is-not        => nothing
        | s-op-satisfies     =>
          wf-error("Cannot use refinement syntax `%(...)` with `satisfies`. "
              + "Consider changing the predicate instead.", l)
        | s-op-satisfies-not =>
          wf-error("Cannot use refinement syntax `%(...)` with `violates`. "
              + "Consider changing the predicate instead.", l)
        | else               =>
          op-name = op.tosource().pretty(80).join-str("\n")
          wf-error("Cannot use refinement syntax `%(...)` with `" + op-name + "`.", l)
      end
    end
    left.visit(self) and self.option(right)
  end,
  s-method-field(self, l, name, params, args, ann, doc, body, _check):
    when reserved-names.member(name):
      reserved-name(l, name)
    end
    when args.length() == 0:
      wf-error("Cannot have a method with zero arguments", l)
    end
    ensure-unique-ids(args)
    cases(Option) _check:
      | none => nothing
      | some(chk) => ensure-empty-block(l, "methods", chk)
    end
    lists.all(_.visit(self), args) and ann.visit(self) and body.visit(self) and wrap-visit-check(self, _check)
  end,
  s-data-field(self, l, name, value):
    when reserved-names.member(name):
      reserved-name(l, name)
    end
    value.visit(self)
  end,
  s-mutable-field(self, l, name, ann, value):
    when reserved-names.member(name):
      reserved-name(l, name)
    end
    ann.visit(self) and value.visit(self)
  end,
  s-method(self, l, params, args, ann, doc, body, _check):
    when args.length() == 0:
      wf-error("Cannot have a method with zero arguments", l)
    end
    ensure-unique-ids(args)
    cases(Option) _check:
      | none => nothing
      | some(chk) => ensure-empty-block(l, "methods", chk)
    end
    lists.all(_.visit(self), args) and ann.visit(self) and body.visit(self) and wrap-visit-check(self, _check)
  end,
  s-lam(self, l, params, args, ann, doc, body, _check):
    ensure-unique-ids(args)
    cases(Option) _check:
      | none => nothing
      | some(chk) => ensure-empty-block(l, "anonymous functions", chk)
    end
    lists.all(_.visit(self), params)
    and lists.all(_.visit(self), args) and ann.visit(self) and body.visit(self) and wrap-visit-check(self, _check)
  end,
  s-fun(self, l, name, params, args, ann, doc, body, _check):
    when reserved-names.member(name):
      reserved-name(l, name)
    end
    ensure-unique-ids(args)
    lists.all(_.visit(self), params)
    and lists.all(_.visit(self), args) and ann.visit(self) and body.visit(self) and wrap-visit-check(self, _check)
  end,
  s-obj(self, l, fields):
    ensure-unique-fields(fields.reverse())
    check-underscore-name(fields, "field name")
    lists.all(_.visit(self), fields)
  end,
  s-m-graph(self, l, bindings):
    for each(binding from bindings):
      when A.is-s-underscore(binding.name.id):
        add-error(C.pointless-graph-id(binding.l))
      end
    end
    lists.all(_.visit(self), bindings)
  end,
  s-graph(self, l, bindings):
    add-error(C.wf-error("graph expressions are not yet supported", l))
    false
  end,
  s-check(self, l, name, body, keyword-check):
    wrap-visit-check(self, some(body))
  end,
  s-if(self, l, branches):
    when branches.length() == 1:
      wf-error("Cannot have an `if` with a single branch", l)
    end
    lists.all(_.visit(self), branches)
  end,
  s-cases(self, l, typ, val, branches):
    ensure-unique-cases(branches)
    typ.visit(self) and val.visit(self) and lists.all(_.visit(self), branches)
  end,
  s-cases-else(self, l, typ, val, branches, _else):
    ensure-unique-cases(branches)
    typ.visit(self) and val.visit(self) and lists.all(_.visit(self), branches) and _else.visit(self)
  end,
  s-frac(self, l, num, den):
    when den == 0:
      add-error(C.zero-fraction(l, num))
    end
    true
  end,
  s-id(self, l, id):
    when (reserved-names.member(tostring(id))):
      reserved-name(l, tostring(id))
    end
    true
  end,
  s-provide(self, l, expr):
    true
  end
}

top-level-visitor = A.default-iter-visitor.{
  s-program(self, l, _provide, _provide-types, imports, body):
    ok-body = cases(A.Expr) body:
      | s-block(l2, stmts) => wf-block-stmts(self, l2, stmts)
      | else => body.visit(self)
    end
    ok-body and (_provide.visit(self)) and _provide-types.visit(self) and (lists.all(_.visit(self), imports))
  end,
  s-type(self, l, name, ann):
    ann.visit(well-formed-visitor)
  end,
  s-newtype(self, l, name, namet):
    true
  end,
  s-type-let-expr(self, l, binds, body):
    lists.all(_.visit(self), binds) and body.visit(well-formed-visitor)
  end,
  s-type-bind(self, l, name, ann):
    ann.visit(well-formed-visitor)
  end,
  s-newtype-bind(self, l, name, namet):
    true
  end,
  s-variant(self, l, constr-loc, name, binds, with-members):
    ids = fields-to-binds(with-members) + binds.map(_.bind)
    ensure-unique-ids(ids)
    underscores = binds.filter(lam(b): A.is-s-underscore(b.bind.id) end)
    when not(is-empty(underscores)):
      wf-error("Cannot use underscore as a field name in data variant ", underscores.first.l)
    end
    check-underscore-name(with-members, "field name")
    is-empty(underscores) and
      lists.all(_.visit(well-formed-visitor), binds) and lists.all(_.visit(well-formed-visitor), with-members)
  end,
  s-singleton-variant(self, l, name, with-members):
    ensure-unique-ids(fields-to-binds(with-members))
    lists.all(_.visit(well-formed-visitor), with-members)
  end,
  s-data(self, l, name, params, mixins, variants, shares, _check):
    ensure-unique-variant-ids(variants)
    check-underscore-name(variants, "data variant name")
    check-underscore-name(shares, "shared field name")
    check-underscore-name([list: {l: l, name: name}], "datatype name")
    the-cur-shared = cur-shared
    cur-shared := fields-to-binds(shares)
    params-v = lists.all(_.visit(well-formed-visitor), params)
    mixins-v = lists.all(_.visit(well-formed-visitor), mixins)
    variants-v = lists.all(_.visit(self), variants)
    shares-v = lists.all(_.visit(well-formed-visitor), shares)
    cur-shared := the-cur-shared
    params-v and mixins-v and variants-v and shares-v and wrap-visit-check(well-formed-visitor, _check)
  end,
  s-datatype-variant(self, l, name, binds, constructor):
    ensure-unique-ids(fields-to-binds(binds))
    lists.all(_.visit(well-formed-visitor), binds) and constructor.visit(well-formed-visitor)
  end,
  s-data-expr(self, l, name, namet, params, mixins, variants, shared, _check):
    ensure-unique-variant-ids(variants)
    underscores = variants.filter(lam(v): v.name == "_" end)
    when not(is-empty(underscores)):
      wf-error("Cannot use underscore as a data variant name ", underscores.first.l)
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
  s-import(_, l, import-type, name):
    well-formed-visitor.s-import(l, import-type, name)
  end,
  s-import-types(_, l, import-type, name, types):
    well-formed-visitor.s-import-types(l, import-type, name, types)
  end,
  s-import-fields(_, l, fields, import-type):
    well-formed-visitor.s-import-fields(l, fields, import-type)
  end,
  s-provide(_, l, expr):
    well-formed-visitor.s-provide(l, expr)
  end,
  s-provide-types(_, l, anns):
    well-formed-visitor.s-provide-types(l, anns)
  end,
  s-bind(_, l, shadows, name, ann):
    well-formed-visitor.s-bind(l, shadows, name, ann)
  end,
  s-var-bind(_, l, bind, expr):
    well-formed-visitor.s-var-bind(l, bind, expr)
  end,
  s-let-bind(_, l, bind, expr):
    well-formed-visitor.s-let-bind(l, bind, expr)
  end,
  s-let-expr(_, l, binds, body):
    well-formed-visitor.s-let-expr(l, binds, body)
  end,
  s-letrec-bind(_, l, bind, expr):
    well-formed-visitor.s-letrec-bind(l, bind, expr)
  end,
  s-letrec(_, l, binds, body):
    well-formed-visitor.s-letrec(l, binds, body)
  end,
  s-hint-exp(_, l :: Loc, hints :: List<A.Hint>, exp :: A.Expr):
    well-formed-visitor.s-hint-exp(l, hints, exp)
  end,
  s-instantiate(_, l :: Loc, expr :: A.Expr, params :: List<A.Ann>):
    well-formed-visitor.s-instantiate(l, expr, params)
  end,
  s-block(_, l, stmts):
    well-formed-visitor.s-block(l, stmts)
  end,
  s-user-block(_, l :: Loc, body :: A.Expr):
    well-formed-visitor.s-user-block(l, body)
  end,
  s-fun(_, l, name, params, args, ann, doc, body, _check):
    well-formed-visitor.s-fun(l, name, params, args, ann, doc, body, _check)
  end,
  s-var(_, l :: Loc, name :: A.Bind, value :: A.Expr):
    well-formed-visitor.s-var(l, name, value)
  end,
  s-rec(_, l :: Loc, name :: A.Bind, value :: A.Expr):
    well-formed-visitor.s-rec(l, name, value)
  end,
  s-let(_, l :: Loc, name :: A.Bind, value :: A.Expr, keyword-val :: Boolean):
    well-formed-visitor.s-let(l, name, value, keyword-val)
  end,
  s-ref(_, l :: Loc, ann :: A.Ann):
    well-formed-visitor.s-ref(l, ann)
  end,
  s-graph(_, l :: Loc, bindings :: List<A.Expr%(is-s-let)>): # PROBLEM HERE
    well-formed-visitor.s-graph(l, bindings)
  end,
  s-when(_, l :: Loc, test :: A.Expr, block :: A.Expr):
    well-formed-visitor.s-when(l, test, block)
  end,
  s-contract(_, l :: Loc, name :: A.Name, ann :: A.Ann):
    well-formed-visitor.s-contract(l, name, ann)
  end,
  s-assign(_, l :: Loc, id :: A.Name, value :: A.Expr):
    well-formed-visitor.s-assign(l, id, value)
  end,
  s-if-branch(_, l :: Loc, test :: A.Expr, body :: A.Expr):
    well-formed-visitor.s-if-branch(l, test, body)
  end,
  s-if-pipe-branch(_, l :: Loc, test :: A.Expr, body :: A.Expr):
    well-formed-visitor.s-if-pipe-branch(l, test, body)
  end,
  s-if(_, l :: Loc, branches :: List<A.IfBranch>):
    well-formed-visitor.s-if(l, branches)
  end,
  s-if-else(_, l :: Loc, branches :: List<A.IfBranch>, _else :: A.Expr):
    well-formed-visitor.s-if-else(l, branches, _else)
  end,
  s-if-pipe(_, l :: Loc, branches :: List<A.IfPipeBranch>):
    well-formed-visitor.s-if-pipe(l, branches)
  end,
  s-if-pipe-else(_, l :: Loc, branches :: List<A.IfPipeBranch>, _else :: A.Expr):
    well-formed-visitor.s-if-pipe-else(l, branches, _else)
  end,
  s-cases-branch(_, l :: Loc, pat-loc :: Loc, name :: String, args :: List<A.CasesBind>, body :: A.Expr):
    well-formed-visitor.s-cases-branch(l, pat-loc, name, args, body)
  end,
  s-singleton-cases-branch(_, l :: Loc, pat-loc :: Loc, name :: String, body :: A.Expr):
    well-formed-visitor.s-singleton-cases-branch(l, pat-loc, name, body)
  end,
  s-cases(_, l :: Loc, typ :: A.Ann, val :: A.Expr, branches :: List<A.CasesBranch>):
    well-formed-visitor.s-cases(l, typ, val, branches)
  end,
  s-cases-else(_, l :: Loc, typ :: A.Ann, val :: A.Expr, branches :: List<A.CasesBranch>, _else :: A.Expr):
    well-formed-visitor.s-cases-else(l, typ, val, branches, _else)
  end,
  s-try(_, l :: Loc, body :: A.Expr, id :: A.Bind, _except :: A.Expr):
    well-formed-visitor.s-try(l, body, id, _except)
  end,
  s-op(_, l :: Loc, op :: String, left :: A.Expr, right :: A.Expr):
    well-formed-visitor.s-op(l, op, left, right)
  end,
  s-check-test(_, l :: Loc, op :: A.CheckOp, refinement :: Option<A.Expr>, left :: A.Expr, right :: Option<A.Expr>):
    well-formed-visitor.s-check-test(l, op, refinement, left, right)
  end,
  s-paren(_, l :: Loc, expr :: A.Expr):
    well-formed-visitor.s-paren(l, expr)
  end,
  s-lam(_, l :: Loc, params :: List<String>, args :: List<A.Bind>, ann :: A.Ann, doc :: String, body :: A.Expr, _check :: Option<A.Expr>):
    well-formed-visitor.s-lam(l, params, args, ann, doc, body, _check)
  end,
  s-method(_, l :: Loc, params :: List<A.Name>, args :: List<A.Bind>, ann :: A.Ann, doc :: String, body :: A.Expr, _check :: Option<A.Expr>):
    well-formed-visitor.s-method(l, params, args, ann, doc, body, _check)
  end,
  s-extend(_, l :: Loc, supe :: A.Expr, fields :: List<A.Member>):
    well-formed-visitor.s-extend(l, supe, fields)
  end,
  s-update(_, l :: Loc, supe :: A.Expr, fields :: List<A.Member>):
    well-formed-visitor.s-update(l, supe, fields)
  end,
  s-obj(_, l :: Loc, fields :: List<A.Member>):
    well-formed-visitor.s-obj(l, fields)
  end,
  s-array(_, l :: Loc, values :: List<A.Expr>):
    well-formed-visitor.s-array(l, values)
  end,
  s-construct(_, l :: Loc, mod :: A.ConstructModifier, constructor :: A.Expr, values :: List<A.Expr>):
    well-formed-visitor.s-construct(l, mod, constructor, values)
  end,
  s-app(_, l :: Loc, _fun :: A.Expr, args :: List<A.Expr>):
    well-formed-visitor.s-app(l, _fun, args)
  end,
  s-prim-app(_, l :: Loc, _fun :: String, args :: List<A.Expr>):
    well-formed-visitor.s-prim-app(l, _fun, args)
  end,
  s-frac(_, l :: Loc, num, den):
    well-formed-visitor.s-frac(l, num, den)
  end,
  s-id(_, l :: Loc, id :: A.Name):
    well-formed-visitor.s-id(l, id)
  end,
  s-id-var(_, l :: Loc, id :: A.Name):
    well-formed-visitor.s-id-var(l, id)
  end,
  s-id-letrec(_, l :: Loc, id :: A.Name, safe :: Boolean):
    well-formed-visitor.s-id-letrec(l, id, safe)
  end,
  s-dot(_, l :: Loc, obj :: A.Expr, field :: String):
    well-formed-visitor.s-dot(l, obj, field)
  end,
  s-get-bang(_, l :: Loc, obj :: A.Expr, field :: String):
    well-formed-visitor.s-get-bang(l, obj, field)
  end,
  s-bracket(_, l :: Loc, obj :: A.Expr, field :: A.Expr):
    well-formed-visitor.s-bracket(l, obj, field)
  end,
  s-for(_, l :: Loc, iterator :: A.Expr, bindings :: List<A.ForBind>, ann :: A.Ann, body :: A.Expr):
    well-formed-visitor.s-for(l, iterator, bindings, ann, body)
  end,
  s-check(_, l :: Loc, name :: Option<String>, body :: A.Expr, keyword-check :: Boolean):
    well-formed-visitor.s-check(l, name, body, keyword-check)
  end,
  s-data-field(_, l :: Loc, name :: A.Expr, value :: A.Expr):
    well-formed-visitor.s-data-field(l, name, value)
  end,
  s-mutable-field(_, l :: Loc, name :: A.Expr, ann :: A.Ann, value :: A.Expr):
    well-formed-visitor.s-mutable-field(l, name, ann, value)
  end,
  s-method-field(_, l :: Loc, name :: A.Expr, params :: List<A.Name>, args :: List<A.Bind>, ann :: A.Ann, doc :: String, body :: A.Expr, _check :: Option<A.Expr>):
    well-formed-visitor.s-method-field(l, name, params, args, ann, doc, body, _check)
  end,
  s-for-bind(_, l :: Loc, bind :: A.Bind, value :: A.Expr):
    well-formed-visitor.s-for-bind(l, bind, value)
  end,
  s-variant-member(_, l :: Loc, member-type :: A.VariantMemberType, bind :: A.Bind):
    well-formed-visitor.s-variant-member(l, member-type, bind)
  end,
  s-datatype-singleton-variant(_, l :: Loc, name :: String, constructor :: A.Constructor):
    well-formed-visitor.s-datatype-singleton-variant(l, name, constructor)
  end,
  s-datatype-constructor(_, l :: Loc, well-formed-visitor-arg :: String, body :: A.Expr):
    well-formed-visitor.s-datatype-constructor(l, well-formed-visitor-arg, body)
  end,
  a-arrow(_, l, args, ret, use-parens):
    well-formed-visitor.a-arrow(l, args, ret, use-parens)
  end,
  a-method(_, l, args, ret):
    well-formed-visitor.a-method(l, args, ret)
  end,
  a-record(_, l, fields):
    well-formed-visitor.a-record(l, fields)
  end,
  a-app(_, l, ann, args):
    well-formed-visitor.a-app(l, ann, args)
  end,
  a-pred(_, l, ann, exp):
    well-formed-visitor.a-pred(l, ann, exp)
  end,
  a-dot(_, l, obj, field):
    well-formed-visitor.a-dot(l, obj, field)
  end,
  a-field(_, l, name, ann):
    well-formed-visitor.a-field(l, name, ann)
  end
}

fun check-well-formed(ast) -> C.CompileResult<A.Program, Any>:
  cur-shared := empty
  errors := empty
  in-check-block := false
  ans =
    if ast.visit(top-level-visitor) and (errors.length() == 0): C.ok(ast)
    else: C.err(errors)
    end
  # cleanup
  cur-shared := empty
  errors := empty
  in-check-block := false
  ans
end
