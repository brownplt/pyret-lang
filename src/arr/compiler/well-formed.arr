provide {
  check-well-formed: check-well-formed
} end
provide-types *

import ast as A
import srcloc as SL
import error-display as ED
import file("compile-structs.arr") as C
import format as F
import string-dict as SD
import sets as S
import lists as L

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
  # TODO: refactor AST so this can be added
  # "table", true
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
    if is-empty(block.stmts): nothing
    else:
      add-error(C.unwelcome-where(tostring(typ), loc, block.l))
    end
  else:
    nothing
  end
end

fun is-block-allowed(expr):
  A.is-binder(expr) or A.is-s-spy-block(expr)
end

fun explicitly-blocky-block(block :: A.Expr % (is-s-block)) -> Boolean block:
  var seen-non-let = false
  var is-blocky = false
  var seen-template = false
  for each(expr from block.stmts):
    ask:
      | A.is-s-template(expr)        then: seen-template := true
      | seen-non-let                 then: is-blocky := true # even if expr is a binder, it's non-consecutive
      | not(is-block-allowed(expr))  then: seen-non-let := true
      | otherwise: nothing
    end
  end
  # any template presence overrules blockiness presence
  is-blocky and not(seen-template)
end

fun wf-blocky-blocks(l :: Loc, blocks :: List<A.Expr % (is-s-block)>):
  explicitly-blocky-blocks = blocks.filter(explicitly-blocky-block)
  when not(is-empty(explicitly-blocky-blocks)):
    add-error(C.block-needed(l, explicitly-blocky-blocks))
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

# This function checks that a set of bindings are distinct, regardless of shadowing
# (e.g. the parameters of a function should never have the same name)
fun ensure-unique-ids(bindings :: List<A.Bind>) block:
  ad = SD.make-mutable-string-dict()
  fun help(bind):
    cases(A.Bind) bind block:
      | s-bind(l, shadows, id, ann) =>
        cases(A.Name) id:
          | s-underscore(_) => nothing
          | else =>
            if ad.has-key-now(id.toname()):
              add-error(C.duplicate-id(id.tosourcestring(), l, ad.get-value-now(id.toname())))
            else:
              ad.set-now(id.toname(), l)
            end
        end
      | s-tuple-bind(l, fields, as-name) =>
        cases(Option) as-name:
          | none => nothing
          | some(n) => help(n)
        end
        fields.each(help)
    end
  end
  bindings.each(help)
end

# This function checks that the bindings within a block are either distinct
# or explicitly shadowed -- even before scope-resolution has a chance to kick in
fun ensure-unique-bindings(bindings :: List<A.Bind>) block:
  ad = SD.make-mutable-string-dict()
  fun help(bind):
    cases(A.Bind) bind block:
      | s-bind(l , shadows, id, ann) =>
        cases(A.Name) id:
          | s-underscore(_) => nothing
          | else =>
            if shadows: nothing
            else if ad.has-key-now(id.toname()):
              add-error(C.duplicate-id(id.tosourcestring(), l, ad.get-value-now(id.toname())))
            else:
              ad.set-now(id.toname(), l)
            end
        end
      | s-tuple-bind(l, fields, as-name) =>
        cases(Option) as-name:
          | none => nothing
          | some(n) => help(n)
        end
        fields.each(help)
    end
  end
  bindings.each(help)
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
                  add-error(C.template-same-line(loc, first.l))
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

fun ensure-unique-variant-ids(variants :: List<A.Variant>, name :: String, data-loc :: Loc):
  cases(List) variants block:
    | empty => true
    | link(f, rest) =>
      if f.name == name:
        add-error(C.data-variant-duplicate-name(f.name, f.l, data-loc))
      else if f.name == ("is-" + name):
        add-error(C.duplicate-is-data(name, f.l, data-loc))
      else if ("is-" + f.name) == name:
        add-error(C.duplicate-is-data-variant(f.name, data-loc, f.l))
      else:
        nothing
      end
      for each(b from rest):
        if b.name == f.name block:
          add-error(C.duplicate-variant(f.name, b.l, f.l))
        else if b.name == ("is-" + f.name):
          add-error(C.duplicate-is-variant(f.name, b.l, f.l))
        else if ("is-" + b.name) == f.name:
          add-error(C.duplicate-is-variant(b.name, f.l, b.l))
        else:
          nothing
        end
      end
      ensure-unique-variant-ids(rest, name, data-loc)
  end
end


fun wf-last-stmt(block-loc, stmt :: A.Expr):
  cases(A.Expr) stmt:
    | s-let(l, _, _, _)                   => add-error(C.block-ending(l, block-loc, "let-binding"))
    | s-var(l, _, _)                      => add-error(C.block-ending(l, block-loc, "var-binding"))
    | s-rec(l, _, _)                      => add-error(C.block-ending(l, block-loc, "rec-binding"))
    | s-fun(l, _, _, _, _, _, _, _, _, _) => add-error(C.block-ending(l, block-loc, "fun-binding"))
    | s-data(l, _, _, _, _, _, _, _)      => add-error(C.block-ending(l, block-loc, "data definition"))
    | s-contract(l, _, _, _)              => add-error(C.block-ending(l, block-loc, "contract"))
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
        add-error(C.mixed-binops(l, opname(op), op-l,  opname(op2), op-l2))
      end
      true
    | else => ast.visit(self)
  end
end

fun reject-standalone-exprs(stmts :: List, ignore-last :: Boolean) block:
  to-examine = if ignore-last:
    # Ignore the last statement, because it might well be an expression
    stmts.reverse().rest.reverse()
  else:
    stmts
  end
  fun bad-stmt(l, stmt):
    cases(A.Expr) stmt:
      | s-op(_, op-l, op, _, _) =>
        ask:
          | op == "op==" then:
            wf-error([list:
                [ED.para: ED.text("A standalone "),
                  ED.highlight(ED.code(ED.text("==")), [list: op-l], 1),
                  ED.text(" operator expression probably isn't intentional.")],
                if in-check-block:
                  [ED.para:
                    ED.text("To write an example or test case, use the "), ED.code(ED.text("is")), ED.text(" operator; "),
                    ED.text("to define a name, use the "), ED.code(ED.text("=")), ED.text(" operator instead.")]

                else:
                  [ED.para:
                    ED.text("To define a name, use the "), ED.code(ED.text("=")), ED.text(" operator instead.")]
                end
              ],
              l)
          | otherwise:
            wf-error([list:
                [ED.para: ED.text("A standalone "),
                  ED.highlight(ED.code(ED.text(string-substring(op, 2, string-length(op)))), [list: op-l], 1),
                  ED.text(" operator expression probably isn't intentional.")]], l)
        end
      | s-id(_, _) => wf-error([list: [ED.para: ED.text("A standalone variable name probably isn't intentional.")]], l)
      | s-num(_, _) => wf-error([list: [ED.para: ED.text("A standalone value probably isn't intentional.")]], l)
      | s-frac(_, _, _) => wf-error([list: [ED.para: ED.text("A standalone value probably isn't intentional.")]], l)
      | s-rfrac(_, _, _) => wf-error([list: [ED.para: ED.text("A standalone value probably isn't intentional.")]], l)
      | s-bool(_, _) => wf-error([list: [ED.para: ED.text("A standalone value probably isn't intentional.")]], l)
      | s-str(_, _) => wf-error([list: [ED.para: ED.text("A standalone value probably isn't intentional.")]], l)
      | s-dot(_, _, _) => wf-error([list: [ED.para: ED.text("A standalone field-lookup expression probably isn't intentional.")]], l)
      | s-lam(_, _, _, _, _, _, _, _, _, _) => wf-error([list: [ED.para: ED.text("A standalone anonymous function expression probably isn't intentional.")]], l)
      | s-paren(_, e) => bad-stmt(l, e)
      | else => nothing
    end
  end
  when not(stmts.any(A.is-s-template)): # Need to check all the statements for ...
    for each(stmt from to-examine): # but only check the non-final statements for standalone expressions
      bad-stmt(stmt.l, stmt)
    end
  end
  true
end

fun wrap-reject-standalones-in-check(target) block:
  cur-in-check = in-check-block
  in-check-block := true
  ret = cases(Option) target:
    | none => true
    | some(t) => reject-standalone-exprs(t.stmts, false)
  end
  in-check-block := cur-in-check
  ret
end


fun wf-block-stmts(visitor, l, stmts :: List%(is-link)) block:
  bind-stmts = stmts.filter(lam(s): A.is-s-var(s) or A.is-s-let(s) or A.is-s-rec(s) end).map(_.name)
  ensure-unique-bindings(bind-stmts)
  ensure-distinct-lines(A.dummy-loc, false, stmts)
  when not(in-check-block):
    reject-standalone-exprs(stmts, true)
  end
  lists.all(_.visit(visitor), stmts)
end

fun wf-examples-body(visitor, body):
  for lists.all(b from body.stmts):
    if not(A.is-s-check-test(b) or A.is-s-template(b)) block:
      add-error(C.non-example(b))
      true
    else:
      true
    end
  end
end

fun wf-table-headers(loc, headers) block:
  for lists.each(h from headers):
    when h.name == "_":
      add-error(C.underscore-as(h.l, "as a table column's name in a table expression"))
    end
  end
  cases(List) headers block:
    | empty =>
      add-error(C.table-empty-header(loc))
      true
    | link(first, rest) =>
      fun dups(shadow first, shadow rest) block:
        when (reserved-names.has-key(first.name)):
          reserved-name(first.l, first.name)
        end
        for each(hname from rest):
          when first.name == hname.name:
            add-error(C.table-duplicate-column-name(first, hname))
          end
        end
        cases(List) rest:
          | empty => nothing
          | link(snd, tail) => dups(snd, tail)
        end
      end
      dups(first, rest)
      true
  end
end


fun is-underscore(e):
  A.is-s-id(e) and A.is-s-underscore(e.id)
end

var parent-block-loc = nothing

well-formed-visitor = A.default-iter-visitor.{
  method s-program(self, l, _provide, _provide-types, imports, body):
    raise("Impossible")
  end,
  method s-special-import(self, l, kind, args) block:
    if kind == "my-gdrive":
      if not(is-link(args) and is-empty(args.rest)) block:
        add-error(C.import-arity-mismatch(l, kind, args, 2, [list: "the name of the file"]))
        false
      else:
        true
      end
    else if kind == "shared-gdrive":
      if not(is-link(args) and is-link(args.rest) and is-empty(args.rest.rest)) block:
        add-error(C.import-arity-mismatch(l, kind, args, 2, [list: "the name of the file", "the file's id, which you can get from the share URL"]))
        false
      else:
        true
      end
    else if kind == "js-http":
      true
    else if kind == "gdrive-js":
      if not(is-link(args) and is-link(args.rest) and is-empty(args.rest.rest)):
        add-error(C.import-arity-mismatch(l, kind, args, 2, [list: "the name of the file", "the file's id"]))
      else:
        true
      end
    else:
      true
      #wf-error("Unsupported import type " + kind + ".  Did you mean my-gdrive or shared-gdrive?", l)
    end
  end,
  method s-data(self, l, name, params, mixins, variants, shares, _check-loc, _check) block:
    add-error(C.non-toplevel("data declaration", l, parent-block-loc))
    true
  end,
  method s-data-expr(self, l, name, namet, params, mixins, variants, shared, _check-loc, _check) block:
    add-error(C.non-toplevel("data declaration", l, parent-block-loc))
    true
  end,
  method s-type(self, l, name, params, ann) block:
    add-error(C.non-toplevel("type alias", l, parent-block-loc))
    true
  end,
  method s-newtype(self, l, name, namet) block:
    add-error(C.non-toplevel("newtype", l, parent-block-loc))
    true
  end,
  method s-let-expr(self, l, binds, body, blocky) block:
    old-pbl = parent-block-loc
    parent-block-loc := l
    when not(blocky):
      wf-blocky-blocks(l, [list: body])
    end
    ans = lists.all(_.visit(self), binds) and body.visit(self)
    parent-block-loc := old-pbl
    ans
  end,
  method s-contract(_, l :: Loc, name :: A.Name, params :: List<A.Name>, ann :: A.Ann) block:
    add-error(C.non-toplevel("contract declaration", l, parent-block-loc))
    true
  end,
  method s-letrec-bind(self, l, bind, expr) block:
    old-pbl = parent-block-loc
    parent-block-loc := l
    cases(A.Bind) bind block:
      | s-bind(_,_,_,_) => nothing
      | s-tuple-bind(l2, _, _) =>
        wf-error([list: ED.text("Recursive bindings must be names and cannot be tuple bindings ")], l2)
        nothing
    end
    ans = bind.visit(self) and expr.visit(self)
    parent-block-loc := old-pbl
    ans
  end,
  method s-letrec(self, l, binds, body, blocky) block:
    old-pbl = parent-block-loc
    parent-block-loc := l
    when not(blocky):
      wf-blocky-blocks(l, [list: body])
    end
    ans = lists.all(_.visit(self), binds) and body.visit(self)
    parent-block-loc := old-pbl
    ans
  end,
  method s-type-let-expr(self, l, binds, body, blocky) block:
    add-error(C.non-toplevel("type alias", l, parent-block-loc))
    true
  end,
  method s-op(self, l, op-l, op, left, right) block:
    reachable-ops(self, l, op-l, op, left) and reachable-ops(self, l, op-l, op, right)
  end,
  method s-cases-branch(self, l, pat-loc, name, args, body) block:
    old-pbl = parent-block-loc
    parent-block-loc := l
    when (name == "_"):
      add-error(C.underscore-as-pattern(pat-loc))
    end
    ensure-unique-ids(args.map(_.bind))
    ans = lists.all(_.visit(self), args) and body.visit(self)
    parent-block-loc := old-pbl
    ans
  end,
  method s-singleton-cases-branch(self, l, pat-loc, name, body) block:
    old-pbl = parent-block-loc
    parent-block-loc := l
    when (name == "_"):
      add-error(C.underscore-as-pattern(pat-loc))
    end
    ans = body.visit(self)
    parent-block-loc := old-pbl
    ans
  end,
  method s-var(self, l, bind, val) block:
    cases(A.Bind) bind block:
      | s-bind(_,_,_,_) =>
        when A.is-s-underscore(bind.id):
          add-error(C.pointless-var(l.at-start() + bind.l))
        end
        bind.visit(self) and val.visit(self)
      | s-tuple-bind(l2, _, _) =>
        wf-error([list: ED.text("Variable bindings must be names and cannot be tuple bindings ")], l2)
        true
    end
  end,
  method s-rec(self, l, bind, val) block:
    cases(A.Bind) bind block:
      | s-bind(_,_,_,_) =>
        when A.is-s-underscore(bind.id):
          add-error(C.pointless-rec(l.at-start() + bind.l))
        end
        bind.visit(self) and val.visit(self)
      | s-tuple-bind(l2, _, _) =>
        wf-error([list: ED.text("Recursive bindings must be names and cannot be tuple bindings ")], l2)
        true
    end
  end,
  method s-var-bind(self, l, bind, val) block:
    cases(A.Bind) bind block:
      | s-bind(_,_,_,_) =>
        when A.is-s-underscore(bind.id):
          add-error(C.pointless-var(l.at-start() + bind.l))
        end
        bind.visit(self) and val.visit(self)
      | s-tuple-bind(l2, _, _) =>
        wf-error([list: ED.text("Variable bindings must be names and cannot be tuple bindings ")], l2)
        true
    end
  end,
  method s-block(self, l, stmts):
    if is-empty(stmts) block:
      add-error(C.wf-empty-block(parent-block-loc))
      true
    else:
      wf-last-stmt(parent-block-loc, stmts.last())
      wf-block-stmts(self, parent-block-loc, stmts)
      true
    end
  end,
  method s-user-block(self, l :: Loc, body :: A.Expr) block:
    old-pbl = parent-block-loc
    parent-block-loc := l
    ans = body.visit(self)
    parent-block-loc := old-pbl
    ans
  end,
  method s-tuple-bind(self, l, fields, as-name) block:
    true
  end,
  method s-bind(self, l, shadows, name, ann) block:
    when (reserved-names.has-key(name.tosourcestring())):
      reserved-name(l, name.tosourcestring())
    end
    when shadows and A.is-s-underscore(name):
      add-error(C.pointless-shadow(l))
    end
    name.visit(self) and ann.visit(self)
  end,
  method s-check-test(self, l, op, refinement, left, right, cause) block:
    when not(in-check-block):
      add-error(C.unwelcome-test(l))
    end
    when is-some(refinement):
      cases(A.CheckOp) op:
        | s-op-is(_)            => nothing
        | s-op-is-not(_)        => nothing
        | else                  =>
          add-error(C.unwelcome-test-refinement(refinement.value, op))
      end
    end
    left.visit(self) and self.option(right) and self.option(cause)
  end,
  method s-method-field(self, l, name, params, args, ann, doc, body, _check-loc, _check, blocky) block:
    old-pbl = parent-block-loc
    parent-block-loc := cases(Option) _check-loc:
      | none => l
      | some(cl) => l.upto-end(cl)
    end
    when reserved-names.has-key(name):
      reserved-name(l, name)
    end
    when is-empty(args):
      add-error(C.no-arguments(A.s-method-field(l, name, params, args, ann, doc, body, _check-loc, _check, blocky)))
    end
    ensure-unique-ids(args)
    cases(Option) _check:
      | none => nothing
      | some(chk) => ensure-empty-block(l, "methods", chk)
    end
    when not(blocky):
      wf-blocky-blocks(l, [list: body])
    end
    ans = lists.all(_.visit(self), args) and ann.visit(self) and body.visit(self)
    cases(Option) _check-loc:
      | none => nothing
      | some(cl) => parent-block-loc := cl.upto-end(l)
    end
    wrap-reject-standalones-in-check(_check)
    shadow ans = ans and wrap-visit-check(self, _check)
    parent-block-loc := old-pbl
    ans
  end,
  method s-data-field(self, l, name, value) block:
    when reserved-names.has-key(name):
      reserved-name(l, name)
    end
    value.visit(self)
  end,
  method s-mutable-field(self, l, name, ann, value) block:
    when reserved-names.has-key(name):
      reserved-name(l, name)
    end
    ann.visit(self) and value.visit(self)
  end,
  method s-method(self, l, name, params, args, ann, doc, body, _check-loc, _check, blocky) block:
    old-pbl = parent-block-loc
    parent-block-loc := cases(Option) _check-loc:
      | none => l
      | some(cl) => l.upto-end(cl)
    end
    when is-empty(args):
      add-error(C.no-arguments(A.s-method(l, name, params, args, ann, doc, body, _check-loc, _check, blocky)))
    end
    ensure-unique-ids(args)
    cases(Option) _check:
      | none => nothing
      | some(chk) => ensure-empty-block(l, "methods", chk)
    end
    when not(blocky):
      wf-blocky-blocks(l, [list: body])
    end
    ans = lists.all(_.visit(self), args) and ann.visit(self) and body.visit(self)
    cases(Option) _check-loc:
      | none => nothing
      | some(cl) => parent-block-loc := cl.upto-end(l)
    end
    wrap-reject-standalones-in-check(_check)
    shadow ans = ans and wrap-visit-check(self, _check)
    parent-block-loc := old-pbl
    ans
  end,
  method s-lam(self, l, name, params, args, ann, doc, body, _check-loc, _check, blocky) block:
    old-pbl = parent-block-loc
    parent-block-loc := cases(Option) _check-loc:
      | none => l
      | some(cl) => l.upto-end(cl)
    end
    ensure-unique-ids(args)
    cases(Option) _check:
      | none => nothing
      | some(chk) => ensure-empty-block(l, "anonymous functions", chk)
    end
    when not(blocky):
      wf-blocky-blocks(l, [list: body])
    end
    ans = lists.all(_.visit(self), params)
    and lists.all(_.visit(self), args) and ann.visit(self) and body.visit(self)
    cases(Option) _check-loc:
      | none => nothing
      | some(cl) => parent-block-loc := cl.upto-end(l)
    end
    wrap-reject-standalones-in-check(_check)
    shadow ans = ans and wrap-visit-check(self, _check)
    parent-block-loc := old-pbl
    ans
  end,
  method s-fun(self, l, name, params, args, ann, doc, body, _check-loc, _check, blocky) block:
    old-pbl = parent-block-loc
    parent-block-loc := cases(Option) _check-loc:
      | none => l
      | some(cl) => l.upto-end(cl)
    end
    when reserved-names.has-key(name):
      reserved-name(l, name)
    end
    when not(blocky):
      wf-blocky-blocks(l, [list: body])
    end
    ensure-unique-ids(args)
    ans = lists.all(_.visit(self), params)
      and lists.all(_.visit(self), args) and ann.visit(self) and body.visit(self)
    cases(Option) _check-loc:
      | none => nothing
      | some(cl) => parent-block-loc := cl.upto-end(l)
    end
    wrap-reject-standalones-in-check(_check)
    shadow ans = ans and wrap-visit-check(self, _check)
    parent-block-loc := old-pbl
    ans
  end,
  method s-obj(self, l, fields) block:
    ensure-unique-fields(fields.reverse())
    check-underscore-name(fields, "a field name")
    lists.all(_.visit(self), fields)
  end,
  method s-extend(self, l :: Loc, supe :: A.Expr, fields :: List<A.Member>) block:
    ensure-unique-fields(fields.reverse())
    check-underscore-name(fields, "a field name")
    lists.all(_.visit(self), fields)
  end,
  method s-dot(self, l :: Loc, obj :: A.Expr, field :: String) block:
    when field == "_":
      add-error(C.underscore-as(l, "a field name"))
    end
    obj.visit(self)
  end,
  method s-tuple-get(self, l, tup, index, index-loc):
    if not(num-is-integer(index)) or (index < 0) or (index > 1000) block:
      add-error(C.tuple-get-bad-index(l, tup, index, index-loc))
      true
    else:
      true
    end
  end,
  method s-check(self, l, name, body, keyword-check) block:
    old-pbl = parent-block-loc
    parent-block-loc := l
    ans = if not(keyword-check) block:
      wrap-visit-check(self, some(body))
      wf-examples-body(self, body)
    else:
      wrap-visit-check(self, some(body))
      wrap-reject-standalones-in-check(some(body))
    end
    parent-block-loc := old-pbl
    ans
  end,
  method s-when(self, l, test, block, blocky) block:
    old-pbl = parent-block-loc
    parent-block-loc := l
    when not(blocky):
      wf-blocky-blocks(l, [list: block])
    end
    ans = test.visit(self) and block.visit(self)
    parent-block-loc := old-pbl
    ans
  end,
  method s-if(self, l, branches, blocky) block:
    when is-link(branches) and is-empty(branches.rest):
      add-error(C.single-branch-if(A.s-if(l, branches, blocky)))
    end
    when not(blocky):
      wf-blocky-blocks(l, branches.map(_.body))
    end
    lists.all(_.visit(self), branches)
  end,
  method s-if-else(self, l, branches, _else, blocky) block:
    old-pbl = parent-block-loc
    parent-block-loc := l
    when not(blocky):
      wf-blocky-blocks(l, link(_else, branches.map(_.body)))
    end
    ans = lists.all(_.visit(self), branches) and _else.visit(self)
    parent-block-loc := old-pbl
    ans
  end,
  method s-if-pipe(self, l :: Loc, branches :: List<A.IfPipeBranch>, blocky :: Boolean) block:
    old-pbl = parent-block-loc
    parent-block-loc := l
    when not(blocky):
      wf-blocky-blocks(l, branches.map(_.body))
    end
    ans = lists.all(_.visit(self), branches)
    parent-block-loc := old-pbl
    ans
  end,
  method s-if-pipe-else(self, l :: Loc, branches :: List<A.IfPipeBranch>, _else :: A.Expr, blocky :: Boolean) block:
    old-pbl = parent-block-loc
    parent-block-loc := l
    when not(blocky):
      wf-blocky-blocks(l, link(_else, branches.map(_.body)))
    end
    ans = lists.all(_.visit(self), branches) and _else.visit(self)
    parent-block-loc := old-pbl
    ans
  end,
  method s-cases(self, l, typ, val, branches, blocky) block:
    old-pbl = parent-block-loc
    parent-block-loc := l
    ensure-unique-cases(branches)
    when not(blocky):
      wf-blocky-blocks(l, branches.map(_.body))
    end
    ans = typ.visit(self) and val.visit(self) and lists.all(_.visit(self), branches)
    parent-block-loc := old-pbl
    ans
  end,
  method s-cases-else(self, l, typ, val, branches, _else, blocky) block:
    old-pbl = parent-block-loc
    parent-block-loc := l
    ensure-unique-cases(branches)
    when not(blocky):
      wf-blocky-blocks(l, link(_else, branches.map(_.body)))
    end
    ans = typ.visit(self) and val.visit(self) and lists.all(_.visit(self), branches) and _else.visit(self)
    parent-block-loc := old-pbl
    ans
  end,
  method s-for(self, l, iterator, bindings, ann, body, blocky) block:
    old-pbl = parent-block-loc
    parent-block-loc := l
    when not(blocky):
      wf-blocky-blocks(l, [list: body])
    end
    ans = iterator.visit(self) and lists.all(_.visit(self), bindings) and ann.visit(self) and body.visit(self)
    parent-block-loc := old-pbl
    ans
  end,
  method s-frac(self, l, num, den) block:
    when den == 0:
      add-error(C.zero-fraction(l, num))
    end
    true
  end,
  method s-rfrac(self, l, num, den) block:
    when den == 0:
      add-error(C.zero-fraction(l, num))
    end
    true
  end,
  method s-id(self, l, id) block:
    when (reserved-names.has-key(id.tosourcestring())):
      reserved-name(l, id.tosourcestring())
    end
    true
  end,
  method s-provide(self, l, expr) block:
    when not(A.is-s-obj(expr)):
      add-error(C.non-object-provide(l))
    end
    true
  end,
  method s-reactor(self, l, fields):
    method-fields = for filter(f from fields): A.is-s-method-field(f) end
    if not(is-empty(method-fields)) block:
      wf-error([list: ED.text("A reactor cannot contain method fields ")], method-fields.first.l)
      true
    else:
      has-init = is-some(for find(f from fields): f.name == "init" end)
      when not(has-init):
        wf-error([list: ED.text("A reactor must have a field named "), ED.code(ED.text("init")),
            ED.text(" for the initial value ")], l)
      end
      fields-dict = SD.make-mutable-string-dict()
      ok-fields = C.reactor-fields
      for each(f from fields) block:
        when not(ok-fields.has-key(f.name)):
          wf-error([list: ED.text("Valid options for reactors are "),
              ED.h-sequence-sep(ok-fields.keys-list().map({(ok): ED.code(ED.text(ok))}), ", ", ", or "),
              ED.text(", but found one named "),
              ED.code(ED.text(f.name)), ED.text(" ")], f.l)
        end
        cases(Option<A.Loc>) fields-dict.get-now(f.name):
          | none => fields-dict.set-now(f.name, f.l)
          | some(l2) => wf-error2("Duplicate option in reactor: " + f.name, f.l, l2)
        end
        f.visit(self)
      end
      true
    end
  end,
  method s-table(self, l :: Loc, header :: List<A.FieldName>, rows :: List<A.TableRow>) block:
    wf-table-headers(l, header)
    if is-empty(header) block:
      true
    else:
      expected-len = header.length()
      for lists.all(_row from rows) block:
        actual-len = _row.elems.length()
        when actual-len == 0:
          add-error(C.table-empty-row(_row.l))
        end
        when (actual-len <> 0) and (actual-len <> expected-len):
          header-loc = header    .get(0).l + header    .last().l
          row-loc    = _row.elems.get(0).l + _row.elems.last().l
          add-error(C.table-row-wrong-size(header-loc, header, _row))
        end
        for lists.all(elem from _row.elems):
          elem.visit(self)
        end
      end
    end
  end,
  method s-table-extend(self, l, column-binds, extensions) block:
    bound-names = S.list-to-tree-set(map(lam(b :: A.Bind): b.id.toname() end, column-binds.binds))
    for L.all(extension from extensions):
      cases(A.TableExtendField) extension block:
        | s-table-extend-field(_, _, val, ann) => val.visit(self) and ann.visit(self)
        | s-table-extend-reducer(_, _, reducer, col, ann) =>
          when (not(bound-names.member(col.toname()))):
            add-error(C.table-reducer-bad-column(extension, column-binds.l))
          end
          reducer.visit(self) and ann.visit(self)
      end
    end
  end,
  method s-load-table(self, l, headers, spec) block:
    this-expr = A.s-load-table(l, headers, spec)
    wf-table-headers(l, headers)
    if is-empty(spec) block:
      add-error(C.load-table-no-body(this-expr))
      false
    else:
      bound-names = S.list-to-tree-set(map(lam(b :: A.FieldName): b.name end, headers))
      var dup-found = false
      header-loc =
        if is-empty(headers):
          l.upto(spec.first.l)
        else:
          headers.first.l + headers.last().l
        end
      {num-srcs; _} = for fold(acc from {0; [SD.string-dict:]}, s from spec):
        cases(A.LoadTableSpec) s block:
          | s-sanitize(_,name,_) =>
            namestr = name.toname()
            when (not(bound-names.member(namestr))):
              add-error(C.table-sanitizer-bad-column(s, header-loc))
            end
            cases(Option) acc.{1}.get(namestr) block:
              | some(expr) =>
                add-error(C.load-table-duplicate-sanitizer(
                    acc.{1}.get-value(namestr), namestr, s))
                dup-found := true
                acc
              | none => {acc.{0}; acc.{1}.set(namestr, s)}
            end
          | s-table-src(_,_) => {acc.{0} + 1; acc.{1}}
        end
      end
      when num-srcs <> 1:
        add-error(C.load-table-bad-number-srcs(this-expr, num-srcs))
      end
      (num-srcs == 1) and not(dup-found) and L.all(_.visit(self), spec)
    end
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
  method s-type(self, l, name, params, ann):
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
  method s-type-bind(self, l, name, params, ann):
    ann.visit(well-formed-visitor)
  end,
  method s-newtype-bind(self, l, name, namet):
    true
  end,
  method s-variant(self, l, constr-loc, name, binds, with-members) block:
    for each(one-bind from binds.map(_.bind)):
      cases(A.Bind) one-bind:
        | s-bind(_,_,_,_) => nothing
        | s-tuple-bind(l2, _, _) => wf-error([list: ED.text("Tuple binding not allowed as variant member ")], l2)
     end
    end
    ids = fields-to-binds(with-members) + binds.map(_.bind)
    ensure-unique-ids(ids)
    underscores = binds.filter(lam(b) block: A.is-s-bind(b.bind) and A.is-s-underscore(b.bind.id) end)
    when not(is-empty(underscores)):
      add-error(C.underscore-as(underscores.first.l, "a data variant name"))
    end
    check-underscore-name(with-members, "a field name")
    lists.each(_.visit(well-formed-visitor), binds)
    lists.each(_.visit(well-formed-visitor), with-members)
    true
  end,
  method s-singleton-variant(self, l, name, with-members) block:
    ensure-unique-ids(fields-to-binds(with-members))
    lists.each(_.visit(well-formed-visitor), with-members)
    true
  end,
  method s-data(self, l, name, params, mixins, variants, shares, _check-loc, _check) block:
    old-pbl = parent-block-loc
    parent-block-loc := cases(Option) _check-loc:
      | none => l
      | some(cl) => l.upto-end(cl)
    end
    ensure-unique-variant-ids(variants, name, l)
    check-underscore-name(variants, "a data variant name")
    check-underscore-name(shares, "a shared field name")
    check-underscore-name([list: {l: l, name: name}], "a datatype name")
    the-cur-shared = cur-shared
    cur-shared := fields-to-binds(shares)
    lists.each(_.visit(well-formed-visitor), params)
    lists.each(_.visit(well-formed-visitor), mixins)
    lists.each(_.visit(self), variants)
    lists.each(_.visit(well-formed-visitor), shares)
    cur-shared := the-cur-shared
    cases(Option) _check-loc:
      | none => nothing
      | some(cl) => parent-block-loc := cl.upto-end(l)
    end
    wrap-reject-standalones-in-check(_check)
    wrap-visit-check(well-formed-visitor, _check)
    parent-block-loc := old-pbl
    true
  end,
  method s-data-expr(self, l, name, namet, params, mixins, variants, shared, _check-loc, _check) block:
    old-pbl = parent-block-loc
    parent-block-loc := cases(Option) _check-loc:
      | none => l
      | some(cl) => l.upto-end(cl)
    end
    ensure-unique-variant-ids(variants, name, l)
    check-underscore-name(variants, "a data variant name")
    check-underscore-name(shared, "a shared field name")
    check-underscore-name([list: {l: l, name: name}], "a datatype name")
    the-cur-shared = cur-shared
    cur-shared := fields-to-binds(shared)
    lists.each(_.visit(well-formed-visitor), params)
    lists.each(_.visit(well-formed-visitor), mixins)
    lists.each(_.visit(well-formed-visitor), variants)
    lists.each(_.visit(well-formed-visitor), shared)
    cur-shared := the-cur-shared
    cases(Option) _check-loc:
      | none => nothing
      | some(cl) => parent-block-loc := cl.upto-end(l)
    end
    wrap-reject-standalones-in-check(_check)
    wrap-visit-check(well-formed-visitor, _check)
    parent-block-loc := old-pbl
    true
  end,

  # Everything else delegates to the non-toplevel visitor
  method s-import(_, l, import-type, name):
    well-formed-visitor.s-import(l, import-type, name)
  end,
  method s-include(_, l, import-type):
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
  method s-letrec-bind(_, l, bind, expr) block:
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
  method s-fun(_, l, name, params, args, ann, doc, body, _check-loc, _check, blocky):
    well-formed-visitor.s-fun(l, name, params, args, ann, doc, body, _check-loc, _check, blocky)
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
  method s-contract(_, l :: Loc, name :: A.Name, params :: List<A.Name>, ann :: A.Ann):
    # TODO
    true
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
  method s-check-test(_, l :: Loc, op :: A.CheckOp, refinement :: Option<A.Expr>, left :: A.Expr, right :: Option<A.Expr>, cause :: Option<A.Expr>):
    well-formed-visitor.s-check-test(l, op, refinement, left, right, cause)
  end,
  method s-paren(_, l :: Loc, expr :: A.Expr):
    well-formed-visitor.s-paren(l, expr)
  end,
  method s-lam(_, l :: Loc, name :: String, params :: List<String>, args :: List<A.Bind>, ann :: A.Ann, doc :: String, body :: A.Expr, _check-loc :: Option<Loc>, _check :: Option<A.Expr>, blocky):
    well-formed-visitor.s-lam(l, name, params, args, ann, doc, body, _check-loc, _check, blocky)
  end,
  method s-method(_, l :: Loc, name :: String, params :: List<A.Name>, args :: List<A.Bind>, ann :: A.Ann, doc :: String, body :: A.Expr, _check-loc :: Option<Loc>, _check :: Option<A.Expr>, blocky):
    well-formed-visitor.s-method(l, name, params, args, ann, doc, body, _check-loc, _check, blocky)
  end,
  method s-extend(_, l :: Loc, supe :: A.Expr, fields :: List<A.Member>):
    well-formed-visitor.s-extend(l, supe, fields)
  end,
  method s-update(_, l :: Loc, supe :: A.Expr, fields :: List<A.Member>):
    well-formed-visitor.s-update(l, supe, fields)
  end,
  method s-tuple-get(_, l :: Loc, tup, index, index-loc):
    well-formed-visitor.s-tuple-get(l, tup, index, index-loc)
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
  method s-app(self, l :: Loc, _fun :: A.Expr, args :: List<A.Expr>):
    if (A.is-s-dot(_fun) and A.is-s-id(_fun.obj)
        and (_fun.obj.id.toname() == "builtins") and (_fun.field == "trace-value")):
      # this is effectively still a top-level expression, so don't penalize it
      # for being inside a desugaring-introduced function call
      _fun.visit(self) and lists.all(_.visit(self), args)
    else:
      well-formed-visitor.s-app(l, _fun, args)
    end
  end,
  method s-prim-app(_, l :: Loc, _fun :: String, args :: List<A.Expr>, app-info :: A.PrimAppInfo):
    well-formed-visitor.s-prim-app(l, _fun, args, app-info)
  end,
  method s-frac(_, l :: Loc, num, den):
    well-formed-visitor.s-frac(l, num, den)
  end,
  method s-reactor(self, l, fields):
    well-formed-visitor.s-reactor(l, fields)
  end,
  method s-rfrac(_, l :: Loc, num, den):
    well-formed-visitor.s-rfrac(l, num, den)
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
  method s-data-field(_, l :: Loc, name :: String, value :: A.Expr):
    well-formed-visitor.s-data-field(l, name, value)
  end,
  method s-mutable-field(_, l :: Loc, name :: String, ann :: A.Ann, value :: A.Expr):
    well-formed-visitor.s-mutable-field(l, name, ann, value)
  end,
  method s-method-field(_, l :: Loc, name :: String, params :: List<A.Name>, args :: List<A.Bind>, ann :: A.Ann, doc :: String, body :: A.Expr, _check-loc :: Option<Loc>, _check :: Option<A.Expr>, blocky):
    well-formed-visitor.s-method-field(l, name, params, args, ann, doc, body, _check-loc, _check, blocky)
  end,
  method s-for-bind(_, l :: Loc, bind :: A.Bind, value :: A.Expr):
    well-formed-visitor.s-for-bind(l, bind, value)
  end,
  method s-variant-member(_, l :: Loc, member-type :: A.VariantMemberType, bind :: A.Bind) block:
    cases(A.Bind) bind:
      | s-bind(_,_,_,_) => well-formed-visitor.s-variant-member(l, member-type, bind)
      | s-tuple-bind(l2, _, _) => wf-error([list: ED.text("Tuple binding not allowed as variant member")], l2)
    end
  end,
  method s-table(_, l :: Loc, header :: List<A.FieldName>, rows :: List<A.TableRow>):
    well-formed-visitor.s-table(l, header, rows)
  end,
  method s-load-table(_, l, header, spec):
    well-formed-visitor.s-load-table(l, header, spec)
  end,
  method s-table-extend(_, l :: Loc, column-binds :: A.ColumnBinds, extensions :: List<A.TableExtendField>):
    well-formed-visitor.s-table-extend(l, column-binds, extensions)
  end,
  method a-arrow(_, l, args, ret, use-parens):
    well-formed-visitor.a-arrow(l, args, ret, use-parens)
  end,
  method a-arrow-argnames(_, l, args, ret, use-parens):
    well-formed-visitor.a-arrow-argnames(l, args, ret, use-parens)
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
    if ast.visit(top-level-visitor) and is-empty(errors): C.ok(ast)
    else: C.err(errors.reverse())
    end
  # cleanup
  cur-shared := empty
  errors := empty
  in-check-block := false
  ans
end
