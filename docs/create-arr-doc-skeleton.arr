#lang pyret

import cmdline as C
import parse-pyret as P
# import "compiler/desugar.arr" as D
import "compiler/desugar-check.arr" as DC
# import "compiler/anf.arr" as A
# import "compiler/compile.arr" as CM
import "compiler/compile-structs.arr" as CS
import "compiler/resolve-scope.arr" as R
import ast as A
import "compiler/ast-util.arr" as U
# import "compiler/anf.arr" as N
# import "compiler/ast-split.arr" as AS
# import "compiler/js-of-pyret.arr" as JS
# import "compiler/desugar-check.arr" as CH
import file as F
import string-dict as S
import pprint as PP

options = {
  width: C.next-val-default(C.Number, 80, some("w"), C.once, "Pretty-printed width"),
  dialect: C.next-val-default(C.String, "Pyret", some("d"), C.once, "Dialect to use")
}

parsed-options = C.parse-cmdline(options)

fun break-if-needed(items):
  for fold(acc from PP.mt-doc, item from items.reverse()):
    if is-comment(item): item.tosource() + acc
    else if is-at-comment(item): item.tosource() + acc
    else if PP.is-mt-doc(acc): item.tosource()
    else: item.tosource() + PP.break(1) + acc
    end
  end
end
data SExp:
  | hashlang(str :: String) with: tosource(self): PP.str("#lang " + self.str) end
  | at-app(name :: String, kids :: List<SExp>) with:
    tosource(self):
      PP.str("@") + PP.parens(break-if-needed(leaf(self.name) ^ link(self.kids)))
    end
  | leaf(val :: String) with: tosource(self): PP.str(self.val) end
  | sexp(name :: String, kids :: List<SExp>) with:
    tosource(self):
      kids = break-if-needed(self.kids)
      PP.parens(PP.nest(2, PP.str(self.name) + PP.break(1) + kids))
    end
  | at-comment(msg :: String) with: tosource(self): PP.str("@; " + self.msg) + PP.hardline end
  | comment(msg :: String) with: tosource(self): PP.str(";; " + self.msg) + PP.hardline end
  | toplevel(kids :: List<SExp>) with:
    tosource(self): break-if-needed(self.kids) end
  | slist(kids :: List<SExp>) with:
    tosource(self):
      PP.parens(break-if-needed(self.kids))
    end
  | hash-key(name :: String, val :: SExp) with:
    tosource(self):
      PP.group(PP.str("#:" + self.name) + PP.break(1) + self.val.tosource())
    end
  | at-exp(name :: String, raw :: Option<List<SExp>>, processed :: Option<List<SExp>>) with:
    tosource(self):
      PP.str("@" + self.name) +
      cases(Option) self.raw:
        | none => PP.mt-doc
        | some(raw) => PP.brackets(PP.nest(2, PP.break(0) + break-if-needed(raw)) + PP.break(0))
      end +
      cases(Option) self.processed:
        | none => PP.mt-doc
        | some(kids) => PP.braces(PP.nest(2, PP.break(0) + break-if-needed(kids)) + PP.break(0))
      end
    end
end

fun spair(name, val):
  if SExp(val): sexp(name, [val])
  else: sexp(name, [leaf(tostring(val))])
  end
end

fun find-result(expr):
  cases(A.Expr) expr:
    | s-obj(_, _) => expr
    | s-block(_, stmts) => find-result(stmts.last())
    | s-let-expr(_, _, body) => find-result(body)
    | s-letrec(_, _, body) => find-result(body)
    | else =>
      print("Got an expression we didn't expect:")
      print(torepr(expr))
      raise("Failure?")
  end
end

fun trim-path(path):
  var ret = path
  var prefixes = S.to-dict({
      trove: "",
      js: "js/",
      base: "",
      compiler: "compiler/"
    })
  ret := string-replace(ret, "\\", "/")
  for each(prefix from prefixes.keys()):
    i = string-index-of(ret, prefix + "/")
    when i >= 0:
      ret := string-replace(string-substring(ret, i, string-length(ret)), prefix + "/", prefixes.get(prefix))
      if string-index-of(prefixes.get(prefix), "/") >= 0:
        ret := '"' + ret + '"'
      else:
        ret := string-replace(ret, ".arr", "")
      end
    end
  end
  ret
end

fun lookup-value(value, bindings):
  fun help(seen, item):
    to-examine =
      if A.Name(item):
        if seen.member(item): none
        else if bindings.has-key(item.key()):
          # print("Found " + item.key() + " in bindings")
          bindings.get(item.key()).expr
        else: none
        end
      else if A.Expr(item): some(item)
      else: none
      end
    cases(Option) to-examine:
      | none => value
      | some(v) =>
        cases(A.Expr) v:
          | s-id(_, id) => help(item^link(seen), id)
          | s-id-letrec(_, id) => help(item^link(seen), id)
          | s-id-var(_, id) => help(item^link(seen), id)
          | s-block(_, stmts) => help(seen, stmts.last())
          | s-user-block(_, body) => help(seen, body)
          | s-let-expr(_, _, body) => help(seen, body)
          | s-letrec(_, _, body) => help(seen, body)
          | s-let(_, _, _, body) => help(seen, body)
          | s-dot(_, obj, field) =>
            help-obj = help(seen, obj)
            cases(A.Expr) help-obj:
              | s-obj(_, obj-fields) =>
                cases(Option) obj-fields.find(fun(f): A.is-s-str(f.name) and (f.name.s == field) end):
                  | none => v
                  | some(new-v) => help(seen, new-v)
                end
              | s-data-expr(_, name, _, _, variants, shared-members, _) =>
                if (name == field):
                  help-obj
                else:
                  cases(Option) variants.find(fun(f): f.name == field end):
                    | some(new-v) => new-v
                    | none =>
                      cases(Option) variants.find(fun(f): A.make-checker-name(f.name) == field end):
                        | some(new-v) =>
                          a-an =
                            if string-index-of("aeiou", string-substring(new-v.name, 0, 1)) >= 0: "an "
                            else: "a "
                            end
                          A.s-lam(new-v.l, empty,
                            [A.s-bind(new-v.l, false, A.s-name(new-v.l, "val"), A.a-any)],
                            A.a-name(new-v.l, "Bool"),
                            "Checks whether the provided argument is in fact " + a-an + new-v.name,
                            A.s-undefined(new-v.l), none)
                        | none =>
                          cases(Option) shared-members.find(fun(f): A.is-s-str(f.name) and (f.name.s == field) end):
                            | some(new-v) => new-v
                            | none => v
                          end
                      end
                  end
                end
              | else => v
            end
          | else => v
        end
    end
  end
  help([], value)
end
fun process-fields(fields, bindings):
  looked-up = S.string-dict()
  for each(field from fields):
    field-name = field.name.tosource().pretty(80).first
    value = lookup-value(field.value, bindings)
    pretty-val = value.tosource().pretty(80)
    looked-up.set(field-name, value)
  end
  data-vals = S.string-dict()
  fun-vals = S.string-dict()
  ignored-vals = S.string-dict()
  unknown-vals = S.string-dict()
  for each(field from looked-up.keys()):
    value = looked-up.get(field)
    cases(A.Expr) value:
      | s-lam(_, _, _, _, _, _, _) =>
        if (string-index-of(field, "\"is-") == 0):
          ignored-vals.set(field, value)
        else:
          fun-vals.set(field, value)
        end
      | s-variant(_, _, _, _, _) => data-vals.set(field, value)
      | s-singleton-variant(_, _, _) => data-vals.set(field, value)
      | s-data-expr(_, _, _, _, _, _, _) => data-vals.set(field, value)
      | else => unknown-vals.set(field, value)
    end
  end
  { data-vals: data-vals, fun-vals: fun-vals, ignored-vals: ignored-vals, unknown-vals: unknown-vals }
end

no-atoms = A.default-map-visitor.{
  s-atom(self, base, _): A.s-name(A.dummy-loc, base) end,
  s-id-letrec(self, l, name): A.s-id(l, A.s-name(l, name.toname())) end,
  s-id-var(self, l, name): A.s-id(l, A.s-name(l, name.toname())) end
}
fun de-atomize(e): e.visit(no-atoms) end

fun process-module(file, fields):
  fun process-item(name, e):
    cases(A.Expr) e: # Not guaranteed to be an Expr!
      | s-lam(_, params, args, ret-ann, doc, _, _check) =>
        at-exp("function", some(
            [ leaf(name),
              hash-key("contract",
                sexp("list", args.map(fun(a): leaf(torepr(a.ann.tosource().pretty(80).join-str("\n"))) end) +
                  [leaf(torepr(ret-ann.tosource().pretty(80).join-str("\n")))]))
            ]),
          _check.andthen(fun(v): some(de-atomize(v).tosource().pretty(80).map(at-comment)) end))
      | s-id(_, id) =>
        spair("unknown-item", spair("name", name))
      | s-variant(_, _, variant-name, members, with-members) =>
        at-comment('Data constructor ' + variant-name)
        # sexp("constr-spec",
        #   [ spair("name", torepr(variant-name)),
        #     spair("members", slist(members.map(fun(m): leaf(torepr(m.bind.id.toname())) end))),
        #     spair("with-members", slist(with-members.map(fun(m): leaf(m.name.tosource().pretty(80).first) end))) ])
      | s-singleton-variant(_,  variant-name, with-members) =>
        at-comment('Singleton data variant ' + variant-name)
        # sexp("singleton-spec",
        #   [ spair("name", torepr(variant-name)),
        #     spair("with-members", slist(with-members.map(fun(m): leaf(m.name.tosource().pretty(80).first) end))) ])
      | s-data-expr(_, data-name, _, _, variants, shared, _) =>
        at-comment('Data expr for ' + data-name)
        # sexp("data-spec",
        #   [ spair("name", torepr(data-name)),
        #     spair("variants", slist(variants.map(fun(m): leaf(torepr(m.name)) end))),
        #     spair("shared", slist(shared.map(fun(m): leaf(m.name.tosource().pretty(80).first) end))) ])
      | else =>
        sexp("unknown-item",
          spair("name", name) ^ link(e.tosource().pretty(70).map(at-comment)))
    end
  end
  at-exp("docmodule", some([leaf(torepr(trim-path(file)))]),
    some(
      [ at-comment("Ignored type testers"),
        at-exp("ignore", some([sexp("list", fields.ignored-vals.keys().map(leaf))]), none)]
        + [ at-comment("Unknown: PLEASE DOCUMENT"),
        at-exp("ignore", some([sexp("list", fields.unknown-vals.keys().map(leaf))]), none)]
        + [at-exp("section", none, some([leaf("Data types")]))]
        + fields.data-vals.keys().map(fun(k): process-item(k, fields.data-vals.get(k)) end)
        + [at-exp("section", none, some([leaf("Functions")]))]
        + fields.fun-vals.keys().map(fun(k): process-item(k, fields.fun-vals.get(k)) end)))
end

cases (C.ParsedArguments) parsed-options:
  | success(opts, rest) =>
    print-width = opts.get("width")
    dialect = opts.get("dialect")
    cases (List) rest:
      | empty => print("Require a file name")
      | link(file, _) =>
        file-contents = F.file-to-string(file)
        parsed = P.parse-dialect(dialect, file-contents, file)
        scoped = R.desugar-scope(DC.desugar-no-checks(U.append-nothing-if-necessary(parsed)), CS.minimal-builtins)
        named-and-bound = R.resolve-names(scoped, CS.minimal-builtins)
        named = named-and-bound.ast
        bindings = named-and-bound.bindings
        body = named.block
        result = find-result(body)
        cases(A.Expr) result:
          | s-obj(_, res) =>
            provides = res.find(fun(f): A.is-s-data-field(f) and A.is-s-str(f.name) and (f.name.s == "provide") end)
            cases(Option) provides:
              | none => print("Got a result object with no provides fields")
              | some(p) =>
                cases(A.Expr) find-result(p.value):
                  | s-obj(_, fields) =>
                    output = toplevel([
                        hashlang("scribble/base"),
                        at-app("require", [leaf(torepr("../scribble-api.rkt"))]),
                        process-module(file, process-fields(fields, bindings))
                      ])
                    nothing
                    output.tosource().pretty(80).each(print)
                  | else => nothing
                end
            end
          | else => print("Got a result we didn't expect")
            print(torepr(result))
        end
    end
  | arg-error(m, _) =>
    each(print,  ("Error: " + m) ^ link(C.usage-info(options)))
end
