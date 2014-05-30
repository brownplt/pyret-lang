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
    else if PP.is-mt-doc(acc): item.tosource()
    else: item.tosource() + PP.sbreak(1) + acc
    end
  end
end
data SExp:
  | leaf(val :: String) with: tosource(self): PP.str(self.val) end
  | sexp(name :: String, kids :: List<SExp>) with:
    tosource(self):
      kids = break-if-needed(leaf(self.name) ^ link(_, self.kids))
      PP.parens(PP.nest(2, kids))
    end
  | comment(msg :: String) with: tosource(self): PP.str(";; " + self.msg) + PP.hardline end
  | toplevel(kids :: List<SExp>) with:
    tosource(self): break-if-needed(self.kids) end
  | slist(kids :: List<SExp>) with:
    tosource(self):
      PP.parens(break-if-needed(self.kids))
    end
end

fun spair(name, val):
  if SExp(val): sexp(name, [list: val])
  else: sexp(name, [list: leaf(tostring(val))])
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
  prefixes = S.to-dict({
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

data CrossRef:
  | crossref(modname :: String, field :: String)
end

fun process-fields(fields, bindings):
  fun lookup-value(value):
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
          # print("Further processing on " + v.tosource().pretty(80).first)
          cases(A.Expr) v:
            | s-import(_, _, _) => v
            | s-id(_, id) => help(item^link(_, seen), id)
            | s-id-letrec(_, id, _) => help(item^link(_, seen), id)
            | s-id-var(_, id) => help(item^link(_, seen), id)
            | s-block(_, stmts) => help(seen, stmts.last())
            | s-user-block(_, body) => help(seen, body)
            | s-let-expr(_, _, body) => help(seen, body)
            | s-letrec(_, _, body) => help(seen, body)
            | s-let(_, _, _, body) => help(seen, body)
            | s-dot(_, obj, field) =>
              help-obj = help(seen, obj)
              # print("Looking for " + field + " on " + help-obj.tosource().pretty(80).join-str("\n"))
              cases(A.Expr) help-obj:
                | s-import(_, file, _) => crossref(file.tosource().pretty(1000).first, field)
                | s-obj(_, obj-fields) =>
                  cases(Option) obj-fields.find(lam(f): A.is-s-str(f.name) and (f.name.s == field) end):
                    | none => v
                    | some(new-v) => help(seen, new-v)
                  end
                | s-data-expr(_, name, _, _, variants, shared-members, _) =>
                  if (name == field):
                    # print("Found " + field + " as a data-expr itself")
                    help-obj
                  else:
                    cases(Option) variants.find(lam(f): f.name == field end):
                      | some(new-v) => new-v
                      | none =>
                        cases(Option) variants.find(lam(f): A.make-checker-name(f.name) == field end):
                          | some(new-v) =>
                            a-an =
                              if string-index-of("aeiou", string-substring(new-v.name, 0, 1)) >= 0: "an "
                              else: "a "
                              end
                            A.s-lam(new-v.l, empty,
                              [list: A.s-bind(new-v.l, false, A.s-name(new-v.l, "val"), A.a-any)],
                              A.a-name(new-v.l, "Bool"),
                              "Checks whether the provided argument is in fact " + a-an + new-v.name,
                              A.s-undefined(new-v.l), none)
                          | none =>
                            cases(Option) shared-members.find(lam(f): A.is-s-str(f.name) and (f.name.s == field) end):
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
    help([list: ], value)
  end
  fun method-spec(meth):
    sexp("method-spec",
      [list: spair("name", meth.name.tosource().pretty(1000).first),
        spair("arity", tostring(meth.args.length())),
        spair("args", slist(meth.args.map(lam(b): leaf(torepr(b.id.toname())) end)))
      ]
        + (if meth.doc == "": [list: ] else: [list: spair("doc", torepr(meth.doc))] end))
  end
  for map(field from fields):
    cases(A.Member) field:
      | s-data-field(_, name, value) =>
        # print("***** Trying to lookup " + value.tosource().pretty(80).join-str("\n"))
        e = lookup-value(value)
        cases(A.Expr) e: # Not guaranteed to be an Expr!
          | crossref(modname, as-name) =>
            sexp("re-export", [list: 
                spair("name", torepr(name.s)),
                sexp("cross-ref", [list: leaf(torepr(modname)), leaf(torepr(as-name))])
              ])
          | s-lam(_, params, args, ann, doc, _, _check) =>
            sexp("fun-spec",
              [list: spair("name", torepr(name.s)),
                spair("arity", tostring(args.length())),
                spair("args", slist(args.map(lam(b): leaf(torepr(b.id.toname())) end)))
              ]
                + (if doc == "": [list: ] else: [list: spair("doc", torepr(doc))] end))
          | s-id(_, id) =>
            spair("unknown-item", spair("name", torepr(name.s)))
          | s-variant(_, _, variant-name, members, with-members) =>
            sexp("constr-spec",
              [list: spair("name", torepr(variant-name)),
                spair("members", slist(members.map(lam(m): leaf(torepr(m.bind.id.toname())) end))),
                spair("with-members", slist(with-members.map(method-spec))) ])
          | s-singleton-variant(_,  variant-name, with-members) =>
            sexp("singleton-spec",
              [list: spair("name", torepr(variant-name)),
                spair("with-members", slist(with-members.map(method-spec))) ])
          | s-data-expr(_, data-name, _, _, variants, shared, _) =>
            sexp("data-spec",
              [list: spair("name", torepr(data-name)),
                spair("variants", slist(variants.map(lam(m): leaf(torepr(m.name)) end))),
                spair("shared", slist(shared.map(method-spec))) ])
          | else =>
            sexp("unknown-item",
              spair("name", torepr(name.s)) ^ link(_, e.tosource().pretty(70).map(comment)))
        end
    end
  end
end

cases (C.ParsedArguments) parsed-options:
  | success(opts, rest) =>
    print-width = opts.get("width")
    dialect = opts.get("dialect")
    cases (List) rest:
      | empty => print("Require a file name")
      | link(file, more) =>
        file-contents = F.file-to-string(file)
        parsed = P.parse-dialect(dialect, file-contents, file)
        scoped = R.desugar-scope(DC.desugar-no-checks(U.append-nothing-if-necessary(parsed).orelse(parsed)), CS.minimal-builtins)
        named-and-bound = R.resolve-names(scoped, CS.minimal-builtins)
        named = named-and-bound.ast
        bindings = named-and-bound.bindings
        body = named.block
        result = find-result(body)
        cases(A.Expr) result:
          | s-obj(_, res) =>
            provides = res.find(lam(f): A.is-s-data-field(f) and A.is-s-str(f.name) and (f.name.s == "provide") end)
            cases(Option) provides:
              | none => print("Got a result object with no provides fields")
              | some(p) =>
                cases(A.Expr) find-result(p.value):
                  | s-obj(_, fields) =>
                    output = toplevel(
                      [list: sexp("module",
                          [list: leaf(torepr(trim-path(file))),
                            spair("path", torepr(string-replace(file, "\\", "/")))
                          ]
                            + process-fields(fields, bindings)) ])
                    outputdoc = output.tosource().pretty(80)
                    cases(List) more:
                      | empty => outputdoc.each(print)
                      | link(outfile, _) =>
                        F.output-file(outfile, false).display(outputdoc.join-str("\n"))
                    end
                  | else => nothing
                end
            end
          | else => print("Got a result we didn't expect")
            print(torepr(result))
        end
    end
  | arg-error(m, _) =>
    each(print,  ("Error: " + m) ^ link(_, C.usage-info(options)))
end
