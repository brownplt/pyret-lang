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
    else: item.tosource() + PP.break(1) + acc
    end
  end
end
data SExp:
  | leaf(val :: String) with: tosource(self): PP.str(self.val) end
  | sexp(name :: String, kids :: List<SExp>) with:
    tosource(self):
      kids = break-if-needed(self.kids)
      PP.parens(PP.nest(2, PP.str(self.name) + PP.break(1) + kids))
    end
  | comment(msg :: String) with: tosource(self): PP.str(";; " + self.msg) + PP.hardline end
  | toplevel(kids :: List<SExp>) with:
    tosource(self): break-if-needed(self.kids) end
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
  for each(prefix from link("trove", link("js", link("compiler", empty)))):
    i = string-index-of(path, prefix)
    when i >= 0:
      ret := string-substring(path, i, string-length(path))
    end
  end
  ret := string-replace(string-replace(ret, "\\", "/"), ".arr", "")
  ret
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
              # print("Looking for " + field + " on " + help-obj.tosource().pretty(80).join-str("\n"))
              cases(A.Expr) help-obj:
                | s-obj(_, obj-fields) =>
                  cases(Option) obj-fields.find(fun(f): A.is-s-str(f.name) and (f.name.s == field) end):
                    | none => v
                    | some(new-v) => help(seen, new-v)
                  end
                | s-data-expr(_, name, _, _, variants, shared-members, _) =>
                  if (name == field):
                    # print("Found " + field + " as a data-expr itself")
                    help-obj
                  else:
                    # print("Looking for " + field + " among variants " + tostring(variants.map(_.name)))
                    cases(Option) variants.find(fun(f): f.name == field end):
                      | none => 
                        # print("Looking for " + field + " among shared " + tostring(shared-members.map(_.name)))
                        cases(Option) shared-members.find(fun(f): A.is-s-str(f.name) and (f.name.s == field) end):
                          | none => v
                          | some(new-v) =>
                            # print("Found " + field + " in shared")
                            new-v
                        end                  
                      | some(new-v) =>
                        # print("Found " + field + " in variants")
                        new-v
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
  for map(field from fields):
    cases(A.Member) field:
      | s-data-field(_, name, value) =>
        # print("***** Trying to lookup " + value.tosource().pretty(80).join-str("\n"))
        e = lookup-value(value)
        cases(A.Expr) e: # Not guaranteed to be an Expr!
          | s-lam(_, params, args, ann, doc, _, _check) =>
            sexp("fun-spec",
              [ sexp("name", [leaf('"' + name.s + '"')]),
                sexp("arity", [leaf(tostring(args.length()))]),
                sexp("args", args.map(fun(b): leaf('"' + b.id.toname() + '"') end))
              ]
                + (if doc == "": [] else: [sexp("doc", [leaf('"' + doc + '"')])] end))
          | s-id(_, id) =>
            sexp("unknown-item", [leaf('"' + name.s + '"')])
          | s-variant(_, _, variant-name, members, with-members) =>
            sexp("constr-spec",
              [ sexp("name", [leaf('"' + variant-name + '"')]),
                sexp("members", members.map(fun(m): leaf('"' + m.bind.id.toname() + '"') end)),
                sexp("with-members", with-members.map(fun(m): leaf(m.name.tosource().pretty(80).first) end)) ])
          | s-singleton-variant(_,  variant-name, with-members) =>
            sexp("singleton-spec",
              [ sexp("name", [leaf('"' + variant-name + '"')]),
                sexp("with-members", with-members.map(fun(m): leaf(m.name.tosource().pretty(80).first) end)) ])
          | s-data-expr(_, data-name, _, _, variants, shared, _) =>
            sexp("data-spec",
              [ sexp("name", [leaf('"' + data-name + '"')]),
                sexp("variants", variants.map(fun(m): leaf('"' + m.name + '"') end)),
                sexp("shared", shared.map(fun(m): leaf('"' + m.name.tosource().pretty(80).first + '"') end)) ])
          | else =>
            sexp("unknown-item",
              sexp("name", [leaf('"' + name.s + '"')]) ^ link(e.tosource().pretty(70).map(comment)))
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
      | link(file, _) =>
        file-contents = F.file-to-string(file)
        parsed = P.parse-dialect(dialect, file-contents, file)
        scoped = R.desugar-scope(DC.desugar-check(U.append-nothing-if-necessary(parsed)), CS.minimal-builtins)
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
                    output = toplevel(
                      [ comment("Processing " + file),
                        sexp("module",
                          leaf('"' + trim-path(file) + '"') ^ link(process-fields(fields, bindings))) ])
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
