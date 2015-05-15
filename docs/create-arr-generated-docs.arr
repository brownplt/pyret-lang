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
import string-dict as S
import file as F
import pprint as PP

import "docs/doc-utils.arr" as DU
quote = DU.quote
at-app = DU.at-app
hashlang = DU.hashlang
leaf = DU.leaf
sexp = DU.sexp
at-comment = DU.at-comment
comment = DU.comment
toplevel = DU.toplevel
slist = DU.slist
hash-key = DU.hash-key
at-exp = DU.at-exp
spair = DU.spair
trim-path = DU.trim-path
relative-dir = DU.relative-dir
crossref = DU.crossref
xref = DU.xref
process-ann = DU.process-ann
lookup-value = DU.lookup-value
process-fields = DU.process-fields

options = [S.string-dict:
  "width",
    C.next-val-default(C.Number, 80, some("w"), C.once, "Pretty-printed width"),
  "dialect",
    C.next-val-default(C.String, "Pyret", some("d"), C.once, "Dialect to use")
]

parsed-options = C.parse-cmdline(options)


fun find-result(expr):
  cases(A.Expr) expr:
    | s-obj(_, _) => expr
    | s-block(_, stmts) => find-result(stmts.last())
    | s-let-expr(_, _, body) => find-result(body)
    | s-type-let-expr(_, _, body) => find-result(body)
    | s-letrec(_, _, body) => find-result(body)
    | s-module(_, _, _, _, _) => expr
    | else =>
      print("Got an expression we didn't expect:")
      print(torepr(expr))
      raise("Failure?")
  end
end


fun process-module(file, fields, types, bindings, type-bindings):
  split-fields = process-fields(trim-path(file), fields, types, bindings, type-bindings)
  fun method-spec(data-name, meth):
    cases(A.Member) meth:
      | s-method-field(_, name, params, args, ann, doc, _, _) =>
        shadow process-ann = process-ann(_, file, split-fields, bindings, type-bindings)
        sexp("method-spec",
          [list: spair("name", "\"" + PP.str(name).pretty(1000).first + "\""),
            spair("arity", tostring(args.length())),
            spair("params", slist(params.map(lam(p): leaf(torepr(p.toname())) end))),
            spair("args", slist(args.map(lam(b): leaf(torepr(b.id.toname())) end))),
            spair("return", process-ann(ann)),
            spair("contract",
              process-ann(
                A.a-arrow(A.dummy-loc,
                  A.a-name(A.dummy-loc, A.s-name(A.dummy-loc, data-name)) ^ link(_, args.rest.map(_.ann)),
                  ann, false)))
          ]
            + (if doc == "": [list: ] else: [list: spair("doc", torepr(doc))] end))
      | s-data-field(_, name, value) =>
        sexp("unknown-item",
          spair("name", torepr(name)) ^ link(_, meth.tosource().pretty(70).map(comment)))
    end
  end
  for map(field from fields):
    cases(A.Member) field:
      | s-data-field(_, name, value) =>
        # print("***** Trying to lookup " + value.tosource().pretty(200).first)
        e = lookup-value(value, bindings)
        cases(Any) e: # Not guaranteed to be an Expr!
          | crossref(modname, as-name) =>
            sexp("re-export", [list: 
                spair("name", torepr(name)),
                sexp("cross-ref", [list: leaf(torepr(modname)), leaf(torepr(as-name))])
              ])
          | s-lam(l, params, args, ann, doc, _, _check) =>
            shadow process-ann = process-ann(_, file, split-fields, bindings, type-bindings)
            sexp("fun-spec",
              [list: spair("name", torepr(name)),
                spair("arity", tostring(args.length())),
                spair("params", params.map(lam(p): leaf(torepr(p.toname())) end)),
                spair("args", slist(args.map(lam(b): leaf(torepr(b.id.toname())) end))),
                spair("return", process-ann(ann)),
                spair("contract", process-ann(A.a-arrow(l, args.map(_.ann), ann, false)))
              ]
                + (if doc == "": [list: ] else: [list: spair("doc", torepr(doc))] end))
          | s-id(_, id) =>
            spair("unknown-item", spair("name", torepr(name)))
          | s-variant(_, _, variant-name, members, with-members) =>
            data-name = split-fields.constructors.get-value(variant-name)
            fun member-type-str(m):
              cases(A.VariantMemberType) m.member-type:
                | s-normal => "normal"
                | s-mutable => "mutable"
              end
            end
            sexp("constr-spec",
              [list: spair("name", torepr(variant-name)),
                spair("members", slist(
                    members.map(lam(m):
                        sexp(torepr(m.bind.id.toname()), [list:
                            spair("type", member-type-str(m)),
                            spair("contract",
                              process-ann(m.bind.ann, file, split-fields, bindings, type-bindings))])
                      end))),
                spair("with-members", slist(with-members.map(method-spec(data-name, _)))) ])
          | s-singleton-variant(_,  variant-name, with-members) =>
            data-name = split-fields.constructors.get-value(variant-name)
            sexp("singleton-spec",
              [list: spair("name", torepr(variant-name)),
                spair("with-members", slist(with-members.map(method-spec(data-name, _)))) ])
          | s-data-expr(_, data-name, _, type-vars, _, variants, shared, _) =>
            if string-index-of(name, "is-") == 0:
              DU.snothing
            else:
              sexp("data-spec",
                [list: spair("name", torepr(data-name)),
                  spair("type-vars", slist(type-vars.map(lam(tv): leaf(torepr(tv)) end))),
                  spair("variants", slist(variants.map(lam(m): leaf(torepr(m.name)) end))),
                  spair("shared", slist(shared.map(method-spec(data-name, _)))) ])
            end
          | else =>
            sexp("unknown-item",
              spair("name", torepr(name)) ^ link(_, e.tosource().pretty(70).map(comment)))
        end
    end
  end
end

cases (C.ParsedArguments) parsed-options:
  | success(opts, rest) =>
    print-width = opts.get-value("width")
    dialect = opts.get-value("dialect")
    cases (List) rest:
      | empty => print("Require a file name")
      | link(file, more) =>
        file-contents = F.file-to-string(file)
        parsed = P.parse-dialect(dialect, file-contents, file)
        scoped = R.desugar-scope(DC.desugar-no-checks(U.append-nothing-if-necessary(parsed).or-else(parsed)))
        named-and-bound = R.resolve-names(scoped, CS.minimal-builtins)
        named = named-and-bound.ast
        bindings = named-and-bound.bindings
        type-bindings = named-and-bound.type-bindings
        body = named.block
        result = find-result(body)
        cases(A.Expr) result:
          | s-module(_, _, provides, provides-types, _) =>
            cases(A.Expr) provides:
              | s-obj(_, fields) =>
                output = toplevel(
                  [list: sexp("module",
                      [list: leaf(torepr(trim-path(file))),
                        spair("path", torepr(string-replace(file, "\\", "/")))
                      ]
                        + process-module(file, fields, provides-types, bindings, type-bindings)) ])
                outputdoc = output.tosource().pretty(80)
                cases(List) more:
                  | empty => outputdoc.each(print)
                  | link(outfile, _) =>
                    F.output-file(outfile, false).display(outputdoc.join-str("\n"))
                end
              | else => nothing
            end
          | else => print("Got a result we didn't expect")
            print(torepr(result))
        end
    end
  | arg-error(m, _) =>
    each(print,  ("Error: " + m) ^ link(_, C.usage-info(options)))
end
