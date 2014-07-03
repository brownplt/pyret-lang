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
process-fields = DU.process-fields

options = {
  width: C.next-val-default(C.Number, 80, some("w"), C.once, "Pretty-printed width"),
  dialect: C.next-val-default(C.String, "Pyret", some("d"), C.once, "Dialect to use")
}

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

fun lookup-value(value, bindings):
  fun help(seen, item):
    to-examine =
      if A.Name(item):
        if seen.member(item): none
        else if bindings.has-key(item.key()):
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
          | s-id(_, id) => help(item ^ link(_, seen), id)
          | s-id-letrec(_, id, _) => help(item ^ link(_, seen), id)
          | s-id-var(_, id) => help(item ^ link(_, seen), id)
          | s-block(_, stmts) => help(seen, stmts.last())
          | s-user-block(_, body) => help(seen, body)
          | s-let-expr(_, _, body) => help(seen, body)
          | s-letrec(_, _, body) => help(seen, body)
          | s-let(_, _, _, body) => help(seen, body)
          | s-dot(_, obj, field) =>
            help-obj = help(seen, obj)
            cases(A.Expr) help-obj:
              | s-import(_, file, _) => crossref(file.tosource().pretty(1000).first, field)
              | s-obj(_, obj-fields) =>
                cases(Option) obj-fields.find(lam(f): A.is-s-str(f.name) and (f.name.s == field) end):
                  | none => v
                  | some(new-v) => help(seen, new-v)
                end
              | s-data-expr(_, name, _, _, _, variants, shared-members, _) =>
                if (name == field):
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
                            A.a-name(new-v.l, A.s-global("Boolean")),
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

no-atoms = A.default-map-visitor.{
  s-atom(self, base, _): A.s-name(A.dummy-loc, base) end,
  s-id-letrec(self, l, name, _): A.s-id(l, A.s-name(l, name.toname())) end,
  s-id-var(self, l, name): A.s-id(l, A.s-name(l, name.toname())) end
}
fun de-atomize(e): e.visit(no-atoms) end


fun process-checks(_check):
  cases (Option) _check:
    | none    => [list: ]
    | some(v) => [list: hash-key("examples",
          quote(at-exp("", none, some(de-atomize(v).tosource().pretty(80).map(at-comment)))))]
  end
end

fun process-member(mem, typ, file, fields, bindings):
  cases(A.Member) mem:
    | s-data-field(_, name, value) =>
      at-exp("member-spec", some([list: leaf(torepr(name.tosource().pretty(1000).first))]), none)
    | s-mutable-field(_, name, ann, value) =>
      at-exp("member-spec",
        some([list:
            leaf(name.tosource().pretty(1000).first),
            hash-key("mutable", "#t"),
            hash-key("contract", process-ann(ann, file, fields, bindings))
          ]),
        none)
    | s-once-field(_, name, ann, value) =>
      at-exp("member-spec",
        some([list:
            leaf(name.tosource().pretty(1000).first),
            hash-key("once", "#t"),
            hash-key("contract", process-ann(ann, file, fields, bindings))
          ]),
        none)
    | s-method-field(_, name, args, ret-ann, doc, body, _check) =>
      at-exp("method-spec", some(
          [list:
            leaf("\"" + name.tosource().pretty(1000).first + "\""),
            # Note: "real" contracts now show up in generated files
            comment("N.B. Pyret contract: " +
              A.a-arrow(A.dummy-loc,
                A.a-name(A.dummy-loc, typ) ^ link(_, args.rest.map(_.ann)),
                ret-ann, true).tosource().pretty(1000).first)
            # hash-key("contract",
            #   sexp("a-arrow",
            #     [list: process-ann(A.a-name(A.dummy-loc, typ), file, fields, bindings)] +
            #     args.rest.map(lam(a): process-ann(a.ann, file, fields, bindings) end) +
            #     [list: process-ann(ret-ann, file, fields, bindings)]))
          ] +
          process-checks(_check)),
        none)
  end
end
fun process-var-member(mem, file, fields, bindings):
  cases(A.VariantMember) mem:
    | s-variant-member(_, typ, b) =>
      at-exp("member-spec",
        some(
          [list:
            leaf(torepr(b.id.toname()))#, hash-key("contract", process-ann(b.ann, file, fields, bindings))
          ]
            + (cases(A.VariantMemberType) typ:
              | s-cyclic => [list: spair("cyclic", "#t")]
              | s-mutable => [list: spair("mutable", "#t")]
              | s-normal => [list: ]
            end)),
        none)
  end
end

fun tag-name(args): leaf(torepr(args.join-str("_"))) end

fun process-module(file, fields, types, bindings, type-bindings):
  shadow fields = process-fields(trim-path(file), fields, types, bindings, type-bindings)
  # print("Binding keys are " + torepr(bindings.keys()))
  # print("Data keys are " + torepr(fields.data-vals.keys()))
  # print("Fun keys are " + torepr(fields.fun-vals.keys()))
  fun process-item(name :: String, e):
    cases(A.Expr) e: # Not guaranteed to be an Expr!
      | crossref(modname, as-name) =>
        at-exp("re-export",
          some([list: leaf(torepr(name)), spair("from", xref(modname, as-name))]),
          some([list:
              at-comment("N.B. need para here to keep xref inline with text"),
              at-exp("para", none,
                some([list: leaf("See"),
                    at-exp("xref", some([list: leaf(torepr(modname)), leaf(torepr(as-name))]), none)]))
            ]))
      | s-lam(_, params, args, ret-ann, doc, _, _check) =>
        new-bindings = for fold(acc from bindings, param from params):
          acc.set(param.tostring(), param.tostring()) # TODO: Fix this
        end
        at-exp("function", some(
            [list: leaf(torepr(name)) ] +
            ( if is-empty(params): [list: ]
              else: [list:  hash-key("params", sexp("list", params.map(lam(p): leaf(torepr(p.toname())) end))) ]
              end) +
            # [list:  hash-key("contract",
            #     sexp("a-arrow", args.map(lam(a): process-ann(a.ann, file, fields, new-bindings) end) +
            #       [list: process-ann(ret-ann, file, fields, new-bindings)]))] +
            process-checks(_check)),
          none)
      | s-id(_, id) =>
        spair("unknown-item", spair("name", name))
      | s-data-expr(_, data-name, _, params, _, variants, shared, _) =>
        if string-index-of(name, "is-") == 0:
          DU.snothing
        else:
          new-bindings = for fold(acc from bindings, param from params):
            acc.set(param.tostring(), param.tostring()) # TODO: Fix this
          end
          at-exp("data-spec",
            some([list:  leaf(torepr(data-name)) ] +
              ( if is-empty(params): [list: ]
                else: [list:  hash-key("params", sexp("list", params.map(lam(p): leaf(torepr(p.toname())) end))) ]
                end)),
            some([list:
                at-exp("variants", none, some(variants.map(lam(v):
                        cases(A.Variant) v:
                          | s-variant(_, _, variant-name, members, with-members) =>
                            at-exp("constr-spec",
                              some([list: leaf(torepr(variant-name))]),
                              some([list:
                                  at-exp("members", none, some(members.map(process-var-member(_, file, fields, new-bindings)))),
                                  at-exp("with-members", none, some(with-members.map(process-member(_, A.s-name(A.dummy-loc, data-name), file, fields, new-bindings))))
                                ]))
                          | s-singleton-variant(_, variant-name, with-members) =>
                            at-exp("singleton-spec",
                              some([list: leaf(torepr(variant-name))]),
                              some([list:
                                  at-exp("with-members", none, some(with-members.map(process-member(_, A.s-name(A.dummy-loc, data-name), file, fields, new-bindings))))
                                ]))
                        end
                      end))),
                at-exp("shared", none, some(shared.map(process-member(_, A.s-name(A.dummy-loc, name), file, fields, new-bindings))))
              ]))
        end
      | s-variant(_, _, variant-name, members, with-members) =>
        at-comment('ERROR: Unprocessed data constructor ' + variant-name)
      | s-singleton-variant(_,  variant-name, with-members) =>
        at-comment('ERROR: Singleton data variant ' + variant-name)
      | else =>
        spair("unknown-item",
          spair("name", name) ^ link(_, e.tosource().pretty(70).map(at-comment)))
    end
  end
  trimmed-name = trim-path(file)
  at-exp("docmodule", some([list: leaf(torepr(trimmed-name))]),
    some(
      ( if fields.ignored-vals.keys().length() > 0:
          [list:  at-comment("Ignored type testers"),
            at-exp("ignore", some([list: sexp("list", fields.ignored-vals.keys().map(lam(i):leaf(torepr(i))end))]), none)]
        else: [list: ]
        end) +
      ( if fields.unknown-vals.keys().length() > 0:
          [list:  at-comment("Unknown: PLEASE DOCUMENT"),
            at-exp("ignore", some([list: sexp("list", fields.unknown-vals.keys().map(lam(i):leaf(torepr(i))end))]), none)]
        else: [list: ]
        end) +
      ( if fields.imports.keys().length() > 0:
          [list: at-exp("section", some([list: hash-key("tag", tag-name([list: trimmed-name, "ReExports"]))]),
              some([list: leaf("Re-exported values")]))]
            + fields.imports.keys().map(lam(k): process-item(k, fields.imports.get(k)) end)
        else: [list: ]
        end) +
      ( if fields.data-vals.keys().length() > 0:
          [list: at-exp("section", some([list: hash-key("tag", tag-name([list: trimmed-name, "DataTypes"]))]),
              some([list: leaf("Data types")]))]
            + fields.data-vals.keys().map(lam(k): process-item(k, fields.data-vals.get(k)) end)
        else: [list: ]
        end) +
      ( if fields.fun-vals.keys().length() > 0:
          [list: at-exp("section", some([list: hash-key("tag", tag-name([list: trimmed-name, "Functions"]))]),
              some([list: leaf("Functions")]))]
            + fields.fun-vals.keys().map(lam(k): process-item(k, fields.fun-vals.get(k)) end)
        else: [list: ]
        end)
      ))
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
        scoped = R.desugar-scope(DC.desugar-no-checks(U.append-nothing-if-necessary(parsed).or-else(parsed)), CS.standard-builtins)
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
                output = toplevel([list:
                    hashlang("scribble/base"),
                    at-app("require", [list: leaf(torepr(relative-dir(file) + "scribble-api.rkt"))]),
                    process-module(file, fields, provides-types, bindings, type-bindings)
                  ])
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
