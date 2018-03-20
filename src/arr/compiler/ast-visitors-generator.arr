#|
Assume that all AST nodes are annotated, this file generates visitors
|#

import file("ast-transformer.arr") as AT
import ast as A
import parse-pyret as SP
import file as F
include either
import string-dict as D
import cmdline as C
import system as SYS

dummy = A.dummy-loc
success-code = 0
failure-code = 1

fun main-real(
    import-type :: A.ImportType,
    in-file :: String,
    out-file :: String,
    requirements :: D.StringDict<Boolean>,
    is-strip-annotation :: Boolean,
    appendix :: Option<String>) -> Number block:

  p = AT.read(in-file)
  shadow appendix = cases (Option) appendix:
    | none => empty
    | some(path) => AT.read(path).block.stmts
  end

  {collected-variants; collected-data-definitions} = AT.collect-ast(p)
  shadow get-arg-type = AT.get-arg-type(_, collected-data-definitions, false)

  fun default-map-visitor-transform(variant :: AT.SimplifiedVariant) -> A.Expr:
    cases (AT.SimplifiedVariant) variant:
      | simplified-variant(_, name, members) =>
        member-args = members.map(lam(b :: A.Bind) -> A.Expr:
          # assume b is a s-bind
          id = AT.bind-to-id(b)
          cases (AT.ArgType) get-arg-type(b.ann):
            | arg-not-visitable => id
            | arg-visitable => AT.make-visit-self(id)
            | arg-list => AT.make-complex-visit(id, "map")
            | arg-option => AT.make-complex-visit(id, "and-then")
          end
        end)
        A.s-app(dummy, AT.make-id(name), member-args)
      | simplified-singleton-variant(_, name, _) => AT.make-id(name)
    end
  end

  fun default-iter-visitor-transform(variant :: AT.SimplifiedVariant) -> A.Expr:
    cases (AT.SimplifiedVariant) variant:
      | simplified-variant(_, name, members) =>
        member-args = for lists.filter-map(b from members):
          # assume b is a s-bind
          id = AT.bind-to-id(b)
          cases (AT.ArgType) get-arg-type(b.ann):
            | arg-not-visitable => none
            | arg-visitable => some(AT.make-visit-self(id))
            | arg-list => some(AT.make-complex-visit(id, "all"))
            | arg-option =>
              AT.make-complex-visit(id, "and-then")
                ^ AT.make-method-call(_, "or-else", [list: A.s-bool(dummy, true)])
                ^ some
          end
        end
        cases (List) member-args:
          | empty => A.s-bool(dummy, true)
          | link(f, r) =>
            # left recursion
            for fold(prev from f, e from r):
              A.s-op(dummy, dummy, "opand", prev, e)
            end
        end
      | simplified-singleton-variant(_, name, _) => A.s-bool(dummy, true)
    end
  end

  fun dummy-loc-visitor-transform(variant :: AT.SimplifiedVariant) -> A.Expr:
    cases (AT.SimplifiedVariant) variant:
      | simplified-variant(_, name, members) =>
        member-args = members.map(lam(b :: A.Bind) -> A.Expr:
          # assume b is a s-bind
          id = AT.bind-to-id(b)
          cases (AT.ArgType) get-arg-type(b.ann):
            | arg-not-visitable => id
            | arg-visitable => AT.make-visit-self(id)
            | arg-list => AT.make-complex-visit(id, "map")
            | arg-option => AT.make-complex-visit(id, "and-then")
          end
        end)
        shadow member-args = for map2(bind from members, arg from member-args):
          # assume bind is a `s-bind`
          if AT.is-loc(bind.ann):
            AT.make-id("dummy-loc")
          else:
            arg
          end
        end
        A.s-app(dummy, AT.make-id(name), member-args)
      | simplified-singleton-variant(_, name, _) => AT.make-id(name)
    end
  end

  transformers = [D.string-dict:
    'default-map-visitor', default-map-visitor-transform,
    'default-iter-visitor', default-iter-visitor-transform,
    'dummy-loc-visitor', dummy-loc-visitor-transform,
  ]

  shadow collected-variants = for filter(v from collected-variants):
    not(collected-data-definitions.get-value(v.type-name))
  end

  body = for lists.filter-map(vname from requirements.keys().to-list().sort()):
    if requirements.get-value(vname):
      AT.visitor-maker(
        collected-variants,
        vname,
        transformers.get-value(vname),
        is-strip-annotation) ^ some
    else:
      none
    end
  end + appendix

  out-program =
    A.s-program(dummy,
      A.s-provide-all(dummy),
      A.s-provide-types-none(dummy),
      [list:
        A.s-include(dummy, import-type),
        A.s-import(dummy, A.s-const-import(dummy, "global"), A.s-underscore(dummy)),
        A.s-import(dummy, A.s-const-import(dummy, "base"), A.s-underscore(dummy))],
      A.s-block(dummy, body))

  as-string = out-program.tosource().pretty(80).join-str("\n")

  F.output-file(out-file, false).display(as-string)
  success-code
end

fun main(args :: List<String>) -> Number block:
  options = [D.string-dict:
    "default-map-visitor",
      C.flag(C.once, "Generate default-map-visitor"),
    "default-iter-visitor",
      C.flag(C.once, "Generate default-iter-visitor"),
    "dummy-loc-visitor",
      C.flag(C.once, "Generate dummy-loc-visitor"),
    "in-file",
      C.next-val(C.String, C.required-once, "The input file (AST definition file)"),
    "out-file",
      C.next-val(C.String, C.required-once, "The output file (AST visitor file)"),
    "appendix-file",
      C.next-val(C.String, C.once, "The appendix file which will be concatenated to the output file"),
    "builtin-import",
      C.next-val(C.String, C.once, "The builtin module that contains AST (e.g., `ast`)"),
    "file-import",
      C.next-val(C.String, C.once, "The file module that contains AST. (e.g., `ast-anf`)"),
    "strip-annotation",
      C.flag(C.once, "Visitors should not have annotation"),
  ]
  cases(C.ParsedArguments) C.parse-args(options, args) block:
    | success(r, rest) =>
      import-type = ask block:
        | r.has-key("builtin-import") and r.has-key("file-import") then:
          print-error("choose either builtin or special\n")
          left(failure-code)
        | r.has-key("builtin-import") then:
          right(A.s-const-import(dummy, r.get-value("builtin-import")))
        | r.has-key("file-import") then:
          right(A.s-special-import(dummy, "file", [list: r.get-value("file-import")]))
        | otherwise:
          print-error("need either builtin or special\n")
          left(failure-code)
      end
      cases (Either) import-type block:
        | left(code) => code
        | right(shadow import-type) =>
          main-real(
            import-type,
            r.get-value('in-file'),
            r.get-value('out-file'),
            [D.string-dict:
              'default-map-visitor', r.has-key('default-map-visitor'),
              'default-iter-visitor', r.has-key('default-iter-visitor'),
              'dummy-loc-visitor', r.has-key('dummy-loc-visitor')],
            r.has-key('strip-annotation'),
            if r.has-key('appendix-file'):
              some(r.get-value('appendix-file'))
            else:
              none
            end)
      end
    | arg-error(message, partial) =>
      print-error(message + "\n")
      print-error(C.usage-info(options).join-str("\n") + '\n')
      failure-code
  end
end

exit-code = main(C.args)
SYS.exit-quiet(exit-code)
