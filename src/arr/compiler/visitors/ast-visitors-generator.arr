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
    is-strip-annotation :: Boolean) -> Number block:

  p = AT.read(in-file)
  {collected-variants; collected-data-definitions} = AT.collect-ast(p)
  shadow get-arg-type = AT.get-arg-type(_, collected-data-definitions)

  fun default-map-visitor-transform(variant :: AT.SimplifiedVariant) -> A.Expr:
    cases (AT.SimplifiedVariant) variant:
      | simplified-variant(_, name, members) =>
        member-args = members.map(lam(b :: A.Bind) -> A.Expr:
          id = AT.bind-to-id(b)
          # assume that arg-list and arg-option is one level deep
          arg-type = get-arg-type(b.ann)
          if arg-type.is-visitable():
            cases (AT.VisitableArgType) arg-type:
              | arg-visitable => AT.make-visit-self(id)
              | arg-list(_) => AT.make-complex-visit(id, "list")
              | arg-option(_) => AT.make-complex-visit(id, "option")
            end
          else:
            id
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
          # assume that arg-list and arg-option is one level deep
          id = AT.bind-to-id(b)
          arg-type = get-arg-type(b.ann)
          if arg-type.is-visitable():
            cases (AT.VisitableArgType) arg-type:
              | arg-visitable => some(AT.make-visit-self(id))
              | arg-list(_) => some(AT.make-complex-visit(id, "list"))
              | arg-option(_) => some(AT.make-complex-visit(id, "option"))
            end
          else:
            none
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
          # assume that arg-list and arg-option is one level deep
          id = AT.bind-to-id(b)
          arg-type = get-arg-type(b.ann)
          if arg-type.is-visitable():
            cases (AT.VisitableArgType) arg-type:
              | arg-visitable => AT.make-visit-self(id)
              | arg-list(_) => AT.make-complex-visit(id, "list")
              | arg-option(_) => AT.make-complex-visit(id, "option")
            end
          else:
            id
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
        shadow member-args = ask:
          | name == 's-lam' then: member-args.set(1, A.s-str(dummy, ''))
          | otherwise: member-args
        end
        A.s-app(dummy, AT.make-id(name), member-args)
      | simplified-singleton-variant(_, name, _) => AT.make-id(name)
    end
  end

  transformers = [D.string-dict:
    'default-map-visitor', {
      transformer: default-map-visitor-transform,
      preamble: ```{
        method option(self, opt):
          cases(Option) opt:
            | none => none
            | some(v) => some(v.visit(self))
          end
        end,
        method list(self, lst):
          cases(List) lst:
            | empty => empty
            | link(f, r) => link(f.visit(self), self.list(r))
          end
        end,
      }```
    },
    'default-iter-visitor', {
      transformer: default-iter-visitor-transform,
      preamble: ```{
        method option(self, opt):
          cases(Option) opt:
            | none => true
            | some(v) => v.visit(self)
          end
        end,
        method list(self, lst):
          cases(List) lst:
            | empty => true
            | link(f, r) => f.visit(self) and self.list(r)
          end
        end,
      }```
    },
    'dummy-loc-visitor', {
      transformer: dummy-loc-visitor-transform,
      preamble: ```{
        method option(self, opt):
          cases(Option) opt:
            | none => none
            | some(v) => some(v.visit(self))
          end
        end,
        method list(self, lst):
          cases(List) lst:
            | empty => empty
            | link(f, r) => link(f.visit(self), self.list(r))
          end
        end,
      }```
    }
  ]

  body = for lists.filter-map(vname from requirements.keys().to-list().sort()):
    if requirements.get-value(vname):
      obj = transformers.get-value(vname)
      AT.visitor-maker(
        collected-variants,
        vname,
        obj.transformer,
        is-strip-annotation,
        obj.preamble) ^ some
    else:
      none
    end
  end

  out-program =
    A.s-program(dummy,
      A.s-provide-all(dummy),
      A.s-provide-types-none(dummy),
      [list:
        A.s-include(dummy, import-type),
        A.s-import(dummy, A.s-const-import(dummy, "global"), A.s-underscore(dummy)),
        A.s-import(dummy, A.s-const-import(dummy, "base"), A.s-underscore(dummy))],
      A.s-block(dummy, body))

  as-string = '# THIS FILE IS AUTOMATICALLY GENERATED FROM ast-visitors-generator.arr. PLEASE DO NOT EDIT.\n' + out-program.tosource().pretty(80).join-str("\n")

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
            r.has-key('strip-annotation'))
      end
    | arg-error(message, partial) =>
      print-error(message + "\n")
      print-error(C.usage-info(options).join-str("\n") + '\n')
      failure-code
  end
end

exit-code = main(C.args)
SYS.exit-quiet(exit-code)
