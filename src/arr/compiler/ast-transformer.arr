#|
Assume that all AST nodes are annotated, this file generates visitors
|#

import ast as A
import ast-visitors as AV
import parse-pyret as SP
import file as F
include either
import string-dict as D
import cmdline as C
import system as SYS

dummy = A.dummy-loc
success-code = 0
failure-code = 1

fun shared-member-has-visit(shared-members :: List<A.Member>) -> Boolean:
  for lists.any(member from shared-members):
    cases (A.Member) member:
      | s-method-field(_, name, _, _, _, _, _, _, _, _) => name == 'visit'
      | else => raise("impossible (I think)")
    end
  end
end

data SimplifiedVariant:
  | simplified-variant(name :: String, members :: List<A.Bind>)
  | simplified-singleton-variant(name :: String, members :: List<A.Bind>%(is-empty))
end

fun simplify-variant(v :: A.Variant) -> SimplifiedVariant:
  doc: ```
       Assume that AST variants are not "fancy"--no ref fields, etc. This
       function extracts the core information and convert it to a simplified
       variant
       ```
  cases (A.Variant) v.visit(AV.dummy-loc-visitor):
    | s-variant(_, _, name, members, _) => simplified-variant(name, members.map(_.bind))
    | s-singleton-variant(_, name, _) => simplified-singleton-variant(name, empty)
  end
end

fun make-name(s :: String) -> A.Name:
  A.s-name(dummy, s)
end

fun make-bind(s :: String) -> A.Bind:
  A.s-bind(dummy, false, make-name(s), A.a-blank)
end

fun make-id(s :: String) -> A.Expr:
  A.s-id(dummy, make-name(s))
end

fun make-visit-self(id :: A.Expr) -> A.Expr:
  A.s-app(dummy, A.s-dot(dummy, id, "visit"), [list: make-id("self")])
end

fun make-method-call(obj :: A.Expr, meth-name :: String, args :: List<A.Expr>) -> A.Expr:
  A.s-app(dummy, A.s-dot(dummy, obj, meth-name), args)
end

fun make-complex-visit(id :: A.Expr, meth :: String) -> A.Expr:
  make-method-call(
    id,
    meth,
    [list: make-visit-self(A.s-id(dummy, A.s-underscore(dummy)))])
end

fun is-loc(ann :: A.Ann) -> Boolean block:
  cases (A.Ann) ann:
    | a-name(_, name) =>
      cases (A.Name) name:
        | s-name(_, id) => id == "Loc"
        | else => false
      end
    | else => false
  end
end

fun strip-annotation(b :: A.Bind) -> A.Bind:
  cases (A.Bind) b:
    | s-bind(l :: A.Loc, shadows :: Boolean, id :: A.Name, ann :: A.Ann) =>
      A.s-bind(l, shadows, id, A.a-blank)
  end
end

fun bind-to-id(b :: A.Bind) -> A.Expr:
  A.s-id(dummy, b.id)
end

data ArgType:
  | arg-not-visitable
  | arg-visitable
  | arg-list
  | arg-option
end

fun main-real(
    import-type :: A.ImportType,
    in-file :: String,
    out-file :: String,
    requirements :: D.StringDict<Boolean>,
    is-strip-annotation :: Boolean,
    appendix :: Option<String>) -> Number block:

  p = SP.surface-parse(F.input-file(in-file).read-file(), in-file)
  shadow appendix = cases (Option) appendix:
    | none => empty
    | some(path) => SP.surface-parse(F.input-file(path).read-file(), path).block.stmts
  end

  var collected-variants = empty
  var collected-data-definitions = empty

  p.visit(AV.default-iter-visitor.{
    method s-data(self, l :: A.Loc, name :: String, params :: List<A.Name>, mixins :: List<A.Expr>, variants :: List<A.Variant>, shared-members :: List<A.Member>, _check-loc :: Option<A.Loc>, _check :: Option<A.Expr>) block:
      when shared-member-has-visit(shared-members) block:
        collected-variants := collected-variants + variants.map(simplify-variant)
        collected-data-definitions := link(name, collected-data-definitions)
      end
      true
    end
  })

  fun visitor-maker(name :: String, transformer :: (SimplifiedVariant -> A.Expr)) -> A.Expr:
    doc: ```
         Assume no variant member has `self` as its name, this makes a visitor
         ```
    methods = for map(variant from collected-variants):
      A.s-method-field(
        dummy,
        variant.name,
        empty, # TODO(Oak): not sure what this is
        link(
          make-bind("self"),
          if is-strip-annotation:
            variant.members.map(strip-annotation)
          else:
            variant.members
          end),
        A.a-blank,
        "",
        A.s-block(dummy, [list: transformer(variant)]),
        none,
        none,
        false)
    end
    A.s-let(dummy, make-bind(name), A.s-obj(dummy, methods), false)
  end

  fun get-arg-type(b :: A.Bind) -> ArgType:
    cases (A.Ann) b.ann:
      | a-blank => raise("You probably want to annotate " + tostring(b))
      | a-name(_, name) =>
        # assume this name is of type Name%(is-s-name)
        cases (A.Name) name:
          | s-name(_, s) =>
            if collected-data-definitions.member(s):
              arg-visitable
            else:
              arg-not-visitable
            end
          | else => raise("impossible (I think) " + tostring(name))
        end
      | a-app(_, ann, args) =>
        arg-type = cases (A.Ann) ann:
          | a-name(_, name) =>
            # assume this name is of type Name%(is-s-name)
            cases (A.Name) name:
              | s-name(_, s) =>
                ask:
                  | s == "List" then: arg-list
                  | s == "Option" then: arg-option
                  | otherwise: arg-not-visitable
                end
              | else => raise("impossible (I think) " + tostring(ann))
            end
          | else => raise("impossible (I think) " + tostring(ann))
        end
        cases (ArgType) arg-type:
          | arg-not-visitable => arg-not-visitable
          | arg-visitable => raise("impossible")
          | else =>
            # this is either List or Option, so args has exactly one element
            cases (A.Ann) args.get(0):
              | a-name(_, name) =>
                # assume this name is of type Name%(is-s-name)
                cases (A.Name) name:
                  | s-name(_, s) =>
                    if collected-data-definitions.member(s):
                      arg-type
                    else:
                      arg-not-visitable
                    end
                  | else => raise("impossible (I think) " + tostring(name))
                end
              | else => raise("impossible (I think) " + tostring(args.get(0)))
            end
        end
      | else => arg-not-visitable
    end
  end

  fun default-map-visitor-transform(variant :: SimplifiedVariant) -> A.Expr:
    cases (SimplifiedVariant) variant:
      | simplified-variant(name, members) =>
        member-args = members.map(lam(b :: A.Bind) -> A.Expr:
          id = bind-to-id(b)
          cases (ArgType) get-arg-type(b):
            | arg-not-visitable => id
            | arg-visitable => make-visit-self(id)
            | arg-list => make-complex-visit(id, "map")
            | arg-option => make-complex-visit(id, "and-then")
          end
        end)
        A.s-app(dummy, make-id(name), member-args)
      | simplified-singleton-variant(name, _) => make-id(name)
    end
  end

  fun default-iter-visitor-transform(variant :: SimplifiedVariant) -> A.Expr:
    cases (SimplifiedVariant) variant:
      | simplified-variant(name, members) =>
        member-args = for lists.filter-map(b from members):
          id = bind-to-id(b)
          cases (ArgType) get-arg-type(b):
            | arg-not-visitable => none
            | arg-visitable => some(make-visit-self(id))
            | arg-list => some(make-complex-visit(id, "all"))
            | arg-option =>
              make-complex-visit(id, "and-then")
                ^ make-method-call(_, "or-else", [list: A.s-bool(dummy, true)])
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
      | simplified-singleton-variant(name, _) => A.s-bool(dummy, true)
    end
  end

  fun dummy-loc-visitor-transform(variant :: SimplifiedVariant) -> A.Expr:
    cases (SimplifiedVariant) variant:
      | simplified-variant(name, members) =>
        member-args = members.map(lam(b :: A.Bind) -> A.Expr:
          id = bind-to-id(b)
          cases (ArgType) get-arg-type(b):
            | arg-not-visitable => id
            | arg-visitable => make-visit-self(id)
            | arg-list => make-complex-visit(id, "map")
            | arg-option => make-complex-visit(id, "and-then")
          end
        end)
        shadow member-args = for map2(bind from members, arg from member-args):
          # assume bind is a `s-bind`
          if is-loc(bind.ann):
            make-id("dummy-loc")
          else:
            arg
          end
        end
        A.s-app(dummy, make-id(name), member-args)
      | simplified-singleton-variant(name, _) => make-id(name)
    end
  end

  transformers = [D.string-dict:
    'default-map-visitor', default-map-visitor-transform,
    'default-iter-visitor', default-iter-visitor-transform,
    'dummy-loc-visitor', dummy-loc-visitor-transform,
  ]

  body = for lists.filter-map(vname from requirements.keys().to-list().sort()):
    if requirements.get-value(vname):
      some(visitor-maker(vname, transformers.get-value(vname)))
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
