provide *

import file("ast-transformer.arr") as AT
import ast-visitors as AV
import ast as A
import parse-pyret as SP
import file as F
include either
import string-dict as D

dummy = A.dummy-loc
term-loc-dummy = AT.make-node-1("wrap-loc", AT.make-id("dummy-loc"))

fun generate-ast-visitor(
    imports :: List<A.Import>,
    in-file :: String,
    out-file :: String,
    requirements :: List<String>,
    is-strip-annotation :: Boolean,
    transformer :: (List<AT.SimplifiedVariant>, List<A.Expr> -> List<A.Expr>)):

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

  fun ast-to-term-visitor-transform(variant :: AT.SimplifiedVariant) -> A.Expr:
    cases (AT.SimplifiedVariant) variant block:
      | simplified-variant(type-name, name, members) =>
        args = for map(b from members):
          # assume b is s-bind
          id = AT.bind-to-id(b)
          arg-type = get-arg-type(b.ann)
          if arg-type.is-visitable():
            cases (AT.VisitableArgType) arg-type:
              | arg-visitable => AT.make-visit-self(id)
              | arg-list(_) => AT.make-node-1("wrap-list", AT.make-complex-visit(id, "list"))
              | arg-option(_) => AT.make-node-1("wrap-option", AT.make-complex-visit(id, "option"))
            end
          else:
            fun make-node-complex(
              tag :: String,
              meth :: String,
              inner :: AT.Tag
            ) -> A.Expr:
              # assume inner is one level deep
              shadow inner = cases (AT.Tag) inner:
                | t-str => "wrap-str"
                | t-num => "wrap-num"
                | t-bool => "wrap-bool"
                | t-loc => "wrap-loc"
              end
              AT.make-node-1(
                tag,
                AT.make-method-call(id, meth, [list: AT.make-id(inner)]))
            end
            cases (AT.Tag) AT.get-tag(b.ann):
              | t-str => AT.make-node-1("wrap-str", id)
              | t-num => AT.make-node-1("wrap-num", id)
              | t-bool => AT.make-node-1("wrap-bool", id)
              | t-loc => AT.make-node-1("wrap-loc", id)
              | t-list(inner) => make-node-complex("wrap-list", "map", inner)
              | t-option(inner) => make-node-complex("wrap-option", "and-then", inner)
            end
          end
        end
        shadow args = cases (List) members:
          | empty => [list: term-loc-dummy]
          | link(first, rest) =>
            cases (AT.Tag) AT.get-tag(first.ann):
              | t-loc => args
              | else => link(term-loc-dummy, args)
            end
        end
        A.s-app(dummy, AT.make-id("wrap-surf"),
          [list: AT.make-str(name), AT.make-list(args)])
      | simplified-singleton-variant(_, name, _) =>
        A.s-app(dummy, AT.make-id("wrap-surf"),
          [list: AT.make-str(name), AT.make-list([list: term-loc-dummy])])
    end
  end
  transformers = [D.string-dict:
    'default-map-visitor', {
      transformer: default-map-visitor-transform,
      preamble: ```{
        method option(self, opt):
          opt.and-then(_.visit(self))
        end,
        method list(self, lst):
          lst.map(_.visit(self))
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
          opt.and-then(_.visit(self))
        end,
        method list(self, lst):
          lst.map(_.visit(self))
        end,
      }```
    },
    'ast-to-term-visitor', {
      transformer: ast-to-term-visitor-transform,
      preamble: ```{
        method option(self, opt):
          opt.and-then(_.visit(self))
        end,
        method list(self, lst):
          lst.map(_.visit(self))
        end,
      }```
    }
  ]

  body = for map(vname from requirements):
    obj = transformers.get-value(vname)
    AT.visitor-maker(
      collected-variants,
      vname,
      obj.transformer,
      is-strip-annotation,
      obj.preamble)
  end

  out-program =
    A.s-program(dummy,
      A.s-provide-all(dummy),
      A.s-provide-types-none(dummy),
      imports + [list:
        A.s-import(dummy, A.s-const-import(dummy, "global"), A.s-underscore(dummy)),
        A.s-import(dummy, A.s-const-import(dummy, "base"), A.s-underscore(dummy))],
      A.s-block(dummy, transformer(collected-variants, body)))

  as-string = '# THIS FILE IS AUTOMATICALLY GENERATED FROM autogenerate.arr. PLEASE DO NOT EDIT.\n' + out-program.tosource().pretty(80).join-str("\n")

  F.output-file(out-file, false).display(as-string)
end


fun get-stmts(s :: String) -> List<A.Expr>:
  SP.surface-parse(s, '').block.stmts
end

fun subst(s, t):
  s.visit(AV.default-map-visitor.{
    method s-template(self, _):
      t
    end
  })
end

fun write-ast-visitors() block:
  generate-ast-visitor(
    [list:
      A.s-include(dummy, A.s-const-import(dummy, 'ast')),
      A.s-include(dummy, A.s-const-import(dummy, 'lists')),
      A.s-include(dummy, A.s-const-import(dummy, 'option'))],
    'src/arr/trove/ast.arr',
    'src/arr/trove/ast-visitors.arr',
    [list: 'default-map-visitor', 'default-iter-visitor', 'dummy-loc-visitor'],
    true,
    lam(_, body): body end)

  generate-ast-visitor(
    [list:
      A.s-include(dummy, A.s-const-import(dummy, 'ast')),
      A.s-include(dummy, A.s-const-import(dummy, 'string-dict')),
      A.s-include(dummy, A.s-const-import(dummy, 'lists')),
      A.s-include(dummy, A.s-const-import(dummy, 'option')),
      A.s-include(dummy, A.s-const-import(dummy, 'json-structs')),
      A.s-include(dummy, A.s-const-import(dummy, 'srcloc')),
    ],
    'src/arr/trove/ast.arr',
    'src/arr/trove/resugar-visitor.arr',
    [list: 'ast-to-term-visitor'],
    true,
    lam(_, body) -> List<A.Expr> block:
      helpers-stmts = ```
aTYPE = "t"
aVALUE = "v"
aNAME = "n"
aPATTERNS = "ps"

aSTRING = "St"
aNUMBER = "N"
aBOOL = "B"
aLOC = "Lo"
aSURFACE = "S"
aCORE = "C"
aLIST = "L"
aNONE = "None"
aSOME = "Some"
aVAR = "Var"
aTAG = "Tag"
aPVAR = "PVar"
aDROP = "Drop"
aFRESH = "Fresh"
aCAPTURE = "Capture"
aEXT = "Ext"
aAUX = "Aux"
aMETA = "Meta"

fun wrap-str(s):
  {t: aSTRING, v: s}
end
fun wrap-num(n):
  {t: aNUMBER, v: num-to-string(n)}
end
fun wrap-bool(b):
  {t: aBOOL, v: b}
end
fun wrap-loc(l):
  {t: aLOC, v: l.serialize()}
end
fun wrap-surf(name, args):
  {t: aSURFACE, n: name, ps: builtins.raw-array-from-list(args)}
end
fun wrap-list(l):
  {t: aLIST, v: builtins.raw-array-from-list(l)}
end
fun wrap-option(opt):
  cases (Option) opt:
    | none => {t: aNONE}
    | some(v) => {t: aSOME, v: v}
  end
end
        ``` ^ get-stmts

      shadow body = A.s-block(dummy, body).visit(AV.default-map-visitor.{
        method s-method-field(self, l, name, params, args, ann, doc, body-expr, _check-loc, _check, blocky):
          # no recursion -- only the top one
          ask:
            | otherwise: body-expr
          end ^ A.s-method-field(l, name, params, args, ann, doc, _, _check-loc, _check, blocky)
        end
      }).stmts

      helpers-stmts + body
    end)

end
