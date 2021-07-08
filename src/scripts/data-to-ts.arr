import cmdline as C
import parse-pyret as P
import pprint as PP
import string-dict as SD
import file as F
import ast as A
cl-options = [SD.string-dict:
  "width",
    C.next-val-default(C.Num, 80, some("w"), C.once, "Pretty-printed width"),
]
parsed-options = C.parse-cmdline(cl-options)
println = lam(s) block:
  print(s)
  print("\n")
end
var width = 80
INDENT = 2
pp-empty-obj = PP.lbrace + PP.rbrace
fun obj(contents):
  PP.surround-separate(INDENT, 1, pp-empty-obj, PP.lbrace, PP.commabreak, PP.rbrace, contents)
end
var old-decls = [list: ]
var new-decls = [list: ]
find-all-data = A.default-iter-visitor.{
  method s-data(self, l, data-name, params, mixins, variants, shared-members, _check-loc, _check) block:
    typarams =
      if is-empty(params): PP.mt-doc
      else: PP.surround-separate(INDENT, 0, PP.mt-doc, PP.langle, PP.commabreak, PP.rangle,
          params.map(_.tosource()))
      end
    
    pp-vars = for map(v from variants) block:
      cases(A.Variant) v block:
        | s-singleton-variant(vl, name, with-members) =>
          {
            old: obj(
                [list:
                  PP.infix(INDENT, 0, A.str-colonspace, PP.str("$name"), PP.dquote(PP.str(name))),
                  PP.infix(INDENT, 0, A.str-colonspace, PP.str("dict"), pp-empty-obj)
                ]),
            anchor: PP.str("DataValueType<{}, DataMetaBase<'" + name + "'>>")
          }
        | s-variant(vl, constr-loc, name, members, with-members) =>
          field-types = for map(m from members):
            cases(A.VariantMember) m:
              | s-variant-member(vml, member-type, bind) =>
                cases(A.Bind) bind:
                  | s-bind(bl, shadows, id, ann) =>
                    pp-ann = cases(A.Ann) ann:
                      | a-name(_, ann-name) =>
                        shadow ann-name = ann-name.tosourcestring()
                        if ann-name == "String": PP.str("string")
                        else if ann-name == "Boolean": PP.str("boolean")
                        else if ann-name == "Any": PP.str("any")
                        else: PP.str(ann-name)
                        end
                      | a-blank => PP.str("unknown")
                      | a-any(_) => PP.str("any")
                      | else => ann.tosource()
                    end
                    PP.nest(INDENT, PP.squote(id.tosource()) + PP.str(": ") + pp-ann)
                end
            end
          end
          {
            old: obj([list:
                  PP.infix(INDENT, 0, A.str-colonspace, PP.str("$name"), PP.dquote(PP.str(name))),
                  PP.infix(INDENT, 0, A.str-colonspace, PP.str("dict"), obj(field-types))
                ]),
            anchor: PP.str("DataValueType<{}, DataMetaBase<'" + name + "'> & ") + obj(field-types) + PP.str(">")
          }
      end
    end
    only-old = pp-vars.map(_.old)
    only-new = pp-vars.map(_.anchor)
    old-decl = PP.infix(INDENT, 0, PP.str(" = "), PP.str("export type " + data-name) + typarams,
      A.str-pipespace + PP.separate(A.break-one + A.str-pipespace, only-old))
    old-decls := link(old-decl, old-decls)
    new-decl = PP.infix(INDENT, 0, PP.str(" = "), PP.str("export type " + data-name) + typarams,
      A.str-pipespace + PP.separate(A.break-one + A.str-pipespace, only-new))
    new-decls := link(new-decl, new-decls)
    true
  end
}
var constrs = [list: ]
var exports = [list:]
fun field(name, val):
  PP.infix(INDENT, 0, A.str-colonspace, PP.squote(PP.str(name)), val)
end
fun app(name, args):
  PP.group(PP.str(name) + PP.parens(PP.nest(INDENT, PP.separate(PP.commabreak, args))))
end
create-constrs = A.default-iter-visitor.{
  method s-data(self, l, data-name, params, mixins, variants, shared-members, _check-loc, _check) block:
    baseName = string-replace(data-name, "-", "_")
    exports := link(field("is-" + data-name, PP.str("PFunction<(val: any) => val is " + baseName + ">")), exports)
    typarams =
      if is-empty(params): PP.mt-doc
      else: PP.surround-separate(INDENT, 0, PP.mt-doc, PP.langle, PP.commabreak, PP.rangle,
          params.map(_.tosource()))
      end
    emptyMethods = obj([list: field("$methods", PP.braces(PP.mt-doc))])
    sharedBaseName = "sharedBase_" + baseName
    sharedBase = PP.infix(INDENT, 0, PP.str(" = "), PP.str("const " + sharedBaseName), emptyMethods)
    constrs := link(sharedBase, constrs)
    ppvars = for map(v from variants) block:
      exports := link(field("is-" + v.name, PP.str("PFunction<(val: any) => val is TCH.Variant<" + baseName + ", '" + v.name + "'>>")), exports)
      varName = string-replace(v.name, "-", "_")
      varBaseName = "variantBase_" + varName
      cases(A.Variant) v block:
        | s-singleton-variant(vl, name, with-members) =>
          variant = PP.infix(INDENT, 0, PP.str(" = "),
            PP.str("const " + varBaseName + " : TCH.Variant<" + baseName + ", '" + name + "'>"),
            app("PRIMITIVES.createVariant", [list:
                PP.str(sharedBaseName),
                emptyMethods,
                obj([list:
                    field("$data", PP.str(varBaseName)),
                    field("$name", PP.dquote(PP.str(name))),
                    field("$fieldNames", PP.str("null"))])]))
          constrs := link(variant, constrs)
      	  singleton-export = field(name, PP.str("TCH.Variant<" + baseName + ", '" + name + "'>"))
	        exports := link(singleton-export, exports)
          pred = PP.str(
            "export function is" + varName + "(val: any): boolean {\n" +
            "  return typeof val === 'object' && val !== null && val['$variant'] === " + sharedBaseName + ".$variant;\n" +
            "}")
          constrs := link(pred, constrs)
        | s-variant(vl, constr-loc, name, members, with-members) =>
          variant = PP.infix(INDENT, 0, PP.str(" = "), PP.str("const " + varBaseName),
            app("PRIMITIVES.createVariant", [list:
                PP.str(sharedBaseName),
                emptyMethods,
                obj([list:
                    field("$data", PP.str(sharedBaseName)),
                    field("$name", PP.dquote(PP.str(name))),
                    field("$fieldNames", PP.brackets(PP.nest(INDENT,
                          PP.separate(PP.commabreak, members.map(lam(m): PP.dquote(m.bind.id.tosource()) end)))))])]))
          constrs := link(variant, constrs)
          constr-arglist = PP.nest(INDENT, PP.surround-separate(INDENT, 0, PP.str("()"),
              PP.lparen, PP.commabreak, PP.rparen,
              for map(m from members):
                cases(A.VariantMember) m:
                  | s-variant-member(vml, member-type, bind) =>
                    cases(A.Bind) bind:
                      | s-bind(bl, shadows, id, ann) =>
                        pp-ann = cases(A.Ann) ann:
                          | a-name(_, ann-name) =>
                            shadow ann-name = ann-name.tosourcestring()
                            if ann-name == "String": PP.str("string")
                            else if ann-name == "Boolean": PP.str("boolean")
                            else if ann-name == "Any": PP.str("any")
                            else: PP.str(ann-name)
                            end
                          | a-blank => PP.str("unknown")
                          | a-any(_) => PP.str("any")
                          | else => ann.tosource()
                        end
                        PP.nest(INDENT,
                          PP.str(string-replace(id.tosourcestring(), "-", "_")) + PP.str(": ") + pp-ann)
                    end
                end
              end))
          constr-header = PP.group(
            PP.str("export function " + varName) + constr-arglist
              + PP.str(": ") + PP.str("TCH.Variant<" + data-name + ", '" + name + "'>"))
          variant-export = field(name, PP.surround(INDENT, 1, PP.str("PFunction<"),
            constr-arglist + PP.str(" => ") + PP.str("TCH.Variant<" + data-name + ", '" + name + "'>"),
            PP.str(">")))
          exports := link(variant-export, exports)
          constr = PP.surround(INDENT, 1, constr-header + PP.str(" {"),
            PP.str("return ") + app("PRIMITIVES.makeDataValue", [list:
                PP.str(varName),
                obj(
                  for map(m from members):
                    field(
                      m.bind.id.tosourcestring(),
                      PP.str(string-replace(m.bind.id.tosourcestring(), "-", "_")))
                      end
                  )])
              + PP.str(";"), PP.str("}"))
          constrs := link(constr, constrs)
          pred = PP.str(
            "export function is" + varName + "(val: any): boolean {\n" +
            "  return typeof val === 'object' && val !== null && val['$variant'] === " + varBaseName + ".$variant;\n" +
            "}")
          constrs := link(pred, constrs)
      end
    end
    true
  end
}

cases(C.ParsedArguments) parsed-options block:
  | success(opts, rest) =>
    width := opts.get-value("width")
    cases(List) rest block:
      | empty => println("Require a file name")
      | link(file, _) =>
        ast = P.surface-parse(F.file-to-string(file), file)
        ast.visit(find-all-data)
        ast.visit(create-constrs)
        println("///////////////////////////// OLD Types ///////////////////////////")
        for each(d from old-decls.reverse()) block:
          d.pretty(width).each(println)
          println("")
        end
        println("/////////////////////////// Exports //////////////////////////")
	println("export interface Exports {")
	println("dict: {values: {dict: {")
        for each(c from exports.reverse()) block:
          c.pretty(width).each(println)
          println("")
        end
	println("}}}}")
        println("///////////////////////////// NEW Types ///////////////////////////")
        for each(d from new-decls.reverse()) block:
          d.pretty(width).each(println)
          println("")
        end
        println("/////////////////////////// Constructors //////////////////////////")
        for each(c from constrs.reverse()) block:
          c.pretty(width).each(println)
          println("")
        end
    end
end
