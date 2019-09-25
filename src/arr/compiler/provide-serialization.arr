provide *

import file("js-ast.arr") as J
import file("concat-lists.arr") as CL
import file("compile-structs.arr") as CS
import file("type-structs.arr") as T
import srcloc as SL
import string-dict as D

type CList = CL.ConcatList
clist = CL.clist
cl-empty = CL.concat-empty
cl-sing = CL.concat-singleton
cl-append = CL.concat-append
cl-cons = CL.concat-cons
cl-snoc = CL.concat-snoc

j-fun = J.j-fun
j-var = J.j-var
j-id = J.j-id
j-method = J.j-method
j-block = J.j-block
j-block1 = J.j-block1
j-bool = J.j-bool
j-true = J.j-true
j-false = J.j-false
j-num = J.j-num
j-str = J.j-str
j-return = J.j-return
j-assign = J.j-assign
j-if = J.j-if
j-if1 = J.j-if1
j-new = J.j-new
j-app = J.j-app
j-list = J.j-list
j-obj = J.j-obj
j-dot = J.j-dot
j-bracket = J.j-bracket
j-field = J.j-field
j-dot-assign = J.j-dot-assign
j-bracket-assign = J.j-bracket-assign
j-try-catch = J.j-try-catch
j-throw = J.j-throw
j-expr = J.j-expr
j-binop = J.j-binop
j-and = J.j-and
j-or = J.j-or
j-lt = J.j-lt
j-eq = J.j-eq
j-neq = J.j-neq
j-geq = J.j-geq
j-unop = J.j-unop
j-decr = J.j-decr
j-incr = J.j-incr
j-not = J.j-not
j-typeof = J.j-typeof
j-instanceof = J.j-instanceof
j-ternary = J.j-ternary
j-null = J.j-null
j-undefined = J.j-undefined
j-parens = J.j-parens
j-switch = J.j-switch
j-case = J.j-case
j-default = J.j-default
j-label = J.j-label
j-break = J.j-break
j-continue = J.j-continue
j-while = J.j-while
j-for = J.j-for
j-raw-code = J.j-raw-code


fun srcloc-to-raw(l):
  cases(SL.Srcloc) l:
    | builtin(uri) => j-list(true, [clist: j-str(uri)])
    | srcloc(uri, sl, sc, si, el, ec, ei) =>
      j-list(true, [clist: j-str(uri), j-num(sl), j-num(sc), j-num(si), j-num(el), j-num(ec), j-num(ei)])
  end
end

fun cl-map-sd(f, sd):
  for D.fold-keys(acc from cl-empty, key from sd):
    cl-cons(f(key), acc)
  end
end

fun compile-origin(bo):
  j-obj([clist:
    j-field("local-bind-site", srcloc-to-raw(bo.local-bind-site)),
    j-field("definition-bind-site", srcloc-to-raw(bo.definition-bind-site)),
    j-field("new-definition", j-bool(bo.new-definition)),
    j-field("uri-of-definition", j-str(bo.uri-of-definition))
  ])
end

fun compile-type-member(name, typ):
  j-field(name, compile-provided-type(typ))
end

fun compile-type-variant(variant):
  cases(T.TypeVariant) variant:
    | t-variant(name, members, with-members, l) =>
      compiled-members = j-list(false, CL.map_list(lam({mem-name; typ}):
          if T.is-t-ref(typ):
            j-list(true, [clist: j-str("ref"), j-str(mem-name), compile-provided-type(typ.typ)])
          else:
            j-list(true, [clist: j-str(mem-name), compile-provided-type(typ)])
          end
        end, members))
      compiled-with-members = j-obj(for cl-map-sd(mem-name from with-members):
            compile-type-member(mem-name, with-members.get-value(mem-name))
          end)
      j-list(true, [clist: j-str(name), compiled-members, compiled-with-members])
    | t-singleton-variant(name, with-members, l) =>
      compiled-with-members = j-obj(for cl-map-sd(mem-name from with-members):
          compile-type-member(mem-name, with-members.get-value(mem-name))
        end)
      j-list(true, [clist: j-str(name), compiled-with-members])
  end
end


fun compile-provided-data(typ :: T.DataType):
  cases(T.DataType) typ:
    | t-data(name, params, variants, members, l) =>
      j-list(false,
        [clist: j-str("data"), j-str(name),
          j-list(false, for CL.map_list(p from params):
              j-str(p.id.key())
            end),
          j-list(false, CL.map_list(compile-type-variant, variants)),
          j-obj(for cl-map-sd(mem-name from members):
            compile-type-member(mem-name, members.get-value(mem-name))
          end)])
  end
end

fun compile-provided-type(typ):
  cases(T.Type) typ:
    | t-name(mod-name, id, l, _) =>
      cases(T.NameOrigin) mod-name:
        | local => j-obj([clist:
              j-field("tag", j-str("name")),
              j-field("origin", j-obj([clist: j-field("import-type", j-str("$ELF"))])),
              j-field("name", j-str(id.toname()))]) # TODO: toname or key?
        | module-uri(uri) =>
          j-obj([clist:
              j-field("tag", j-str("name")),
              j-field("origin", j-obj([clist: j-field("import-type", j-str("uri")), j-field("uri", j-str(uri))])),
              j-field("name", j-str(id.toname()))]) # TODO: toname or key?
        | dependency(dep) =>
          raise("Dependency-origin names in provided-types shouldn't be possible")
      end
    | t-var(name, l, _) => j-list(true, [clist: j-str("tid"), j-str(name.key())]) # NOTE(joe): changed to .key()
    | t-arrow(args, ret, l, _) =>
      j-list(true,
        [clist: j-str("arrow"),
          j-list(true, CL.map_list(compile-provided-type, args)), compile-provided-type(ret)])
    | t-app(base, args, l, _) =>
      j-list(false,
        [clist: j-str("tyapp"), compile-provided-type(base),
          j-list(true, CL.map_list(compile-provided-type, args))])
    | t-top(_, _) => j-str("tany")
    | t-bot(_, _) => j-str("tbot")
    | t-record(fields, l, _) =>
      j-list(false,
        [clist: j-str("record"), j-obj(for cl-map-sd(key from fields):
              compile-type-member(key, fields.get-value(key))
            end)])
    | t-tuple(elts, l, _) =>
      j-list(false,
        [clist: j-str("tuple"), j-list(false, CL.map_list(compile-provided-type, elts))])
    | t-forall(params, body, l, _) =>
      j-list(true,
        [clist: j-str("forall"),
          j-list(false, for CL.map_list(p from params):
            j-str(p.id.key())
          end), compile-provided-type(body)])
      # | t-ref(_, _) =>
      # | t-existential(_, _) =>
    | t-data-refinement(base-typ, variant-name, l, _) =>
      j-list(true,
        [clist: j-str("data%"), compile-provided-type(base-typ), j-str(variant-name)])
    | else => j-ternary(j-false, j-str(tostring(typ)), j-str("tany"))
  end
end

fun compile-provides(provides):
  cases(CS.Provides) provides:
    | provides(thismod-uri, modules, values, aliases, data-defs) =>
      module-fields = for cl-map-sd(m from modules):
        j-field(m, j-obj([clist: j-field("uri", j-str(modules.get-value(m)))]))
      end
      value-fields = for cl-map-sd(v from values):
        cases(CS.ValueExport) values.get-value(v):
          | v-alias(origin, name) =>
            j-field(v, j-obj([clist:
              j-field("bind", j-str("alias")),
              j-field("origin", compile-origin(origin)),
              j-field("original-name", j-str(name)),
              j-field("typ", j-false)
            ]))
          | v-just-type(origin, t) => j-field(v, j-obj([clist:
              j-field("bind", j-str("let")),
              j-field("origin", compile-origin(origin)),
              j-field("typ", compile-provided-type(t))
            ]))
          | v-var(origin, t) => j-field(v, j-obj([clist:
              j-field("bind", j-str("var")),
              j-field("origin", compile-origin(origin)),
              j-field("typ", compile-provided-type(t))
            ]))
          | v-fun(origin, t, name, flatness) =>
            j-field(v, j-obj([clist:
              j-field("bind", j-str("fun")),
              j-field("origin", compile-origin(origin)),
              j-field("flatness", flatness.and-then(j-num).or-else(j-false)),
              j-field("name", j-str(name)),
              j-field("typ", compile-provided-type(t))
            ]))
        end
      end
      data-fields = for cl-map-sd(d from data-defs):
        j-field(d, compile-provided-data(data-defs.get-value(d)))
      end
      alias-fields = for cl-map-sd(a from aliases):
        j-field(a, compile-provided-type(aliases.get-value(a)))
      end
      j-obj([clist:
          j-field("modules", j-obj(module-fields)),
          j-field("values", j-obj(value-fields)),
          j-field("datatypes", j-obj(data-fields)),
          j-field("aliases", j-obj(alias-fields))
        ])
  end
end
