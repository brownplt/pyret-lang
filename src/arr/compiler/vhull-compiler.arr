provide *
import ast as A
import string-dict as D
import srcloc as SL
import sets as S
import sha as sha
import file("ast-anf.arr") as N
import file("js-ast.arr") as J
import file("gensym.arr") as G
import file("compile-structs.arr") as CS
import file("concat-lists.arr") as CL
import file("js-dag-utils.arr") as DAG
import file("ast-util.arr") as AU
import file("type-structs.arr") as T

string-dic = D.string-dict
mutable-string-dict = D.mutable-string-dict

clist = CL.clist
type Loc = SL.Srcloc
type CList = CL.ConcatList
data BindType:
  | b-let(value :: N.ABind)
  | b-array(value :: N.ABind, idx :: Number)
end

fun get-exp(o): o.exp end
fun get-id(o): o.id end
fun get-name(o): o.name end
fun get-l(o): o.l end
fun get-bind(o): o.bind end
fun o-get-field(o): o.field end

cl-empty = CL.concat-empty
cl-sing = CL.concat-singleton
cl-append = CL.concat-append
cl-cons = CL.concat-cons
cl-snoc = CL.concat-snoc

fun cl-map-sd(f, sd):
  for D.fold-keys(acc from cl-empty, key from sd):
    cl-cons(f(key), acc)
  end
end

j-fun = J.j-fun
j-var = J.j-var
j-id = J.j-id
j-method = J.j-method
j-block = J.j-block
j-block1 = J.j-block1
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
j-instanceof = J.j-instanceof
j-ternary = J.j-ternary
j-null = J.j-null
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
make-label-sequence = J.make-label-sequence

c-exp = DAG.c-exp
c-field = DAG.c-field
c-block = DAG.c-block
is-c-exp = DAG.is-c-exp
is-c-field = DAG.is-c-field
is-c-block = DAG.is-c-block

fun ann-loc(ann):
  if A.is-a-blank(ann): A.dummy-loc
  else: ann.l
  end
end

rt-name-map = [D.string-dict:
  "addModuleToNamespace", "aMTN",
  "checkArityC", "cAC",
  "checkRefAnns", "cRA",
  "derefField", "dF",
  "getColonFieldLoc", "gCFL",
  "getDotAnn", "gDA",
  "getField", "gF",
  "getFieldRef", "gFR",
  "hasBrand", "hB",
  "isActivationRecord", "isAR",
  "isCont", "isC",
  "isFunction", "isF",
  "isMethod", "isM",
  "isPyretException", "isPE",
  "isPyretTrue", "isPT",
  "makeActivationRecord", "mAR",
  "makeBoolean", "mB",
  "makeBranderAnn", "mBA",
  "makeCont", "mC",
  "makeDataValue", "mDV",
  "makeFunction", "mF",
  "makeGraphableRef", "mGR",
  "makeMatch", "mM",
  "makeMethod", "mMet",
  "makeMethodN", "mMN",
  "makeObject", "mO",
  "makePredAnn", "mPA",
  "makeRecordAnn", "mRA",
  "makeTupleAnn", "mTA",
  "makeVariantConstructor", "mVC",
  "namedBrander", "nB",
  "traceEnter", "tEn",
  "traceErrExit", "tErEx",
  "traceExit", "tEx",
  '_checkAnn', '_cA'
]


fun const-id(name :: String):
  A.s-name(A.dummy-loc, name)
end

fun compiler-name(id):
  const-id(string-append("$", id))
end

js-names = A.MakeName(0)
js-ids = D.make-mutable-string-dict()
effective-ids = D.make-mutable-string-dict()

fun fresh-id(id :: A.Name) -> A.Name:
  base-name = if A.is-s-type-global(id): id.tosourcestring() else: id.toname() end
  no-hyphens = string-replace(base-name, "-", "$")
  n = js-names.make-atom(no-hyphens)
  if effective-ids.has-key-now(n.tosourcestring()) block: #awkward name collision!
    fresh-id(id)
  else:
    effective-ids.set-now(n.tosourcestring(), true)
    n
  end
end

fun js-id-of(id :: A.Name) -> A.Name:
  s = id.key()
  if js-ids.has-key-now(s) block:
    js-ids.get-value-now(s)
  else:
    safe-id = fresh-id(id)
    js-ids.set-now(s, safe-id)
    safe-id
  end
end

fun formal-shadow-name(id :: A.Name) -> A.Name:
  js-id = js-id-of(id)
  A.s-name(A.dummy-loc, string-append("$", js-id.tosourcestring()))
end

fun wrap-with-srcnode(l, expr :: J.JExpr):
  cases(Loc) l:
    | builtin(name) => expr
    | srcloc(source, _, _, _, _, _, _) =>
      J.j-sourcenode(l, source, expr)
  end
end

get-field-loc = j-id(const-id("G"))
throw-uninitialized = j-id(const-id("U"))
source-name = j-id(const-id("M"))
undefined = j-id(const-id("D"))
RUNTIME = j-id(const-id("R"))
NAMESPACE = j-id(const-id("NAMESPACE"))
THIS = j-id(const-id("this"))
ARGUMENTS = j-id(const-id("arguments"))

fun rt-field(name): j-dot(RUNTIME, name) end

fun rt-method(name, args):
  rt-name = cases(Option) rt-name-map.get(name):
    | none => name
    | some(short-name) => short-name
  end

  j-method(RUNTIME, rt-name, args)
end

fun app(l, f, args):
  cases(SL.Srcloc) l:
    | builtin(n) => j-method(f, "app", args)
    | else =>
        J.j-sourcenode(l, l.source, j-method(f, "app", args))
  end
end

fun make-fun-name(compiler, loc) -> String:
  "_" + sha.sha256(compiler.uri) + "__" + num-to-string(compiler.get-loc-id(loc))
end

fun type-name(str :: String) -> String:
  string-append("$type$", str)
end

fun raise-id-exn(loc, name):
  j-app(throw-uninitialized, [clist: loc, j-str])
end

fun import-key(i): AU.import-to-dep-anf(i).key() end

# TODO(rachit): Factor these out from the anf-loop-compiler into compiler-utils
fun mk-abbrevs(l):
  loc = const-id("loc")
  name = const-id("name")
  [clist:
    j-var(const-id("G"), rt-field("getFieldLoc")),
    j-var(const-id("U"), j-fun(J.next-j-fun-id(), "throw_error", [clist: loc, name],
        j-block1(j-expr(j-method(rt-field("ffi"), "throwUninitializedIdMkLoc",
            [clist: j-id(loc), j-id(name)]))))),
    j-var(const-id("M"), j-str(l.source)),
    j-var(const-id("D"), rt-field("undefined"))
  ]
end

fun obj-of-loc(l):
  cases(Loc) l:
    | builtin(name) => j-list(false, [clist: j-str(name)])
    | srcloc(_, start-line, start-col, start-char, end-line, end-col, end-char) =>
      j-list(false, [clist:
          j-id(const-id("M")),
          j-num(start-line),
          j-num(start-col),
          j-num(start-char),
          j-num(end-line),
          j-num(end-col),
          j-num(end-char)
        ])
  end
end

fun compile-type-variant(variant):
  # TODO -- support with-members
  cases(T.TypeVariant) variant:
    | t-variant(name, members, with-members, l) =>
      j-list(true, [clist: j-str(name),
        j-list(false, CL.map_list(lam({mem-name; typ}):
          if T.is-t-ref(typ):
            j-list(true, [clist: j-str("ref"), j-str(mem-name), compile-provided-type(typ.typ)])
          else:
            j-list(true, [clist: j-str(mem-name), compile-provided-type(typ)])
          end
        end, members))])
    | t-singleton-variant(name, with-members, l) =>
      j-list(true, [clist: j-str(name)])
  end
end

fun compile-type-member(name, typ):
  j-field(name, compile-provided-type(typ))
end

fun compile-provided-data(typ :: T.DataType):
  cases(T.DataType) typ:
    | t-data(name, params, variants, members, l) =>
      j-list(false,
        [clist: j-str("data"), j-str(name),
          j-list(false, for CL.map_list(p from params):
              j-str(tostring(p))
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
    | t-var(name, l, _) => j-list(true, [clist: j-str("tid"), j-str(name.toname())])
    | t-arrow(args, ret, l, _) =>
      j-list(true,
        [clist: j-str("arrow"),
          j-list(true, CL.map_list(compile-provided-type, args)), compile-provided-type(ret)])
    | t-app(base, args, l, _) =>
      j-list(false,
        [clist: j-str("tyapp"), compile-provided-type(base),
          j-list(true, CL.map_list(compile-provided-type, args))])
    | t-top(_, _) => j-str("tany")
      # | t-bot(_) =>
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
            j-str(tostring(p))
          end), compile-provided-type(body)])
      # | t-ref(_, _) =>
      # | t-existential(_, _) =>
      # | t-data-refinement(_, _, _) =>
    | else => j-ternary(j-false, j-str(tostring(typ)), j-str("tany"))
  end
end


fun compile-provides(provides):
  cases(CS.Provides) provides:
    | provides(thismod-uri, values, aliases, data-defs) =>
      value-fields = for cl-map-sd(v from values):
        cases(CS.ValueExport) values.get-value(v):
          | v-just-type(t) => j-field(v, compile-provided-type(t))
          | v-var(t) => j-field(v, j-obj([clist:
              j-field("bind", j-str("var")),
              j-field("typ", compile-provided-type(t))
            ]))
          | v-fun(t, name, flatness) =>
            j-field(v, j-obj([clist:
              j-field("bind", j-str("fun")),
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
          j-field("values", j-obj(value-fields)),
          j-field("datatypes", j-obj(data-fields)),
          j-field("aliases", j-obj(alias-fields))
        ])
  end
end

var total-time = 0

fun compile-ann(ann :: A.Ann, visitor) -> DAG.CaseResults%(is-c-exp):
  cases(A.Ann) ann:
    | a-name(_, n) => c-exp(j-id(js-id-of(n)), cl-empty)
    | a-type-var(_, _) => c-exp(rt-field("Any"), cl-empty)
    | a-arrow(_, _, _, _) => c-exp(rt-field("Function"), cl-empty)
    | a-method(_, _, _) => c-exp(rt-field("Method"), cl-empty)
    | a-app(l, base, _) => compile-ann(base, visitor)
    | a-record(l, fields) =>
      comp-fields =
        for fold(acc from {names: cl-empty, locs: cl-empty, fields: cl-empty, others: cl-empty},
            field from fields):
          compiled = compile-ann(field.ann, visitor)
          {
            names: cl-snoc(acc.names, j-str(field.name)),
            locs: cl-snoc(acc.locs, visitor.get-loc(field.l)),
            fields: cl-snoc(acc.fields, j-field(field.name, compiled.exp)),
            others: cl-append(acc.others, compiled.other-stmts)
          }
        end
      c-exp(
        rt-method("makeRecordAnn", [clist:
            j-list(false, comp-fields.names),
            j-list(false, comp-fields.locs),
            j-obj(comp-fields.fields)
          ]),
        comp-fields.others
        )
    | a-tuple(l, tuple-fields) =>
      comp-fields = for fold(acc from {locs: cl-empty, fields: cl-empty, others: cl-empty},
         field from tuple-fields):
       compiled = compile-ann(field, visitor)
       {
          locs: cl-snoc(acc.locs, visitor.get-loc(ann-loc(field))),
          fields: cl-snoc(acc.fields, compiled.exp),
          others: cl-append(acc.others, compiled.other-stmts)
       }
       end
     c-exp(
       rt-method("makeTupleAnn", [clist:
           j-list(false, comp-fields.locs),
           j-list(false, comp-fields.fields)
        ]),
       comp-fields.others
      )
    | a-pred(l, base, exp) =>
      name = cases(A.Expr) exp:
        | s-id(_, id) => id.toname()
        | s-id-letrec(_, id, _) => id.toname()
      end
      expr-to-compile = cases(A.Expr) exp:
        | s-id(l2, id) => N.a-id(l2, id)
        | s-id-letrec(l2, id, ok) => N.a-id-letrec(l2, id, ok)
      end
      compiled-base = compile-ann(base, visitor)
      compiled-exp = expr-to-compile.visit(visitor)
      c-exp(
        rt-method("makePredAnn", [clist: compiled-base.exp, compiled-exp.exp, j-str(name)]),
        cl-append(compiled-base.other-stmts, compiled-exp.other-stmts)
        )
    | a-dot(l, m, field) =>
      c-exp(
        rt-method("getDotAnn", [clist:
            visitor.get-loc(l),
            j-str(m.toname()),
            j-id(js-id-of(m)),
            j-str(field)]),
        cl-empty)
    | a-blank => c-exp(rt-field("Any"), cl-empty)
    | a-any(l) => c-exp(rt-field("Any"), cl-empty)
  end
end

fun compile-anns(visitor, binds :: List<N.ABind>):
  for lists.fold(acc from cl-empty, b from binds):
    if A.is-a-blank(b.ann) or A.is-a-any(b.ann):
      acc
    else if A.is-a-typle(b.ann) and b.ann.fields.all(lam(a): A.is-a-blank(a) or A.is-a-any(a) end):
      check-tup-len = j-block([clist:
        j-expr(rt-method("checkTupleBind",
          [clist:
            j-id(js-id-of(b.id)),
            j-num(b.ann.fields.length()),
            visitor.get-loc(b.ann.l)]))])
      cl-snoc(acc, check-tup-len)
    else:
      ann-result = fresh-id(compiler-name("ann-check"))
      compiled-ann = compile-ann(b.ann, visitor)
      new-case = j-block(cl-append(compiled-ann.other-stmts,
        [clist:
          j-var(ann-result, rt-method("_checkAnn",
            [clist:
              visitor.get-loc(b.ann.l),
              compiled-ann.exp,
              j-id(js-id-of(b.id))
            ]
          ))
        ]
      ))
      cl-snoc(acc, new-case)
    end
  end
end

fun compile-fun-body(l :: Loc, fun-name :: A.Name, compiler,
      args :: List<N.ABind>,
      body :: N.AExpr,
      is-flat :: Boolean) -> J.JBlock block:

  # All functions return the final answer using this variable.
  ans = fresh-id(compiler-name("ans"))
  local-compiler = compiler.{cur-ans: ans}

  # NOTE(rachit): Skipping the formal-args reassignment optimization here.
  visited-body :: DAG.CaseResults%(is-c-block) = body.visit(local-compiler)
  ann-checks = compile-anns(local-compiler, args)
  fun-body =
    cl-empty
    ^ cl-append(_, ann-checks)
    ^ cl-snoc(_, visited-body.block)
    ^ cl-append(_, visited-body.new-cases)

  j-block(
    cl-sing(j-var(local-compiler.cur-ans, undefined)) ^
    cl-append(_, fun-body) ^
    cl-append(_, cl-sing(j-return(j-id(ans)))))
end

# NOTE(rachit): Assumptions about the results of compilation:
#   - ALettable -> c-exp
#   - AExpr -> c-block
compiler-visitor = {
  method a-module(self, l, answer, dvs, dts, provides, types, checks):
    types-obj-fields = for fold(acc from {fields: cl-empty, others: cl-empty}, ann from types):
      compiled = compile-ann(ann.ann, self)
      {
        fields: cl-snoc(acc.fields, j-field(ann.name, compiled.exp)),
        others: cl-append(acc.others, compiled.other-stmts)
      }
    end

    compiled-provides = provides.visit(self)
    compiled-answer = answer.visit(self)
    compiled-checks = checks.visit(self)
    c-exp(
      rt-method("makeObject", [clist:
          j-obj([clist:
              j-field("answer", compiled-answer.exp),
              j-field("namespace", NAMESPACE),
              j-field("locations", j-id(const-id("L"))),
              j-field("defined-values",
                j-obj(
                  for CL.map_list(dv from dvs):
                    cases(N.ADefinedValue) dv:
                      | a-defined-value(name, value) =>
                        compiled-val = dv.value.visit(self).exp
                        j-field(dv.name, compiled-val)
                      | a-defined-var(name, id) =>
                        j-field(dv.name, j-id(js-id-of(id)))
                    end
                  end)),
              j-field("defined-types",
                j-obj(
                  for CL.map_list(dt from dts):
                    compiled-ann = compile-ann(dt.typ, self).exp
                    j-field(dt.name, compiled-ann)
                  end)),
              j-field("provide-plus-types",
                rt-method("makeObject", [clist: j-obj([clist:
                        j-field("values", compiled-provides.exp),
                        j-field("types", j-obj(types-obj-fields.fields))
                    ])])),
              j-field("checks", compiled-checks.exp)])]),

      types-obj-fields.others ^
      cl-append(_, compiled-provides.other-stmts) ^
      cl-append(_, compiled-answer.other-stmts) ^
      cl-append(_, compiled-checks.other-stmts))
  end,
  method a-type-let(self, l, bind, body):
    cases(N.ATypeBind) bind:
      | a-type-bind(l2, name, ann) =>
        visited-body = body.visit(self)
        compiled-ann = compile-ann(ann, self)
        c-block(
          j-block(
            compiled-ann.other-stmts ^
            cl-snoc(_, j-var(js-id-of(name), compiled-ann.exp)) ^
            cl-append(_, visited-body.block.stmts)),
          visited-body.new-cases)
      | a-newtype-bind(l2, name, nameb) =>
        brander-id = js-id-of(nameb)
        visited-body = body.visit(self)
        c-block(
          j-block(
            [clist:
              j-var(brander-id, rt-method("namedBrander", [clist: j-str(name.toname()), self.get-loc(l2)])),
              j-var(js-id-of(name), rt-method("makeBranderAnn", [clist: j-id(brander-id), j-str(name.toname())]))
            ] ^
            cl-append(_, visited-body.block.stmts)),
          visited-body.new-cases)
    end
  end,
  method a-let(self, _, b :: N.ABind, e :: N.ALettable, body :: N.AExpr):
    cases (N.ALettable) e:
      | a-if(l, p, c, a) =>
        visited-p = p.visit(self)
        visited-c = c.visit(self.{cur-ans: b.id})
        visited-a = a.visit(self.{cur-ans: b.id})
        c-block(j-block([clist:
              j-if(visited-p, visited-c, visited-a)
        ]), cl-empty)
      | a-cases(l, typ, val, branches, _else) =>
        raise("a-cases in a-let not implemented")
      | else => block:
        compiled-e :: DAG.CaseResults%(is-c-exp) = e.visit(self)
        compiled-body :: DAG.CaseResults%(is-c-block) = body.visit(self)
        if A.is-a-blank(b.ann) or A.is-a-any(b.ann):
          c-block(
            j-block(
              compiled-e.other-stmts ^
              cl-append(_, cl-sing(j-var(js-id-of(b.id), compiled-e.exp))) ^
              cl-append(_, compiled-body.block.stmts)),
            compiled-body.new-cases)
        else:
          compiled-ann = compile-ann(b.ann, self)
          ann-result = j-expr(rt-method("_checkAnn",
            [clist:
              self.get-loc(b.ann.l), compiled-ann.exp, j-id(js-id-of(b.id))]))
          c-block(
            j-block(
              compiled-e.other-stmts ^
              cl-append(_, cl-sing(j-var(js-id-of(b.id), compiled-e.exp))) ^
              cl-append(_, compiled-ann.other-stmts) ^
              cl-append(_, [clist: ann-result]) ^
              cl-append(_, compiled-body.block.stmts)),
            compiled-body.new-cases)
        end
      end
    end
  end,
  method a-arr-let(self, _, b :: N.ABind, idx :: Number, e :: N.ALettable,
    body :: N.AExpr):
    raise("a-arr-let not implemented")
  end,
  method a-var(self, l :: Loc, b :: N.ABind, e :: N.ALettable,
    body :: N.AExpr):
    compiled-body = body.visit(self)
    compiled-e = e.visit(self)
    c-block(
      j-block(
        j-var(js-id-of(b.id),
          j-obj([clist: j-field("$var", compiled-e.exp)
          ]))
        ^ cl-cons(_, compiled-body.block.stmts)),
      compiled-body.new-cases)
  end,
  method a-seq(self, _, e1, e2):
    raise("a-seq is not implemented")
    #|v-e1 = e1.visit(self)
    v-e2 = e2.visit(self)
    first-stmt = if J.is-JStmt(v-e1.exp): v-e1.exp else: j-expr(v-e1.exp) end
    c-block(
      j-block(
        cl-append(v-e1.other-stmts, cl-cons(first-stmt, v-e2.block.stmts)),
        v-e2.new-cases
      )
    )|#
  end,
  method a-if(self, l :: Loc, cond :: N.AVal, cons :: N.AExpr, alt :: N.AExpr):
    raise("a-if is not implemented")
    #|ccons = cons.visit(self)
    calt = alt.visit(self)
    ccond = rt-method("checkPyretTrue", [clist: cond.visit(self)])
    c-block(j-block([clist:
          j-if(ccond, ccons, calt)
    ]), cl-empty)|#
  end,
  method a-cases(self, l :: Loc, typ :: A.Ann, val :: N.AVal,
    branches :: List<N.ACasesBranch>, _else :: N.AExpr):
    raise("a-cases not implemented")
  end,
  method a-update(self, l, obj, fields):
    compiled-obj = obj.visit(self).exp
    compiled-fields = CL.map_list(_.value.visit(self).exp, fields)
    field-names = CL.map_list(j-str(_.name), fields)
    field-locs = CL.map_list(self.get-loc(_.l), fields)
    c-exp(j-expr(rt-method("checkRefAnns",
      [clist:
        compiled-obj,
        j-list(false, field-names),
        j-list(false, compiled-fields),
        j-list(false, field-locs),
        self.get-loc(l),
        self.get-loc(obj.l)
      ]
    )), cl-empty)
  end,
  method a-lettable(self, _, e :: N.ALettable):
    # TODO(rachit): This might have to change because of a-if
    visit-e :: DAG.CaseResults%(is-c-exp) = e.visit(self)
    c-block(
      j-block(visit-e.other-stmts ^
        cl-append(_, [clist:
          j-expr(j-assign(self.cur-ans, visit-e.exp))])),
      cl-empty)
  end,
  method a-assign(self, l :: Loc, id :: A.Name, value :: N.AVal):
    visit-value = value.visit(self)
    c-exp(rt-field("nothing"),
      cl-snoc(visit-value.other-stmts,
        j-expr(j-dot-assign(j-id(js-id-of(id)), "$var", visit-value.exp))))
  end,
  method a-app(self, l :: Loc, f :: N.AVal, args :: List<N.AVal>, app-info):
    compiled-f = f.visit(self).exp
    compiled-args = CL.map_list(lam(a): a.visit(self).exp end, args)
    # NOTE(rachit): Always assigning to cur-ans here.
    c-exp(
      j-assign(self.cur-ans, app(l, compiled-f, compiled-args)),
      cl-empty)
  end,
  method a-method-app(self, l, obj, meth, args):
    compiled-obj :: J.JExpr = obj.visit(self).exp
    compiled-args = CL.map_list(lam(a): a.visit(self).exp end, args)

    if J.is-j-id(compiled-obj):
      call = wrap-with-srcnode(l,
        rt-method("maybeMethodCall",
          cl-append([clist: compiled-obj,
            j-str(meth),
            self.get-loc(l)],
          compiled-args)))
      # NOTE(rachit): Always assigning to cur-ans here.
      c-exp(
        j-assign(self.cur-ans, call),
        cl-empty)
    else:
      ...
    end
  end,
  method a-prim-app(self, l :: Loc, f :: String, args :: List<N.AVal>):
    visit-args = args.map(_.visit(self))
    set-loc = [clist:
      j-expr(j-assign(self.cur-apploc, self.get-loc(l)))
    ]
    c-exp(rt-method(f, CL.map_list(get-exp, visit-args)), set-loc)
  end,
  method a-ref(self, l, maybe-ann):
    cases(Option) maybe-ann:
      | none => c-exp(rt-method("makeGraphableRef", cl-empty), cl-empty)
      | some(ann) => raise("Cannot handle annotations in refs yet")
    end
  end,
  method a-obj(self, l :: Loc, fields :: List<N.AField>):
    visit-fields = fields.map(_.visit(self))
    c-exp(rt-method("makeObject",
        [clist: j-obj(CL.map_list(o-get-field, visit-fields))]), cl-empty)
  end,
  method a-get-bang(self, l :: Loc, obj :: N.AVal, field :: String):
    visit-obj = obj.visit(self)
    c-exp(
      rt-method("getFieldRef", [clist:
          visit-obj.exp,
          j-str(field),
          self.get-loc(l)
        ]),
      visit-obj.other-stmts)
  end,
  method a-extend(self, l :: Loc, obj :: N.AVal, fields :: List<N.AField>):
    visit-obj = obj.visit(self)
    visit-fields = fields.map(_.visit(self))
    c-exp(
      rt-method("extendObj",
        [clist:
          self.get-loc(l),
          visit-obj.exp,
          j-obj(CL.map_list(o-get-field, visit-fields))]),
      cl-empty)
  end,
  method a-dot(self, l :: Loc, obj :: N.AVal, field :: String):
    raise("a-dot not implemented")
  end,
  method a-colon(self, l :: Loc, obj :: N.AVal, field :: String):
    raise("a-colon not implemented")
  end,
  method a-method(self, l :: Loc, name :: String,
    args :: List<N.ABind>, ret :: A.Ann, body :: N.AExpr):
    raise("a-method not implemented")
  end,
  method a-val(self, l :: Loc, v :: N.AVal):
    v.visit(self)
  end,
  method a-field(self, l :: Loc, name :: String, value :: N.AVal):
    visit-v = value.visit(self)
    c-field(j-field(name, visit-v.exp), visit-v.other-stmts)
  end,
  method a-tuple(self, l, values):
    visit-vals = values.map(_.visit(self))
    c-exp(
      rt-method("makeTuple",
        [clist: j-list(false,
          CL.map_list(get-exp, CL.map_list(get-exp, visit-vals)))]),
      cl-empty
    )
  end,
  method a-tuple-get(self, l, tup, index):
    visit-name = tup.visit(self)
    c-exp(rt-method("getTuple",
        [clist: visit-name.exp, j-num(index), self.get-loc(l)]),
      cl-empty
    )
  end,
  method a-array(self, l, values):
    visit-vals = values.map(_.visit(self))
    other-stmts = visit-vals.foldr(
      lam(v, acc): cl-append(v.other-stmts, acc) end,
      cl-empty
    )
    c-exp(j-list(false, CL.map_list(get-exp, visit-vals)), other-stmts)
  end,
  method a-srcloc(self, l, loc):
    c-exp(self.get-loc(loc), cl-empty)
  end,
  method a-num(self, l :: Loc, n :: Number):
    if num-is-fixnum(n):
      c-exp(j-parens(j-num(n)), cl-empty)
    else:
      c-exp(
        rt-method("makeNumberFromString", [clist: j-str(tostring(n))]),
        cl-empty
      )
    end
  end,
  method a-str(self, l :: Loc, s :: String):
    c-exp(j-parens(j-str(s)), cl-empty)
  end,
  method a-bool(self, l :: Loc, b :: Boolean):
    c-exp(j-parens(if b: j-true else: j-false end), cl-empty)
  end,
  method a-undefined(self, l :: Loc):
    c-exp(undefined, cl-empty)
  end,
  method a-id(self, l :: Loc, id :: A.Name):
    c-exp(j-id(js-id-of(id)), cl-empty)
  end,
  method a-id-var(self, l :: Loc, id :: A.Name):
    c-exp(j-dot(j-id(js-id-of(id)), "$var"), cl-empty)
  end,
  method a-id-safe-letrec(self, l :: Loc, id :: A.Name):
    s = j-id(js-id-of(id))
    c-exp(j-dot(s, "$var"), cl-empty)
  end,
  method a-id-letrec(self, l :: Loc, id :: A.Name, safe :: Boolean):
    s = j-id(js-id-of(id))
    if safe:
      c-exp(j-dot(s, "$var"), cl-empty)
    else:
     c-exp(
       j-ternary(
         j-binop(j-dot(s, "$var"), j-eq, undefined),
         raise-id-exn(self.get-loc(l), id.toname()),
         j-dot(s, "$var")),
       cl-empty
     )
   end
  end,
  method a-data-expr(self, l, name, namet, variants, shared):
    fun brand-name(base):
      js-id-of(compiler-name(string-append("brand-", base))).toname()
    end
    raise("a-data-expr not implemented")
  end,
}

fun compile-module(self, l, imports-in, prog, freevars, provides, env, flatness-env) block:
  js-names.reset()
  shadow freevars = freevars.unfreeze()
  fun inst(id): j-app(j-id(id), [clist: RUNTIME, NAMESPACE]) end
  imports = imports-in.sort-by(
      lam(i1, i2): import-key(i1.import-type) < import-key(i2.import-type)  end,
      lam(i1, i2): import-key(i1.import-type) == import-key(i2.import-type) end
    )

  for each(i from imports) block:
    freevars.remove-now(i.vals-name.key())
    freevars.remove-now(i.types-name.key())
  end

  import-keys = {vs: [mutable-string-dict:], ts: [mutable-string-dict:]}

  for each(i from imports) block:
    for each(v from i.values):
      import-keys.vs.set-now(v.key(), v)
    end
    for each(t from i.types):
      import-keys.ts.set-now(t.key(), t)
    end
  end

  free-ids = freevars.map-keys-now(freevars.get-value-now(_))
  module-and-global-binds = lists.partition(A.is-s-atom, free-ids)
  global-binds = for CL.map_list(n from module-and-global-binds.is-false):
    # NOTE(joe): below, we use the special case for globals for bootstrapping
    # reasons, because shared compiled files didn't agree on globals
    cases(A.Name) n:
      | s-global(s) =>
        dep = env.globals.values.get-value(n.toname())
        uri = cases(Option) env.mods.get(dep):
          | some(d) => d.from-uri
          | none => raise(dep + " not found in: " + torepr(env.mods))
        end
        j-var(js-id-of(n),
          j-bracket(
             rt-method("getField", [clist:
                  j-bracket(j-dot(RUNTIME, "modules"), j-str(uri)),
                  j-str("defined-values")
                ]),
              j-str(n.toname())))
      | s-type-global(_) =>
        dep = env.globals.types.get-value(n.toname())
        uri = cases(Option) env.mods.get(dep):
          | some(d) => d.from-uri
          | none => raise(dep + " not found in: " + torepr(env.mods))
        end
        j-var(js-id-of(n),
          j-bracket(
              rt-method("getField", [clist:
                  j-bracket(j-dot(RUNTIME, "modules"), j-str(uri)),
                j-str("defined-types")]),
              j-str(n.toname())))
    end

#    j-var(js-id-of(n), j-method(NAMESPACE, "get", [clist: j-str(bind-name)]))
  end
  module-binds = for CL.map_list(n from module-and-global-binds.is-true):
    bind-name = cases(A.Name) n:
      | s-atom(_, _) =>
        if import-keys.vs.has-key-now(n.key()):
          n.toname()
        else if import-keys.ts.has-key-now(n.key()):
          type-name(n.toname())
        else:
          raise("Unaware of imported name: " + n.key())
        end
    end
    j-var(js-id-of(n), j-method(NAMESPACE, "get", [clist: j-str(bind-name)]))
  end
  fun clean-import-name(name):
    if A.is-s-atom(name) and (name.base == "$import"): fresh-id(name)
    else: js-id-of(name)
    end
  end
  ids = imports.map(lam(i): clean-import-name(i.vals-name) end)
  type-imports = imports.filter(N.is-a-import-complete)
  type-ids = type-imports.map(lam(i): clean-import-name(i.types-name) end)
  module-locators = imports.map(lam(i):
    cases(N.AImportType) i.import-type:
      | a-import-builtin(_, name) => CS.builtin(name)
      | a-import-special(_, typ, args) => CS.dependency(typ, args)
    end
  end)
  filenames = imports.map(lam(i):
      cases(N.AImportType) i.import-type:
        | a-import-builtin(_, name) => "trove/" + name
        | a-import-special(_, typ, args) =>
          if typ == "my-gdrive":
            "@my-gdrive/" + args.first
          else if typ == "shared-gdrive":
            "@shared-gdrive/" + args.first + "/" + args.rest.first
          else if typ == "js-http":
            "@js-http/" + args.first
          else if typ == "gdrive-js":
            "@gdrive-js/" + args.first + "/" + args.rest.first
          else:
            # NOTE(joe): under new module loading, this doesn't actually matter
            # NOTE(joe): yes it does, this is how we get a serialized rep of
            # the dependencies for the next time we need to check it
            CS.dependency(typ, args).key()
          end
      end
    end)
  # this needs to be freshened to support multiple repl interactions with the "same" source
  module-id = fresh-id(compiler-name(l.source)).tosourcestring()
  module-ref = lam(name): j-bracket(rt-field("modules"), j-str(name)) end
  input-ids = CL.map_list(lam(i):
      if A.is-s-atom(i) and (i.base == "$import"): js-names.make-atom("$$import")
      else: js-id-of(compiler-name(i.toname()))
      end
    end, ids)
  fun wrap-modules(modules, body-name, body-fun):
    mod-input-names = CL.map_list(_.input-id, modules)
    mod-input-ids = mod-input-names.map(j-id)
    mod-input-ids-list = mod-input-ids.to-list()
    mod-val-ids = modules.map(get-id)
    moduleVal = const-id("moduleVal")
    j-block(
      for lists.fold2(acc from cl-empty, m from mod-val-ids, in from mod-input-ids-list):
        if (in.id.base == "$$import"): acc
        else: acc ^ cl-snoc(_, j-var(m, rt-method("getField", [clist: in, j-str("values")])))
        end
      end +
      for lists.fold2(acc from cl-empty, mt from type-ids, in from mod-input-ids-list):
        if (in.id.base == "$$import"): acc
        else: acc ^ cl-snoc(_, j-var(mt, rt-method("getField", [clist: in, j-str("types")])))
        end
      end +
      for CL.map_list(m from modules):
        j-expr(j-assign(NAMESPACE.id, rt-method("addModuleToNamespace",
          [clist:
            NAMESPACE,
            j-list(false, CL.map_list(lam(i): j-str(i.toname()) end, m.imp.values)),
            j-list(false, CL.map_list(lam(i): j-str(i.toname()) end, m.imp.types)),
            j-id(m.input-id)])))
      end +
      module-binds +
      [clist:
        j-var(body-name, body-fun),
        j-return(rt-method(
            "safeCall", [clist:
              j-id(body-name),
              j-fun(J.next-j-fun-id(),
                "module_load",
                [clist: moduleVal],
                j-block([clist:
                    j-expr(j-bracket-assign(rt-field("modules"), j-str(module-id), j-id(moduleVal))),
                    j-return(j-id(moduleVal))
                  ])),
              j-str("Evaluating " + body-name.toname())
        ]))])
  end
  module-specs = for map3(i from imports, id from ids, in-id from input-ids.to-list()):
    { id: id, input-id: in-id, imp: i}
  end
  var locations = cl-empty
  var loc-count = 0
  var loc-cache = D.make-mutable-string-dict()
  LOCS = const-id("L")
  fun get-loc-id(shadow l :: Loc):
    as-str = l.key()
    if loc-cache.has-key-now(as-str) block:
      loc-cache.get-value-now(as-str)
    else:
      ans = loc-count
      loc-cache.set-now(as-str, ans)
      loc-count := loc-count + 1
      locations := cl-snoc(locations, obj-of-loc(l))
      ans
    end
  end
  fun get-loc(shadow l :: Loc):
    j-bracket(j-id(LOCS), j-num(get-loc-id(l)))
  end

  fun wrap-new-module(compiler, module-body):
    module-locators-as-js = for CL.map_list(m from module-locators):
      cases(CS.Dependency) m:
        | builtin(name) =>
          j-obj([clist:
            j-field("import-type", j-str("builtin")),
            j-field("name", j-str(name))])
        | dependency(protocol, args) =>
          j-obj([clist:
            j-field("import-type", j-str("dependency")),
            j-field("protocol", j-str(protocol)),
            j-field("args", j-list(true, CL.map_list(j-str, args)))])
      end
    end
    provides-obj = compile-provides(provides)
    the-module = j-fun(J.next-j-fun-id(), make-fun-name(compiler, l),
      [clist: RUNTIME.id, NAMESPACE.id, source-name.id] + input-ids, module-body)
    module-and-map = the-module.to-ugly-sourcemap(provides.from-uri, 1, 1, provides.from-uri)
    [D.string-dict:
      "requires", j-list(true, module-locators-as-js),
      "provides", provides-obj,
      "nativeRequires", j-list(true, [clist:]),
      "theModule", if compiler.options.collect-all: the-module else: J.j-str(module-and-map.code) end,
      "theMap", J.j-str(module-and-map.map)
      ]
  end

  toplevel-name = fresh-id(compiler-name("toplevel"))
  apploc = fresh-id(compiler-name("al"))
  body-compiler = self.{
    get-loc: get-loc,
    get-loc-id: get-loc-id,
    cur-apploc: apploc,
    allow-tco: false
  }
  visited-body = compile-fun-body(l, toplevel-name,
    body-compiler,
    [list: ], prog, false)
  toplevel-fun = j-fun(J.next-j-fun-id(),
    make-fun-name(body-compiler, l), [clist: ], visited-body)
  define-locations = j-var(LOCS, j-list(true, locations))
  module-body = j-block(
    #                    [clist: j-expr(j-str("use strict"))] +
    mk-abbrevs(l) ^
    cl-snoc(_, define-locations) ^
    cl-append(_, global-binds) ^
    cl-snoc(_, wrap-modules(module-specs, toplevel-name, toplevel-fun)))
  wrap-new-module(body-compiler, module-body)
end


fun vhull-compiler(env, add-phase, flatness-env, provides, options):
  compiler-visitor.{
    uri: provides.from-uri,
    add-phase: add-phase,
    options: options,
    flatness-en: flatness-env,
    method a-program(self, l, _, imports, body) block:
      total-time := 0
      freevars = N.freevars-e(body)
      add-phase("Freevars-e", freevars)
      ans = compile-module(self, l, imports, body, freevars, provides, env,
        flatness-env)
      add-phase(string-append("Total simplification: ", tostring(total-time)),
        nothing)
      ans
    end
  }
end
