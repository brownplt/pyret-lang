provide *
import ast as A
import file("ast-anf.arr") as N
import file("js-ast.arr") as J
import file("gensym.arr") as G
import file("compile-structs.arr") as CS
import file("concat-lists.arr") as CL
import file("js-dag-utils.arr") as DAG
import file("ast-util.arr") as AU
import file("type-structs.arr") as T
import string-dict as D
import srcloc as SL
import sets as S
import sha as sha

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

string-dict = D.string-dict
mutable-string-dict = D.mutable-string-dict

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

effective-ids = D.make-mutable-string-dict()
js-ids = D.make-mutable-string-dict()
js-names = A.MakeName(0)
fun fresh-id(id :: A.Name) -> A.Name:
  base-name = if A.is-s-type-global(id):
      id.tosourcestring()
    else:
      id.toname()
    end
  no-hyphens = string-replace(base-name, "-", "$")
  n = js-names.make-atom(no-hyphens)
  if effective-ids.has-key-now(n.tosourcestring()) block: # name collision
      fresh-id(id)
    else:
      effective-ids.set-now(n.tosourcestring(), true)
      n
  end
end

fun const-id(name :: String):
  A.s-name(A.dummy-loc, name)
end

fun compiler-name(id):
  const-id(string-append("$", id))
end

fun js-id-of(id :: A.name) -> A.name:
  s = id.key()
  if js-ids.has-key-now(s) block:
    js-ids.get-value-now(s)
  else:
    safe-id = fresh-id(id)
    js-ids.set-now(s, safe-id)
    safe-id
  end
end

undefined = j-id(const-id("D"))
RUNTIME = j-id(const-id("R"))
get-field-loc = j-id(const-id("G"))

type Loc = SL.Srcloc
type CList = CL.ConcatList
clist = CL.clist

fun rt-field(name):
  j-dot(RUNTIME, name)
end

fun rt-method(name, args):
  rt-name = cases(Option) rt-name-map.get(name):
    | none => name
    | some(short-name) => short-name
  end

  j-method(RUNTIME, rt-name, args)
end

compiler-visitor = {
  method a-module(self, l, answer, dvs, dts, provides, types, checks):
    raise("a-module not implemented")
  end,
  method a-type-let(self, l, bind, body):
    raise("a-type-let not implemented")
  end,
  method a-let(self, _, b :: N.ABind, e :: N.ALettable, body :: N.AExpr):
    raise("a-let not implemented")
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
    raise("a-seq not implemented")
  end,
  method a-if(self, l :: Loc, cond :: N.AVal, consq :: N.AExpr,
    alt :: N.AExpr):
    raise("a-if not implemented")
  end,
  method a-cases(self, l :: Loc, typ :: A.Ann, val :: N.AVal,
    branches :: List<N.ACasesBranch>, _else :: N.AExpr):
    raise("a-cases not implemented")
  end,
  method a-update(self, l, obj, fields):
    raise("a-update not implemented")
  end,
  method a-lettable(self, _, e :: N.ALettable):
    raise("a-lettable not implemented")
  end,
  method a-assign(self, l :: Loc, id :: A.Name, value :: N.AVal):
    raise("a-assign not implemented")
  end,
  method a-app(self, l :: Loc, f :: N.AVal, args :: List<N.AVal>):
    raise("a-app not implemented")
  end,
  method a-prim-app(self, l :: Loc, f :: String, args :: List<N.AVal>):
    raise("a-prim-app not implemented")
  end,
  method a-ref(self, l, maybe-ann):
    raise("a-ref not implemented")
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
    raise("a-id-safe-letrec not implemented")
  end,
  method a-id-letrec(self, l :: Loc, id :: A.Name, safe :: Boolean):
    raise("a-id-letrec not implemented")
  end,
  method a-data-expr(self, l, name, namet, variants, shared):
    raise("a-data-expr not implemented")
  end,
  method a-program(self, l, _, imports, body) block:
    raise("a-program not implemented")
  end
}

fun vhull-compiler(env, add-phase, flatness-env, provides, options):
  compiler-visitor
end
