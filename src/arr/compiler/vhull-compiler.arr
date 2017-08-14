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
import file("anf-loop-compiler.arr") as C

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

const-id = C.const-id
compiler-name = C.compiler-name
wrap-with-srcnode = C.wrap-with-srcnode
rt-field = C.rt-field
rt-method = C.rt-method
app = C.app
raise-id-exn = C.raise-id-exn

undefined = j-id(const-id("D"))
RUNTIME = j-id(const-id("R"))
get-field-loc = j-id(const-id("G"))

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

type Loc = SL.Srcloc
type CList = CL.ConcatList
type BindType = C.BindType
clist = CL.clist

var total-time = 0

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
      compiled-ann = C.compile-ann(b.ann, visitor)
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
      opt-arity :: Option<Number>,
      body :: N.AExpr,
      should-report-error-frame :: Boolean,
      is-flat :: Boolean) -> J.JBlock block:

  # All functions return the final answer using this variable.
  ans = fresh-id(compiler-name("ans"))
  local-compiler = compiler.{cur-ans: ans}

  # NOTE(rachit): Skipping the formal-args reassignment optimization here.
  visited-body = body.visit(local-compiler)
  ann-checks = compile-anns(local-compiler, args)
  main-body-block =
    cl-empty
    ^ cl-append(_, ann-checks)
    ^ cl-snoc(_, visited-body.block)
    ^ cl-append(_, visited-body.new-cases)
  ...
end

# NOTE(rachit): Find out what `is-fn` does.
fun compile-a-app(l :: N.Loc, compiler, b :: Option<BindType>, f :: N.AVal,
      args :: List<N.AVal>,
      opt-body :: Option<N.AExpr>,
      app-info :: A.AppInfo,
      is-fn :: Boolean) block:
  # Variable to which the result of applications need to be bound.
  ans = compiler.cur-ans

  compiled-f = f.visit(compiler).exp
  compiled-args = CL.map_list(_.visit(compiler).expm, args)

  call-code = [clist:
    j-expr(
      wrap-with-srcnode(l, j-assign(ans, app(l, compiled-f, compiled-args))))
  ]
  ...
end

fun compile-method-app(l, compiler, obj, methname, args):
  c-obj = obj.visit(compiler).exp
  c-args = CL.map_list(_.visit(compiler).exp, args)

  #|if J.is-j-id(compiled-obj):
    call = wrap-with-srcnode(l,
      rt-method("maybeMethodCall",
        cl-append([clist: c-obj,
          j-str(methname),
          compiler.get-loc(l)
        ], c-args)
      )
    )|#
  ...
end

fun compile-lettable(compiler, b :: Option<BindType>, e :: N.ALettable,
      opt-body :: Option<N.AExpr>,
      else-case :: (DAG.CaseResults -> DAG.CaseResults)):
  cases(N.ALettable) e:
    | a-app(l2, f, args, app-info) =>
        compile-a-app(l2, f, args, compiler, b, opt-body, app-info)
    | a-method-app(l2, obj, m, args) =>
        compile-method-app(l2, compiler, obj, m, args)
  end
end

compiler-visitor = {
  method a-module(self, l, answer, dvs, dts, provides, types, checks):
    raise("a-module not implemented")
  end,
  method a-type-let(self, l, bind, body):
    cases(N.ATypeBind) bind:
      | a-type-bind(l2, name, ann) =>
        visited-body = body.visit(self)
        compiled-ann = C.compile-ann(ann, self)
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
  method a-if(self, l :: Loc, cond :: N.AVal, cons :: N.AExpr, alt :: N.AExpr):
    #|ccons = cons.visit(self)
    calt = alt.visit(self)
    ccond = rt-method("checkPyretTrue", [clist: cond.visit(self)])
    c-block(j-block([clist:
          j-if(ccond, ccons, calt)
    ]))|#
    raise("a-if is not implemented")
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
    )))
  end,
  method a-lettable(self, _, e :: N.ALettable):
    # TODO(rachit): This may have to change for if.
    e.visit(self)
  end,
  method a-assign(self, l :: Loc, id :: A.Name, value :: N.AVal):
    visit-value = value.visit(self)
    c-exp(rt-field("nothing"),
          cl-snoc(visit-value.other-stmts,
                  j-expr(
                    j-dot-assign(j-id(js-id-of(id)), "$var", visit-value.exp)
                  )
          )
    )
  end,
  method a-app(self, l :: Loc, f :: N.AVal, args :: List<N.AVal>):
    compiled-f = f.visit(self).exp
    compiled-args = CL.map_list(_.visit(self).exp, args)
    j-expr(app(l, compiled-f, compiled-args))
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
      ans = C.compile-module(self, l, imports, body, freevars, provides, env,
        flatness-env)
      add-phase(string-append("Total simplification: ", tostring(total-time)),
        nothing)
      ans
    end
  }
end
