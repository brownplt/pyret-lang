provide *
provide-types *

import srcloc as SL
import file("desugar-helpers.arr") as DH
import file("ast.arr") as A
import file("ast-util.arr") as AU
import file("js-ast.arr") as J
import file("gensym.arr") as G
import file("concat-lists.arr") as CL
import file("type-structs.arr") as T
import pathlib as P
import sha as sha
import string-dict as D

flat-prim-app = A.prim-app-info-c(false)

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

fun find-index<a>(f :: (a -> Boolean), lst :: List<a>) -> Option<{a; Number}> block:
  doc: ```Returns value and its index or -1 depending on if element is found```
  var i = 0
  var val = none
  var seen = false
  builtins.raw-list-fold(lam(_, elt):
      if f(elt) and not(seen) block:
        seen := true
        val := some({elt; i})
      else if not(seen):
        i := i + 1
      else:
        nothing
      end
    end, nothing, lst)
  val
end

fun make-fun-name(compiler, loc) -> String:
  "_" + sha.sha256(compiler.uri) + "__" + num-to-string(compiler.get-loc-id(loc))
end

type Loc = SL.Srcloc

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

fun const-id(name :: String):
  A.s-name(A.dummy-loc, name)
end

fun compiler-name(id):
  const-id(string-append("$",id))
end

GLOBAL = const-id("_global")
NOTHING = const-id("_nothing")

RUNTIME = j-id(const-id("R"))
NAMESPACE = j-id(const-id("NAMESPACE"))
source-name = j-id(const-id("M"))

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

fun rt-field(name): j-dot(RUNTIME, name) end

fun rt-method(name, args):
  rt-name = cases(Option) rt-name-map.get(name):
    | none => name
    | some(short-name) => short-name
  end

  j-method(RUNTIME, rt-name, args)
end

fun compile-list(context, exprs):
  if is-empty(exprs):
    { [clist:]; [clist:] }
  else:
    {first-ans; start-stmts} = compile-expr(context, exprs.first)
    {rest-ans; rest-stmts} = compile-list(context, exprs.rest)
    { cl-cons(first-ans, rest-ans) ; start-stmts + rest-stmts}
  end
end

fun compile-seq(context, exprs):
  if is-empty(exprs.rest):
    compile-expr(context, exprs.first)
  else:
    {first-ans; start-stmts} = compile-expr(context, exprs.first)
    {ans; rest-stmts} = compile-seq(context, exprs.rest)
    {ans; start-stmts + [clist: j-expr(first-ans)] + rest-stmts}
  end
end

fun wrap-with-srcnode(l, expr :: J.JExpr):
  cases(Loc) l:
    | builtin(name) => expr
    | srcloc(source, _, _, _, _, _, _) =>
      J.j-sourcenode(l, source, expr)
  end
end

# Use when we're sure the field will exist
fun get-field-unsafe(obj :: J.JExpr, field :: J.JExpr, loc-expr :: J.JExpr):
  rt-method("getFieldLoc", [clist: obj, field, loc-expr])
end

# When the field may not exist, add source mapping so if we can't find it
# we get a useful stacktrace
fun get-field-safe(l, obj :: J.JExpr, field :: J.JExpr, loc-expr :: J.JExpr):
  wrap-with-srcnode(l, get-field-unsafe(obj, field, loc-expr))
end

fun console(args):
  j-app(j-dot(j-id(const-id("console")), "log"), args)
end

fun nyi(name):
  { j-str("not implemented: " + name); [clist: j-expr(console([clist: j-str(name)]))] }
end


fun compile-expr(context, expr) -> { J.JExpr; CList<J.JStmt>}:
  cases(A.Expr) expr block:
    | s-module(l, answer, dms, dvs, dts, checks) =>
      {a-exp; a-stmts} = compile-expr(context, answer)

      # Expose top-level values and variables to outside modules
      # Use variable names as keys
      {fields; stmts} = for fold({fields; stmts} from {cl-empty; cl-empty}, dv from dvs) block:
        cases(A.DefinedValue) dv:
          | s-defined-value(name, def-v) =>
            block:
              {val; field-stmts} = compile-expr(context, def-v)

              { cl-cons(j-field(name, val), fields); field-stmts + stmts }
            end

          | s-defined-var(name, id) =>
            block:
              # TODO(alex): Box variables so external code can mutate variables
              { cl-cons(j-field(name, j-id(js-id-of(id))), fields); stmts }
            end
        end
      end

      ans = j-obj(fields + [clist:
                j-field("$answer", a-exp),
                j-field("$checks", J.j-undefined)])

      assign-ans = j-bracket-assign(j-id(const-id("module")), j-str("exports"), ans)
      {assign-ans; a-stmts + stmts}
    | s-block(l, exprs) => compile-seq(context, exprs)
    | s-num(l, n) => 
      e = if num-is-fixnum(n):
        j-parens(j-num(n))
      else:
        rt-method("makeNumberFromString", [clist: j-str(tostring(n))])
      end
      {e; cl-empty}
    | s-id(l, id) => {j-id(js-id-of(id)); cl-empty}
    | s-id-letrec(l, id, _) => {j-id(js-id-of(id)); cl-empty}
    | s-id-modref(l, id, _, field) =>
      {objv; obj-stmts} = compile-expr(context, A.s-id(l, id))
      {j-bracket(objv, j-str(field)); obj-stmts}
    | s-prim-app(l, name, args, _) =>
      {argvs; argstmts} = compile-list(context, args)

      { j-app(j-bracket(j-id(GLOBAL), j-str(name)), argvs); argstmts }
      
    | s-app-enriched(l, f, args, info) =>
      # TODO(joe): Use info
      {fv; fstmts} = compile-expr(context, f)
      {argvs; argstmts} = compile-list(context, args)
      { j-app(fv, argvs); fstmts + argstmts }

    | s-app(l, f, args) =>
      {fv; fstmts} = compile-expr(context, f)
      {argvs; argstmts} = compile-list(context, args)
      { j-app(fv, argvs); fstmts + argstmts }

    | s-srcloc(_, l) => { j-str("srcloc"); cl-empty }

    | s-op(l, op-l, op, left, right) =>
      { lv; lstmts } = compile-expr(context, left)
      { rv; rstmts } = compile-expr(context, right)
      val = ask:
        | op == "op+" then: j-binop(lv, J.j-plus, rv)
        | op == "op-" then: j-binop(lv, J.j-minus, rv)
        | op == "op*" then: j-binop(lv, J.j-times, rv)
        | op == "op/" then: j-binop(lv, J.j-divide, rv)
        | op == "op<" then: j-binop(lv, J.j-lt, rv)
        | op == "op>" then: j-binop(lv, J.j-gt, rv)
        # TODO(alex): Use equal-always, equal-now, etc
        | op == "op==" then: j-binop(lv, J.j-eq, rv)
        | op == "op<>" then: j-binop(lv, J.j-neq, rv)
        | op == "op<=>" then: j-binop(lv, J.j-eq, rv)
        | op == "opor" then: j-binop(lv, J.j-or, rv)
        | otherwise: nyi(op)
      end
      { val; lstmts + rstmts }

    | s-lam(l, name, _, args, _, _, body, _, _, _) =>

      { body-val; body-stmts } = compile-expr(context, body)

      js-args = for CL.map_list(a from args): js-id-of(a.id) end
      
      {j-fun("0", js-id-of(const-id(name)).toname(), js-args,
        j-block(body-stmts + [clist: j-return(body-val)])); cl-empty}

    | s-let-expr(l, binds, body, _) =>

      prelude = for fold(stmts from cl-empty, v from binds.reverse()):
        { val; v-stmts } = compile-expr(context, v.value)
        v-stmts + [clist: j-var(js-id-of(v.b.id), val)] + stmts
      end
      {bv; body-stmts} = compile-expr(context, body)
      {bv; prelude + body-stmts}
      
    | s-letrec(l, binds, body, _) =>
      
      prelude = for fold(stmts from cl-empty, v from binds.reverse()):
        { val; v-stmts } = compile-expr(context, v.value)
        v-stmts + [clist: j-var(js-id-of(v.b.id), val)] + stmts
      end
      {bv; body-stmts} = compile-expr(context, body)
      {bv; prelude + body-stmts}

    | s-type-let-expr(_, binds, body, _) =>
      # Because if we're taking type seriously, this can't fail! 
      compile-expr(context, body)

    | s-data-expr(l, name, namet, params, mixins, variants, shared, _check-loc, _check) =>

      variant-uniqs = for fold(uniqs from [D.string-dict:], v from variants):
        uniqs.set(v.name, fresh-id(compiler-name(v.name)))
      end

      variant-uniq-defs = for CL.map_list(v from variants):
        names = cases(A.Variant) v:
          | s-singleton-variant(_, _, _) => j-false
          | s-variant(_, _, _, members, _) =>
            j-list(false, for CL.map_list(m from members): j-str(m.bind.id.toname()) end)
        end
        j-var(js-id-of(variant-uniqs.get-value(v.name)), j-obj([clist:
          j-field("names", names)
        ]))
      end

      variant-constructors = for CL.map_list_n(local-tag from 0, v from variants):
        cases(A.Variant) v:
          | s-variant(_, cl, shadow name, members, _) =>
            args = for CL.map_list(m from members): js-id-of(m.bind.id) end
            j-field(name,
              j-fun("0", js-id-of(const-id(name)).toname(), args,
                j-block1(
                  j-return(j-obj(
                    [clist: j-field("$brand", j-id(js-id-of(variant-uniqs.get-value(name)))),
                            j-field("$tag", j-num(local-tag))] +
                    for CL.map_list(m from members):
                      j-field(m.bind.id.toname(), j-id(js-id-of(m.bind.id)))
                    end)))))
          | s-singleton-variant(_, shadow name, with-members) =>
            j-field(name, j-obj([clist:
              j-field("$brand", j-id(js-id-of(variant-uniqs.get-value(name)))),
              j-field("$tag", j-num(local-tag))]))
        end
      end

      variant-recognizers = for CL.map_list(v from variants):
        j-field("is-" + v.name,
          j-fun("0", js-id-of(const-id(v.name)).toname(), [clist: const-id("val")],
            j-block1(
              j-return(j-binop(j-dot(j-id(const-id("val")), "$brand"), j-eq, j-id(js-id-of(variant-uniqs.get-value(v.name))))))))
      end

      { j-obj(variant-constructors + variant-recognizers); variant-uniq-defs }
      
    | s-dot(l, obj, field) =>
      
      {objv; obj-stmts} = compile-expr(context, obj)
      
      {j-bracket(objv, j-str(field)); obj-stmts}

    | s-if-else(l, branches, _else, _) =>

      ans = fresh-id(compiler-name("ans"))

      { else-v; else-stmts } = compile-expr(context, _else)
      else-block = j-block(else-stmts + [clist: j-assign(ans, else-v)])
      
      blck = for fold(blck from else-block, b from branches.reverse()):
        { test-v; test-stmts } = compile-expr(context, b.test)
        { body-v; body-stmts } = compile-expr(context, b.body)
        j-block(test-stmts + [clist:
          j-if(test-v, j-block(body-stmts + [clist: j-assign(ans, body-v)]), blck)])
      end

      { j-id(ans); [clist: j-var(ans, j-undefined)] + blck.stmts }

    | s-cases-else(l, typ, val, branches, _else, blocky) =>

      ans = fresh-id(compiler-name("ans"))
      
      { val-v; val-stmts } = compile-expr(context, val)

      datatype = cases(A.Ann) typ block:
        | a-name(_, name) =>
          print(context.datatypes.keys-now())
          # Datatypes in env are key'd by the raw string name
          cases(Option) context.datatypes.get-now(name.toname()):
            | some(dt) => 
              # Note(alex): Next line necessary?
              # context.provides.data-definitions.get-value(name.toname())
              dt
            | none => 
              # TODO(alex): split into helper method on CompileEnvironment
              # TODO(alex): Perform a recursive lookup on type aliases
              type-bind = context.post-env.type-bindings.get-value-now(name.key())
              type-uri = type-bind.origin.uri-of-definition
              type-original-name = type-bind.origin.original-name.toname()
              provides-result = context.env.provides-by-uri-value(type-uri)
              dt = provides-result.data-definitions.get-value(type-original-name)
              dt
          end
        | else => raise("Can only do cases on a known datatype annotation, not on " + to-repr(typ))
      end

      fun get-tag-and-variant(name) block:
        val-and-tag = for find-index(v from datatype.variants):
          v.name == name
        end
        when(is-none(val-and-tag)): raise("No such variant: " + name) end
        val-and-tag.value
      end

      switch-blocks = for CL.map_list(b from branches):
        {variant; tag} = get-tag-and-variant(b.name)
        cases(A.CasesBranch) b:
          | s-cases-branch(_, pl, name, args, body) =>
            {body-val; body-stmts} = compile-expr(context, body)
            arg-binds = for CL.map_list2(a from args, {m; _} from variant.fields):
              j-var(js-id-of(a.bind.id), j-bracket(val-v, j-str(m)))
            end
            j-case(j-num(tag),
              j-block(arg-binds + body-stmts + [clist: j-expr(j-assign(ans, body-val)), j-break]))
          | s-singleton-cases-branch(_, pl, name, body) =>
            {body-val; body-stmts} = compile-expr(context, body)
            j-case(j-num(tag),
              j-block(body-stmts + [clist: j-expr(j-assign(ans, body-val)), j-break]))
        end
      end
      
      { else-v; else-stmts } = compile-expr(context, _else)

      else-case = j-default(j-block(else-stmts + [clist: j-expr(j-assign(ans, else-v))]))

      {
        j-id(ans);
        val-stmts +
        [clist:
          j-var(ans, j-undefined),
          j-switch(j-dot(val-v, "$tag"), switch-blocks + [clist: else-case])
          ]
      }


    | s-obj(l, fields) =>

      {fieldvs; stmts} = for fold({fieldvs; stmts} from {cl-empty; cl-empty}, f from fields) block:
        when not(A.is-s-data-field(f)):
          raise("Can only provide data fields")
        end

        {val; field-stmts} = compile-expr(context, f.value)

        { cl-cons(j-field(f.name, val), fieldvs); field-stmts + stmts }
      end

      { j-obj(fieldvs); stmts }

    | s-array(l, elts) =>
      { elts-vals; elts-stmts } = compile-list(context, elts)
      { j-list(false, elts-vals); elts-stmts }

    | s-construct(l, modifier, constructor, elts) =>
      { c-val; c-stmts } = compile-expr(context, constructor)
      { elts-vals; elts-stmts } = compile-list(context, elts)
      { j-app(j-bracket(c-val, j-str("make")), [clist: j-list(false, elts-vals)]); c-stmts + elts-stmts }

    | s-instantiate(l, inner-expr, params) => compile-expr(context, inner-expr)
    | s-user-block(l, body) => 
        # Just emit the body as an expression
        compile-expr(context, body)
    | s-template(l) => nyi("s-template")
    | s-method(l, name, params, args, ann, doc, body, _check-loc, _check, blocky) => nyi("s-method")
    | s-type(l, name, params, ann) => raise("s-type already removed")
    | s-newtype(l, name, namet) => raise("s-newtype already removed")
    | s-when(l, test, body, blocky) => 
      compile-expr(
        context,
        A.s-if-else(l, 
                    [list: A.s-if-branch(l, test, body)],
                    A.s-id(l, A.s-global("nothing")),   
                    blocky)
      )
    | s-if(l, branches, blocky) => 
      # TODO(ALEX): check s-if handling
      # Desugar into s-if-else with raise in last branch
      compile-expr(
        context,
        A.s-if-else(l, 
                    branches,
                    A.s-prim-app(l, 
                      "throwNoBranchesMatched", 
                      [list: A.s-srcloc(l, l), A.s-str(l, "if")], 
                      flat-prim-app),
                    blocky)
      )
    | s-if-pipe(l, branches, blocky) => 
      compile-expr(context, 
                   A.s-if(l, 
                          for map(b from branches): b.to-if-branch() end, 
                          blocky))
    | s-if-pipe-else(l, branches, _else, blocky) => 
      compile-expr(context, 
                   A.s-if-else(l, 
                               for map(b from branches): b.to-if-branch() end,
                               _else, 
                               blocky))
    | s-cases(l, typ, val, branches, blocky) =>
      compile-expr(context,
                   A.s-cases-else(l, typ, val, branches,
                     A.s-prim-app(l, 
                                  "throwNoBranchesMatched",
                                  [list: A.s-srcloc(l, l), A.s-str(l, "cases")], 
                                  flat-prim-app),
                     blocky))
    | s-assign(l, id, val) => 
      block:
        { e-val; e-stmts } = compile-expr(context, val)
        { j-assign(js-id-of(id), e-val); e-stmts }
      end
    | s-bracket(l, obj, key) => nyi("s-bracket")
    | s-get-bang(l, obj, field) => nyi("s-get-bang")
    | s-update(l, obj, fields) => nyi("s-update")
    | s-extend(l, obj, fields) => nyi("s-extend")
    | s-for(l, iter, bindings, ann, body, blocky) => 
      compile-expr(context, DH.desugar-s-for(l, iter, bindings, ann, body))
    | s-id-var(l, ident) => 
      { j-id(js-id-of(ident)); cl-empty }
    | s-frac(l, num, den) => 
        # Following the s-num convention of paren wrapping
        # TODO: Properly generate the Number object
        { j-parens(j-num(num / den)); cl-empty }
    | s-rfrac(l, num, den) => 
        compile-expr(context, A.s-frac(l, num, den))
    | s-str(l, str) => {j-str( str ); cl-empty}
    | s-bool(l, bool) =>
        if bool:
          {j-true; cl-empty}
        else:
          {j-false; cl-empty}
        end
    | s-tuple(l, fields) =>

      # Fields are in reverse order
      {fieldvs; stmts} = for fold({fieldvs; stmts} from {cl-empty; cl-empty}, f from fields.reverse()) block:

        {val; field-stmts} = compile-expr(context, f)

        { cl-cons(val, fieldvs); field-stmts + stmts }
      end

      # Represent tuples as arrays
      { j-list(false, fieldvs); stmts }
    | s-tuple-get(l, tup, index, index-loc) => 

      {tupv; tup-stmts} = compile-expr(context, tup)
      
      # Tuples represented as arrays
      {j-bracket(tupv, j-num(index)); tup-stmts}

    | s-ref(l, ann) => nyi("s-ref")
    | s-reactor(l, fields) => nyi("s-reactor")
    | s-table(l, headers, rows) => nyi("s-table")
    | s-paren(l, e) => 
        { e-ans; e-stmts } = compile-expr(context, e)
        { j-parens(e-ans); e-stmts }
    | s-let(_, _, _, _) => raise("desugared into s-let-expr")
    | s-var(l, name, value) => raise("desugared into s-let-expr")
    | s-check(l, name, body, keyword-check) => nyi("s-check")
    | s-check-test(l, op, refinement, left, right) => nyi("s-check-test")
    | s-load-table(l, headers, spec) => nyi("s-load-table")
    | s-table-extend(l, column-binds, extensions) => nyi("s-table-extend")
    | s-table-update(l, column-binds, updates) => nyi("s-table-update")
    | s-table-select(l, columns, table) => nyi("s-table-select")
    | s-table-extract(l, column, table) => nyi("s-table-extract")
    | s-table-order(l, table, ordering) => nyi("s-table-order")
    | s-table-filter(l, column-binds, predicate) => nyi("s-table-filter")
    | s-spy-block(l, message, contents) => nyi("s-spy-block")
    | else => raise("NYI (compile): " + torepr(expr))
  end

end

fun gen-tuple-bind(context, fields, as-name, value):
  var count = 0
  {bindings; stmts} = for fold({bind; stmt-list} from {cl-empty; cl-empty}, b from fields) block:
    { bind-v; bind-stmts} = cases(A.Bind) b:
      | s-bind(bl, doShadow, id, ann) => { j-var(id, j-bracket(value, j-num(count))); cl-empty }
      | s-tuple-bind(l, shadow fields, shadow as-name) => gen-tuple-bind(context, fields, as-name, j-bracket(value, j-num(count)))
    end

    count := count + 1

    { cl-cons(bind-v, bind); bind-stmts + stmt-list }
  end

  { shadow bindings; shadow stmts } = cases(Option<A.Bind>) as-name:
    | some(b) => 
      cases(A.Bind) b:
        | s-bind(bl, doShadow, id, ann) => { j-var(id, value); cl-empty }
        | s-tuple-bind(l, shadow fields, shadow as-name) => 
          { as-bind-v; as-stmts } = gen-tuple-bind(context, fields, as-name, value)
          { cl-cons(as-bind-v, bindings); as-stmts + stmts }
      end

    | none => { bindings; stmts }
  end

  { j-block(bindings); stmts }
end


fun node-prelude(prog, provides, env, options) block:
  runtime-builtin-relative-path = options.runtime-builtin-relative-path
  fun get-base-dir( source, build-dir ):
    source-head = ask:
      | string-index-of( source, "file://" ) == 0 then: 7
      | string-index-of( source, "jsfile://" ) == 0 then: 9
    end

    fun find-cutoff( dir-A, dir-B, index-A, index-B, length ):
      if (index-A >= string-length(dir-A)) or (index-B >= string-length(dir-B)):
        length
      else if ( string-char-at( dir-A, index-A ) == string-char-at( dir-B, index-B ) ):
        find-cutoff( dir-A, dir-B, index-A + 1, index-B + 1, length + 1 )
      else:
        length
      end
    end

    cutoff-index = find-cutoff( source, build-dir, source-head, 0, 0 )
    { string-substring( build-dir, 0, cutoff-index );
      string-substring( source, source-head, string-length( source ) ) }
  end

  fun get-compiled-relative-path( base-dir, source ):
    shadow base-dir = P.resolve(base-dir)
    shadow source = P.resolve(source)
    cutoff = string-substring( source, string-length( base-dir ) + 1, string-length( source ) )
    
    fun calculate-relative-path( path ):
      if string-contains( path, "/" ):
        slash-location = string-index-of( path, "/" )
        remaining-path = string-substring( path, slash-location + 1, string-length( path ) )

        string-append( "../", calculate-relative-path( remaining-path ) ) 
      else:
        "./"
      end
    end

    calculate-relative-path( cutoff )
  end

  { base-dir; absolute-source } = get-base-dir( provides.from-uri, options.base-dir )
  pre-append-dir = get-compiled-relative-path( base-dir, absolute-source )
  relative-path = pre-append-dir #string-append( pre-append-dir, options.runtime-path )

  imports = cases( A.Program ) prog:
    | s-program( _, _, _, _, shadow imports, _ ) => imports
  end

  fun uri-to-real-fs-path(uri):
    full-path = ask:
      | string-index-of(uri, "jsfile://") == 0 then: string-substring(uri, 9, string-length(uri)) + ".js"
      | string-index-of(uri, "file://") == 0 then: string-substring(uri, 7, string-length(uri)) + ".js"
    end
    full-path
  end

  fun starts-with(s, prefix):
    string-index-of(s, prefix) == 0
  end

  fun uri-to-import(uri, name) block:
    ask:
      | starts-with(uri, "builtin://") then:
        builtin-name = string-substring(uri, 10, string-length(uri))
        J.j-var(js-id-of(name), j-app(j-id(const-id("require")), [clist: j-str( relative-path + runtime-builtin-relative-path + builtin-name + ".arr.js")]))
      | starts-with(uri, "jsfile://") or starts-with(uri, "file://") then:
        target-path = uri-to-real-fs-path(uri)
        this-path = uri-to-real-fs-path(provides.from-uri)
        js-require-path = P.relative(P.dirname(this-path), target-path)
        J.j-var(js-id-of(name), j-app(j-id(const-id("require")), [clist: j-str("./" + js-require-path)]))
    end
  end

  global-names = AU.get-globals(prog)
  uri-to-local-js-name = [D.mutable-string-dict:]

  # manually emit global import
  global-import = J.j-var(GLOBAL, 
                          j-app(j-id(const-id("require")), 
                                [clist: j-str( relative-path + runtime-builtin-relative-path + "global.arr.js")]))

  nothing-import = J.j-var(NOTHING, j-undefined)

  manual-imports = [clist: global-import, nothing-import]

  # We create a JS require() statement for each import in the Pyret program
  # and bind it to a unique name. dep-to-local-js-names helps us look
  # up these names later if we need to access a value from that module
  explicit-imports = for CL.map_list(import-stmt from imports):
    cases( A.Import ) import-stmt block:
      | s-import(l, file, name) =>
        dep-key = AU.import-to-dep(file).key()
        uri = env.uri-by-dep-key(dep-key)
        uri-to-local-js-name.set-now(uri, name)
        uri-to-import(uri, name)
      | else => J.j-var(const-id("SKIP"), J.j-undefined)
    end
  end

  # We _also_ insert a require for any modules that have a globally-referenced
  # name. This won't re-instantiate them since require() caches modules; it just
  # gives us a local name to use, and leverages the built-in Node module system
  # rather than having Pyret's runtime track all loaded modules.
  non-imported-global-names = for filter(g from global-names.keys-list-now()):
    not(uri-to-local-js-name.has-key-now(env.uri-by-value-name-value(g)))
  end
  var implicit-imports = cl-empty
  for each(g from non-imported-global-names):
    uri = env.uri-by-value-name-value(g)
    when not(uri-to-local-js-name.has-key-now(uri)) block:
      new-name = fresh-id(compiler-name("G"))
      uri-to-local-js-name.set-now(uri, new-name)
      implicit-imports := cl-cons(uri-to-import(uri, new-name), implicit-imports)
    end
  end

  import-stmts = explicit-imports + implicit-imports + manual-imports

  # We also build up a list of var statements that bind local JS names for
  # all the globals used as identifiers, to make compiling uses of s-global
  # straightforward.
  pyret-globals-as-js-ids = for CL.map_list(g from global-names.keys-list-now()):
    uri = env.uri-by-value-name-value(g)
    imported-as = uri-to-local-js-name.get-value-now(uri)
    J.j-var(js-id-of(A.s-global(g)), J.j-dot(j-id(js-id-of(imported-as)), g))
  end

  import-stmts + pyret-globals-as-js-ids

end

fun compile-program(prog :: A.Program, uri, env, post-env, provides, options) block:
  # TODO(alex): Find out if a uri is actually required by AU.data-expr-to-datatype

  # Translate datatypes from Expr form to Type form in order to be useful for cases expressions
  translated-datatype-map = D.make-mutable-string-dict()
  _ = for map(key from post-env.datatypes.keys-now().to-list()):
    translated-datatype = AU.data-expr-to-datatype(uri, env, post-env.datatypes.get-value-now(key))
    translated-datatype-map.set-now(key, translated-datatype)
  end

  {ans; stmts} = compile-expr({
    uri: provides.from-uri,
    options: options,
    provides: provides,
    datatypes: translated-datatype-map,
    env: env,
    post-env: post-env,
  }, prog.block)

  prelude = node-prelude(prog, provides, env, options)

  # module-body = J.j-block(global-binds + stmts + [clist: j-return(ans)])
  module-body = J.j-block(prelude + stmts + [clist: j-return(ans)])

  the-module = module-body

  module-and-map = the-module.to-ugly-sourcemap(provides.from-uri, 1, 1, provides.from-uri)

  [D.string-dict:
    "requires", j-list(true, [clist:]),
    "provides", j-obj([clist:]),
    "nativeRequires", j-list(true, [clist:]),
    "theModule", J.j-raw-code(module-and-map.code),
    "theMap", J.j-str(module-and-map.map)
    ]
end
