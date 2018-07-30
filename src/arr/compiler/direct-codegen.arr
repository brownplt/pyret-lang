provide *
provide-types *

import srcloc as SL
import file("ast.arr") as A
import file("js-ast.arr") as J
import file("gensym.arr") as G
import file("concat-lists.arr") as CL
import file("ast-util.arr") as AU
import file("type-structs.arr") as T
import sha as sha
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
    | s-module(l, answer, dvs, dts, provides, types, checks) =>
      {a-exp; a-stmts} = compile-expr(context, answer)

      ans = j-obj([clist:
                j-field("answer", a-exp),
                j-field("namespace", J.j-undefined),
                j-field("defined-values", j-obj(cl-empty)),
                j-field("defined-types", j-obj(cl-empty)),
                j-field("provide-plus-types",
                  j-obj([clist:
                          j-field("values", J.j-undefined),
                          j-field("types", J.j-undefined)
                      ])),
                j-field("checks", J.j-undefined)])

      {ans; a-stmts}
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
    | s-prim-app(l, name, args, _) =>
      {argvs; argstmts} = for fold({argvs; argstmts} from {cl-empty; cl-empty}, a from args):
        {argv; stmts} = compile-expr(context, a)
        {cl-snoc(argvs, argv); argstmts + stmts}
      end
      { console([clist: j-str(name)] + argvs); argstmts }
      
    | s-app-enriched(l, f, args, info) =>
      # TODO(joe): Use info
      {fv; fstmts} = compile-expr(context, f)
      {argvs; argstmts} = for fold({argvs; argstmts} from {cl-empty; cl-empty}, a from args):
        {argv; stmts} = compile-expr(context, a)
        {cl-snoc(argvs, argv); argstmts + stmts}
      end
      { j-app(fv, argvs); fstmts + argstmts }

    | s-srcloc(_, l) => { j-str("srcloc"); cl-empty }

    | s-op(l, op-l, op, left, right) =>
      { lv; lstmts } = compile-expr(context, left)
      { rv; rstmts } = compile-expr(context, right)
      val = ask:
        | op == "op+" then: j-binop(lv, J.j-plus, rv)
        | op == "op-" then: j-binop(lv, J.j-minus, rv)
        | op == "op==" then: rt-method("py_equal", [list: lv, rv])
        | op == "op<=>" then: j-binop(lv, J.j-eq, rv)
        | otherwise: nyi(op)
      end
      { val; lstmts + rstmts }

    | s-lam(l, name, _, args, _, _, body, _, _, _) =>

      { body-val; body-stmts } = compile-expr(context, body)

      js-args = for CL.map_list(a from args): js-id-of(a.id) end
      
      {j-fun(0, js-id-of(const-id(name)).toname(), js-args,
        j-block(body-stmts + [clist: j-return(body-val)])); cl-empty}

    | s-app(l, f, args) =>
      {fv; fstmts} = compile-expr(context, f)
      {argvs; argstmts} = for fold({argvs; argstmts} from {cl-empty; cl-empty}, a from args):
        {argv; stmts} = compile-expr(context, a)
        {cl-snoc(argvs, argv); argstmts + stmts}
      end
      { j-app(fv, argvs); fstmts + argstmts }

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

    | s-data-expr(l, name, _, namet, params, mixins, variants, shared, _check-loc, _check) =>

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
            args = for map(m from members): js-id-of(m.bind.id) end
            j-field(name,
              j-fun(0, js-id-of(const-id(name)).toname(), args,
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
          j-fun(0, js-id-of(const-id(v.name)).toname(), [clist: const-id("val")],
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
          cases(Option) context.datatypes.get-now(name.key()):
            | some(dt) => dt
            | none => raise("Unknown datatype name: " + to-repr(typ))
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
            arg-binds = for CL.map_list2(a from args, m from variant.members):
              j-var(js-id-of(a.bind.id), j-bracket(val-v, j-str(m.bind.id.toname())))
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

    | s-instantiate(l, inner-expr, params) => nyi("s-instantiate")
    | s-user-block(l, body) => nyi("s-user-block")
    | s-template(l) => nyi("s-template")
    | s-method(l, name, params, args, ann, doc, body, _check-loc, _check, blocky) => nyi("s-method")
    | s-type(l, name, params, ann) => nyi("s-type")
    | s-newtype(l, name, namet) => nyi("s-newtype")
    | s-when(l, test, body, blocky) => nyi("s-when")
    | s-if(l, branches, blocky) => nyi("s-if")
    | s-if-pipe(l, branches, blocky) => nyi("s-if-pipe")
    | s-if-pipe-else(l, branches, _else, blocky) => nyi("s-if-pipe-else")
    | s-cases(l, typ, val, branches, blocky) => nyi("s-cases")
    | s-assign(l, id, val) => nyi("s-assign")
    | s-bracket(l, obj, key) => nyi("s-bracket")
    | s-get-bang(l, obj, field) => nyi("s-get-bang")
    | s-update(l, obj, fields) => nyi("s-update")
    | s-extend(l, obj, fields) => nyi("s-extend")
    | s-for(l, iter, bindings, ann, body, blocky) => nyi("s-for")
    | s-id-var(l, x) => nyi("s-id-var")
    | s-frac(l, num, den) => nyi("s-frac")
    | s-rfrac(l, num, den) => nyi("s-rfrac")
    | s-str(l, str) => {j-str( str ); cl-empty}
    | s-bool(l, bool) =>
        if bool:
          {j-true; cl-empty}
        else:
          {j-false; cl-empty}
        end
    | s-obj(l, fields) => nyi("s-obj")
    | s-tuple(l, fields) => nyi("s-tuple")
    | s-tuple-get(l, tup, index, index-loc) => nyi("s-tuple-get")
    | s-ref(l, ann) => nyi("s-ref")
    | s-construct(l, modifier, constructor, elts) => nyi("s-construct")
    | s-reactor(l, fields) => nyi("s-reactor")
    | s-table(l, headers, rows) => nyi("s-table")
    | s-paren(l, e) => nyi("s-paren")
    | s-let(_, _, _, _)           => nyi("s-let")
    | s-var(_, _, _)              => nyi("s-var")
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

fun compile-program(prog :: A.Program, env, datatypes, provides, options) block:
  {ans; stmts} = compile-expr({
    uri: provides.from-uri,
    options: options,
    provides: provides,
    datatypes: datatypes,
    env: env
  }, prog.block)

  globals = D.make-mutable-string-dict()

  fun global-bind(n):
    name = n.toname()
    dep = env.globals.values.get-value(name)
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
          j-str(name)))
  end

  fun global-type-bind(n):
    name = n.toname()
    dep = env.globals.types.get-value(name)
    uri = cases(Option) env.mods.get(dep):
      | some(d) => d.from-uri
      | none => raise(dep + " not found in: " + torepr(env.mods))
    end
    j-var(js-id-of(n),
      j-bracket(
          rt-method("getField", [clist:
              j-bracket(j-dot(RUNTIME, "modules"), j-str(uri)),
            j-str("defined-types")]),
          j-str(name)))
  end

  var global-bind-dict = D.make-mutable-string-dict()

  collect-globals = A.default-iter-visitor.{
    method s-id(self, l, name) block:
      when A.is-s-global(name):
        global-bind-dict.set-now(name.toname(), global-bind(name))
      end
      true
    end,
    method a-name(self, l, name) block:
      when A.is-s-global(name):
        global-bind-dict.set-now(name.toname(), global-type-bind(name))
      end
      true
    end
  }

  # prog.visit(collect-globals)

  var global-binds = for CL.map_list(k from global-bind-dict.keys-list-now()):
    global-bind-dict.get-value-now(k)
  end

  fun get-base-dir( source-dir, build-dir ):
    source-head = ask:
      | string-index-of( source-dir, "file://" ) == 0 then: 7
      | string-index-of( source-dir, "jsfile://" ) == 0 then: 9
    end

    fun find-cutoff( dir-A, dir-B, index-A, index-B, length ):
      if ( string-char-at( dir-A, index-A ) == string-char-at( dir-B, index-B ) ):
        find-cutoff( dir-A, dir-B, index-A + 1, index-B + 1, length + 1 )
      else:
        length
      end
    end

    cutoff-index = find-cutoff( source-dir, build-dir, source-head, 0, 0 )
    { string-substring( build-dir, 0, cutoff-index );
      string-substring( source-dir, source-head, string-length( source-dir ) ) }
  end

  fun get-compiled-relative-path( base-dir, source-dir ):
    cutoff = string-substring( source-dir, string-length( base-dir ), string-length( source-dir ) )
    
    fun calculate-relative-path( path ):
      if string-contains( path, "/" ):
        slash-location = string-index-of( path, "/" ) + 1
        remaining-path = string-substring( path, slash-location, string-length( path ) )

        string-append( "../", calculate-relative-path( remaining-path ) ) 
      else:
        ""
      end
    end

    calculate-relative-path( cutoff )
  end

  { base-dir; absolute-source-dir } = get-base-dir( provides.from-uri, options.this-pyret-dir )
  pre-append-dir = get-compiled-relative-path( base-dir, absolute-source-dir )
  relative-path = pre-append-dir #string-append( pre-append-dir, options.runtime-path )

  imports = cases( A.Program ) prog:
    | s-program( _, _, _, shadow imports, _ ) =>
        imports
  end

  import-stmts = imports.foldl( lam( import-module, import-list ):
    cases( A.Import ) import-module:
      | s-import-complete( _,  _, _, import-type, name, _ ) =>
          cases( A.ImportType ) import-type:
            | s-const-import( _, file ) =>
                [clist: J.j-var(name, j-app(j-id(const-id("require")), [clist: j-str( relative-path + "../builtin/" + file + ".js")]))] + import-list
          end
    end
  end, cl-empty )
  
  # module-body = J.j-block(global-binds + stmts + [clist: j-return(ans)])
  module-body = J.j-block(import-stmts + stmts + [clist: j-return(ans)])

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
