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
import file("provide-serialization.arr") as PSE
import pathlib as P
import sha as sha
import string-dict as D

flat-prim-app = A.prim-app-info-c(false)

string-dict = D.string-dict
mtd = [string-dict:]

type Ann = A.Ann
type Bind = A.Bind
type Name = A.Name
type ColumnBinds = A.ColumnBinds
type ColumnSort = A.ColumnSort
type ColumnSortOrder = A.ColumnSortOrder
type Expr = A.Expr
type FieldName = A.FieldName
type LoadTableSpec = A.LoadTableSpec
type TableExtendField = A.TableExtendField

type CList = CL.ConcatList
clist = CL.clist
cl-empty = CL.concat-empty
cl-sing = CL.concat-singleton
cl-append = CL.concat-append
cl-cons = CL.concat-cons
cl-snoc = CL.concat-snoc

type JExpr = J.JExpr
type JStmt = J.JStmt
type JBlock = J.JBlock
type JField = J.JField
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

default-import-flags = {
  array-import: false,
  number-import: false,
  reactor-import: false,
  table-import: false,
}

# Update by 'import-flags := import-flags.{ flags to change here }'
var import-flags = default-import-flags

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

EQUAL-ALWAYS = "equal-always"
IDENTICAL = "identical"
MAKETUPLE = "PTuple"

GLOBAL = const-id("_global")
ARRAY = const-id("_array")
TABLE = const-id("_table")
REACTOR = const-id("_reactor")
NUMBER = const-id("_number")
NOTHING = const-id("_nothing")

RUNTIME = const-id("_runtime")
NAMESPACE = j-id(const-id("NAMESPACE"))
source-name = j-id(const-id("M"))
OBJECT = const-id("Object")

NUMBER_ERR_CALLBACKS = "_errCallbacks"

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

fun rt-field(name): j-bracket(j-id(RUNTIME), j-str(name)) end

fun rt-method(name, args):
  rt-name = cases(Option) rt-name-map.get(name):
    | none => name
    | some(short-name) => short-name
  end

  j-app(j-bracket(j-id(RUNTIME), j-str(rt-name)), args)
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

data BindableKind:
  | unbindable(value :: JExpr)
  | to-bind(binder :: JExpr)    # JS function expecting 1 arg: the object to bind
end

fun compile-member(context, member :: A.Member) -> { BindableKind; CList<JStmt> }:
  cases(A.Member) member:
  | s-data-field(l :: Loc, name :: String, value :: Expr) =>
    { field-val; field-stmts } = compile-expr(context, value)
    # Assume s-method can only be at the top level (i.e. no nesting)
    # Any nesting is a well-formedness error
    cases(Expr) value:
      | s-method(_, _, _, _, _, _, _, _, _, _) => { to-bind(field-val); field-stmts }
      | else => { unbindable(field-val); field-stmts }
    end

  | s-mutable-field(l :: Loc, name :: String, ann :: Ann, value :: Expr) => 
    raise("Mutable member fields not supported")

  | s-method-field(
      l :: Loc,
      name :: String,
      params :: List<Name>,
      args :: List<Bind>, # Value parameters
      ann :: Ann, # return type
      doc :: String,
      body :: Expr,
      _check-loc :: Option<Loc>,
      _check :: Option<Expr>,
      blocky :: Boolean
    ) => 
      { binder-func; binder-stmts } = compile-method(context, l, name, args, body)
      { to-bind(binder-func); binder-stmts }
  end
end

#
# Does NOT support method expressions
# TODO(alex): Deprecate method expressions?
#
# Generates a function and a nested function of the form:
#
#   function binderNAME(self) {
#     var inner = function innerNAME(method-args-no-self) { ... };
#     inner["$brand"] = METHOD-BRAND;
#     inner["$binder'] = binderNAME;
#     return inner;
#   } 
#
# Instantiating a method on a data variant:
#
#   var $singletonTMP = {
#     ...
#     "methodName": bindermethodName($singletonTMP)
#     ...
#   };
#
#   ...
#
#   "variant": function(...) {
#     var tmpObj = {
#       ...
#       "methodName": bindermethodName(tmpObj),
#       ...
#     };
#     return tmpObj;
#   },
#   "singleton": $singletonTMP,
#
# Rebinding methods should be handled by a RUNTIME function.
# Rebinding should simply be calling something like:
#   'oldObject.method["$binder"](newObject)'
#
# TODO(alex): Generate rebinding call
#
fun compile-method(context, 
      l :: Loc,
      name :: String,
      args :: List<Bind>, # Value parameters
      body :: Expr) -> { JExpr; CList<JStmt> }:

  fun remove-self<a>(my-list :: CList<a>) -> { a; CList<a> }: 
    cases(CList) my-list:
      | concat-empty => raise("Always have at least 1 method parameter (self). Found none")

      | concat-singleton(self-arg) => { self-arg; cl-empty }

      | concat-append(left :: CList<a>, right :: CList<a>) =>
        { self-arg; rest-left } = remove-self(list)
        { self-arg; cl-append(rest-left, right) }

      | concat-cons(self-arg :: a, rest :: CList<a>) =>
        { self-arg; rest }

      | concat-snoc(head :: CList<a>, last :: a) =>
        { self-arg; rest } = remove-self(list)
        { self-arg; cl-snoc(rest, last) }
    end
  end
  { js-body-val; js-body-stmts } = compile-expr(context, body) 

  # 'self' is included by s-method.args
  js-args-with-self = for CL.map_list(a from args): js-id-of(a.id) end

  # NOTE(alex): assuming 'self' is always first
  { self; js-args-without-self } = remove-self(js-args-with-self)

  # Generate a function that closes over the 'self' arg given by the binder function
  inner-fun = j-fun("0", 
    js-id-of(const-id("inner" + name)).toname(), 
    js-args-without-self,
    j-block(cl-snoc(js-body-stmts, j-return(js-body-val)))
  )

  binder-fun-name = fresh-id(compiler-name("binder" + name))

  inner-fun-bind = fresh-id(compiler-name("inner"))

  # Assign inner function to a variable
  inner-fun-var = j-var(inner-fun-bind, inner-fun)

  # Give the inner function a method brand
  inner-fun-brand = j-bracket-assign(
    j-id(inner-fun-bind), 
    j-str("$brand"),
    j-str("METHOD")
  )

  # Give the inner function a reference to the binder function (for rebinding)
  inner-fun-binder = j-bracket-assign(
    j-id(inner-fun-bind), 
    j-str("$binder"),
    j-id(binder-fun-name)
  )

  # Generate the binder function
  binder-fun = j-fun("0",
    binder-fun-name.to-compiled(),
    cl-sing(self),
    j-block([clist: inner-fun-var, 
                    j-expr(inner-fun-brand), 
                    j-expr(inner-fun-binder),
                    j-return(j-id(inner-fun-bind))
            ])
  )

  { j-id(binder-fun-name); cl-sing(j-expr(binder-fun)) }
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
        rt-method("_makeNumberFromString", [clist: j-str(tostring(n)), rt-field(NUMBER_ERR_CALLBACKS)])
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
        # Pyret number operations compatible with JS numbers
        # Always assume Pyret numbers when compiling
        | (op == "op+") then: 
          rt-method("_add", 
                    [clist: lv, rv, rt-field(NUMBER_ERR_CALLBACKS)])
        | (op == "op-") then: 
          rt-method("_subtract", 
                    [clist: lv, rv, rt-field(NUMBER_ERR_CALLBACKS)])
        | (op == "op*") then: 
          rt-method("_multiply", 
                    [clist: lv, rv, rt-field(NUMBER_ERR_CALLBACKS)])
        | (op == "op/") then:
          rt-method("_divide", 
                    [clist: lv, rv, rt-field(NUMBER_ERR_CALLBACKS)])
        | (op == "op<") then:
          rt-method("_lessThan", 
                    [clist: lv, rv, rt-field(NUMBER_ERR_CALLBACKS)])
        | (op == "op>") then:
          rt-method("_greaterThan", 
                    [clist: lv, rv, rt-field(NUMBER_ERR_CALLBACKS)])
        | (op == "op<=") then:
          rt-method("_lessThanOrEqual", 
                    [clist: lv, rv, rt-field(NUMBER_ERR_CALLBACKS)])
        | (op == "op>=") then:
          rt-method("_greaterThanOrEqual", 
                    [clist: lv, rv, rt-field(NUMBER_ERR_CALLBACKS)])

        # TODO(alex): Use equal-always, equal-now, etc
        # Call Global.py_equal
        | op == "op==" then: 
          argvs = cl-cons(lv, cl-sing(rv))
          j-app(j-bracket(j-id(GLOBAL), j-str(EQUAL-ALWAYS)), argvs)
        | op == "op<>" then:
          # Logical negation of equal-always()
          argvs = cl-cons(lv, cl-sing(rv))
          j-unop(j-app(j-bracket(j-id(GLOBAL), j-str(EQUAL-ALWAYS)), argvs), J.j-not)
        | op == "op<=>" then:
          argvs = cl-cons(lv, cl-sing(rv))
          j-app(j-bracket(j-id(GLOBAL), j-str(IDENTICAL)), argvs)
        | op == "opor" then: j-binop(lv, J.j-or, rv)
        | op == "opand" then: j-binop(lv, J.j-and, rv)
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

      # Combine compiled-shared, compiled-with, and members to initialize 
      #   any overlapping fields once
      # Priority order (i.e. what name gets initialized to what):
      #   1) Members
      #   2) With Members
      #   3) Shared Members
      fun resolve-init-names(constructed-obj :: JExpr, compiled-shared, 
                             compiled-with, variant-members) 
        -> { CList<JField>; CList<JStmt> }:

        # Given a shared/with member, emit the code to set the field
        # NOTE(alex): Currently cannot do recursive object initialization
        #   Manually assign the shared/with member with j-bracket vs returning a j-field
        fun compile-nonlocal-member(shadow constructed-obj :: JExpr,
                                    member :: A.Member, 
                                    member-val :: BindableKind, 
                                    member-stmts :: CList<JStmt>) -> CList<JStmt>: 
          fun bind(binder-func):
            init-expr-rhs = j-app(binder-func, [clist: constructed-obj])
            j-expr(j-bracket-assign(constructed-obj, j-str(member.name), init-expr-rhs))
          end

          cases(BindableKind) member-val:
            | unbindable(value) =>
              cl-sing(j-expr(j-bracket-assign(constructed-obj, j-str(member.name), value)))

            | to-bind(binder) =>
              # Found a method; bind the method function to the constructed object
              #   by calling the method's binder function and passing in 'self'
              cl-snoc(member-stmts, bind(binder))
          end
        end
        
        # Construct dictionary of variant member inits
        variant-member-map = for fold(dict from [string-dict: ], m from variant-members):
          field-name = m.bind.id.toname()
          init-expr = j-field(field-name, j-id(js-id-of(m.bind.id)))
          dict.set(field-name, { some(init-expr); cl-empty})
        end

        # Construct dictionary of with-member inits and NON-conflicting variant member inits
        # Variant members have priority
        with-member-map = for CL.foldl(dict from variant-member-map, m from compiled-with):
          { member; { member-value; member-stmts}} = m
          if dict.has-key(member.name):
            # with-member overrided by variant member
            dict
          else:
            # No conflicting variant member
            compiled-stmts = compile-nonlocal-member(
              constructed-obj, 
              member, 
              member-value, 
              member-stmts
            )
            dict.set(member.name, 
                     { none; compiled-stmts})
          end
        end

        # Construct dictionary of shared-member inits 
        #   and NON-conflicting variant member inits and with-member inits
        # Variant members and with-members have priority
        shared-member-map = for CL.foldl(dict from with-member-map, m from compiled-shared):
          { member; { member-value; member-stmts}} = m
          if dict.has-key(member.name):
            # shared-member overrided by variant member OR with-member
            dict
          else:
            # No conflicting variant member OR with-member
            compiled-stmts = compile-nonlocal-member(
              constructed-obj, 
              member, 
              member-value, 
              member-stmts
            )
            dict.set(member.name, 
                     { none; compiled-stmts})
          end
        end

        field-stmt-list = for CL.map_list(k from shared-member-map.keys().to-list()):
          shared-member-map.get-value(k)
        end

        field-init = for CL.foldl({ fields; stmts } from { cl-empty; cl-empty },
                                  { optional-field; shared-stmts } from field-stmt-list):
          cases(Option) optional-field:
            | some(f) => { cl-append(fields, cl-sing(f)); cl-append(stmts, shared-stmts) }
            | none => { fields; cl-append(stmts, shared-stmts) }
          end
        end

        field-init
      end

      compiled-shared = for CL.map_list(member from shared):
        { member; compile-member(context, member) }
      end

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
          | s-variant(_, cl, shadow name, members, with-members) =>
            compiled-with = for CL.map_list(member from with-members):
              { member; compile-member(context, member) }
            end
            args = for CL.map_list(m from members): js-id-of(m.bind.id) end

            # Give object a temporary name to bind methods against
            constructor-tmp = fresh-id(compiler-name("constructorTMP"))
            { constructed-fields; constructed-stmts } = 
              resolve-init-names(j-id(constructor-tmp),
                                 compiled-shared, compiled-with, members)
            tmp-obj = j-obj(
                    [clist: j-field("$brand", j-id(js-id-of(variant-uniqs.get-value(name)))),
                            j-field("$tag", j-num(local-tag))] + 
                    constructed-fields
            )
            tmp-obj-var = j-var(constructor-tmp, tmp-obj)

            { j-field(name,
              j-fun("0", js-id-of(const-id(name)).toname(), args,
                j-block(cl-cons(tmp-obj-var, constructed-stmts) + 
                  cl-sing(j-return(j-id(constructor-tmp)))
                )
              )
            ); cl-empty }
          | s-singleton-variant(_, shadow name, with-members) =>
            compiled-with = for CL.map_list(member from with-members):
              { member; compile-member(context, member) }
            end

            constructor-tmp = fresh-id(compiler-name("constructorTMP"))
            { constructed-fields; constructed-stmts } = 
              resolve-init-names(j-id(constructor-tmp),
                                 compiled-shared, compiled-with, [list:])
            tmp-obj = j-obj(
                    [clist: j-field("$brand", j-id(js-id-of(variant-uniqs.get-value(name)))),
                            j-field("$tag", j-num(local-tag))] +
                    constructed-fields
            )
            tmp-obj-var = j-var(constructor-tmp, tmp-obj)

            { j-field(name, j-id(constructor-tmp)); cl-cons(tmp-obj-var, constructed-stmts) }
        end
      end

      { shadow variant-constructors; variant-cons-stmts } = 
       for CL.foldl({constructors; statements} from {cl-empty; cl-empty}, {vcons; vstmts} from variant-constructors):
        { cl-append(constructors, cl-sing(vcons)); cl-append(statements, vstmts) }
      end

      variant-recognizers = for CL.map_list(v from variants):
        j-field("is-" + v.name,
          j-fun("0", js-id-of(const-id(v.name)).toname(), [clist: const-id("val")],
            j-block1(
              j-return(j-binop(j-dot(j-id(const-id("val")), "$brand"), j-eq, j-id(js-id-of(variant-uniqs.get-value(v.name))))))))
      end

      compiled-shared-stmts = for CL.foldl(all-stmts from cl-empty, 
                                           { _shared-member; { shared-member-val; shared-member-stmts }} from compiled-shared):
        cl-append(all-stmts, shared-member-stmts)
      end

      { 
        j-obj(variant-constructors + variant-recognizers); 
        variant-uniq-defs + variant-cons-stmts + compiled-shared-stmts
      }
      
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

      tmp-bind = fresh-id(compiler-name("temporary"))

      {fieldvs; stmts; binds} = for fold({fieldvs; stmts; binds} from {cl-empty; cl-empty; cl-empty}, f from fields) block:
        when not(A.is-s-data-field(f)):
          raise("Can only provide data fields")
        end

        {val; compiled-stmts} = compile-expr(context, f.value)

        # Can only have s-method as the top-level expression of a field (i.e. no nesting)
        cases(Expr) f.value:
          | s-method(_, _, _, _, _, _, _, _, _, _) =>
            binder-func = val
            binder-stmts = compiled-stmts

            init-expr-rhs = j-app(binder-func, [clist: j-id(tmp-bind)])
            bind = j-expr(j-bracket-assign(j-id(tmp-bind), j-str(f.name), init-expr-rhs))

            # Binder function must be generated first
            { fieldvs; cl-append(binder-stmts, stmts); cl-cons(bind, binds) }

          | else => 
            # Fields are evaluated top to bottom
            { cl-snoc(fieldvs, j-field(f.name, val)); cl-append(stmts, compiled-stmts); binds }
        end
      end

      # Emit a temporary object to bind against
      var-obj = j-var(tmp-bind, j-obj(fieldvs))

      # Init statements and object bind come before method binding
      init-stmts = cl-snoc(stmts, var-obj)
      ordered-stmts = cl-append(init-stmts, binds)

      { j-id(tmp-bind); ordered-stmts }

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
    | s-method(l, name, params, args, ann, doc, body, _check-loc, _check, _blocky) =>
      # name is always empty according to parse-pyret.js:1280
      # TODO(alex): Make s-method in non(s-obj) or with/shared member context a well-formedness error
      #   Can only have s-method as the top-level expression of a field (i.e. no nesting)
      
      # NOTE(alex): Currently cannot do recursive object initialization
      #   Manually assign the shared/with member with j-bracket vs returning a j-field
      { binder-func; method-stmts } = compile-method(context, l, name, args, body)

      # Assume callers will generate binding code correctly
      { binder-func; method-stmts }
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
    | s-extend(l :: Loc, obj :: A.Expr, fields :: List<A.Member>) =>

      # Get the object to extend
      { to-extend; obj-stmts } = compile-expr(context, obj)

      # Perform a shallow copy of obj with JS(Object.assign)
      shallow-copy-fn = j-bracket(j-id(OBJECT), j-str("assign"))
      shallow-copy-name = fresh-id(compiler-name("shallow-copy"))
      shallow-copy-call = j-app(shallow-copy-fn, [clist: j-obj(cl-empty), to-extend])
      shallow-copy = j-var(shallow-copy-name, shallow-copy-call)

      prelude-stmts = cl-append(obj-stmts, cl-sing(shallow-copy))
      # Update the fields
      extend-stmts = for fold(stmts from prelude-stmts, field from fields) block:
        # TODO(alex): Assuming A.Member.s-data-field
        { extend-ans; extend-stmts } = compile-expr(context, field.value)
        field-extend = j-bracket-assign(j-id(shallow-copy-name), 
                                        j-str(field.name), 
                                        extend-ans)
        cl-append(stmts, cl-sing(j-expr(field-extend)))
      end

      rebind-stmt = j-expr(rt-method("$rebind", [clist: j-id(shallow-copy-name)]))

      { j-id(shallow-copy-name); cl-snoc(extend-stmts, rebind-stmt) }

    | s-for(l, iter, bindings, ann, body, blocky) => 
      compile-expr(context, DH.desugar-s-for(l, iter, bindings, ann, body))
    | s-id-var(l, ident) => 
      { j-id(js-id-of(ident)); cl-empty }
    | s-frac(l, num, den) => 
        # Generates a Rational (exact fraction)
        e = rt-method("_makeRational", 
                      [clist: j-num(num), j-num(den), rt-field(NUMBER_ERR_CALLBACKS)])
        { e; cl-empty }
    | s-rfrac(l, num, den) => 
        # Generates a Roughnum
        e = rt-method("_makeRoughnum", 
                      [clist: j-num(num / den), rt-field(NUMBER_ERR_CALLBACKS)])
        { e; cl-empty }
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

      # Create tuples by calling RUNTIME.MAKETUPLE(js-tuple-array)
      js-tuple-array = j-list(false, fieldvs)
      { j-app(j-bracket(j-id(RUNTIME), j-str(MAKETUPLE)), cl-sing(js-tuple-array)); stmts }
    | s-tuple-get(l, tup, index, index-loc) => 

      {tupv; tup-stmts} = compile-expr(context, tup)
      
      # Tuples represented as arrays
      {j-bracket(tupv, j-num(index)); tup-stmts}

    | s-ref(l, ann) => nyi("s-ref")
    | s-reactor(l, fields) => nyi("s-reactor")
    | s-table(l, headers, rows) =>
      # Set the table-import flag
      import-flags := import-flags.{ table-import: true }

      func = j-bracket(j-id(TABLE), j-str("_makeTable"))

      js-headers = for fold(the-list from cl-empty, h from headers):
	      cl-append( the-list, cl-sing( j-str(h.name) ) )
	    end

      { js-rows; js-row-stmts } = for fold({ value-list; stmt-list } from { cl-empty; cl-empty }, r from rows):
        row-elems = r.elems
        { elem-values; elem-stmts } = for fold({ elem-values; elem-stmts } from { cl-empty; cl-empty }, row-expr from row-elems ):
          { v; stmts } = compile-expr(context, row-expr)
          { cl-append(elem-values, cl-sing(v)); cl-append(elem-stmts, stmts) }
        end
        js-row = j-list(false, elem-values)
        
        # CList<CList<JExpr>> (CList<CList<j-list>>)
        { cl-append(value-list, cl-sing(js-row)); cl-append(stmt-list, elem-stmts) }
      end

      args = cl-cons(j-list(false, js-headers), cl-sing(j-list(false, js-rows)))

      { j-app(func, args); js-row-stmts }
    | s-paren(l, e) => 
        { e-ans; e-stmts } = compile-expr(context, e)
        { j-parens(e-ans); e-stmts }
    | s-let(_, _, _, _) => raise("desugared into s-let-expr")
    | s-var(l, name, value) => raise("desugared into s-let-expr")
    | s-check(l, name, body, keyword-check) => nyi("s-check")
    | s-check-test(l, op, refinement, left, right) => nyi("s-check-test")
    | s-load-table(
        l :: Loc,
        headers :: List<FieldName>,
        spec :: List<LoadTableSpec>) =>

      # This case handles `load-table` syntax. The lines in the following Pyret
      # code,
      #
      # | my-table = load-table: a, b, c
      # |   source: csv-open('my-table.csv')
      # | end
      #
      # compile into JavaScript code that resembles the following:
      #
      # | var myTable = _makeTableFromTableSkeleton(
      # |                 _tableSkeletonChangeHeaders(
      # |                   csvOpen('csv.txt'),
      # |                   ["a", "b", "

      # NOTE(michael):
      #  s-load-table is currently implemented for a single LoadTableSpec of type
      #  s-table-src, meaning that using one or more `sanitize` forms will result in
      #  a not-yet-implemented error.

      if spec.length() <> 1:
        nyi("s-load-table")
      else:
        cases (LoadTableSpec) spec.get(0) block:
          | s-sanitize(spec-l :: Loc, name :: Name, sanitizer :: Expr) =>
            nyi("s-load-table")
          | s-table-src(spec-l :: Loc, src :: Expr) =>
            # Set the table-import flag
            import-flags := import-flags.{ table-import: true }

            table-id :: JExpr = j-id(TABLE)
            make-table-func :: JExpr =
              j-bracket(table-id, j-str("_makeTableFromTableSkeleton"))
            change-headers-func :: JExpr =
              j-bracket(table-id, j-str("_tableSkeletonChangeHeaders"))

            { headers-expr-args :: JExpr; headers-expr-stmts :: CList<JStmt> } =
              compile-expr(context, src)

            header-strings-list :: CList<JExpr> =
              for fold(acc from cl-empty, field-name from headers):
                cl-append(acc, cl-sing(j-str(field-name.name)))
              end

            header-strings :: JExpr = j-list(false, header-strings-list)

            change-headers-expr :: JExpr =
              j-app(change-headers-func,
                cl-append(cl-sing(headers-expr-args), cl-sing(header-strings)))

            expr-args :: CList<JExpr> = cl-sing(change-headers-expr)
            make-table-expr :: JExpr = j-app(make-table-func, expr-args)

            { make-table-expr; headers-expr-stmts }
        end
      end
    | s-table-extend(
        l :: Loc,
        column-binds :: ColumnBinds,
        extensions :: List<TableExtendField>) =>

      # Set the table-import flag
      import-flags := import-flags.{ table-import: true }

      # This case handles `extend` syntax. The starred lines in the following
      # Pyret code,
      #
      #        | my-table = table: a, b, c
      #        |   row: 1, 2, 3
      #        |   row: 4, 5, 6
      #        |   row: 7, 8, 9
      #        | end
      #        |
      # *      | my-extended-table = extend my-table using a, b
      # *(Map) |   d: a / 2,           # a "Mapping" extension
      # *(Red) |   e: running-sum of b # a "Reducer"
      # *      | end
      #
      # compile into JavaScript code that resembles the following:
      #
      # *      | var myExtendedTable = _tableReduce(
      # *      |   myTable,
      # *      |   [
      # *(Map) |     { "type": "map",
      # *(Map) |       "reduce": (rowNumber) => {
      # *(Map) |         var columnNumberB = _tableGetColumnIndex(myTable, "b");
      # *(Map) |         var b = myTable["_rows"][rowNumber][columnNumberB];
      # *(Map) |         var columnNumberA = _tableGetColumnIndex(myTable, "a");
      # *(Map) |         var a = myTable["_rows"][rowNumber][columnNumberA];
      # *(Map) |         return a / 2; },
      # *(Map) |       "extending": "d" },
      # *(Red) |     { "type": "reduce",
      # *(Red) |       "one": runningSum["one"],
      # *(Red) |       "reduce": runningSum["reduce"],
      # *(Red) |       "using": "b",
      # *(Red) |       "extending": "e" }
      # *      |   ]);
      #
      # The actual "extending" work is done by _tableReduce at runtime.

      column-binds-l :: Loc = column-binds.l
      column-binds-binds :: List<Bind> = column-binds.binds
      column-binds-table :: Expr = column-binds.table
      { table-expr :: JExpr;
        table-stmts :: CList<JStmt> } =
        compile-expr(context, column-binds-table)

      { reducer-exprs :: CList<JExpr>;
        reducer-stmts :: CList<JStmt> } =
        for fold(
            { acc-exprs; acc-stmts } from { cl-empty; cl-empty },
            extension from extensions):
          cases (TableExtendField) extension block:
            | s-table-extend-reducer(
                shadow l :: Loc,
                name :: String,
                reducer :: Expr,
                col :: Name,
                ann :: Ann) =>

              # Handles Reducer forms, like `e: running-sum of b`.

              { reducer-expr :: JExpr;
                reducer-stmts :: CList<JStmt> } =
                compile-expr(context, reducer)

              type-field-name :: String = "type"
              type-field-value :: JExpr = j-str("reduce")
              type-field :: JField = j-field(type-field-name, type-field-value)

              one-field-name :: String = "one"
              one-field-value-obj :: JExpr = reducer-expr
              one-field-value-field :: JExpr = j-str("one")
              one-field-value :: JExpr =
                j-bracket(one-field-value-obj, one-field-value-field)
              one-field :: JField = j-field(one-field-name, one-field-value)

              reduce-field-name :: String = "reduce"
              reduce-field-value-obj :: JExpr = reducer-expr
              reduce-field-value-field :: JExpr = j-str("reduce")
              reduce-field-value :: JExpr =
                j-bracket(reduce-field-value-obj, reduce-field-value-field)
              reduce-field :: JField =
                j-field(reduce-field-name, reduce-field-value)

              using-field-name :: String = "using"
              using-field-value :: JExpr = j-str(col.toname())
              using-field :: JField =
                j-field(using-field-name, using-field-value)

              extending-field-name :: String = "extending"
              extending-field-value :: JExpr = j-str(name)
              extending-field :: JField =
                j-field(extending-field-name, extending-field-value)

              reducer-object-fields :: CList<JExpr> =
                cl-cons(type-field,
                  cl-cons(one-field,
                    cl-cons(reduce-field,
                      cl-cons(using-field,
                        cl-sing(extending-field)))))
              reducer-object :: JExpr = j-obj(reducer-object-fields)

              { cl-append(acc-exprs, cl-sing(reducer-object));
                cl-append(acc-stmts, reducer-stmts)}
            | s-table-extend-field(
                shadow l :: Loc,
                name :: String, # name of the new column
                value :: Expr,  # value of the element of the column in this row
                ann :: Ann) =>

              # Handles Mapping forms, like `d: a / 2`.

              type-field-name :: String = "type"
              type-field-value :: JExpr = j-str("map")
              type-field :: JField = j-field(type-field-name, type-field-value)

              reduce-field-name :: String = "reduce"

              fun-id :: String = "0"
              fun-name :: String = fresh-id(compiler-name("s-table-extend-field")).toname()
              row-number-name :: Name = fresh-id(compiler-name("row-number"))
              fun-args :: CList<Name> = cl-sing(row-number-name)
              indexing-stmts :: CList<JStmt> =
                for fold(stmts from cl-empty, bind from column-binds.binds):
                  bind-id :: Name = bind.id

                  get-index-name :: Name = fresh-id(compiler-name("column-number"))
                  get-column-index :: JExpr =
                    j-bracket(j-id(TABLE), j-str("_tableGetColumnIndex"))
                  column-index-args :: CList<JExpr> =
                    cl-cons(table-expr, cl-sing(j-str(bind-id.toname())))
                  get-index-rhs :: JExpr = j-app(get-column-index, column-index-args)
                  get-index-stmt :: JStmt = j-var(get-index-name, get-index-rhs)

                  index-name :: Name = bind-id
                  table-rows :: JExpr = j-bracket(table-expr, j-str("_rows"))
                  current-row :: JExpr = j-bracket(table-rows, j-id(row-number-name))
                  index-rhs :: JExpr = j-bracket(current-row, j-id(get-index-name))
                  assign-index-stmt :: JStmt = j-var(js-id-of(index-name), index-rhs)

                  cl-append(stmts, cl-cons(get-index-stmt, cl-sing(assign-index-stmt)))
                end

              { return-expr :: JExpr;
                return-compiled-stmts :: CList<JStmt> } =
                compile-expr(context, value)
              return-stmt :: JStmt = j-return(return-expr)

              body-stmts :: CList<JStmt> =
                cl-append(indexing-stmts, cl-sing(return-stmt))
              fun-body :: JBlock = j-block(body-stmts)

              reduce-field-value :: JExpr =
                j-fun(fun-id, fun-name, fun-args, fun-body)
              reduce-field :: JField =
                j-field(reduce-field-name, reduce-field-value)

              extending-field-name :: String = "extending"
              extending-field-value :: JExpr = j-str(name)
              extending-field :: JField =
                j-field(extending-field-name, extending-field-value)

              mapping-object-fields :: CList<JExpr> =
                cl-cons(type-field,
                  cl-cons(reduce-field,
                    cl-sing(extending-field)))
              mapping-object :: JExpr = j-obj(mapping-object-fields)

              { cl-append(
                  acc-exprs,
                  cl-append(return-compiled-stmts, cl-sing(mapping-object)));
                acc-stmts}
          end
        end

      expr-func-obj :: JExpr = j-id(TABLE)
      expr-func-field :: JExpr = j-str("_tableReduce")
      apply-expr-func :: JExpr = j-bracket(expr-func-obj, expr-func-field)
      apply-expr-args :: CList<JExpr> =
        cl-cons(table-expr, cl-sing(j-list(false, reducer-exprs)))
      apply-expr :: JExpr = j-app(apply-expr-func, apply-expr-args)

      apply-stmts :: CList<JStmt> =
        cl-append(table-stmts, reducer-stmts)

      { apply-expr; apply-stmts }
    | s-table-update(
        l :: Loc,
        column-binds :: ColumnBinds,
        updates :: List<A.Member>) =>

      # Set the table-import flag
      import-flags := import-flags.{ table-import: true }

      # This case handles `transform` syntax. The starred lines in the following
      # Pyret code,
      #
      #   | my-table = table: name, age, favorite-color
      #   |   row: "Bob", 12, "blue"
      #   |   row: "Alice", 17, "green"
      #   |   row: "Eve", 14, "red"
      #   | end
      #   |
      # * | age-fixed = transform my-table using age:
      # * |   age: age + 1
      # * | end
      #
      # compile into JavaScript code that resembles the following:
      #
      # * | var ageFixed = _tableTransform(
      # * |   myTable,
      # * |   ["age"],
      # * |   []
      # * | );
      #
      # The actual "transforming" work is done by _tableTransform at runtime.

      { table-expr :: JExpr; table-stmts :: CList<JStmt> } =
        compile-expr(context, column-binds.table)

      # makes a list of strings (column names)
      list-colnames :: CList<JExpr> = for fold(col-list from cl-empty, u from updates):
	      cl-append( col-list, cl-sing( j-str(u.name) ) )
	    end

      column-update-zip :: List<{ A.Binds; A.Expr}> = map2(lam(cb, up): { cb; up.value } end,
                               column-binds.binds,
                               updates)

      # makes a list of functions
      fun-id :: String = "0"
      fun-name :: String = fresh-id(compiler-name("s-table-transform")).toname()
      list-updates :: CList<JExpr> = for fold(update-list from cl-empty, 
                                              { bind; update-expr} from column-update-zip):
        
        # Use the Bind in ColumnBind as the parameter in the generated function
        fun-args :: CList<A.Name> = cl-sing(js-id-of(bind.id))

        spy: id: bind.id end

	      { u-value-expr; u-value-stmts } = compile-expr(context, update-expr)
        block-return-stmt :: JStmt = j-return(u-value-expr)
        block-stmts :: CList<JStmt> = cl-append(table-stmts, cl-sing(block-return-stmt))
        fun-body :: JBlock = j-block(block-stmts)
        u-fun :: JExpr = j-fun(fun-id, fun-name, fun-args, fun-body)
        cl-append( update-list, cl-sing( u-fun ) )
	    end

      app-func :: JExpr = j-bracket(j-id(TABLE), j-str("_tableTransform"))
      app-args :: CList<JExpr> = cl-cons( table-expr, 
        cl-cons( j-list(false, list-colnames), cl-sing(j-list(false, list-updates ))) )

      return-expr :: JExpr = j-app(app-func, app-args)
      return-stmts :: CList<JStmt> = table-stmts

      # tableTansform(table, colnames, updates)
      { return-expr; return-stmts }
    | s-table-select(l, columns, table) =>
      # Set the table-import flag
      import-flags := import-flags.{ table-import: true }

      func = j-bracket(j-id(TABLE), j-str("_selectColumns"))
     
      js-columns = for fold(the-list from cl-empty, c from columns):
	      cl-append( the-list, cl-sing( j-str(c.toname()) ) )
	    end
 
      { js-table; js-table-stmts } = compile-expr(context, table)

      args = cl-cons( js-table, cl-sing(j-list(false, js-columns)) )

      # selectColumns(table, colnames)
      { j-app(func, args); js-table-stmts }
    | s-table-extract(
        l :: Loc,
        column :: Name,
        table :: Expr) =>

      # Set the table-import flag
      import-flags := import-flags.{ table-import: true }

      # This case handles `extract` syntax. The starred line in the following
      # Pyret code,
      #
      #   | my-table = table: a, b, c
      #   |   row: 1, 2, 3
      #   |   row: 4, 5, 6
      #   |   row: 7, 8, 9
      #   | end
      #   |
      # * | column-b = extract b from my-table end
      #
      # compiles into JavaScript code that resembles the following:
      #
      # * | var columnB = _tableExtractColumn(myTable, "b");
      #
      # The actual "extracting" work is done by _tableExtractColumn at runtime.

      {table-expr :: JExpr; table-stmts :: CList<JStmt>} =
        compile-expr(context, table)

      app-func :: JExpr = j-bracket(j-id(TABLE), j-str("_tableExtractColumn"))
      app-args :: CList<JExpr> = cl-cons(table-expr, cl-sing(j-str(column.toname())))
      apply :: JExpr = j-app(app-func, app-args)

      return-expr :: JExpr = apply
      return-stmts :: CList<JStmt> = table-stmts

      { return-expr; return-stmts }
    | s-table-order(
        l :: Loc,
        table :: Any,
        ordering :: List<ColumnSort>) =>

      # Set the table-import flag
      import-flags := import-flags.{ table-import: true }

      # This case handles `order` syntax. The starred lines in the following
      # Pyret code,
      #
      #   | my-table = table: name, age, favorite-color
      #   |   row: "Bob", 12, "blue"
      #   |   row: "Alice", 12, "green"
      #   |   row: "Eve", 13, "red"
      #   | end
      #   |
      # * | name-ordered = order my-table:
      # * |   age descending,
      # * |   name ascending
      # * | end
      #
      # compile into JavaScript code that resembles the following:
      #
      # * | var nameOrdered = _tableOrder(
      # * |   myTable,
      # * |   [{"column": "age", "direction": "descending"},
      # * |    {"column": "name", "direction": "ascending"}]);
      #
      # The actual "ordering" work is done by _tableOrder at runtime.

      {table-expr :: JExpr; table-stmts :: CList<JStmt>} =
        compile-expr(context, table)

      ordering-list-elements :: CList<JExpr> =
        for fold(elements from cl-empty, the-order from ordering):
          the-order-column :: Name = the-order.column
          the-order-direction :: ColumnSortOrder = the-order.direction

          order-column-field :: JField =
            j-field("column", j-str(the-order-column.toname()))
          order-direction-field :: JField =
            j-field(
              "direction",
              j-str(
                cases (ColumnSortOrder) the-order-direction block:
                  | ASCENDING => "ascending"
                  | DESCENDING => "descending"
                end))

          order-fields :: CList<JField> =
            cl-cons(order-column-field, cl-sing(order-direction-field))
          order-obj :: JExpr = j-obj(order-fields)

          cl-append(elements, cl-sing(order-obj))
        end

      ordering-list-expr :: JExpr = j-list(false, ordering-list-elements)

      app-func :: JExpr = j-bracket(j-id(TABLE), j-str("_tableOrder"))
      app-args :: CList<JExpr> = cl-cons(table-expr, cl-sing(ordering-list-expr))
      apply :: JExpr = j-app(app-func, app-args)

      return-expr :: JExpr = apply
      return-stmts :: CList<JStmt> = table-stmts

      { return-expr; return-stmts }
    | s-table-filter(
        l :: Loc,
        column-binds :: ColumnBinds,
        predicate :: Expr) =>

      # Set the table-import flag
      import-flags := import-flags.{ table-import: true }

      # This case handles `sieve` syntax. The starred lines in the following
      # Pyret code,
      #
      #   | my-table = table: a, b, c
      #   |   row: 1, 2, 3
      #   |   row: 4, 5, 6
      #   |   row: 7, 8, 9
      #   | end
      #   | 
      # * | my-filtered-table = sieve my-table using b:
      # * |   (b / 4) == 2
      # * | end
      #
      # compile into JavaScript code that resembles the following:
      #
      # * | var myFilteredTable = _tableFilter(myTable, function sTableFilter(row) {
      # * |     var index = _tableGetColumnIndex(myTable, "b");
      # * |     var b = row[index];
      # * |     return (b / 4) == 2;
      # * | }
      #
      # The actual "sieving" work is done by _tableFilter at runtime.

      {table-expr :: JExpr; table-stmts :: CList<JStmt>} =
        compile-expr(context, column-binds.table)

      row-name :: Name = fresh-id(compiler-name("row"))

      block-row-element-stmts :: CList<JStmt> =
        # generate the `var index = _tableGetColumnIndex(myTable, "b");` lines.
        for fold(stmts from cl-empty, bind from column-binds.binds):
        app-func :: JExpr = j-bracket(j-id(TABLE), j-str("_tableGetColumnIndex"))
        app-args :: CList<JExpr> = cl-cons(table-expr, cl-sing(j-str(bind.id.toname())))

        # generate the `var b = row[index];` lines.
        column-index-expr :: JExpr = j-app(app-func, app-args)
        column-index-id :: Name = fresh-id(compiler-name("index"))
        column-index-stmt :: JStmt = j-var(column-index-id, column-index-expr)
        var-stmt :: JStmt = j-var(js-id-of(bind.id), j-bracket(j-id(row-name), j-id(column-index-id)))

        cl-append(stmts, cl-cons(column-index-stmt, cl-sing(var-stmt)))
      end

      fun-id :: String = "0"
      fun-name :: String = fresh-id(compiler-name("s-table-filter")).toname()
      fun-args :: CList<Name> = cl-sing(row-name)

      {predicate-expr :: JExpr; predicate-stmts :: CList<JStmt>} = compile-expr(context, predicate)
      block-return-stmt :: JStmt = j-return(predicate-expr)
      block-stmts :: CList<JStmt> = cl-append(block-row-element-stmts, cl-sing(block-return-stmt))
      fun-body :: JBlock = j-block(block-stmts)
      filter-fun :: JExpr = j-fun(fun-id, fun-name, fun-args, fun-body)

      app-func :: JExpr = j-bracket(j-id(TABLE), j-str("_tableFilter"))
      app-args :: CList<JExpr> = cl-cons(table-expr, cl-sing(filter-fun))
      apply :: JExpr = j-app(app-func, app-args)

      return-expr :: JExpr = apply
      return-stmts :: CList<JStmt> = cl-append(predicate-stmts, table-stmts)

      { return-expr; return-stmts }

    | s-spy-block(loc, message, contents) =>

      # TODO(alex): make code generation aware of spy block options
      # Emit with a special do-print/do-eval flag
      # Do not emit

      # Model each spy block as a spy block object
      # SpyBlockObject {
      #   message: () -> String,
      #   loc: String,
      #   exprs: List<{ key: String, expr: () -> JSValue, loc: String }>
      # }
      #
      # Translate spy blocks into:
      #   builtinSpyFunction(SpyBlockObject)
      if context.options.enable-spies:

        # Generate spy code

        # Generate message code
        { js-message-value; js-message-stmts } = cases(Option) message:
          | some(message-expr) => compile-expr(context, message-expr)
          
          # TODO(alex): null or empty string?
          | none => { j-null; cl-empty }
        end

        # Create the message generation function
        js-message-func-name = fresh-id(compiler-name("spy-message"))
        js-message-return = j-return(js-message-value)
        js-message-func-block = j-block(cl-append(js-message-stmts, cl-sing(js-message-return)))
        js-message-func = j-fun("0", js-message-func-name.to-compiled(), cl-empty, js-message-func-block)

        # Compile each spy expression into the expression list
        js-spy-fields = for fold(js-spy-fields from cl-empty, pyret-spy-field from contents):
          { js-spy-value; js-spy-stmts } = compile-expr(context, pyret-spy-field.value)

          js-spy-expr-func-name = fresh-id(compiler-name("spy-expr"))

          # TODO(alex): what are j-fun.id
          js-spy-return = j-return(js-spy-value)
          js-spy-expr-func-block = j-block(cl-append(js-spy-stmts, cl-sing(js-spy-return)))
          js-spy-expr-fun = j-fun("0", js-spy-expr-func-name.to-compiled(), cl-empty, js-spy-expr-func-block)

          # Create the spy expression object
          js-spy-key = j-field("key", j-str(pyret-spy-field.name))
          js-spy-expr = j-field("expr", js-spy-expr-fun)
          js-spy-loc = j-field("loc", j-str(pyret-spy-field.l.format(true)))
          js-spy-expr-obj = j-obj(cl-cons(js-spy-key, cl-cons(js-spy-expr, cl-sing(js-spy-loc))))

          cl-append(js-spy-fields, cl-sing(js-spy-expr-obj))
        end

        js-spy-loc = j-str(loc.format(true))
        
        # Create the SpyBlockObject
        js-spy-fields-list = j-list(false, js-spy-fields)
        spy-block-obj = j-obj(cl-cons(j-field("message", js-message-func),
                                cl-cons(j-field("loc", js-spy-loc),
                                  cl-sing(j-field("exprs", js-spy-fields-list))
                                  )
                                )
                              )

        # TODO(alex): builtin spy function call or inline formatting/reporting?
        # Builtin spy function call
        spy-call = rt-method("$spy", cl-sing(spy-block-obj))

        { j-undefined; cl-sing(spy-call) }
      else:
        # Do NOT generate spy code
        { j-undefined; cl-empty }
      end
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


fun create-prelude(prog, provides, env, options, shadow import-flags) block:
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

  fun import-builtin(bind-name :: A.Name, name :: String):
    J.j-var(bind-name, 
            j-app(j-id(const-id("require")), 
                  [clist: 
                    j-str( relative-path + runtime-builtin-relative-path + name)]))
  end

  global-import = import-builtin(GLOBAL, "global.arr.js")
  runtime-import = import-builtin(RUNTIME, "runtime.js")
  nothing-import = J.j-var(NOTHING, j-undefined)

  array-import = import-builtin(ARRAY, "array.arr.js")
  table-import = import-builtin(TABLE, "tables.arr.js")
  reactor-import = import-builtin(REACTOR,"reactor.arr.js")

  # Always emit global import
  manual-imports = [clist: global-import, runtime-import, nothing-import]

  shadow manual-imports = if import-flags.table-import:
    cl-append(manual-imports, cl-sing(table-import))
  else:
    manual-imports
  end
  shadow manual-imports = if import-flags.reactor-import:
    # TODO(alex): Implement reactor.arr.js
    # cl-append(manual-imports, cl-sing(reactor-import))
    raise("reactor.arr.js NYI")
  else:
    manual-imports
  end
  shadow manual-imports = if import-flags.array-import:
    # TODO(alex): Implement reactor.arr.js
    # cl-append(manual-imports, cl-sing(reactor-import))
    raise("array.arr.js NYI")
  else:
    manual-imports
  end

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
  # Reset import flags between compile-program calls
  import-flags := default-import-flags

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

  prelude = create-prelude(prog, provides, env, options, import-flags)

  # module-body = J.j-block(global-binds + stmts + [clist: j-return(ans)])
  module-body = J.j-block(prelude + stmts + [clist: j-return(ans)])

  the-module = module-body

  module-and-map = the-module.to-ugly-sourcemap(provides.from-uri, 1, 1, provides.from-uri)

  serialized-provides = PSE.compile-provides(provides)

  [D.string-dict:
    "requires", j-list(true, [clist:]),
    "provides", serialized-provides,
    "nativeRequires", j-list(true, [clist:]),
    "theModule", J.j-raw-code(module-and-map.code),
    "theMap", J.j-str(module-and-map.map)
    ]
end
