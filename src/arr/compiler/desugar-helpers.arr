provide *
provide-types *

import srcloc as SL
import file("ast.arr") as A
import file("ast-util.arr") as AU
import file("js-ast.arr") as J
import file("gensym.arr") as G
import file("concat-lists.arr") as CL
import file("type-structs.arr") as T
import pathlib as P
import sha as sha
import string-dict as D

type Expr = A.Expr
type Loc = SL.Srcloc
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


data DesugarResult:
  | pyret(ast)
  | js(ast)
end

fun desugar-s-for(loc, iter :: A.Expr, bindings :: List<A.ForBind>, ann :: A.Ann, body :: A.Expr):
  # Split binds and their values
  { binds; args } = for fold({ bl; el } from { empty; empty }, fb from bindings):
    cases(A.ForBind) fb:
      | s-for-bind(l, bind, arg) =>
        { bl.push(bind); el.push(arg) }
    end
  end

  shadow binds = binds.reverse()
  shadow args = args.reverse()

  # TODO(alex): for loops are documented to desugar
  #   for f1(v1 from s1..., vn from sn) block end
  # into
  #   f1(lam(v1...vn) block end, s1...sn)
  # HOWEVER: Some iterator-like functions are of the form:
  #   f1(s1...sn, lam(v1..vn) block end)
  # (i.e. builtin list map)
  # Is that an error? Should for loops be desugared into that instead?
  lambda-for = A.s-lam(loc, "", empty, binds, ann, "", body, none, none, true)
  A.s-app(loc,
          iter,
          link(lambda-for, args)
         )
end

fun desugar-s-op(loc, op-l, op, l, r):
  ask:
    | op == "op==" then:
      A.s-prim-app(
                  loc,
                  "equal-always", 
                  [list: l, r], flat-prim-app 
      ) 
    | op == "op<>" then:
      inner = A.s-prim-app(
                  loc,
                  "equal-always", 
                  [list: l, r], flat-prim-app 
      )
      A.s-prim-app(
                  loc,
                  "not", 
                  [list: inner], flat-prim-app 
      )
    | otherwise:
      A.s-op(loc, op-l, op, l, r)
    end
end
