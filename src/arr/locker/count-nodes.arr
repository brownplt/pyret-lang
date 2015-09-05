#lang pyret

provide *
import ast as A
import json as J
import file as F
import directory as D
import pyret-eval as E
import format as format
import namespaces as N
import io as IO

JS-ENV = N.library-env.{
  equiv: true,
  data-equals: true,
  data-to-repr: true
}

fun read-then-close(path):
  file = F.input-file(path)
  contents = file.read-file()
  file.close-file()
  contents
end

moorings-ast = A.parse-tc(
    read-then-close("libs/moorings.arr"),
    "moorings.arr",
     { env : JS-ENV }
  )

fun count-in(str):
   prog = A.parse(str, "count", {env : JS-ENV}).post-desugar
   count-nodes(prog.block)
end

#Counts the number of nodes in an ast
fun count-nodes(ast):
  cases(A.Expr) ast:
    | s-block(_, stmts) =>
       stmt-count = for fold(total from 0, stmt from stmts):
            total + count-nodes(stmt)
        end
       1 + stmt-count
    | s-user-block(s, expr) => count-nodes(expr) + 1
    | s-num(_, n) => 1
#    | s-frac(_, num, den) => 1
    | s-str(_, s) => 1
    | s-bool(_, b) => 1
    | s-lam(_, _, args, _, doc, body, _) => 1 + count-nodes(body)
    | s-method(_, args, _, doc, body, _) => 1 + count-nodes(body)
    | s-app(_, f, args) =>
        arg-count = for fold(total from 0, arg from args):
            total + count-nodes(arg)
        end
        1 + arg-count + count-nodes(f)
    | s-bracket(_, obj, f) => 1 + count-nodes(obj)
    | s-bracket-k(_, conts, obj, f) => 1 + count-nodes(obj) + count-nodes(conts)
    | s-update-k(_, conts, obj, fields) => 1 + count-nodes(obj) + count-nodes(conts)
    | s-id(_, id) => 1
    | s-var(_, bind, value) => 1 + count-nodes(value)
    | s-assign(_, id, value) => 1 + count-nodes(value)
    | s-let(_, bind, value) =>  1 + count-nodes(value)
    | s-obj(_, fields) =>
        field-count = for fold(total from 0, field from fields):
            total + count-nodes(field.value)
        end
        1 + field-count
    | s-extend(_, obj, fields) =>
        field-count = for fold(total from 0, field from fields):
            total + count-nodes(field.value)
        end
        1 + field-count + count-nodes(obj)
    | s-if-else(_, branches,_else) =>
        branch-count = for fold(total from 0, branch from branches):
            total + count-nodes(branch.body) + count-nodes(branch.test)
        end
        1 + branch-count + count-nodes(_else)
    | s-try(_, body, bind, _except) => 1 + count-nodes(body) + count-nodes(_except)
    | s-get-bang(l, obj, field) => 1 + count-nodes(obj)
    | s-update(l, supe, fields) =>
        field-count = for fold(total from 0, field from fields):
            total + count-nodes(field.value)
        end
        1 + field-count + count-nodes(supe)
    | else => raise("ELSE")
    end
where:
    count-in("1") is 1 + 1
    count-in("{a : 1}") is 1 + 1 + 1
    count-in("1 + 1") is 1 + 2 + 1 + 1
    count-in("{a : 1, b : 3, c : 4}") is 1 + 1 + 3

    count-in("{a : 1 + 1}") is 1 + 1 + 4
    count-in("{a : {}}") is 1 + 1 + 1
    count-in("1\n1") is 1 + 1 + 1

    count-in("if (true): 1 else: 0;") is 7
    count-in("if (true): 1 else if (false): 1 + 1 else: 0;") is 13
    count-in("{}.x") is 3
    count-in("{}:x") is 3
    count-in("{}!x") is 3

    count-in("var x = 1 + 1 \n x := f()") is 4 + 1 + 1 + 3
    count-in("x") is 2
    count-in(```fun x(n):
                    f(n)
                    print(n)
                end```
            ) is 10
    count-in("{}.{a : 1 + 1, b : f()}") is 1 + 1 + 1 + 4 + 2
    count-in("{}!{a : 1 + 1, b : f()}") is 1 + 1 + 1 + 4 + 2
    count-in("{}!{a : 1 + 1, b : lam(x): x;}") is 1 + 1 + 1 + 4 + 3
end

#moorings ast should be an s-program, so we'll grab the block from it
#print("NOT CPS")
#count-nodes(moorings-ast.block)

#import pyret-to-js.arr as P
#print("CPS")
#count-nodes(P.cps(moorings-ast.block))
