#lang pyret

import string-dict as D
import srcloc as S
import AST as A
import "compiler/resugar.arr" as R

dummy-loc = S.builtin("dummy location")

fun is-List(x):
  is-empty(x) or is-link(x)
end

fun apply(func, args):
  len = args.length()
  ask:
    | len == 0 then:
      func(args.get(0))
    | len == 1 then:
      func(args.get(0), args.get(1))
    | len == 2 then:
      func(args.get(0), args.get(1), args.get(2))
    | len == 3 then:
      func(args.get(0), args.get(1), args.get(2), args.get(3))
    | len == 4 then:
      func(args.get(0), args.get(1), args.get(2), args.get(3), args.get(4))
    | len == 5 then:
      func(args.get(0), args.get(1), args.get(2), args.get(3), args.get(4),
      args.get(5))
    | len == 6 then:
      func(args.get(0), args.get(1), args.get(2), args.get(3), args.get(4),
      args.get(5), args.get(6))
    | len == 7 then:
      func(args.get(0), args.get(1), args.get(2), args.get(3), args.get(4),
        args.get(5), args.get(6), args.get(7))
    | len == 8 then:
      func(args.get(0), args.get(1), args.get(2), args.get(3), args.get(4),
        args.get(5), args.get(6), args.get(7), args.get(8))
    | otherwise:
      raise("apply: Eight arguments should be enough for anybody.")
  end
end


fun nt(constr):
  { constructor: constr }
end

node-types = [D.string-dict:
  "s-program", nt(A.s-program)
]


fun from-ast(ast):
  ask:
    | A.is-Name(ast) then:
      "NYI"
    | is-List(ast) then:
      cases(List) ast:
        | empty             =>
          R.node("Empty", dummy-loc, [list:])
        | link(first, rest) =>
          R.node("Link:" + first.label(), first.l,
            [list: from-ast(first), from-ast(rest)])
      end
    | otherwise:
      name = ast.label()
      children = ast.children()
      R.node(name, ast.l, map(from-ast, children))
  end
end

fun to-ast(node :: R.Node):
  cases(R.Node) node:
    | t-decl(v)  => "NYI"
    | t-refn(v)  => "NYI"
    | t-val(_)   => raise("to-ast: cannot store values in an AST")
    | t-hole(_)  => raise("to-ast: cannot convert context hole")
    | t-tag(_, _, term) => raise("to-ast: cannot conver tag")
    | t-node(name, id, l, ts) =>
      ask:
        | name == "Empty" then:
          [list:]
        | string-contains(name, "Link:") then:
          link(to-ast(node.get(0)), to-ast(node.get(1)))
        | otherwise:
          children = map(to-ast, ts)
          constructor = node-types.get(name).constructor
          apply(constructor, link(l, children))
      end
  end
end

check:
  2 is 2
end