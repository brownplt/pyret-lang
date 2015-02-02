#lang pyret

import AST as A

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

fun from-ast(ast):
  ask:
    | is-Name(ast): "NYI"
    | is-List(ast):
      cases(List) ast:
        | empty             =>
          node("Empty", no-loc, [list:])
        | link(first, rest) =>
          node("Link:" + first.label(), first.l,
            [list: from-ast(first), from-ast(rest)])
      end
    | otherwise:
      name = ast.label()
      children = ast.children()
      node(name, ast.l, map(from-ast, children))
  end
end

fun to-ast(node):
  ask:
    | node.name == "Empty" then:
      [list:]
    | string-contains(node.name, "Link:") then:
      link(to-ast(node.get(0)), to-ast(node.get(1)))
    | otherwise:
      "NYI"
  end
end
