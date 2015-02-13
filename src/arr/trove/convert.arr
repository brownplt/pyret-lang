#lang pyret

provide {
  to-ast: to-ast,
  from-ast: from-ast
} end

import string-dict as D
import srcloc as SL
import resugar as R
import ast as A


dummy-loc = SL.builtin("dummy location")

fun is-List(x):
  is-empty(x) or is-link(x)
end

fun is-Option(x):
  Option(x)
end

fun loc(ast):
  if A.is-ConstructModifier(ast) or A.is-VariantMemberType(ast) or A.is-CasesBindType
    or A.is-a-blank(ast) or A.is-a-any or A.is-a-checked:
    dummy-loc
  else:
    ast.l
  end
end

fun apply(func, args):
  len = args.length()
  ask:
    | len == 0 then:
      func()
    | len == 1 then:
      func(args.get(0))
    | len == 2 then:
      func(args.get(0), args.get(1))
    | len == 3 then:
      func(args.get(0), args.get(1), args.get(2))
    | len == 4 then:
      func(args.get(0), args.get(1), args.get(2), args.get(3))
    | len == 5 then:
      func(args.get(0), args.get(1), args.get(2), args.get(3), args.get(4))
    | len == 6 then:
      func(args.get(0), args.get(1), args.get(2), args.get(3), args.get(4),
      args.get(5))
    | len == 7 then:
      func(args.get(0), args.get(1), args.get(2), args.get(3), args.get(4),
      args.get(5), args.get(6))
    | len == 8 then:
      func(args.get(0), args.get(1), args.get(2), args.get(3), args.get(4),
        args.get(5), args.get(6), args.get(7))
    | len == 9 then:
      func(args.get(0), args.get(1), args.get(2), args.get(3), args.get(4),
        args.get(5), args.get(6), args.get(7), args.get(8))
    | otherwise:
      raise("apply: Nine arguments should be enough for anybody.")
  end
end

  # After desugaring:
  # - contains no s-var, s-fun, s-data, s-check, or s-check-test
  # - contains no s-provide in headers
  # - all where blocks are none
  # - contains no s-name (e.g. call resolve-names first)
  # - contains no s-for, s-if, s-op, s-method-field,
  #               s-not, s-when, s-if-pipe, s-paren
  # - contains no s-underscore in expression position (but it may
  #   appear in binding positions as in s-let-bind, s-letrec-bind)

constructors = [D.string-dict:
  # Not handled (because they are outside the main program body)
  "s-program", A.s-program,
  "s-import", A.s-import,
  "s-import-types", A.s-import-types,
  "s-import-fields", A.s-import-fields,
  "s-provide", A.s-provide,
  "s-provide-all", A.s-provide-all,
  "s-provide-none", A.s-provide-none,
  "s-provide-types", A.s-provide-types,
  "s-provide-types-all", A.s-provide-types-all,
  "s-provide-types-none", A.s-provide-types-none,
  "s-file-import", A.s-file-import,
  "s-const-import", A.s-const-import,
  "s-special-import", A.s-special-import,
  "s-hint-exp", A.s-hint-exp,
  "s-hint", A.h-use-loc,
  # Desugared:
  "s-var", A.s-var,
  "s-fun", A.s-fun,
  "s-op", A.s-op,
  "s-method-field", A.s-method-field,
  "s-paren", A.s-paren,

  "s-data", A.s-data,
  "s-variant-member", A.s-variant-member,
  "s-variant", A.s-variant,
  "s-singleton-variant", A.s-singleton-variant,
  "s-datatype-constructor", A.s-datatype-constructor,
  "s-normal", A.s-normal,
  "s-mutable", A.s-mutable,

  "s-check", A.s-check,
  "s-check-test", A.s-check-test,
  "s-check-expr", A.s-check-expr, #?

  "s-for", A.s-for,
  "s-for-bind", A.s-for-bind,

  "s-if", A.s-if,
  "s-if-pipe", A.s-if-pipe,
  "s-when", A.s-when,

  "s-op-is", A.s-op-is,
  "s-op-is-op", A.s-op-is-op,
  "s-op-is-not", A.s-op-is-not,
  "s-op-is-not-op", A.s-op-is-not-op,
  "s-op-satisfies", A.s-op-satisfies,
  "s-op-satisfies-not", A.s-op-satisfies-not,
  "s-op-raises", A.s-op-raises,
  "s-op-raises-other", A.s-op-raises-other,
  "s-op-raises-not", A.s-op-raises-not,
  "s-op-raises-satisfies", A.s-op-raises-satisfies,
  "s-op-raises-violates", A.s-op-raises-violates,

  "s-let", A.s-let,

  # Dead code?:
  "s-datatype-variant", A.s-datatype-variant,
  "s-datatype-singleton-variant", A.s-datatype-singleton-variant,
  
  # TODO:
  "s-let-bind", A.s-let-bind,
  "s-var-bind", A.s-var-bind,
  "s-letrec-bind", A.s-letrec-bind,
  "s-type-bind", A.s-type-bind,
  "s-newtype-bind", A.s-newtype-bind,
  "s-module", A.s-module,
  "s-type-let-expr", A.s-type-let-expr,
  "s-let-expr", A.s-let-expr,
  "s-letrec", A.s-letrec,
  "s-instantiate", A.s-instantiate,
  "s-block", A.s-block,
  "s-user-block", A.s-user-block,
  "s-type", A.s-type,
  "s-newtype", A.s-newtype,
  "s-rec", A.s-rec,
  "s-ref", A.s-ref,
  "s-contract", A.s-contract,
  "s-assign", A.s-assign,
  "s-if-pipe-else", A.s-if-pipe-else,
  "s-if-else", A.s-if-else,
  "s-cases", A.s-cases,
  "s-cases-else", A.s-cases-else,
  "s-lam", A.s-lam,
  "s-method", A.s-method,
  "s-extend", A.s-extend,
  "s-update", A.s-update,
  "s-obj", A.s-obj,
  "s-array", A.s-array,
  "s-construct", A.s-construct,
  "s-app", A.s-app,
  "s-prim-app", A.s-prim-app,
  "s-prim-val", A.s-prim-val,
  "s-id", A.s-id,
  "s-id-var", A.s-id-var,
  "s-id-letrec", A.s-id-letrec,
  "s-undefined", A.s-undefined,
  "s-srcloc", A.s-srcloc,
  "s-num", A.s-num,
  "s-frac", A.s-frac,
  "s-bool", A.s-bool,
  "s-str", A.s-str,
  "s-dot", A.s-dot,
  "s-get-bang", A.s-get-bang,
  "s-bracket", A.s-bracket,
  "s-data-expr", A.s-data-expr,
  "s-construct-normal", A.s-construct-normal,
  "s-construct-lazy", A.s-construct-lazy,
  "s-bind", A.s-bind,
  "s-data-field", A.s-data-field,
  "s-mutable-field", A.s-mutable-field,
  "s-if-branch", A.s-if-branch,
  "s-if-pipe-branch", A.s-if-pipe-branch,
  "s-cases-bind-ref", A.s-cases-bind-ref,
  "s-cases-bind-normal", A.s-cases-bind-normal,
  "s-cases-bind", A.s-cases-bind,
  "s-cases-branch", A.s-cases-branch,
  "s-singleton-cases-branch", A.s-singleton-cases-branch,
  "a-blank", A.a-blank,
  "a-any", A.a-any,
  "a-name", A.a-name,
  "a-type-var", A.a-type-var,
  "a-arrow", A.a-arrow,
  "a-method", A.a-method,
  "a-record", A.a-record,
  "a-app", A.a-app,
  "a-pred", A.a-pred,
  "a-dot", A.a-dot,
  "a-checked", A.a-checked,
  "a-field", A.a-field
]

fun from-ast(ast):
  ask:
    | A.is-Name(ast) then:
      "NYI"
    | A.is-s-value(ast) then:
      R.t-val(ast.val)
    | SL.is-Srcloc(ast) then:
      raise("from-ast: Srclocs not yet implemented")
    | is-boolean(ast) then:
      R.node("Bool", ast.l, [list: R.value(ast)])
    | is-string(ast) then:
      R.node("Str", ast.l, [list: R.value(ast)])
    | is-Option(ast) then:
      cases(Option) ast:
        | none =>
          R.node("None", dummy-loc, [list:])
        | some(shadow ast) =>
          R.node("Some",
            loc(ast),
            [list: from-ast(ast)])
      end
    | is-List(ast) then:
      cases(List) ast:
        | empty             =>
          R.node("Empty", dummy-loc, [list:])
        | link(first, rest) =>
          R.node("Link",
            loc(first),
            [list: from-ast(first), from-ast(rest)])
      end
    | otherwise:
      name = ast.label()
      children = ast.children()
      R.node(name, loc(ast), map(from-ast, children))
  end
end

fun to-ast(node :: R.Term):
  cases(R.Term) node:
    | t-decl(v)  => "NYI"
    | t-refn(v)  => "NYI"
    | t-val(val)   =>
      A.s-value(val)
    | t-hole(_)  => raise("to-ast: cannot convert context hole")
    | t-tag(_, _, term) => raise("to-ast: cannot conver tag")
    | t-node(name, id, l, ts) =>
      ask:
        | name == "Bool" then:
          node.get(0).val
        | name == "Str" then:
          node.get(0).val
        | name == "Empty" then:
          [list:]
        | name == "s-bool" then:
          A.s-bool(l, ts.get(0).val)
        | name == "s-num" then:
          A.s-num(l, ts.get(0).val)
        | name == "s-str" then:
          A.s-str(l, ts.get(0).val)
        | name == "Link" then:
          link(to-ast(node.get(0)), to-ast(node.get(1)))
        | name == "None" then:
          none
        | name == "Some" then:
          some(to-ast(node.get(0)))
        | otherwise:
          children = map(to-ast, ts)
          cases(Option) constructors.get(name):
            | some(con) =>
              apply(con, link(l, children))
            | none =>
              raise("to-ast: Unrecognized AST node: " + name)
          end
      end
  end
end
