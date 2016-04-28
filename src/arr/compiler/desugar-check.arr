#lang pyret

provide *
provide-types *
import ast as A
import srcloc as SL
import "compiler/gensym.arr" as G
import "compiler/ast-util.arr" as U

data CheckInfo:
  | check-info(l :: SL.Srcloc, name :: String, body :: A.Expr)
end


fun ast-pretty(ast):
  A.s-str(ast.l, ast.tosource().pretty(80).join-str("\n"))
end

fun ast-lam(ast):
  A.s-lam(ast.l, [list: ], [list: ], A.a-blank, "", ast, none)
end

fun ast-srcloc(l):
  A.s-prim-app(l, "makeSrcloc", [list: A.s-srcloc(l, l)])
end

check-stmts-visitor = A.default-map-visitor.{
  s-check-test(self, l, op, refinement, left, right):
    term = A.s-check-test(l, op, refinement, left, right)
    fun check-op(fieldname):
      A.s-app(l, A.s-dot(l, U.checkers(l), fieldname),
        [list: ast-lam(left), ast-lam(right.value), ast-srcloc(l)])
    end
    fun check-refinement(shadow refinement, fieldname):
      A.s-app(l, A.s-dot(l, U.checkers(l), fieldname),
        [list: refinement, ast-lam(left), ast-lam(right.value), ast-srcloc(l)])
    end
    cases(A.CheckOp) op:
      | s-op-is            =>
        cases(Option) refinement:
          | none                    => check-op("check-is")
          | some(shadow refinement) => check-refinement(refinement, "check-is-refinement")
        end
      | s-op-is-not        =>
        cases(Option) refinement:
          | none                    => check-op("check-is-not")
          | some(shadow refinement) => check-refinement(refinement, "check-is-not-refinement")
        end
      | s-op-is-op(opname)    =>
        check-refinement(A.s-id(l, A.s-name(l, A.get-op-fun-name(opname))), "check-is-refinement")
      | s-op-is-not-op(opname) =>
        check-refinement(A.s-id(l, A.s-name(l, A.get-op-fun-name(opname))), "check-is-not-refinement")
      | s-op-satisfies        =>
        check-op("check-satisfies-delayed")
      | s-op-satisfies-not    =>
        check-op("check-satisfies-not-delayed")
      | s-op-raises           =>
        A.s-app(l, A.s-dot(l, U.checkers(l), "check-raises-str"),
          [list: ast-lam(left), right.value, ast-srcloc(l)])
      | s-op-raises-not       =>
        A.s-app(l, A.s-dot(l, U.checkers(l), "check-raises-not"),
          [list: ast-lam(left), ast-srcloc(l)])
      | s-op-raises-other     =>
        A.s-app(l, A.s-dot(l, U.checkers(l), "check-raises-other-str"),
          [list: ast-lam(left), right.value, ast-srcloc(l)])
      | s-op-raises-satisfies =>
        A.s-app(l, A.s-dot(l, U.checkers(l), "check-raises-satisfies"),
          [list: ast-lam(left), right.value, ast-srcloc(l)])
      | s-op-raises-violates  =>
        A.s-app(l, A.s-dot(l, U.checkers(l), "check-raises-violates"),
          [list: ast-lam(left), right.value, ast-srcloc(l)])
      | else => raise("Check test operator " + op.label() + " not yet implemented at " + torepr(l))
    end
  end,
  s-check(self, l, name, body, keyword-check):
    # collapse check blocks into top layer
    body.visit(self)
  end
}

fun get-checks(stmts):
  var standalone-counter = 0
  fun add-check(stmt, lst):
    cases(A.Expr) stmt:
      | s-fun(l, name, _, _, _, _, _, _check) =>
        cases(Option) _check:
          | some(v) => link(check-info(l, name, v.visit(check-stmts-visitor)), lst)
          | none => lst
        end
      | s-data(l, name, _, _, _, _, _check) =>
        cases(Option) _check:
          | some(v) => link(check-info(l, name, v.visit(check-stmts-visitor)), lst)
          | none => lst
        end
     | s-check(l, name, body, keyword-check) =>
        check-name = cases(Option) name:
          | none =>
            standalone-counter := standalone-counter + 1
            "check-block-" + tostring(standalone-counter)
          | some(v) => v
        end
        link(check-info(l, check-name, body.visit(check-stmts-visitor)), lst)
      | else => lst
    end
  end
  stmts.foldr(add-check, [list: ])
end

fun create-check-block(l, checks):
  fun create-checker(c):
    cases(CheckInfo) c:
      | check-info(l2, name, body) =>
        check-fun = make-lam(l2, [list: ], body)
        A.s-obj(l2, [list: 
            A.s-data-field(l2, "name", A.s-str(l2, name)),
            A.s-data-field(l2, "run", check-fun),
            A.s-data-field(l2, "location", A.s-prim-app(l2, "makeSrcloc", [list: A.s-srcloc(l2, l2)]))
          ])
    end
  end
  checkers = checks.map(create-checker)
  A.s-block(l, [list: 
      A.s-app(l, A.s-dot(l, U.checkers(l), "run-checks"), [list: 
          A.s-str(l, l.source),
          # TODO(joe): need to make this a s-global somehow
          A.s-construct(l, A.s-construct-normal, A.s-id(l, A.s-name(l, "list")), checkers)
        ])
    ])
end

fun make-lam(l, args, body):
  A.s-lam(l, [list: ], args.map(lam(sym): A.s-bind(l, false, sym, A.a-blank) end), A.a-blank, "", body, none)
end

no-checks-visitor = A.default-map-visitor.{
  s-block(self, l, stmts):
    A.s-block(l, stmts.map(_.visit(self)))
  end,
  s-fun(self, l, name, params, args, ann, doc, body, _):
    A.s-fun(l, name, params, args, ann, doc, body, none)
  end,
  s-data(self, l, name, params, mixins, variants, shared-members, _):
    A.s-data(l, name, params, mixins, variants, shared-members, none)
  end,
  s-lam(self, l, params, args, ann, doc, body, _):
    A.s-lam(l, params, args, ann, doc, body, none)
  end,
  s-check(self, l, name, body, keyword-check):
    A.s-id(l, A.s-name(l, "nothing"))
  end
}

check-visitor = A.default-map-visitor.{
  s-block(self, l, stmts):
    checks-to-perform = get-checks(stmts)
    ds-stmts = stmts.map(_.visit(self))
    do-checks = create-check-block(l, checks-to-perform)
    if is-empty(checks-to-perform): A.s-block(l, ds-stmts)
    else if is-empty(ds-stmts): raise("Empty block")
    else:
      id-result = A.s-name(l, G.make-name("result-after-checks"))
      last-expr = ds-stmts.last()
      A.s-block(
          l,
          ds-stmts.take(ds-stmts.length() - 1) +
            [list: 
              A.s-let(l, A.s-bind(l, false, id-result, A.a-blank), last-expr, false),
              do-checks,
              A.s-id(l, id-result)
            ]
        )
    end
  end
}

fun desugar-check(prog):
  doc: ```
        Desugars all check blocks to be calls to the current checker
        Preconditions on prog:
          - well-formed
        Postconditions on prog:
          - contains no s-check or s-check-test statements
          - all where blocks on s-lam, s-fun, s-data, s-method are none
        ```
  prog.visit(check-visitor)
end

fun desugar-no-checks(prog):
  prog.visit(no-checks-visitor)
end
