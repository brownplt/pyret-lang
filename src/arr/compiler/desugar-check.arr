#lang pyret

provide *
provide-types *
import ast as A
import ast-visitors as AV
import srcloc as SL
import lists as L
import file("gensym.arr") as G
import file("ast-util.arr") as U

data CheckInfo:
  | check-info(l :: SL.Srcloc, name :: String, body :: A.Expr)
end


fun ast-pretty(ast):
  A.s-str(ast.l, ast.tosource().pretty(80).join-str("\n"))
end

fun ast-lam(ast):
  A.s-lam(ast.l, "", [list: ], [list: ], A.a-blank, "", ast, none, none, true)
end

fun ast-srcloc(l):
  A.s-prim-app(l, "makeSrcloc", [list: A.s-srcloc(l, l)])
end

check-stmts-visitor = AV.default-map-visitor.{
  method s-check-test(self, l, op, refinement, left, right):
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
      | s-op-is(_)                =>
        cases(Option) refinement:
          | none                    => check-op("check-is")
          | some(shadow refinement) => check-refinement(refinement, "check-is-refinement")
        end
      | s-op-is-roughly(_) =>
        check-op("check-is-roughly")
      | s-op-is-not(_)              =>
        cases(Option) refinement:
          | none                    => check-op("check-is-not")
          | some(shadow refinement) => check-refinement(refinement, "check-is-not-refinement")
        end
      | s-op-is-op(_, opname)     =>
        check-refinement(A.s-id(l, A.s-name(l, A.get-op-fun-name(opname))), "check-is-refinement")
      | s-op-is-not-op(_, opname) =>
        check-refinement(A.s-id(l, A.s-name(l, A.get-op-fun-name(opname))), "check-is-not-refinement")
      | s-op-satisfies(_)         =>
        check-op("check-satisfies-delayed")
      | s-op-satisfies-not(_)     =>
        check-op("check-satisfies-not-delayed")
      | s-op-raises(_)            =>
        A.s-app(l, A.s-dot(l, U.checkers(l), "check-raises-str"),
          [list: ast-lam(left), right.value, ast-srcloc(l)])
      | s-op-raises-not(_)        =>
        A.s-app(l, A.s-dot(l, U.checkers(l), "check-raises-not"),
          [list: ast-lam(left), ast-srcloc(l)])
      | s-op-raises-other(_)      =>
        A.s-app(l, A.s-dot(l, U.checkers(l), "check-raises-other-str"),
          [list: ast-lam(left), right.value, ast-srcloc(l)])
      | s-op-raises-satisfies(_)  =>
        A.s-app(l, A.s-dot(l, U.checkers(l), "check-raises-satisfies"),
          [list: ast-lam(left), right.value, ast-srcloc(l)])
      | s-op-raises-violates(_)   =>
        A.s-app(l, A.s-dot(l, U.checkers(l), "check-raises-violates"),
          [list: ast-lam(left), right.value, ast-srcloc(l)])
      | else => raise("Check test operator " + op.label() + " not yet implemented at " + torepr(l))
    end
  end,
  method s-check(self, l, name, body, keyword-check):
    # collapse check blocks into top layer
    body.visit(self)
  end
}

fun get-checks(stmts):
  var standalone-counter = 0
  fun add-check(shadow stmts):
    # Note: manually writing this fold, rather than using existing functions
    # foldr produces numbers that are backwards, and
    # foldl would require an extra list allocation and reversal
    cases(List) stmts:
      | empty => empty
      | link(stmt, rest) =>
        cases(A.Expr) stmt:
          | s-fun(l, name, _, _, _, _, _, _, _check, _) =>
            cases(Option) _check:
              | some(v) => link(check-info(l, name, v.visit(check-stmts-visitor)), add-check(rest))
              | none => add-check(rest)
            end
          | s-data(l, name, _, _, _, _, _, _check) =>
            cases(Option) _check:
              | some(v) => link(check-info(l, name, v.visit(check-stmts-visitor)), add-check(rest))
              | none => add-check(rest)
            end
          | s-check(l, name, body, keyword-check) =>
            check-name = cases(Option) name block:
              | none =>
                standalone-counter := standalone-counter + 1
                (if keyword-check: "check-block-" else: "examples-block-" end)
                  + tostring(standalone-counter)
              | some(v) => v
            end
            link(check-info(l, check-name, body.visit(check-stmts-visitor)), add-check(rest))
          | else => add-check(rest)
        end
    end
  end
  add-check(stmts)
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
  A.s-lam(l, "", [list: ], args.map(lam(sym): A.s-bind(l, false, sym, A.a-blank) end), A.a-blank, "", body, none, none, true)
end

no-checks-visitor = AV.default-map-visitor.{
  method s-block(self, l, stmts):
    new-stmts = for L.foldr(acc from empty, stmt from stmts):
      new-stmt = stmt.visit(self)
      if A.is-s-id(new-stmt) and A.is-s-name(new-stmt.id) and (new-stmt.id.s == "$elidedCheckBlock"):
        acc
      else:
        link(new-stmt, acc)
      end
    end
    A.s-block(l, new-stmts)
  end,
  method s-fun(self, l, name, params, args, ann, doc, body, _, _, blocky):
    A.s-fun(l, name, params, args, ann, doc, body, none, none, blocky)
  end,
  method s-data(self, l, name, params, mixins, variants, shared-members, _, _):
    A.s-data(l, name, params, mixins, variants, shared-members, none, none)
  end,
  method s-lam(self, l, name, params, args, ann, doc, body, _, _, blocky):
    A.s-lam(l, name, params, args, ann, doc, body, none, none, blocky)
  end,
  method s-check(self, l, name, body, keyword-check):
    # Because we now weave contracts in, and because examples blocks can go between
    # mutually-recursive functions, we need to change our desugaring of elided check blocks
    # to be completely removed, rather than be a nilpotent expression
    A.s-id(l, A.s-name(l, "$elidedCheckBlock"))
  end
}

check-visitor = AV.default-map-visitor.{
  method s-block(self, l, stmts):
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
              A.s-let(l, A.s-bind(l, true, id-result, A.a-blank), last-expr, false),
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
