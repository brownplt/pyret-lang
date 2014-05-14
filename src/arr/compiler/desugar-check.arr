#lang pyret

provide *
import ast as A
import srcloc as SL
import "compiler/gensym.arr" as G
import "compiler/ast-util.arr" as U

data CheckInfo:
  | check-info(l :: SL.Srcloc, name :: String, body :: A.Expr)
end


check-stmts-visitor = A.default-map-visitor.{
  s-check-test(self, l, op, left, right):
    fun check-op(fieldname):
      A.s-app(l, A.s-dot(l, U.checkers(l), fieldname),
        [list: 
          A.s-str(l, A.s-check-test(l, op, left, right).tosource().pretty(80).join-str("\n")),
          left,
          right,
          A.build-loc(l)
        ])
    end
    if op == "opis": check-op("check-is")
    else if op == "opsatisfies": check-op("check-satisfies")
    else if op == "opraises":
      A.s-app(l, A.s-dot(l, U.checkers(l), "check-raises-str"),
        [list: 
          A.s-str(l, A.s-check-test(l, op, left, right).tosource().pretty(80).join-str("\n")),
          A.s-lam(l, [list: ], [list: ], A.a-blank, "", left, none),
          right,
          A.build-loc(l)
        ])
    else:
      raise("Check test operator " + op + " not yet implemented at " + torepr(l))
    end
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
        check-fun = lam(l2, [list: ], body)
        A.s-obj(l2, [list: 
            A.s-data-field(l2, A.s-str(l2, "name"), A.s-str(l2, name)),
            A.s-data-field(l2, A.s-str(l2, "run"), check-fun),
            A.s-data-field(l2, A.s-str(l2, "location"), A.build-loc(l2))
          ])
    end
  end
  checkers = checks.map(create-checker)
  A.s-block(l, [list: 
      A.s-app(l, A.s-dot(l, U.checkers(l), "run-checks"), [list: 
          A.s-str(l, l.source),
          A.s-list(l, checkers)
        ])
    ])
end

fun lam(l, args, body):
  A.s-lam(l, [list: ], args.map(fun(sym): A.s-bind(l, false, sym, A.a-blank) end), A.a-blank, "", body, none)
end

no-checks-visitor = A.default-map-visitor.{
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
  end,
  s-check(self, l, name, body, keyword-check):
    A.s-id(l, A.s-name(l, "nothing"))
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
