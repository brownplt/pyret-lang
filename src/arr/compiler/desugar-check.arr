#lang pyret

provide *
import ast as A
import srcloc as SL
import "./gensym.arr" as G
import "./ast-util.arr" as U

data CheckInfo:
  | check-info(l :: SL.Srcloc, name :: String, body :: A.Expr)
end


check-stmts-visitor = A.default-map-visitor.{
  s_check_test(self, l, op, left, right):
    fun check-op(fieldname):
      A.s_app(l, A.s_dot(l, U.checkers(l), fieldname),
        [
          A.s_str(l, A.s_check_test(l, op, left, right).tosource().pretty(80).join-str("\n")),
          left,
          right,
          A.build-loc(l)
        ])
    end
    if op == "opis": check-op("check-is")
    else if op == "opsatisfies": check-op("check-satisfies")
    else if op == "opraises":
      A.s_app(l, A.s_dot(l, U.checkers(l), "check-raises-str"),
        [
          A.s_str(l, A.s_check_test(l, op, left, right).tosource().pretty(80).join-str("\n")),
          A.s_lam(l, [], [], A.a_blank, "", left, none),
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
      | s_fun(l, name, _, _, _, _, _, _check) =>
        cases(Option) _check:
          | some(v) => link(check-info(l, name, v.visit(check-stmts-visitor)), lst)
          | none => lst
        end
      | s_data(l, name, _, _, _, _, _check) =>
        cases(Option) _check:
          | some(v) => link(check-info(l, name, v.visit(check-stmts-visitor)), lst)
          | none => lst
        end
     | s_check(l, name, body, keyword-check) =>
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
  stmts.foldr(add-check, [])
end

fun create-check-block(l, checks):
  fun create-checker(c):
    cases(CheckInfo) c:
      | check-info(l2, name, body) =>
        check-fun = lam(l2, [], body)
        A.s_obj(l2, [
            A.s_data_field(l2, A.s_str(l2, "name"), A.s_str(l2, name)),
            A.s_data_field(l2, A.s_str(l2, "run"), check-fun),
            A.s_data_field(l2, A.s_str(l2, "location"), A.build-loc(l2))
          ])
    end
  end
  checkers = checks.map(create-checker)
  A.s_block(l, [
      A.s_app(l, A.s_dot(l, U.checkers(l), "run-checks"), [
          A.s_str(l, l.source),
          A.s_list(l, checkers)
        ])
    ])
end

fun lam(l, args, body):
  A.s_lam(l, [], args.map(fun(sym): A.s-bind(l, false, sym, A.a_blank) end), A.a_blank, "", body, none)
end

no-checks-visitor = A.default-map-visitor.{
  s_check(self, l, name, body, keyword-check):
    A.s_id(l, A.s_name("nothing"))
  end
}

check-visitor = A.default-map-visitor.{
  s_block(self, l, stmts):
    checks-to-perform = get-checks(stmts)
    ds-stmts = stmts.map(_.visit(self))
    do-checks = create-check-block(l, checks-to-perform)
    if is-empty(checks-to-perform): A.s_block(l, ds-stmts)
    else if is-empty(ds-stmts): raise("Empty block")
    else:
      id-result = A.s_name(G.make-name("result-after-checks"))
      last-expr = ds-stmts.last()
      A.s_block(
          l,
          ds-stmts.take(ds-stmts.length() - 1) +
            [
              A.s_let(l, A.s_bind(l, false, id-result, A.a_blank), last-expr, false),
              do-checks,
              A.s_id(l, id-result)
            ]
        )
    end
  end,
  s_check(self, l, name, body, keyword-check):
    A.s_id(l, A.s_name("nothing"))
  end
}

fun desugar-check(prog):
  doc: "Desugars all check blocks to be calls to the current checker
        Preconditions on prog:
          - well-formed
        Postconditions on prog:
          - contains no s_check or s_check_test statements
          - all where blocks on s_lam, s_fun, s_data, s_method are none"
  prog.visit(check-visitor)
end

fun desugar-no-checks(prog):
  prog.visit(no-checks-visitor)
end
