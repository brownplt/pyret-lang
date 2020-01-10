#lang pyret

provide *
provide-types *
import file("ast.arr") as A
import srcloc as SL
import lists as L
import file("gensym.arr") as G
import file("ast-util.arr") as U

data CheckInfo:
  | check-info(l :: SL.Srcloc, name :: String, body :: A.Expr, keyword-check :: Boolean)
end


fun ast-pretty(ast):
  A.s-str(ast.l, ast.tosource().pretty(80).join-str("\n"))
end

fun ast-lam(ast):
  A.s-lam(ast.l, "", [list: ], [list: ], A.a-blank, "", ast, none, none, true)
end

flat-prim-app = A.prim-app-info-c(false)
fun ast-srcloc(l):
  A.s-prim-app(l, "makeSrcloc", [list: A.s-srcloc(l, l)], flat-prim-app)
end

check-stmts-visitor = A.default-map-visitor.{
  method s-check-test(self, l, op, refinement, left, right, cause):
    fun check-op(fieldname):
      A.s-app(l, A.s-dot(l, U.checkers(l), fieldname),
        [list: ast-lam(left), ast-lam(right.value), ast-srcloc(l)])
    end
    fun check-op-cause(shadow cause, fieldname):
      A.s-app(l, A.s-dot(l, U.checkers(l), fieldname),
        [list: ast-lam(left), ast-lam(right.value), ast-lam(cause), ast-srcloc(l)])
    end
    fun check-refinement(shadow refinement, fieldname):
      A.s-app(l, A.s-dot(l, U.checkers(l), fieldname),
        [list: refinement, ast-lam(left), ast-lam(right.value), ast-srcloc(l)])
    end
    fun check-refinement-cause(shadow refinement, shadow cause, fieldname):
      A.s-app(l, A.s-dot(l, U.checkers(l), fieldname),
        [list: refinement, ast-lam(left), ast-lam(right.value), ast-lam(cause), ast-srcloc(l)])
    end
    fun check-raises(fieldname):
      A.s-app(l, A.s-dot(l, U.checkers(l), fieldname),
        [list: ast-lam(left), right.value, ast-srcloc(l)])
    end
    fun check-raises-not(fieldname):
      A.s-app(l, A.s-dot(l, U.checkers(l), fieldname),
        [list: ast-lam(left), ast-srcloc(l)])
    end
    fun check-raises-cause(shadow cause, fieldname):
      A.s-app(l, A.s-dot(l, U.checkers(l), fieldname),
        [list: ast-lam(left), right.value, ast-lam(cause), ast-srcloc(l)])
    end
    fun check-raises-not-cause(shadow cause, fieldname):
      A.s-app(l, A.s-dot(l, U.checkers(l), fieldname),
        [list: ast-lam(left), ast-lam(cause), ast-srcloc(l)])
    end
    cases(A.CheckOp) op:
      | s-op-is(_)                =>
        cases(Option) refinement:
          | none                    =>
            cases(Option) cause:
              | none                => check-op("check-is")
              | some(shadow cause)  => check-op-cause(cause, "check-is-cause")
            end
          | some(shadow refinement) =>
            cases(Option) cause:
              | none                => check-refinement(refinement, "check-is-refinement")
              | some(shadow cause)  => check-refinement-cause(refinement, cause, "check-is-refinement-cause")
            end
        end
      | s-op-is-roughly(_) =>
        cases(Option) cause:
          | none               => check-op("check-is-roughly")
          | some(shadow cause) => check-op-cause(cause, "check-is-roughly-cause")
        end
      | s-op-is-not(_)              =>
        cases(Option) refinement:
          | none                    =>
            cases(Option) cause:
              | none                => check-op("check-is-not")
              | some(shadow cause)  => check-op-cause(cause, "check-is-not-cause")
            end
          | some(shadow refinement) =>
            cases(Option) cause:
              | none                => check-refinement(refinement, "check-is-not-refinement")
              | some(shadow cause)  => check-refinement-cause(refinement, cause, "check-is-not-refinement-cause")
            end
        end
      | s-op-is-op(_, opname)     =>
        shadow refinement = A.s-id(l, A.s-name(l, A.get-op-fun-name(opname)))
        cases(Option) cause:
          | none                => check-refinement(refinement, "check-is-refinement")
          | some(shadow cause)  => check-refinement-cause(refinement, cause, "check-is-refinement-cause")
        end
      | s-op-is-not-op(_, opname) =>
        shadow refinement = A.s-id(l, A.s-name(l, A.get-op-fun-name(opname)))
        cases(Option) cause:
          | none               => check-refinement(refinement, "check-is-not-refinement")
          | some(shadow cause) => check-refinement-cause(refinement, cause, "check-is-not-refinement-cause")
        end
      | s-op-satisfies(_)         =>
        cases(Option) cause:
          | none                  => check-op("check-satisfies-delayed")
          | some(shadow cause)    => check-op-cause(cause, "check-satisfies-delayed-cause")
        end
      | s-op-satisfies-not(_)     =>
        cases(Option) cause:
          | none                  => check-op("check-satisfies-not-delayed")
          | some(shadow cause)    => check-op-cause(cause, "check-satisfies-not-delayed-cause")
        end
      | s-op-raises(_)            =>
        cases(Option) cause:
          | none                  => check-raises("check-raises-str")
          | some(shadow cause)    => check-raises-cause(cause, "check-raises-str-cause")
        end
      | s-op-raises-not(_)        =>
        cases(Option) cause:
          | none                  => check-raises-not("check-raises-not")
          | some(shadow cause)    => check-raises-not-cause(cause, "check-raises-not-cause")
        end
      | s-op-raises-other(_)      =>
        cases(Option) cause:
          | none                  => check-raises("check-raises-other-str")
          | some(shadow cause)    => check-raises-cause(cause, "check-raises-other-str-cause")
        end
      | s-op-raises-satisfies(_)  =>
        cases(Option) cause:
          | none                  => check-raises("check-raises-satisfies")
          | some(shadow cause)    => check-raises-cause(cause, "check-raises-satisfies-cause")
        end
      | s-op-raises-violates(_)   =>
        cases(Option) cause:
          | none                  => check-raises("check-raises-violates")
          | some(shadow cause)    => check-raises-cause(cause, "check-raises-violates-cause")
        end
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
              | some(v) => link(check-info(l, name, v.visit(check-stmts-visitor), true), add-check(rest))
              | none => add-check(rest)
            end
          | s-data(l, name, _, _, _, _, _, _check) =>
            cases(Option) _check:
              | some(v) => link(check-info(l, name, v.visit(check-stmts-visitor), true), add-check(rest))
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
            link(check-info(l, check-name, body.visit(check-stmts-visitor), keyword-check), add-check(rest))
          | else => add-check(rest)
        end
    end
  end
  add-check(stmts)
end

fun create-check-block(l, checks):
  fun create-checker(c):
    cases(CheckInfo) c:
      | check-info(l2, name, body, keyword-check) =>
        check-fun = make-lam(l2, [list: ], body)
        A.s-obj(l2, [list: 
            A.s-data-field(l2, "name", A.s-str(l2, name)),
            A.s-data-field(l2, "run", check-fun),
            A.s-data-field(l2, "keyword-check", A.s-bool(l2, keyword-check)),
            A.s-data-field(l2, "location", A.s-prim-app(l2, "makeSrcloc", [list: A.s-srcloc(l2, l2)], flat-prim-app))
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

no-checks-visitor = A.default-map-visitor.{
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

check-visitor = A.default-map-visitor.{
  method s-block(self, l, stmts):
    checks-to-perform = get-checks(stmts)
    ds-stmts = stmts.map(_.visit(self))
    do-checks = create-check-block(l, checks-to-perform)
    if is-empty(checks-to-perform): A.s-block(l, ds-stmts)
    else if is-empty(ds-stmts): raise("Empty block")
    else:
      id-result = A.s-name(l, G.make-name("$result-after-checks"))
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
