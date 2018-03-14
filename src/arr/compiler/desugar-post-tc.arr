#lang pyret

provide *
provide-types *
import ast as A
import ast-visitors as AV
import file("desugar.arr") as D
import file("compile-structs.arr") as C

mk-id = D.mk-id
no-branches-exn = D.no-branches-exn

fun no-cases-exn(l, val):
  A.s-prim-app(l, "throwNoCasesMatched", [list: A.s-srcloc(l, l), val])
end

desugar-visitor = AV.default-map-visitor.{
  method s-template(self, l):
    A.s-prim-app(l, "throwUnfinishedTemplate", [list: A.s-srcloc(l, l)])
  end,
  method s-cases-else(self, l, typ, val, branches, els, blocky):
    name = A.global-names.make-atom("cases")
    typ-compiled = typ.visit(self)
    val-exp = val.visit(self)
    val-id = A.s-id(l, name)
    A.s-let-expr(l, [list: A.s-let-bind(l, A.s-bind(l, false, name, typ-compiled), val-exp)],
      A.s-cases-else(l, A.a-blank, val-id, branches.map(_.visit(self)),
        els.visit(self), true), false)
  end,
  method s-cases(self, l, typ, val, branches, blocky):
    name = A.global-names.make-atom("cases")
    typ-compiled = typ.visit(self)
    val-exp = val.visit(self)
    val-id = A.s-id(l, name)
    A.s-let-expr(l, [list: A.s-let-bind(l, A.s-bind(l, false, name, typ-compiled), val-exp)],
      A.s-cases-else(l, A.a-blank, val-id, branches.map(_.visit(self)),
        A.s-block(l, [list: no-cases-exn(l, val-id)]), true), false)
  end,
  method s-check(self, l, name, body, keyword-check):
    A.s-id(l, A.s-global("nothing"))
  end
}

fun desugar-post-tc(program :: A.Program, compile-env :: C.CompileEnvironment):
  doc: ```
        Desugar non-scope and non-check based constructs.
        Preconditions on program:
          - well-formed
          - has been type-checked
          - contains no s-var, s-fun, s-data, s-check, or s-check-test
          - contains no s-provide in headers
          - all where blocks are none
          - contains no s-name (e.g. call resolve-names first)
          - contains no s-for, s-if, s-op, s-method-field,
                        s-not, s-when, s-if-pipe, s-paren
          - contains no s-underscore in expression position (but it may
            appear in binding positions as in s-let-bind, s-letrec-bind)
        Postconditions on program:
          - in addition to preconditions,
            contains no s-cases, s-cases-else, s-instantiate
        ```
  cases(A.Program) program:
    | s-program(l, _provide, provided-types, imports, body) =>
      A.s-program(l, _provide, provided-types, imports, body.visit(desugar-visitor))
    | else => raise("Attempt to desugar non-program: " + torepr(program))
  end
end
