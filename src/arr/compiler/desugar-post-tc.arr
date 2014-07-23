#lang pyret

provide *
provide-types *
import ast as A
import "compiler/desugar.arr" as D
import "compiler/compile-structs.arr" as C

mk-id = D.mk-id
no-branches-exn = D.no-branches-exn

desugar-post-tc =
letrec desugar-visitor = A.default-map-visitor.{
         s-cases(self, l, typ, val, branches):
           desugar-cases(l, typ, val.visit(self), branches.map(desugar-case-branch),
             A.s-block(l, [list: no-branches-exn(l, "cases")]))
         end,
         s-cases-else(self, l, typ, val, branches, _else):
           desugar-cases(l, typ, val.visit(self), branches.map(desugar-case-branch), _else.visit(self))
         end,
         s-instantiate(self, l, body, args):
           body.visit(self)
         end
       },
       desugar-case-branch = lam(c):
         cases(A.CasesBranch) c:
           | s-cases-branch(l, name, args, body) =>  
             A.s-data-field(l, name,
               A.s-lam(l, [list: ], args, A.a-blank, "", body.visit(desugar-visitor), none))
         end
       end,
       desugar-cases = lam(l, ann, val, branches, else-block):
         val-id = mk-id(l, "cases-val")
         cases-object = A.s-obj(l, branches)
         else-thunk = A.s-lam(l, [list: ], [list: ], A.a-blank, "", else-block, none)
         A.s-let-expr(l, [list: 
               A.s-let-bind(l, val-id.id-b, val)
             ],
             A.s-app(l, A.s-dot(l, val-id.id-e, "_match"), [list: cases-object, else-thunk])
           )
       end:
         lam (program :: A.Program, compile-env :: C.CompileEnvironment):
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
end
