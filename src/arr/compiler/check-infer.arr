#lang pyret

provide *
provide-types *

import ast as A
import "compiler/type-structs.arr" as TS
import "compiler/type-check-structs.arr" as TCS
import "compiler/type-constraints.arr" as TC

type Type                 = TS.Type
t-name                    = TS.t-name
t-var                     = TS.t-var
t-arrow                   = TS.t-arrow
t-top                     = TS.t-top
t-bot                     = TS.t-bot
t-app                     = TS.t-app
t-record                  = TS.t-record
t-forall                  = TS.t-forall

type TypeVariable         = TS.TypeVariable
t-variable                = TS.t-variable

type TypeMember           = TS.TypeMember
t-member                  = TS.t-member

type DataType             = TS.DataType
t-datatype                = TS.t-datatype

t-number                  = TS.t-number
t-string                  = TS.t-string
t-boolean                 = TS.t-boolean

type TypeConstraint       = TC.TypeConstraint
type TypeConstraints      = TC.TypeConstraints
empty-type-constraints    = TC.empty-type-constraints
satisfies-type            = TC.satisfies-type
least-upper-bound         = TC.least-upper-bound
greatest-lower-bound      = TC.greatest-lower-bound
Equality                  = TC.Equality
Bounds                    = TC.Bounds

type FoldResult           = TCS.FoldResult
fold-result               = TCS.fold-result
fold-errors               = TCS.fold-errors

bind                      = TCS.bind

foldl2-result             = TCS.foldl2-result
foldr-result              = TCS.foldr-result
foldr2-result             = TCS.foldr2-result
map-result                = TCS.map-result
map2-result               = TCS.map2-result

var global-constraints = empty-type-constraints
info = TCS.empty-tc-info()

fun get-upper(tc :: TypeConstraint) -> Type:
  cases(TypeConstraint) tc:
    | Equality(typ)  => typ
    | Bounds(_, typ) => typ
  end
end

fun get-lower(tc :: TypeConstraint) -> Type:
  cases(TypeConstraint) tc:
    | Equality(typ)  => typ
    | Bounds(typ, _) => typ
  end
end

data ApplicationDescription:
  | app-desc(name :: String, args :: List<TypeConstraint>, ret :: TypeConstraint)
sharing:
  produce-constraints(self, ret-constraint :: TypeConstraint) -> FoldResult<TypeConstraints>:
    cases(Option<TypeConstraint>) self.ret.meet(ret-constraint):
      | some(ret) =>
        lower = t-arrow(self.args.map(get-upper), get-lower(ret))
        upper = t-arrow(self.args.map(get-lower), get-upper(ret))
        arrow-tc = Bounds(lower, upper)
        empty-type-constraints._insert(self.name, arrow-tc, info)
          ^ fold-result
      | none =>
        fold-errors([list: raise("place a CompileError here")])
    end
  end
end

fun process-check-left(left :: A.Expr, constraints :: TypeConstraints) -> FoldResult<Option<ApplicationDescription>>:
  cases(A.Expr) left:
    | s-app(_, _fun, args) =>
      cases(A.Expr) _fun:
        | s-id(_, id) =>
          for bind(arg-constraints from map-result(generate-constraints, args)):
            some(app-desc(id.key(), arg-constraints, Bounds(t-bot, t-top)))
              ^ fold-result
          end
        | else =>
          fold-result(none)
      end
    | else =>
      fold-result(none)
  end
end

fun process-check(_check :: A.Expr,
                  constraints :: TypeConstraints) -> TypeConstraints:
  cases(A.Expr) _check:
    | s-check-test(_, op, maybe-refinement, left, right) => 
      cases(Option<A.Expr>) maybe-refinement:
        | some(refinement) =>
          cases(A.CheckOp) op:
            | s-op-is            =>
              raise("s-op-is w/ refinement not yet handled")
            | s-op-is-not        =>
              raise("s-op-is-not w/ refinement not yet handled")
            | s-op-satisfies     =>
              raise("s-op-satisfies w/ refinement not yet handled")
            | s-op-satisfies-not =>
              raise("s-op-satisfies-not w/ refinement not yet handled")
            | s-op-raises        =>
              raise("s-op-raises w/ refinement not yet handled")
              # Gives no info
          end
        | none =>
          cases(A.CheckOp) op:
            | s-op-is            =>
              for bind(right-constraints from generate-constraints(right, constraints)):
                for bind(left-constraints from process-check-left(left, constraints)):
                  left-constraints
                    .produce-constraints(right-constraints)
                    ^ fold-result
                end
              end
            | s-op-is-not        =>
              raise("s-op-is-not not yet handled")
            | s-op-satisfies     =>
              raise("s-op-satisfies not yet handled")
            | s-op-satisfies-not =>
              raise("s-op-satisfies-not not yet handled")
            | s-op-raises        =>
              raise("s-op-raises not yet handled")
              # Gives no info
          end
      end
  end
end

fun generate-constraints(e :: A.Expr, constraints :: TypeConstraints) -> FoldResult<TypeConstraint>:
  cases(A.Expr) e:
    | s-lam(_, params, args, ann, doc, body, _) =>
      lower = t-arrow(args.map(lam(_): t-top end), t-bot)
      upper = t-arrow(args.map(lam(_): t-bot end), t-top)
      fold-result(Bounds(lower, upper))
    | s-data-expr(l, name, namet, params, mixins, variants, shared-members, _check) =>
      fold-result(Bounds(t-bot, t-top))
    | s-num(_, n)  =>
      fold-result(Equality(t-number))
    | s-bool(_, b) =>
      fold-result(Equality(t-boolean))
    | s-str(_, s)  =>
      fold-result(Equality(t-string))
    | s-id(_, id)  =>
      cases(Option<TypeConstraint>) constraints._get(id.key()):
        | some(tc) =>
          fold-result(tc)
        | none =>
          fold-errors([list: raise("Create a CompileError for me!")])
      end
    | else =>
      fold-result(Bounds(t-bot, t-top))
  end
end

fun process-bind(shadow constraints :: TypeConstraints,
                 binding :: { l :: A.Loc, b :: A.Bind, value :: A.Expr }
) -> FoldResult<TypeConstraints>:
  generate-constraints(binding.value, constraints)
    .map(constraints._insert(binding.b.id.key(), _, info))
end

check-infer-visitor = A.default-map-visitor.{
  s-data-expr(self, l, name, namet, params, mixins, variants, shared-members, _check):
    A.s-data(l, name, params, mixins, variants, shared-members, _check)
  end,
  s-let-expr(self, l, binds, body):
    for bind(c from foldr-result(process-bind, fold-result(global-constraints), binds)):
      global-constraints := c
      fold-result(c)
    end
    new-body = body.visit(self)
    A.s-let-expr(l, binds, new-body)
  end,
  s-letrec(self, l, binds, body):
    for bind(c from foldr-result(process-bind, fold-result(global-constraints), binds)):
      global-constraints := c
      fold-result(c)
    end
    new-body = body.visit(self)
    A.s-letrec(l, binds, new-body)
  end,
  s-check(self, l, name, body, keyword-check):
    A.s-check(self, l, name, body, keyword-check)
  end
}

fun check-infer(prog :: A.Program) -> TypeConstraints:
  doc: ```
        ```
  prog.visit(check-infer-visitor)
  global-constraints
end

