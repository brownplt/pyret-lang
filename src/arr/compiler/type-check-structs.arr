provide *
provide-types *

import ast as A
import string-dict as SD
import "compiler/type-structs.arr" as TS
import "compiler/compile-structs.arr" as C

type Name                 = A.Name

type Type                 = TS.Type
t-name                    = TS.t-name
t-var                     = TS.t-var
t-arrow                   = TS.t-arrow
t-top                     = TS.t-top
t-bot                     = TS.t-bot
t-app                     = TS.t-app
t-record                  = TS.t-record
t-forall                  = TS.t-forall

t-number                  = TS.t-number
t-string                  = TS.t-string
t-boolean                 = TS.t-boolean
t-srcloc                  = TS.t-srcloc

type Variance             = TS.Variance
constant                  = TS.constant
invariant                 = TS.invariant
covariant                 = TS.covariant
contravariant             = TS.contravariant

type TypeVariable         = TS.TypeVariable
t-variable                = TS.t-variable

type DataType              = TS.DataType
type Bindings              = SD.StringDict<TS.Type>
empty-bindings :: Bindings = SD.immutable-string-dict()


data TCInfo:
  | tc-info(typs       :: SD.StringDict<TS.Type>,
            aliases    :: SD.StringDict<TS.Type>,
            data-exprs :: SD.StringDict<TS.DataType>,
            branders   :: SD.StringDict<TS.Type>,
            binds      :: Bindings,
            errors     :: { insert :: (C.CompileError -> List<C.CompileError>),
                            get    :: (-> List<C.CompileError>)})
end

t-number-binop = t-arrow([list: t-number, t-number], t-number)

default-typs = SD.string-dict()
default-typs.set(A.s-global("nothing").key(), t-name(none, A.s-type-global("Nothing")))
default-typs.set("isBoolean", t-arrow([list: t-top], t-boolean))
default-typs.set(A.s-global("torepr").key(), t-arrow([list: t-top], t-string))
default-typs.set("throwNonBooleanCondition", t-arrow([list: t-srcloc, t-string, t-top], t-bot))
default-typs.set("throwNoBranchesMatched", t-arrow([list: t-srcloc, t-string], t-bot))
default-typs.set(A.s-global("raise").key(), t-arrow([list: t-top], t-bot))
default-typs.set(A.s-global("equal-always").key(), t-arrow([list: t-top, t-top], t-boolean))
default-typs.set(A.s-global("equal-now").key(), t-arrow([list: t-top, t-top], t-boolean))
default-typs.set(A.s-global("identical").key(), t-arrow([list: t-top, t-top], t-boolean))
default-typs.set("hasField", t-arrow([list: t-record(empty), t-string], t-boolean))
default-typs.set(A.s-global("tostring").key(), t-arrow([list: t-top], t-string))
default-typs.set(A.s-global("_times").key(), t-number-binop)
default-typs.set(A.s-global("_minus").key(), t-number-binop)
default-typs.set(A.s-global("_divide").key(), t-number-binop)
default-typs.set(A.s-global("_plus").key(), t-number-binop)
default-typs.set(A.s-global("_lessthan").key(), t-number-binop)
default-typs.set(A.s-global("_lessequal").key(), t-number-binop)
default-typs.set(A.s-global("_greaterthan").key(), t-number-binop)
default-typs.set(A.s-global("_greaterequal").key(), t-number-binop)
print-variable = A.s-atom(gensym("A"), 1)
default-typs.set(A.s-global("print").key(), t-forall([list: t-variable(A.dummy-loc, print-variable, t-top, invariant)], t-arrow([list: t-var(print-variable)], t-var(print-variable))))

fun empty-tc-info() -> TCInfo:
  errors = block:
    var err-list = empty
    {
      insert: lam(err :: C.CompileError):
        err-list := link(err, err-list)
      end,
      get: lam():
        err-list
      end
    }
  end
  tc-info(default-typs, SD.string-dict(), SD.string-dict(), SD.string-dict(), empty-bindings, errors)
end

fun add-binding-string(id :: String, bound :: Type, info :: TCInfo) -> TCInfo:
  new-binds = info.binds.set(id, bound)
  tc-info(info.typs, info.aliases, info.data-exprs, info.branders, new-binds, info.errors)
end

fun add-binding(id :: Name, bound :: Type, info :: TCInfo) -> TCInfo:
  add-binding-string(id.key(), bound, info)
end

fun add-type-variable(tv :: TS.TypeVariable, info :: TCInfo) -> TCInfo:
  add-binding(tv.id, tv.upper-bound, info)
end

fun get-data-type(typ :: Type, info :: TCInfo) -> Option<DataType>:
  cases(Type) typ:
    | t-name(module-name, name) =>
      key = typ.key()
      if info.data-exprs.has-key(key):
        some(info.data-exprs.get(key))
      else:
        none
      end
    | t-app(base-typ, args) =>
      key = base-typ.key()
      if info.data-exprs.has-key(key):
        data-type = info.data-exprs.get(key).introduce(args)
        cases(Option<DataType>) data-type:
          | some(dt) => data-type
          | none => raise("Internal type-checking error: This shouldn't happen, since the length of type arguments should have already been compared against the length of parameters")
        end
      else:
        none
      end
    | else =>
      none
  end
end

fun bind(f, a): a.bind(f);
fun map-bind(f, a): a.map-bind(f);
fun check-bind(f, a): a.check-bind(f);
fun synth-bind(f, a): a.synth-bind(f);
fun fold-bind(f, a): a.fold-bind(f);

data SynthesisResult:
  | synthesis-result(ast :: A.Expr, loc :: A.Loc, typ :: Type) with:
    bind(self, f) -> SynthesisResult:
      f(self.ast, self.loc, self.typ)
    end,
    map-expr(self, f) -> SynthesisResult:
      synthesis-result(f(self.ast), self.loc, self.typ)
    end,
    map-typ(self, f) -> SynthesisResult:
      synthesis-result(self.ast, self.loc, f(self.typ))
    end,
    synth-bind(self, f) -> SynthesisResult:
      f(self.ast, self.loc, self.typ)
    end,
    check-bind(self, f) -> CheckingResult:
      f(self.ast, self.loc, self.typ)
    end,
    fold-bind(self, f) -> FoldResult:
      f(self.ast, self.loc, self.typ)
    end
  | synthesis-binding-result(let-bind, typ :: Type) with:
    bind(self, f) -> SynthesisResult:
      f(self.let-bind, self.typ)
    end,
    map-expr(self, f) -> SynthesisResult:
      raise("Cannot map expr on synthesis-binding-result!")
    end,
    map-typ(self, f) -> SynthesisResult:
      synthesis-binding-result(self.let-bind, f(self.typ))
    end,
    check-bind(self, f) -> CheckingResult:
      f(self.let-bind, self.typ)
    end
  | synthesis-err(errors :: List<C.CompileError>) with:
    bind(self, f) -> SynthesisResult:
      self
    end,
    map-expr(self, f) -> SynthesisResult:
      self
    end,
    map-typ(self, f) -> SynthesisResult:
      self
    end,
    synth-bind(self, f) -> SynthesisResult:
      self
    end,
    check-bind(self, f) -> CheckingResult:
      checking-err(self.errors)
    end,
    fold-bind(self, f) -> FoldResult:
      fold-errors(self.errors)
    end
end

fun <B> map-synthesis(f :: (B -> SynthesisResult), lst :: List<B>) -> FoldResult<List>:
  cases(List<A>) lst:
    | link(first, rest) =>
      cases(SynthesisResult) f(first):
        | synthesis-result(ast, loc, typ) =>
          map-synthesis(f, rest).bind(lam(asts): fold-result(link(ast, asts));)
        | synthesis-binding-result(binding, typ) =>
          map-synthesis(f, rest).bind(lam(asts): fold-result(link(binding, asts));)
        | synthesis-err(errors) =>
          fold-errors(errors)
      end
    | empty =>
      fold-result(empty)
  end
end


data CheckingResult:
  | checking-result(ast :: A.Expr) with:
    bind(self, f) -> CheckingResult:
      f(self.ast)
    end,
    map(self, f) -> CheckingResult:
      checking-result(f(self.ast))
    end,
    check-bind(self, f) -> CheckingResult:
      f(self.ast)
    end,
    synth-bind(self, f) -> SynthesisResult:
      f(self.ast)
    end,
    fold-bind(self, f) -> FoldResult:
      f(self.ast)
    end,
    map-bind(self, f) -> CheckingMapResult:
      f(self.ast)
    end
  | checking-err(errors :: List<C.CompileError>) with:
    bind(self, f) -> CheckingResult:
      self
    end,
    check-bind(self, f) -> CheckingResult:
      self
    end,
    map(self, f) -> CheckingResult:
      self
    end,
    synth-bind(self, f) -> SynthesisResult:
      synthesis-err(self.errors)
    end,
    fold-bind(self, f) -> FoldResult:
      fold-errors(self.errors)
    end,
    map-bind(self, f) -> CheckingMapResult:
      checking-map-errors(self.errors)
    end
end

data FoldResult<V>:
  | fold-result(v :: V) with:
    tostring(self, shadow tostring):
      "fold-result(" + tostring(self.v) + ")"
    end,
    bind(self, f) -> FoldResult<V>:
      f(self.v)
    end,
    map(self, f) -> FoldResult:
      fold-result(f(self.v))
    end,
    check-bind(self, f) -> CheckingResult:
      f(self.v)
    end,
    synth-bind(self, f) -> SynthesisResult:
      f(self.v)
    end
  | fold-errors(errors :: List<C.CompileError>) with:
    tostring(self, shadow tostring):
      "fold-errors(" + tostring(self.errors) + ")"
    end,
    bind(self, f) -> FoldResult<V>:
      self
    end,
    map(self, f) -> FoldResult:
      self
    end,
    check-bind(self, f) -> CheckingResult:
      checking-err(self.errors)
    end,
    synth-bind(self, f) -> SynthesisResult:
      synthesis-err(self.errors)
    end
end

fun foldl2-result(not-equal :: C.CompileError):
  fun <E,B,D> helper(f :: (E, B, D -> FoldResult<E>), base :: FoldResult<E>, lst-1 :: List<B>, lst-2 :: List<D>) -> FoldResult<E>:
    cases(List<B>) lst-1:
      | link(first-1, rest-1) =>
        cases(List<D>) lst-2:
          | link(first-2, rest-2) =>
            for bind(v from base):
              new-base = f(v, first-1, first-2)
              helper(f, new-base, rest-1, rest-2)
            end
          | empty =>
            fold-errors([list: not-equal])
        end
      | empty =>
        cases(List<D>) lst-2:
          | link(_, _) =>
            fold-errors([list: not-equal])
          | empty =>
            base
        end
    end
  end
  helper
end

fun foldl3-result(not-equal :: C.CompileError):
  fun <E,B,F,D> helper(f :: (E, B, F, D -> FoldResult<E>), base :: FoldResult<E>, lst-1 :: List<B>, lst-2 :: List<F>, lst-3 :: List<D>) -> FoldResult<E>:
    cases(List<B>) lst-1:
      | link(first-1, rest-1) =>
        cases(List<F>) lst-2:
          | link(first-2, rest-2) =>
            cases(List<D>) lst-3:
              | link(first-3, rest-3) =>
                for bind(v from base):
                  new-base = f(v, first-1, first-2, first-3)
                  helper(f, new-base, rest-1, rest-2, rest-3)
                end
              | empty =>
                fold-errors([list: not-equal])
            end
          | empty =>
            fold-errors([list: not-equal])
        end
      | empty =>
        cases(List<F>) lst-2:
          | link(_, _) =>
            fold-errors([list: not-equal])
          | empty =>
            cases(List<D>) lst-3:
              | link(_, _) =>
                fold-errors([list: not-equal])
              | empty =>
                base
            end
        end
    end
  end
  helper
end

fun foldr2-result(not-equal :: C.CompileError):
  fun <E,B,D> helper(f :: (E, B, D -> FoldResult<E>), base :: FoldResult<E>, lst-1 :: List<B>, lst-2 :: List<D>) -> FoldResult<E>:
    cases(List<B>) lst-1:
      | link(first-1, rest-1) =>
        cases(List<D>) lst-2:
          | link(first-2, rest-2) =>
            for bind(result from helper(f, base, rest-1, rest-2)):
              f(result, first-1, first-2)
            end
          | empty =>
            fold-errors([list: not-equal])
        end
      | empty =>
        cases(List<D>) lst-2:
          | link(_, _) =>
            fold-errors([list: not-equal])
          | empty =>
            base
        end
    end
  end
  helper
end


fun map2-result(not-equal :: C.CompileError):
  fun <E,B,D> helper(f :: (B, D -> FoldResult<E>), lst-1 :: List<B>, lst-2 :: List<D>) -> FoldResult<List<E>>:
    fun process-and-prepend(lst :: List<E>, b :: B, d :: D) -> FoldResult<List<E>>:
      for bind(result from f(b, d)):
        fold-result(link(result, lst))
      end
    end
    foldr2-result(not-equal)(process-and-prepend, fold-result(empty), lst-1, lst-2)
  end
  helper
end

fun <E,B,D> foldl-result(f :: (E, B -> FoldResult<E>), base :: FoldResult<E>, lst-1 :: List<B>) -> FoldResult<E>:
  cases(List<B>) lst-1:
    | link(first, rest) =>
      for bind(v from base):
        result = f(v, first)
        foldr-result(f, result, rest)
      end
    | empty =>
      base
  end
end

fun <E,B,D> foldr-result(f :: (E, B -> FoldResult<E>), base :: FoldResult<E>, lst-1 :: List<B>) -> FoldResult<E>:
  cases(List<B>) lst-1:
    | link(first, rest) =>
      for bind(v from foldr-result(f, base, rest)):
        f(v, first)
      end
    | empty =>
      base
  end
end




data CheckingMapResult:
  | checking-map(lst :: List<A.Expr>) with:
    bind(self, f) -> CheckingMapResult:
      f(self.lst)
    end,
    check-bind(self, f) -> CheckingResult:
      f(self.lst)
    end,
    synth-bind(self, f) -> SynthesisResult:
      f(self.lst)
    end,
    prepend(self, ast :: A.Expr) -> CheckingMapResult:
      checking-map(link(ast, self.lst))
    end
  | checking-map-errors(errors :: List<C.CompileError>) with:
    bind(self, f) -> CheckingMapResult:
      self
    end,
    check-bind(self, f) -> CheckingResult:
      checking-err(self.errors)
    end,
    synth-bind(self, f) -> SynthesisResult:
      synthesis-err(self.errors)
    end,
    prepend(self, ast :: A.Expr) -> CheckingMapResult:
      self
    end
end


fun map2-checking(not-equal :: C.CompileError):
  fun <B,D> helper(f :: (B, D -> CheckingResult), lst-1 :: List<B>, lst-2 :: List<D>) -> CheckingMapResult:
    cases(List<B>) lst-1:
      | link(first-1, rest-1) =>
        cases(List<D>) lst-2:
          | link(first-2, rest-2) =>
            cases(CheckingResult) f(first-1, first-2):
              | checking-result(ast) =>
                helper(f, rest-1, rest-2)
                  .bind(lam(asts): checking-map(link(ast, asts));)
              | checking-err(errors) =>
                checking-map-errors(errors)
            end
          | empty =>
            checking-map-errors([list: not-equal])
        end
      | empty =>
        cases(List<D>) lst-2:
          | link(_, _) =>
            checking-map-errors([list: not-equal])
          | empty =>
            checking-map(empty)
        end
    end
  end
  helper
end

fun <B> map-checking(f :: (B -> CheckingResult), lst :: List<B>) -> CheckingMapResult:
  cases(List<B>) lst:
    | link(first, rest) =>
      cases(CheckingResult) f(first):
        | checking-result(ast) =>
          map-checking(f, rest)
            .bind(lam(asts): checking-map(link(ast, asts));)
        | checking-err(errors) =>
          checking-map-errors(errors)
      end
    | empty =>
      checking-map(empty)
  end
end




fun <B,D> map-result(f :: (B -> FoldResult<D>), lst :: List<B>) -> FoldResult<List<D>>:
  cases(List<B>) lst:
    | link(first, rest) =>
      cases(FoldResult<D>) f(first):
        | fold-result(d) =>
          map-result(f, rest).bind(lam(ds): fold-result(link(d, ds));)
        | fold-errors(errors) =>
          fold-errors(errors)
      end
    | empty =>
      fold-result(empty)
  end
end
