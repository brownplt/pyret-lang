provide *
provide-types *

import ast as A
import string-dict as SD
import "compiler/type-structs.arr" as TS
import "compiler/compile-structs.arr" as C

type Type                  = TS.Type
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

fun add-binding(info :: TCInfo, id :: String, bound :: Type) -> TCInfo:
  new-binds = info.binds.set(id, bound)
  tc-info(info.typs, info.aliases, info.data-exprs, info.branders, new-binds, info.errors)
end

fun add-type-variable(info :: TCInfo, tv :: TS.TypeVariable) -> TCInfo:
  add-binding(info, tv.id, tv.upper-bound)
end

fun bind(f, a): a.bind(f);
fun map-bind(f, a): a.map-bind(f);
fun check-bind(f, a): a.check-bind(f);
fun synth-bind(f, a): a.synth-bind(f);
fun fold-bind(f, a): a.fold-bind(f);

data SynthesisResult:
  | synthesis-result(ast :: A.Expr, typ :: Type) with:
    bind(self, f) -> SynthesisResult:
      a = f(self.ast, self.typ)
      a
    end,
    map-expr(self, f) -> SynthesisResult:
      a = synthesis-result(f(self.ast), self.typ)
      a
    end,
    map-typ(self, f) -> SynthesisResult:
      a = synthesis-result(self.ast, f(self.typ))
      a
    end,
    synth-bind(self, f) -> SynthesisResult:
      a = f(self.ast, self.typ)
      a
    end,
    check-bind(self, f) -> CheckingResult:
      a = f(self.ast, self.typ)
      a
    end,
    fold-bind(self, f) -> FoldResult:
      a = f(self.ast, self.typ)
      a
    end
  | synthesis-binding-result(let-bind, typ :: Type) with:
    bind(self, f) -> SynthesisResult:
      a = f(self.let-bind, self.typ)
      a
    end,
    map-expr(self, f) -> SynthesisResult:
      raise("Cannot map expr on synthesis-binding-result!")
    end,
    map-typ(self, f) -> SynthesisResult:
      a = synthesis-binding-result(self.let-bind, f(self.typ))
      a
    end,
    check-bind(self, f) -> CheckingResult:
      a = f(self.let-bind, self.typ)
      a
    end
  | synthesis-err(errors :: List<C.CompileError>) with:
    bind(self, f) -> SynthesisResult:
      a = self
      a
    end,
    map-expr(self, f) -> SynthesisResult:
      a = self
      a
    end,
    map-typ(self, f) -> SynthesisResult:
      a = self
      a
    end,
    synth-bind(self, f) -> SynthesisResult:
      a = self
      a
    end,
    check-bind(self, f) -> CheckingResult:
      a = checking-err(self.errors)
      a
    end,
    fold-bind(self, f) -> FoldResult:
      a = fold-errors(self.errors)
      a
    end
end

fun <B> map-synthesis(f :: (B -> SynthesisResult), lst :: List<B>) -> FoldResult<List>:
  cases(List<A>) lst:
    | link(first, rest) =>
      cases(SynthesisResult) f(first):
        | synthesis-result(ast, typ) =>
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
      a = f(self.ast)
      a
    end,
    map(self, f) -> CheckingResult:
      a = checking-result(f(self.ast))
      a
    end,
    check-bind(self, f) -> CheckingResult:
      a = f(self.ast)
      a
    end,
    synth-bind(self, f) -> SynthesisResult:
      a = f(self.ast)
      a
    end,
    fold-bind(self, f) -> FoldResult:
      a = f(self.ast)
      a
    end,
    map-bind(self, f) -> CheckingMapResult:
      a = f(self.ast)
      a
    end
  | checking-err(errors :: List<C.CompileError>) with:
    bind(self, f) -> CheckingResult:
      a = self
      a
    end,
    check-bind(self, f) -> CheckingResult:
      a = self
      a
    end,
    map(self, f) -> CheckingResult:
      a = self
      a
    end,
    synth-bind(self, f) -> SynthesisResult:
      a = synthesis-err(self.errors)
      a
    end,
    fold-bind(self, f) -> FoldResult:
      a = fold-errors(self.errors)
      a
    end,
    map-bind(self, f) -> CheckingMapResult:
      a = checking-map-errors(self.errors)
      a
    end
end

data FoldResult<V>:
  | fold-result(v :: V) with:
    bind(self, f) -> FoldResult<V>:
      a = f(self.v)
      a
    end,
    map(self, f) -> FoldResult:
      a = fold-result(f(self.v))
      a
    end,
    check-bind(self, f) -> CheckingResult:
      a = f(self.v)
      a
    end,
    synth-bind(self, f) -> SynthesisResult:
      a = f(self.v)
      a
    end
  | fold-errors(errors :: List<C.CompileError>) with:
    bind(self, f) -> FoldResult<V>:
      a = self
      a
    end,
    map(self, f) -> FoldResult:
      a = self
      a
    end,
    check-bind(self, f) -> CheckingResult:
      a = checking-err(self.errors)
      a
    end,
    synth-bind(self, f) -> SynthesisResult:
      a = synthesis-err(self.errors)
      a
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
      a = f(self.lst)
      a
    end,
    check-bind(self, f) -> CheckingResult:
      a = f(self.lst)
      a
    end,
    synth-bind(self, f) -> SynthesisResult:
      a = f(self.lst)
      a
    end,
    prepend(self, ast :: A.Expr) -> CheckingMapResult:
      a = checking-map(link(ast, self.lst))
      a
    end
  | checking-map-errors(errors :: List<C.CompileError>) with:
    bind(self, f) -> CheckingMapResult:
      a = self
      a
    end,
    check-bind(self, f) -> CheckingResult:
      a = checking-err(self.errors)
      a
    end,
    synth-bind(self, f) -> SynthesisResult:
      a = synthesis-err(self.errors)
      a
    end,
    prepend(self, ast :: A.Expr) -> CheckingMapResult:
      a = self
      a
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
