#lang pyret

provide *
provide-types *

fun<I> identity(i :: I) -> I: i end

data Either<L, R>:
  | left(v :: L) with:
    bi-map(self, f :: (L -> Any), _ :: (R -> Any)) -> Either<Any, Any>:
      left(f(self.v))
    end,
  | right(v :: R) with:
    bi-map(self, _ :: (L -> Any), g :: (R -> Any)) -> Either<Any, Any>:
      right(g(self.v))
    end,
end

fun<A, B, R> map-left(f :: (A -> B), e :: Either<A, R>) -> Either<B, R>:
   e.bi-map(f, identity)
end

fun<A, B, L> map-right(f :: (A -> B), e :: Either<L, A>) -> Either<L, B>:
   e.bi-map(identity, f)
end
