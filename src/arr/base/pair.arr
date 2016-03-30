#lang pyret

provide *
provide-types *

fun <I> identity(i :: I) -> I: i end

data Pair<L,R>:
  | pair(left :: L, right :: R)
sharing:
  bi-map(self, f :: (L -> Any), g :: (R -> Any)) -> Pair<Any, Any>:
    pair(f(self.left), g(self.right))
  end,
end

fun<A, B, R> map-left(f :: (A -> B), p :: Pair<A, R>) -> Pair<B, R>:
   p.bi-map(f, identity)
end

fun<A, B, L> map-right(f :: (A -> B), p :: Pair<L, A>) -> Pair<L, B>:
   p.bi-map(identity, f)
end
