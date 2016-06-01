import either as E

left = E.left
right = E.right
type Either = E.Either

fun f<A>(x :: List<A>) -> List<A>:
  x
end

type MyList<A> = List<A>
type MyMonoList = List<Number>

x :: MyList<Number> = [list: 1, 2, 3]
y :: MyMonoList = [list: 1, 2, 3]

type MyEither = Either
type MyEither2<A, B> = Either<A, B>

x2 :: MyEither<String, Number> = left("a")
y2 :: MyEither2<String, Number> = right(1)
