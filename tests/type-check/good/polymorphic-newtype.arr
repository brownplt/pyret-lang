fun f<A>(x :: List<A>) -> List<A>:
  x
end

type MyList<A> = List<A>
type MyMonoList = List<Number>

x :: MyList<Number> = [list: 1, 2, 3]
y :: MyMonoList = [list: 1, 2, 3]
