data MyList<A>:
  | my-empty 
  | my-link(first :: A, rest :: MyList<A>)
end

fun test-map<A,B>(lst :: MyList<A>, f :: (A -> B)):
  cases(MyList<A>) lst:
    | my-empty =>
      my-empty
    | my-link(first, rest) =>
      my-link(f(first), test-map(rest, f))
  end
end

input = my-link(5, my-link(6, my-empty))

fun transform(n :: Number) -> String:
  "hello"
end

result :: MyList<String> = test-map(input, transform)
