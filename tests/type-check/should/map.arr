
data MyList<A>:
  | my-empty 
  | my-link(first :: A, rest :: MyList<A>)
end

fun <A,B> map(lst :: MyList<A>, f :: (A -> B)):
  cases(MyList<A>) lst:
    | my-empty =>
      my-empty
    | my-link(first, rest) =>
      my-link(f(first), map(rest, f))
  end
end

input = my-link(5, my-link(6, my-empty))

fun transform(n :: Number) -> String:
  "hello"
end

result :: MyList<String> = map(input, transform)
