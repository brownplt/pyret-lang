provide *
provide-types *

import option as O
import either as E
import error  as Err

a = O.some(5)

b :: Number = cases(O.Option<Number>) a:
  | some(n) => n
  | none => 0
end

c = E.left(5)
d = E.right("hello world")

fun wants-either(v :: E.Either<Number,String>):
  E.is-left(v)
end

t :: Boolean = wants-either(c)
f :: Boolean = wants-either(d)

my-s :: String = cases(E.Either<Number,String>) c:
  | left(n) =>
    tostring(n)
  | right(s) =>
    s
end


my-err :: Err.RuntimeError = Err.numeric-binop-error(5, "hello", "+", "_plus")

my-str :: String = cases(Err.RuntimeError) my-err:
  | numeric-binop-error(_, _, _, _) =>
    "hello"
  | else =>
    "world"
end
