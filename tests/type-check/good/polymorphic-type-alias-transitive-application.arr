type Id<T> = T
type Id2<T> = Id<T>

fun f<T>(x :: Id2<T>) -> T:
  x
end
