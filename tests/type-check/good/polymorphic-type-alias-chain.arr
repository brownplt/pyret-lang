type A<T> = T
type B<T> = A<T>
type C<T> = B<T>

fun test<T>(x :: C<T>) -> T:
  x
end
