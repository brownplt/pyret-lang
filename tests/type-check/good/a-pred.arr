
fun is-five(n :: Number):
  n == 5
end

fun test(n :: Number % (is-five)):
  true
end
