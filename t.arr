type N = Number%<m>

fun id(n :: N%(num-is-integer)): n end
id(1%<m>)

print(within(10)(~1/2%<m>, ~1/2%<m>))
