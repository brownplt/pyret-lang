#lang pyret

data D:
  | var1(a, b)
  | var2(a, b, c)
  | var3()
end

fun matcher(v):
  cases(D) v:
    | var1(a, b) => a + b
    | var2(a, b, c) => a * b * c
    | var3() => 42
  end
end

total-sum = for list.fold(sum from 0, i from list.range(0, 10000)):
  choice = i.modulo(3)
  if choice == 0:
    sum + matcher(var1(1, 2))
  else if choice == 1:
    sum + matcher(var2(1, 2, 3))
  else:
    sum + matcher(var3())
  end
end

print(total-sum)
