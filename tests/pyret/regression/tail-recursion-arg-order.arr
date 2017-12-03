# https://github.com/brownplt/pyret-lang/issues/1230

fun iter(a, b, k):
  if k == 0:
    b
  else:
    iter(a + b, a, k - 1)
  end
where:
  iter(1, 0, 10) is 55
end

fun oops(a, b):
  if a == 1: b else: oops(1, a) end
end
fun working(a, b):
  if a == 1: b else: working(1, a) + 0 end
end

examples:
  oops(4, 5) is working(4, 5)
end

# When an argument is wrapped in a lambda to delay evaluation, mutation
# in the tail recursion optimization will be unsound. Consider:

fun oops2(a, b):
  foo = lam(): a end
  if a == 1: b() else: oops2(1, foo) end
end

# Note that
#
# b := lam(): a end
# a := 1
#
# will make the second iteration eveluates to the mutated `a` which is wrong
# An easy way to fix this is to simply disable TCO in this case

fun working2(a, b):
  foo = lam(): a end
  if a == 1: b() else: working2(1, foo) + 0 end
end

examples:
  oops2(4, 5) is working2(4, 5)
end
