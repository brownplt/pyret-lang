#lang pyret
data MyList:
  | l-empty
  | l-link(f, r)
end

fun sum-list(l):
  cases(MyList) l:
    | l-empty => 0
    | l-link(f, r) => f + sum-list(r)
  end
end

fun build-list(n):
  if n < 1:
    l-empty
  else:
    l-link(n, build-list(n - 1))
  end
end

test-print(sum-list(build-list(10000)))
test-print(sum-list(build-list(20000)))

