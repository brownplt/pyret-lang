
data MyList:
  | l-empty
  | l-link(f, r)
end

fun build-list(n):
  if n < 1:
    l-empty
  else:
    l-link(n, build-list(n - 1))
  end
end

test-print(build-list(10000) == build-list(10000))
test-print(build-list(10001) == build-list(10000))


