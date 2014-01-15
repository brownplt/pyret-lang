data MyList:
  | l-empty
  | l-link(f, r)
end

test-print(is-l-empty(l-empty))
test-print(is-l-link(l-link(1, 2)))
test-print(l-link(1,2).r)
test-print(l-link(1,2).f)

