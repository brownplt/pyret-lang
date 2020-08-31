include cpo

check "cpo-list":
  link(5, link(100, empty)) is [list: 5, 100]
  empty is empty
end


