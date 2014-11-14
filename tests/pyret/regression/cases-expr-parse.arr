check "https://github.com/brownplt/pyret-lang/issues/370":
  res = cases(List) [list: 1] + [list: 2]:
    | empty => false
    | link(f, r) => 1
  end
  res is 1
end
