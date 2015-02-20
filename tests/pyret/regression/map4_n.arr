check "https://github.com/brownplt/pyret-lang/issues/403":
  result = for map4_n(n from 0,
             a from [list: 1],
             b from [list: 2], 
             c from [list: 3],
             d from [list: 4]):
    a + b + c + d
  end
  result is [list: 10]
end
