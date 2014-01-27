fun f():
  is-v2(v1)
  v2.m()
end

data D1:
  | v1
end

data D2:
  | v2 with:
    m(self): is-v1(self) end
end

test-print(f())
test-print(v2.m())
