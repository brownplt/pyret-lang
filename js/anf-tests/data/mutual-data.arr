
data D1:
  | v1 with:
    m(self): is-v2(self) end
end

data D2:
  | v2 with:
    m(self): is-v1(self) end
end

test-print(v1.m())
test-print(v2.m())

