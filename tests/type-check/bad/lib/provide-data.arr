provide:
  data D
end

data D:
  | d1 with:
    method only-defined-on-d1(self): "method called" end
  | d2(x :: Number) with:
    method only-defined-on-d2(self): "method called" end
end
