provide:
  data D
end

data D:
  | d1 with:
    method method-name(self): "method called" end
  | d2(x :: Number) with:
    method another-method-name(self): "method called" end
end
