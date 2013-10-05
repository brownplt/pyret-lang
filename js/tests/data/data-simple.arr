data D:
  | some-variant(a,b,c)
end
d = some-variant(1,2,3)
{a: d.a, b: d.b, c: d.c}
