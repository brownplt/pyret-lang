data Buck:
  | mt
  | lnk(f,r)
end

fun fld(f, b, l):
  if is-mt(l):
    b
  else:
    fld(f, f(b, l.f), l.r)
  end
end

for fld(base from 1, n from lnk(1,lnk(2,mt))):
  base + n
end