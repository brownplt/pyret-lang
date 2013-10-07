data Buck:
  | mt
  | lnk(f,r)
end

fun mp(f, l):
  if is-mt(l):
    mt
  else:
    lnk(f(l.f), mp(f, l.r))
  end
end

for mp(i from lnk(1,lnk(2,mt))):
  i+1
end