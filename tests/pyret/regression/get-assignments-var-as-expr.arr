var zs = empty
fun f(x, y, z) block:
  zs := link(z, zs)
  if z == 12:
    x + y + z
  else:
    f(y, x, y)
  end
end

check:
  f(12, 11, 14) is 35
  zs is [list: 12, 11, 14]
end
