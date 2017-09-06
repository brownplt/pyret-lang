fun ranj(i, j):
  if j <= i: empty
  else:
    link(i, ranj(i + 1, j))
  end
end
range(0,100000) 
# test allocation of a lot of links
