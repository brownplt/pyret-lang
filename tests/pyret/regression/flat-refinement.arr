data D:
  | d(x)
end


fun flat(my-d :: D%(is-d)):
  my-d.x
end

some-d = d(10)

fun recur(upto):
  if upto == 0 block: nothing
  else:
    thing2 = flat(some-d) + 1
    ans = recur(upto - 1)
    thing = 1 + 1
    ans
  end
end

check:
  recur(3000) is nothing
  recur(300) is nothing
  recur(500) is nothing
  recur(391) is nothing
  recur(5) is nothing
end
