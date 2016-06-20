fun measure(func, timesToRun, l):
  var lst = [list: ]
  for each(i from range(0, timesToRun)):
	start = time-now()
    func(l)
	fin = time-now()
	lst := link((fin - start), lst)
  end
  lst.reverse()
end

fun createList(length):
  for fold(l from [list: ], i from range(0, length)):
    link(random(length * 10), l)
  end
end

fun sum_and_product_tup(l):
  {fsum; fprod} = for fold(totals from {0; 1}, item from l):
    {insum; inprod} = totals
    {insum + item ; inprod * item}
  end
  [list: fsum, fprod]
end


fun sum_and_product_obj(l):
  fsumandprod = for fold(acc from {insum: 0, inprod: 1}, item from l):
    {insum: acc.insum + item, inprod: acc.inprod * item}
  end
  [list: fsumandprod.insum, fsumandprod.inprod]
end

lst = createList(5000)

print("using tuples 10 times")
print(measure(sum_and_product_tup, 10, lst))
print("using object 10 times")
print(measure(sum_and_product_obj, 10, lst))
