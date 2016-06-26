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

fun sum_and_product_tup_comp(l):
  {fsum; fprod} = for fold(totals from {0; 1}, item from l):
    {insum; inprod} = totals
    var computation = insum + inprod + (insum * inprod)
    #var more_comp = ((insum * (inprod - insum)) + (inprod + inprod)) * (insum - inprod) 
    {insum + item ; inprod * item}
  end
  [list: fsum, fprod]
end


fun sum_and_product_obj_comp(l):
  fsumandprod = for fold(acc from {insum: 0, inprod: 1}, item from l):
    var computation = acc.insum + acc.inprod + (acc.insum * acc.inprod)
    #  var more_comp = ((acc.insum * (acc.inprod - acc.insum)) + (acc.inprod + acc.inprod)) * (acc.insum - acc.inprod) 
    {insum: acc.insum + item, inprod: acc.inprod * item}
  end
  [list: fsumandprod.insum, fsumandprod.inprod]
end


shadow lst = createList(10000)

print("using tuples 5 times with comp")
print(measure(sum_and_product_tup_comp, 5, lst))
print("using object 5 times with comp")
print(measure(sum_and_product_obj_comp, 5, lst))
