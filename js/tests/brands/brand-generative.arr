fun f(x):
  b = brander()
  {brand: b, val: b.brand(x)}
end
o1 = f({})
o2 = f({})
o1.brand.test(o1.val) and o2.brand.test(o2.val) and (not o1.brand.test(o2.val)) and (not (o2.brand.test(o1.val)))
