import str-dict as SD

fun measure(func, length, timesToRun):
  var dict = createDict(length)
  var lst = [list: ]
  for each(i from range(0, timesToRun)):
	start = time-now()
	func(dict)
	fin = time-now()
	lst := link((fin - start), lst)
  end
  lst.reverse()
end

fun createDict(length):
  for fold(dict from [SD.string-dict: {"a"; 0}], i from range(0, length)):
	dict.set(tostring(random(length * 10)), i)
  end
end

fun each_f(dict):
  var key_lst = [list: ]
  for each(tup from dict.items()):
	tup.{0}
  end
end


fun dict_eachf(dict):
  var key_lst = [list: ]
  for SD.dict-each(tup from dict):
	tup.{0}
  end
end

fun dict_each_loopf(dict):
  var key_lst = [list: ]
  for SD.dict-each-loop(tup from dict):
	tup.{0}
  end
end

fun old_eachf(dict):
  var key_lst = [list: ]
  for each(name from dict.keys-list()):
	dict.get-value(name)
  end
end

fun loop(dict):
  for each(i from range(0, dict.count())):
	nothing
  end
end

fun rangef(dict):
  range(0, dict.count())
end

fun dict_items(dict):
  dict.items()
end

fun dict_keys(dict):
  dict.keys-list()
end

fun dostuff(x):
  range(0, x)
end

fun sum-dict(d, f):
  var sum = 0
  var i = 0
  for f(tup from d):
	i := i + 1
	when num-modulo(i, 337) == 0:
  	dostuff(1000)
	end
	sum := sum + tup.{1}
  end
  sum
end

d = createDict(3000)

check:
  sum-dict(d, SD.dict-each-loop) is sum-dict(d, SD.dict-each)
end


print("each_f 10000 dict ran 10 times")
print(measure(each_f, 10000, 10))

print("dict_eachf 10000 dict ran 10 times")
print(measure(dict_eachf, 10000, 10))

print("dict_each_loopf 10000 dict ran 10 times")
print(measure(dict_each_loopf, 10000, 10))


print("old_eachf 10000 dict ran 10 times")
print(measure(old_eachf, 10000, 10))
 

print("each_f 10000 dict ran 20 times")
print(measure(each_f, 10000, 20))

print("dict_eachf 10000 dict ran 20 times")
print(measure(dict_eachf, 10000, 20))

print("dict_each_loopf 10000 dict ran 20 times")
print(measure(dict_each_loopf, 10000, 20))


print("old_eachf 10000 dict ran 20 times")
print(measure(old_eachf, 10000, 20))

 
print("loop 10000 times ran 5 times")
print(measure(loop, 10000, 5))

print("rangef to 10000 ran 5 times")
print(measure(rangef, 10000, 5))

print("dict.items() called on 10000 dict ran 5 times")
print(measure(dict_items, 10000, 5))

print("dict.keys-list() called on 10000 dict ran 5 times")
print(measure(dict_keys, 10000, 5))
