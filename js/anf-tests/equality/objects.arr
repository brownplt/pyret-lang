o = {}
test-print(o == o)
o2 = {}
test-print(o == o2)
o3 = {x:5}
test-print(o == o3)
test-print(o2 == o3)
test-print(o3 == o3)
o4 = {x:5}
test-print(o3 == o4)
test-print(o4 == o3)

o5 = {obj: o3, y:10}
o6 = {obj: o4, y:12}
o7 = {obj: o4, y:10}

test-print(o5 == o6)
test-print(o5.obj == o6.obj)
test-print(o5 == o7)
test-print(o5.obj == o7.obj)

