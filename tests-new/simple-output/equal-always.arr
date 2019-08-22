### true
import global as G

not = G.not

data FooData: 
  | foo
  | foobar(x :: Boolean)
end

data BarData:
  | bar
  | barfoo(x :: Boolean)
  | barbar(y :: Boolean)
end

t1 = 5 == 5
t2 = true == true
t3 = "string" == "string"
t4 = { 1; false; "foo" } == { 1; false; "foo" }
t5 = { foo: 5, bar: false } == { bar: false, foo: 5 }
t6 = foo == foo
t7 = foo == bar
t8 = foobar(true) == foobar(true)
t9 = foobar(true) == barfoo(true)

true-result = t1 
  and t2 
  and t3
  and t4
  and t5
  and t6
  and t7
  and t8
  and t9

f1 = 5 == 4
f2 = true == false
f3 = "string" == "foo"
f4a = { 1; false; "foo" } == { 1; false; "bar" }
f4b = { 1; false; "foo" } == { 0; false; "foo" }
f4c = { 1; false; "foo" } == { 1; true; "foo" }
f4d = { 1; false; "foo" } == { 1; false; "bar"; "baz" }
f5 = { foo: 5, bar: false } == { bar: true, foo: 5 }
f6 = foo == foobar(true)
f7 = foobar(true) == foobar(false)
f8 = foobar(true) == barfoo(false)
f9 = foobar(true) == barbar(true)

false-result = not(f1) 
  and not(f2) 
  and not(f3) 
  and not(f4a)
  and not(f4b)
  and not(f4c)
  and not(f4d)
  and not(f5)
  and not(f6)
  and not(f7)
  and not(f8)
  and not(f9)

G.console-log(true-result and false-result)
