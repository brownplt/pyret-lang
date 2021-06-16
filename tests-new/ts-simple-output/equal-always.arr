### pass
import global as G
import lists as L

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
t7 = foobar(true) == foobar(true)

true-result = t1
  and t2
  and t3
  and t4
  and t5
  and t6
  and t7

G.console-log("f1")
f1 = 5 == 4
G.console-log("f2")
f2 = true == false
G.console-log("f3")
f3 = "string" == "foo"
G.console-log("f4a")
f4a = { 1; false; "foo" } == { 1; false; "bar" }
G.console-log("f4b")
f4b = { 1; false; "foo" } == { 0; false; "foo" }
G.console-log("f4c")
f4c = { 1; false; "foo" } == { 1; true; "foo" }
G.console-log("f4d")
f4d = { 1; false; "foo" } == { 1; false; "bar"; "baz" }
G.console-log("f5")
f5 = { foo: 5, bar: false } == { bar: true, foo: 5 }
G.console-log("f6")
f6 = foo == foobar(true)
G.console-log("f7")
f7 = foobar(true) == foobar(false)
G.console-log("f8")
f8 = foobar(true) == barfoo(false)
G.console-log("f9")
f9 = foobar(true) == barbar(true)
G.console-log("f10")
f10 = foobar(true) == { x: true }
G.console-log("f11")
f11 = foobar(true) == foobar(true).{ x: false }
G.console-log("f12")
f12 = foo == bar
G.console-log("f13")
f13 = foobar(true) == barfoo(true)
G.console-log("f14")
f14 = foobar(true) == barfoo(false).{x: true}
G.console-log("done")

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
  and not(f10)
  and not(f11)
  and not(f12)
  and not(f13)
  and not(f14)

if true-result == false:
  G.console-log([L.list:
    t1,
    t2,
    t3,
    t4,
    t5,
    t6,
    t7,
  ])
else if false-result == false:
  G.console-log([L.list:
    f1,
    f2,
    f3,
    f4a,
    f4b,
    f4c,
    f4d,
    f5,
    f6,
    f7,
    f8,
    f9,
    f10,
    f11,
    f12,
    f13,
    f14,
  ])
else:
  G.console-log("pass")
end
