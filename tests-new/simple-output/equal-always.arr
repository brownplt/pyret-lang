### true
import global as G

not = G.not

t1 = 5 == 5
t2 = true == true
t3 = "string" == "string"
t4 = { 1; false; "foo" } == { 1; false; "foo" }

true-result = t1 
  and t2 
  and t3
  and t4

f1 = 5 == 4
f2 = true == false
f3 = "string" == "foo"
f4a = { 1; false; "foo" } == { 1; false; "bar" }
f4b = { 1; false; "foo" } == { 0; false; "foo" }
f4c = { 1; false; "foo" } == { 1; true; "foo" }
f4d = { 1; false; "foo" } == { 1; false; "bar"; "baz" }

false-result = not(f1) 
  and not(f2) 
  and not(f3) 
  and not(f4a)
  and not(f4b)
  and not(f4c)
  and not(f4d)

G.console-log(true-result and false-result)
