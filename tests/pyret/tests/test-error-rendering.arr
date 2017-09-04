import file("../test-compile-helper.arr") as C
import load-lib as L

check "constructor arity from function":

  ans = C.run-to-result(```
data D:
  c(a, b)
end
fun f():
  c(1)  
end
f()
```)

  message = L.render-error-message(ans.v).message

  # Should report the callee at line 2
  message satisfies string-contains(_, ":2:2-2:9")
  # Should report the caller at line 5
  message satisfies string-contains(_, ":5:2-5:6")

end

check "builtin constructor arity from function":

  ans = C.run-to-result(```
fun f():
  link(1)  
end
f()
```)

  message = L.render-error-message(ans.v).message

  # Should report the callee within lists
  message satisfies string-contains(_, "lists")
  # Should report the caller at line 2
  message satisfies string-contains(_, ":2:2-2:9")

end


check "constructor via a builtin callback":

  ans = C.run-to-result(```
data D:
  c(a, b)
end
fun f():
  map(c, [list: "irrelevant"])
end
f()
```)

  message = L.render-error-message(ans.v).message

  # Should report the caller within lists
  message satisfies string-contains(_, "lists")
  # Should report the callee at line 2
  message satisfies string-contains(_, ":2:2-2:9")

end


check "table row length mismatch":
  ans = C.run-to-result(```
    t = table: cola, colb end  
    [t.new-row: 1, 2, 3]
  ```)

  message = L.render-error-message(ans.v).message
  message satisfies string-contains(_, "The row could not be constructed")
end

