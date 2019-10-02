import file("defines-vars.arr") as D
include from D:
  x, y
end

check:
  fun f(cur-var-value, _):
    cur-var-value
  end

  ans = f(D.x, D.g(12)) # g updates x to the given value
  ans is 10 # x was initially 10
  D.x is 12
end

check:
  fun f(cur-var-value, _):
    cur-var-value
  end

  fun update-y(new-y) block:
    y := new-y
    y
  end

  ans = f(D.y, update-y(30))
  ans is 100 # y was initially 100
  D.y is 30
  y is 30
end

