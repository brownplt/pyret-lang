#lang pyret

check:
  data D:
    | var1(x)
  end
  try:
    fun:;(var1(3))
  except(e):
    e.message.contains("Expected 0 arguments, but got 1") is true
    e.message.contains("var1(3)") is true
  end
end

check:
  data D:
    | var2(x)
  end
  try:
    fun(x, y): x + y;(var2(5))
  except(e):
    e.message.contains("Expected 2 arguments, but got 1") is true
    e.message.contains("The 1 provided argument was:") is true
    e.message.contains("var2(5)") is true
  end
end

check:
  data D:
    | var3(x)
  end
  try:
    fun(x): x;(var3(5), var3(10))
  except(e):
    e.message.contains("Expected 1 argument, but got 2") is true
    e.message.contains("The 2 provided arguments were:") is true
    e.message.contains("var3(5)") is true
    e.message.contains("var3(10)") is true
  end
end

check:
  data D:
    | var4(y)
  end
  try:
    fun(x): x;()
  except(e):
    e.message.contains("Expected 1 argument, but got none") is true
  end
end

check:
  data D:
    | var4(y)
  end
  try:
    fun(x, y): x;()
  except(e):
    e.message.contains("Expected 2 arguments, but got none") is true
  end
end
