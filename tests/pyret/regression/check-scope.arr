# https://github.com/brownplt/pyret-lang/issues/518
fun f():
  g()
end

check:
  f() is 4 
end

fun g():
  4
end

