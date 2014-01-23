fun f():
  var x = 0
  fun g():
    x := x + 1
  end
  fun h():
    x
  end
  {g:g, h:h}
end
gh = f()
gh2 = f()
gh.g()
test-print(gh.h())
test-print(gh2.h())
gh.g()
test-print(gh.h())
test-print(gh2.h())
gh2.g()
test-print(gh.h())
test-print(gh2.h())
gh2.g()
test-print(gh.h())
test-print(gh2.h())