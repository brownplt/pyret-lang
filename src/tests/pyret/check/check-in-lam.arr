#lang pyret

var tests = 0

f = fun:
  fun h():
    5
  check
    when h() == 5:
      tests := tests + 1
    end
  end
end

f()
f()

when tests <> 2:
  print("Expected 2 test runs, got " + tests.tostring())
end

