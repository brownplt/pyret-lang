#lang pyret

f = fun:
  fun h():
    5
  check
    when h() == 5:
      checkers.check-equals(0, 0)
    end
  end
  h
end

f()
f()


