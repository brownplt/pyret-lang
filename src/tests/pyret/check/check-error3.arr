#lang pyret

fun f(x):
  fun h():

  check:
    checkers.check-equals(0, 0)
    when x == 0:
      raise("Done checking")
    end
  end
  nothing
check:
  f(0) # increment success & error, recover
  checkers.check-equals(0, 1) # increment failed count
  f(1) # increment success
  checkers.check-equals(1, 1) # increment success
end

