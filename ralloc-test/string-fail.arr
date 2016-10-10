check "For loop is not borked":
  fun run-test():
    ix1 = random(65535)
    ix2 = random(65535)
    ix3 = random(65535)
    str = string-from-code-points([list: ix1, ix2, ix3])
    # this should always be true for non-astral plane characters
    codes = string-to-code-points(str)
    codes is [list: ix1, ix2, ix3]
  end
  for each(i from range(0, 10)):
    run-test()
  end
end