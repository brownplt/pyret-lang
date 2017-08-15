import js-file("./test-each-loop") as TEL

check:
  fun run-and-print(start, stop, g, rg) block:
    var count = 0
    ans = TEL.run-each-loop(lam(x): count := count + x end, start, stop, g, rg)
    count is ((stop - start) * ((stop - start) - 1)) / 2
    print(ans)
    print("\n")
  end

  run-and-print(0, 100, 10, 10)
  run-and-print(0, 100, 10, 5)
  run-and-print(0, 100, 5, 10)
  run-and-print(0, 100, 5, 2)
  run-and-print(0, 100, 2, 2)
  run-and-print(0, 100, 2, 100)

end

