# https://github.com/brownplt/pyret-lang/commit/b193292fdf937eb756508e9ae5a0c80a9eb6ecfa
check:
  var count = 0
  builtins.raw-each-loop(lam(i):
      run-task(lam() block:
          count := count + 1
          "should not return"
        end)
    end, 0, 6) is nothing
  count is 6
end
