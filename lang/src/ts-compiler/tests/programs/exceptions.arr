import either as E

result = run-task(lam(): raise("boom") end)
cases(E.Either) result:
  | left(v) => print("no exception\n")
  | right(exn) => print("caught\n")
end

ok = run-task(lam(): 42 end)
cases(E.Either) ok:
  | left(v) => print("value " + tostring(v) + "\n")
  | right(exn) => print("exn\n")
end
