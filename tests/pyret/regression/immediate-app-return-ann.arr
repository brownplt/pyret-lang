import contracts as C
check:
  (lam() -> Number: "a" end)() raises-satisfies C.is-fail
  (lam(): "a" end)() is "a"
end
