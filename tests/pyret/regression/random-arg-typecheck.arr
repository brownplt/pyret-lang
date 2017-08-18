import contracts as C
check:
  random("a") raises-satisfies C.is-fail
end
