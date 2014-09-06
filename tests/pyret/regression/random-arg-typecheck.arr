import error as E
check:
  random("a") raises-satisfies E.is-generic-type-mismatch
end
