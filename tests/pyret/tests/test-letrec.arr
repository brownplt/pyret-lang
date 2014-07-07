import error as E

check:
  fun get-err(thunk):
    cases(Either) run-task(thunk):
      | left(v) => raise("no error")
      | right(v) => v
    end
  end
  letrec f = lam(n):
    if n < 1: 1
    else: n * f(n - 1)
    end
  end:
    f(4) is 24
  end

  letrec
    even = lam(n):
      if n == 1: false
      else: odd(n - 1)
      end
    end,
    odd = lam(n):
      if n == 1: true
      else: even(n - 1)
      end
    end:
    even(100) is true
    odd(5) is true
    odd(6) is false
    even(3) is false
  end

  e8 = get-err(lam(): letrec x-unbound = x-unbound(): 5 end end)
  e8 satisfies E.is-uninitialized-id
  e8.name is "x-unbound"
 
  e9 = get-err(lam(): letrec x = y-unbound, y-unbound = 10: x end end)
  e9 satisfies E.is-uninitialized-id
  e9.name is "y-unbound"

  fun app(f): f() end
  e29 = get-err(lam(): letrec x-appd = app(lam(): x-appd end): x-appd end end)
  e29 satisfies E.is-uninitialized-id
  e29.name is "x-appd"

end
