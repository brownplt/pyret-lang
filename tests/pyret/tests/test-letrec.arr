check:
  letrec f = fun(n):
    if n < 1: 1
    else: n * f(n - 1)
    end
  end:
    f(4) is 24
  end

  letrec
    even = fun(n):
      if n == 1: false
      else: odd(n - 1)
      end
    end,
    odd = fun(n):
      if n == 1: true
      else: even(n - 1)
      end
    end:
    even(100) is true
    odd(5) is true
    odd(6) is false
    even(3) is false
  end
  
end
