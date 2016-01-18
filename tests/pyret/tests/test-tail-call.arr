fun len(l, acc):
  cases (List) l:
    | empty => acc
    | link(_, r) => len(r, 1 + acc)
  end
where:
  # Test TCO on cases-expr
  len(empty, 0) is 0
  len(range(0, 10), 0) is 10
end

data Test:
  | zero
  | one(e :: Test, n :: Number)
  | two(e :: Test, n :: Number)
  | three(e :: Test, n :: Number)
end

fun f<a>(t :: Test) -> Number:
  rec help = lam(shadow t :: Test, acc :: Number) -> Number:
    if is-zero(t):
      acc
    else:
      negative = num-modulo(t.n, 2) == 0
      cases (Test) t:
        | one(e, n) =>
          if negative:
            help(e, acc - n)
          else:
            help(e, acc + n)
          end
        | two(e, n) =>
          if negative:
            help(e, acc - n)
          else:
            help(e, acc + n)
          end
        | three(e, n) =>
          if negative:
            help(e, acc - n)
          else:
            help(e, acc + n)
          end
      end
    end
  end
  help(t, 0)
where:
  # Test TCO on a complex function / not complex cases-expr
  f(zero) is 0
  f(one(two(one(two(three(three(one(two(three(one(three(one(two(three(zero, 1), 2), 3), 4), 5), 6), 7), 8), 9), 10), 11), 12), 13), 14))
    is (1 + 3 + 5 + 7 + 9 + 11 + 13) - (2 + 4 + 6 + 8 + 10 + 12 + 14)
end

fun len(l, acc):
  cases (List) l:
    | empty => acc
    | link(_, r) =>
      a = 1
      b = a + 2
      c = b + 3
      d = c + 4
      e = d + 5
      f = e + 6
      g = f + 7
      h = g + 8
      i = h + 9
      j = i + 10
      k = j + 11
      m = k + 12
      len(r, 1 + acc)
  end
where:
  # Test TCO on very complex cases-expr
  len(empty, 0) is 0
  len(range(0, 10), 0) is 10
end

fun triangle(n :: Number) -> Number:
  fun help(shadow n :: Number, acc :: Number) -> Number:
    if n == 0:
      acc
    else:
      help(n - 1, acc + n)
    end
  end
  help(n, 0)
where:
  # test what-would-be tall stack frame
  triangle(0) is 0
  triangle(10) is 55
  triangle(1000000) is 500000500000
end