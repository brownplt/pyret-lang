check "https://github.com/brownplt/pyret-lang/issues/362":
  f =
    lam(k):
      k((lam(shadow k):
            k(1)
          end)(lam(l-v):
            (lam(shadow k):
                k(2)
              end)
            (lam(r-v):
                k(l-v + r-v)
              end)
          end))
    end

  f(lam(x): x end) satisfies is-function

  g = lam():
    lam(x): x end
    lam(x): x + 1 end
  end
  g()(5) is 6


  h = block:
    lam(): 10 end
    lam(): 20 end
    lam(): 30 end
    lam(): 40 end
  end

  h() is 40

  fun noop(): "noop" end

  s = if block:
        lam(): 10 end
        noop()
        lam(): 20 end
        lam(): 30 end
        noop()
        lam(): 40 end
      end() == 40:
    "worked"
  else:
    "didn't"
  end

  s is "worked"


  block:
    lam(): 10 end
    { x: 55 }
  end.x is 55

end
