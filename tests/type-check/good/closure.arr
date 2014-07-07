

fun replace-with(a :: Number, b :: Number):
  lam(x :: Number):
    if x == a:
      b
    else:
      x
    end
  end
end

my-fun = replace-with(5, 6)
my-fun(5)
my-fun(7)
