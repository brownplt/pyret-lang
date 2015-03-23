# zero-tolerance is used for comparing roughnums
zero-tolerance = ~1e-7

# quadrature-error is the error constraint for our numerical quadrature.
# For the adaptive numerical algorithm we use, we will refine the error as we
# drill down into the intervals
quadrature-error = ~1e-5

# quadrature-simpson(f, a, b) numerically estimates ∫{a..b} f(x)dx using
# an adaptive Simpson algorithm
fun quadrature-simpson(f, a, b):
  fun simpson-rule(h, y0, y1, y2):
    (1/3) * h * (y0 + (4 * y1) + y2)
  end
  fun quadrature-segment(x2, y0, y2, y4, h, err, sum-0-2-4):
    x1 = x2 - h
    x3 = x2 + h
    y1 = f(x1)
    y3 = f(x3)
    sum-0-1-2 = simpson-rule(h, y0, y1, y2)
    sum-2-3-4 = simpson-rule(h, y2, y3, y4)
    sum-0-1-2-3-4 = sum-0-1-2 + sum-2-3-4
    if num-abs(sum-0-1-2-3-4 - sum-0-2-4) <= (15 * err):
      sum-0-1-2-3-4
    else:
      h1 = h / 2
      err1 = err / 2
      if num-within(zero-tolerance)(h1, 0) or
        num-within(zero-tolerance)(err1, 0):
        sum-0-1-2-3-4
      else:
        quadrature-segment(x1, y0, y1, y2, h1, err1, sum-0-1-2) +
        quadrature-segment(x3, y2, y3, y4, h1, err1, sum-2-3-4)
      end
    end
  end
  h = (b - a) / 2
  m = a + h
  fa = f(a)
  fb = f(b)
  fm = f(m)
  quadrature-segment(m,
  fa, fm, fb,
  h / 2,
  quadrature-error,
  simpson-rule(h, fa, fm, fb))
end

# in the following, we define some functions
# P(x)
# and their corresponding known indefinite integrals (to within a const)
# intP(x) = ∫P(x)dx.
# we then check that
# quadrature-simpson(P, a, b) = intP(b) - intP(a)

# P(x) = x^9 + e^(2x) + cos 3x
fun P(x):
  num-expt(x, 9) + num-exp(2 * x) + num-cos(3 * x)
end

# ∫P(x)dx = (x^10)/10 + (e^(2x))/2 + (sin 3x)/3
fun intP(x):
  (num-expt(x, 10) / 10) + (num-exp(2 * x) / 2) + (num-sin(3 * x) / 3)
end

# Q(x) = x^2 e^(ax)
fun Q(x):
  a = 2
  x * x * num-exp(a * x)
end

# ∫Q(x)dx = [(e^(ax))/(a^3)] * (a^2 x^2 - 2ax + 2)
fun intQ(x):
  a = 2
  (num-exp(a * x) / (a * a * a)) * ((a * a * x * x) + (-2 * a * x) + 2)
end

check "match Simpson numerical integration results against known actual answers":
  quadrature-simpson(P, 1, 5) is%(num-within(quadrature-error)) (intP(5) - intP(1))
  quadrature-simpson(Q, 2, 3) is%(num-within(quadrature-error)) (intQ(3) - intQ(2))
end
