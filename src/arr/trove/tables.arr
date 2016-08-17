provide *
provide-types *

import global as _

type Reducer<Acc, InVal, OutVal> = {
  one :: (InVal -> {Acc; OutVal}),
  reduce :: (Acc, InVal -> {Acc; OutVal})
}

difference-from = lam(init :: Number) -> Reducer<Number, Number, Number>:
  {
    one: lam(n :: Number) -> {Number; Number}:
      { n; n - init; }
    end,
    reduce: lam(last-n :: Number, n :: Number) -> {Number; Number}:
      { n; n - last-n; }
    end
  }
end

difference = difference-from(0)

running-mean :: Reducer<{Number; Number}, Number, Number> = {
  one: lam(n :: Number) -> {{Number; Number}; Number}:
    { {n; 1}; n / 1 }
  end,
  reduce: lam({sum; count}, n) -> {{Number; Number}; Number}:
    next-sum = sum + n
    next-count = count + 1
    { {next-sum; next-count}; next-sum / next-count }
  end
}

running-fold = lam<A>(op :: (A, A -> A)) -> Reducer<A, A, A>:
  {
    one: lam(n): {n; n} end,
    reduce: lam(m, n):
      next-val = op(m, n)
      { next-val; next-val }
    end
  }
end

running-max :: Reducer<Number, Number, Number> = running-fold(num-max)

running-min :: Reducer<Number, Number, Number> = running-fold(num-min)

running-sum :: Reducer<Number, Number, Number> = running-fold(_ + _)

