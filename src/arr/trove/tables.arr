provide *
provide-types *

import global as _
include lists

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

running-fold = lam<Result, Col>(init :: Result, op :: (Result, Col -> Result)) -> Reducer<Result, Col, Result>:
  {
    one: lam(n):
      first-row = op(init, n)
      { first-row; first-row }
    end,
    reduce: lam(m, n):
      next-val = op(m, n)
      { next-val; next-val }
    end
  }
end

running-reduce = lam<Col>(op :: (Col, Col -> Col)) -> Reducer<Col, Col, Col>:
  {
    one: lam(n): {n; n} end,
    reduce: lam(m, n):
      next-val = op(m, n)
      { next-val; next-val }
    end
  }
end

running-max :: Reducer<Number, Number, Number> = running-reduce(num-max)

running-min :: Reducer<Number, Number, Number> = running-reduce(num-min)

running-sum :: Reducer<Number, Number, Number> = running-reduce(_ + _)

raw-row = {
  make: lam(arr): builtins.raw-make-row(arr) end,
  make0: lam(): builtins.raw-make-row([raw-array:]) end,
  make1: lam(t): builtins.raw-make-row([raw-array: t]) end,
  make2: lam(t1, t2): builtins.raw-make-row([raw-array: t1, t2]) end,
  make3: lam(t1, t2, t3): builtins.raw-make-row([raw-array: t1, t2, t3]) end,
  make4: lam(t1, t2, t3, t4): builtins.raw-make-row([raw-array: t1, t2, t3, t4]) end,
  make5: lam(t1, t2, t3, t4, t5): builtins.raw-make-row([raw-array: t1, t2, t3, t4, t5]) end,
}

fun table-from-raw-array(arr):
  cols = raw-array-get(arr, 0).get-column-names()
  with-cols =
    for fold(t from table: ignore end.drop("ignore"),
            c from cols):
      t.add-column(c, empty)
    end
  for raw-array-fold(t from with-cols, r from arr, _ from 0):
    t.add-row(r)
  end
end

table-from-rows = {
  make: table-from-raw-array,
  make0: lam(): table-from-raw-array([raw-array:]) end,
  make1: lam(t): table-from-raw-array([raw-array: t]) end,
  make2: lam(t1, t2): table-from-raw-array([raw-array: t1, t2]) end,
  make3: lam(t1, t2, t3): table-from-raw-array([raw-array: t1, t2, t3]) end,
  make4: lam(t1, t2, t3, t4): table-from-raw-array([raw-array: t1, t2, t3, t4]) end,
  make5: lam(t1, t2, t3, t4, t5): table-from-raw-array([raw-array: t1, t2, t3, t4, t5]) end,
}

fun table-from-column<A>(col-name :: String, values :: List<A>) -> Table:
  rows = for map(v from values):
    raw-row.make([raw-array: {col-name; v}])
  end
  table-from-rows.make(raw-array-from-list(rows))
end

