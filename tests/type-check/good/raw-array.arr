
a = {
  make: lam(arr :: RawArray<Number>):
      arr
    end
  # make0: lam(): [raw-array: ] end,
  # make1: lam(a): [raw-array: a] end,
  # make2: lam(a, b): [raw-array: a, b] end,
  # make3: lam(a, b, c): [raw-array: a, b, c] end,
  # make4: lam(a, b, c, d): [raw-array: a, b, c, d] end,
  # make5: lam(a, b, c, d, e): [raw-array: a, b, c, d, e] end,
}

c :: RawArray<Number> = [a: 1, 2, 3, 4, 5, 6]
d = [a: 1, 2, 3, 4, 5, 6]
e :: RawArray<Number> = d



f = {
    make: lam<A>(arr :: RawArray<A>):
        arr
    end
}

g :: RawArray<Number> = [f: 1, 2, 3, 4, 5, 6]
h = [f: 1, 2, 3, 4, 5, 6]
i :: RawArray<Number> = h

j :: RawArray<Any> = [f: 1, 2, 3, 4, 5, "hello"]
