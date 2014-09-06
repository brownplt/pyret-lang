data MutCar:
  | mpair(ref car, cdr)
end

check:
  m1 = mpair("a", "b")

  cases(MutCar) m1:
    | mpair(car, cdr) => car + cdr
  end raises "ref field"

  cases(MutCar) m1:
    | mpair(ref car, cdr) => car + cdr
  end is "ab"

  cases(MutCar) m1:
    | mpair(car, ref cdr) => car + cdr
  end raises "ref field"

  cases(MutCar) m1:
    | mpair(ref car, ref cdr) => car + cdr
  end raises "non-ref field"


  ref-graph:
    r = r
  end
  ref-set(r, "b")
  m2 = mpair("a", r)

  cases(MutCar) m2:
    | mpair(car, cdr) => car + cdr
  end raises "ref field"

  cases(MutCar) m2:
    | mpair(ref car, cdr) => car + ref-get(cdr)
  end is "ab"

  cases(MutCar) m2:
    | mpair(car, ref cdr) => car + cdr
  end raises "ref field"

  cases(MutCar) m2:
    | mpair(ref car, ref cdr) => car + cdr
  end is "ab"

end
