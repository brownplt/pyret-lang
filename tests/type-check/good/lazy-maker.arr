a = {
    lazy-make: lam(constructors :: RawArray<(-> Number)>):
        raw-array-to-list(constructors).map(lam(c): c() end)
    end
}

c :: List<Number> = [lazy a: 1, 2]