rc = builtins.record-concat

check:

    a = { a: 1, b: 2 }
    b = { a: 10, c: 50 }

    rc(a, b) is { a: 10, b: 2, c: 50 }

    rc({}, {}) is {}
    rc({}, { x: 200 }) is { x : 200 }
    obj1 = {}
    merged = rc({ x: obj1 }, { x: {} })
    merged is { x: {} }
    merged.x is-not<=> obj1

end

check "merging methods":
    o = {
        m: 50,
        x: 100
    }
    merged = rc(o, { method m(self): self.x end })
    merged.m() is 100

    o2 = {
        x: 100
    }
    merged2 = rc(o, { method m(self): self.x end, x: 99 })
    merged2.m() is 99
end

data Point:
    | point(x, y)
end

check "merging data":
    mergedpoints = rc(point(100, 200), point(50, 30))
    mergedpoints.x is 50
    mergedpoints.y is 30
    rc(point(100, 200), point(50, 30)) violates is-point
    rc(point(100, 200), { unrelated-field: 100 }) satisfies is-point
end