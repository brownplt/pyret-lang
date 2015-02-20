
a = {
    make: lam(arr :: RawArray<Number>):
        arr
    end
}

c :: RawArray<Number> = [a: 1, 2, 3, 4]
d = [a: 1, 2, 3, 4]
e :: RawArray<Number> = d



f = {
    make: lam<A>(arr :: RawArray<A>):
        arr
    end
}

g :: RawArray<Number> = [f: 1, 2, 3, 4]
h = [f: 1, 2, 3, 4]
i :: RawArray<Number> = h

j :: RawArray<Any> = [f: 1, 2, 3, "hello"]
