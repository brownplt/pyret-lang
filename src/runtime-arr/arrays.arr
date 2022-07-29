provide: type Array, * end

include global
include lists
include raw-array
include valueskeleton

get-arr-key = {}

data Array<A>:
  | array(elements :: RawArray<A>)
sharing:
    method get-arr(self, key):
      if key == get-arr-key: self.elements else: raise("Cannot get arr externally") end
    end,
    method get-now(self, ix :: Number): raw-array-get(self.elements, ix) end,
    method set-now(self, ix :: Number, val) -> Nothing block:
      raw-array-set(self.elements, ix, val)
      nothing
    end,
    method length(self): raw-array-length(self.elements) end,
    method to-list-now(self): raw-array-to-list(self.elements) end,
    method _equals(self, other :: Array<A>, eq :: (Any, Any -> Boolean)):
      eq(self.get-arr(get-arr-key), other.get-arr(get-arr-key))
    end,
    method _output(self, rec-output :: (Any -> ValueSkeleton)) -> ValueSkeleton:
      vs-collection("array", raw-array-map(rec-output, self.elements))
    end
end

make = array

fun build-array<A>(f :: (Number -> A), len :: Number) -> Array<A> block:
  arr = if len == 0: [raw-array:]
  else:
    raw-array-of(f(0), len)
  end
  fun loop(i):
    when i < len block:
      raw-array-set(arr, i, f(i))
      loop(i + 1)
    end
  end
  loop(1)
  array(arr)
end

fun array-from-list<A>(l :: List<A>) -> Array<A> block:
  arr = cases(List) l:
    | empty => [raw-array:]
    | link(f, _) => raw-array-of(f, l.length())
  end
  for each_n(n from 0, elt from l) block:
    raw-array-set(arr, n, elt)
    nothing
  end
  make(arr)
end

fun array-of<a>(elt :: a, count :: Number) -> Array<a>:
  arr = raw-array-of(elt, count)
  make(arr)
end

fun array-set-now<a>(arr :: Array<a>, index :: Number, val :: a) -> Nothing:
  arr.set-now(index, val)
end

fun array-get-now<a>(arr :: Array<a>, index :: Number) -> a:
  arr.get-now(index)
end

fun array-length<a>(arr :: Array<a>) -> Number:
  arr.length()
end

fun array-to-list-now<a>(arr :: Array<a>) -> List<a>:
  arr.to-list-now()
end

shadow array = {
  make: make,
  make0: lam<A>() -> Array<A>: make([raw-array: ]) end,
  make1: lam<A>(a :: A) -> Array<A>: make([raw-array: a]) end,
  make2: lam<A>(a :: A, b :: A) -> Array<A>: make([raw-array: a, b]) end,
  make3: lam<A>(a :: A, b :: A, c :: A) -> Array<A>: make([raw-array: a, b, c]) end,
  make4: lam<A>(a :: A, b :: A, c :: A, d :: A) -> Array<A>: make([raw-array: a, b, c, d]) end,
  make5: lam<A>(a :: A, b :: A, c :: A, d :: A, e :: A) -> Array<A>: make([raw-array: a, b, c, d, e]) end
}
