# import srcloc as S

# type Srcloc = S.Srcloc

lst1 :: List<Number> = empty
lst-result1 :: List<Number> = lst1 + [list: 2]

lst2 :: List<String> = [list: "a"]
lst-result2 :: List<String> = lst2 + [list: "b"]

data Foo<A>:
  | foo1
  | foo2
sharing:
  method _plus(self, foo-thing :: Foo<A>) -> Number:
    4
  end
end

foo1 + foo2

data Bar:
  | bar1(a :: Number) with:
    method _plus(self, bar-thing :: Bar%(is-bar1)):
      bar2
    end
  | bar2
end

bar-result :: Bar = bar1(1) + bar1(2)

data ConcatList<a>:
  | concat-empty
  | concat-singleton(element :: a)
  | concat-append(left :: ConcatList<a>, right :: ConcatList<a>)
  | concat-cons(first :: a, rest :: ConcatList<a>)
  | concat-snoc(head :: ConcatList<a>, last :: a)
sharing:
  method _plus(self, other :: ConcatList<a>) -> ConcatList<a>:
    if is-concat-empty(self): other
    else if is-concat-empty(other): self
    else: concat-append(self, other)
    end
  end
end

clst1 :: ConcatList<Number> = concat-empty
clst-result1 :: ConcatList<Number> = clst1 + concat-cons(2, concat-empty)

clst2 :: ConcatList<String> = concat-cons("a", concat-empty)
clst-result2 :: ConcatList<String> = clst2 + concat-cons("b", concat-empty)

# src1 :: Srcloc = S.srcloc("a", 1, 2, 3, 4, 5, 6)
# src-result1 :: Srcloc = src1 + S.srcloc("b", 2, 3, 4, 5, 6, 7)
