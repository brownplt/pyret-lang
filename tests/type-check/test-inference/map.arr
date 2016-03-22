fun add1(x :: Number) -> Number:
  x + 1
where:
  add1(1) is 2
  add1(2) is 3
end

fun str-to-str(s :: String) -> String:
  s
where:
  str-to-str("a") is "a"
  str-to-str("ab") is "ab"
end

fun str-to-num(s :: String) -> Number:
  1
where:
  str-to-num("a") is 1
  str-to-num("") is 1
end

fun id<A>(x :: A) -> A:
  x
where:
  id(1) is 1
  id("a") is "a"
end

fun forall-to-num<A>(x :: A) -> Number:
  1
where:
  forall-to-num(1) is 1
  forall-to-num("a") is 1
end

fun my-map<A,B>(f :: (A -> B), xs :: List<A>) -> List<B>:
  cases(List<A>) xs:
    | empty => empty
    | link(first, rest) =>
      link(f(first), my-map(f, rest))
  end
where:
  my-map(add1, empty) is empty
  my-map(add1, [list: 1, 2, 3]) is [list: 2, 3, 4]
  my-map(str-to-str, empty) is empty
  my-map(str-to-str, [list: "a", "b", "c"]) is [list: "a", "b", "c"]
  my-map(str-to-num, empty) is empty
  my-map(str-to-num, [list: "a", "ab", "abc"]) is [list: 1, 1, 1]
end

#fun my-map<A,B>(f :: (A -> B), xs :: List<A>) -> List<B>:
#  cases(List<A>) xs:
#    | empty => empty
#    | link(first, rest) =>
#      link(f(first), my-map(f, rest))
#  end
#where:
#  my-map(add1, empty) is empty
#  my-map(add1, [list: 1, 2, 3]) is [list: 2, 3, 4]
#  my-map(str-to-str, empty) is empty
#end

#fun my-map<A, B>(f :: (A -> B), xs :: List<A>) -> List<B>:
#  cases(List<A>) xs:
#    | empty => empty
#    | link(first, rest) =>
#      link(f(first), my-map(f, rest))
#  end
#where:
#  my-map(add1, empty) is empty
#  my-map(add1, [list: 1, 2, 3]) is [list: 2, 3, 4]
#  my-map(str-to-num, empty) is empty
#end

#fun my-map<A, B>(f :: (A -> B), xs :: List<A>) -> List<B>:
#  cases(List<A>) xs:
#    | empty => empty
#    | link(first, rest) =>
#      link(f(first), my-map(f, rest))
#  end
#where:
#  my-map(id, empty) is empty
#  my-map(id, [list: 1, 2, 3]) is [list: 1, 2, 3]
#  my-map(id, [list: "a", "b", "c"]) is [list: "a", "b", "c"]
#  my-map(forall-to-num, empty) is empty
#  my-map(forall-to-num, [list: 1, 2, 3]) is [list: 1, 1, 1]
#  my-map(forall-to-num, [list: "a"]) is [list: 1]
#end
