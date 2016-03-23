provide *
provide-types *

fun identity<T>(t :: T) -> T: t end

fun all2-strict<A, B>(f :: (A, B -> Boolean), l1 :: List<A>, l2 :: List<B>) -> Boolean:
  doc: ```
        all2 returns false if any application of f returns false, or if the lengths differ.
        his behavior is choosen to maintain the short-circuiting semantics. If one wants to
        distinguish between lists of different lengths, and f returning false, use

        map2-strict(f, l1, l2).and-then(all(identity, _))
       ```
  cases (List<A>) l1:
    | empty       => cases (List<B>) l2:
        | empty       => true
        | link(_, _)  => false
      end
    | link(a, ar) => cases (List<B>) l2:
        | empty       => false
        | link(b, br) => f(a, b) and all2-strict(f, ar, br)
      end
  end
where:
  all2-strict(lam(n, m): false end, [list: 1, 2, 3], empty) is false
  all2-strict(lam(n, m): true  end, [list: 1, 2, 3], empty) is false
  all2-strict(lam(n, m): false end, empty, [list: 1, 2, 3]) is false
  all2-strict(lam(n, m): true  end, empty, [list: 1, 2, 3]) is false 
  all2-strict(lam(n, m): n > m end,        [list: 1, 2, 3], [list: 0, 1, 2]) is true
  all2-strict(lam(n, m): (n + m) == 3 end, [list: 1, 2, 3], [list: 2, 1, 0]) is true
  all2-strict(lam(n, m): n < m end,        [list: 1, 2, 3], [list: 0, 1, 2]) is false
  all2-strict(lam(_, _): true  end, empty, empty) is true
  all2-strict(lam(_, _): false end, empty, empty) is true
end

fun map2-strict<A, B, R>(f :: (A, B -> R), l1 :: List<A>, l2 :: List<B>) -> Option<List<R>>:
  cases (List<A>) l1:
    | empty       => cases (List<B>) l2:
        | empty       => some(empty)
        | link(_, _)  => none
      end
    | link(a, ar) => cases (List<B>) l2:
        | empty       => none
        | link(b, br) => map2-strict(f, ar, br).and-then(lam(rest :: List<R>):
              link(f(a, b), rest)
            end)
     end
  end
where:
  map2-strict(lam(_, _): raise("shipwrecked!");, [list: ], [list: 1 ]) is none
  map2-strict(lam(x, y): x or y;, [list: true, false], [list: false]) is none
  map2-strict(lam(_, _): raise("shipwrecked!");, [list: ], [list: ]) is some([list: ])
  map2-strict(lam(x, y): x or y;, [list: true, false], [list: false, false]) is some([list: true, false])
end

fun fold2-strict<A, B, R>(f :: (R, A, B -> R), base :: R, l1 :: List<A>, l2 :: List<B>) -> Option<R>:
  cases (List<A>) l1:
    | empty       => cases (List<B>) l2:
        | empty       => some(base)
        | link(_, _)  => none
      end
    | link(a, ar) => cases (List<B>) l2:
        | empty       => none
        | link(b, br) => fold2-strict(f, f(base, a, b), ar, br)
      end
  end
where:
  fold2-strict(lam(x, y, z): x - y - z;, 6, [list: 1, 1, 1], [list: ]) is none
  fold2-strict(lam(x, y, z): x - y - z;, 6, [list: ], [list: 1, 1, 1]) is none
  fold2-strict(lam(x, y, z): x - y - z;, 6, [list: 1, 1, 1], [list: 1, 1, 1]) is some(0)
end
