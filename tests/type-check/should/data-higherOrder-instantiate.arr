data My-List<A>:
  | my-empty() # less clear what to do with non-thunk version
  | my-link(first :: A, rest :: My-List<A>)
end

fun<A, B> my-map(f :: (A -> B), l :: My-List<A>) -> My-List<B>:
  cases (My-List) l:
    | my-empty()    => my-empty<B>()
    | my-link(a, r) => my-link<B>(f(a), my-map<A, B>(f, r))
  end
end

fun<A, B> my-foldl(f :: (A B -> B), init :: B, l :: My-List<A>) -> B:
  cases (My-List) l:
    | my-empty()    => init
    | my-link(a, r) => my-foldl<A, B>(f, f(a, init), r)
  end
end

fun<A, B> my-foldr(f :: (A B -> B), init :: B, l :: My-List<A>) -> B:
  cases (My-List) l:
    | my-empty()    => init
    | my-link(a, r) => f(a, my-foldr<A, B>(f, init, r))
  end
end
