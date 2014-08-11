data My-List<A>:
  | my-empty() # less clear what to do with non-thunk version
  | my-link(first :: A, rest :: My-List<A>)
end

fun<A, B> my-map(f :: (A -> B), l :: My-List<A>) -> My-List<B>:
  cases (My-List<A>) l:
    | my-empty()    => my-empty()
    | my-link(a, r) => my-link(f(a), my-map(f, r))
  end
end

fun<A, B> my-foldl(f :: (A, B -> B), init :: B, l :: My-List<A>) -> B:
  cases (My-List<A>) l:
    | my-empty()    => init
    | my-link(a, r) => my-foldl(f, f(a, init), r)
  end
end

fun<A, B> my-foldr(f :: (A, B -> B), init :: B, l :: My-List<A>) -> B:
  cases (My-List<A>) l:
    | my-empty()    => init
    | my-link(a, r) => f(a, my-foldr(f, init, r))
  end
end
