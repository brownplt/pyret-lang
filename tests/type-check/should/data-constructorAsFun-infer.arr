data Natural:
  | zero
  | succ(prev :: Natural)
end

data My-List<A>:
  | my-empty() # less clear what to do with non-thunk version
  | my-link(first :: A, rest :: My-List<A>)
end

fun<A, B> my-map(f :: (A -> B), l :: My-List<A>) -> My-List<B>:
  cases (My-List) l:
    | my-empty()    => my-empty()
    | my-link(a, r) => my-link(f(a), my-map(f, r))
  end
end

fun<A> my-singleton(v :: A) -> My-List<A>: my-link(v, my-empty()) end

zero-list-synth                     = my-link(zero, my-link(zero, my-empty()))
zero-list-check :: My-List<Natural> = my-link(zero, my-link(zero, my-empty()))

one-list-synth                     = my-map(my-succ, zero-list-synth)
one-list-check :: My-List<Natural> = my-map(my-succ, zero-list-check)

zero-list-list-synth                              = my-map(my-singleton, zero-list-synth)
zero-list-list-check :: My-List<My-List<Natural>> = my-map(my-singleton, zero-list-synth)
