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
    | my-empty()    => my-empty<B>()
    | my-link(a, r) => my-link<B>(f(a), my-map<A, B>(f, r))
  end
end

fun<A> my-singleton(v :: A) -> My-List<A>: my-link(v, my-empty<A>()) end

zero-list-synth                     = my-link<Natural>(zero, my-link<Natural>(zero, my-empty<Natural>()))
zero-list-check :: My-List<Natural> = my-link<Natural>(zero, my-link<Natural>(zero, my-empty<Natural>()))

one-list-synth                     = my-map<Natural, Natural>(my-succ, zero-list-synth)
one-list-check :: My-List<Natural> = my-map<Natural, Natural>(my-succ, zero-list-check)

zero-list-list-synth                             =
  my-map<Natural, My-List<Natural>>(my-singleton<Natural>, zero-list-synth)
zero-list-list-check:: My-List<My-List<Natural>> =
  my-map<Natural, My-List<Natural>>(my-singleton<Natural>, zero-list-synth)
