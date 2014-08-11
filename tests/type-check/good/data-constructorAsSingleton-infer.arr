data Natural:
  | zero
  | succ(prev :: Natural)
end

data My-List<C>:
  | my-empty
  | my-link(first :: C, rest :: My-List<C>)
end

fun<A, B> my-map(f :: (A -> B), l :: My-List<A>) -> My-List<B>:
  cases(My-List<A>) l:
    | my-empty      => my-empty
    | my-link(a, r) => my-link(f(a), my-map(f, r))
  end
end

fun<A> my-singleton(v :: A) -> My-List<A>:
  my-link(v, my-empty<A>)
end

zero-list-synth                     = my-link(zero, my-link(zero, my-empty<Natural>))
zero-list-check :: My-List<Natural> = my-link(zero, my-link(zero, my-empty<Natural>))

one-list-synth                     = my-map(succ, zero-list-synth)
one-list-check :: My-List<Natural> = my-map(succ, zero-list-check)

zero-list-list-synth                              = my-map(my-singleton<Natural>, zero-list-synth)
zero-list-list-check :: My-List<My-List<Natural>> = my-map(my-singleton<Natural>, zero-list-check)
