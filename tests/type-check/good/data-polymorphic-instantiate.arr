data My-List<A>:
  | my-empty() # less clear what to do with non-thunk version
  | my-link(first :: A, rest :: My-List<A>)
end

fun is-my-list-empty<A>(l :: My-List<A>) -> Boolean:
  cases (My-List<A>) l:
    | my-empty() => true
    | my-link(_, _) => false
  end
end

a-synth                    = my-link<Number>(1, my-empty<Number>())
a-check :: My-List<Number> = my-link<Number>(1, my-empty<Number>())
