data My-List<A>:
  | my-empty() # less clear what to do with non-thunk version
  | my-link(first :: A, rest :: My-List<A>)
end

# cases example had not instantiation to infer

a-synth                    = my-link(1, my-empty())
a-check :: My-List<Number> = my-link(1, my-empty())
