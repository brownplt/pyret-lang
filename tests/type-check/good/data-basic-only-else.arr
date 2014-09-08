data Natural:
  | zero
  | succ(prev :: Natural)
end

a-synth            = zero
a-check :: Natural = zero

fun is-natural-number(n :: Natural) -> Boolean:
  cases (Natural) n:
    | else    => true
  end
end
