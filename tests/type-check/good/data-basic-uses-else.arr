data Natural:
  | zero
  | succ(prev :: Natural)
end

a-synth            = zero
a-check :: Natural = zero

fun is-natural-zero(n :: Natural) -> Boolean:
  cases (Natural) n:
    | zero    => true
    | else    => false
  end
end
