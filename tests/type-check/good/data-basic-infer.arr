data Natural:
  | zero
  | succ(prev :: Natural)
end

a-synth            = zero
a-check :: Natural = zero

fun is-natural-zero(n :: Natural):
  cases (Natural) n:
    | zero    => true
    | succ(_) => false
  end
end

should-be-bool :: Boolean = is-natural-zero(a-synth)
