### Looks shipshape
include global
include string
include number
check:
  2 + 2 is 4
  s = "ac"
  2 + 2 is-roughly 4

  "a" + "b" is== "ab"
  s is<=> "ac"
  2 is-not<=> 3
  s is=~ s
  "a" + "b" is-not== "c"
  s is-not=~ "banana"

  fun samelength(a :: String, b :: String) block:
    string-length(a) == string-length(b)
  end

  "a" is%(samelength) "b"
  "a" is-not%(samelength) "bc"

  fun is-odd(n :: Number):
    num-modulo(n, 2) == 1
  end

  5 satisfies is-odd
  2 violates is-odd

  5 / 0 raises "zero"
  5 / 0 raises-other-than "another-kind-of-error"
  5 + 0 does-not-raise

  raise(100) raises-satisfies lam(x): x == 100 end
  raise(100) raises-violates lam(x): x == 101 end

end