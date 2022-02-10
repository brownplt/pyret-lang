### Looks shipshape
include global
include string
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

end