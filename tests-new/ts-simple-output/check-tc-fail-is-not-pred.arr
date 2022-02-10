##! type-mismatch
include global
include string
check:
  fun samelength(a :: String, b :: String) block:
    string-length(a) == string-length(b)
  end
  5 is-not%(samelength) "a"
end