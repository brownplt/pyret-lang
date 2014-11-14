check "roughnum":
  1 + 2 is 3
  1 + ~2 is ~3
  num-sin(0) is 0
  num-sin(1) is ~0.8414709848078965
  1 < 2 is true
  ~1 < ~2 is true
  2 == (1 + 1) is true
  ~2 == (~1 + ~1) is true #actually false eventually
  1.5 is 3/2
  num-sqrt(-1) raises "sqrt of negative number"
end
