check "complex-number functions":
  z = 3+4i
  num-realpart(z) is 3
  num-imagpart(z) is 4
  num-conjugate(z) is 3-4i
  num-magnitude(z) is 5
  num-angle(z) is%(within-abs(0.01)) 0.93

  num-realpart(3) is 3
  num-imagpart(3) is 0
  num-conjugate(3) is 3
  num-magnitude(3) is 3
  num-angle(3) is 0

  num-realpart(-3) is -3
  num-imagpart(-3) is 0
  num-conjugate(-3) is -3
  num-magnitude(-3) is 3
  num-angle(-3) is%(within(0.01)) ~3.14

  num-is-complexrational(1+1i) is true
  num-is-complexroughnum(~1+1i) is true
  num-is-complexrational(1) is true
  num-is-complexroughnum(~1) is true

  num-is-complexroughnum(1+1i) is false
  num-is-complexrational(~1+1i) is false
  num-is-complexrational(~1) is false
  num-is-complexroughnum(1) is false

  num-sqrt(-1) is 0+1i

  num-log(0) raises "zero argument"
  num-imagpart(num-log(-1)) is%(within-abs(0.01)) 3.14

  z1 = 1+1i
  num-realpart(num-log(z1)) is%(within-abs(0.01)) 0.35

  ez1 = num-exp(z1)
  num-realpart(ez1) is%(within-abs(0.01)) 1.47
  num-imagpart(ez1) is%(within-abs(0.01)) 2.29
  num-realpart(num-expt(z1, z1)) is%(within-abs(0.01)) 0.27

  sz1 = num-sin(z1)
  num-realpart(sz1) is%(within-abs(0.01)) 1.3
  num-imagpart(sz1) is%(within-abs(0.01)) 0.63

  cz1 = num-cos(z1)
  num-realpart(cz1) is%(within-abs(0.01)) 0.83
  num-imagpart(cz1) is%(within-abs(0.01)) -0.99

  tz1 = num-tan(z1)
  num-realpart(tz1) is%(within-abs(0.01)) 0.27
  num-imagpart(tz1) is%(within-abs(0.01)) 1.08

  asz1 = num-asin(z1)
  num-realpart(asz1) is%(within-abs(0.01)) 0.67
  num-imagpart(asz1) is%(within-abs(0.01)) 1.06

  acz1 = num-acos(z1)
  num-realpart(acz1) is%(within-abs(0.01)) 0.9
  num-imagpart(acz1) is%(within-abs(0.01)) -1.06

  atz1 = num-atan(z1)
  num-realpart(atz1) is%(within-abs(0.01)) 1.02
  num-imagpart(atz1) is%(within-abs(0.01)) 0.4
end
