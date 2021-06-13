include matrix
emp = [mat(0,0):]
check "Empty Matrix":
  emp.get-shape() is {0;0}
end

check "Matrix rejects incorrect intializations":
  [mat(1,0): 3] raises ""
  [mat(0,1):913] raises ""
  [mat(2,2):1,2,4] raises ""
  [mat(1,2):1.3,4.1,3,4] raises ""
end

check "Matrix intialized with specified dimensions":
  x = [mat(5,1):1,3,55,5325,900]
  y = fill-mat(13,45,0)
  x.get-height() is 5
  x.get-width() is 1
  y.get-shape() is {13;45}
end

check "matrix equals":
  x = [mat(2,2):1,2,3,4]
  y = [mat(2,2):1,3,2,4]
  z = [mat(2,2):1,2,3,4]
  xr = [mat(1,2):1,~2]
  yr = [mat(1,2):1,~2]
  equal-now(x,y) is false
  equal-now(x,z) is true
  equal-now(xr,yr) raises ""
  equal-now([mat(1,1):~12],[mat(1,1):~12]) raises  ""
  equal-now([mat(2,2):1,~2,4,5],[mat(2,2):1,2,4,6]) is false
end

check "fill-mat":
  a = fill-mat(31,47,13.315)
  a.get-shape() is {31;47}
  b = fill-mat(1,93,1/3)
  b.get-shape() is {1;93}
  get-elem(b,0,14) is 1/3
end

check "add-mat":
  t = fill-mat(12,23,9)
  g = fill-mat(12,23,10)
  z = fill-mat(12,23,19)
  add-mat(t,g) is z
  l = [mat(3,2): 5,2,6,81,4,1]
  b = [mat(2,3): 3, 3,1,5,6,7]
  m = [mat(2,2): 5,8,90,12]
  add-mat(l,b) raises ""
  add-mat(l,b) raises ""
  add-mat(m,fill-mat(2,2,3)) is [mat(2,2): 8,11,93,15]
  add-mat(l,l) is [mat(3,2): 10,4,12,162,8,2]
end

check "sub-mat":
  a = [mat(1,2): 1,-2]
  b = [mat(1,2): 1,3]
  sub-mat(a,b) is [mat(1,2):0,-5]
  a - b is [mat(1,2):0,-5]
end

check "mult-mat":
  iden = identity-mat(3)
  a = [mat(3,3):1132,13,42,42,-24,12,63,43,941]
  iden * a is a
  mult-mat(a,iden) is a
  mult-mat(iden,iden) is iden
  b = [mat(2,2): -12/42,~32.34865,23/13,201480214809]
  is-mat(mult-mat(b,b)) is true
end

check "map-mat":
  map-mat(fill-mat(3,3,1),lam(i,j,v):i + j + v end) is [mat(3,3): 1, 2, 3, 2, 3, 4, 3, 4, 5]

  get-elem(map-mat(fill-mat(32,121,32/7),lam(i,j,v): i * j * v end),29,107)
    is%(within(0.001)) ~14185.1428
end

check "get-elem":
  t = map-mat(fill-mat(23,42,0),lam(i,j,_): i + (2 * j) end)
  get-elem(t,0,0) is 0
  get-elem(t,0,-1) raises ""
  get-elem(t,0,42) raises ""
  get-elem(t,22,41) is 104
end

check "reshape":
  a = fill-mat(6,4,0)
  b = reshape(a,12,2)
  b.get-shape() is {12;2}
  reshape(a,24,1).get-shape() is {24;1}
  reshape(a,24,0) raises ""
  reshape(a,12,3) raises ""
  reshape(a,6,2) raises ""
end

check "set-elem":
  a = map-mat(fill-mat(3,3,1),lam(i,j,v):i + j + v end)
  get-elem(a,2,0) is 3
  b = set-elem(a,2,0,~12.42)
  get-elem(b,2,0) is-roughly ~12.42
end

check "get-row get-col":
  a = map-mat(fill-mat(3,3,1),lam(i,j,v):i * j  end)
  get-row(a,0) is [vector(3): 0, 0, 0]
  get-col(a,1) is [vector(3): 0, 1,     2]
  get-row(a,3) raises ""
  get-col(a,-1) raises ""
  b = [mat(2,3):1,2,3,4,5,6]
  get-row(a,3) raises ""
end

check "vdot":
  v1 = [vector(3): 1,3,5]
  v2 = [vector(3): -2,-2,8/5]
  v3 = [vector(3): -2418/13,-1/26,~0]
  vdot(v1,v1) is 35
  vdot(v1,v2) is 0 
  vdot(v2,v3) is-roughly num-to-roughnum(372 + (1/13))
end

check "conversions":
  vector-to-list([vector(5):13,41,5,1,1]) is [list:13,41,5,1,1]
  equal-now(vector-to-array([vector(5):13,41,5,1,1]) , [raw-array:13,41,5,1,1]) is true
end
check "matrix types":
  is-row-mat(emp) is false
  is-col-mat(emp) is false
  is-square-mat(emp) is true
  a = [mat(2,2): 1,4,6,-23]
  b = [mat(1,2):52,1]
  c = [mat(2,1):-9,23/38]
  is-row-mat(a) is false
  is-row-mat(b) is true
  is-row-mat(c) is false

  is-col-mat(a) is false
  is-col-mat(b) is false
  is-col-mat(c) is true

  is-square-mat(a) is true
  is-square-mat(b) is false
  is-square-mat(c) is false

  is-mat(emp) is true
  is-mat(a) is true
  is-mat(b) is true
  is-mat([vector(2):14,4]) is false
  is-mat([list:13,5,235]) is false
  is-mat("EFsdF") is false
end

check "trace-mat":
  a = [mat(3,3):4,5,4,9,0,324,1,6,-7/23]
  trace-mat(a) is 85/23
end

check "gram-schmidt and qr" :
  a = [mat(3,3): 12,-51,4,6,167,-68,-4,24,-41]
  ans = [mat(3,3): 6/7, -69/175, -58/175, 3/7, 158/175, 6/175, -2/7, 6/35, -33/35]
  qr-mat(a)  is {ans;[mat(3,3): 14, 21, -14, 0, 175, -70, 0, 0, 35]}
end

check "vector equality":
  equal-now([vector(3):-31/24,-5.6,12],[vector(3):-31/24,-5.6,12]) is true
  equal-now([vector(3):321,~5,-2],[vector(3):312,~5,3]) is false
end
check "magnitude":
  magnitude([vector(5):13/41,-123/37,42,10,9]) is-roughly ~44.2284
end
check "normalize":
  normalize([vector(1):4]) is [vector(1):1]
end

check "vscale":
  vscale([vector(3):-12.42,~91/43,56],9.12) is [vector(3):9.12 * -12.42,9.12 * ~91/43,9.12 * 56]
end

check "vector operations":
  v1 = [vector(3): 1,3,5]
  v2 = [vector(3): -2,-2,8/5]
  v3 = [vector(3): -2418/13,-1/26,~0]
  (v1 - v2) is [vector(3): 3, 5, 3.4]
  #(v2 + v3) is [vector(3): -188, -53/26, ~1.6]
  #(v1 * v3) is [vector(3): -186, -3/26, ~0]
end

check "determinant":
  a = [mat(2,2):1,2,3,4]
  determinant(a) is -2
  b = [mat(2,2):-1/23,45.231,~6.89,4098]
  determinant(b) is-roughly ((-1/23 * 4098) - (45.231 * ~6.89))
  determinant(identity-mat(42)) is 1
  determinant(fill-mat(21,22,1)) raises "" 
  determinant(fill-mat(5,5,0)) raises "" 
end

check "inverse":
  inverse(fill-mat(21,24,1)) raises "" 
  inverse(identity-mat(45)) is identity-mat(45)
  a = [mat(2,2):56,-21,4.89,12]
  b = [mat(2,2): 400/25823, 100/3689, -163/25823, 800/11067]
  inverse(a) is b
  inverse(b) is a 
  inverse(a) * a is identity-mat(2)
end

check "least-squares and solve":
  a = [mat(2,2):89,-21,34.56,124]
  noninv = [mat(2,2):89,-21,-712/7,24]
  b = [mat(2,1):3,5]
  x = [mat(2,1): 225/5548, 161/5548]
  least-squares(a,b) is x
  solve-mat(a,b) is x
  solve-mat(noninv,b) raises "Cannot solve " 
  least-squares(noninv,b) is [mat(2,1): -133/10057, 0]
end

check "rref-mat":
  rref-mat(identity-mat(13)) is identity-mat(13)
  a = [mat(3,3):1,5,-8/31,4,9,0,0,0,4]
  rref-mat(a) is identity-mat(3)
  b = [mat(2,3): 2,5,90,-14/39,3.598,0]
  rref-mat(b) is [mat(2,3): 1, 0, 451035/12523, 0, 1, 45000/12523]
end

