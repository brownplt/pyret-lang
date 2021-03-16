include matrix

check "Empty Matrix":
  emp = [mat(0,0):]
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
  y = mat-of(13,45,0)
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
end

check "mat-of":
  a = mat-of(31,47,13.315)
  a.get-shape() is {31;47}
  b = mat-of(1,93,1/3)
  b.get-shape() is {1;93}
  get-elem(b,0,14) is 1/3
end

check "add-mat":
  t = mat-of(12,23,9)
  g = mat-of(12,23,10)
  z = mat-of(12,23,19)
  add-mat(t,g) is z
  l = [mat(3,2): 5,2,6,81,4,1]
  b = [mat(2,3): 3, 3,1,5,6,7]
  m = [mat(2,2): 5,8,90,12]
  add-mat(l,b) raises ""
  add-mat(l,b) raises ""
  add-mat(m,mat-of(2,2,3)) is [mat(2,2): 8,11,93,15]
  add-mat(l,l) is [mat(3,2): 10,4,12,162,8,2]
end

check "sub-mat":
  a = [mat(1,2): 1,-2]
  b = [mat(1,2): 1,3]
  sub-mat(a,b) is [mat(1,2):0,-5]
  a - b is [mat(1,2):0,-5]
end

check "mult-mat":
  iden = matrix-map(mat-of(3,3,0),lam(i,j,_):if i == j: 1 else: 0 end end)
  a = [mat(3,3):1132,13,42,42,-24,12,63,43,941]
  iden * a is a
  mult-mat(a,iden) is a
  mult-mat(iden,iden) is iden
end

check "matrix-map":
  matrix-map(mat-of(3,3,1),lam(i,j,v):i + j + v end) is [mat(3,3): 1, 2, 3, 2, 3, 4, 3, 4, 5]

  get-elem(matrix-map(mat-of(32,121,32/7),lam(i,j,v): i * j * v end),29,107)
    is%(within(0.001)) ~14185.1428
end

check "get-elem":
  t = matrix-map(mat-of(23,42,0),lam(i,j,_): i + (2 * j) end)
  get-elem(t,0,0) is 0
  get-elem(t,0,-1) raises ""
  get-elem(t,0,42) raises ""
  get-elem(t,22,41) is 104
end

check "reshape":
  a = mat-of(6,4,0)
  b = reshape(a,12,2)
  b.get-shape() is {12;2}
  reshape(a,24,1).get-shape() is {24;1}
  reshape(a,24,0) raises ""
  reshape(a,12,3) raises ""
  reshape(a,6,2) raises ""
end

check "set-elem":
  a = matrix-map(mat-of(3,3,1),lam(i,j,v):i + j + v end)
  get-elem(a,2,0) is 3
  b = set-elem(a,2,0,~12.42)
  get-elem(b,2,0) is-roughly ~12.42
end

check "get-row get-col":
  a = matrix-map(mat-of(3,3,1),lam(i,j,v):i * j  end)
  get-row(a,0) is [vector(3): 0, 0, 0]
  get-col(a,1) is [vector(3): 0, 1,     2]
  get-row(a,3) raises ""
  get-col(a,-1) raises ""
  b = [mat(2,3):1,2,3,4,5,6]
  get-row(a,3) raises ""
end

check "dot-product":
  v1 = [vector(3): 1,3,5]
  dot-product(v1,v1) is 35
end

check "conversions":
  vector-to-list([vector(5):13,41,5,1,1]) is [list:13,41,5,1,1]
  equal-now(vector-to-array([vector(5):13,41,5,1,1]) , [raw-array:13,41,5,1,1]) is true

end