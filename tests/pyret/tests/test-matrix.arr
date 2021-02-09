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

# sub-mat mult-mat ...

check "mat-of":
  a = mat-of(31,47,13.315)
  a.get-shape() is {31;47}
  b = mat-of(1,93,1/3)
  b.get-shape() is {1;93}
  get-elem(b,0,14) is 1/3
end
