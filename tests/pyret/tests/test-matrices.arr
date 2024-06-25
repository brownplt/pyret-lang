# import matrix as MX
import matrices as M
include from M:
  matrix,
  matrix-within,
  row-matrix,
  col-matrix,
  vector,
  vector-within
end
include from lists: all2 end
# include from MX: * hiding(vector), type * end

fun list-matrix-within(n :: Number) -> (List<M.Matrix>, List<M.Matrix> -> Boolean):
  lam(a, b):
    wn = matrix-within(n)
    all2(wn,a,b)
  end
end

mtx1 = [matrix(2,3): 1, 2, 3, 
                     4, 5, 6]
mtx2 = [matrix(3,3): 1, 1, 1, 
                     2, 2, 2, 
                     3, 3, 3]
mtx3 = [matrix(3,2): 1, 2,
                     3, 4, 
                     5, 6]

mtx4 = [matrix(3,1): 1, 
                     2, 
                     3]
mtx5 = [matrix(3,3): 1, 0, 0, 
                     2, 0, 0,
                     3, 0, 0]

check "Constructor Errors":
  [matrix(1,2):] raises "Invalid 1x2 Matrix Input: Expected 2 elements; Received 0 elements"
  [row-matrix:] raises "Invalid Matrix Input: Cannot construct a zero-length row matrix"
  [col-matrix:] raises "Invalid Matrix Input: Cannot construct a zero-length column matrix"
  [matrix(1,0):] raises-other-than "Impossible" # Check that the type refinement is working
  M.is-matrix([matrix(1,1): 1]) is true
end

check "Vector Operations":
  [vector: 1, 2, 3].magnitude() is%(within(0.001)) num-sqrt(14)

  [vector: 2, -3, 1].cross([vector: -2, 1, 1]) is [vector: -4, -4, -4]

  [vector: 1, 2, 3].normalize() is%(vector-within(0.001))
  [vector: (1 / num-sqrt(14)), (2 / num-sqrt(14)), (3 / num-sqrt(14))]

  # Functional API
  M.vec-magnitude([vector: 1, 2, 3]) is%(within(0.001)) num-sqrt(14)

  M.vec-cross([vector: 2, -3, 1], [vector: -2, 1, 1]) is [vector: -4, -4, -4]

  M.vec-normalize([vector: 1, 2, 3]) is%(vector-within(0.001))
  [vector: (1 / num-sqrt(14)), (2 / num-sqrt(14)), (3 / num-sqrt(14))]
end

check "Basic Matrix Accessors":
    
  mtx1.get(1,1) is 5
  mtx1.get(0,2) is 3
  
  mtx1.rows is 2
  mtx1.cols is 3
  
  mtx1.to-list() is [list: 1, 2, 3, 4, 5, 6]
  
  mtx1.row(0) is [matrix(1,3): 1, 2, 3]
  mtx1.row(1) is [matrix(1,3): 4, 5, 6]
  mtx1.col(1) is [matrix(2,1): 2, 
                               5]
  mtx1.col(2) is [matrix(2,1): 3,
                               6]
  
  mtx1.row(0).to-vector() is [vector: 1, 2, 3]
  mtx1.col(1).to-vector() is [vector: 2, 5]

  # Functional API
  M.mtx-get(mtx1, 1, 1) is 5
  M.mtx-get(mtx1, 0, 2) is 3
  
  M.mtx-to-list(mtx1) is [list: 1, 2, 3, 4, 5, 6]
  
  M.mtx-row(mtx1, 0) is [matrix(1,3): 1, 2, 3]
  M.mtx-row(mtx1, 1) is [matrix(1,3): 4, 5, 6]
  M.mtx-col(mtx1, 1) is [matrix(2,1): 2, 
                                      5]
  M.mtx-col(mtx1, 2) is [matrix(2,1): 3,
                                      6]
  
  M.mtx-row(mtx1, 0).to-vector() is [vector: 1, 2, 3]
  M.mtx-col(mtx1, 1).to-vector() is [vector: 2, 5]

end

check "Submatrices":

  mtx1.submatrix([list: 0, 1], [list: 0, 2]) is
  [matrix(2,2): 1, 3, 
                4, 6]
  mtx1.submatrix([list: 1], [list: 0, 1, 2]) is
  [matrix(1,3): 4, 5, 6]
  mtx1.submatrix([list: 0], [list: 1, 2]) is
  [matrix(1,2): 2, 3]

  # Functional API
  M.mtx-submatrix(mtx1, [list: 0, 1], [list: 0, 2]) is
  [matrix(2,2): 1, 3, 
                4, 6]
  M.mtx-submatrix(mtx1, [list: 1], [list: 0, 1, 2]) is
  [matrix(1,3): 4, 5, 6]
  M.mtx-submatrix(mtx1, [list: 0], [list: 1, 2]) is
  [matrix(1,2): 2, 3]
end

check "Matrix Transpose":
  
  mtx1.transpose() is 
  [matrix(3,2): 1, 4, 
                2, 5, 
                3, 6]

  # Functional API
  M.mtx-transpose(mtx1) is 
  [matrix(3,2): 1, 4, 
                2, 5, 
                3, 6]

end
check "Matrix Diagonal":
  mtx1.diagonal() is
  [matrix(1,2): 1, 5]
  mtx2.diagonal() is
  [matrix(1,3): 1, 2, 3]
  mtx3.diagonal() is
  [matrix(1,2): 1, 4]

  # Functional API
  M.mtx-diagonal(mtx1) is
  [matrix(1,2): 1, 5]
  M.mtx-diagonal(mtx2) is
  [matrix(1,3): 1, 2, 3]
  M.mtx-diagonal(mtx3) is
  [matrix(1,2): 1, 4]
end

check "Matrix Upper/Lower Triangles":

  mtx1.upper-triangle() raises "Cannot make a non-square upper-triangular matrix"
  mtx2.upper-triangle() is
  [matrix(3, 3): 1, 1, 1,
                 0, 2, 2, 
                 0, 0, 3]
  mtx1.lower-triangle() raises "Cannot make a non-square lower-triangular matrix"
  mtx2.lower-triangle() is
  [matrix(3,3): 1, 0, 0, 
                2, 2, 0, 
                3, 3, 3]

  # Functional API
  M.mtx-upper-triangle(mtx1) raises "Cannot make a non-square upper-triangular matrix"
  M.mtx-upper-triangle(mtx2) is
  [matrix(3, 3): 1, 1, 1,
                 0, 2, 2, 
                 0, 0, 3]
  M.mtx-lower-triangle(mtx1) raises "Cannot make a non-square lower-triangular matrix"
  M.mtx-lower-triangle(mtx2) is
  [matrix(3,3): 1, 0, 0, 
                2, 2, 0, 
                3, 3, 3]

end

check "Matrix-to-List Conversion":

  mtx1.row-list() is [list: [matrix(1,3): 1, 2, 3], [matrix(1,3): 4, 5, 6]]
  mtx3.col-list() is [list: 
    [matrix(3,1): 1, 3, 5],
    [matrix(3,1): 2, 4, 6]]

  # Functional API
  M.mtx-row-list(mtx1) is [list: [matrix(1,3): 1, 2, 3], [matrix(1,3): 4, 5, 6]]
  M.mtx-col-list(mtx3) is [list: 
    [matrix(3,1): 1, 3, 5],
    [matrix(3,1): 2, 4, 6]]

end

check "Matrix Morphisms":

  mtx1.map(num-sqr) is 
  [matrix(2,3): 1,  4,  9,
               16, 25, 36]
  
  mtx1.row-map(lam(r): r.scale(2) end) is
  [matrix(2,3): 2,  4,  6,
                8, 10, 12]
  
  mtx1.col-map(lam(c): [matrix(1,1): c.dot(c)] end) is
  [matrix(1,3): 17, 29, 45]

  # Functional API
  M.mtx-map(num-sqr, mtx1) is 
  [matrix(2,3): 1,  4,  9,
               16, 25, 36]
  
  M.mtx-row-map(lam(r): r.scale(2) end, mtx1) is
  [matrix(2,3): 2,  4,  6,
                8, 10, 12]
  
  M.mtx-col-map(lam(c): [matrix(1,1): c.dot(c)] end, mtx1) is
  [matrix(1,3): 17, 29, 45]

end

check "Matrix Concatenation":

  mtx2.augment(mtx3) is
  [matrix(3,5): 1, 1, 1, 1, 2,
                2, 2, 2, 3, 4,
                3, 3, 3, 5, 6]
  
  mtx1.stack(mtx2) is
  [matrix(5, 3): 1, 2, 3,
                 4, 5, 6,
                 1, 1, 1,
                 2, 2, 2,
                 3, 3, 3]

  # Functional API
  M.mtx-augment(mtx2, mtx3) is
  [matrix(3,5): 1, 1, 1, 1, 2,
                2, 2, 2, 3, 4,
                3, 3, 3, 5, 6]
  
  M.mtx-stack(mtx1, mtx2) is
  [matrix(5, 3): 1, 2, 3,
                 4, 5, 6,
                 1, 1, 1,
                 2, 2, 2,
                 3, 3, 3]

end

check "Matrix Scaling and Trace":
  mtx1.scale(2) is [matrix(2,3): 2,  4,  6,
                                 8, 10, 12]
  
  mtx2.trace() is 6

  # Functional API
  M.mtx-scale(mtx1, 2) is [matrix(2,3): 2,  4,  6,
                                        8, 10, 12]
  
  M.mtx-trace(mtx2) is 6

end

check "Matrix Operations":

  (mtx1 + mtx1) is [matrix(2,3): 2,  4,  6,
                                 8, 10, 12]
  
  mtx1 + [matrix(2,3): 1, 1, 1,
                       1, 1, 1] is
  [matrix(2,3): 2, 3, 4,
                5, 6, 7]
  
  mtx1 - [matrix(2,3): 1, 1, 1,
                       1, 1, 1] is
  [matrix(2,3): 0, 1, 2, 
                3, 4, 5]  
  
  mtx1 * [matrix(3,2): 1, 1, 1,
                       1, 1, 1] is
  [matrix(2,2): 6,  6, 
               15, 15]
  
  mtx2.expt(2) is mtx2 * mtx2
  
  [matrix(3,3): 1, 1, 1, 
                1, 1, 1, 
                2, 1, 2].expt(3) is
  [matrix(3,3): 15, 11, 15,
                15, 11, 15, 
                26, 19, 26]

  # Functional API
  M.mtx-add(mtx1, mtx1) is [matrix(2,3): 2,  4,  6,
                                         8, 10, 12]
  
  M.mtx-add(mtx1, [matrix(2,3): 1, 1, 1,
                                1, 1, 1]) is
  [matrix(2,3): 2, 3, 4,
                5, 6, 7]
  
  M.mtx-sub(mtx1, [matrix(2,3): 1, 1, 1,
                                1, 1, 1]) is
  [matrix(2,3): 0, 1, 2, 
                3, 4, 5]  
  
  M.mtx-mult(mtx1, [matrix(3,2): 1, 1, 1,
                                 1, 1, 1]) is
  [matrix(2,2): 6,  6, 
               15, 15]
  
  M.mtx-expt(mtx2, 2) is M.mtx-mult(mtx2, mtx2)
  
  M.mtx-expt([matrix(3,3): 1, 1, 1, 
                           1, 1, 1, 
                           2, 1, 2], 3) is
  [matrix(3,3): 15, 11, 15,
                15, 11, 15, 
                26, 19, 26]
end

check "Matrix Determinant and Reduced-Row Echelon Form":

  [matrix(5,5): 1, 2,1,2, 3,
                2, 3,1,0, 1,
                2, 2,1,0, 0,
                1, 1,1,1, 1,
                0,-2,0,2,-2].determinant() is -2
  
  mtx1.rref() is
  [matrix(2,3): 1, 0, -1,
                0, 1,  2]

  # Functional API
  M.mtx-determinant([matrix(5,5): 1, 2,1,2, 3,
                                  2, 3,1,0, 1,
                                  2, 2,1,0, 0,
                                  1, 1,1,1, 1,
                                  0,-2,0,2,-2]) is -2
  
  M.mtx-rref(mtx1) is
  [matrix(2,3): 1, 0, -1,
                0, 1,  2]
end

check "Matrix Norms":

  mtx4.lp-norm(3) is%(within(0.00001)) num-expt(6, 2/3)
  mtx5.lp-norm(3) is%(within(0.00001)) (mtx5 * mtx4).lp-norm(3)
  
  mtx4.l1-norm() is%(within(0.00001)) 6
  mtx4.l2-norm() is%(within(0.00001)) num-sqrt(14)
  mtx4.l-inf-norm() is%(within(0.00001)) 3
  
  mtx5.l-inf-norm() is%(within(0.00001)) (mtx5 * mtx4).l-inf-norm()

  # Functional API
  M.mtx-lp-norm(mtx4, 3) is%(within(0.00001)) num-expt(6, 2/3)
  M.mtx-lp-norm(mtx5, 3) is%(within(0.00001)) M.mtx-lp-norm(mtx5 * mtx4, 3)
  
  M.mtx-l1-norm(mtx4) is%(within(0.00001)) 6
  M.mtx-l2-norm(mtx4) is%(within(0.00001)) num-sqrt(14)
  M.mtx-l-inf-norm(mtx4) is%(within(0.00001)) 3
  
  M.mtx-l-inf-norm(mtx5) is%(within(0.00001)) M.mtx-l-inf-norm((mtx5 * mtx4))

end

check "Matrix Decomposition":

  decomp = [matrix(3,3):1, 2, 3, -1, 0, -3, 0, -2, 3].qr-decomposition()

  decomp.Q is%(matrix-within(0.00001))
  [matrix(3,3):(1 / num-sqrt(2)), (1 / num-sqrt(6)), (1 / num-sqrt(3)),
    (-1 / num-sqrt(2)), (1 / num-sqrt(6)), (1 / num-sqrt(3)),
    0, (-2 / num-sqrt(6)), (1 / num-sqrt(3))]

  decomp.R is%(matrix-within(0.00001))
  [matrix(3,3): num-sqrt(2), num-sqrt(2), num-sqrt(18),
    0, num-sqrt(6), -1 * num-sqrt(6),
    0, 0, num-sqrt(3)]
  
  [matrix(3,3):1, 2, 3, -1, 0, -3, 0, -2, 3].gram-schmidt() is%(matrix-within(0.00001))
    [matrix(3,3):(1 / num-sqrt(2)), (1 / num-sqrt(6)), (1 / num-sqrt(3)),
    (-1 / num-sqrt(2)), (1 / num-sqrt(6)), (1 / num-sqrt(3)),
    0, (-2 / num-sqrt(6)), (1 / num-sqrt(3))]

  # Functional API
  func-decomp = M.mtx-qr-decomposition([matrix(3,3):1, 2, 3, -1, 0, -3, 0, -2, 3])
  func-decomp.Q is%(matrix-within(0.00001)) decomp.Q
  func-decomp.R is%(matrix-within(0.00001)) decomp.R
end

check "Alternative Matrix Constructors":

  [row-matrix: 1, 2, 3, 4] is [matrix(1,4): 1, 2, 3, 4]
  
  [col-matrix: 1, 2, 3, 4] is [matrix(4,1): 1, 2, 3, 4]

end

check "Matrix Inversion":

  [matrix(3,3): 1, 0, 4, 1, 1, 6, -3, 0, -10].inverse() is 
  [matrix(3,3): -5, 0, -2, -4, 1, -1, 3/2, 0, 1/2]

  # Functional API
  M.mtx-inverse([matrix(3,3): 1, 0, 4, 1, 1, 6, -3, 0, -10]) is 
  [matrix(3,3): -5, 0, -2, -4, 1, -1, 3/2, 0, 1/2]

end

check "Linear System Solvability":

  [matrix(3,2): 3, -6, 4, -8, 0, 1].least-squares-solve([matrix(3,1): -1, 7, 2]) is 
  [matrix(2,1): 5, 2]

  # Functional API
  M.mtx-least-squares-solve([matrix(3,2): 3, -6, 4, -8, 0, 1], [matrix(3,1): -1, 7, 2]) is 
  [matrix(2,1): 5, 2]

end


############################
#|
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
  [mat(2, 2): 1, 2, ~3, 4] == [mat(2, 2): 1, 2, ~5, 4] raises "" 
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
  get-row(a,0) is [MX.vector(3): 0, 0, 0]
  get-col(a,1) is [MX.vector(3): 0, 1,     2]
  get-row(a,3) raises ""
  get-col(a,-1) raises ""
  b = [mat(2,3):1,2,3,4,5,6]
  get-row(a,3) raises ""
end

check "vdot":
  v1 = [MX.vector(3): 1,3,5]
  v2 = [MX.vector(3): -2,-2,8/5]
  v3 = [MX.vector(3): -2418/13,-1/26,~0]
  vdot(v1,v1) is 35
  vdot(v1,v2) is 0 
  vdot(v2,v3) is-roughly num-to-roughnum(372 + (1/13))
end

check "conversions":
  MX.vector-to-list([MX.vector(5):13,41,5,1,1]) is [list:13,41,5,1,1]
  equal-now(MX.vector-to-array([MX.vector(5):13,41,5,1,1]) , [raw-array:13,41,5,1,1]) is true
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
  is-mat([MX.vector(2):14,4]) is false
  is-mat([list:13,5,235]) is false
  is-mat("EFsdF") is false
end

check "trace-mat":
  a = [mat(3,3):4,5,4,9,0,324,1,6,-7/23]
  trace-mat(a) is 85/23
  trace-mat(identity-mat(45)) is 45
end

check "gram-schmidt and qr" :
  a = [mat(3,3): 12,-51,4,6,167,-68,-4,24,-41]
  ans = [mat(3,3): 6/7, -69/175, -58/175, 3/7, 158/175, 6/175, -2/7, 6/35, -33/35]
  qr-mat(a)  is {ans;[mat(3,3): 14, 21, -14, 0, 175, -70, 0, 0, 35]}
end
check "MX.vector intilization":
  [MX.vector(3): 1,3,"ds"] raises " "
  [MX.vector(1.3):1.3] raises ""
  [MX.vector(~2):2,3] raises "" 
  [MX.vector(true): 1] raises " "
  [MX.vector("vs"):1] raises "" 
end

check "MX.vector equality":
  equal-now([MX.vector(3):-31/24,-5.6,12],[MX.vector(3):-31/24,-5.6,12]) is true
  equal-now([MX.vector(3):321,~5,-2],[MX.vector(3):312,~5,3]) is false
end
check "magnitude":
  magnitude([MX.vector(5):13/41,-123/37,42,10,9]) is-roughly ~44.2284
end
check "normalize":
  normalize([MX.vector(1):4]) is [MX.vector(1):1]
end

check "vscale":
  vscale([MX.vector(3):-12.42,~91/43,56],9.12) is [MX.vector(3):9.12 * -12.42,9.12 * ~91/43,9.12 * 56]
end

check "MX.vector operations":
  v1 = [MX.vector(3): 1,3,5]
  v2 = [MX.vector(3): -2,-2,8/5]
  v3 = [MX.vector(3): -2418/13,-1/26,~0]
  (v1 - v2) is [MX.vector(3): 3, 5, 3.4]
  (v2 + v3) is-roughly [MX.vector(3): -188, -53/26, ~1.6]
  (v1 * v3) is-roughly [MX.vector(3): -186, -3/26, ~0]
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
check "is-invertible":
  is-invertible(identity-mat(34)) is true
  is-invertible([mat(2,2):1,4,5,2]) is true
  is-invertible([mat(2,2):1,5,8,40]) is false
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


check "submatrix":
  a = build-mat(4,3,lam(i,j): i + j end)
  submatrix(a,1,1,2,2) is [mat(5,1): 2, 3, 2, 3, 4]
  submatrix([mat(2,2):1,~4,5/7,-9],0,0,1,1) is-roughly [mat(4,1):1,~4,5/7,-9]
end

check "lup-mat": 
  a = [mat(3,3): 3,4,5.5,7,0,-1,~3,43/7,32]
  b = lup-mat(a)
  l = b.{1}
  u = b.{2}
  p = b.{0}
  l is-roughly [mat(3,3): 1, 0, 0, 3/7, 1, 0, ~0.42857142857142855, 43/28, 1]
  u is-roughly [mat(3,3): 7, 0, -1, 0, 4, 83/14, ~0, 0, ~23.32397959183674]
  p is [mat(3,3): 0, 1, 0, 1, 0, 0, 0, 0, 1]
  (l * u) is-roughly (p * a) 
end

check "augment-mat stack-mat":
  a = [mat(2,2): 1,2,5,6]
  b = [mat(2,2):3,4,7,8]
  c = [mat(2,4):1,2,3,4,5,6,7,8]
  d = [mat(1,2): 5.5,6]
  augment-mat(a,b) is c
  augment-mat(a,d) raises " " 
  stack-mat(a,d) is [mat(3,2):1,2,5,6,5.5,6]

end

check "exp-mat":
  a = [mat(2,2):1,~4,-5,6.53]
  exp-mat(a,3) is-roughly [mat(2,2): ~-169.6, ~120.68360000000001, ~-150.8545, ~-2.7549229999999625]
  exp-mat(a,~2) raises "" 
  exp-mat(a,-2) raises "" 
  exp-mat(a,2/3) raises ""    
end

check "norms":
  a = [mat(3,3):1,5,~7/8,-9.42,0,0,89,100,17]
  frob-norm(a) is-roughly ~135.37171796575532
  norm-mat(a,1) is-roughly ~203.45499999999998
  norm-mat(a,2) is-roughly ~135.3717179657553
end

check "transpose":
  transpose(fill-mat(3,4,1)) is fill-mat(4,3,1)
  transpose(identity-mat(5)) is identity-mat(5)
  transpose([mat(2,2):1,5,5,9]) is [mat(2,2):1,5,5,9]
  transpose([mat(2,2):~5,7/8,8,91]) is-roughly [mat(2,2):~5,8,7/8,91]
end

check "scale-mat":
  scale-mat(fill-mat(3,3,5.6),3) is fill-mat(3,3,5.6 * 3)
  scale-mat([mat(2,2):1,2,3,4],5) is [mat(2,2):5,10,15,20]
end

check "dims-mat":
  dims-mat(emp) is {0;0}
  dims-mat([mat(3,2):1,42,3,53,2,5]) is {3;2}
end

check "build-mat":
  build-mat(4,5,lam(i,j): i * j end) is [mat(4,5): 0, 0, 0, 0, 0, 0, 1, 2, 3, 4, 0, 2, 4, 6, 8, 0, 3, 6, 9, 12]
  build-mat(3,2,lam(i,j): "hello" end) raises "" 
  build-mat(2,2,lam(i,j): ~3 end) is-roughly [mat(2,2):~3,~3,~3,~3]
end

|#
