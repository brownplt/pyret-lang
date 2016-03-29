import matrices as M
import all2 from lists

matrix = M.matrix
matrix-within = M.matrix-within
row-matrix = M.row-matrix
col-matrix = M.col-matrix

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
  
  mtx1.row(0).to-vector() is [list: 1, 2, 3]
  mtx1.col(1).to-vector() is [list: 2, 5]

end

check "Submatrices":

  mtx1.submatrix([list: 0, 1], [list: 0, 2]) is
  [matrix(2,2): 1, 3, 
                4, 6]
  mtx1.submatrix([list: 1], [list: 0, 1, 2]) is
  [matrix(1,3): 4, 5, 6]
  mtx1.submatrix([list: 0], [list: 1, 2]) is
  [matrix(1,2): 2, 3]
end

check "Matrix Transpose":
  
  mtx1.transpose() is 
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

end

check "Matrix-to-List Conversion":

  mtx1.row-list() is [list: [matrix(1,3): 1, 2, 3], [matrix(1,3): 4, 5, 6]]
  mtx3.col-list() is [list: 
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

end

check "Matrix Scaling and Trace":
  mtx1.scale(2) is [matrix(2,3): 2,  4,  6,
                                 8, 10, 12]
  
  mtx2.trace() is 6

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
end

check "Matrix Norms":

  mtx4.lp-norm(3) is%(within(0.00001)) num-expt(6, 2/3)
  mtx5.lp-norm(3) is%(within(0.00001)) (mtx5 * mtx4).lp-norm(3)
  
  mtx4.l1-norm() is%(within(0.00001)) 6
  mtx4.l2-norm() is%(within(0.00001)) num-sqrt(14)
  mtx4.l-inf-norm() is%(within(0.00001)) 3
  
  mtx5.l-inf-norm() is%(within(0.00001)) (mtx5 * mtx4).l-inf-norm()

end

check "Matrix Decomposition":

  [matrix(3,3):1, 2, 3, -1, 0, -3, 0, -2, 3].qr-decomposition() is%(list-matrix-within(0.00001))
  [list: 
    [matrix(3,3):(1 / num-sqrt(2)), (1 / num-sqrt(6)), (1 / num-sqrt(3)),
      (-1 / num-sqrt(2)), (1 / num-sqrt(6)), (1 / num-sqrt(3)),
      0, (-2 / num-sqrt(6)), (1 / num-sqrt(3))],
    [matrix(3,3): num-sqrt(2), num-sqrt(2), num-sqrt(18),
      0, num-sqrt(6), -1 * num-sqrt(6),
      0, 0, num-sqrt(3)]]
  
  [matrix(3,3):1, 2, 3, -1, 0, -3, 0, -2, 3].gram-schmidt() is%(matrix-within(0.00001))
    [matrix(3,3):(1 / num-sqrt(2)), (1 / num-sqrt(6)), (1 / num-sqrt(3)),
      (-1 / num-sqrt(2)), (1 / num-sqrt(6)), (1 / num-sqrt(3)),
      0, (-2 / num-sqrt(6)), (1 / num-sqrt(3))]
end

check "Alternative Matrix Constructors":

  [row-matrix: 1, 2, 3, 4] is [matrix(1,4): 1, 2, 3, 4]
  
  [col-matrix: 1, 2, 3, 4] is [matrix(4,1): 1, 2, 3, 4]

end

check "Matrix Inversion":

  [matrix(3,3): 1, 0, 4, 1, 1, 6, -3, 0, -10].inverse() is 
  [matrix(3,3): -5, 0, -2, -4, 1, -1, 3/2, 0, 1/2]

end

check "Linear System Solvability":

  [matrix(3,2): 3, -6, 4, -8, 0, 1].least-squares-solve([matrix(3,1): -1, 7, 2]) is 
  [matrix(2,1): 5, 2]

end