include matrices
import lists as L

rec random-matrix = {
  make: lam(_): raise("Invalid matrix-config size") end,
  make0: lam(): raise("Invalid matrix-config size") end,
  make1: lam(_): raise("Invalid matrix-config size") end,
  make2: lam(rows, cols): random-matrix.make4(rows, cols, 0, 100) end,
  make3: lam(rows, cols, high): random-matrix.make4(rows, cols, 0, high) end,
  make4: lam(rows, cols, low, high):
      build-matrix(rows, cols, {(_, _): num-random(high - low) + low})
    end,
  make5: lam(_, _, _, _, _): raise("Invalid matrix-config size") end
}

fun index-matrix(rows, cols):
  build-matrix(rows, cols, {(r, c): (r * cols) + c})
end

fun matrix-q(m): m.qr-decomposition().Q end

check "construction of matrices":
  lists-to-matrix([list: [list: 1]]) is [matrix(1,1): 1]
  lists-to-matrix([list: [list: 1, 2, 3, 4]]) is [matrix(1, 4): 1, 2, 3, 4]
  lists-to-matrix([list: [list: 1, 2], [list: 3, 4]]) is [matrix(2, 2): 1, 2, 3, 4]
  lists-to-matrix([list: [list: 1], [list: 2], [list: 3], [list: 4]]) is [matrix(4, 1): 1, 2, 3, 4]
  [row-matrix: 1, 2, 3, 4] is [matrix(1, 4): 1, 2, 3, 4]
  [col-matrix: 1, 2, 3, 4] is [matrix(4, 1): 1, 2, 3, 4]
  lists-to-matrix(empty) raises ""
end

# ==================================================================================================
#  Predicates
check "Type predicates":
  is-matrix([matrix(1,1): 1]) is true
  is-matrix([raw-array: 1]) is false
  is-matrix(1) is false
end

check "Shape predicates":
  is-square-matrix([matrix(1,1): 1]) is true
  is-square-matrix([matrix(2,2): 1, 1, 1, 1]) is true
  is-square-matrix([matrix(1,2): 1, 2]) is false
  is-square-matrix([matrix(2,1): 1, 2]) is false
  is-square-matrix([raw-array: 1,2]) raises ""
  is-square-matrix(2) raises ""


  is-row-matrix([matrix(1, 4): 1, 2, 3, 4]) is true
  is-row-matrix([matrix(1, 1): 1]) is true
  is-row-matrix([matrix(4, 1): 1, 2, 3, 4]) is false
  is-row-matrix([raw-array: 1,2]) raises ""
  is-row-matrix(2) raises ""

  is-col-matrix([matrix(4, 1): 1, 2, 3, 4]) is true
  is-col-matrix([matrix(1, 1): 1]) is true
  is-col-matrix([matrix(1, 4): 1, 2, 3, 4]) is false
  is-col-matrix([raw-array: 1,2]) raises ""
  is-col-matrix(2) raises ""
end


# ==================================================================================================
#  Constructors

#  identity-matrix

check "Identity matrix":
  identity-matrix(1) is [matrix(1, 1): 1]
  identity-matrix(2) is [matrix(2, 2): 1, 0, 0, 1]
  identity-matrix(3)
    is [matrix(3, 3):
    1, 0, 0,
    0, 1, 0,
    0, 0, 1]
  identity-matrix(0) raises ""
  identity-matrix(2.5) raises ""
end

check "make-matrix":
  make-matrix(1, 1, 4) is [matrix(1, 1): 4]
  make-matrix(2, 2, 3) is [matrix(2, 2): 3, 3, 3, 3]
  make-matrix(1, 0, 4) raises ""
  make-matrix(0, 1, 4) raises ""
end

check "build-matrix":
  build-matrix(4, 4, lam(r, c): r + c end)
    is lists-to-matrix(range(0, 4).map(lam(r): range(0, 4).map(lam(c): r + c end) end))

  build-matrix(1, 0, num-expt) raises ""
  build-matrix(0, 1, num-expt) raises ""
end

check "diagonal-matrix":
  diagonal-matrix([list: 1, 2, 3, 4])
    is [matrix(4, 4):
    1, 0, 0, 0,
    0, 2, 0, 0,
    0, 0, 3, 0,
    0, 0, 0, 4]

  diagonal-matrix(empty) raises ""
end

#|
#  block-diagonal-matrix

(let ([m  (random-matrix 4 4 100)])
  (check-equal? (block-diagonal-matrix (list m))
                m))

(check-equal?
 (block-diagonal-matrix
  (list (matrix [[1 2] [3 4]])
        (matrix [[1 2 3] [4 5 6]])
        (matrix [[1] [3] [5]])
        (matrix [[2 4 6]])))
 (matrix [[1 2 0 0 0 0 0 0 0]
          [3 4 0 0 0 0 0 0 0]
          [0 0 1 2 3 0 0 0 0]
          [0 0 4 5 6 0 0 0 0]
          [0 0 0 0 0 1 0 0 0]
          [0 0 0 0 0 3 0 0 0]
          [0 0 0 0 0 5 0 0 0]
          [0 0 0 0 0 0 2 4 6]]))

(check-equal?
 (block-diagonal-matrix (map (λ: ([i : Integer]) (matrix [[i]])) '(1 2 3 4)))
 (diagonal-matrix '(1 2 3 4)))

(check-exn exn:fail:contract? (λ () (block-diagonal-matrix '())))

#  Vandermonde matrix

(check-equal? (vandermonde-matrix '(10) 1)
              (matrix [[1]]))
(check-equal? (vandermonde-matrix '(10) 4)
              (matrix [[1 10 100 1000]]))
(check-equal? (vandermonde-matrix '(1 2 3 4) 3)
              (matrix [[1 1 1] [1 2 4] [1 3 9] [1 4 16]]))
(check-exn exn:fail:contract? (λ () (vandermonde-matrix '() 1)))
(check-exn exn:fail:contract? (λ () (vandermonde-matrix '(1) 0)))
|#
   
# ==================================================================================================
check "Flat conversion":
  list-to-matrix(1, 3, [list: 1, 2, 3]) is [row-matrix: 1, 2, 3]
  list-to-matrix(3, 1, [list: 1, 2, 3]) is [col-matrix: 1, 2, 3]
  list-to-matrix(0, 1, empty) raises ""
  list-to-matrix(1, 0, empty) raises ""
  list-to-matrix(1, 1, [list: 1, 2]) raises ""

  mtx-to-list([matrix(2, 3): 1, 2, 3, 4, 5, 6]) is [list: 1, 2, 3, 4, 5, 6]
  mtx-to-list([row-matrix: 1, 2, 3]) is [list: 1, 2, 3]
  mtx-to-list([col-matrix: 1, 2, 3]) is [list: 1, 2, 3]
end

# ==================================================================================================
check "Nested conversion":
  lists-to-matrix([list: [list: 1, 2, 3], [list: 4, 5, 6]]) is [matrix(2, 3): 1, 2, 3, 4, 5, 6]
  lists-to-matrix([list: [list: 1, 2, 3], [list: 4, 5]]) raises ""
  lists-to-matrix([list: empty, empty, empty]) raises ""
  lists-to-matrix(empty) raises ""

  mtx-to-lists([matrix(2, 3): 1, 2, 3, 4, 5, 6]) is [list: [list: 1, 2, 3], [list: 4, 5, 6]]
end

# ==================================================================================================
check "Equality":
  matrix-eq = within(0.0001)
  [matrix(2, 3): 1, 2, 3, 4, 5, 6] == [matrix(2, 3): ~1, ~2, ~3, ~4, ~5, ~6] raises ""
  matrix-eq([matrix(2, 3): 1, 2, 3, 4, 5, 6], [matrix(2, 3): ~1, ~2, ~3, ~4, ~5, ~6]) is true
  matrix-eq([matrix(2, 3): 1, 2, 3, 4, 5, 6], [matrix(2, 3): 1, 2, 3, 4, 5, 7]) is false
  matrix-eq([matrix(2, 3): 0, 2, 3, 4, 5, 6], [matrix(2, 3): 1, 2, 3, 4, 5, 6]) is false
  matrix-eq([matrix(2, 3): 1, 2, 3, 4, 5, 6], [matrix(3, 2): 1, 2, 3, 4, 5, 6]) is false
end

# ==================================================================================================
#  Pointwise operations

check "Pointwise operations":
  fun check-fun(f-elt1, f-elt2):
    for each(rows from [list: 2, 3, 4]):
      for each(cols from [list: 2, 3, 4]) block:
        a0 = [random-matrix: rows, cols]
        a1 = [random-matrix: rows, cols]
        mtx-map(f-elt1, a0).to-list() is-roughly L.map(f-elt1, a0.to-list())
        mtx-map2(f-elt2, a0, a1).to-list() is-roughly L.map2(f-elt2, a0.to-list(), a1.to-list())
      end
    end
  end
  check-fun(num-sqr, num-expt)
  check-fun(num-sqrt, lam(a, b): a - b end)

  [matrix(2, 3): 1, 2, 3, 4, 5, 6] + [matrix(2, 3): 0, 1, 2, 3, 4, 5] is [matrix(2, 3): 1, 3, 5, 7, 9, 11]
  mtx-add([matrix(2, 3): 1, 2, 3, 4, 5, 6], [matrix(2, 3): 0, 1, 2, 3, 4, 5]) is [matrix(2, 3): 1, 3, 5, 7, 9, 11]

  [matrix(2, 3): 1, 2, 3, 4, 5, 6] - [matrix(2, 3): 0, 1, 2, 3, 4, 5] is [matrix(2, 3): 1, 1, 1, 1, 1, 1]
  mtx-sub([matrix(2, 3): 1, 2, 3, 4, 5, 6], [matrix(2, 3): 0, 1, 2, 3, 4, 5]) is [matrix(2, 3): 1, 1, 1, 1, 1, 1]

  mtx-scale([matrix(2, 3): 1, 2, 3, 4, 5, 6], 10) is [matrix(2, 3): 10, 20, 30, 40, 50, 60]
end

# ==================================================================================================
#  Multiplication

check "Matrix multiplication":
  m19 = [matrix(3, 3): 1, 2, 3, 4, 5, 6, 7, 8, 9]
  r14 = [row-matrix: 1, 2, 3, 4]
  c14 = [col-matrix: 1, 2, 3, 4]

  m19 * m19 is [matrix(3, 3):
    30, 36, 42,
    66, 81, 96,
    102, 126, 150]

  r14 * c14 is [matrix(1,1): 30]
  c14 * r14 is [matrix(4, 4):
    1, 2, 3, 4,
    2, 4, 6, 8,
    3, 6, 9, 12,
    4, 8, 12, 16]

  [matrix(1,1): 3] * [matrix(1,1): 7] is [matrix(1,1): 21]

  #  Left/right identity
  m = [random-matrix: 2, 2]
  identity-matrix(2) * m is m
  m * identity-matrix(2) is m

  m0 = [random-matrix: 4, 5]
  m1 = [random-matrix: 5, 2]
  m2 = [random-matrix: 2, 10]

  m0m1 = m0 * m1
  m1m2 = m1 * m2
  m0m1m2 = m0 * m1m2

  m0m1.rows is 4
  m0m1.cols is 2
  m1m2.rows is 5
  m1m2.cols is 10
  m0m1m2.rows is 4
  m0m1m2.cols is 10

  (m0 * m2) raises ""

  m0 * (m1 * m2) is (m0 * m1) * m2
end

# ==================================================================================================
check "Exponentiation":
  A = [matrix(2, 2): 1, 2, 3, 4]
  mtx-expt(A, 0) is identity-matrix(2)
  mtx-expt(A, 1) is A
  mtx-expt(A, 2) is [matrix(2, 2): 7, 10, 15, 22] because A * A
  mtx-expt(A, 3) is [matrix(2, 2): 37, 54, 81, 118] because A * A * A
  mtx-expt(A, 8) is [matrix(2, 2): 165751, 241570, 362355, 528106]
    because A * A * A * A * A * A * A * A
  mtx-expt([row-matrix: 1, 2, 3, 4], 1) raises ""
  mtx-expt([col-matrix: 1, 2, 3, 4], 1) raises ""
  mtx-expt([row-matrix: 1, 2, 3, 4], 1.5) raises ""
  mtx-expt([row-matrix: 1, 2, 3, 4], -1) raises ""
  mtx-expt([row-matrix: 1, 2, 3, 4], ~1) raises ""

  mtx-expt([matrix(1,1): 2], 10) is [matrix(1,1): num-expt(2, 10)]
end

# ==================================================================================================
#  Kronecker product

# (check-equal? (matrix-kronecker (matrix [[1 2]
#                                          [3 4]
#                                          [5 6]])
#                                 (matrix [[7 8]
#                                          [9 10]]))
#               (matrix [[7 8 14 16]
#                        [9 10 18 20]
#                        [21 24 28 32]
#                        [27 30 36 40]
#                        [35 40 42 48]
#                        [45 50 54 60]]))

# ==================================================================================================
#  Comprehensions

# #  for:/matrix and friends are defined in terms of for:/array and friends, so we only need to test
# #  that it works for one case each, and that they properly raise exceptions when given zero-length
# #  axes

# (check-equal?
#  (for/matrix: 2 2 ([i  (in-range 4)]) i)
#  (matrix [[0 1] [2 3]]))

# (check-equal?
#  (for*/matrix: 2 2 ([i  (in-range 2)] [j  (in-range 2)]) (+ i j))
#  (matrix [[0 1] [1 2]]))

# (check-exn exn:fail:contract? (λ () (for/matrix: 2 0 () 0)))
# (check-exn exn:fail:contract? (λ () (for/matrix: 0 2 () 0)))
# (check-exn exn:fail:contract? (λ () (for*/matrix: 2 0 () 0)))
# (check-exn exn:fail:contract? (λ () (for*/matrix: 0 2 () 0)))

# ==================================================================================================
check "Extraction":
  A = [matrix(2, 2): 10, 11, 12, 13]
  mtx-get(A, 0, 0) is 10
  mtx-get(A, 0, 1) is 11
  mtx-get(A, 1, 0) is 12
  mtx-get(A, 1, 1) is 13
  mtx-get(A, 2, 0) raises ""
  mtx-get(A, 0, 2) raises ""
  mtx-get(A, ~1, 1) raises ""
  mtx-get(A, 0, -1) raises ""
  mtx-get(A, 0, 0.5) raises ""

  mtx-diagonal(A) is [row-matrix: 10, 13]
  mtx-diagonal(diagonal-matrix([list: 1, 2, 3, 4])) is [row-matrix: 1, 2, 3, 4]

  mtx-submatrix(identity-matrix(8), range(2, 4), range(2, 4)) is identity-matrix(2)
end

check "matrix-row":
  A = [matrix(2, 3): 1, 2, 3, 4, 5, 6]
  mtx-row(A, 0) is [row-matrix: 1, 2, 3]
  mtx-row(A, 1) is [row-matrix: 4, 5, 6]
  mtx-row(A, -1) raises ""
  mtx-row(A, 2) raises ""
  mtx-row(A, 1.5) raises ""
  mtx-row(A, ~1) raises ""
end

check "matrix-col":
  A = [matrix(2, 3): 1, 2, 3, 4, 5, 6]
  mtx-col(A, 0) is [col-matrix: 1, 4]
  mtx-col(A, 1) is [col-matrix: 2, 5]
  mtx-col(A, 2) is [col-matrix: 3, 6]
  mtx-col(A, -1) raises ""
  mtx-col(A, 3) raises ""
  mtx-col(A, 1.5) raises ""
  mtx-col(A, ~1) raises ""
end

check "matrix-row-list and matrix-col-list":
  mtx-row-list([matrix(2, 3): 1, 2, 3, 4, 5, 6])
    is [list: [row-matrix: 1, 2, 3], [row-matrix: 4, 5, 6]]
  mtx-col-list([matrix(2, 3): 1, 2, 3, 4, 5, 6])
    is [list: [col-matrix: 1, 4], [col-matrix: 2, 5], [col-matrix: 3, 6]]
end

# (for: ([a  (in-list nonmatrices)])
#   (check-exn exn:fail:contract? (λ () (matrix-cols a))))

#  TODO: matrix-upper-triangle

#  TODO: matrix-lower-triangle

# # ==================================================================================================
# #  Embiggenment (it's a perfectly cromulent word)

# #  matrix-augment

# (let ([a  (random-matrix 3 5)])
#   (check-equal? (matrix-augment (list a)) a)
#   (check-equal? (matrix-augment (matrix-cols a)) a))

# (check-equal? (matrix-augment (list (col-matrix [1 2 3]) (col-matrix [4 5 6])))
#               (matrix [[1 4] [2 5] [3 6]]))

# (check-equal? (matrix-augment (list (matrix [[1 2] [4 5]]) (col-matrix [3 6])))
#               (matrix [[1 2 3] [4 5 6]]))

# (check-exn exn:fail? (λ () (matrix-augment (list (matrix [[1 2] [4 5]]) (col-matrix [3])))))
# (check-exn exn:fail:contract? (λ () (matrix-augment '())))
# (for: ([a  (in-list nonmatrices)])
#   (check-exn exn:fail:contract? (λ () (matrix-augment (list a))))
#   (check-exn exn:fail:contract? (λ () (matrix-augment (list (matrix [[1]]) a)))))

# #  matrix-stack

# (let ([a  (random-matrix 5 3)])
#   (check-equal? (matrix-stack (list a)) a)
#   (check-equal? (matrix-stack (matrix-rows a)) a))

# (check-equal? (matrix-stack (list (row-matrix [1 2 3]) (row-matrix [4 5 6])))
#               (matrix [[1 2 3] [4 5 6]]))

# (check-equal? (matrix-stack (list (matrix [[1 2 3] [4 5 6]]) (row-matrix [7 8 9])))
#               (matrix [[1 2 3] [4 5 6] [7 8 9]]))

# (check-exn exn:fail? (λ () (matrix-stack (list (matrix [[1 2 3] [4 5 6]]) (row-matrix [7 8])))))
# (check-exn exn:fail:contract? (λ () (matrix-stack '())))
# (for: ([a  (in-list nonmatrices)])
#   (check-exn exn:fail:contract? (λ () (matrix-stack (list a))))
#   (check-exn exn:fail:contract? (λ () (matrix-stack (list (matrix [[1]]) a)))))

# # ==================================================================================================
# #  Setters

# (let ([sys (matrix [[1 2 3]
#                     [4 5 6]
#                     [7 8 9]])]
#       [new-col (col-matrix [-1 -2 -3])])
#   (check-equal? (matrix-set-col sys 0 new-col)
#                 (array #[#[-1 2 3] #[-2 5 6] #[-3 8 9]])))

# (let ([sys (matrix [[1 2 3]
#                     [4 5 6]
#                     [7 8 9]])]
#       [new-row (row-matrix [-1 -2 -3])])
#   (check-equal? (matrix-set-row sys 0 new-row)
#                 (array #[#[-1 -2 -3] #[4 5 6] #[7 8 9]])))

# ==================================================================================================
#  Inner product space

check "matrix-norm":
  mtx-l2-norm([matrix(2, 3): 1, 2, 3, 4, 5, 6])
    is-roughly num-sqrt((1 * 1) + (2 * 2) + (3 * 3) + (4 * 4) + (5 * 5) + (6 * 6))

  mtx-euclidean-norm([matrix(2, 3): 1, 2, 3, 4, 5, 6])
    is-roughly mtx-l2-norm([matrix(2, 3): 1, 2, 3, 4, 5, 6])

  mtx-l1-norm([matrix(2, 3): 1, 2, 3, 4, 5, 6]) is 1 + 2 + 3 + 4 + 5 + 6
  mtx-taxicab-norm([matrix(2, 3): 1, 2, 3, 4, 5, 6])
    is mtx-l1-norm([matrix(2, 3): 1, 2, 3, 4, 5, 6])

  mtx-l-inf-norm([matrix(2, 3): 1, 2, 3, 4, 5, 6]) is 6
  mtx-lp-norm([matrix(2, 3): 1, 2, 3, 4, 5, 6], 100) is-roughly 6.0
  # original test used 1000 instead of 100, but that overflows in Pyret for now

  
  
# this currently overflows in Pyret due to use of num-expt(_, 0.5) instead of num-sqrt
# #  This shouldn't overflow (so we check against `flhypot', which also shouldn't overflow)
# (check-equal? (matrix-norm (matrix [[1e200 1e199]]))
#               (flhypot 1e200 1e199))

  mtx-dot([matrix(2, 3): 1, -2, 3, -4, 5, -6], [matrix(2, 3): -1, 2, -3, 4, -5, 6])
    is (1 * -1) + (-2 * 2) + (3 * -3) + (-4 * 4) + (5 * -5) + (-6 * 6)

  mtx-dot([random-matrix: 1, 3], [random-matrix: 3, 1]) raises ""
end

#  TODO: matrix-angle

#  TODO: matrix-normalize

# ==================================================================================================
#  Simple operators

check "matrix-transpose":
  mtx-transpose([matrix(2, 3): 1, 2, 3, 4, 5, 6]) is [matrix(3, 2): 1, 4, 2, 5, 3, 6]
end

# #  matrix-conjugate

# (check-equal? (matrix-conjugate (matrix [[1+i 2-i] [3+i 4-i]]))
#               (matrix [[1-i 2+i] [3-i 4+i]]))

# (for: ([a  (in-list nonmatrices)])
#   (check-exn exn:fail:contract? (λ () (matrix-conjugate a))))

# #  matrix-hermitian

# (let ([a  (array-make-rectangular (random-matrix 5 6 -100 100)
#                                   (random-matrix 5 6 -100 100))])
#   (check-equal? (matrix-hermitian a)
#                 (matrix-conjugate (matrix-transpose a)))
#   (check-equal? (matrix-hermitian a)
#                 (matrix-transpose (matrix-conjugate a))))

# (for: ([a  (in-list nonmatrices)])
#   (check-exn exn:fail:contract? (λ () (matrix-hermitian a))))

check "matrix-trace":
  mtx-trace([matrix(3, 3): 1, 2, 3, 4, 5, 6, 7, 8, 9]) is 1 + 5 + 9
  mtx-trace([row-matrix: 1, 2, 3]) raises ""
  mtx-trace([col-matrix: 1, 2, 3]) raises ""
end

# ==================================================================================================
#  Row/column operators

#  TODO: matrix-map-rows

#  TODO: matrix-map-cols

#  TODO: matrix-normalize-rows

#  TODO: matrix-normalize-cols

# ==================================================================================================
#  Operator norms

#  TODO: matrix-op-1norm

#  TODO: matrix-op-2norm (after it's implemented)

#  TODO: matrix-op-inf-norm

# ==================================================================================================
#  Error

# (for*: ([x  (in-list '(-inf.0 -10.0 -1.0 -0.25 -0.0 0.0 0.25 1.0 10.0 +inf.0 +nan.0))]
#         [y  (in-list '(-inf.0 -10.0 -1.0 -0.25 -0.0 0.0 0.25 1.0 10.0 +inf.0 +nan.0))])
#   (check-eqv? (fl (matrix-absolute-error (row-matrix [x])
#                                          (row-matrix [y])))
#               (fl (absolute-error x y))
#               (format "x = ~v  y = ~v" x y))
#   (check-eqv? (fl (matrix-relative-error (row-matrix [x])
#                                          (row-matrix [y])))
#               (fl (relative-error x y))
#               (format "x = ~v  y = ~v" x y)))

# (check-equal? (matrix-absolute-error (row-matrix [1 2])
#                                      (row-matrix [1 2]))
#               0)

# (check-equal? (matrix-absolute-error (row-matrix [1 2])
#                                      (row-matrix [2 2]))
#               1)

# (check-equal? (matrix-absolute-error (row-matrix [1 2])
#                                      (row-matrix [2 +nan.0]))
#               +inf.0)

# (check-equal? (matrix-relative-error (row-matrix [1 2])
#                                      (row-matrix [1 2]))
#               0)

# (check-equal? (matrix-relative-error (row-matrix [1 2])
#                                      (row-matrix [2 2]))
#               (/ 1 (matrix-op-inf-norm (row-matrix [2 2]))))

# (check-equal? (matrix-relative-error (row-matrix [1 2])
#                                      (row-matrix [2 +nan.0]))
#               +inf.0)

#  TODO: matrix-basis-angle

# ==================================================================================================
#  Approximate predicates

#  matrix-zero? (TODO: approximations)

check "matrix-zero?":
  mtx-is-roughly-zero(make-matrix(4, 3, 0)) is true
  mtx-is-roughly-zero(make-matrix(4, 3, ~0)) is true
  mtx-is-roughly-zero([row-matrix: 0, 0, 0, 0, 0, 1]) is false
end

#  TODO: matrix-rows-orthogonal?

#  TODO: matrix-cols-orthogonal?

#  TODO: matrix-identity?

#  TODO: matrix-orthonormal?

# ==================================================================================================
#  Gaussian elimination

check "Gaussian elimination":
  mtx-row-echelon([matrix(2,2): 2, 4, 3, 4], false, false, first-nonzero-element)
    is [matrix(2,2): 2, 4, 0, -2]
  mtx-row-echelon([matrix(2,2): 2, 4, 3, 4], false, false, largest-element)
    is [matrix(2,2): 3, 4, 0, 4/3]
  mtx-row-echelon([matrix(2,2): 2, 4, 3, 4], false, true, largest-element)
    is [matrix(2,2): 1, 4/3, 0, 1]

  mtx-row-echelon([matrix(2,2): 1, 2, 2, 4], false, false, largest-element)
    is [matrix(2,2): 2, 4, 0, 0]
  mtx-row-echelon([matrix(2,2): 1, 4, 2, 4], false, true, largest-element)
    is [matrix(2,2): 1, 2, 0, 1]

  mtx-row-echelon([matrix(3,4):
      2, 1, -1, 8,
      -3, -1, 2, -11,
      -2, 1, 2, -3], false, true, largest-element)
    is [matrix(3,4):
    1, 1/3, -2/3, 11/3,
    0, 1, 2/5, 13/5,
    0, 0, 1, -1]

  mtx-row-echelon([matrix(3,4):
      2, 1, -1, 8,
      -3, -1, 2, -11,
      -2, 1, 2, -3], true, true, largest-element)
    is [matrix(3,4):
    1, 0, 0, 2,
    0, 1, 0, 3,
    0, 0, 1, -1]

  mtx-row-echelon([matrix(3,4):
      2, 1, -1, 8,
      -3, -1, 2, -11,
      -2, 1, 2, -3], true, true, first-nonzero-element)
    is [matrix(3,4):
    1, 0, 0, 2,
    0, 1, 0, 3,
    0, 0, 1, -1]

end

check "matrix-rank":
  mtx-rank([matrix(2,2): 0, 0, 0, 0]) is 0
  mtx-rank([matrix(2,2): 1, 0, 0, 0]) is 1
  mtx-rank([matrix(2,2): 1, 0, 0, 3]) is 2
  mtx-rank([matrix(2,2): 1, 2, 2, 4]) is 1
  mtx-rank([matrix(2,2): 1, 2, 3, 4]) is 2
  mtx-rank([matrix(1, 3): 1, 2, 3]) is 1
  mtx-rank([matrix(2, 3): 1, 2, 3, 2, 3, 5]) is 2
  mtx-rank([matrix(3, 3): 1, 2, 3, 2, 3, 5, 3, 4, 7]) is 2
  mtx-rank([matrix(4, 3): 1, 2, 3, 2, 3, 5, 3, 4, 7, 4, 5, 9]) is 2
  mtx-rank([matrix(2, 4): 1, 2, 3, 4, 2, 3, 5, 8]) is 2
  mtx-rank([matrix(2, 4): 1, 5, 2, 3, 2, 8, 3, 5]) is 2
end

check "matrix-nullity":
  mtx-nullity([matrix(2,2): 0, 0, 0, 0]) is 2
  mtx-nullity([matrix(2,2): 1, 0, 0, 0]) is 1
  mtx-nullity([matrix(2,2): 1, 0, 0, 3]) is 0
  mtx-nullity([matrix(2,2): 1, 2, 2, 4]) is 1
  mtx-nullity([matrix(2,2): 1, 2, 3, 4]) is 0
  mtx-nullity([matrix(1, 3): 1, 2, 3]) is 2
  mtx-nullity([matrix(2, 3): 1, 2, 3, 2, 3, 5]) is 1
  mtx-nullity([matrix(3, 3): 1, 2, 3, 2, 3, 5, 3, 4, 7]) is 1
  mtx-nullity([matrix(4, 3): 1, 2, 3, 2, 3, 5, 3, 4, 7, 4, 5, 9]) is 1
  mtx-nullity([matrix(2, 4): 1, 2, 3, 4, 2, 3, 5, 8]) is 2
  mtx-nullity([matrix(2, 4): 1, 5, 2, 3, 2, 8, 3, 5]) is 2
end

# ==================================================================================================

check "Determinant":
  mtx-determinant([matrix(1,1): 3]) is 3
  mtx-determinant([matrix(2,2): 1, 2, 3, 4]) is (1 * 4) - (2 * 3)
  mtx-determinant([matrix(3,3): 1, 2, 3, 4, 5, 6, 7, 8, 9]) is 0
  mtx-determinant([matrix(3,3): 1, 2, 3, 4, -5, 6, 7, 8, 9]) is 120
  mtx-determinant([matrix(4,4): 1, 2, 3, 4, -5, 6, 7, 8, 9, 10, -11, 12, 13, 14, 15, 16]) is 5280

  mtx-determinant([matrix(2,3): 1, 2, 3, 4, 5, 6]) raises ""
  mtx-determinant([matrix(3,2): 1, 2, 3, 4, 5, 6]) raises ""
end

# ==================================================================================================
check "Solving linear systems":
  for each(_ from range(0, 100)):
    M = [random-matrix: 3, 3, -3, 4]
    B = [random-matrix: 3, 1 + num-random(10), -3, 4]
    if mtx-is-invertible(M):
      X = mtx-solve(M, B)
      M * X is-roughly B
    else:
      mtx-solve(M, B) raises ""
    end
  end

  mtx-solve([random-matrix: 3, 4], [random-matrix: 3, 1]) raises ""
  mtx-solve([random-matrix: 4, 3], [random-matrix: 4, 1]) raises ""
end


# ==================================================================================================
check "Inversion":
  ident = identity-matrix(3)
  for each(_ from range(0, 100)):
    A = [random-matrix: 3, 3, -3, 4]
    if mtx-is-invertible(A) block:
      A-inv = mtx-inverse(A)
      A * A-inv is-roughly ident
      A-inv * A is-roughly ident
    else:
      mtx-inverse(A) raises ""
    end
  end

  mtx-inverse([random-matrix: 3, 4]) raises ""
  mtx-inverse([random-matrix: 4, 3]) raises ""
end

# ==================================================================================================
check "LU decomposition":
  M = [matrix(4,4):
    1, 1, 0, 3,
    2, 1, -1, 1,
    3, -1, -1, 2,
    -1, 2, 3, -1]
  LU = mtx-lu-decomposition(M)
  LU.L is [matrix(4,4):
    1, 0, 0, 0,
    2, 1, 0, 0,
    3, 4, 1, 0,
    -1, -3, 0, 1]
  LU.U is [matrix(4,4):
    1, 1, 0, 3,
    0, -1, -1, -5,
    0, 0, 3, 13,
    0, 0, 0, -13]

  mtx-lu-decomposition([matrix(4,4):
      1, 1, 0, 2,
      0, 2, 0, 1,
      1, 0, 0, 0,
      1, 1, 2, 1]) raises ""

  mtx-lu-decomposition([random-matrix: 3, 4]) raises ""
  mtx-lu-decomposition([random-matrix: 4, 3]) raises ""
end

# ==================================================================================================
check "Gram-Schmidt":
  m1 = [matrix(2,2): 3, 2, 1, 2]
  # non-normalized
  # mtx-gram-schmidt(m1) is-roughly [matrix(2,2): 3, -2/5, 1, 6/5]
  # normalized
  mtx-gram-schmidt(m1) is-roughly [matrix(2,2): 3, -1, 1, 3].scale(num-sqrt(1/10))

  m2 = [matrix(3,3): 12, -51, 4, 6, 167, -68, -4, 24, -41]
  mtx-gram-schmidt(m2) 
    is [matrix(3,3):
    6/7, -69/175, -58/175,
    3/7, 158/175, 6/175,
    -2/7, 6/35, -33/35]

  mtx-gram-schmidt([matrix(3,2): 12, -51, 6, 167, -4, 24])
    is [matrix(3,2): 6/7, -69/175, 3/7, 158/175, -2/7, 6/35]

  # normalized
  mtx-gram-schmidt([col-matrix: 12, 6, -4]) is [col-matrix: 6/7, 3/7, -2/7]
  # non-normalized
  # mtx-gram-schmidt([col-matrix: 12, 6, -4]) is [col-matrix: 12, 6, -4]
end

# ==================================================================================================
check "QR decomposition":
  mtx-is-orthonormal(matrix-q(index-matrix(50, 1))) is true

  ### NOTE(Ben): These next few tests run *really excruciatingly slowly* on recent node versions,
  ### but do pass almost instantly in a browser!  Commenting them out for performance, for now...
  # qr = mtx-qr-decomposition([matrix(3,3):
  #     12, -51, 4,
  #     6, 167, -68,
  #     -4, 24, -41])
  # qr.Q is [matrix(3,3):
  #   6/7, -69/175, -58/175,
  #   3/7, 158/175, 6/175,
  #   -2/7, 6/35, -33/35]
  # qr.R is [matrix(3,3):
  #   14, 21, -14,
  #   0, 175, -70,
  #   0, 0, 35]

  # #  A particularly tricky test case used to demonstrate loss of orthogonality
  # #  QR has to generate a better Q than Gram-Schmidt alone (which fails this test)
  # mtx-is-orthonormal(matrix-q([matrix(2,2): 0.70000, 0.70711, 0.70001, 0.70711])) is true
  ### END NOTE

#   #  Fuzz test the heck out of it: 100 matrices, random shape, random entries, sometimes rank-deficient
#   for each(_ from range(0, 100)):
#     m = 1 + num-random(10)
#     n = 1 + num-random(10)
#     N = [random-matrix: m, n, -3, 4]

    
#     #  Full QR, real matrix
#     (let-values ([(Q R)  (matrix-qr M #t)])
#             (check-true (matrix-orthonormal? Q)
#               (format "M = ~a  Q = ~a" M Q))
#             (check-true (<= (matrix-relative-error (matrix* Q R) M)
#                 (* 10 epsilon.0))))
#           #  Reduced QR, real matrix
#           (let-values ([(Q R)  (matrix-qr M #f)])
#                   (check-true (matrix-cols-orthogonal? Q)
#                     (format "M = ~a  Q = ~a" M Q))
#                   (check-true (<= (matrix-relative-error (matrix* Q R) M)
#                     (* 10 epsilon.0))))
#   (define N (random-matrix m n -3 4))
#   (define M+N (array-make-rectangular M N))
#   #  Full QR, complex matrix
#   (let-values ([(Q R)  (matrix-qr M+N #t)])
#     (check-true (matrix-orthonormal? Q)
#                 (format "M+N = ~a  Q = ~a" M+N Q))
#     (check-true (<= (matrix-relative-error (matrix* Q R) M+N)
#                     (* 10 epsilon.0))))
#   #  Reduced QR, complex matrix
#   (let-values ([(Q R)  (matrix-qr M+N #f)])
#     (check-true (matrix-cols-orthogonal? Q)
#                 (format "M+N = ~a  Q = ~a" M+N Q))
#     (check-true (<= (matrix-relative-error (matrix* Q R) M+N)
#                     (* 10 epsilon.0)))))

# (for: ([M  (in-list nonmatrices)])
#   (check-exn exn:fail:contract? (λ () (matrix-q M))))
end

#|
# ==================================================================================================
#  Tests not yet converted to rackunit

(begin

  (begin
    "matrix-operations.rkt"
    (list 'column-project
          (equal? (column-project #(1 2 3) #(4 5 6)) (col-matrix [128/77 160/77 192/77]))
          (equal? (column-project (col-matrix [1 2 3]) (col-matrix [2 4 3]))
                  (matrix-scale (col-matrix [2 4 3]) 19/29)))
    (list 'projection-on-orthogonal-basis
          (equal? (projection-on-orthogonal-basis #(3 -2 2) (list #(-1 0 2) #( 2 5 1)))
                  (col-matrix [-1/3 -1/3 1/3]))
          (equal? (projection-on-orthogonal-basis
                   (col-matrix [3 -2 2]) (list #(-1 0 2) (col-matrix [2 5 1])))
                  (col-matrix [-1/3 -1/3 1/3])))
    (list 'projection-on-orthonormal-basis
          (equal? (projection-on-orthonormal-basis
                   #(1 2 3 4)
                   (list (matrix-scale (col-matrix [ 1  1  1  1]) 1/2)
                         (matrix-scale (col-matrix [-1  1 -1  1]) 1/2)
                         (matrix-scale (col-matrix [ 1 -1 -1  1]) 1/2)))
                  (col-matrix [2 3 2 3])))
    (list 'projection-on-subspace
          (equal? (projection-on-subspace #(1 2 3) '(#(2 4 3)))
                  (matrix-scale (col-matrix [2 4 3]) 19/29)))
  #;
  (begin
    "matrix-2d.rkt"
    (let ()
      (define  e1  (matrix-transpose (vector->matrix #(#( 1  0)))))
      (define  e2  (matrix-transpose (vector->matrix #(#( 0  1)))))
      (define -e1  (matrix-transpose (vector->matrix #(#(-1  0)))))
      (define -e2  (matrix-transpose (vector->matrix #(#( 0 -1)))))
      (define   O  (matrix-transpose (vector->matrix #(#( 0  0)))))
      (define 2*e1 (matrix-scale e1 2))
      (define 4*e1 (matrix-scale e1 4))
      (define 3*e2 (matrix-scale e2 3))
      (define 4*e2 (matrix-scale e2 4))
      (begin
        (list 'matrix-2d-rotation
              (<= (matrix-norm (matrix- (matrix* (matrix-2d-rotation (/ pi 2)) e1) e2 )) epsilon.0)
              (<= (matrix-norm (matrix- (matrix* (matrix-2d-rotation (/ pi 2)) e2) -e1)) epsilon.0))
        (list
         'matrix-2d-scaling
         (equal? (matrix* (matrix-2d-scaling 2 3) (matrix+ e1 e2)) (matrix+ 2*e1 3*e2)))
        (list
         'matrix-2d-shear-x
         (equal? (matrix* (matrix-2d-shear-x 3) (matrix+ e1 e2))   (matrix+ 4*e1   e2)))
        (list
         'matrix-2d-shear-y
         (equal? (matrix* (matrix-2d-shear-y 3) (matrix+ e1 e2))   (matrix+   e1 4*e2)))
        (list
         'matrix-2d-reflection
         (equal? (matrix* (matrix-2d-reflection 0 1) e1)           -e1)
         (equal? (matrix* (matrix-2d-reflection 0 1) e2)            e2)
         (equal? (matrix* (matrix-2d-reflection 1 0) e1)            e1)
         (equal? (matrix* (matrix-2d-reflection 1 0) e2)           -e2))
        (list
         'matrix-2d-orthogonal-projection
         (equal? (matrix* (matrix-2d-orthogonal-projection 1 0) e1) e1)
         (equal? (matrix* (matrix-2d-orthogonal-projection 1 0) e2) O)
         (equal? (matrix* (matrix-2d-orthogonal-projection 0 1) e1) O)
         (equal? (matrix* (matrix-2d-orthogonal-projection 0 1) e2) e2))))))
|#
