### PYRET MATRIX LIBRARY
### Maintained by (i.e.
### Blame all issues on):
### Philip Blair
###   - Github: belph
###   - Email : peblairman@gmail.com

### Last Modified: 30 Mar 2016

### The following is a matrix library which
###   has been heavily influenced by the Racket
###   Linear Algebra functionality (however, all
###   code was done from scratch).

### For individual function documentation, see
###   the appropriate doc strings.

### DATA REPRESENTATION
### A Vector is a custom data structure with
###   one accessor:
###   Vector._contents = (RawArray<Number>)
###                      The elements of the vector
### A Matrix is a custom data structure with
###   three accessors:
###   Matrix.rows = (Number)
###                 The number of rows
###                     in the matrix
###   Matrix.cols = (Number)
###                 The number of columns
###                     in the matrix
###   Matrix.elts = (RawArray<Number>)
###                 The elements of the matrix

### The rest should be relatively
###   straightforward, given the doc strings
###   and example checks.

import vector-util as VU
include from VU: raw-array-fold2, raw-array-map2 end
import matrix-util as MU
include from MU:
  data Pivoting,
end
provide from MU: gauss-elim, qr-decomposition, lu-decomposition, gram-schmidt, data Pivoting end



provide:
  matrix-to-2d-array,
  matrix-from-2d-array,
  mk-mtx as matrix,
  mk-vector as vector,
  vector3d,
  vector-within,
  is-row-matrix,
  is-col-matrix,
  is-square-matrix,
  zero-matrix,
  identity-matrix,
  diagonal-matrix,
  make-matrix,
  build-matrix,
  vector-to-matrix,
  list-to-vector,
  list-to-matrix,
  list-to-row-matrix,
  list-to-col-matrix,
  lists-to-matrix,
  table-to-matrix,
  vectors-to-matrix,
  matrix-within,
  matrix-within-rel,
  matrix-within-abs,
  row-matrix,
  col-matrix,
  vec-get,
  vec-length,
  vec-dot,
  vec-magnitude,
  vec-cross,
  vec-normalize,
  vec-scale,
  vec-add,
  vec-sub,
  vec-to-row-matrix,
  vec-to-col-matrix,
  mtx-get,
  mtx-to-list,
  mtx-to-vector,
  mtx-to-lists,
  mtx-to-vectors,
  mtx-row,
  mtx-col,
  mtx-submatrix,
  mtx-transpose,
  mtx-hermitian,
  mtx-diagonal,
  mtx-upper-triangle,
  mtx-lower-triangle,
  mtx-row-list,
  mtx-col-list,
  mtx-map,
  mtx-map2,
  mtx-row-map,
  mtx-col-map,
  mtx-augment,
  mtx-stack,
  mtx-trace,
  mtx-scale,
  mtx-dot,
  mtx-expt,
  mtx-determinant,
  mtx-is-invertible,
  mtx-rref,
  mtx-row-echelon,
  mtx-rank,
  mtx-nullity,
  mtx-inverse,
  mtx-solve,
  mtx-least-squares-solve,
  mtx-lp-norm,
  mtx-l1-norm,
  mtx-taxicab-norm,
  mtx-l2-norm,
  mtx-euclidean-norm,
  mtx-l-inf-norm,
  mtx-qr-decomposition,
  mtx-lu-decomposition,
  mtx-gram-schmidt,
  mtx-is-roughly-zero,
  mtx-add,
  mtx-sub,
  mtx-mult,
  mtx-is-orthonormal,
  data Vector hiding(vector),
  type Vector3D,
  data Matrix hiding(matrix)
end

import equality as E
include from E: Equal, NotEqual end
import lists as L
include from L:
  map,
  range,
  fold,
  fold2,
  each,
  each2,
  each_n,
  empty,
  is-link,
  type List
end
import valueskeleton as VS
import global as G
include from G:
  raw-array-from-list,
  raw-array-duplicate,
  raw-each-loop
end

fun NonZeroNat(n :: Number): num-is-integer(n) and (n > 0) end
fun Nat(n :: Number): num-is-integer(n) and (n >= 0) end

type NonZeroNat = Number%(NonZeroNat)
type Nat = Number%(Nat)


fun rc-to-index(row :: Nat, col :: Nat, m-cols :: Nat):
  (row * m-cols) + col
end
fun index-to-row(idx :: Nat, m-cols :: Nat):
  num-floor(idx / m-cols)
end
fun index-to-col(idx :: Nat, m-cols :: Nat):
  num-modulo(idx, m-cols)
end

fun length3(v :: Vector): raw-array-length(v._contents) == 3 end
# Precondition: _contents is nonempty
data Vector:
  | vector(_contents :: RawArray<Number>)
sharing:
  
  method get(self, shadow index :: Number) -> Number block:
    doc: "Returns the entry in this vector at the given location"
    when (index < 0) or (index >= raw-array-length(self._contents)):
      raise("Vector index " + tostring(index) + " out of bounds for vector (expected number between 0 and " + tostring(self.length() - 1) + ")")
    end
    raw-array-get(self._contents, index)
  end,
  
  method dot(self, other :: Vector) -> Number:
    doc: "Returns the dot product of this vector with the given vector"
    if self.length() <> other.length():
      raise("Dot products are only defined between two vectors of the same length")
    else:
      VU.dot(self._contents, other._contents)
    end
  end,
  
  method magnitude(self) -> Number:
    doc: "Returns the magnitude of this vector"
    VU.magnitude(self._contents)
  end,
  
  # TODO: Can this be a Vector3D (i.e. is Vector3D in scope?)
  method cross(self, other :: Vector) -> Vector:
    doc: "Returns the cross product of this 3D vector with the given 3D vector"
    vector(VU.cross(self._contents, other._contents))
  end,
  
  method normalize(self) -> Vector:
    doc: "Normalizes this vector into a unit vector"
    vector(VU.normalize(self._contents))
  end,
  
  method scale(self, scalar :: Number) -> Vector:
    doc: "Scales this vector by the given constant"
    vector(VU.scale(self._contents, scalar))
  end,
  
  method _plus(self, other :: Vector) -> Vector:
    doc: "Adds the two given vectors"
    vector(VU.plus(self._contents, other._contents))
  end,
  
  method _minus(self, other :: Vector) -> Vector:
    doc: "Subtracts the two given vectors"
    vector(VU.minus(self._contents, other._contents))
  end,
  
  method _equals(self, other :: Vector, eq):
    if (raw-array-length(self._contents) <> raw-array-length(other._contents)):
      NotEqual("Vectors have different sizes", self, other)
    else:
      for raw-array-fold2(acc from Equal, self-elt from self._contents, other-elt from other._contents, i from 0):
        E.equal-and(acc, eq(self-elt, other-elt))
      end
    end
  end,
  
  method length(self) -> Number:
    doc: "Returns the length of this vector"
    raw-array-length(self._contents)
  end,

  method to-row-matrix(self) -> Matrix:
    doc: "Returns the equivalent 1-row matrix containing this vector's contents"
    matrix(1, self.length(), raw-array-duplicate(self._contents))
  end,

  method to-col-matrix(self) -> Matrix:
    doc: "Returns the equivalent 1-column matrix containing this vector's contents"
    matrix(self.length(), 1, raw-array-duplicate(self._contents))
  end,
  
  method _output(self) -> VS.ValueSkeleton:
    VS.vs-collection("vector", raw-array-to-list(self._contents).map(VS.vs-value))
  end
end

# Internal use only!
rec vector-of-raw-array = vector

rec mk-vector = {
  make0: lam(): raise("Cannot construct a Vector with 0 elements") end,
  make1: lam(a :: Number): vector-of-raw-array([raw-array: a]) end,
  make2: lam(a :: Number, b :: Number): vector-of-raw-array([raw-array: a, b]) end,
  make3: lam(a :: Number, b :: Number, c :: Number):
    vector-of-raw-array([raw-array: a, b, c]) end,
  make4: lam(a :: Number, b :: Number, c :: Number, d :: Number):
    vector-of-raw-array([raw-array: a, b, c, d]) end,
  make5: lam(a :: Number, b :: Number, c :: Number, d :: Number, e :: Number):
    vector-of-raw-array([raw-array: a, b, c, d, e]) end,
  make : lam(arr :: RawArray<Number>): vector-of-raw-array(arr) end
}

rec vector3d = block:
  fun bad-arity(n):
    "Cannot construct a 3D Vector with " + tostring(n) + " element"
      + (if (n <> 1): "s" else: "" end)
  end
  {
    make0: lam(): raise(bad-arity(0)) end,
    make1: lam(a :: Number): raise(bad-arity(1)) end,
    make2: lam(a :: Number, b :: Number): raise(bad-arity(2)) end,
    make3: lam(a :: Number, b :: Number, c :: Number): [mk-vector: a, b, c] end,
    make4: lam(a :: Number, b :: Number, c :: Number, d :: Number): raise(bad-arity(4)) end,
    make5: lam(a :: Number, b :: Number, c :: Number, d :: Number, e :: Number): raise(bad-arity(5)) end,
    make : lam(arr :: RawArray<Number>): raise(bad-arity(raw-array-length(arr))) end
  }
end

fun vector-within(n :: Number) -> (Vector, Vector -> Boolean):
  lam(v :: Vector, w :: Vector) -> Boolean:
    shadow within = within(n)
    for raw-array-fold2(acc from true, v-elt from v._contents, w-elt from w._contents, i from 0):
      acc and (within(v-elt, w-elt))
    end
  end
end

fun is-row-matrix(mtx :: Matrix):
  doc: "Returns true if the given matrix has exactly one row"
  mtx.rows == 1
end

fun is-col-matrix(mtx :: Matrix):
  doc: "Returns true if the given matrix has exactly one column"
  mtx.cols == 1
end

fun is-square-matrix(mtx :: Matrix):
  doc: "Returns true if the given matrix has the same number of rows and columns"
  mtx.rows == mtx.cols
end

fun make-raw-array-fold2<a, b, c>(start1 :: Number, start2 :: Number, stride1 :: Number, stride2 :: Number, stop1 :: Number, stop2 :: Number) -> ((a, b, c -> a), a, RawArray<b>, RawArray<c> -> a):
  doc: "RawArray fold2 Implementation with striding ability"
  lam(f :: (a, b, c -> a), base :: a, arr1 :: RawArray<b>, arr2 :: RawArray<c>) -> a:
    len1 = num-min(raw-array-length(arr1), stop1)
    len2 = num-min(raw-array-length(arr2), stop2)
    fun help(acc, idx1, idx2):
      if (idx1 > len1) or (idx2 > len2): acc
      else:
        help(f(acc, raw-array-get(arr1, idx1), raw-array-get(arr2, idx2)), idx1 + stride1, idx2 + stride2)
      end
    end
    help(base, start1, start2)
  end
end


data Matrix:
  | matrix(rows :: NonZeroNat, cols :: NonZeroNat, elts :: RawArray<Number>)
sharing:
  method get(self, i :: Nat, j :: Nat) -> Number:
    doc: "Returns the matrix's entry in the i'th row and j'th column"
    if (i < 0) or (i >= self.rows) or (j < 0) or (j >= self.cols):
      raise("Index out of bounds for matrix dimensions")
    else:
      raw-array-get(self.elts, rc-to-index(i, j, self.cols))
    end
  end,
  
  method to-list(self) -> List<Number>:
    doc: "Returns the matrix as a list of numbers"
    raw-array-to-list(self.elts)
  end,
  
  method to-vector(self) -> Vector:
    doc: "Returns the one-row/one-column matrix as a vector"
    if ((self.cols == 1) or (self.rows == 1)):
      vector-of-raw-array(self.elts)
    else:
      raise("Cannot convert non-vector matrix to vector")
    end
  end,
  
  method to-lists(self) -> List<List<Number>>:
    doc: "Returns the matrix as a list of lists of numbers, with each list corresponding to a row"
    self.row-list().map(lam(r): r.to-list() end)
  end,
  
  method to-vectors(self) -> List<Vector>:
    doc: "Returns the matrix a a list of vectors, with each vector corresponding to a column"
    self.col-list().map(lam(c): c.to-vector() end)
  end,
  
  method row(self, i :: Number) -> Matrix:
    doc: "Returns a one-row matrix with the matrix's i'th row"
    raw-arr = for raw-array-build(n from self.cols):
      raw-array-get(self.elts, rc-to-index(i, n, self.cols))
    end
    matrix(1, self.cols, raw-arr)
  end,
  method col(self, j :: Number) -> Matrix:
    doc: "Returns a one-column matrix with the matrix's j'th column"
    raw-arr = for raw-array-build(n from self.rows):
      raw-array-get(self.elts, rc-to-index(n, j, self.cols))
    end
    matrix(self.rows, 1, raw-arr)
  end,
  
  method submatrix(self, loi :: List<Number>, loj :: List<Number>) -> Matrix block:
    doc: "Returns the matrix's submatrix, containing the rows in the first list and the columns in the second"
    loil = loi.length()
    lojl = loj.length()
    if (loil == 0) or (lojl == 0) block:
      raise("Cannot extract an empty submatrix")
    else:
      for each(i from loi):
        when (i < 0) or (i >= self.rows):
          raise("Invalid row index " + to-string(i))
        end
      end
      for each(j from loj):
        when (j < 0) or (j >= self.cols):
          raise("Invalid column index " + to-string(j))
        end
      end
    end
    raw-arr = raw-array-of(0, (loil * lojl))
    # Loops manually unwrapped to avoid excessive allocations
    raw-each-loop(lam(r):
        row-num = loi.get(r)
        raw-each-loop(lam(c) block:
            col-num = loj.get(c)
            raw-array-set(raw-arr, (r * lojl) + c, 
              raw-array-get(self.elts, rc-to-index(row-num, col-num, self.cols)))
            nothing
          end, 0, lojl)
      end, 0, loil)
    matrix(loil, lojl, raw-arr)
  end,
  
  method transpose(self) -> Matrix block:
    doc: "Returns the matrix's transpose"
    raw-arr = raw-array-of(0, (self.rows * self.cols))
    raw-each-loop(lam(r):
        raw-each-loop(lam(c) block:
            raw-array-set(raw-arr, rc-to-index(c, r, self.rows),
              raw-array-get(self.elts, rc-to-index(r, c, self.cols)))
            nothing
          end, 0, self.cols)
      end, 0, self.rows)
    matrix(self.cols, self.rows, raw-arr)
  end,

  method hermitian(self) -> Matrix:
    self.transpose() # since we have no complex numbers, no need to conjugate them
  end,
  
  method diagonal(self) -> Matrix block:
    doc: "Returns a one-row matrix with the matrix's diagonal entries"
    num-diag = num-min(self.rows, self.cols)
    raw-arr = raw-array-of(0, num-diag)
    raw-each-loop(lam(n) block:
        raw-array-set(raw-arr, n,
          raw-array-get(self.elts, rc-to-index(n, n, self.cols)))
        nothing
      end, 0, num-diag)
    matrix(1, num-diag, raw-arr)
  end,
  
  method upper-triangle(self) -> Matrix:
    doc: "Returns the upper triangle of the matrix"
    if not(is-square-matrix(self)) block:
      raise("Cannot make a non-square upper-triangular matrix")
    else:
      raw-arr = raw-array-of(0, (self.rows * self.cols))
      raw-each-loop(lam(r):
          raw-each-loop(lam(on-col) block:
              raw-array-set(raw-arr, rc-to-index(r, on-col, self.cols),
                raw-array-get(self.elts, rc-to-index(r, on-col, self.cols)))
              nothing
            end, r, self.cols)
        end, 0, self.rows)
      matrix(self.rows, self.cols, raw-arr)
    end
  end,
  
  method lower-triangle(self) -> Matrix:
    doc: "Returns the lower triangle of the matrix"
    if not(is-square-matrix(self)) block:
      raise("Cannot make a non-square lower-triangular matrix")
    else:
      raw-arr = raw-array-of(0, (self.rows * self.cols))
      raw-each-loop(lam(r):
          raw-each-loop(lam(on-col) block:
              raw-array-set(raw-arr, rc-to-index(r, on-col, self.cols),
                raw-array-get(self.elts, rc-to-index(r, on-col, self.cols)))
              nothing
            end, 0, r + 1)
        end, 0, self.rows)
      matrix(self.rows, self.cols, raw-arr)
    end
  end,
  
  method row-list(self) -> List<Matrix>:
    doc: "Returns the matrix as a list of one-row matrices"
    for map(i from range(0, self.rows)):
      self.row(i)
    end
  end,
  
  method col-list(self) -> List<Matrix>:
    doc: "Returns the matrix as a list of one-column matrices"
    for map(i from range(0, self.cols)):
      self.col(i)
    end
  end,
  
  method map(self, func :: (Number -> Number)):
    doc: "Maps the given function over each entry in the matrix"
    self-len = raw-array-length(self.elts)
    matrix(self.rows, self.cols, raw-array-build(lam(i): func(raw-array-get(self.elts, i)) end, self-len))
  end,
  
  method map2(self, other :: Matrix, func :: (Number, Number -> Number)):
    doc: "Maps the given function over the corresponding entries in the given matrices"
    if (self.rows <> other.rows) or (self.cols <> other.cols):
      raise("Matrix sizes do not match")
    else:
      self-len = raw-array-length(self.elts)
      self-elts = self.elts
      other-elts = other.elts
      matrix(self.rows, self.cols, raw-array-build(
          lam(i): func(raw-array-get(self-elts, i), raw-array-get(other-elts, i)) end,
          self-len))
    end
  end,
  
  method row-map(self, func :: (Matrix -> Matrix)):
    doc: "Maps the given function over each row in the matrix"
    to-stack = self.row-list().map(func)
    cases(List) to-stack:
      | empty => self
      | link(first, rest) =>
        for fold(acc from first, cur from rest):
          acc.stack(cur)
        end
    end
  end,
  
  method col-map(self, func :: (Matrix -> Matrix)):
    doc: "Maps the given function over each column in the matrix"
    to-aug = self.col-list().map(func)
    cases(List) to-aug:
      | empty => self
      | link(first, rest) =>
        for fold(acc from first, cur from rest):
          acc.augment(cur)
        end
    end
  end,
  
  method augment(self, other :: Matrix) -> Matrix:
    doc: "Augments the given matrix onto the matrix"
    if not(self.rows == other.rows):
      raise("Cannot augment matrices with different numbers of rows")
    else:
      new-size = ((self.cols + other.cols) * self.rows)
      raw-arr = raw-array-of(0, new-size)
      fun get-el(n :: Number):
        col = index-to-col(n, (self.cols + other.cols))
        row = index-to-row(n, (self.cols + other.cols))
        if col >= self.cols:
          raw-array-get(other.elts, rc-to-index(row, col - self.cols, other.cols))
        else:
          raw-array-get(self.elts, rc-to-index(row, col, self.cols))
        end
      end
      matrix(self.rows, (self.cols + other.cols), raw-array-build(get-el, new-size))
    end
  end,
  
  method stack(self, other :: Matrix) -> Matrix:
    doc: "Returns the matrix stacked on top of the given matrix"
    if not(self.cols == other.cols):
      raise("Cannot stack matrices with different row lengths")
    else:
      new-size = ((self.rows + other.rows) * self.cols)
      old-size = raw-array-length(self.elts)
      fun get-el(n :: Number):
        if n < old-size:
          raw-array-get(self.elts, n)
        else:
          raw-array-get(other.elts, (n - old-size))
        end
      end
      matrix((self.rows + other.rows), self.cols, raw-array-build(get-el, new-size))
    end
  end,
  
  method trace(self) -> Number block:
    doc: "Returns the trace of the matrix (sum of the diagonal values)"
    when self.rows <> self.cols:
      raise("Trace is not defined for non-square matrices")
    end
    for L.fold(acc from 0, i from range(0, num-min(self.rows, self.cols))):
      acc + raw-array-get(self.elts, rc-to-index(i, i, self.cols))
    end
  end,
  
  method scale(self, k :: Number) -> Matrix:
    doc: "Scales the matrix by the given value"
    self.map(_ * k)
  end,
  
  method dot(self, b :: Matrix) -> Number block:
    doc: "Returns the Frobenius Product of the matrix with the given matrix"
    when (not(self.rows == b.rows) or not(self.cols == b.cols)):
      raise("Cannot return the inner product of two different-sized matrices")
    end
    # Defined as (self.transpose() * b).trace(), but this is O(n^3) expensive to compute
    # (also, ignoring the complex conjugation here, because Pyret has only real numbers)
    for raw-array-fold2(acc from 0, elt from self.elts, other-elt from b.elts, _ from 0):
      acc + (elt * other-elt)
    end
  end,
  
  
  # Arithmetic Operators
  method _plus(self, b :: Matrix) -> Matrix block:
    doc: "Adds the given matrix to the matrix"
    when (not(self.rows == b.rows)
        or not(self.cols == b.cols)):
      raise("Cannot add two different sized matricies")
    end
    size = raw-array-length(self.elts)
    raw-arr = raw-array-build(lam(i):
        raw-array-get(self.elts, i) + raw-array-get(b.elts, i)
      end, size)
    matrix(self.rows, self.cols, raw-arr)
  end,
  
  method _minus(self, b :: Matrix) -> Matrix block:
    doc: "Subtracts the given matrix from the matrix"
    when (not(self.rows == b.rows)
        or not(self.cols == b.cols)):
      raise("Cannot add two different sized matricies")
    end
    size = raw-array-length(self.elts)
    raw-arr = raw-array-build(lam(i):
        raw-array-get(self.elts, i) - raw-array-get(b.elts, i)
      end, size)
    matrix(self.rows, self.cols, raw-arr)
  end,
  
  method _times(self, b :: Matrix) -> Matrix block:
    #C_ik = Sum(A_ij * B_jk)
    doc: "Multiplies the matrix by the given matrix"
    when not(self.cols == b.rows):
      raise("Matrix multiplication is undefined for the given inputs")
    end
    new-size = (self.rows * b.cols)
    raw-arr = raw-array-of(0, new-size)
    raw-each-loop(lam(i):
        raw-each-loop(lam(k) block:
            strided-fold = make-raw-array-fold2(i * self.cols, k, 1, b.cols, ((i + 1) * self.cols) - 1, (b.rows * b.cols) + k)
            raw-array-set(raw-arr, rc-to-index(i, k, b.cols),
              for strided-fold(base from 0, self_ij from self.elts, b_jk from b.elts):
                base + (self_ij * b_jk)
              end)
            nothing
          end, 0, b.cols)
      end, 0, self.rows)
    matrix(self.rows, b.cols, raw-arr)
  end,
  
  method expt(self, n :: Nat) -> Matrix:
    doc: "Multiplies the matrix by itself the given number of times"
    if n == 0:
      identity-matrix(self.rows)
    else if (num-modulo(n, 2) == 0):
      to-sqr = self.expt(n / 2)
      to-sqr * to-sqr
    else:
      to-sqr = self.expt((n - 1) / 2)
      self * (to-sqr * to-sqr)
    end
  end,
  
  method determinant(self) -> Number:
    doc: "Returns the determinant of the matrix"
    # Recursive implementation of Laplace Expansion
    if not(is-square-matrix(self)):
      raise("Cannot take the determinant of a non-square matrix")
    else:
      elts-array = matrix-to-2d-array(self)
      MU.determinant(elts-array)
    end
  end,
  
  method is-invertible(self) -> Boolean:
    doc: "Returns true if the given matrix is invertible"
    is-square-matrix(self) and not(within-abs(1e-6)(self.determinant(), 0))
  end,

  method is-orthonormal(self) -> Boolean:
    is-square-matrix(self) and roughly-equal(self * self.hermitian(), identity-matrix(self.rows))
  end,
  
  method rref(self) -> Matrix block:
    doc: "Returns the Reduced Row Echelon Form of the Matrix"
    elts-array = matrix-to-2d-array(self)
    ans = MU.gauss-elim(elts-array, true, true, MU.largest-element)
    matrix-from-2d-array(ans.elim, self.rows, self.cols)
  end,
  
  method inverse(self) -> Matrix block:
    when not(self.is-invertible()):
      raise("Cannot find inverse of non-invertible matrix")
    end
    to-ret = matrix(self.rows, self.cols, raw-array-duplicate(self.elts))
    to-aug = identity-matrix(self.rows)
    to-chop = to-ret.augment(to-aug).rref()
    to-chop.submatrix(range(0, self.rows), range(self.cols, 2 * self.cols))
  end,
  
  method solve(self, b :: Matrix) -> Matrix:
    doc: "Returns the matrix needed to multiply with this matrix to receive the given matrix (i.e. the solution for a system of equations), if it exists"
    if not(self.is-invertible()):
      raise("Cannot find exact solution for non-invertible matrix")
    else:
      self.inverse() * b
    end
  end,
  
  method least-squares-solve(self, b :: Matrix) -> Matrix:
    doc: "Returns the least-squares solution for self * x = b"
    # Self is m*n
    if self.rows < self.cols: # Underdetermined case
      # first find the QR factorization of the transpose of A
      qr = self.transpose().qr-decomposition()
      q = qr.Q
      r = qr.R
      # r has the form mtx-stack(r1, zeros), extract the r1 part
      r1 = r.submatrix(range(0, r.cols), range(0, r.cols))
      r1tinv-b = r1.transpose().inverse() * b
      full-r1tinv-b = r1tinv-b.stack(zero-matrix(q.cols, r1tinv-b.cols))
      q * full-r1tinv-b
    else: # Overdetermined case
      qr = self.qr-decomposition()
      q = qr.Q
      r = qr.R
      # q has the form mtx-stack(q1, zeros), extract the q1 part
      q1 = q.submatrix(range(0, self.rows), range(0, self.cols))
      # r has the form mtx-stack(r1, zeros), extract the r1 part
      r1 = r.submatrix(range(0, r.cols), range(0, r.cols))
      r1.inverse() * (q1.transpose() * b)
    end
  end,

  method lu-decomposition(self) -> { L :: Matrix, U :: Matrix }:
    if not(is-square-matrix(self)):
      raise("Cannot take the determinant of a non-square matrix")
    else:
      elts-array = matrix-to-2d-array(self)
      ans = MU.lu-decomposition(elts-array)
      {
        L : matrix-from-2d-array(ans.L, self.rows, self.cols),
        U : matrix-from-2d-array(ans.U, self.rows, self.cols)
      }
    end
  end,
  
  # While perhaps not the most useful in practice,
  #   multi-column matricies do have norms defined,
  #   so they are defined here.    
  method lp-norm(self, p :: Number) -> Number:
    doc: "Computes the Lp norm of the matrix"
    summed = for raw-array-fold(acc from 0, cur from self.elts, _ from 0):
      acc + num-expt(num-abs(cur), p)
    end
    num-expt(summed, (1 / p))
  end,
  
  method l1-norm(self) -> Number:
    doc: "Computes the L1 norm (or Taxicab norm) of the matrix"
    # Defined to be self.lp-norm(1), but we can omit the excess work
    for raw-array-fold(acc from 0, cur from self.elts, _ from 0):
      acc + num-abs(cur)
    end
  end,
  
  method l2-norm(self) -> Number:
    doc: "Computes the L2 norm (or Euclidean norm) of the matrix"
    # Defined to be self.lp-norm(2), but we can use num-sqr and num-sqrt instead,
    # which are cheaper and overflow less often
    summed = for raw-array-fold(acc from 0, cur from self.elts, _ from 0):
      acc + num-sqr(cur)
    end
    num-sqrt(summed)
  end,
  
  method l-inf-norm(self) -> Number:
    doc: "Computes the L-Infinity norm of the matrix"
    raw-array-fold(lam(acc, cur, i): num-max(acc, num-abs(cur)) end, 0, self.elts, 0)
  end,
  
  method qr-decomposition(self) -> {Q :: Matrix, R :: Matrix} block:
    doc: "Returns the QR Decomposition of the matrix"
    elts-array = matrix-to-2d-array(self)
    ans = MU.qr-decomposition(elts-array, true)
    { Q: matrix-from-2d-array(ans.Q, self.rows, self.rows), R: matrix-from-2d-array(ans.R, self.rows, self.cols) }
  end,
  
  
  method gram-schmidt(self) -> Matrix:
    doc: "Returns an orthogonal matrix whose image is the same as the span of the Matrix's columns"
    elts-array = matrix-to-2d-array(self)
    ans = MU.gram-schmidt(elts-array, true)
    new-rows = raw-array-length(ans)
    new-cols = if new-rows == 0: 0 else: raw-array-length(raw-array-get(ans, 0)) end
    matrix-from-2d-array(ans, new-rows, new-cols)
  end,
  
  method _torepr(self, shadow torepr) -> String:
    fun pad(str :: String, to-len :: Number):
      if to-len <= string-length(str):
        str
      else:
        string-append(
          string-repeat(" ", (to-len - string-length(str))), str)
      end
    end
    max-len = for raw-array-fold(acc from 0, n from self.elts, i from 0):
      num-max(string-length(num-tostring(n)), acc)
    end
    fun elt-tostr(i :: Number):
      if num-is-integer((i + 1) / self.cols):
        pad(num-tostring(raw-array-get(self.elts, i)), max-len) + "\n"
      else:
        pad(num-tostring(raw-array-get(self.elts, i)), max-len) + "\t"
      end
    end
    "\n" + for raw-array-fold(acc from "", n from self.elts, i from 0):
      acc + elt-tostr(i)
    end
  end,
  
  method _output(self):
    VS.vs-matrix(self.rows, self.cols, raw-array-map(VS.vs-value, self.elts))
  end,
  
  method _equals(self, a :: Matrix, eq):
    if (self.rows <> a.rows) or (self.cols <> a.cols):
      NotEqual("Matrices have different sizes", self, a)
    else:
      for raw-array-fold2<Number, Number, Number>(acc from Equal, self-elt from self.elts, other-elt from a.elts, i from 0):
        E.equal-and(acc, eq(self-elt, other-elt))
      end
    end
  end
end

fun build-matrix(rows :: NonZeroNat, cols :: NonZeroNat, proc :: (Number, Number -> Number)):
  doc: "Constructs a matrix of the given size, where entry (i, j) is the result of proc(i, j)"
  raw-arr = raw-array-build(lam(i):
      proc(index-to-row(i, cols), index-to-col(i, cols))
    end, (rows * cols))
  matrix(rows, cols, raw-arr)
where:
  build-matrix(3, 2, lam(i, j): i + j end) is
  [mk-mtx(3, 2): 0, 1, 1, 2, 2, 3]
end

fun matrix-to-2d-array(m):
  raw-array-from-list(m.to-lists().map(raw-array-from-list))
end

fun matrix-from-2d-array(elts, rows, cols):
  build-matrix(rows, cols, lam(r, c) -> Number: raw-array-get(raw-array-get(elts, r), c) end)
end

fun zero-matrix(rows :: NonZeroNat, cols :: NonZeroNat) -> Matrix:
  matrix(rows, cols, raw-array-of(0, (rows * cols)))
end
  
fun identity-matrix(n :: NonZeroNat):
  doc: "Returns the identity matrix of the given size"
  fun id(r :: Number, c :: Number) -> RawArray<Number> block:
    raw-arr = raw-array-of(0, (r * c))
    raw-each-loop(lam(i) block:
        raw-array-set(raw-arr, (i * c) + i, 1)
        nothing
      end, 0, r)
    raw-arr
  end
  matrix(n, n, id(n, n))
end

fun diagonal-matrix(d :: List<Number>%(is-link)) block:
  doc: "Returns a matrix with the given numbers along the diagonal"
  len = d.length()
  raw-arr = raw-array-of(0, len * len)
  for each_n(i :: Number from 0, v :: Number from d) block:
    raw-array-set(raw-arr, (i * len) + i, v)
    nothing
  end
  matrix(len, len, raw-arr)
end
  

type MatrixMaker = {
  make0 :: ( -> Matrix),
  make1 :: (Number -> Matrix),
  make2 :: (Number, Number -> Matrix),
  make3 :: (Number, Number, Number -> Matrix),
  make4 :: (Number, Number, Number, Number -> Matrix),
  make5 :: (Number, Number, Number, Number, Number -> Matrix),
  make :: (RawArray<Number> -> Matrix)
}

fun mk-mtx(rows :: NonZeroNat, cols :: NonZeroNat) -> MatrixMaker block:
  fun invalid-msg(num-given :: Number):
    pre = "Invalid " + tostring(rows) + "x"
      + tostring(cols) + " Matrix Input: Expected "
      + tostring(cols * rows) + " elements; Received "
    post = " elements."
    pre + tostring(num-given) + post
  end
  fun make-unexpected(n :: Number):
    lam(arr): raise(invalid-msg(raw-array-length(arr))) end
  end
  rec ans = {
    make0: lam(): raise(invalid-msg(0)) end,
    make1: lam(a :: Number): ans.make([raw-array: a]) end,
    make2: lam(a :: Number, b :: Number): ans.make([raw-array: a, b]) end,
    make3: lam(a :: Number, b :: Number, c :: Number): ans.make([raw-array: a, b, c]) end,
    make4: lam(a :: Number, b :: Number, c :: Number, d :: Number): ans.make([raw-array: a, b, c, d]) end,
    make5: lam(a :: Number, b :: Number, c :: Number, d :: Number, e :: Number): ans.make([raw-array: a, b, c, d, e]) end,
    make : lam(arr :: RawArray<Number>) -> Matrix:
        if not(raw-array-length(arr) == (rows * cols)):
          raise(invalid-msg(raw-array-length(arr)))
        else:
          matrix(rows, cols, arr)
        end
      end
  }
  ans
where:
  [mk-mtx(1, 1): 1] is matrix(1, 1, [raw-array: 1])
  [mk-mtx(3, 1): 1, 
                2,
                3] is matrix(3, 1, [raw-array: 1, 2, 3])
  [mk-mtx(1, 3): 3, 2, 1] is matrix(1, 3, [raw-array: 3, 2, 1])
  [mk-mtx(2, 2): 1, 2, 
                3, 4] is matrix(2, 2, [raw-array: 1, 2, 3, 4])
end

fun make-matrix(rows :: NonZeroNat, cols :: NonZeroNat, x :: Number):
  doc: "Constructs a matrix of the given size with the given elements"
  matrix(rows, cols, raw-array-of(x, (rows * cols)))
where:
  make-matrix(2, 3, 1) is [mk-mtx(2, 3): 1, 1, 1, 1, 1, 1]
  make-matrix(3, 2, 5) is [mk-mtx(3, 2): 5, 5, 5, 5, 5, 5]
end

row-matrix :: MatrixMaker =
  {
    make: lam(arr :: RawArray<Number>):
        matrix(1, raw-array-length(arr), arr)
      end,
    make0: lam(): raise("Invalid Matrix Input: Cannot construct a zero-length row matrix") end,
    make1: lam(a): matrix(1, 1, [raw-array: a]) end,
    make2: lam(a, b): matrix(1, 2, [raw-array: a, b]) end,
    make3: lam(a, b, c): matrix(1, 3, [raw-array: a, b, c]) end,
    make4: lam(a, b, c, d): matrix(1, 4, [raw-array: a, b, c, d]) end,
    make5: lam(a, b, c, d, e): matrix(1, 5, [raw-array: a, b, c, d, e]) end
  }

col-matrix :: MatrixMaker =
  {
    make: lam(arr :: RawArray<Number>):
        matrix(raw-array-length(arr), 1, arr)
      end,
    make0: lam(): raise("Invalid Matrix Input: Cannot construct a zero-length column matrix") end,
    make1: lam(a): matrix(1, 1, [raw-array: a]) end,
    make2: lam(a, b): matrix(2, 1, [raw-array: a, b]) end,
    make3: lam(a, b, c): matrix(3, 1, [raw-array: a, b, c]) end,
    make4: lam(a, b, c, d): matrix(4, 1, [raw-array: a, b, c, d]) end,
    make5: lam(a, b, c, d, e): matrix(5, 1, [raw-array: a, b, c, d, e]) end
  }

fun vector-to-matrix(v :: Vector):
  doc: "Converts the given vector into a one-row matrix"
  matrix(1, raw-array-length(v._contents), raw-array-duplicate(v._contents))
where:
  vector-to-matrix([mk-vector: 1, 2, 3]) is
  [mk-mtx(1, 3): 1, 2, 3]
end

fun list-to-vector(lst :: List<Number>):
  doc: "Converts the given list of numbers into a vector"
  list-to-row-matrix(lst).to-vector()
end

fun list-to-matrix(rows :: NonZeroNat, cols :: NonZeroNat, lst :: List<Number>) block:
  doc: "Converts the given list of numbers into a matrix of the given size"
  when not(lst.length() == (rows * cols)):
    raise("Provided list does not have arguments corresponding to provided matrix size")
  end
  matrix(rows, cols, raw-array-from-list(lst))
where:
  list-to-matrix(2, 3, [list: 1, 2]) raises "Provided list does not have arguments corresponding to provided matrix size"
  list-to-matrix(3, 2, [list: 1, 2, 3, 4, 5, 6]) is [mk-mtx(3, 2): 1, 2, 3, 4, 5, 6]
  list-to-matrix(1, 4, [list: 2, 4, 6, 8]) is [mk-mtx(1, 4): 2, 4, 6, 8]
end

fun list-to-row-matrix(lst :: List<Number>):
  doc: "Converts the given list into a row matrix"
  list-to-matrix(1, lst.length(), lst)
where:
  list-to-row-matrix([list: 2, 3, 4, 5]) is [mk-mtx(1, 4): 2, 3, 4, 5]
end

fun list-to-col-matrix(lst :: List<Number>):
  doc: "Converts the given list into a column matrix"
  list-to-matrix(lst.length(), 1, lst)
where:
  list-to-col-matrix([list: 1, 2, 3]) is [mk-mtx(3, 1): 1, 2, 3]
end

fun lists-to-matrix(lst :: List<List<Number>>) block:
  doc: "Converts the given list of lists into a matrix, with each contained list as a row"
  rows = lst.length()
  when rows == 0:
    raise("Invalid Matrix input")
  end
  cols = lst.get(0).length()
  # Check that all Lists are the same length
  when fold(lam(r, f): not(f.length() == cols) or r end, false, lst):
    raise("Invalid Matrix input")
  end
  matrix(rows, cols, raw-array-from-list(lst.foldr(_ + _, empty)))
where:
  lists-to-matrix([list: [list: 1, 2, 3], [list: 4, 5, 6]]) is
  [mk-mtx(2, 3): 1, 2, 3, 4, 5, 6]
  
  lists-to-matrix([list: [list: 1, 2, 3], [list: 1, 2]]) raises
  "Invalid Matrix input"
end

fun table-to-matrix(t :: Table):
  doc: "Converts the given table into a matrix, ignoring column names, and ensuring all values are numeric."
  rows = t.length()
  cols = raw-array-length(t._header-raw-array)
  matrix-from-2d-array(t._rows-raw-array, rows, cols)
end

# NOTE:
# The Racket implementation of this function works
#   the same way as lists-to-matrix does. The reasons
#   I opted to make the vectors columns instead are twofold:
#   1.) The way Vectors are defined (as of this implementation),
#       they are functionally indistinguishable from 
#       List<List<Number>>, so one may still convert a List<Vector>
#       into a matrix with the vectors as rows using that function.
#   2.) Mathematically, vectors (usually) have components in
#       different dimensions. The matrix representation dimensions
#       usually corresponds to the matrix's rows, so I felt it
#       was perhaps more appropriate to line up the dimensional
#       components of the vectors.
# Am I wrong? Maybe.
fun vectors-to-matrix(lst :: List<Vector>) block:
  doc: "Converts the given list of vectors into a matrix, with each vector as a column"
  rows = raw-array-length(lst.get(0)._contents)
  cols = lst.length()
  # Check that all Vectors are the same length
  when fold(lam(r, f): not(raw-array-length(f._contents) == rows) or r end, false, lst):
    raise("Invalid Matrix input")
  end
  raw-arr = raw-array-of(0, (rows) * (cols))
  for each(i from range(0, rows)):
    for each(j from range(0, cols)):
      raw-array-set(raw-arr, rc-to-index(i, j, cols), lst.get(j).get(i))
    end
  end
  matrix(rows, cols, raw-arr)
where:
  vectors-to-matrix([list: [mk-vector: 1, 2, 3], [mk-vector: 4, 5, 6]]) is
  [mk-mtx(3, 2): 1, 4, 2, 5, 3, 6]
end

fun matrix-within(n :: Number) -> (Matrix, Matrix -> Boolean):
  matrix-within-gen(within(n))
end
fun matrix-within-rel(n :: Number) -> (Matrix, Matrix -> Boolean):
  matrix-within-gen(within-rel(n))
end
fun matrix-within-abs(n :: Number) -> (Matrix, Matrix -> Boolean):
  matrix-within-gen(within-abs(n))
end
fun matrix-within-gen(comp :: (Any, Any -> Boolean)) -> (Matrix, Matrix -> Boolean):
  lam(a :: Matrix, b :: Matrix) -> Boolean:
    if (a.rows <> b.rows) or (a.cols <> b.cols):
      raise("Matrix sizes do not match")
    else:
      for raw-array-fold2(acc from true, a-elt from a.elts, b-elt from b.elts, _ from 0):
        acc and comp(a-elt, b-elt)
      end
    end
  end
end

fun mtx-is-roughly-zero(m :: Matrix) -> Boolean:
  VU.is-zero(m.elts) # punning off the 1d-array nature
end

## FUNCTION-STYLE METHOD WRAPPERS

# VECTORS

type Vector3D = Vector%(length3)

fun vec-get(v :: Vector, i :: Nat):
  v.get(i)
end
fun vec-length(v :: Vector):
  v.length()
end
fun vec-dot(v1 :: Vector, v2 :: Vector):
  v1.dot(v2)
end
fun vec-magnitude(v :: Vector):
  v.magnitude()
end
fun vec-cross(v1 :: Vector3D, v2 :: Vector3D):
  v1.cross(v2)
end
fun vec-normalize(v :: Vector):
  v.normalize()
end
fun vec-scale(v :: Vector, scalar :: Number):
  v.scale(scalar)
end
fun vec-add(v1 :: Vector, v2 :: Vector):
  v1 + v2
end
fun vec-sub(v1 :: Vector, v2 :: Vector):
  v1 - v2
end

fun vec-to-row-matrix(v :: Vector):
  v.to-row-matrix()
end
fun vec-to-col-matrix(v :: Vector):
  v.to-col-matrix()
end

# MATRICES
fun mtx-get(m :: Matrix, i :: Nat, j :: Nat):
  m.get(i, j)
end
fun mtx-to-list(m :: Matrix):
  m.to-list()
end
fun mtx-to-vector(m :: Matrix):
  m.to-vector()
end
fun mtx-to-lists(m :: Matrix):
  m.to-lists()
end
fun mtx-to-vectors(m :: Matrix):
  m.to-vectors()
end
fun mtx-row(m :: Matrix, i :: Number):
  m.row(i)
end
fun mtx-col(m :: Matrix, j :: Number):
  m.col(j)
end
fun mtx-submatrix(m :: Matrix, loi :: List<Number>, loj :: List<Number>):
  m.submatrix(loi, loj)
end
fun mtx-transpose(m :: Matrix):
  m.transpose()
end
fun mtx-hermitian(m :: Matrix):
  m.hermitian()
end
fun mtx-diagonal(m :: Matrix):
  m.diagonal()
end
fun mtx-upper-triangle(m :: Matrix):
  m.upper-triangle()
end
fun mtx-lower-triangle(m :: Matrix):
  m.lower-triangle()
end
fun mtx-row-list(m :: Matrix):
  m.row-list()
end
fun mtx-col-list(m :: Matrix):
  m.col-list()
end
# func goes first here to be consistent w/ lists.map
fun mtx-map(func :: (Number -> Number), m :: Matrix):
  m.map(func)
end
fun mtx-map2(func :: (Number, Number -> Number), m :: Matrix, n :: Matrix):
  m.map2(n, func)
end
fun mtx-row-map(func :: (Matrix -> Matrix), m :: Matrix):
  m.row-map(func)
end
fun mtx-col-map(func :: (Matrix -> Matrix), m :: Matrix):
  m.col-map(func)
end
fun mtx-augment(m1 :: Matrix, m2 :: Matrix):
  m1.augment(m2)
end
fun mtx-stack(m1 :: Matrix, m2 :: Matrix):
  m1.stack(m2)
end
fun mtx-trace(m :: Matrix):
  m.trace()
end
fun mtx-scale(m :: Matrix, scalar :: Number):
  m.scale(scalar)
end
fun mtx-dot(m1 :: Matrix, m2 :: Matrix):
  m1.dot(m2)
end
fun mtx-expt(m :: Matrix, power :: Nat):
  m.expt(power)
end
fun mtx-determinant(m :: Matrix):
  m.determinant()
end
fun mtx-is-invertible(m :: Matrix):
  m.is-invertible()
end
fun mtx-is-orthonormal(m :: Matrix):
  m.is-orthonormal()
end
fun mtx-rref(m :: Matrix):
  m.rref()
end
fun mtx-inverse(m :: Matrix):
  m.inverse()
end
fun mtx-solve(m1 :: Matrix, m2 :: Matrix):
  m1.solve(m2)
end
fun mtx-least-squares-solve(m1 :: Matrix, m2 :: Matrix):
  m1.least-squares-solve(m2)
end
fun mtx-lp-norm(m :: Matrix, power :: Number):
  m.lp-norm(power)
end
fun mtx-l1-norm(m :: Matrix):
  m.l1-norm()
end
fun mtx-taxicab-norm(m :: Matrix):
  m.l1-norm()
end
fun mtx-l2-norm(m :: Matrix):
  m.l2-norm()
end
fun mtx-euclidean-norm(m :: Matrix):
  m.l2-norm()
end
fun mtx-l-inf-norm(m :: Matrix):
  m.l-inf-norm()
end
fun mtx-lu-decomposition(m :: Matrix):
  m.lu-decomposition()
end
fun mtx-qr-decomposition(m :: Matrix):
  m.qr-decomposition()
end
fun mtx-gram-schmidt(m :: Matrix):
  m.gram-schmidt()
end
fun mtx-add(m1 :: Matrix, m2 :: Matrix):
  m1 + m2
end
fun mtx-sub(m1 :: Matrix, m2 :: Matrix):
  m1 - m2
end
fun mtx-mult(m1 :: Matrix, m2 :: Matrix):
  m1 * m2
end

fun mtx-row-echelon(m :: Matrix, use-jordan :: Boolean, unitize-pivot :: Boolean, pivoting :: Pivoting):
  elts-array = matrix-to-2d-array(m)
  ans = MU.gauss-elim(elts-array, use-jordan, unitize-pivot, pivoting)
  matrix-from-2d-array(ans.elim, m.rows, m.cols)
end

fun mtx-rank(m :: Matrix) -> Nat:
  elts-array = matrix-to-2d-array(m)
  ans = MU.gauss-elim(elts-array, false, false, MU.largest-element)
  m.cols - ans.non-pivots.length()
end

fun mtx-nullity(m :: Matrix) -> Nat:
  elts-array = matrix-to-2d-array(m)
  ans = MU.gauss-elim(elts-array, false, false, MU.largest-element)
  ans.non-pivots.length()
end
