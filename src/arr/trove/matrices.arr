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

provide {
  matrix: mk-mtx,
  vector: vector,
  vector3d: vector3d,
  vector-within: vector-within,
  is-matrix: is-matrix,
  is-row-matrix: is-row-matrix,
  is-col-matrix: is-col-matrix,
  is-square-matrix: is-square-matrix,
  identity-matrix: identity-matrix,
  make-matrix: make-matrix,
  build-matrix: build-matrix,
  vector-to-matrix: vector-to-matrix,
  list-to-matrix: list-to-matrix,
  list-to-row-matrix: list-to-row-matrix,
  list-to-col-matrix: list-to-col-matrix,
  lists-to-matrix: lists-to-matrix,
  vectors-to-matrix: vectors-to-matrix,
  matrix-within: matrix-within,
  row-matrix: row-matrix,
  col-matrix: col-matrix
} end
provide-types {
  Vector :: Vector,
  Vector3D :: Vector3D,
  Matrix :: Matrix
}

import Equal, NotEqual from equality
import lists as L
import valueskeleton as VS

fun nonzeronat(n :: Number): num-is-integer(n) and (n > 0) end
fun nat(n :: Number): num-is-integer(n) and (n >= 0) end

length3 = lam(v): raw-array-length(v._contents) == 3 end
type NonZeroNat = Number%(nonzeronat)
type Nat = Number%(nat)

## RawArray utility functions
fun raw-array-fold2<a, b, c>(f :: (a, b, c, Number -> a), base :: a, a1 :: RawArray<b>, a2 :: RawArray<c>, start-idx :: Number) -> a:
  doc: "Adaptation of lists.fold2 to work with RawArrays"
  fun fold2help(acc, idx):
    if (idx >= raw-array-length(a1)) or (idx >= raw-array-length(a2)):
      acc
    else:
      fold2help(f(acc, raw-array-get(a1, idx), raw-array-get(a2, idx), idx), idx + 1)
    end
  end
  fold2help(base, start-idx)
where:
  raw-array-fold2(lam(l, a, b, n): link(a, link(b, l)) end, [list:], [raw-array: 1, 2], [raw-array: 3, 4], 0)
    is [list: 2, 4, 1, 3]
end

fun raw-array-map<a, b>(f :: (a -> b), arr :: RawArray<a>) -> RawArray<b>:
  doc: "Like lists.map, but for RawArrays"
  arrlen = raw-array-length(arr)
  if arrlen == 0:
    [raw-array:]
  else:
    outvec = raw-array-of(raw-array-get(arr, 0), arrlen)
    for raw-array-fold(acc from nothing, elt from arr, idx from 0):
      raw-array-set(outvec, idx, f(elt))
      nothing
    end
    outvec
  end
end

fun raw-array-map2<a, b, c>(f :: (a, b -> c), arr1 :: RawArray<a>, arr2 :: RawArray<b>) -> RawArray<c>:
  doc: "Like lists.map, but for RawArrays"
  arrlen = num-min(raw-array-length(arr1), raw-array-length(arr2))
  if arrlen == 0:
    [raw-array:]
  else:
    outvec = raw-array-of(raw-array-get(arr1, 0), arrlen)
    for raw-array-fold2(acc from nothing, elt1 from arr1, elt2 from arr2, idx from 0):
      raw-array-set(outvec, idx, f(elt1, elt2))
      nothing
    end
    outvec
  end
end

# Precondition: _contents is nonempty
data Vector:
  | vector(_contents :: RawArray<Number>) with:

    get(self, shadow index :: Number) -> Number:
      doc: "Returns the entry in this vector at the given location"
      when (index < 0) or (index >= raw-array-length(self._contents)):
        raise("Vector index " + tostring(index) + " out of bounds for vector " + tostring(self))
      end
      raw-array-get(self._contents, index)
    end,

    dot(self, other :: Vector) -> Number:
      doc: "Returns the dot product of this vector with the given vector"
      when raw-array-length(self._contents) <> raw-array-length(other._contents):
        raise("Cannot take the dot product of vectors with different lengths: "
            + tostring(self) + " and " + tostring(other))
      end
      for raw-array-fold2(acc from 0, self-elt from self._contents, other-elt from other._contents, i from 0):
        (self-elt * other-elt) + acc
      end
    end,

    magnitude(self) -> Number:
      doc: "Returns the magnitude of this vector"
      num-sqrt(for raw-array-fold(acc from 0, elt from self._contents, i from 0):
          acc + num-sqr(elt)
        end)
    end,

    # TODO: Can this be a Vector3D (i.e. is Vector3D in scope?)
    cross(self, other :: Vector) -> Vector:
      doc: "Returns the cross product of this 3D vector with the given 3D vector"
      when not(length3(self)):
        raise("Cannot take the cross product of " + tostring(raw-array-length(self._contents))
            + "-dimensional vector: " + tostring(self))
      end
      when not(length3(other)):
        raise("Cannot take the cross product of " + tostring(raw-array-length(other._contents))
            + "-dimensional vector: " + tostring(other))
      end
      s0 = raw-array-get(self._contents, 0)
      s1 = raw-array-get(self._contents, 1)
      s2 = raw-array-get(self._contents, 2)
      o0 = raw-array-get(other._contents, 0)
      o1 = raw-array-get(other._contents, 1)
      o2 = raw-array-get(other._contents, 2)
      vector([raw-array:
          (s1 * o2) - (s2 * o1),
          (s2 * o0) - (s0 * o2),
          (s0 * o1) - (s1 * o0)])
    end,

    normalize(self) -> Vector:
      doc: "Normalizes this vector into a unit vector"
      m = self.magnitude()
      vector(for raw-array-map(elt from self._contents):
          elt / m
        end)
    end,

    scale(self, scalar :: Number) -> Vector:
      doc: "Scales this vector by the given constant"
      vector(for raw-array-map(elt from self._contents):
          elt * scalar
        end)
    end,

    _plus(self, other :: Vector) -> Vector:
      doc: "Adds the two given vectors"
      when raw-array-length(self._contents) <> raw-array-length(other._contents):
        raise("Cannot add vectors of different lengths")
      end
      vector(for raw-array-map2(self-elt from self._contents, other-elt from other._contents):
          self-elt + other-elt
        end)
    end,

    _minus(self, other :: Vector) -> Vector:
      doc: "Subtracts the two given vectors"
      when raw-array-length(self._contents) <> raw-array-length(other._contents):
        raise("Cannot add vectors of different lengths")
      end
      vector(for raw-array-map2(self-elt from self._contents, other-elt from other._contents):
          self-elt - other-elt
        end)
    end,

    equal-to(self, other :: Vector, eq):
      circa = within-rel(0.0001) # Because floating points
      basically-equal = lam(a1, a2):
        for raw-array-fold2(acc from true, elt1 from a1, elt2 from a2, idx from 0):
          acc and circa(elt1, elt2)
        end
      end
      shadow self = self._contents
      shadow other = other._contents
      if (self =~ other) or basically-equal(self, other):
        Equal
      else:
        NotEqual("Vectors are not equal")
      end
    end,

    # Not sure why this is separate, but I'm replicating the structure
    # of Matrix's equality checking
    _equals(self, other :: Vector, eq): self.equal-to(other, eq) end,

    length(self) -> Number:
      doc: "Returns the length of this vector"
      raw-array-length(self._contents)
    end,

    _output(self) -> VS.ValueSkeleton:
      VS.vs-collection("vector", raw-array-to-list(self._contents).map(VS.vs-value))
    end
end

type Vector3D = Vector%(length3)

# Internal use only!
vector-of-raw-array = vector

shadow vector = {
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

vector3d = block:
  fun bad-arity(n):
    "Cannot construct a 3D Vector with " + tostring(n) + " element"
      + (if (n <> 1): "s" else: "" end)
  end
  {
    make0: lam(): raise(bad-arity(0)) end,
    make1: lam(a): raise(bad-arity(1)) end,
    make2: lam(a, b): raise(bad-arity(2)) end,
    make3: lam(a :: Number, b :: Number, c :: Number): [vector: a, b, c] end,
    make4: lam(a, b, c, d): raise(bad-arity(4)) end,
    make5: lam(a, b, c, d, e): raise(bad-arity(5)) end,
    make : lam(arr): raise(bad-arity(raw-array-length(arr))) end
  }
end

fun vector-within(n :: Number):
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

fun make-raw-array-fold2(start1, start2, stride1, stride2, stop1, stop2):
  doc: "RawArray fold2 Implementation with striding ability"
  lam(f, base, arr1, arr2):
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

fun rc-to-index(row :: Nat, col :: Nat, m-cols :: Nat):
  (row * m-cols) + col
end

# This ought to be a runtime function...
fun raw-array-duplicate(arr :: RawArray) -> RawArray:
  # Enables pass-by-value behavior
  doc: "Returns a new copy of the given array"
  ret = raw-array-of(0, raw-array-length(arr))
  for each(i from range(0, raw-array-length(arr))):
    raw-array-set(ret, i, raw-array-get(arr, i))
  end
  ret
where:
  a = [raw-array: 1, 2, 3]
  # Assign by value
  b = raw-array-duplicate(a)
  # Assign by reference
  c = a
  a is=~ b
  a is=~ c
  raw-array-set(a, 0, 7)
  # Values now differ
  a is-not=~ b
  # Reference has new value
  a is=~ c
end

fun list-to-raw-array(lst :: List) -> RawArray:
  doc: "Converts the given list into a RawArray"
  raw-arr = raw-array-of(0, lst.length())
  for each(i from range(0, lst.length())):
    raw-array-set(raw-arr, i, lst.get(i))
  end
  raw-arr
where:
  list-to-raw-array([list: 1, 2, 3]) is=~ [raw-array: 1, 2, 3]
end

data Matrix:
  | matrix(rows :: NonZeroNat, cols :: NonZeroNat, elts :: RawArray<Number>)
    with:
    get(self, i :: Number, j :: Number) -> Number:
      doc: "Returns the matrix's entry in the i'th row and j'th column"
      raw-array-get(self.elts, rc-to-index(i, j, self.cols))
    end,
    
    to-list(self) -> List<Number>:
      doc: "Returns the matrix as a list of numbers"
      raw-array-to-list(self.elts)
    end,
    
    to-vector(self) -> Vector:
      doc: "Returns the one-row/one-column matrix as a vector"
      if ((self.cols == 1) <> (self.rows == 1)):
        vector-of-raw-array(self.elts)
      else:
        raise("Cannot convert non-vector matrix to vector")
      end
    end,
    
    to-lists(self) -> List<List<Number>>:
      doc: "Returns the matrix as a list of lists of numbers, with each list corresponding to a row"
      self.row-list().map(lam(r): r.to-list() end)
    end,
    
    to-vectors(self) -> List<Vector>:
      doc: "Returns the matrix a a list of vectors, with each vector corresponding to a column"
      self.col-list().map(lam(c): c.to-vector() end)
    end,
    
    row(self, i :: Number) -> Matrix:
      doc: "Returns a one-row matrix with the matrix's i'th row"
      raw-arr = raw-array-of(0, self.cols)
      for each(n from range(0, self.cols)):
        raw-array-set(raw-arr, n, raw-array-get(self.elts, rc-to-index(i, n, self.cols)))
      end
      matrix(1, self.cols, raw-arr)
    end,
    col(self, j :: Number) -> Matrix:
      doc: "Returns a one-column matrix with the matrix's j'th column"
      raw-arr = raw-array-of(0, self.rows)
      for each(n from range(0, self.rows)):
        raw-array-set(raw-arr, n, raw-array-get(self.elts, rc-to-index(n, j, self.cols)))
      end
      matrix(self.rows, 1, raw-arr)
    end,
    
    submatrix(self, loi :: List<Number>, loj :: List<Number>) -> Matrix:
      doc: "Returns the matrix's submatrix, containing the rows in the first list and the columns in the second"
      loil = loi.length()
      lojl = loj.length()
      raw-arr = raw-array-of(0, (loil * lojl))
      # Loops manually unwrapped to avoid excessive allocations
      fun fetch-row(r :: Number):
        when r < loil:
          n = loi.get(r)
          fun fetch-cols(c :: Number):
            when c < lojl:
              colnum = loj.get(c)
              raw-array-set(raw-arr, (r * lojl) + c, 
                raw-array-get(self.elts, rc-to-index(n, colnum, self.cols)))
              fetch-cols(c + 1)
            end
          end
          fetch-cols(0)
          fetch-row(r + 1)
        end
      end
      fetch-row(0)
      matrix(loil, lojl, raw-arr)
    end,
    
    transpose(self) -> Matrix:
      doc: "Returns the matrix's transpose"
      raw-arr = raw-array-of(0, (self.rows * self.cols))
      for each(r from range(0, self.rows)):
        for each(c from range(0, self.cols)):
          raw-array-set(raw-arr, rc-to-index(c, r, self.rows),
            raw-array-get(self.elts, rc-to-index(r, c, self.cols)))
        end
      end
      matrix(self.cols, self.rows, raw-arr)
    end,

    # TODO: Change to for(...) loops
    diagonal(self) -> Matrix:
      doc: "Returns a one-row matrix with the matrix's diagonal entries"
      num-diag = num-min(self.rows, self.cols)
      raw-arr = raw-array-of(0, num-diag)
      fun fetch-diag(n :: Number):
        when n < num-diag:
          raw-array-set(raw-arr, n,
            raw-array-get(self.elts, rc-to-index(n, n, self.cols)))
          fetch-diag(n + 1)
        end
      end
      fetch-diag(0)
      matrix(1, num-diag, raw-arr)
    end,
    
    upper-triangle(self) -> Matrix:
      doc: "Returns the upper triangle of the matrix"
      if not(is-square-matrix(self)):
        raise("Cannot make a non-square upper-triangular matrix")
      else:
        raw-arr = raw-array-of(0, (self.rows * self.cols))
        fun set-row(r :: Number):
          when r < self.rows:
            fun set-col(nonzeros :: Number, on-col :: Number):
              when nonzeros > 0:
                raw-array-set(raw-arr, rc-to-index(r, on-col, self.cols),
                  raw-array-get(self.elts, rc-to-index(r, on-col, self.cols)))
                set-col(nonzeros - 1, on-col - 1)
              end
            end
            set-col((self.rows - r), self.cols - 1)
            set-row(r + 1)
          end
        end
        set-row(0)
        matrix(self.rows, self.cols, raw-arr)
      end
    end,
    
    lower-triangle(self) -> Matrix:
      doc: "Returns the lower triangle of the matrix"
      if not(is-square-matrix(self)):
        raise("Cannot make a non-square lower-triangular matrix")
      else:
        raw-arr = raw-array-of(0, (self.rows * self.cols))
        fun set-row(r :: Number):
          when r < self.rows:
            fun set-col(nonzeros :: Number, on-col :: Number):
              when nonzeros > 0:
                raw-array-set(raw-arr, rc-to-index(r, on-col, self.cols),
                  raw-array-get(self.elts, rc-to-index(r, on-col, self.cols)))
                set-col(nonzeros - 1, on-col + 1)
              end
            end
            set-col((r + 1), 0)
            set-row(r + 1)
          end
        end
        set-row(0)
        matrix(self.rows, self.cols, raw-arr)
      end
    end,
    
    row-list(self) -> List<Matrix>:
      doc: "Returns the matrix as a list of one-row matrices"
      for map(i from range(0, self.rows)):
        self.row(i)
      end
    end,
    
    col-list(self) -> List<Matrix>:
      doc: "Returns the matrix as a list of one-column matrices"
      for map(i from range(0, self.cols)):
        self.col(i)
      end
    end,
    
    map(self, func):
      doc: "Maps the given function over each entry in the matrix"
      self-len = raw-array-length(self.elts)
      raw-arr = raw-array-of(0, self-len)
      for each(i from range(0, self-len)):
        raw-array-set(raw-arr, i, func(raw-array-get(self.elts, i)))
      end
      matrix(self.rows, self.cols, raw-arr)
    end,
    
    row-map(self, func :: (Matrix -> Matrix)):
      doc: "Maps the given function over each row in the matrix"
      to-stack = self.row-list().map(func)
      for fold(acc from to-stack.get(0), cur from to-stack.rest):
        acc.stack(cur)
      end
    end,
    
    col-map(self, func :: (Matrix -> Matrix)):
      doc: "Maps the given function over each column in the matrix"
      to-aug = self.col-list().map(func)
      for fold(acc from to-aug.get(0), cur from to-aug.rest):
        acc.augment(cur)
      end
    end,
    
    augment(self, other :: Matrix):
      doc: "Augments the given matrix onto the matrix"
      if not(self.rows == other.rows):
        raise("Cannot augment matrices with different numbers of rows")
      else:
        new-size = ((self.cols + other.cols) * self.rows)
        raw-arr = raw-array-of(0, new-size)
        fun get-el(n :: Number):
          col = num-modulo(n, (self.cols + other.cols))
          row = num-floor(n / (self.cols + other.cols))
          if col >= self.cols:
            raw-array-get(other.elts, (other.cols * row) + num-modulo(col, self.cols))
          else:
            raw-array-get(self.elts, (self.cols * row) + col)
          end
        end
        for each(i from range(0, new-size)):
          raw-array-set(raw-arr, i, get-el(i))
        end
        matrix(self.rows, (self.cols + other.cols), raw-arr)
      end
    end,
    
    stack(self, other :: Matrix):
      doc: "Returns the matrix stacked on top of the given matrix"
      if not(self.cols == other.cols):
        raise("Cannot stack matrices with different row lengths")
      else:
        new-size = ((self.rows + other.rows) * self.cols)
        old-size = raw-array-length(self.elts)
        raw-arr = raw-array-of(0, new-size)
        fun get-el(n :: Number):
          if n < old-size:
            raw-array-get(self.elts, n)
          else:
            raw-array-get(other.elts, (n - old-size))
          end
        end
        for each(i from range(0, new-size)):
          raw-array-set(raw-arr, i, get-el(i))
        end
        matrix((self.rows + other.rows), self.cols, raw-arr)
      end
    end,
    
    trace(self) -> Number:
      doc: "Returns the trace of the matrix (sum of the diagonal values)"
      for L.fold(acc from 0, i from range(0, num-min(self.rows, self.cols))):
        acc + raw-array-get(self.elts, rc-to-index(i, i, self.cols))
      end
    end,
    
    scale(self, k :: Number) -> Matrix:
      doc: "Scales the matrix by the given value"
      self.map(_ * k)
    end,
    
    dot(self, b :: Matrix) -> Number:
      doc: "Returns the Frobenius Product of the matrix with the given matrix"
      when (not(self.rows == b.rows) or not(self.cols == b.cols)):
        raise("Cannot return the inner product of two different-sized matrices")
      end
      (self * b.transpose()).trace()
    end,
    
    
    # Arithmetic Operators
    _plus(self, b :: Matrix) -> Matrix:
      doc: "Adds the given matrix to the matrix"
      when (not(self.rows == b.rows)
          or not(self.cols == b.cols)):
        raise("Cannot add two different sized matricies")
      end
      size = raw-array-length(self.elts)
      raw-arr = raw-array-of(0, size)
      for each(i from range(0, size)):
        raw-array-set(raw-arr, i,
          (raw-array-get(self.elts, i) +
            raw-array-get(b.elts, i)))
      end
      matrix(self.rows, self.cols, raw-arr)
    end,
    
    _minus(self, b :: Matrix) -> Matrix:
      doc: "Subtracts the given matrix from the matrix"
      when (not(self.rows == b.rows)
          or not(self.cols == b.cols)):
        raise("Cannot add two different sized matricies")
      end
      size = raw-array-length(self.elts)
      raw-arr = raw-array-of(0, size)
      for each(i from range(0, size)):
        raw-array-set(raw-arr, i,
          (raw-array-get(self.elts, i) -
            raw-array-get(b.elts, i)))
      end
      matrix(self.rows, self.cols, raw-arr)
    end,
    
    _times(self, b :: Matrix) -> Matrix:
      #C_ik = Sum(A_ij * B_jk)
      doc: "Multiplies the matrix by the given matrix"
      when not(self.cols == b.rows):
        raise("Matrix multiplication is undefined for the given inputs")
      end
      new-size = (self.rows * b.cols)
      raw-arr = raw-array-of(0, new-size)
      for each(i from range(0, self.rows)):
        for each(k from range(0, b.cols)):
          strided-fold = make-raw-array-fold2(i * self.cols, k, 1, b.cols, ((i + 1) * self.cols) - 1, (b.rows * b.cols) + k)
          raw-array-set(raw-arr, rc-to-index(i, k, b.cols),
            for strided-fold(base from 0, self_ij from self.elts, b_jk from b.elts):
              base + (self_ij * b_jk)
            end)
        end
      end
      matrix(self.rows, b.cols, raw-arr)
    end,
    
    expt(self, n :: Number) -> Matrix:
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
    
    determinant(self) -> Number:
      doc: "Returns the determinant of the matrix"
      # Recursive implementation of Laplace Expansion
      if not(is-square-matrix(self)):
        raise("Cannot take the determinant of a non-square matrix")
      else:
        ask:
          | self.rows == 2 then:
            (self.get(0, 0) * self.get(1, 1)) -
            (self.get(0, 1) * self.get(1, 0))
          | otherwise:
            for raw-array-fold(acc from 0, cur from self.row(0).elts, n from 0):
              (num-expt(-1, n) * (cur * self.submatrix(
                    range( 1, self.rows ), 
                    range( 0, n) + range(n + 1, self.cols) ).determinant())) + acc
            end
        end
      end
    end,
    
    is-invertible(self) -> Boolean:
      doc: "Returns true if the given matrix is invertible"
      is-square-matrix(self) and not(within(0.0001)(self.determinant(), 0))
    end,
    
    rref(self) -> Matrix:
      doc: "Returns the Reduced Row Echelon Form of the Matrix"
      # Copy matrix into new one to avoid game of temporary
      #      matrix hot potato between helper functions
      to-ret = matrix(self.rows, self.cols, self.elts)
      
      fun row-range(row :: Nat):
        doc: "Returns the index range for the given row"
        range(rc-to-index(row, 0, to-ret.cols),
          rc-to-index(row, to-ret.cols, to-ret.cols))
      end
      
      fun leading-col(row :: Nat):
        doc: "Returns the leading nonzero column of the given row"
        for fold(acc from (to-ret.cols - 1), cur from range(0, to-ret.cols).reverse()):
          if not(within(0.0001)(to-ret.get(row, cur), 0)):
            cur
          else:
            acc
          end
        end
      end
      
      fun mult(row :: Nat, scalar :: Number):
        doc: "Multiplies the given row by the given scalar"
        for each(j from row-range(row)):
          raw-array-set(to-ret.elts, j,
            raw-array-get(to-ret.elts, j) * scalar)
        end
      end
      
      fun divide-to-one(row :: Nat):
        doc: "Divides the given row such that the leading entry is a one"
        mult(row, 1 / to-ret.get(row, leading-col(row)))
      end
      
      fun sub-mult(sub-from :: Nat, subbing :: Nat, scalar :: Number):
        doc: "Subtracts a multiple of the second row from the first"
        for each2(i from row-range(sub-from), j from row-range(subbing)):
          raw-array-set(to-ret.elts, i,
            (raw-array-get(to-ret.elts, i) -
              (raw-array-get(to-ret.elts, j) * scalar)))
        end
      end
      
      fun reduce-row(to-reduce :: Nat, based-on :: Nat):
        doc: "Reduces the given row such that it has a 0 where the based-on row has its leading entry"
        sub-mult(to-reduce, based-on, to-ret.get(to-reduce, leading-col(based-on)))
      end
      
      for each(i from range(0, to-ret.rows)):
        divide-to-one(i)
        for each(k from (range(0,i) + range(i + 1, to-ret.rows))):
          reduce-row(k, i)
        end
      end
      
      to-ret
    end,

    # FIXME: Use submatrix
    inverse(self) -> Matrix:
      when not(self.is-invertible()):
        raise("Cannot find inverse of non-invertible matrix")
      end
      to-ret = matrix(self.rows, self.cols, raw-array-duplicate(self.elts))
      to-aug = identity-matrix(self.rows)
      to-chop = to-ret.augment(to-aug).rref().col-list().drop(self.cols)
      for fold(acc from to-chop.first, cur from to-chop.rest):
        acc.augment(cur)
      end
    end,
    
    solve(self, b :: Matrix) -> Matrix:
      doc: "Returns the matrix needed to multiply with this matrix to receive the given matrix (i.e. the solution for a system of equations), if it exists"
      when not(self.is-invertible()):
        raise("Cannot find exact solution for non-invertible matrix")
      end
      self.inverse() * b
    end,
    
    least-squares-solve(self, b :: Matrix) -> Matrix:
      doc: "Returns the least-squares solution for this and the given matrix"
      # Take the QR Decomposition and pull out results (and transpose Q)
      qrres = self.qr-decomposition()
      qt = qrres.Q.transpose()
      r = qrres.R
      # Solve for x: r * x = qt * b
      r.inverse() * (qt * b)
    end,
    
    # While perhaps not the most useful in practice,
    #   multi-column matricies do have norms defined,
    #   so they are defined here.    
    lp-norm(self, p :: Number) -> Number:
      doc: "Computes the Lp norm of the matrix"
      summed = for raw-array-fold(acc from 0, cur from self.elts, i from 0):
        acc + num-expt(cur, p)
      end
      num-expt(summed, (1 / p))
    end,
    
    l1-norm(self) -> Number:
      doc: "Computes the L1 norm (or Taxicab norm) of the matrix"
      self.lp-norm(1)
    end,
    
    l2-norm(self) -> Number:
      doc: "Computes the L2 norm (or Euclidean norm) of the matrix"
      self.lp-norm(2)
    end,
    
    l-inf-norm(self) -> Number:
      doc: "Computes the L-Infinity norm of the matrix"
      raw-array-fold(lam(acc, cur, i): num-max(acc, num-abs(cur)) end, 0, self.elts, 0)
    end,

    qr-decomposition(self) -> {Q :: Matrix, R :: Matrix}:
      doc: "Returns the QR Decomposition of the matrix"
      # (Ample comments because debugging)
      q-arr = raw-array-duplicate(self.elts) # Copy Current List of elements for modification
      r-arr = raw-array-of(0, raw-array-length(self.elts))
      cols = self.col-list()
      first-col = cols.get(0).to-vector() # Get the first column
      raw-array-set(r-arr, 0, first-col.magnitude()) # Store its magnitude in R Matrix
      norm-first = first-col.normalize()
      for each(i from range(0, self.rows)):
        n = norm-first.get(i)
        raw-array-set(q-arr, rc-to-index(i, 0, self.cols), n) # Normalize and store in Q Matrix
      end
      fun get-q-col(j :: Nat) -> Vector:
        doc: "Returns the given column in the Q matrix as a vector"
        outarr = raw-array-of(0, self.rows)
        for each(n from range(0, self.rows)):
          raw-array-set(outarr, n, raw-array-get(q-arr, rc-to-index(n, j, self.cols)))
        end
        vector-of-raw-array(outarr)
      end
      for each(i from range(1, self.cols)):
        v-vec = cols.get(i).to-vector() # Convert the current column into a vector (needs to be able to dot)
        first-dot = v-vec.dot(get-q-col(0)) # Dot the current vector with the first unit vector
        raw-array-set(r-arr, rc-to-index(0, i, self.cols), first-dot) # Store dot in R Matrix
        sum-mults = for fold(acc from get-q-col(0).scale(first-dot), j from range(1, i)): # Loop over all 
                                                                                          #   already-established
                                                                                          #   unit vectors
          dotted = v-vec.dot(get-q-col(j)) # Dot the current vector with the given unit vector
          raw-array-set(r-arr, rc-to-index(j, i, self.cols), dotted) # Store dot in R matrix
          (acc + get-q-col(j).scale(dotted)) # Add the scaled unit vector to the accumulated result
        end
        v-perp = cols.get(i).to-vector() - sum-mults # Subtract the accumulated result
                                                                #   from the current vector to get
                                                                #   its perpendicular component
        raw-array-set(r-arr, rc-to-index(i, i, self.cols), v-perp.magnitude()) # Store magnitude in R matrix
        v-perp-norm = v-perp.normalize()
        for each(k from range(0, self.rows)):
          n = v-perp-norm.get(k)
          raw-array-set(q-arr, rc-to-index(k, i, self.cols), n) # Store Normalized Perp. Component in Q Matrix
        end
      end
      # Shrink R Matrix to a (self.cols * self.cols) square matrix
      r-ret-arr = raw-array-of(0, (self.cols * self.cols))
      for each(i from range(0, (self.cols * self.cols))):
        raw-array-set(r-ret-arr, i, raw-array-get(r-arr, i))
      end
      { Q : matrix(self.rows, self.cols, q-arr), R : matrix(self.cols, self.cols, r-ret-arr)}
    end,
          
    
    gram-schmidt(self) -> Matrix:
      doc: "Returns an orthogonal matrix whose image is the same as the span of the Matrix's columns"
      self.qr-decomposition().Q
    end,
    
    _torepr(self, shadow torepr) -> String:
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

    _output(self):
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
      VS.vs-str("\n" + for raw-array-fold(acc from "", n from self.elts, i from 0):
          acc + elt-tostr(i)
      end)
    end,
    
    equal-to(self, a :: Matrix, eq):
      circa = within-rel(0.0001) #Because floating points
      basically-equal = lam(l1, l2): fold2(lam(x, y, z): x and circa(y, z) end, true, l1, l2) end
      if ((self.to-list() == a.to-list()) or basically-equal(self.to-list(), a.to-list())):
        Equal
      else:
        NotEqual("Matrices are not equal")
      end
    end,
    
    _equals(self, a :: Matrix, eq):
      self.equal-to(a, eq)
    end
end

fun identity-matrix(n :: NonZeroNat):
  doc: "Returns the identity matrix of the given size"
  fun id(r :: Number, c :: Number) -> RawArray:
    raw-arr = raw-array-of(0, (r * c))
    for each(i from range(0, r)):
      raw-array-set(raw-arr, (i * (c + 1)), 1)
    end
    raw-arr
  end
  matrix(n, n, id(n, n))
end

fun mk-mtx(rows :: NonZeroNat, cols :: NonZeroNat):
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
  # Implement all constructors so we only allocate RawArrays if
  # absolutely necessary
  var make0 = lam(): raise(invalid-msg(0)) end
  var make1 = lam(a): raise(invalid-msg(1)) end
  var make2 = lam(a, b): raise(invalid-msg(2)) end
  var make3 = lam(a, b, c): raise(invalid-msg(3)) end
  var make4 = lam(a, b, c, d): raise(invalid-msg(4)) end
  var make5 = lam(a, b, c, d, e): raise(invalid-msg(5)) end
  var make  = lam(arr): raise(invalid-msg(raw-array-length(arr))) end
  expected = rows * cols
  ask:
    | expected == 0 then: raise("Impossible") # NonZeroNat should handle this
    | expected == 1 then: make1 := lam(a): matrix(rows, cols, [raw-array: a]) end
    | expected == 2 then: make2 := lam(a, b): matrix(rows, cols, [raw-array: a, b]) end
    | expected == 3 then: make3 := lam(a, b, c): matrix(rows, cols, [raw-array: a, b, c]) end
    | expected == 4 then: make4 := lam(a, b, c, d): matrix(rows, cols, [raw-array: a, b, c, d]) end
    | expected == 5 then: make5 := lam(a, b, c, d, e): matrix(rows, cols, [raw-array: a, b, c, d, e]) end
    | otherwise: make := lam(arr):
        if not(raw-array-length(arr) == (rows * cols)):
          raise(invalid-msg(raw-array-length(arr)))
        else:
          matrix(rows, cols, arr)
        end
      end
  end
  {
    make0: make0,
    make1: make1,
    make2: make2,
    make3: make3,
    make4: make4,
    make5: make5,
    make : make
  }
where:
  [mk-mtx(1, 1): 1] is matrix(1, 1, [raw-array: 1])
  [mk-mtx(3, 1): 1, 
                2,
                3] is matrix(3, 1, [raw-array: 1, 2, 3])
  [mk-mtx(1, 3): 3, 2, 1] is matrix(1, 3, [raw-array: 3, 2, 1])
  [mk-mtx(2, 2): 1, 2, 
                3, 4] is matrix(2, 2, [raw-array: 1, 2, 3, 4])
end

row-matrix =
  {
    make: lam(arr :: RawArray):
        matrix(1, raw-array-length(arr), arr)
      end,
    make0: lam(): raise("Invalid Matrix Input: Cannot construct a zero-length row matrix") end,
    make1: lam(a): matrix(1, 1, [raw-array: a]) end,
    make2: lam(a, b): matrix(1, 2, [raw-array: a, b]) end,
    make3: lam(a, b, c): matrix(1, 3, [raw-array: a, b, c]) end,
    make4: lam(a, b, c, d): matrix(1, 4, [raw-array: a, b, c, d]) end,
    make5: lam(a, b, c, d, e): matrix(1, 5, [raw-array: a, b, c, d, e]) end
  }

col-matrix =
  {
    make: lam(arr :: RawArray):
        matrix(raw-array-length(arr), 1, arr)
      end,
    make0: lam(): raise("Invalid Matrix Input: Cannot construct a zero-length column matrix") end,
    make1: lam(a): matrix(1, 1, [raw-array: a]) end,
    make2: lam(a, b): matrix(2, 1, [raw-array: a, b]) end,
    make3: lam(a, b, c): matrix(3, 1, [raw-array: a, b, c]) end,
    make4: lam(a, b, c, d): matrix(4, 1, [raw-array: a, b, c, d]) end,
    make5: lam(a, b, c, d, e): matrix(5, 1, [raw-array: a, b, c, d, e]) end
  }

fun make-matrix(rows :: NonZeroNat, cols :: NonZeroNat, x :: Number):
  doc: "Constructs a matrix of the given size with the given elements"
  matrix(rows, cols, raw-array-of(x, (rows * cols)))
where:
  make-matrix(2, 3, 1) is [mk-mtx(2, 3): 1, 1, 1, 1, 1, 1]
  make-matrix(3, 2, 5) is [mk-mtx(3, 2): 5, 5, 5, 5, 5, 5]
end

fun build-matrix(rows :: NonZeroNat, cols :: NonZeroNat, proc :: (Number, Number -> Number)):
  doc: "Constructs a matrix of the given size, where entry (i, j) is the result of proc(i, j)"
  raw-arr = raw-array-of(0, (rows * cols))
  for each(i from range(0, rows)):
    for each(j from range(0, cols)):
      raw-array-set(raw-arr, rc-to-index(i, j, cols), proc(i, j))
    end
  end
  matrix(rows, cols, raw-arr)
where:
  build-matrix(3, 2, lam(i, j): i + j end) is
  [mk-mtx(3, 2): 0, 1, 1, 2, 2, 3]
end

fun vector-to-matrix(v :: Vector):
  doc: "Converts the given vector into a one-row matrix"
  matrix(1, raw-array-length(v._contents), raw-array-duplicate(v._contents))
where:
  vector-to-matrix([vector: 1, 2, 3]) is
  [mk-mtx(1, 3): 1, 2, 3]
end

fun list-to-matrix(rows :: NonZeroNat, cols :: NonZeroNat, lst :: List<Number>):
  doc: "Converts the given list of numbers into a matrix of the given size"
  when not(lst.length() == (rows * cols)):
    raise("Provided list does not have arguments corresponding to provided matrix size")
  end
  matrix(rows, cols, list-to-raw-array(lst))
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

fun lists-to-matrix(lst :: List<List<Number>>):
  doc: "Converts the given list of lists into a matrix, with each contained list as a row"
  rows = lst.length()
  cols = lst.get(0).length()
  # Check that all Lists are the same length
  when fold(lam(r, f): not(f.length() == cols) or r end, false, lst):
    raise("Invalid Matrix input")
  end
  matrix(rows, cols, list-to-raw-array(lst.foldr(_ + _, empty)))
where:
  lists-to-matrix([list: [list: 1, 2, 3], [list: 4, 5, 6]]) is
  [mk-mtx(2, 3): 1, 2, 3, 4, 5, 6]
  
  lists-to-matrix([list: [list: 1, 2, 3], [list: 1, 2]]) raises
  "Invalid Matrix input"
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
fun vectors-to-matrix(lst :: List<Vector>):
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
  vectors-to-matrix([list: [vector: 1, 2, 3], [vector: 4, 5, 6]]) is
  [mk-mtx(3, 2): 1, 4, 2, 5, 3, 6]
end

fun matrix-within(n :: Number) -> (Matrix, Matrix -> Boolean):
  lam(a, b):
    if (raw-array-length(a.elts) == raw-array-length(b.elts)):
      f = make-raw-array-fold2(0, 0, 1, 1, raw-array-length(a.elts) - 1, raw-array-length(b.elts) - 1)
      wn = within(n)
      f(lam(acc, a-elt, b-elt): wn(a-elt, b-elt) and acc end, true, a.elts, b.elts)
    else:
      false
    end
  end
end