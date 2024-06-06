provide:
  find-first-pivot,
  find-partial-pivot,
  gauss-elim,
  elim-rows,
  determinant,
  lu-decomposition,
  gram-schmidt,
  qr-decomposition,
  identity,
  plus,
  minus,
  times,
  data Pivoting,
end
import global as G
include from G:
  raw-array-concat,
  raw-array-sort-by,
  raw-array-from-list,
  raw-each-loop,
  raw-array-duplicate,
end
import lists as L
include from L:
  link,
  empty,
  list,
  type List
end
import vector-util as VU
include from VU:
  raw-array-fold2,
  raw-array-map2,
  type *
end
provide from VU:
  type Nat,
  type NonZeroNat
end


type RawMatrix = RawArray<RawArray<Number>>

fun get-2d(elts, row, col):
  raw-array-get(raw-array-get(elts, row), col)
end

fun set-2d(elts, row, col, val):
  raw-array-set(raw-array-get(elts, row), col, val)
end

fun is-square(elts :: RawMatrix) -> Boolean:
  num-rows = raw-array-length(elts)
  (num-rows > 0) and (num-rows == raw-array-length(raw-array-get(elts, 0)))
end

fun is-zero(elts :: RawMatrix) -> Boolean:
  raw-array-and-mapi({(v, _): VU.is-zero(v)}, elts, 0)
end

fun plus(elts1 :: RawMatrix, elts2 :: RawMatrix) -> RawMatrix:
  doc: "Computes the sum of elts1 and elts2, assuming they are the same size"
  num-rows = raw-array-length(elts1)
  num-cols = raw-array-length(raw-array-get(elts1, 0))
  raw-array-build(lam(r):
      raw-array-build(lam(c): get-2d(elts1, r, c) + get-2d(elts2, r, c) end, num-cols)
    end, num-rows)
end


fun minus(elts1 :: RawMatrix, elts2 :: RawMatrix) -> RawMatrix:
  doc: "Computes the difference of elts1 and elts2, assuming they are the same size"
  num-rows = raw-array-length(elts1)
  num-cols = raw-array-length(raw-array-get(elts1, 0))
  raw-array-build(lam(r):
      raw-array-build(lam(c): get-2d(elts1, r, c) - get-2d(elts2, r, c) end, num-cols)
    end, num-rows)
end

fun times(elts1 :: RawMatrix, elts2 :: RawMatrix) -> RawMatrix:
  doc: "Computes the matrix product of elts1 and elts2, assuming they are compatible size"
  num-rows = raw-array-length(elts1)
  num-mid = raw-array-length(elts2)
  elts2-transpose = transpose(elts2)
  num-cols = raw-array-length(elts2-transpose)
  raw-array-build(lam(r):
      raw-array-build(lam(c):
          VU.dot(raw-array-get(elts1, r), raw-array-get(elts2-transpose, c))
        end, num-cols)
    end, num-rows)  
end

fun identity(n :: NonZeroNat) -> RawMatrix:
  one-diagonal(n, n)
end

fun one-diagonal(num-rows :: NonZeroNat, num-cols :: NonZeroNat) -> RawMatrix block:
  raw-array-build(
    {(r): raw-array-build({(c): if r == c: 1 else: 0 end}, num-cols)},
    num-rows)
end

fun transpose(elts :: RawMatrix) -> RawMatrix:
  num-rows = raw-array-length(elts)
  num-cols = if num-rows == 0: 0 else: raw-array-length(raw-array-get(elts, 0)) end
  raw-array-build(lam(c):
      raw-array-build(get-2d(elts, _, c), num-rows)
    end, num-cols)
end

fun hermitian(elts :: RawMatrix) -> RawMatrix:
  transpose(elts) # since we have no complex numbers, no need to conjugate them
end

fun swap-rows(elts, src, dest) block:
  temp = raw-array-get(elts, src)
  raw-array-set(elts, src, raw-array-get(elts, dest))
  raw-array-set(elts, dest, temp)
end

fun find-first-pivot(
    rows :: Nat,
    elts :: RawMatrix,
    i :: Nat,
    j :: Nat) -> Nat:
  doc: "Finds the row-index of the first nonzero element in column j"
  pivot = get-2d(elts, i, j)
  if num-abs(pivot) > 0:
    i
  else:
    fun help(l :: Nat):
      if l < rows:
        shadow pivot = get-2d(elts, l, j)
        if num-abs(pivot) > 0:
          l
        else:
          help(l + 1)
        end
      else:
        i
      end
    end
    help(i + 1)
  end
end

fun find-partial-pivot(
    rows :: Nat,
    elts :: RawMatrix,
    i :: Nat,
    j :: Nat) -> Nat:
  doc: "Find the row-index of the element with the maximum magnitude in column j"
  fun help(l, p, pivot, mag-pivot):
    if l < rows:
      new-pivot = get-2d(elts, l, j)
      mag-new-pivot = num-abs(new-pivot)
      if mag-new-pivot > mag-pivot:
        help(l + 1, l, new-pivot, mag-new-pivot)
      else:
        help(l + 1, p, pivot, mag-pivot)
      end
    else:
      p
    end
  end
  pivot = get-2d(elts, i, j)
  mag-pivot = num-abs(pivot)
  help(i + 1, i, pivot, mag-pivot)
end

data Pivoting:
  | largest-element
  | first-nonzero-element
sharing:
  method find-pivot-row(self, rows, elts, i, j):
    cases(Pivoting) self:
      | largest-element => find-partial-pivot(rows, elts, i, j)
      | first-nonzero-element => find-first-pivot(rows, elts, i, j)
    end
  end
end


fun elim-rows(
    num-rows :: Nat,
    elts :: RawMatrix,
    i :: Nat,
    j :: Nat,
    pivot :: Number,
    start :: Number) -> Nothing:
  row-i = raw-array-get(elts, i)
  fun help(l):
    when l < num-rows block:
      when l <> i:
        row-l = raw-array-get(elts, l)
        x_lj = raw-array-get(row-l, j)
        when not(roughly-equal(x_lj, 0)) block:
          VU.add-scaled-now(row-l, row-i, -1 * (x_lj / pivot), j)
          raw-array-set(row-l, j, 0)
        end
      end
      help(l + 1)
    end
  end
  help(start)
end

fun gauss-elim(
    elts :: RawMatrix,
    use-jordan :: Boolean,
    unitize-pivot :: Boolean,
    find-pivot :: Pivoting) -> { elim :: RawMatrix, non-pivots :: List<Nat> }:
  num-rows = raw-array-length(elts)
  num-cols = raw-array-length(raw-array-get(elts, 0))
  shadow elts = raw-array-map(raw-array-duplicate, elts)
  fun loop(i, j, non-pivots):
    if j >= num-cols:
      { elim: elts, non-pivots: non-pivots.reverse() }
    else if i >= num-rows:
      fun add-remaining-pivots(shadow j, shadow non-pivots):
        if j < num-cols: add-remaining-pivots(j + 1, link(j, non-pivots))
        else: non-pivots.reverse()
        end
      end
      { elim: elts, non-pivots: add-remaining-pivots(j, non-pivots) }
    else:
      pivot-row = find-pivot.find-pivot-row(num-rows, elts, i, j)
      pivot-val = get-2d(elts, pivot-row, j)
      if roughly-equal(pivot-val, 0) block:
        loop(i, j + 1, link(j, non-pivots))
      else:
        swap-rows(elts, i, pivot-row)
        shadow pivot-val =
          if unitize-pivot block:
            VU.scale-now(raw-array-get(elts, i), 1 / pivot-val)
            pivot-val / pivot-val
          else:
            pivot-val
          end
        elim-rows(num-rows, elts, i, j, pivot-val, if use-jordan: 0 else: i + 1 end)
        loop(i + 1, j + 1, non-pivots)
      end
    end
  end
  loop(0, 0, empty)
end

fun determinant-by-elim(num-rows :: Nat, elts :: RawMatrix) -> Number:
  fun loop(i, sign):
    if i < num-rows:
      pivot-row = largest-element.find-pivot-row(num-rows, elts, i, i)
      pivot-val = get-2d(elts, pivot-row, i)
      if roughly-equal(pivot-val, 0) block:
        pivot-val
      else:
        shadow sign = if i == pivot-row block:
          sign
        else:
          swap-rows(elts, i, pivot-row)
          -1 * sign
        end
        elim-rows(num-rows, elts, i, i, pivot-val, i + 1)
        loop(i + 1, sign)
      end
    else:
      fun product(shadow i, prod):
        if i < num-rows:
          product(i + 1, prod * get-2d(elts, i, i))
        else:
          prod * sign
        end
      end
      product(1, get-2d(elts, 0, 0))
    end
  end
  loop(0, 1)
end

fun determinant(elts :: RawMatrix) -> Number:
  num-rows = raw-array-length(elts)
  ask:
    | num-rows == 1 then: get-2d(elts, 0, 0)
    | num-rows == 2 then:
      (get-2d(elts, 0, 0) * get-2d(elts, 1, 1)) -
      (get-2d(elts, 0, 1) * get-2d(elts, 1, 0))
    | num-rows == 3 then:
      (get-2d(elts, 0, 0) *
        ((get-2d(elts, 1, 1) * get-2d(elts, 2, 2)) - (get-2d(elts, 1, 2) * get-2d(elts, 2, 1))))
        + (-1 * get-2d(elts, 0, 1) *
        ((get-2d(elts, 1, 0) * get-2d(elts, 2, 2)) - (get-2d(elts, 1, 2) * get-2d(elts, 2, 0))))
        + (get-2d(elts, 0, 2) *
        ((get-2d(elts, 1, 0) * get-2d(elts, 2, 1)) - (get-2d(elts, 1, 1) * get-2d(elts, 2, 0))))
    | otherwise:
      determinant-by-elim(num-rows, elts)
  end
end

fun lu-decomposition(
    elts :: RawMatrix)
  -> { L :: RawMatrix, U :: RawMatrix }:
  doc: "Mutates elts into U, and constructs L"
  num-rows = raw-array-length(elts)
  L = raw-array-build({(_): raw-array-of(0, num-rows)}, num-rows)
  fun loop(i):
    if i < num-rows block:
      pivot-val = get-2d(elts, i, i)
      if roughly-equal(pivot-val, 0):
        raise("Cannot decompose matrix into LU factorization")
      else:
        fun l-loop(l):
          if l < num-rows block:
            x_li = get-2d(elts, l, i)
            y_li = x_li / pivot-val
            when not(roughly-equal(x_li, 0)) block:
              set-2d(L, l, i, y_li)
              VU.add-scaled-now(raw-array-get(elts, l), raw-array-get(elts, i), -1 * y_li, 0)
            end
            l-loop(l + 1)
          else:
            loop(i + 1)
          end
        end
        l-loop(i + 1)
      end
    else:
      raw-each-loop(lam(shadow i): set-2d(L, i, i, 1) end, 0, num-rows)
      { L : L, U : elts }
    end
  end
  loop(0)
end

fun find-first-nonzero-vector(vecs :: RawArray<RawVector>) -> Number:
  size = raw-array-length(vecs)
  fun loop(i):
    if i >= size: -1
    else if not(VU.is-zero(raw-array-get(vecs, i))): i
    else: loop(i + 1)
    end
  end
  loop(0)
end

fun sub-projection-now(dest :: RawVector, src :: RawVector, unit :: Boolean) block:
  t = if unit: 1 else: VU.magnitude-squared(src) end
  when not(num-is-roughnum(t)) and (t <> 0):
    s = VU.dot(dest, src) / t
    VU.add-scaled-now(dest, src, 0 - s, 0)
  end
  t
end

fun subtract-projections-now(vecs :: RawArray<RawVector>, startIdx :: Nat, endIdx :: Nat, row :: RawVector):
  raw-each-loop(lam(i):
      sub-projection-now(raw-array-get(vecs, i), row, false)
    end, startIdx, endIdx)
end
  
fun gram-schmidt-start(
    elts :: RawMatrix,
    normalize :: Boolean,
    start :: Nat)
  -> RawMatrix:
  doc: ```
       Performs Gram-Schmidt orthogonalization on M, assuming the rows before start are already
       orthogonal.
       ```
  elts-transpose = transpose(elts)
  num-elts-transpose = raw-array-length(elts-transpose)
  i = find-first-nonzero-vector(elts-transpose)
  if i < 0 block:
    [raw-array: ]
  else:
    row-i = raw-array-get(elts-transpose, i)
    subtract-projections-now(elts-transpose, num-max(start, i + 1), num-elts-transpose, row-i)
    when normalize: VU.normalize-now(row-i) end
    fun loop(shadow i, bs):
      if i < num-elts-transpose:
        shadow row-i = raw-array-get(elts-transpose, i)
        if VU.is-zero(row-i) block:
          loop(i + 1, bs)
        else:
          subtract-projections-now(elts-transpose, num-max(start, i + 1), num-elts-transpose, row-i)
          when normalize: VU.normalize-now(row-i) end
          loop(i + 1, link(row-i, bs))
        end
      else:
        transpose(raw-array-from-list(bs.reverse()))
      end
    end
    loop(i + 1, [list: row-i])
  end
end

fun gram-schmidt(
    elts :: RawMatrix,
    normalize :: Boolean
    )
  -> RawMatrix:
  gram-schmidt-start(elts, normalize, 0)
end

fun submatrix(elts :: RawMatrix,
    fromRow :: Nat,
    toRow :: Nat,
    fromCol :: Nat,
    toCol :: Nat) -> RawMatrix:
  raw-array-build(lam(r):
      raw-array-build(lam(c):
          get-2d(elts, r + fromRow, c + fromCol)
        end, toCol - fromCol)
    end, toRow - fromRow)
end

fun augment(elts1 :: RawMatrix, elts2 :: RawMatrix) -> RawMatrix:
  num-rows = raw-array-length(elts1)
  num-cols1 = raw-array-length(raw-array-get(elts1, 0))
  num-cols2 = raw-array-length(raw-array-get(elts2, 0))
  raw-array-build(lam(r):
      raw-array-build(lam(c):
          if c < num-cols1: get-2d(elts1, r, c)
          else: get-2d(elts2, r, c - num-cols1)
          end
        end, num-cols1 + num-cols2)
    end, num-rows)
end

fun stack(elts1 :: RawMatrix, elts2 :: RawMatrix) -> RawMatrix:
  raw-array-concat(elts1, elts2)
end

fun basis-extension(elts :: RawMatrix) -> RawArray<RawVector>:
  num-rows = raw-array-length(elts)
  num-cols = raw-array-length(raw-array-get(elts, 0))
  x00 = get-2d(elts, 0, 0)
  if num-cols < num-rows:
    S = gram-schmidt-start(augment(elts, identity(num-rows)), false, num-cols)
    R = submatrix(S, 0, num-rows, num-cols, raw-array-length(raw-array-get(S, 0)))
    R-cols = transpose(R)
    R-cols-sorted = raw-array-sort-by(R-cols, VU.magnitude-squared, false)
    front-R-cols-sorted = raw-array-build(raw-array-get(R-cols-sorted, _), num-rows - num-cols)
    transpose(front-R-cols-sorted)
  else if num-cols == num-rows:
    [raw-array: ]
  else:
    raise("Cannot extend row basis when width < height")
  end
end

fun upper-triangle(elts :: RawMatrix) -> RawMatrix:
  num-rows = raw-array-length(elts)
  num-cols = if num-rows == 0: 0 else: raw-array-length(raw-array-get(elts, 0)) end
  raw-array-build(lam(r):
      raw-array-build({(c): if r <= c: get-2d(elts, r, c) else: 0 end}, num-cols)
    end, num-rows)
end

fun lower-triangle(elts :: RawMatrix) -> RawMatrix:
  num-rows = raw-array-length(elts)
  num-cols = if num-rows == 0: 0 else: raw-array-length(raw-array-get(elts, 0)) end
  raw-array-build(lam(r):
      raw-array-build({(c): if r >= c: get-2d(elts, r, c) else: 0 end}, num-cols)
    end, num-rows)
end

fun qr-decomposition(
    elts :: RawMatrix,
    full :: Boolean)
  -> { Q :: RawMatrix, R :: RawMatrix }:
  x00 = get-2d(elts, 0, 0)
  B = gram-schmidt(elts, false)
  next-Q-arg = if is-square(B) or not(full): B
    else if raw-array-length(B) > 0: augment(B, basis-extension(B))
    else if full: identity(raw-array-length(elts))
    else: one-diagonal(raw-array-length(elts), 1)
    end
  Q = gram-schmidt(next-Q-arg, true)
  { Q: Q, R: upper-triangle(times(hermitian(Q), elts)) }
end
