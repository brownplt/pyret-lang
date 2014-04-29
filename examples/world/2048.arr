
import image as I
import world as W


fun row-left(a-row):
  cases(List) a-row:
    | link(first, rest) =>
      cases(List) rest:
        | link(second, rest2) =>
          if first == 0:
            row-left([second] + rest2) + [0]
          else if second == 0:
            row-left([first] + rest2) + [0]
          else if first == second:
            [first + second] + row-left(rest2) + [0]
          else:
            [first] + row-left(rest)
          end
        | empty => [first]
      end
    | empty => []
  end
where:
  row-left([0, 0, 0, 0]) is [0, 0, 0, 0]
  row-left([0, 2, 0, 0]) is [2, 0, 0, 0]
  row-left([0, 2, 0, 2]) is [4, 0, 0, 0]
  row-left([0, 0, 0, 2]) is [2, 0, 0, 0]
  row-left([2, 2, 0, 0]) is [4, 0, 0, 0]
  row-left([2, 2, 4, 4]) is [4, 8, 0, 0]
  row-left([2, 4, 4, 8]) is [2, 8, 8, 0]
  row-left([2, 4, 6, 8]) is [2, 4, 6, 8]
  row-left([2, 2, 2, 2]) is [4, 4, 0, 0]
  row-left([2, 4, 4, 2]) is [2, 8, 2, 0]
end

fun grid-left(a-grid):
  for map(row from a-grid):
    row-left(row)
  end
where:
  grid-left([
      [2, 4, 6, 8],
      [2, 2, 0, 0],
      [0, 0, 2, 2],
      [4, 4, 4, 4]
    ]) is
  [
    [2, 4, 6, 8],
    [4, 0, 0, 0],
    [4, 0, 0, 0],
    [8, 8, 0, 0]
  ]   
end
  
fun flip(a-grid):
  for map(row from a-grid):
    row.reverse()
  end
end

fun rotate(a-grid):
  new-grid-init = for map(row from a-grid): [] end
  for fold(new-grid from new-grid-init, row from a-grid):
    for map2(new-grid-row from new-grid, elt from row):
      [elt] + new-grid-row
    end
  end
where:
  rotate([[1, 2], [3, 4]]) is [[3, 1], [4, 2]]
end

fun grid-right(a-grid):
  flip(grid-left(flip(a-grid)))
where:
  grid-right([[2, 2], [4, 0]]) is [[0, 4], [0, 4]]
end

fun grid-down(a-grid):
  rotate(rotate(rotate(grid-left(rotate(a-grid)))))
where:
  grid-down([[2, 2], [2, 2]]) is [[0, 0], [4, 4]]
end

fun grid-up(a-grid):
  rotate(grid-left(rotate(rotate(rotate(a-grid)))))
where:
  grid-up([[2, 2], [2, 2]]) is [[4, 4], [0, 0]]
end

square-width = 50

fun color-of-num(n):
  if n == 0: "white"
  else if n == 2: "blue"
  else if n == 4: "green"
  else if n == 8: "red"
  else: "yellow"
  end
end

fun to-draw(a-grid):
  l = a-grid.length()
  board-side = square-width * l
  square-middle = square-width / 2
  for fold2(
      i from I.square(board-side, "solid", "black"),
      row from a-grid,
      x from range(0, l)
      ):
    for fold2(shadow i from i, elt from row, y from range(0, l)):
      text-img = I.text(tostring(elt), 12, "black")
      square-img = I.place-image(
        text-img,
        square-middle,
        square-middle,
        I.square(square-width, "solid", color-of-num(elt)))
      I.place-image(
        square-img,
        (y * square-width) + square-middle,
        (x * square-width) + square-middle,
        i)
    end
  end
end

fun on-key(a-grid, key):
  new-grid =
    if W.is-key-equal(key, "up"): grid-up(a-grid)
    else if W.is-key-equal(key, "down"): grid-down(a-grid)
    else if W.is-key-equal(key, "left"): grid-left(a-grid)
    else if W.is-key-equal(key, "right"): grid-right(a-grid)
    else: a-grid
    end
  new-grid
where:
  grid = [[2, 2], [4, 4]]
  on-key(grid, "left") is grid-left(grid)
  on-key(grid, "right") is grid-right(grid)
  on-key(grid, "up") is grid-up(grid)
  on-key(grid, "down") is grid-down(grid)
end

big-start-grid = for map(i from range(0, 24)):
  [2, 2, 4, 0, 2, 2, 4, 0, 2, 2, 4, 0, 2, 2, 4, 0, 2, 2, 4, 0, 2, 2, 4, 0]
end

W.big-bang(big-start-grid, [
    W.on-key(on-key),
    W.to-draw(to-draw),
    W.on-tick(fun(v): "keepalive" v end)
  ])


