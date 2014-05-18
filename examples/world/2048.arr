import image as I
import world as W
import image-structs as IS

TICKS-PER-ANIMATION = 10
SQUARE-WIDTH = 110
SQUARE-BORDER = 15
SQUARE-MIDDLE = (SQUARE-WIDTH / 2)

data Move:
  | row-move(row-number, start-col, end-col)
  | col-move(col-number, start-row, end-row)
end

data WorldState:
  | animating(tick :: Number, original-grid :: List<List<Number>>, target-grid :: List<List<Number>>, moves :: List<Move>)
  | waiting-for-input(grid :: List<List<Number>>)
end

fun row-shift(a-row):
  cases(List) a-row:
    | link(first, rest) =>
      cases(List) rest:
        | link(second, rest2) =>
          if first == 0:
            row-shift([list: second] + rest2) + [list: 0]
          else if second == 0:
            row-shift([list: first] + rest2) + [list: 0]
          else if first == second:
            [list: first + second] + row-shift(rest2) + [list: 0]
          else:
            [list: first] + row-shift(rest)
          end
        | empty => [list: first]
      end
    | empty => [list: ]
  end
where:
  row-shift([list: 0, 0, 0, 0]) is [list: 0, 0, 0, 0]
  row-shift([list: 0, 2, 0, 0]) is [list: 2, 0, 0, 0]
  row-shift([list: 0, 2, 0, 2]) is [list: 4, 0, 0, 0]
  row-shift([list: 0, 0, 0, 2]) is [list: 2, 0, 0, 0]
  row-shift([list: 2, 2, 0, 0]) is [list: 4, 0, 0, 0]
  row-shift([list: 2, 2, 4, 4]) is [list: 4, 8, 0, 0]
  row-shift([list: 2, 4, 4, 8]) is [list: 2, 8, 8, 0]
  row-shift([list: 2, 4, 6, 8]) is [list: 2, 4, 6, 8]
  row-shift([list: 2, 2, 2, 2]) is [list: 4, 4, 0, 0]
  row-shift([list: 2, 4, 4, 2]) is [list: 2, 8, 2, 0]
end


fun row-moves(
    row-number :: Number,
    target-value :: Number,
    target-index :: Number,
    current-index :: Number,
    a-row :: List<Number>
    ) -> List<Move>:
  cases(List) a-row:
    | link(first, rest) =>
      if first == 0:
        row-moves(
          row-number,
          target-value,
          target-index,
          current-index + 1,
          rest
          )
      else if first == target-value:
        rest-of-row = row-moves(
          row-number,
            -1,
          target-index + 1,
          current-index + 1,
          rest
          )
        link(row-move(row-number, current-index, target-index), rest-of-row)
      else if target-value == -1:
        rest-of-row = row-moves(
          row-number,
          first,
          target-index,
          current-index + 1,
          rest
          )
        link(row-move(row-number, current-index, target-index), rest-of-row)
      else:
        rest-of-row = row-moves(
          row-number,
          first,
          target-index + 1,
          current-index + 1,
          rest
          )
        link(row-move(row-number, current-index, target-index + 1), rest-of-row)
      end
    | empty => empty
  end
where:
  r = row-moves(0, -1, 0, 0, _)
  r([list: 0, 0, 0, 0]) is empty
  r([list: 0, 2, 0, 0]) is [list: row-move(0, 1, 0)]
  r([list: 0, 2, 0, 2]) is [list: row-move(0, 1, 0), row-move(0, 3, 0)]
  r([list: 0, 0, 0, 2]) is [list: row-move(0, 3, 0)]
  r([list: 2, 2, 0, 0]) is [list: row-move(0, 0, 0), row-move(0, 1, 0)]
  r([list: 2, 2, 4, 4]) is [list: row-move(0, 0, 0), row-move(0, 1, 0), row-move(0, 2, 1), row-move(0, 3, 1)]
  r([list: 2, 4, 4, 8]) is [list: row-move(0, 0, 0), row-move(0, 1, 1), row-move(0, 2, 1), row-move(0, 3, 2)]
  r([list: 2, 4, 6, 8]) is [list: row-move(0, 0, 0), row-move(0, 1, 1), row-move(0, 2, 2), row-move(0, 3, 3)]
  r([list: 2, 2, 2, 2]) is [list: row-move(0, 0, 0), row-move(0, 1, 0), row-move(0, 2, 1), row-move(0, 3, 1)]
  r([list: 2, 4, 4, 2]) is [list: row-move(0, 0, 0), row-move(0, 1, 1), row-move(0, 2, 1), row-move(0, 3, 2)]
end

get-moves = row-moves(_, -1, 0, 0, _)

fun grid-left(a-grid):
  for map(row from a-grid):
    row-shift(row)
  end
where:
  grid-left([list: 
      [list: 2, 4, 6, 8],
      [list: 2, 2, 0, 0],
      [list: 0, 0, 2, 2],
      [list: 4, 4, 4, 4]
    ]) is
  [list: 
    [list: 2, 4, 6, 8],
    [list: 4, 0, 0, 0],
    [list: 4, 0, 0, 0],
    [list: 8, 8, 0, 0]
  ]   
end
  
fun flip(a-grid):
  for map(row from a-grid):
    row.reverse()
  end
end

fun rotate(a-grid):
  new-grid-init = for map(row from a-grid): [list: ] end
  for fold(new-grid from new-grid-init, row from a-grid):
    for map2(new-grid-row from new-grid, elt from row):
      [list: elt] + new-grid-row
    end
  end
where:
  rotate([list: [list: 1, 2], [list: 3, 4]]) is [list: [list: 3, 1], [list: 4, 2]]
end

fun rotate-move(move, len):
  cases(Move) move:
    | row-move(rn, s, e) =>
      col-move(len - rn, s, e)
    | col-move(rn, s, e) =>
      row-move(rn, len - s, len - e)
  end
where:
  fun cycle(m, len2):
    r = rotate-move(_, len2)
    r(r(r(r(m))))
  end
  rotate-move(row-move(0, 1, 0), 3) is col-move(3, 1, 0)
  rotate-move(row-move(2, 3, 2), 3) is col-move(1, 3, 2)
  
  cycle-tests = [list:
    row-move(0, 1, 0),
    row-move(3, 2, 1),
    row-move(2, 2, 2),
    col-move(0, 0, 0),
    col-move(1, 2, 3),
    col-move(1, 3, 2)
  ]
  for each(c from cycle-tests):
    cycle(c, 3) is c
  end
end

fun grid-right(a-grid):
  flip(grid-left(flip(a-grid)))
where:
  grid-right([list: [list: 2, 2], [list: 4, 0]]) is [list: [list: 0, 4], [list: 0, 4]]
end

fun grid-down(a-grid):
  rotate(rotate(rotate(grid-left(rotate(a-grid)))))
where:
  grid-down([list: [list: 2, 2], [list: 2, 2]]) is [list: [list: 0, 0], [list: 4, 4]]
end

fun grid-up(a-grid):
  rotate(grid-left(rotate(rotate(rotate(a-grid)))))
where:
  grid-up([list: [list: 2, 2], [list: 2, 2]]) is [list: [list: 4, 4], [list: 0, 0]]
end

fun moves-left(a-grid):
  for lists.fold_n(n from 0, acc from [list:], row from a-grid):
    acc + get-moves(n, row)
  end
end

fun moves-down(a-grid):
  moves-grid = rotate(a-grid)
  l = a-grid.length() - 1
  fun triple-rotate-move(m): rotate-move(rotate-move(rotate-move(m, l), l), l); 
  for lists.fold_n(n from 0, acc from [list:], row from moves-grid):
    acc + get-moves(n, row).map(triple-rotate-move)
  end
end

fun moves-right(a-grid):
  moves-grid = rotate(rotate(a-grid))
  l = a-grid.length() - 1
  fun double-rotate-move(m): rotate-move(rotate-move(m, l), l); 
  for lists.fold_n(n from 0, acc from [list:], row from moves-grid):
    acc + get-moves(n, row).map(double-rotate-move)
  end
end

fun moves-up(a-grid):
  moves-grid = rotate(rotate(rotate(a-grid)))
  l = a-grid.length() - 1
  for lists.fold_n(n from 0, acc from [list:], row from moves-grid):
    acc + get-moves(n, row).map(rotate-move(_, l))
  end
end

fun get-value(grid, row, col):
  grid.get(row).get(col)
end


fun color-of-num(n):
  if n == 0: IS.color(187, 173, 169, 1)
  else if n == 2: IS.color(238, 228, 218, 1)
  else if n == 4: IS.color(237, 224, 203, 1)
  else if n == 8: IS.color(242, 177, 121, 1)
  else if n == 16: IS.color(245, 149, 99, 1)
  else if n == 32: IS.color(246, 124, 95, 1)
  else if n == 64: IS.color(246, 94, 59, 1)
  else if n == 128: IS.color(237, 207, 114, 1)
  else if n == 256: IS.color(237, 204, 97, 1)
  else if n == 512: IS.color(249, 200, 80, 1)
  else if n == 1024: IS.color(249, 197, 63, 1)
  else if n == 2048: IS.color(249, 194, 46, 1)
  else: "yellow"
  end
end

fun draw-square(value):
  str = if value == 0: "" else: tostring(value);
  text-img = I.text(str, 24, "black") 
  I.place-image(
    text-img,
    SQUARE-MIDDLE,
    SQUARE-MIDDLE,
    I.square(SQUARE-WIDTH, "solid", color-of-num(value)))
end

fun draw-move(ticks, grid, move, background):
  fun row-xy(r, s, e):
    y-pos = (r * SQUARE-WIDTH) + SQUARE-MIDDLE + ((r + 1) * SQUARE-BORDER)
    progress = (ticks / TICKS-PER-ANIMATION)
    offset = ((e - s) * SQUARE-WIDTH) * progress
    x-pos = offset + ((s * SQUARE-WIDTH) + SQUARE-MIDDLE) + ((s + 1) * SQUARE-BORDER)
    { x: x-pos, y: y-pos }
  end
  xyv = cases(Move) move:
    | row-move(r, s, e) =>
      rp = row-xy(r, s, e)
      { x: rp.x, y: rp.y, v: get-value(grid, r, s) }
    | col-move(c, s, e) =>
      rp = row-xy(c, s, e)
      { x: rp.y, y: rp.x, v: get-value(grid, s, c) }
  end
  square-img = draw-square(xyv.v)
  I.place-image(square-img, xyv.x, xyv.y, background)
end

fun to-draw(a-world):
  cases(WorldState) a-world:
    | animating(ticks, orig-grid, grid, moves) =>
      zero-grid = for map(row from orig-grid):
        for map(_ from row):
          0
        end
      end
      board-side = SQUARE-WIDTH * grid.length()
      for fold(
          i from draw-grid(zero-grid),
          m from moves):
        draw-move(ticks, orig-grid, m, i)
      end
    | waiting-for-input(grid) => draw-grid(grid) 
  end
end

fun draw-grid(a-grid):
  l = a-grid.length()
  board-side = SQUARE-WIDTH * l
  for fold2(
      i from I.square(board-side + (SQUARE-BORDER * (l + 1)), "solid", IS.color(119, 110, 101, 1)),
      row from a-grid,
      x from range(0, l)
      ):
    for fold2(shadow i from i, elt from row, y from range(0, l)):
      square-img = draw-square(elt)
      I.place-image(
        square-img,
        (y * SQUARE-WIDTH) + ((y + 1) * SQUARE-BORDER) + SQUARE-MIDDLE,
        (x * SQUARE-WIDTH) + ((x + 1) * SQUARE-BORDER) + SQUARE-MIDDLE,
        i)
    end
  end
end

fun on-key(a-world, key):
  cases(WorldState) a-world:
    | waiting-for-input(a-grid) =>
      if W.is-key-equal(key, "up"):
        animating(0, a-grid, grid-up(a-grid), moves-up(a-grid))
      else if W.is-key-equal(key, "down"):
        animating(0, a-grid, grid-down(a-grid), moves-down(a-grid))
      else if W.is-key-equal(key, "left"):
        animating(0, a-grid, grid-left(a-grid), moves-left(a-grid))
      else if W.is-key-equal(key, "right"):
        animating(0, a-grid, grid-right(a-grid), moves-right(a-grid))
      else:
        a-world
      end
    | animating(_, _, _, _) => a-world
  end
where:
  grid = [list: [list: 2, 2], [list: 4, 4]]
  on-key(waiting-for-input(grid), "left").target-grid is grid-left(grid)
  on-key(waiting-for-input(grid), "right").target-grid is grid-right(grid)
  on-key(waiting-for-input(grid), "up").target-grid is grid-up(grid)
  on-key(waiting-for-input(grid), "down").target-grid is grid-down(grid)
end

big-start-grid = for map(i from range(0, 4)):
  [list: 2, 2, 4, 0]
end

fun something-moved(moves):
  fun moved(move):
    cases(Move) move:
      | row-move(_, s, e) => s <> e
      | col-move(_, s, e) => s <> e
    end
  end
  (moves^filter(moved, _)).length() > 0
end

fun find-zeroes(grid):
  for lists.fold_n(r from 0, lst from [list: ], row from grid):
    for lists.fold_n(c from 0, shadow lst from lst, elt from row):
      if elt == 0: link({r: r, c: c}, lst)
      else: lst
      end
    end
  end
end

fun add-if-necessary(grid, moves):
  if not(something-moved(moves)): grid
  else:
    zero-coords = find-zeroes(grid)
    zero-to-replace = zero-coords.get(random(zero-coords.length()))
    v = if random(2) == 0: 2 else: 4;
    r = zero-to-replace.r
    c = zero-to-replace.c
    grid.set(r, grid.get(r).set(c, v))
  end
end

fun on-tick(a-world):
  cases(WorldState) a-world:
    | animating(ticks, orig-grid, grid, moves) =>
      if (ticks > TICKS-PER-ANIMATION) or
        (not(something-moved(moves))):
        waiting-for-input(add-if-necessary(grid, moves))
      else:
        animating(ticks + 1, orig-grid, grid, moves)
      end
    | waiting-for-input(_) => a-world
  end
end

fun start():
  W.big-bang(waiting-for-input(big-start-grid), [list:
      W.on-key(on-key),
      W.to-draw(to-draw),
      W.on-tick-n(on-tick, 1/60)
    ])
end


