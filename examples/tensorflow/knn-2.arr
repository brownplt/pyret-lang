import tensorflow as TF
import reactors as R
import lists as L

include image
include image-structs

data Point:
  | point(x :: Number, y :: Number, group :: Number)
end

data DistancePoint:
  | dist-pt(dist :: Number, pt :: Point)
end

# Edit the constants below to set up reactor:

K-VALUE    = 3  # the number of neighbors to use to make a classification
NUM-POINTS = 15 # number of points to randomly generate
POINTS     =    # randomly generated points
  for fold(base from [list:], count from range(0, NUM-POINTS)):
    link(
      point(num-random(320) + 40, num-random(320) + 40, num-modulo(count, 2)),
      base)
  end

fun dist-2(points :: List<Point>, width :: Number, height :: Number) block:
  num-points   = points.length()
  num-queries  = width * height
  point-buffer = TF.make-buffer([list: num-points * num-queries, 2])
  L.each(lam(i):
      L.each_n(lam(j, pt) block:
          point-buffer.set-now(pt.x, [list: i * j, 0])
          point-buffer.set-now(pt.y, [list: i * j, 1])
        end, 0, points)
    end, range(0, num-queries))

  query-buffer = TF.make-buffer([list: num-points * num-queries, 2])
  L.each(lam(x):
      L.each(lam(y):
          L.each_n(lam(j, _) block:
              point-buffer.set-now(x, [list: x * y * j, 0])
              point-buffer.set-now(y, [list: x * y * j, 1])
            end, 0, points)
        end, range(0, height))
    end, range(0, width))

  point-tensor = point-buffer.to-tensor()
  query-tensor = query-buffer.to-tensor()

  TF.arg-min(TF.reduce-sum(TF.tensor-square(point-tensor.subtract(query-tensor)), some(1)).as-2d(num-queries, num-points), some(1)).as-2d(400, 400)
end

# Reactor functions:

fun mouse-move(reactor-state, x, y, event) block:
  fun clamp(v):
    ask:
      | v < 0 then: 0
      | v > 400 then: 400
      | otherwise: v
    end
  end

  {x: clamp(x), y: clamp(y), k: reactor-state.k, overlay: reactor-state.overlay}
end

fun distance-from-state(state, points):
  dist-wrap = for fold(base from [list:], pt from points):
    dist = num-sqrt(num-sqr(state.x - pt.x) + num-sqr(state.y - pt.y))
    link(dist-pt(dist, pt), base)
  end

  dist-wrap.sort-by(
    lam(a, b): a.dist < b.dist end,
    lam(a, b): within(0.00000001)(a.dist, b.dist) end)
end

fun get-pt-color(pt-group):
  ask:
    | pt-group == 0 then: "red"
    | pt-group == 1 then: "blue"
    | pt-group == 2 then: "green"
  end
end

fun get-highest-group(points):
  fun get-highest-index(lst, highest-index, highest-total, current-index):
    cases (List) lst:
      | empty => highest-index
      | link(f, r) =>
        if f > highest-total:
          get-highest-index(r, current-index, f, current-index + 1)
        else:
          get-highest-index(r, highest-index, highest-total, current-index + 1)
        end
    end
  end

  group-counts = for fold(base from L.repeat(2, 0), d-pt from points):
    base.set(d-pt.pt.group, base.get(d-pt.pt.group) + 1)
  end

  get-highest-index(group-counts, 0, 0, 0)
end

fun draw-scene(state):
  points      = POINTS
  k-value     = state.k
  overlay-map = state.overlay
  scene       = place-image(overlay-map, 200, 200, empty-scene(400, 400))

  nearest-points = distance-from-state(state, points).take(k-value)
  added-lines = for fold2(
      base from scene,
      d-pt from nearest-points,
      neighbor-index from range(0, nearest-points.length())):
    pt = d-pt.pt
    line-color = get-pt-color(pt.group)
    with-line = add-line(base, state.x, state.y, pt.x, pt.y, line-color)

    gui-dot = circle(3, "solid", line-color)
    place-image(gui-dot, 60 + (neighbor-index * 10), 20, with-line)
  end

  classification = get-highest-group(nearest-points)
  class-color    = get-pt-color(classification)
  dot            = circle(6, "solid", class-color)
  pointer        = place-image(dot, state.x, state.y, added-lines)

  gui = text("k = " + num-to-string(k-value), 16, "black")
  place-image(gui, 26, 20, pointer)
end

fun initial-grid(points, k-value):
  TOTAL-WIDTH  = 400
  TOTAL-HEIGHT = 400
  CELL-SIZE    = 8

  scene = empty-scene(TOTAL-WIDTH, TOTAL-HEIGHT)

  number-of-x-cells = num-ceiling(TOTAL-WIDTH / CELL-SIZE) + 1
  number-of-y-cells = num-ceiling(TOTAL-HEIGHT / CELL-SIZE) + 1

  mid-cell-offset = CELL-SIZE / 2

  mapping = for fold(outer from scene, i from range(1, number-of-x-cells)):
    curr-x = i * CELL-SIZE

    for fold(base from outer, j from range(1, number-of-y-cells)):
      curr-y = j * CELL-SIZE

      nearest-points =
        distance-from-state({x: curr-x, y: curr-y}, points).take(k-value)
      classification = get-highest-group(nearest-points)
      pt-color       = get-pt-color(classification)
      region-color   =
        ask:
          | pt-color == "red" then: color(255, 0, 0, 0.3)
          | pt-color == "blue" then: color(0, 0, 255, 0.3)
          | pt-color == "green" then: color(0, 255, 0, 0.3)
        end

      place-image(
        rectangle(CELL-SIZE, CELL-SIZE, "solid", region-color),
        curr-x - mid-cell-offset,
        curr-y - mid-cell-offset,
        base)
    end
  end

  for fold(base from mapping, pt from points):
    pt-color = get-pt-color(pt.group)
    new-dot = circle(4, "solid", pt-color)
    place-image(new-dot, pt.x, pt.y, base)
  end
end

fun key-press(state, key):
  new-k-value = ask:
    | key == "1" then: 1
    | key == "2" then: 2
    | key == "3" then: 3
    | key == "4" then: 4
    | key == "5" then: 5
    | key == "6" then: 6
    | key == "7" then: 7
    | key == "8" then: 8
    | key == "9" then: 9
    | otherwise: state.k
  end

  if state.k == new-k-value:
    state
  else:
    new-overlay-map = initial-grid(POINTS, new-k-value)
    {x: state.x, y: state.x, k: new-k-value, overlay: new-overlay-map}
  end
end

fun start-reactor():
  react = reactor:
    seconds-per-tick: 0.1,
    title: "K-Nearest Neighbors",
    on-mouse: mouse-move,
    on-key: key-press,
    init: {x: 0, y: 0, k: K-VALUE, overlay: initial-grid(POINTS, K-VALUE)},
    to-draw: draw-scene
  end

  R.interact(react)
end
