import image as I
import world as W
II = I

data Posn:
  | posn(x, y)
end

data World:
  | world(p :: Posn, b :: Posn, f :: Number)
end

AIRPLANE-URL = 
  "http://world.cs.brown.edu/1/clipart/airplane-small.png"
AIRPLANE = I.image-url(AIRPLANE-URL)

BALLOON-URL =
  "http://world.cs.brown.edu/1/clipart/balloon-small.png"
BALLOON = I.image-url(BALLOON-URL)

WIDTH = 800
HEIGHT = 500
BLANK-SCENE = I.empty-scene(WIDTH, HEIGHT)

#BALLOON-LOC = posn(600, 300)
INIT-BALLOON-LOC = posn(random(WIDTH), random(HEIGHT))
COLLISION-THRESHOLD = 75

BASE-HEIGHT = 50
WATER-WIDTH = 500

WATER = I.rectangle(WATER-WIDTH, BASE-HEIGHT, "solid", "blue")
LAND = I.rectangle(WIDTH - WATER-WIDTH, BASE-HEIGHT, "solid", "brown")

BASE = I.beside(WATER, LAND)

BACKGROUND =
  I.place-image(BASE,
    WIDTH / 2, HEIGHT - (BASE-HEIGHT / 2),
    BLANK-SCENE)

AIRPLANE-X-MOVE = 10
AIRPLANE-Y-MOVE = 3

INIT-FUEL = 10

INIT-WORLD = world(posn(0, 0), INIT-BALLOON-LOC, INIT-FUEL)

check:
  move-airplane-x-on-tick(50) is 50 + AIRPLANE-X-MOVE
  move-airplane-x-on-tick(0) is 0 + AIRPLANE-X-MOVE
  move-airplane-x-on-tick(100) is 100 + AIRPLANE-X-MOVE
end

fun move-airplane-x-on-tick(x):
  x + AIRPLANE-X-MOVE
end
fun move-airplane-wrapping-x-on-tick(x):
#  num-modulo(x + AIRPLANE-X-MOVE, WIDTH)
  num-modulo(move-airplane-x-on-tick(x), WIDTH)
end
fun move-airplane-y-on-tick(y):
  y + AIRPLANE-Y-MOVE
end

fun move-balloon(p):
  posn(p.x + random(9) + -4,
    p.y + random(9) + -4)
end

fun move-airplane-balloon-xy-on-tick(w :: World):
  world(
    posn(
      move-airplane-wrapping-x-on-tick(w.p.x),
      move-airplane-y-on-tick(w.p.y)),
    move-balloon(w.b),
    w.f)
end

fun place-airplane-and-balloon-xy(w):
  I.place-image(AIRPLANE,
    w.p.x,
    w.p.y,
    I.place-image(BALLOON,
      w.b.x,
      w.b.y,
      BACKGROUND))
end

KEY-DISTANCE = 10

fun alter-airplane-y-on-key(w, key):
  ask:
    | key == "up"   then: 
      if w.f > 0:
        world(posn(w.p.x, w.p.y - KEY-DISTANCE), w.b, w.f - 1)
      else:
        w
      end
    | key == "down" then: 
      world(posn(w.p.x, w.p.y + KEY-DISTANCE), w.b, w.f)
    | otherwise: w
  end
end

fun is-on-land-or-water(w):
  w.p.y >= (HEIGHT - BASE-HEIGHT)
end

fun are-overlapping(airplane-posn, balloon-posn):
  distance(airplane-posn, balloon-posn) 
    < COLLISION-THRESHOLD
end

fun distance(p1, p2):
  fun square(n): n * n end
  num-sqrt(square(p1.x - p2.x) + square(p1.y - p2.y))
end

fun game-ends(w):
  is-on-land-or-water(w) or are-overlapping(w.p, w.b)
end

W.big-bang(INIT-WORLD, [list:
    W.on-tick(move-airplane-balloon-xy-on-tick),
    W.on-key(alter-airplane-y-on-key),
    W.to-draw(place-airplane-and-balloon-xy),
    W.stop-when(game-ends)])
