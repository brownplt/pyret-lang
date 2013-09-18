#lang pyret

import "image2.arr" as image
import big-bang as bb
import math as Math
big-bang = bb.big-bang

width = 400
height = 100

init-world = []

data Posn:
  | posn(x, y :: Number)
end

ufo = image.circle(15, "solid", "red")

fun draw(w):
  fun draw-help(w2):
    cases (list.List) w2:
      | empty => image.rectangle(width, height, "solid", "white")
      | link(h, t) => draw-help(t).place-image(h.y, h.x, ufo)
    end
  end
  draw-help(w).to-image()
end

fun key(w, k):
  list.link(posn((w.length() * ufo.width()) + (ufo.width() / 2) + 2,
                 (width / 2)),
            w)
end

fun perturb(p):
  posn(p.x, (p.y + (Math.random(9) - 4)))
end

fun tick(w):
  cases (list.List) w:
    | empty => w
    | link(f, r) => list.link(perturb(f), tick(r))
  end
end

big-bang(init-world,
  { on-key: key,
    to-draw: draw,
    on-tick: tick })

