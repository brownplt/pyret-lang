#lang pyret

provide *
provide-types *

import global as _

data Point:
  | point-xy(x :: Number, y :: Number)
  | point-polar(r :: Number, theta :: Number)
end

# alias xy-point to point
point = point-xy
# alias Point2D to Point
type Point2D = Point

data XPlace: x-left | x-middle | x-pinhole | x-right end
data YPlace: y-top | y-center | y-pinhole | y-baseline | y-bottom end
x-center = x-middle # Allow these aliases, but
y-middle = y-center # don't bloat the data definitions
fun is-transparency(n :: Number) -> Boolean:
  (n >= 0) and (n <= 1)
end
data FillMode: mode-solid | mode-outline | mode-fade(n :: Number%(is-transparency)) end
data FontFamily:
  | ff-default
  | ff-decorative
  | ff-roman
  | ff-script
  | ff-swiss
  | ff-modern
  | ff-symbol
  | ff-system
end
data FontStyle: fs-normal | fs-italic | fs-slant end
data FontWeight: fw-normal | fw-bold | fw-light end
