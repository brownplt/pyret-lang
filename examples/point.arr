#lang pyret

# Point -> make-point
# Cut color-points
# ColorPoint -> make-color-point

import equality as E

point-methods = {
  method dist(self, other):
    ysquared = num-sqr(other.y - self.y)
    xsquared = num-sqr(other.x - self.x)
    num-sqrt(ysquared + xsquared)
  end
}

fun make-point(x, y):
  point-methods.{ x: x, y: y }
end

check:
  make-point(1, 1).dist(make-point(1, 3)) is 2  
end

data Color:
  | red
    with: method torgb(_): rgb(255, 0, 0) end
  | green
    with: method torgb(_): rgb(0, 255, 0) end
  | blue
    with: method torgb(_): rgb(0, 0, 255) end
  | rgb(r, g, b)
    with: method torgb(self): self end
sharing:
  method mix(self, other):
    fun avg(n1, n2): (n1 + n2) / 2 end
    rgb1 = self.torgb()
    rgb2 = other.torgb()
    rgb(
        avg(rgb1.r, rgb2.r),
        avg(rgb1.g, rgb2.g),
        avg(rgb1.b, rgb2.b)
      )
  end
end

rec colorpoint-methods = point-methods.{
  method midpoint(self, other):
    midx = self.x + ((other.x - self.x) / 2)
    midy = self.y + ((other.y - self.y) / 2)
    midcolor = self.color.mix(other.color)
    make-color-point(midx, midy, midcolor)
  end,
  method _equals(self, other, eq):
    E.equal-and(eq(self.x, other.x),
      E.equal-and(eq(self.y, other.y),
        eq(self.color, other.color)))
  end
}

fun make-color-point(x, y, color):
  colorpoint-methods.{ x: x, y: y, color: color }
end

check:
  make-color-point(1, 3, red).dist(make-point(1, 3)) is 0
  make-color-point(1, 3, red).dist(make-color-point(3, 3, green)) is 2
  make-color-point(1, 3, green).midpoint(make-color-point(2, 4, red))
    is make-color-point(1.5, 3.5, rgb(127.5, 127.5, 0))
  make-color-point(1, 3, green).midpoint(make-color-point(2, 4, red))
    is { x: 1.5, y: 3.5, color: rgb(127.5, 127.5, 0) }
end

