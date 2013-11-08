#lang pyret

# Point -> make-point
# Cut color-points
# ColorPoint -> make-color-point

point-methods = {
  dist(self, other):
    ysquared = (other.y - self.y).sqr()
    xsquared = (other.x - self.x).sqr()
    (ysquared + xsquared).sqrt()
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
    with: torgb(_): rgb(255, 0, 0);
  | green
    with: torgb(_): rgb(0, 255, 0);
  | blue
    with: torgb(_): rgb(0, 0, 255);
  | rgb(r, g, b)
    with: torgb(self): self;
sharing:
  mix(self, other):
    fun avg(n1, n2): (n1 + n2) / 2;
    rgb1 = self.torgb()
    rgb2 = other.torgb()
    rgb(
        avg(rgb1.r, rgb2.r),
        avg(rgb1.g, rgb2.g),
        avg(rgb1.b, rgb2.b)
      )
  end
end

colorpoint-methods = point-methods.{
  midpoint(self, other):
    midx = self.x + ((other.x - self.x) / 2)
    midy = self.y + ((other.y - self.y) / 2)
    midcolor = self.color.mix(other.color)
    make-color-point(midx, midy, midcolor)
  end,
  _equals(self, other):
    (self.x == other.x) and 
      (self.y == other.y) and
      (self.color == other.color)
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

