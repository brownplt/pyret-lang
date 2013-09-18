#lang pyret

import image as image
provide {
  star: star,
  circle: circle,
  rectangle: rectangle,
  triangle: triangle,
  isosceles-triangle: isosceles-triangle
} end

data Image:
  | star(side-length :: Number, style :: String, color :: String) with:
    to-image(self):
      image.star(self.side-length, self.style, self.color)
    end
  | circle(radius :: Number, style :: String, color :: String) with:
    to-image(self):
      image.circle(self.radius, self.style, self.color)
    end
  | rectangle(width :: Number, height :: Number, style :: String, color :: String) with:
    to-image(self):
      image.rectangle(self.width, self.height, self.style, self.color)
    end
  | triangle(length :: Number, style :: String, color :: String) with:
    to-image(self):
      image.triangle(self.length, self.style, self.color)    
    end
  | isosceles-triangle(length :: Number, angle :: Number, style :: String, color :: String) with:
    to-image(self):
      image.isosceles-triangle(self.length, self.angle, self.style, self.color)
    end
  | composite(left :: Image, x :: Number, y :: Number, right :: Image) with:
    to-image(self):
      image.place-image(self.left.to-image(), self.x, self.y, self.right.to-image())
    end
  | rotated(img :: Image, angle :: Number) with:
    to-image(self):
      image.rotate(self.angle, self.img.to-image())
    end
sharing:
  place-image(self, x :: Number, y :: Number, other :: Image):
    composite(other, x, y, self)
  end,
  rotate(self, angle :: Number):
    rotated(self, angle)
  end,
  width(self): image.image-width(self.to-image()) end,
  height(self): image.image-height(self.to-image()) end
end

