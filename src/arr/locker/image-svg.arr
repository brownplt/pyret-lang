#lang pyret

import pprint as PP

fun between(lo :: Number, mid :: Number, hi :: Number) -> Boolean:
  (lo <= mid) and (mid <= hi)
end

fun positive(n :: Number) -> Boolean: n > 0 end
fun nonneg(n :: Number) -> Boolean: n >= 0 end

INDENT = 2

PI = 3.1415926535
fun cosdeg(angle): (angle * (PI / 180)).cos() end
fun sindeg(angle): (angle * (PI / 180)).sin() end

fun tohex(num, minwidth):
  fun help(n, hexits):
    if (n == 0):
      if hexits.length() < minwidth: help(n, "0" ^ link(_, hexits))
      else: hexits.join-str("")
      end
    else:
      hexit = n.modulo(16)
      next = (n - hexit) / 16
      if (hexit < 10): help(next, tostring(hexit) ^ link(_, hexits))
      else if hexit == 10: help(next, "A" ^ link(_, hexits))
      else if hexit == 11: help(next, "B" ^ link(_, hexits))
      else if hexit == 12: help(next, "C" ^ link(_, hexits))
      else if hexit == 13: help(next, "D" ^ link(_, hexits))
      else if hexit == 14: help(next, "E" ^ link(_, hexits))
      else:                help(next, "F" ^ link(_, hexits))
      end
    end
  end
  help(num, [list: ])
end

data Attr:
  | simple(name :: String, val :: String) with:
    tostring(self): self.name + '="' + self.val + '"' end,
    topretty(self): PP.group(PP.str(self.name) + PP.str("=") + PP.sbreak(0) + PP.dquote(PP.str(self.val))) end
  | mixed(name :: String, vals :: List<Attr>) with:
    tostring(self): self.name + '="' + self.vals.map(lam(v): v.name + ":" + v.val end).join-str(":") + '"' end,
    topretty(self):
      if is-empty(self.vals): PP.mt-doc
      else:
        PP.str(self.name) + PP.str('="') 
          + PP.flow-map(PP.str(";") + PP.sbreak(1),
          lam(v): PP.group(PP.str(v.name) + PP.str(":") + PP.sbreak(1) + PP.str(v.val)) end,
          self.vals)
          + PP.str(';"')
      end
    end
end
data XML:
  | node(name :: String, attrs :: List<Attr>, kids :: List<XML>) with:
    tostring(self): self.topretty().pretty(80).join-str("\n") end,
    topretty(self):
      attrs = self.attrs.map(_.topretty())
      empty-tag =
        cases(List) attrs:
          | empty => PP.group(PP.langle + PP.str(self.name) + PP.str(" />"))
          | else => PP.group(PP.langle + PP.nest(2 * INDENT,
                PP.flow(PP.str(self.name) ^ link(_, attrs)) + PP.str("/>")))
        end
      open-tag =
        cases(List) attrs:
          | empty => PP.group(PP.langle + PP.str(self.name) + PP.str(">"))
          | else => PP.group(PP.langle + PP.nest(2 * INDENT,
                PP.flow(PP.str(self.name) ^ link(_, attrs)) + PP.str(">")))
        end
      close-tag = PP.str("</" + self.name + ">")
      PP.surround-separate(INDENT, 1, empty-tag, 
          open-tag, PP.sbreak(1), close-tag, self.kids.map(_.topretty()))
    end
  | xtext(str :: String) with:
    tostring(self): self.str end,
    topretty(self): PP.str(self.str) end
end         
      

data Color:
  | color(
      red :: Number%(between(0, _, 255)),
      green :: Number%(between(0, _, 255)),
      blue :: Number%(between(0, _, 255)),
      alpha :: Number%(between(0, _, 255))) with:
    tostring(self):
      "#" + tohex(self.red, 2) + tohex(self.green, 2) + tohex(self.blue, 2)
    end 
end

data PenStyle:
  | solid
  | dot
  | long-dash
  | short-dash
  | dot-dash
end
data PenCap:
  | round
  | projecting
  | butt
end
data PenJoin:
  | rounded
  | bevel
  | miter
end
data Pen:
  | pen(color :: Color, width :: Number%(between(0, _, 255)),
      style :: PenStyle, cap :: PenCap, join :: PenJoin)
end

data Mode:
  | filled
  | outline
  | opacity(o :: Number%(between(0, _, 255)))
end

data CombineMode:
  | crop
  | union
end
# Todo: outline mode/pen combos

data Point:
  | pt(x, y) with:
    tostring(self): self.x.tostring-fixed(5) + "," + self.y.tostring-fixed(5) end,
    min(self, other): pt(self.x.min(other.x), self.y.min(other.y)) end,
    max(self, other): pt(self.x.max(other.x), self.y.max(other.y)) end,
    translate(self, dx, dy): pt(self.x + dx, self.y + dy) end,
    scale(self, dx, dy): pt(self.x * dx, self.y * dy) end,
    rotate(self, theta):
      r = ((self.x * self.x) + (self.y * self.y)).sqr()
      th = (self.y / self.x).atan() + (theta * (PI / 180))
      pt(r * th.cos(), r * th.sin())
    end
end
data Region:
  | rg(tl :: Point, br :: Point) with:
    union(self, other): rg(self.tl.min(other.tl), self.br.max(other.br)) end,
    translate(self, dx, dy): rg(self.tl.translate(dx, dy), self.br.translate(dx, dy)) end,
    scale(self, dx, dy): rg(self.tl.scale(dx, dy), self.br.scale(dx, dy)) end,
    rotate(self, theta):
      new-tl = self.tl.rotate(theta)
      new-br = self.br.rotate(theta)
      rg(new-tl.min(new-br), new-tl.max(new-br))
    end
end

data Image:
  | empty-image(width :: Number%(nonneg), height :: Number%(nonneg), color :: Color) with:
    as-svg(self):
      node("rect",
        [list: simple("width", tostring(self.width)), simple("height", tostring(self.height)),
          simple("transform", "translate(" + tostring((0 - self.width) / 2) + "," + tostring((0 - self.height) / 2) + ")"),
          mixed("style", [list: simple("fill", tostring(self.color)), simple("fill-opacity", tostring(self.color.alpha / 255)), simple("stroke-width", "1"),
              simple("stroke", "rgb(0,0,0)")])],
        [list: ])
    end,
    bounds(self):
      p = pt(self.width / 2, self.height / 2)
      rg(p.scale(-1, -1), p)
    end
  | itext(str :: String, size :: Number%(between(1, _, 255)), color :: Color) with: # Todo: fonts
    as-svg(self):
      node("text",
        [list: simple("x", "0"), simple("y", "0"),
          mixed("style", [list: simple("fill", tostring(self.color)), simple("fill-opacity", tostring(self.color.alpha / 255)), simple("font-size", tostring(self.size))])],
        [list: xtext(self.str)])
    end,
    bounds(self): rg(pt(0, 0), pt(self.str.length() * self.size, self.size)) end #TODO
  | iplace-image(image :: Image, x :: Number, y :: Number, combine :: CombineMode, scene :: Image) with:
    as-svg(self):
      node("g", [list: ],
        [list: self.scene.as-svg(),
          node("g", [list: simple("transform", "translate(" + tostring(self.x) + "," + tostring(self.y) + ")")],
            [list: self.image.as-svg()])])
    end,
    bounds(self):
      cases(CombineMode) self.combine:
        | crop => self.scene.bounds()
        | union => self.image.bounds().translate(self.x, self.y).union(self.scene.bounds())
      end
    end
  | icircle(radius :: Number%(nonneg), mode :: Mode, color :: Color) with:
    as-svg(self): # Todo: Mode
      node("circle", [list: simple("r", tostring(self.radius)),
          mixed("style", [list: simple("fill", tostring(self.color)),
              simple("fill-opacity", tostring(self.color.alpha / 255))])], [list: ])
    end,
    bounds(self): rg(pt(0 - self.radius, 0 - self.radius), pt(self.radius, self.radius)) end
  | iellipse(width :: Number%(nonneg), height :: Number%(nonneg), mode :: Mode, color :: Color) with:
    as-svg(self): # Todo: Mode
      node("ellipse", [list: simple("rx", tostring(self.width / 2)), simple("ry", tostring(self.height / 2)),
          mixed("style", [list: simple("fill", tostring(self.color)),
              simple("fill-opacity", tostring(self.color.alpha / 255))])], [list: ])
    end,
    bounds(self): rg(pt((0 - self.width) / 2, (0 - self.height) / 2), pt(self.width / 2, self.height / 2)) end
  | irectangle(width :: Number%(nonneg), height :: Number%(nonneg), mode :: Mode, color :: Color) with:
    as-svg(self): # Todo: Mode
      node("rect", [list: simple("width", tostring(self.width)), simple("height", tostring(self.height)),
          mixed("style", [list: simple("fill", tostring(self.color)), simple("fill-opacity", tostring(self.color.alpha / 255))])], [list: ])
    end,
    bounds(self): rg(pt(0, 0), pt(self.width, self.height)) end
  | ipolygon(points :: List<Point>, mode :: Mode, color :: Color) with:
    as-svg(self):
      points-str =
        self.points.join-str(" ")
      node("polygon",
        [list: simple("points", points-str),
          mixed("style", [list: simple("fill", tostring(self.color)),
              simple("fill-opacity", tostring(self.color.alpha / 255))])],
        [list: ])
    end,
    bounds(self):
      min = for lists.fold(acc from self.points.first, shadow pt from self.points.rest):
        acc.min(pt)
      end
      max = for lists.fold(acc from self.points.first, shadow pt from self.points.rest):
        acc.max(pt)
      end
      rg(min, max)
    end
  | istar(side-length :: Number%(nonneg), mode :: Mode, color :: Color) with:
    as-svg(self):
      radius = self.side-length / ((2 * (1 - cosdeg(72))).sqrt())
      points = for lists.map(i from range(0, 5)):
        theta = 144 * i
        pt(radius * cosdeg(theta + 90), radius * sindeg(theta + 90))
      end
      ipolygon(points, self.mode, self.color).as-svg()
    end,
    bounds(self):
      radius = self.side-length / ((2 * (1 - cosdeg(72))).sqrt())
      points = for lists.map(i from range(0, 5)):
        theta = 144 * i
        pt(radius * cosdeg(theta), radius * sindeg(theta))
      end
      ipolygon(points, self.mode, self.color).bounds()
    end
  | itriangle(side-length :: Number%(nonneg), mode :: Mode, color :: Color) with:
    as-svg(self):
      third-point-y = 0 - (self.side-length * sindeg(60))
      points =
        for lists.map(p from [list: pt(0,0), pt(self.side-length, 0), pt(self.side-length / 2, third-point-y)]):
          p.translate((0 - self.side-length) / 2, (0 - third-point-y) / 2)
        end
      ipolygon(points, self.mode, self.color).as-svg()
    end,
    bounds(self):
      third-point-y = self.side-length * sindeg(60)
      points =
        for lists.map(p from [list: pt(0,0), pt(self.side-length, 0), pt(self.side-length / 2, third-point-y)]):
          p.translate((0 - self.side-length) / 2, (0 - third-point-y) / 2)
        end
      ipolygon(points, self.mode, self.color).bounds()
    end
  | iisosceles-triangle(side-length :: Number%(nonneg), angle :: Number%(between(0, _, 360)),
      mode :: Mode, color :: Color) with:
    as-svg(self):
      other-angles = (180 - self.angle) / 2
      x-width = 2 * cosdeg(other-angles)
      third-point-y = 0 - sindeg(other-angles)
      points =
        for lists.map(p from [list: pt(0,0), pt(x-width, 0), pt(x-width / 2, third-point-y)]):
          p.translate((0 - x-width) / 2, (0 - third-point-y) / 2)
        end
      ipolygon(points, self.mode, self.color).as-svg()
    end,
    bounds(self):
      other-angles = (180 - self.angle) / 2
      x-width = 2 * cosdeg(other-angles)
      third-point-y = sindeg(other-angles)
      points =
        for lists.map(p from [list: pt(0,0), pt(x-width, 0), pt(x-width / 2, third-point-y)]):
          p.translate((0 - x-width) / 2, (0 - third-point-y) / 2)
        end
      ipolygon(points, self.mode, self.color).bounds()
    end
  | irotate(angle :: Number%(between(0, _, 360)), image :: Image) with:
    as-svg(self):
      node("g",
        [list: simple("transform", "rotate(" + self.angle.tostring-fixed(5) + ",0,0)")],
        [list: self.image.as-svg()])
    end,
    bounds(self): self.image.bounds().rotate(self.angle) end
  | iscale(factor :: Number%(nonneg), image :: Image) with:
    as-svg(self):
      node("g",
        [list: simple("transform", "scale(" + self.factor.tostring-fixed(5) + ")")],
        [list: self.image.as-svg()])
    end,
    bounds(self): self.image.bounds().scale(self.factor) end
  | itranslate(dx :: Number, dy :: Number, image :: Image) with:
    as-svg(self):
      node("g",
        [list: simple("transform", "translate(" + self.dx.tostring-fixed(5) + "," + self.dy.tostring-fixed(5) + ")")],
        [list: self.image.as-svg()])
    end,
    bounds(self): self.image.bounds().translate(self.dx, self.dy) end,
  | ibitmap-url(url :: String) with:
    as-svg(self):
      node("image", [list: simple("xlink:href", self.url), simple("x", "0"), simple("y", "0")], [list: ])
    end,
    bounds(self): rg(pt(0,0), pt(0,0)) end # TODO
  | ibitmap-file(file :: String) with:
    as-svg(self):
      node("image", [list: simple("xlink:href", self.url), simple("x", "0"), simple("y", "0")], [list: ])
    end,
    bounds(self): rg(pt(0,0), pt(0,0)) end
  | icolor-list-to-bitmap(pixels :: List<Color>) with:
    as-svg(self):
      node("text", [list: ], [list: xtext("Not Yet Implemented")])
    end,
    bounds(self): rg(pt(0,0), pt(0,0)) end
sharing:
  to-svg(self):
    b = self.bounds()
    node("svg",
      [list: simple("xmlns:xlink", "http://www.w3.org/1999/xlink"),
        simple("transform",
          "translate(" + ((0 - b.tl.x).tostring-fixed(5)) + "," + ((0 - b.tl.y).tostring-fixed(5)) + ")")],
      [list: self.as-svg()])
  end,
  save-image(self, filename :: String) -> Boolean: false end,
  image-width(self) -> Number:
    b = self.bounds()
    b.br.x - b.tl.x
  end,
  image-height(self) -> Number:
    b = self.bounds()
    b.br.y - b.tl.y
  end,
  center-origin(self):
    cases(Image) self:
      | itranslate(_, _, i) => i.center-origin()
      | else =>
        b = self.bounds()
        dx = 0 - ((b.br.x + b.tl.x) / 2)
        dy = 0 - ((b.br.y + b.tl.y) / 2)
        # print("Bounds: " + torepr(b))
        # print("dx = " + tostring(dx) + ", dy = " + tostring(dy))
        itranslate(dx, dy, self)
    end
  end
end

fun rgb(
    red :: Number%(between(0, _, 255)),
    green :: Number%(between(0, _, 255)),
    blue :: Number%(between(0, _, 255))):
  color(red, green, blue, 255)
end

colors = {
  red: rgb(255, 0, 0),
  green: rgb(0, 255, 0),
  blue: rgb(0, 0, 255)
}

fun image-to-color-list(image :: Image) -> List<Color>:
  [list: ]
end


fun overlay(images): # overlay centers all its images
  if (images.length() < 2): raise("Not enough images to overlay")
  else:
    rev-images = images.reverse()
    for lists.fold(scene from rev-images.first.center-origin(), image from rev-images.rest):
      iplace-image(image.center-origin(), 0, 0, union, scene)
    end
  end
end

example = overlay([list: 
    irectangle(30, 60, filled, rgb(255, 128, 0)), 
    iellipse(60, 30, filled, rgb(255, 0, 255))])
_ = print(tostring(example.to-svg()))
red = rgb(255, 0, 0)
black = rgb(0,0,0)
white = rgb(255,255,255)
example2 = overlay([list: 
    istar(45, filled, rgb(128,0,0)),
    iellipse(10, 10, filled, red),
    iellipse(20, 20, filled, black),
    iellipse(30, 30, filled, red),
    iellipse(40, 40, filled, black),
    iellipse(50, 50, filled, red),
    iellipse(60, 60, filled, black),
    empty-image(120, 120, white)
  ])
_ = print(tostring(example2.to-svg()))
example3 = iplace-image(
  itriangle(32, filled, red), 24, 24, crop, irectangle(48, 48, filled, rgb(128,128,128)))
_ = print(tostring(example3.to-svg()))
nothing
