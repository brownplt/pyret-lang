point = {
  x: 3,
  y: 4,
  method dist(self):
    num-sqrt((self.x * self.x) + (self.y * self.y))
  end,
  method translate(self, dx, dy):
    self.{x: self.x + dx, y: self.y + dy}
  end
}

print(tostring(point.dist()) + "\n")
p2 = point.translate(3, 0)
print(tostring(p2.x) + "," + tostring(p2.y) + "\n")

{a; b} = {1; 2}
print(tostring(a + b) + "\n")

tup = {10; 20; 30}
print(tostring(tup.{2}) + "\n")
