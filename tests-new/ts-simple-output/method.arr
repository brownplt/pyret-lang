### 1099
include global

o = { method m(self, x :: Number): x + self.y end, y: 100 }

console-log(o.m(999))
