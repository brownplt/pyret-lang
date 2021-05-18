### 300

include global

o = { method m(self): self.x end, x: 100 }

o2 = o.{ x: 300 }

console-log(o2.m())
