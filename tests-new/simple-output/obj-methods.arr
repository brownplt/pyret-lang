### foobar
import global as G

my-obj = {
  a: "foo",
  method get-a(self):
    self.a
  end,
  method get-d(self):
    self.d
  end,
  d: "bar"
}

msg = my-obj.get-a() + my-obj.get-d()

G.console-log(msg)
