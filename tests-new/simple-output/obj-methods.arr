### foobar
import global as G

my-obj = {
  a: "foo",
  get-a: method(self):
    self.a
  end,
  get-d: method(self):
    self.d
  end,
  d: "bar"
}

msg = my-obj.get-a() + my-obj.get-d()

G.console-log(msg)
