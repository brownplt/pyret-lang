import valueskeleton as VS

fun render(args):
  for raw-array-fold(str from "", elt from args, i from 0):
    str + elt
  end
end

data Point:
    | point(x, y)
sharing:
    method _output(self):
        VS.vs-constr-render("point", [list: VS.vs-value(self.x), VS.vs-value(self.y)], { cli: render })
    end
end

print(point(4, 5))

