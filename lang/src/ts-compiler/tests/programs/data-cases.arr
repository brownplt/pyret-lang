data Tree:
  | leaf
  | node(v :: Number, l :: Tree, r :: Tree)
end

fun tsum(t :: Tree) -> Number:
  cases(Tree) t:
    | leaf => 0
    | node(v, l, r) => v + tsum(l) + tsum(r)
  end
end

t = node(4, node(2, leaf, leaf), node(6, node(1, leaf, leaf), leaf))
print(tostring(tsum(t)) + "\n")
print(tostring(is-node(t)) + "\n")

data Color:
  | red
  | green
  | blue
sharing:
  method describe(self):
    cases(Color) self:
      | red => "warm"
      | green => "cool"
      | blue => "cool"
    end
  end
end

print(blue.describe() + "\n")
