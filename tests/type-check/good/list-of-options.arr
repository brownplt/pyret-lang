x = cases(Option) some(2):
  | none => [list: none]
  | some(num) => [list: some(num)]
end
y = cases(List) x:
  | empty => some(0)
  | link(first, rest) => first
end
z = cases(Option) y:
  | none => 0
  | some(num) => num + 10
end
