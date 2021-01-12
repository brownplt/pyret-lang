x = cases(Option) some(2):
  | none => [list: none]
  | some(num) => [list: some(num)]
end
y = cases(List) x:
  | empty => some(2)
  | link(first, rest) => first
end
z = cases(Option) y:
  | none => "a"
  | some(num) => num + "a"
end
