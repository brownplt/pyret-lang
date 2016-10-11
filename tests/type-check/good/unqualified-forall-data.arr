x :: Option<Number> = some(6)

y :: Number = cases(Option) x:
  | some(num) => num
  | none => 6
end
