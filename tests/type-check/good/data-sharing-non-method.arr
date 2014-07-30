data Natural:
  | zero
  | succ(prev :: Natural)
sharing:
  description : "These are the natural numbers"
end

print(zero.description)
