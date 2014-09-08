data Natural:
  | zero
  | succ(prev :: Natural)
sharing:
  description : "These are the natural numbers"
end

a :: String = print(zero.description)
