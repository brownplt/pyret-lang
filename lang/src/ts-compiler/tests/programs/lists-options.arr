import lists as L

l = [list: 1, 2, 3, 4, 5]
doubled = l.map(lam(n): n * 2 end)
print(tostring(doubled) + "\n")
total = L.fold(lam(acc, n): acc + n end, 0, l)
print(tostring(total) + "\n")
evens = l.filter(lam(n): num-modulo(n, 2) == 0 end)
print(tostring(evens) + "\n")

o = some(10)
v = cases(Option) o:
  | none => 0
  | some(n) => n + 1
end
print(tostring(v) + "\n")

for each(n from range(0, 3)):
  print(tostring(n) + " ")
end
print("\n")
