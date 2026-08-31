fun add1(n :: Number) -> Number:
  n + 1
end

data Animal:
  | dog(name :: String)
  | cat(lives :: Number)
end

fun describe(a :: Animal) -> String:
  cases(Animal) a:
    | dog(n) => "dog " + n
    | cat(l) => "cat " + num-to-string(l)
  end
end

print(num-to-string(add1(41)) + "\n")
print(describe(dog("rex")) + "\n")
