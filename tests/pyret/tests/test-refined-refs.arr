
fun raw(v):
  if v < 0: false
  else if v == 0: true
  else: raw(v - 3)
  end
end

data Caviar:
  | tin(ref eggs :: Number % (raw), ref flavor :: String)
end

check "creation with deep refinement works":
  t1 = tin(3000, "bold")
  t1!eggs is 3000
  t1!flavor is "bold"
end

check "update with deep refinement works":
  t1 = tin(3000, "bold")

  t1!{ eggs: 6000 }

  t1!eggs is 6000
  t1!flavor is "bold"

  t1!{ eggs: 9000, flavor: "mild" }

  t1!eggs is 9000
  t1!flavor is "mild"

  t1!{ flavor: "smelly", eggs : 4 } raises "predicate"
  t1!eggs is 9000
  t1!flavor is "mild"

end

check "Instantiation errors":
  tin(1000, "bold") raises "predicate"
  tin(3000, 5) raises "String"
end
