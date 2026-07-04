include dictionary

d = make-dict()

check:
  d.set-now(5, 1)
  d.get-value-now(5) is 1
end

check:
  d.set-now(0, 1)
  d.set-now(0, 2)
  d.get-value-now(0) is 2
end

check:
  o = {}
  o2 = {}
  d.set-now(o, 1)
  d.set-now(o2, 3)
  d.get-value-now(o, 1) is 1
  d.get-value-now(o2, 3) is 3
end

check:
  d.set-now(~4, 100) raises "roughnum"
  d.set-now(lam(): nothing end, 100) raises "function"
end
