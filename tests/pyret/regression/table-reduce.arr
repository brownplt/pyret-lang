import tables as T

t = table: x, y
  row: 3, 4
  row: 9999999999999, 6
  row: 0, 8
  row: 0, 10
end

t2 = extend t using x:
  sum: T.running-sum of x
end

check:
  t.reduce("x", T.running-sum) is-not 3
  t.reduce("x", T.running-sum) is t2.get-column("sum").last()
end
extend t using x:
  sum: T.running-sum of x
end
