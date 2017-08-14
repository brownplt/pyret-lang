import data-source as DS
import tables as TS
import valueskeleton as VS
import error as E


tbl = table: name, age
  row: "Bob", 12
  row: "Alice", 15
  row: "Eve", 13
end

check "Annotation and predicate":
  t :: Table = tbl
  t satisfies is-table
end

check "Per-row extensions":
  ids = extend tbl using name:
    is-alice: (name == "Alice")
  end
  ids is table: name, age, is-alice
    row: "Bob", 12, false
    row: "Alice", 15, true
    row: "Eve", 13, false
  end
  # Non-mutation check
  ids is-not tbl
end

check "Reducer extensions":
  with-ages = extend tbl using age:
    total-age: TS.running-sum of age
  end
  with-ages2 = extend tbl using age:
    total-age: TS.running-reduce(_ + _) of age
  end
  with-ages is table: name, age, total-age
    row: "Bob", 12, 12
    row: "Alice", 15, 27
    row: "Eve", 13, 40
  end
  with-ages is with-ages2

  with-difference = extend tbl using age:
    diff: TS.difference-from(10) of age
  end
  with-difference is table: name, age, diff
    row: "Bob", 12, 2
    row: "Alice", 15, 3
    row: "Eve", 13, -2
  end
end

check:

  my-table = table: name :: String, age :: Number, favorite-color :: String
    row: "Bob", 12, "blue"
    row: "Alice", 17, "green"
    row: "Eve", 13, "red"
  end

  can-drive-col = extend my-table using age:
    can-drive: age >= 16
  end

  sum-of-true = TS.running-fold(0,
    {(acc, cur): acc + (if cur: 1 else: 0 end)})

  num-can-drive-col = extend can-drive-col using can-drive:
    num-can-drive: sum-of-true of can-drive
  end

  num-can-drive-col is table: name, age, favorite-color, can-drive, num-can-drive
    row: "Bob", 12, "blue", false, 0
    row: "Alice", 17, "green", true, 1
    row: "Eve", 13, "red", false, 1
  end
end


check "Basic Table Loading":
  fun with-tl(name, titles, sanitizer):
    shadow sanitizer = raw-array-get(sanitizer, 0)
    headers = [raw-array: {raw-array-get(titles, 0); DS.string-sanitizer},
      {raw-array-get(titles, 1); sanitizer.sanitizer}]
    contents = [raw-array: [raw-array: DS.c-str(name), DS.c-str("12")],
      [raw-array: DS.c-str("Alice"), DS.c-num(15)]]
    {headers; contents}
  end
  make-loader = {(x): {load: lam(titles, sanitizer): with-tl(x, titles, sanitizer) end}}
  (load-table: name, age
    source: make-loader("Bob")
    sanitize age using DS.num-sanitizer
   end)
  is table: name, age row: "Bob", 12 row: "Alice", 15 end
end


check "Table ordering":
  fun slow-val(n):
    fun wasteTime(i):
      if i == 0: n
      else: wasteTime(i - 1)
      end
    end
    {
      val: n,
      method _lessthan(self, right):
        wasteTime(3000) < right.val
      end,
      method _greaterthan(self, right):
        wasteTime(3000) > right.val
      end,
      method _equals(self, right, eq):
        eq(wasteTime(3000), right.val)
      end,
      method _output(self): VS.vs-value(n) end
    }
  end

  t = table: x, y, z
    row: slow-val(1), slow-val(1), slow-val(1)
    row: slow-val(2), slow-val(1), slow-val(1)
    row: slow-val(3), slow-val(1), slow-val(1)
    row: slow-val(4), slow-val(1), slow-val(1)
    row: slow-val(5), slow-val(1), slow-val(1)
    row: slow-val(6), slow-val(1), slow-val(1)
    row: slow-val(7), slow-val(1), slow-val(1)
    row: slow-val(8), slow-val(1), slow-val(1)
    row: slow-val(9), slow-val(1), slow-val(1)
    row: slow-val(1), slow-val(2), slow-val(1)
    row: slow-val(2), slow-val(2), slow-val(1)
    row: slow-val(3), slow-val(2), slow-val(1)
    row: slow-val(4), slow-val(2), slow-val(1)
    row: slow-val(5), slow-val(2), slow-val(1)
    row: slow-val(6), slow-val(2), slow-val(1)
    row: slow-val(7), slow-val(2), slow-val(1)
    row: slow-val(8), slow-val(2), slow-val(1)
    row: slow-val(9), slow-val(2), slow-val(1)
    row: slow-val(1), slow-val(1), slow-val(2)
    row: slow-val(2), slow-val(1), slow-val(2)
    row: slow-val(3), slow-val(1), slow-val(2)
    row: slow-val(4), slow-val(1), slow-val(2)
    row: slow-val(5), slow-val(1), slow-val(2)
    row: slow-val(6), slow-val(1), slow-val(2)
    row: slow-val(7), slow-val(1), slow-val(2)
    row: slow-val(8), slow-val(1), slow-val(2)
    row: slow-val(9), slow-val(1), slow-val(2)
    row: slow-val(1), slow-val(2), slow-val(2)
    row: slow-val(2), slow-val(2), slow-val(2)
    row: slow-val(3), slow-val(2), slow-val(2)
    row: slow-val(4), slow-val(2), slow-val(2)
    row: slow-val(5), slow-val(2), slow-val(2)
    row: slow-val(6), slow-val(2), slow-val(2)
    row: slow-val(7), slow-val(2), slow-val(2)
    row: slow-val(8), slow-val(2), slow-val(2)
    row: slow-val(9), slow-val(2), slow-val(2)
  end
  sort-t = table: x, y, z
    row: slow-val(9), slow-val(2), slow-val(1)
    row: slow-val(8), slow-val(2), slow-val(1)
    row: slow-val(7), slow-val(2), slow-val(1)
    row: slow-val(6), slow-val(2), slow-val(1)
    row: slow-val(5), slow-val(2), slow-val(1)
    row: slow-val(4), slow-val(2), slow-val(1)
    row: slow-val(3), slow-val(2), slow-val(1)
    row: slow-val(2), slow-val(2), slow-val(1)
    row: slow-val(1), slow-val(2), slow-val(1)
    row: slow-val(9), slow-val(1), slow-val(1)
    row: slow-val(8), slow-val(1), slow-val(1)
    row: slow-val(7), slow-val(1), slow-val(1)
    row: slow-val(6), slow-val(1), slow-val(1)
    row: slow-val(5), slow-val(1), slow-val(1)
    row: slow-val(4), slow-val(1), slow-val(1)
    row: slow-val(3), slow-val(1), slow-val(1)
    row: slow-val(2), slow-val(1), slow-val(1)
    row: slow-val(1), slow-val(1), slow-val(1)
    row: slow-val(9), slow-val(2), slow-val(2)
    row: slow-val(8), slow-val(2), slow-val(2)
    row: slow-val(7), slow-val(2), slow-val(2)
    row: slow-val(6), slow-val(2), slow-val(2)
    row: slow-val(5), slow-val(2), slow-val(2)
    row: slow-val(4), slow-val(2), slow-val(2)
    row: slow-val(3), slow-val(2), slow-val(2)
    row: slow-val(2), slow-val(2), slow-val(2)
    row: slow-val(1), slow-val(2), slow-val(2)
    row: slow-val(9), slow-val(1), slow-val(2)
    row: slow-val(8), slow-val(1), slow-val(2)
    row: slow-val(7), slow-val(1), slow-val(2)
    row: slow-val(6), slow-val(1), slow-val(2)
    row: slow-val(5), slow-val(1), slow-val(2)
    row: slow-val(4), slow-val(1), slow-val(2)
    row: slow-val(3), slow-val(1), slow-val(2)
    row: slow-val(2), slow-val(1), slow-val(2)
    row: slow-val(1), slow-val(1), slow-val(2)
  end
  order t: z ascending, y descending, x descending end is sort-t

end


raw-row = TS.raw-row

check "raw-row":
  r1 = [raw-row: {"a"; 3}, {"b"; 4}]
  r1["a"] is 3
  r1["b"] is 4

  [raw-row: {"a"; 3}, {"b"; 4}] is r1

  [raw-row: {"a"; 3}, {"a"; 5}] raises "Duplicate"

  r1["f"] raises "No such column"
end


check "generated constructors":
  t = table: a, b
    row: 1, 2
  end

  t.row(3, 4) is [raw-row: {"a"; 3}, {"b"; 4}]
  t.row(~3, ~4) is-roughly [raw-row: {"a"; ~3}, {"b"; ~4}]

  r1 = t.row(3, 4)
  r1["a"] is 3
  r1["b"] is 4

  r1["f"] raises "No such column"

  t.row(3, 4) is t.row(3, 4)

  t.row(1, 2, 3) raises-satisfies E.is-row-length-mismatch
  t.row(1) raises-satisfies E.is-row-length-mismatch


  t2 = t.build-column("c", {(r): "tokyo"})

  t2.row(1, 2) raises-satisfies E.is-row-length-mismatch
  t2.row(1, 2, "hamburg") is [raw-row: {"a"; 1}, {"b"; 2}, {"c"; "hamburg"}]

  r2 = t2.row(4, 5, "paris")
  r2["a"] is 4
  r2["b"] is 5
  r2["c"] is "paris"

  # Original should be unchanged
  t.row(3, 4) is [raw-row: {"a"; 3}, {"b"; 4}]
  t.row(~3, ~4) is-roughly [raw-row: {"a"; ~3}, {"b"; ~4}]

  r1a = t.row(3, 4)
  r1a["a"] is 3
  r1a["b"] is 4

end

check "generated collection constructor":
  t = table: a, b
    row: 1, 2
  end

  [t.new-row: 3, 4] is [raw-row: {"a"; 3}, {"b"; 4}]
  [t.new-row: ~3, ~4] is-roughly [raw-row: {"a"; ~3}, {"b"; ~4}]

  r1 = [t.new-row: 3, 4]
  r1["a"] is 3
  r1["b"] is 4


  [t.new-row: 1, 2, 3] raises-satisfies E.is-row-length-mismatch
  [t.new-row: 1] raises-satisfies E.is-row-length-mismatch
end

check "add-column":
  t = table: a, b, c
    row: 1, 2, "hamburg" 
    row: 4, 5, 6
  end

  t2 = t.add-column("d", [list: "red", "blue"])
  answer = table: a, b, c, d
    row: 1, 2, "hamburg", "red"
    row: 4, 5, 6, "blue"
  end
  t2 is answer

  t.add-column("d", [list:]) raises "column-length-mismatch"
  t.add-column("d", [list: 5, 6, 7, 8]) raises "column-length-mismatch"
  t.add-column("d") raises-satisfies E.is-arity-mismatch
end

check "add-row":
  t = table: a, b, c
    row: true, false, 10
    row: false, false, 11
  end

  t2 = table: a, b, c
    row: true, false, 10
    row: false, false, 11
  end

  t3 = table: c, a, b
    row: 10, true, false
    row: 11, false, false
  end

  answer = table: a, b, c
    row: true, false, 10
    row: false, false, 11
    row: true, true, 12
  end

  t.add-row([raw-row: {"a"; true}, {"b"; true}, {"c"; 12}]) is answer
  t.add-row(t.row(true, true, 12)) is answer
  t.add-row(t2.row(true, true, 12)) is answer

  t.add-row("a", t.row(true, true, 12)) raises-satisfies E.is-arity-mismatch
  t.add-row(table: a end) raises-satisfies E.is-generic-type-mismatch

  t.add-row([raw-row:]) raises "row-length"
  t.add-row([raw-row: {"a"; true}, {"b"; true}, {"c"; false}, {"d"; 22}]) raises "row-length"

  t.add-row(t3.row(10, false, true)) raises "row-name"

end

check "row-n":
  t = table: a, b
    row: "stockholm", 22
    row: "beijing", 43
  end

  t.row-n(0) is t.row("stockholm", 22)
  t.row-n(1) is t.row("beijing", 43)
  t.row-n(0) is [t.new-row: "stockholm", 22]
  t.row-n(1) is [t.new-row: "beijing", 43]

  t.row-n(45) raises-satisfies E.is-message-exception
  t.row-n(-4) raises-satisfies E.is-generic-type-mismatch
  t.row-n(4.3) raises-satisfies E.is-generic-type-mismatch
  t.row-n("a") raises-satisfies E.is-generic-type-mismatch
  t.row-n(44, 45) raises-satisfies E.is-arity-mismatch
end

check "column":
  t = table: a, b
    row: "stockholm", 22
    row: "beijing", 43
    row: "jakarta", 7
  end

  t.column("a") is [list: "stockholm", "beijing", "jakarta"]
  t.column("b") is [list: 22, 43, 7]

  t.column("d") raises "no-such-column"
  t.column("d", 2) raises-satisfies E.is-arity-mismatch
  t.column(0) raises-satisfies E.is-generic-type-mismatch
end

check "column-n":
  t = table: a, b
    row: "stockholm", 22
    row: "beijing", 43
    row: "jakarta", 7
  end

  t.column-n(0) is [list: "stockholm", "beijing", "jakarta"]
  t.column-n(1) is [list: 22, 43, 7]

  t.column-n(3) raises "column-n-too-large"
  t.column("d", 2) raises-satisfies E.is-arity-mismatch
  t.column(-1) raises-satisfies E.is-generic-type-mismatch
  t.column(1.2) raises-satisfies E.is-generic-type-mismatch
end

check "column-names":
  t = table: a, b end
  t.column-names() is [list: "a", "b"]
  t.column-names(5) raises-satisfies E.is-arity-mismatch

  t2 = table: a end
  t2.column-names() is [list: "a"]

  t3 = table: a end.add-column("a nother column", [list:])
  t3.column-names() is [list: "a", "a nother column"]
end

check "all-rows":
  t = table: a, b end
  t.all-rows() is empty

  t2 = table: a, b, c
    row: 1, 2, 3
    row: 4, 5, 6
  end
  t2.all-rows() is [list:
    t2.row(1, 2, 3),
    t2.row(4, 5, 6)
  ]

  t3 = table: num1, num2, num3
    row: 1, 2, 3
  end
  t3.add-row(t3.row(7, 8, 9)).all-rows() is [list:
    t3.row(1, 2, 3),
    [raw-row: {"num1"; 7}, {"num2"; 8}, {"num3"; 9}]
  ]
end
