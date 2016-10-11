import data-source as DS
import tables as TS

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

