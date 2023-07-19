import tables as T

email :: Table =
  table: sender, subject
    row: "Matthias", "hi"
    row: "Kathi", "foo"
    row: "Joe", "bar"
    row: "Matthias", "bye"
  end

tbl = table: name, age
  row: "Bob", 12
  row: "Alice", 15
  row: "Eve", 13
end

extended-tbl-3-rows = tbl.add-column("gender", [list: "man", "lady", "lady"])

check "adding columns does not add rows":
  extended-tbl-3-rows.length() is tbl.length()
end

row-1 = extended-tbl-3-rows.row-n(1)
row-1-column-names = row-1.get-column-names()
age-column = extended-tbl-3-rows.column("age")

check "can extract value":
  row-1.get-value("age") is 15
end

# TODO: doesn't work
# check "can create a table from a column":
  # T.table-from-column("foo", [list: "bar", "baz"]) satisfies is-table
# end

all-columns-tbl = tbl.all-columns()
all-columns-tbl-selected = tbl.select-columns([list: "name", "age"])

check "all rows grabs all rows":
  tbl.all-rows().length() is tbl.length()
end

check "fetchs all table names":
  tbl.column-names() is [list: "name", "age"]
end

check "can build-column":
  foods = table: name, grams, calories
    row: "Fries", 200, 500
    row: "Milkshake", 400, 600
  end
  foods-with-cpg = table: name, grams, calories, cal-per-gram
    row: "Fries", 200, 500, 500/200
    row: "Milkshake", 400, 600, 600/400
  end

  fun add-cpg(r :: Row) -> Boolean:
# fun add-cpg(r :: Row) -> Number:
    # r.get-value("calories") / r.get-value("grams")
    true
  end
  
  foods.build-column("cal-per-gram", add-cpg) is foods-with-cpg

  foods-with-cpg satisfies is-table
end
