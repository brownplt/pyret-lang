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

check "can select all columns":
  all-columns-tbl = tbl.all-columns()
  all-columns-tbl-selected = tbl.select-columns([list: "name", "age"])

  all-columns-tbl is all-columns-tbl-selected
end

check "all rows grabs all rows":
  tbl.all-rows().length() is tbl.length()
end

check "fetchs all table names":
  tbl.column-names() is [list: "name", "age"]
end
