email :: Table =
  table: sender, subject
    row: "Matthias", "hi"
    row: "Kathi", "foo"
    row: "Joe", "bar"
    row: "Matthias", "bye"
  end

# select-columns :: List<Striong> -> Table
all-columns-tbl-selected = tbl.select-columns([list: 10, 11])
