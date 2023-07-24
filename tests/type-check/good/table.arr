import tables as T

#=============== Set Up ================== #

tbl = table: name, age
  row: "Bob", 12
  row: "Alice", 15
  row: "Eve", 13
end

tbl-2 = table: name, age
  row: "Rob", 14
end

fun allTrue(x) -> Boolean:
  true
end

#=============== Table ================== #

# length :: () -> Number
tbl.length()

# add-column :: (String, List<Col>) -> Table
tbl.add-column("gender", [list: "man", "lady", "lady"])

# row-n :: Number -> Row
row-1 = tbl.row-n(1)

# column :: (String) -> List<Col>
age-column = tbl.column("age")

# select-columns :: (List<String>) -> Table
tbl.select-columns([list: "name", "age"])

# all-columns :: () -> List<List<Col>>
tbl.all-columns()

# all-rows :: () -> List<Row>
tbl.all-rows()

# column-names :: () -> List<String>
tbl.column-names()

# build-column :: (String, (Row -> Col)) -> Table
tbl.build-column("is-cool", allTrue)

# increasing-by :: (String) -> Table
tbl.increasing-by("age")

# decreasing-by :: (String) -> Table
tbl.decreasing-by("age")

# order-by :: (String, Boolean) -> Table
tbl.order-by("age", false)

# stack :: (Table) -> Table
tbl.stack(tbl-2)

# empty :: () -> Table
tbl.empty()

# drop :: (String) -> Table
tbl.drop("age")

# rename-column :: (String, String) -> Table
tbl.rename-column("age", "years-old")

# column-n :: (Number) -> Col
tbl.column-n(1)

# filter :: (Col -> Boolean) -> Table
tbl.filter(allTrue)

# filter-by :: (String, (Col -> Boolean)) -> Table
tbl.filter-by("age", allTrue)

# TODO: after fix raw-row, use a uniquely constructed row here
# add-row :: Row -> Table
tbl.add-row(row-1)

#=============== Row ================== #

# get-column-names :: () -> List<String>
row-1-column-names = row-1.get-column-names()

# get-value :: (String) -> Col
row-1.get-value("age")
