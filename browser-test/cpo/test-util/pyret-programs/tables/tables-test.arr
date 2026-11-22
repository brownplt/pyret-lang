import tables as T

pixels-1 = [list: [list: 1, 0, 1, 0],
  [list: 1, 0, 0, 1],
  [list: 1, 1, 0, 0]]

pixels-2 = [list: [list: 0, 0, 0, 0],
  [list: 1, 1, 1, 1],
  [list: 0, 0, 0, 1]]

t-complex = table: file, pixels
  row: "foo.png", pixels-1
  row: "bar.gif", pixels-2
end

fun print-test(val, tbl-num, row, col) block:
  print(num-to-string(tbl-num) + ":[" 
      + num-to-string(row) + "][" 
      + num-to-string(col) + "]")
  print(val)
end

# Note that row/col addresses are 1-indexed
data TestCase:
  | tc(table, row, col, val) with:
    method serialize(self):
      table = self.table
      row = num-to-string(self.row)
      col = num-to-string(self.col)
      val = self.val
      '{"table": "' + table
        + '", "row": ' + row
        + ', "col": ' + col
        + ', "val": "' + val + '"}'
    end
end

fun print-tests(tcs):
  print('[' + tcs.map(_.serialize()).join-str(',') + ']')
end

print-tests([list:
    tc('t-complex', 1, 2, 'pixels-1'),
    tc('t-complex', 2, 2, 'pixels-2')])

# make sure that raw-rows render and don't crash
[T.raw-row: {"a"; 1}, {"b"; 2}]


