import file as F
import csv-lib as csv-lib
import option as O

include from O: is-some end

provide from csv-lib:
  parse-string,
end
provide:
  csv-table,
  default-options,
  csv-table-opt,
  csv-table-str,
  csv-table-file,
  type CSVOptions
end

import data-source as DS
import string-dict as SD

type CSVOptions = {
  header-row :: Boolean
}

default-options = {
  header-row: true
}



fun to-content<A>(csv :: RawArray<RawArray<String>>)
  -> RawArray<RawArray<DS.CellContent<A>>>:
  doc: ```
       Assume this is the actual data of a table.
       This generates the content from the values in the cell.
       ```
  for raw-array-map(row from csv):
    for raw-array-map(val from row):
      ask:
        | string-to-upper(val) == "TRUE" then: DS.c-bool(true)
        | string-to-upper(val) == "FALSE" then: DS.c-bool(false)
        | is-some(string-to-number(val)) then: DS.c-num(string-to-number(val).value)
        | otherwise: DS.c-str(val)
      end
    end
  end
end


fun csv-table-opt(csv :: RawArray<RawArray<String>>, opts :: CSVOptions):
  {
    load: lam(headers, sanis) block:
        var i = 0
        contents = to-content(csv)
        sd = [SD.mutable-string-dict:]
        for raw-array-map(s from sanis):
          sd.set-now(s.col, s.sanitizer)
        end
        shadow headers = for raw-array-map(h from headers) block:
          result = { h; sd.get-now(h).or-else(DS.id-sanitizer) }
          i := i + 1
          result
        end

        { headers; contents }
      end
  }
end

fun csv-table(csv :: RawArray<RawArray<String>>) -> { load :: Function }:
  csv-table-opt(csv, default-options)
end

fun csv-table-str(csv :: String, opts :: CSVOptions):
  skip-rows = if opts.header-row: 1 else: 0 end
  rows = csv-lib.parse-string(csv, { skipRows: skip-rows })
  csv-table-opt(rows, opts)
end

fun csv-table-file(path :: String, opts :: CSVOptions):
  contents = F.file-to-string(path)
  csv-table-str(contents, opts)
end