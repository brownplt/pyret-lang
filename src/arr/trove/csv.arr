import file as F
import csv-lib as csv-lib
import option as O
import fetch as Fetch
import either as E

include from O: is-some end

provide from csv-lib:
  parse-string,
end
provide:
  csv-table,
  default-options,
  csv-table-opt,
  csv-table-opt as csv-table-options,
  csv-table-str,
  csv-table-file,
  csv-table-url,
  type CSVOptions
end

import data-source as DS
import string-dict as SD

type CSVOptions = {
  header-row :: Boolean,
  infer-content :: Boolean,
  orig-headers :: O.Option<RawArray<String>>
}

default-options = {
  header-row: true,
  infer-content: false,
  orig-headers: O.none
}

fun to-str-content(csv :: RawArray<RawArray<String>>):
  for raw-array-map(row from csv):
    for raw-array-map(val from row):
      DS.c-str(val)
    end
  end
end

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
  shadow opts = builtins.record-concat(default-options, opts)
  {
    load: lam(headers, sanis) block:
        var i = 0
        contents = if opts.infer-content: to-content(csv) else: to-str-content(csv) end
        sd = [SD.mutable-string-dict:]
        for raw-array-map(s from sanis):
          sd.set-now(s.col, s.sanitizer)
        end
        shadow headers = for raw-array-map(h from headers) block:
          result = { h; sd.get-now(h).or-else(DS.id-sanitizer) }
          i := i + 1
          result
        end

        { headers; contents; opts.orig-headers }
      end
  }
end

fun csv-table(csv :: RawArray<RawArray<String>>) -> { load :: Function }:
  csv-table-opt(csv, default-options)
end

fun csv-table-str(csv :: String, opts):
  shadow opts = builtins.record-concat(default-options, opts)
  skip-rows = if opts.header-row: 1 else: 0 end
  rows = csv-lib.parse-string(csv, {})
  contents = if opts.header-row:
    raw-array-from-list(raw-array-to-list(rows).drop(1))
  else:
    rows
  end
  headers = if opts.header-row and (raw-array-length(rows) > 0) and O.is-none(opts.orig-headers):
    O.some(raw-array-get(rows, 0))
  else:
    O.none
  end
  csv-table-opt(contents, opts.{ orig-headers: headers })
end

fun csv-table-file(path :: String, opts):
  contents = F.file-to-string(path)
  csv-table-str(contents, opts)
end

fun csv-table-url(url :: String, opts):
  contents = Fetch.fetch(url)
  cases(E.Either) contents:
    | left(str) => csv-table-str(str, opts)
    | right(err) => raise(err)
  end
end
