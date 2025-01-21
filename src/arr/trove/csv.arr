import csv-lib as csv-lib

provide from csv-lib:
  parse-string
end
provide: csv-table end

import data-source as DS
import string-dict as SD

fun csv-table(csv :: RawArray<RawArray<String>>) -> { load :: Function }:
  {
    load: lam(headers, sanis) block:
        var i = 0
        contents = for raw-array-map(row from csv):
          for raw-array-map(val from row):
            DS.c-str(val)
          end
        end
        sd = [SD.mutable-string-dict:]
        for raw-array-map(s from sanis):
          sd.set-now(s.col, s.sanitizer)
        end
        shadow headers = for raw-array-map(h from headers) block:
          result = { h; sd.get-now(h).or-else(DS.string-sanitizer) }
          i := i + 1
          result
        end

        { headers; contents }
      end
  }
end

