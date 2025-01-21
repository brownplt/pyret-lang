import csv as csv
import csv-lib as csv-lib

abc123 = [list: "a,b,c", "1,2,3"].join-str("\n")

check:
  d = csv-lib.parse-string(abc123)
  d is=~ [raw-array:
    [raw-array: "a", "b", "c"],
    [raw-array: "1", "2", "3"]
  ]
end

check:
  t = load-table: x, y, z
    source: csv.csv-table([raw-array:
          [raw-array: "a", "b", "c"],
          [raw-array: "1", "2", "3"]
        ]) 
  end

  t2 = load-table: x, y, z
    source: csv.csv-table(csv.parse-string(abc123)) 
  end
  
  d = csv.csv-table(csv.parse-string(abc123))

  t3 = load-table: x, y, z
    source: d 
  end

  t is table: x, y, z
    row: "a", "b", "c"
    row: "1", "2", "3"
  end
  t is t2
  t2 is t3
end