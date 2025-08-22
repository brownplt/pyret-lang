###! Did you mean to use csv-table-file instead?

import csv as csv

load-table: name, species, sex, age, fixed, legs, pounds, weeks
  source: csv.csv-table-url("/some/path/that/hopefully/doesnt/exist", {infer-content: true})
end
