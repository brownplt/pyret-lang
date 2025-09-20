###! Did you mean to use csv-table-url instead?

import csv as csv

load-table: name, species, sex, age, fixed, legs, pounds, weeks
  source: csv.csv-table-file("http://pyret-lang.org", {infer-content: true})
end
