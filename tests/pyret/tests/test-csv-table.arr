import csv as csv
import csv-lib as csv-lib

abc123 = [list: "a,b,c", "1,2,3"].join-str("\n")

check:
  d = csv-lib.parse-string(abc123, {})
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
    source: csv.csv-table(csv.parse-string(abc123, {})) 
  end

  d = csv.csv-table(csv.parse-string(abc123, {}))

  t3 = load-table: x, y, z
    source: d 
  end

  t is table: x, y, z
    row: "a", "b", "c"
    row: 1, 2, 3
  end
  t is t2
  t2 is t3
end

check:
  t = load-table: a, b, c
    source: csv.csv-table-str(abc123, { header-row: true })
  end

  t is table: a, b, c
    row: 1, 2, 3
  end
end

check:
  # load the 'animals' sheet as a table
  animals-table = 
    load-table: name, species, sex, age, fixed, legs, pounds, weeks
      source: csv.csv-table-file("tests/pyret/tests/csvs/animals-ds-2024.csv", { header-row: true })
    end
  animals-table is table: name, species, sex, age, fixed, legs, pounds, weeks
    row: "Sasha","cat","female",1,false,4,6.5,3
    row: "Snuffles","rabbit","female",3,true,4,3.5,8
    row: "Mittens","cat","female",2,true,4,7.4,1
    row: "Sunflower","cat","female",5,true,4,8.1,6
    row: "Felix","cat","male",16,true,4,9.2,5
    row: "Sheba","cat","female",7,true,4,8.4,6
    row: "Billie","snail","hermaphrodite",0.5,false,0,0.1,3
    row: "Snowcone","cat","female",2,true,4,6.5,5
    row: "Wade","cat","male",1,false,4,3.2,1
    row: "Hercules","cat","male",3,false,4,13.4,2
    row: "Toggle","dog","female",3,true,4,48,1
    row: "Boo-boo","dog","male",11,true,4,123,24
    row: "Fritz","dog","male",4,true,4,92,3
    row: "Midnight","dog","female",5,false,4,112,4
    row: "Rex","dog","male",1,false,4,28.9,9
    row: "Gir","dog","male",8,false,4,88,5
    row: "Max","dog","male",3,false,4,52.8,8
    row: "Nori","dog","female",3,true,4,35.3,1
    row: "Mr. Peanutbutter","dog","male",10,false,4,161,6
    row: "Lucky","dog","male",3,true,3,45.4,9
    row: "Kujo","dog","male",8,false,4,172,30
    row: "Buddy","lizard","male",2,false,4,0.3,3
    row: "Gila","lizard","female",3,true,4,1.2,4
    row: "Bo","dog","male",8,true,4,76.1,10
    row: "Nibblet","rabbit","male",6,false,4,4.3,2
    row: "Snuggles","tarantula","female",2,false,8,0.1,1
    row: "Daisy","dog","female",5,true,4,68,8
    row: "Ada","dog","female",2,true,4,32,3
    row: "Miaulis","cat","male",7,false,4,8.8,4
    row: "Heathcliff","cat","male",1,true,4,2.1,2
    row: "Tinkles","cat","female",1,true,4,1.7,3
    row: "Maple","dog","female",3,true,4,51.6,4
  end
end