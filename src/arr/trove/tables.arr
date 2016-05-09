#lang pyret/library

provide {
  make-table: make-table,
  is-table: is-table,
  is-Table: is-table
} end
provide-types *

import lists as L
import valueskeleton as VS
import string-dict as S
import either as E
import equality as EQ

newtype Table as TableT

is-table = TableT.test

fun make-table(headers :: RawArray, rows :: L.List<RawArray>):
  TableT.brand({
      headers: headers,
      internal-headers: for L.fold_n(
            i from 0,
            dict from [S.string-dict:],
            header from raw-array-to-list(headers)):
          dict.set(header, i)
        end,
      internal-rows: rows,
      rows: rows,

      _equals(self, other, eq):
        headers-eq =
          eq(raw-array-to-list(self.headers),
             raw-array-to-list(other.headers))
        rows-same-len = eq(self.internal-rows.length(), other.internal-rows.length())
        rows-eq = for L.fold2(
            is-eq from EQ.Equal,
            row1 from self.internal-rows,
            row2 from other.internal-rows):
          EQ.equal-and(is-eq, eq(raw-array-to-list(row1), raw-array-to-list(row2)))
        end
        EQ.equal-and(headers-eq,
          EQ.equal-and(rows-same-len, rows-eq))
      end,
          length(self) -> Number:
      doc: "Takes no other arguments and returns the number of links in the list"
      1 + self.rest.length()
    end,
    
    last(self):
      raw-array-obj-destructure(self.rows.last(), self.headers)
    end,
  
    first(self):
      raw-array-obj-destructure(self.rows.first, self.headers)
    end
    })
end

check:
  t1 = make-table([raw-array: "area", "pop"],
    [list: [raw-array: 1, 2], [raw-array: 3, 4], [raw-array: 5, 6]])

  t2 = make-table([raw-array: "pop", "area"],
    [list: [raw-array: 1, 2], [raw-array: 3, 4], [raw-array: 5, 6]])

  t3 = make-table([raw-array: "area", "pop"],
    [list: [raw-array: 2, 1], [raw-array: 4, 3], [raw-array: 6, 5]])

  t4 = make-table([raw-array: "area", "pop"],
    [list: [raw-array: 1, 2], [raw-array: 5, 6], [raw-array: 3, 4]])

  t5 = make-table([raw-array: "area", "pop"],
    [list: [raw-array: 1, 2], [raw-array: 3, 4]])

  t6 = make-table([raw-array: "area", "pop"],
    [list: [raw-array: 1, 2], [raw-array: 3, 4], [raw-array: 5, 6]])

  t1 is-not t2
  t1 is-not t3
  t1 is-not t4
  t1 is-not t5
  t1 is t6

end
