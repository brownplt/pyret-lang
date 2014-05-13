#lang pyret/library

provide {
  array: {
    make: array-from-list
  },
  build-array: build-array,
  array-from-list: array-from-list,
  is-array: is-array
} end
import list as lists

array-brand = brander()
do-brand = array-brand.brand
check-brand = array-brand.test
fun make(arr):
  do-brand({
    get(_, ix): raw-array-get(arr, ix) end,
    set(self, ix, val):
      raw-array-set(arr, ix, val)
      self
    end,
    length(_): raw-array-length(arr) end,
    to-list(_): raw-array-to-list(arr) end,
    _torepr(self):
      "[array: " + self.to-list().map(torepr).join-str(", ") + "]"
    end
  })
end

is-array = check-brand

fun <a> build-array(f :: (Number -> a), len :: Number):
  arr = raw-array-of(nothing, len)
  fun loop(i):
    when i < len:
      raw-array-set(arr, i, f(i))
      loop(i + 1)
    end
  end
  loop(0)
  make(arr)
end

fun array-from-list(l):
  arr = raw-array-of(0, l.length())
  for lists.each_n(n from 0, elt from l):
    raw-array-set(arr, n, elt)
  end
  make(arr)
end

