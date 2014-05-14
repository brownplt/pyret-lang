import string-dict as SD

check:
  every-other = {
    make: fun(arr):
      var l = empty
      len = raw-array-length(arr)
      for each(i from range(0, len)):
        when num-modulo(i, 2) == 0:
          l := link(raw-array-get(arr, i), l)
        end
      end
      l.reverse()
    end
  }

  [every-other: 1, 2, 3, 4] is link(1, link(3, empty))
  [every-other: ] is []

  
  dictkv = {
    make: fun(arr):
      d = SD.string-dict()
      for each(i from range(0, raw-array-length(arr))):
        elt = raw-array-get(arr, i)
        d.set(elt.k, elt.v)
      end
      d
    end
  }
  kv = {
    make: fun(arr):
      when raw-array-length(arr) <> 2:
        raise("Bad key-value pair")
      end
      { k: raw-array-get(arr, 0), v: raw-array-get(arr, 1) }
    end
  }

  d1 = [dictkv:
    [kv: "a", 10],
    [kv: "b", 42],
    [kv: "c", [dictkv: [kv: "d", 6]]]
  ]

  d1.get("a") is 10
  d1.get("b") is 42
  d1.get("c").get("d") is 6

  [kv: "a", 1, 2] raises "Bad key-value"

  dict-list = {
    make: fun(arr):
      len = raw-array-length(arr)
      when num-modulo(len, 2) <> 0: raise("Odd number of arguments to dict-list") end
      d = SD.string-dict()
      for each(i from range(0, len / 2)):
        ix = i * 2
        d.set(raw-array-get(arr, ix), raw-array-get(arr, ix + 1))
      end
      d
    end
  }

  d2 = [dict-list:
    "a", 10,
    "b", 42,
    "c", [dict-list: "d", 6]
  ]

  d2.get("a") is 10
  d2.get("b") is 42
  d2.get("c").get("d") is 6

  [dict-list: "a"] raises "Odd number"

end
