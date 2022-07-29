### Looks shipshape

import error as ERR
include lists
include raw-array
include number
include global
import string-dict as SD

type KV<A> = { k :: String, v :: A }

check:
  every-other = {
    make: lam<A>(arr :: RawArray<A>) -> List<A> block:
      var l = empty
      len = raw-array-length(arr)
      for each(i from range(0, len)):
        when num-modulo(i, 2) == 0:
          l := link(raw-array-get(arr, i), l)
        end
      end
      l.reverse()
    end,
    make0: lam<A>() -> List<A>: empty end,
    make1: lam<A>(a :: A) -> List<A>: [list: a] end,
    make2: lam<A>(a :: A, b :: A) -> List<A>: [list: a] end,
    make3: lam<A>(a :: A, b :: A, c :: A) -> List<A>: [list: a, c] end,
    make4: lam<A>(a :: A, b :: A, c :: A, d :: A) -> List<A>: [list: a, c] end,
    make5: lam<A>(a :: A, b :: A, c :: A, d :: A, e :: A) -> List<A>: [list: a, c, e] end
  }

  [every-other: 1, 2, 3, 4, 5, 6, 7] is link(1, link(3, link(5, link(7, empty))))
  [every-other: 1, 2, 3, 4] is link(1, link(3, empty))
  [every-other: ] is [list: ]

  
  dictkv = {
    make: lam<A>(arr :: RawArray<KV<A>>) -> SD.MutableStringDict<A> block:
      ret = SD.make-mutable-string-dict()
      for each(i from range(0, raw-array-length(arr))) block:
        elt = raw-array-get(arr, i)
        ret.set-now(elt.k, elt.v)
      end
      ret
    end,
    make0: lam<A>() -> SD.MutableStringDict<A> : SD.make-mutable-string-dict() end,
    make1: lam<A>(a :: KV<A>) -> SD.MutableStringDict<A>  block:
        ret = SD.make-mutable-string-dict()
        ret.set-now(a.k, a.v)
        ret
      end,
    make2: lam<A>(a :: KV<A>, b :: KV<A>) -> SD.MutableStringDict<A>  block:
        ret = SD.make-mutable-string-dict()
        ret.set-now(a.k, a.v)
        ret.set-now(b.k, b.v)
        ret
      end,
    make3: lam<A>(a :: KV<A>, b :: KV<A>, c :: KV<A>) -> SD.MutableStringDict<A> block:
        ret = SD.make-mutable-string-dict()
        ret.set-now(a.k, a.v)
        ret.set-now(b.k, b.v)
        ret.set-now(c.k, c.v)
        ret
      end,
    make4: lam<A>(a :: KV<A>, b :: KV<A>, c :: KV<A>, d :: KV<A>) -> SD.MutableStringDict<A> block:
        ret = SD.make-mutable-string-dict()
        ret.set-now(a.k, a.v)
        ret.set-now(b.k, b.v)
        ret.set-now(c.k, c.v)
        ret.set-now(d.k, d.v)
        ret
      end,
    make5: lam<A>(a :: KV<A>, b :: KV<A>, c :: KV<A>, d :: KV<A>, e :: KV<A>) -> SD.MutableStringDict<A> block:
        ret = SD.make-mutable-string-dict()
        ret.set-now(a.k, a.v)
        ret.set-now(b.k, b.v)
        ret.set-now(c.k, c.v)
        ret.set-now(d.k, d.v)
        ret.set-now(e.k, e.v)
        ret
      end
  }

  d1 = [dictkv:
    { k : "a", v : 10 },
    { k : "b", v : 42 },
    { k: "c", v : 6 }
  ]

  d1.get-value-now("a") is 10
  d1.get-value-now("b") is 42
  d1.get-value-now("c") is 6

end

#| NOTE(joe/ben May 2022): The below uses dynamic features / difficult to
# type-check features that we should support after adding back dynamic checks in
# a non-type-check mode

check:
  
  dictkv = {
    make: lam(arr) block:
      ret = SD.make-mutable-string-dict()
      for each(i from range(0, raw-array-length(arr))) block:
        elt = raw-array-get(arr, i)
        ret.set-now(elt.k, elt.v)
      end
      ret
    end,
    make0: lam(): SD.make-mutable-string-dict() end,
    make1: lam(a) block:
        ret = SD.make-mutable-string-dict()
        ret.set-now(a.k, a.v)
        ret
      end,
    make2: lam(a, b) block:
        ret = SD.make-mutable-string-dict()
        ret.set-now(a.k, a.v)
        ret.set-now(b.k, b.v)
        ret
      end,
    make3: lam(a, b, c) block:
        ret = SD.make-mutable-string-dict()
        ret.set-now(a.k, a.v)
        ret.set-now(b.k, b.v)
        ret.set-now(c.k, c.v)
        ret
      end,
    make4: lam(a, b, c, d) block:
        ret = SD.make-mutable-string-dict()
        ret.set-now(a.k, a.v)
        ret.set-now(b.k, b.v)
        ret.set-now(c.k, c.v)
        ret.set-now(d.k, d.v)
        ret
      end,
    make5: lam(a, b, c, d, e) block:
        ret = SD.make-mutable-string-dict()
        ret.set-now(a.k, a.v)
        ret.set-now(b.k, b.v)
        ret.set-now(c.k, c.v)
        ret.set-now(d.k, d.v)
        ret.set-now(e.k, e.v)
        ret
      end
  }
  kv = {
    make: lam(arr) block:
      when raw-array-length(arr) <> 2:
        raise("Bad key-value pair")
      end
      { k: raw-array-get(arr, 0), v: raw-array-get(arr, 1) }
    end,
    make0: lam(): raise("Bad key-value pair") end,
    make1: lam(a): raise("Bad key-value pair") end,
    make2: lam(k, v): {k : k, v : v} end,
    make3: lam(a, b, c): raise("Bad key-value pair") end,
    make4: lam(a, b, c, d): raise("Bad key-value pair") end,
    make5: lam(a, b, c, d, e): raise("Bad key-value pair") end,
  }

  d1 = [dictkv:
    [kv: "a", 10],
    [kv: "b", 42],
    [kv: "c", [dictkv: [kv: "d", 6]]]
  ]

  d1.get-value-now("a") is 10
  d1.get-value-now("b") is 42
  d1.get-value-now("c").get-value-now("d") is 6

  [kv: "a", 1, 2] raises "Bad key-value"

  dict-list = {
    make: lam(arr) block:
      len = raw-array-length(arr)
      when num-modulo(len, 2) <> 0: raise("Odd number of arguments to dict-list") end
      d = SD.make-mutable-string-dict()
      for each(i from range(0, len / 2)):
        ix = i * 2
        d.set-now(raw-array-get(arr, ix), raw-array-get(arr, ix + 1))
      end
      d
    end,
    make0: lam(): SD.make-mutable-string-dict() end,
    make1: lam(a): raise("Odd number of arguments to dict-list") end,
    make2: lam(a, b) block:
        ret = SD.make-mutable-string-dict()
        ret.set-now(a, b)
        ret
      end,
    make3: lam(a, b, c): raise("Odd number of arguments to dict-list") end,
    make4: lam(a, b, c, d) block:
        ret = SD.make-mutable-string-dict()
        ret.set-now(a, b)
        ret.set-now(c, d)
        ret
      end,
    make5: lam(a, b, c, d, e): raise("Odd number of arguments to dict-list") end
  }

  d2 = [dict-list:
    "a", 10,
    "b", 42,
    "c", [dict-list: "d", 6]
  ]

  d2.get-value-now("a") is 10
  d2.get-value-now("b") is 42
  d2.get-value-now("c").get-value-now("d") is 6

  [dict-list: "a"] raises "Odd number"

end
|#