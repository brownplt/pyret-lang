
data MList:
  | mlink(ref first :: MList, ref rest)
  | mempty
end
mlist = {
  make: lam(arr):
    var sentinel = mlink(mempty, mempty)
    initial = sentinel
    for raw-array-fold(mlist from mempty, elt from arr, i from 0):
      sentinel!{ rest: mlink(elt, mempty) }
      sentinel := sentinel!rest
    end
    initial!rest
  end
}

data MNumList:
  | mnlink(first :: Number, ref rest :: MNumList)
  | mnempty
end

check:
  graph:
    BOS = [mlist: WOR, PROV]
    PROV = [mlist: BOS]
    WOR = [mlist: BOS]
  end

  graph:
    SF = [mlist: OAK, MV]
    MV = [mlist: SF]
    OAK = [mlist: SF]
  end

  SF is SF
  SF!first!first is SF
  SF!rest!first is MV
  SF!rest!first!first is SF
  SF!rest!first!first!first is OAK

  # Succeed because isomorphic structurally
  (SF == BOS) is false
  (PROV == WOR) is false

  graph:
    ONES = mnlink(1, ONES)
  end


  # These all are equal because of eq
  ONES is ONES
  ONES!rest is ONES
  ONES!rest!rest!rest!rest is ONES

end

check "bogus refs":

  fun g():
    fun f(r):
      o = { x: r }
      o!{x: 10}
    end

    graph:
      R = f(R)
    end
    R
  end

  g() raises "unsettable"
end

check "using unset ref":
  fun f():
    graph:
      L1 = L1
    end
    L1
  end
  L = f()
  ref-get(L) is L
  ref-get(ref-get(L)) is L
end

check "more programmatic cycles":
  graph:
  OTTF = mnlink(1, mnlink(2, mnlink(3, mnlink(4, OTTF))))
  end

  OTTF.first is 1
  OTTF!rest!rest.first is 3
  OTTF!rest!rest!rest!rest is OTTF

  fun make-num-cycle(n):
    fun make-cycle(base-elt, m):
      if m == (n + 1): base-elt
      else:
        mnlink(m, make-cycle(base-elt, m + 1))
      end
    end
    graph:
    BASE = make-cycle(BASE, 1)
    end
    BASE
  end

  OTTF2 = make-num-cycle(4)
  OTTF2.first is 1
  OTTF2!rest!rest.first is 3
  OTTF2!rest!rest!rest!rest is OTTF2

end

check "post-initialization type error":
  fun f():
    graph:
      ONES = mnlink(1, NOT-ONES)
      NOT-ONES = 42
    end
    ONES
  end
  f() raises "MNumList"

end

