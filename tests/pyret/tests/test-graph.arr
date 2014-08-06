
data MList:
  | mlink(ref first :: MList, ref rest)
  | mempty
end
mlist = {
  make: lam(arr):
    var sentinel = mlink(nothing, mempty)
    initial = sentinel
    for raw-array-fold(mlist from mempty, elt from arr, i from 0):
      sentinel!{ rest: mlink(elt, mempty) }
      sentinel := sentinel!rest
    end
    initial!rest
  end
}

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
    ONES = mlink(1, ONES)
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
  f() raises "unsettable"
end

