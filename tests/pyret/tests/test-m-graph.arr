
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
  ref-graph:
    BOS = [mlist: WOR, PROV]
    PROV = [mlist: BOS]
    WOR = [mlist: BOS]
  end

  ref-graph:
    SF = [mlist: OAK, MV]
    MV = [mlist: SF]
    OAK = [mlist: SF]
  end

  SF is SF
  ref-get(SF)!first!first is ref-get(SF)
  ref-get(SF)!rest!first is ref-get(MV)
  ref-get(SF)!rest!first!first is ref-get(SF)
  ref-get(SF)!rest!first!first!first is ref-get(OAK)

  # Will never succeed because isomorphic structurally but == doesn't visit that
  (SF == BOS) is false
  (PROV == WOR) is false

  # Should eventually succeed when we have equal-now
  # SF is[equal-now] BOS
  # PROV is[equal-now] WOR

  # Should succeed because settable later
  ref-get(SF)!{first : ref-get(PROV)}
  ref-get(SF)!first is ref-get(PROV)

  # Should fail because PROV is a ref, not a MList itself
  ref-get(SF)!{first : PROV} raises "MList"


  ref-graph:
    ONES = mnlink(1, ONES)
  end


  # These all are equal because of eq
  ONES is ONES
  ref-get(ONES)!rest is ref-get(ONES) # Lists inside are equal
  ref-get(ONES).rest is ONES # Ref in the field is equal
  ref-get(ONES)!rest!rest!rest!rest is ref-get(ONES)

end

check "bogus refs":

  fun g():
    fun f(r):
      o = { x: r }
      o!{x: 10}
    end

    ref-graph:
      R = f(R)
    end
    R
  end

  g() raises "unsettable"
end

check "using unset ref":
  fun f():
    ref-graph:
      L1 = L1
    end
    L1
  end
  L = f()
  ref-get(L) is L
  ref-get(ref-get(L)) is L
end

check "more programmatic cycles":
  ref-graph:
  OTTF = mnlink(1, mnlink(2, mnlink(3, mnlink(4, OTTF))))
  end

  ref-get(OTTF).first is 1
  ref-get(OTTF)!rest!rest.first is 3
  ref-get(OTTF)!rest!rest!rest!rest is ref-get(OTTF)

  fun make-num-cycle(n):
    fun make-cycle(base-elt, m):
      if m == (n + 1): base-elt
      else:
        mnlink(m, make-cycle(base-elt, m + 1))
      end
    end
    ref-graph:
    BASE = make-cycle(BASE, 1)
    end
    BASE
  end

  OTTF2 = make-num-cycle(4)
  ref-get(OTTF2).first is 1
  ref-get(OTTF2)!rest!rest.first is 3
  ref-get(OTTF2)!rest!rest!rest!rest is ref-get(OTTF2)

end

check "post-initialization type error":
  fun f():
    ref-graph:
      ONES = mnlink(1, NOT-ONES)
      NOT-ONES = 42
    end
    ONES
  end
  f() raises "MNumList"
end

