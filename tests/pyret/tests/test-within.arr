check "num-within":
   1  is%(num-within(0.1))       1
   1  is%(num-within(0.1))      ~1
  ~3  is%(num-within(0.1))      ~3
  ~2  is-not%(num-within(0.1))  ~3
  ~2  is%(num-within(1.1))      ~3
  ~2  is-not%(num-within(~1))   ~3
   2  is-not%(num-within(1))    ~3
   5  is%(num-within(4))         3

   num-within(-0.1)(1, 1.05) raises "negative tolerance"
end

check "within":
   1  is%(within(0.1))       1
   1  is%(within(0.1))      ~1
  ~3  is%(within(0.1))      ~3
  ~2  is-not%(within(0.1))  ~3
  ~2  is%(within(1.1))      ~3
  ~2  is-not%(within(~1))   ~3
   2  is-not%(within(1))    ~3
   5  is%(within(4))         3

   within(-0.1)(1, 1.05) raises "negative tolerance"

   l1 = [list: 1]
   l2 = [list: 1.2]
   l1 is%(within(0.5))  l2
   l1 is-not%(within(0.1)) l2
   l1 is%(within(~0.5))  l2
   l1 is-not%(within(~0.1)) l2

   l3 = [list: ~1]
   l4 = [list: 1.2]
   l3 is%(within(0.5))  l4
   l3 is-not%(within(0.1)) l4
   l3 is%(within(~0.5))  l4
   l3 is-not%(within(~0.1)) l4

   l5 = [list: 1]
   l6 = [list: ~1.2]
   l5 is%(within(0.5))  l6
   l5 is-not%(within(0.1)) l6
   l5 is%(within(~0.5))  l6
   l5 is-not%(within(~0.1)) l6

   l7 = [list: 1]
   l8 = [list: ~1.2]
   l7 is%(within(0.5))  l8
   l7 is-not%(within(0.1)) l8
   l7 is%(within(~0.5))  l8
   l7 is-not%(within(~0.1)) l8
end

check "within-rel-err":
   1  is%(within-rel-err(0.1))       1
   1  is%(within-rel-err(0.1))      ~1
  ~3  is%(within-rel-err(0.1))      ~3
  ~2  is-not%(within-rel-err(0.1))  ~3
  ~2  is%(within-rel-err(1.1))      ~3  # but should we allow RE > 1?
  ~20  is-not%(within-rel-err(~0.3))   ~30
   2  is-not%(within-rel-err(0.1))    ~3
   5  is%(within-rel-err(4))         3

   within-rel-err(-0.1)(1, 1.05) raises "negative tolerance"

   l1 = [list: 1]
   l2 = [list: 1.2]
   l1 is%(within-rel-err(0.5))  l2
   l1 is-not%(within-rel-err(0.1)) l2
   l1 is%(within-rel-err(~0.5))  l2
   l1 is-not%(within-rel-err(~0.1)) l2

   l3 = [list: ~1]
   l4 = [list: 1.2]
   l3 is%(within-rel-err(0.5))  l4
   l3 is-not%(within-rel-err(0.1)) l4
   l3 is%(within-rel-err(~0.5))  l4
   l3 is-not%(within-rel-err(~0.1)) l4

   l5 = [list: 1]
   l6 = [list: ~1.2]
   l5 is%(within-rel-err(0.5))  l6
   l5 is-not%(within-rel-err(0.1)) l6
   l5 is%(within-rel-err(~0.5))  l6
   l5 is-not%(within-rel-err(~0.1)) l6

   l7 = [list: 1]
   l8 = [list: ~1.2]
   l7 is%(within-rel-err(0.5))  l8
   l7 is-not%(within-rel-err(0.1)) l8
   l7 is%(within-rel-err(~0.5))  l8
   l7 is-not%(within-rel-err(~0.1)) l8
end

check "user-def-num-within-rel-error":
  num-within-rel-err = lam(rel-tol):
                     lam(a, b):
                       abs-tol = ((a + b) / 2) * rel-tol
                       (num-within(abs-tol))(a, b)
                     end
                    end
  100000 is%(num-within-rel-err(0.1)) 95000
  100000 is-not%(num-within-rel-err(0.1)) 85000
end
