check "num-within-abs":
   1  is%(num-within-abs(0.1))       1
   1  is%(num-within-abs(0.1))      ~1
  ~3  is%(num-within-abs(0.1))      ~3
  ~2  is-not%(num-within-abs(0.1))  ~3
  ~2  is%(num-within-abs(1.1))      ~3
  ~2  is%(num-within-abs(1))        ~3
  ~2  is%(num-within-abs(~1))       ~3
  ~2  is-not%(num-within-abs(~0.9)) ~3
   2  is%(num-within-abs(1))        ~3
   2  is%(num-within-abs(1.1))      ~3
   2  is-not%(num-within-abs(0.9))  ~3
   5  is%(num-within-abs(4))         3

   num-within-abs(-0.1)(1, 1.05) raises "negative tolerance"
end

check "within":
   1  is%(within-abs(0.1))       1
   1  is%(within-abs(0.1))      ~1
  ~3  is%(within-abs(0.1))      ~3
  ~2  is-not%(within-abs(0.1))  ~3
  ~2  is%(within-abs(1.1))      ~3
  ~2  is%(within-abs(1))        ~3
  ~2  is%(within-abs(~1))       ~3
  ~2  is-not%(within-abs(~0.9)) ~3
   2  is%(within-abs(1))        ~3
   2  is%(within-abs(1.1))      ~3
   2  is-not%(within-abs(0.9))  ~3
   5  is%(within-abs(4))         3

   within-abs(-0.1)(1, 1.05) raises "negative tolerance"

   l1 = [list: 1]
   l2 = [list: 1.2]
   l1 is%(within-abs(0.5))  l2
   l1 is-not%(within-abs(0.1)) l2
   l1 is%(within(~0.5))  l2
   l1 is-not%(within-abs(~0.1)) l2

   l3 = [list: ~1]
   l4 = [list: 1.2]
   l3 is%(within-abs(0.5))  l4
   l3 is-not%(within-abs(0.1)) l4
   l3 is%(within-abs(~0.5))  l4
   l3 is-not%(within-abs(~0.1)) l4

   l5 = [list: 1]
   l6 = [list: ~1.2]
   l5 is%(within-abs(0.5))  l6
   l5 is-not%(within-abs(0.1)) l6
   l5 is%(within-abs(~0.5))  l6
   l5 is-not%(within-abs(~0.1)) l6

   l7 = [list: 1]
   l8 = [list: ~1.2]
   l7 is%(within-abs(0.5))  l8
   l7 is-not%(within-abs(0.1)) l8
   l7 is%(within-abs(~0.5))  l8
   l7 is-not%(within-abs(~0.1)) l8
end

check "within-rel":
   1  is%(within-rel(0.1))       1
   1  is%(within-rel(0.1))      ~1
  ~3  is%(within-rel(0.1))      ~3
  ~2  is-not%(within-rel(0.1))  ~3
  ~2  is%(within-rel(0.4))      ~3
  ~20  is-not%(within-rel(~0.3))   ~30
   2  is-not%(within-rel(0.1))    ~3
   5  is%(within-rel(0.5))         3

   within-rel(-0.1)(1, 1.05) raises "tolerance"
   within-rel(1.1)(~2, ~3)   raises "relative tolerance"
   within-rel(4)(5, 3)       raises "relative tolerance"

   l1 = [list: 1]
   l2 = [list: 1.2]
   l1 is%(within-rel(0.5))  l2
   l1 is-not%(within-rel(0.1)) l2
   l1 is%(within-rel(~0.5))  l2
   l1 is-not%(within-rel(~0.1)) l2

   l3 = [list: ~1]
   l4 = [list: 1.2]
   l3 is%(within-rel(0.5))  l4
   l3 is-not%(within-rel(0.1)) l4
   l3 is%(within-rel(~0.5))  l4
   l3 is-not%(within-rel(~0.1)) l4

   l5 = [list: 1]
   l6 = [list: ~1.2]
   l5 is%(within-rel(0.5))  l6
   l5 is-not%(within-rel(0.1)) l6
   l5 is%(within-rel(~0.5))  l6
   l5 is-not%(within-rel(~0.1)) l6

   l7 = [list: 1]
   l8 = [list: ~1.2]
   l7 is%(within-rel(0.5))  l8
   l7 is-not%(within-rel(0.1)) l8
   l7 is%(within-rel(~0.5))  l8
   l7 is-not%(within-rel(~0.1)) l8
end

data Box:
  | box(ref v)
end

check "within-abs-now":
  b1 = box(5)
  b2 = box(5)
  l1 = [list: 2, b1]
  l2 = [list: 2.1, b2]

  2 is%(within-abs(0.3)) 2.1
  l1 is%(within-abs-now(0.3)) l2
  l1 is%(within-rel-now(0.5)) l2
  l1 is-not%(within-abs(0.3)) l2
  l1 is-not%(within-rel(0.5)) l2

  b1!{v: 10}

  l1 is-not%(within-abs-now(0.3)) l2
  l1 is-not%(within-rel-now(0.5)) l2
end

check "num-within-rel":
  100000 is%(num-within-rel(0.1)) 95000
  100000 is-not%(num-within-rel(0.1)) 85000
  -100000 is%(num-within-rel(0.1)) -95000
  -100000 is-not%(num-within-rel(0.1)) -85000
  usr-num-within-rel = lam(rel-tol):
                     lam(a, b):
                       abs-tol = num-abs(((a + b) / 2) * rel-tol)
                       (num-within-abs(abs-tol))(a, b)
                     end
                    end
  -100000 is%(num-within-rel(0.1)) -95000
  -100000 is-not%(num-within-rel(0.1)) -85000
  100000 is%(usr-num-within-rel(0.1)) 95000
  100000 is-not%(usr-num-within-rel(0.1)) 85000
  -100000 is%(usr-num-within-rel(0.1)) -95000
  -100000 is-not%(usr-num-within-rel(0.1)) -85000
end
