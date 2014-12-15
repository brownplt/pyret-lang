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
