#|
   Implementation of the binary-tress benchmark from
   the computer language benchmarks game.

   Implemented by Rachit Nigam on 10/31/2017
|#

import file("benchmark-base.arr") as B

data BTree:
  | leaf
  | node(t1, t2)
end

fun make(d):
  if d == 0:
    node(leaf, leaf)
  else:
    node(make(d - 1), make(d - 1))
  end
where:
  make(0) is node(leaf, leaf)
  make(1) is node(node(leaf, leaf), node(leaf, leaf))
end

fun chec(t):
  cases(BTree) t:
    | leaf => 0
    | node(t1, t2) => 1 + chec(t1) + chec(t2)
  end
where:
  chec(make(0)) is 1
end


fun my-range(s, e, st):
  if s >= e: empty
  else: link(s, my-range(s + st, e, st))
  end
end

fun main(n) block:
  min-depth = 4
  max-depth = num-max(min-depth + 2, n)
  stretch-depth = max-depth + 1
  print("stretch tree of depth " +
    tostring(stretch-depth) + "\tcheck: " +
    tostring(chec(make(stretch-depth))) + "\n")
  long-lived-tree = make(max-depth)
  for map(d from my-range(min-depth, max-depth + 1, 2)):
    iterations = num-expt(2, ((max-depth - d) + min-depth))
    sum = for fold(acc from 0, i from range(0, iterations)):
      acc + chec(make(d))
    end
    print(tostring(iterations) + "\ttrees of depth "
        + tostring(d) + "\tcheck: " + tostring(sum) + "\n")
  end
  print("long lived tree of depth " + tostring(max-depth)
      + "\tcheck: " + tostring(chec(long-lived-tree)))
  nothing
end

B.benchmark(lam(): main(14) end, 1)
