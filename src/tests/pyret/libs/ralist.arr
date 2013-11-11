#lang pyret

check:
  RandomAccessList(rempty) is true
  RandomAccessList(rlink(1, rempty)) is true

  # rfirst
  rfirst(rempty) raises "first"
  rfirst(rlink("foo", rempty)) is "foo"
  rfirst(rlink("foo", rlink("bar", rlink("baz", rempty)))) is "foo"

  # rrest
  rrest(rempty) raises "rest"
  rrest(rlink("foo", rempty)) is rempty
  rrest(rlink("foo", rlink("bar", rempty))) is rlink("bar", rempty)
  rrest(rlink("foo", rlink("bar", rlink("baz", rempty))))
    is rlink("bar", rlink("baz", rempty))

  # rget
  rget(rempty, 0) raises "get"
  rget(rlink("foo", rempty), 0) is "foo"
  rget(rlink("foo", rempty), 1) raises "get"
  rget(rlink("foo", rlink("bar", rlink("baz", rlink("qux", rempty)))), 2) is "baz"

  # rset
  rset(rempty, 0, "foo") raises "set"
  rset(rlink("foo", rempty), 0, "foo2") is rlink("foo2", rempty)
  rset(rlink("foo", rempty), 1, "foo2") raises "set"
  rset(rlink("foo", rlink("bar", rlink("baz", rlink("qux", rempty)))), 2, "baz2")
    is rlink("foo", rlink("bar", rlink("baz2", rlink("qux", rempty))))

  # rlength
  rlength(rempty) is 0
  rlength(rlink("foo", rempty)) is 1
  rlength(rlink("foo", rlink("bar", rempty))) is 2
  rlength(rlink("foo", rlink("bar", rlink("baz", rlink("qux", rempty))))) is 4

  # rmap
  rmap-inc = fun(rl): rmap(fun(n): n + 1 end, rl) end
  
  rmap-inc(rempty) is rempty
  rmap-inc(rlink(1, rempty)) is rlink(2, rempty)
  rmap-inc(rlink(1, rlink(2, rempty))) is rlink(2, rlink(3, rempty))
  rmap-inc(rlink(1, rlink(2, rlink(3, rlink(4, rempty)))))
    is rlink(2, rlink(3, rlink(4, rlink(5, rempty))))

  # rfold
  rfold-sum = fun(rl): rfold(fun(n, m): n + m end, 0, rl) end
  
  rfold-sum(rempty) is 0
  rfold-sum(rlink(1, rempty)) is 1
  rfold-sum(rlink(1, rlink(2, rempty))) is 3
  rfold-sum(rlink(1, rlink(2, rlink(3, rlink(4, rempty))))) is 10

  # rlist-to-list
  rlist-to-list(rempty) is []
  rlist-to-list(rlink(1, rempty)) is [1]
  rlist-to-list(rlink(1, rlink(2, rempty))) is [1, 2]
  rlist-to-list(rlink(1, rlink(2, rlink(3, rlink(4, rempty))))) is [1, 2, 3, 4]

  # list-to-rlist
  list-to-rlist([]) is rempty
  list-to-rlist([1]) is rlink(1, rempty)
  list-to-rlist([1, 2]) is rlink(1, rlink(2, rempty))
  list-to-rlist([1, 2, 3, 4]) is rlink(1, rlink(2, rlink(3, rlink(4, rempty))))
end
