#lang pyret

fun add1(x): x + 1;

check:
  add1(1) is 2
  add1(3) is 4
end

data Lst:
  | mt
  | lnk(f, r)
where:
  is-mt(mt) is true
  is-lnk(lnk(1,mt)) is true;

data D: | var1(x);
check:
  is-var1(var1(5)) is true
end

check:
  o = { m1(self): mt;, m2(self): lnk end }
  o.m1() is mt
  is-lnk(o.m2()(1, mt)) is true
end

check:
  total = for list.fold(total-sum from 0, lst from [[1,2],[3,4],[5,6]]): 
    total-sum + for list.fold(inner-sum from 0, elt from lst):
      inner-sum + elt;;

  total is 21
end

check:
  graph:
  x = [1,y]
  y = [x,1];

  x.rest.first is y
  y.first is x
end

check:
  when true:
    when add1(1) == 2:
      when add1(2) == 3:
        [2,3,4] is for list.map(x from [1,2,3]):
          x + 1;end;end;

