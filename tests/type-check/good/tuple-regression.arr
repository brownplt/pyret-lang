import parse-pyret as P
import pprint as PP


check "basic tuple access":
  x = {1; 3; 10}
  y = x.{2}
  x.{0} is 1
  x.{1} is 3
  x.{2} is 10
end 


check "print tuple":
  x = {13; 1 + 4; 41; 1}
  torepr(x) is "{ 13; 5; 41; 1 }"
end 

check "tuple equals":
  x = {1; 3; 5; 2}
  y = {1; 3; 5; 2}
  z = {1; 3; 4; 2}
  a = {1; 3}
  x is y
  x is-not z
  x is x
  a is-not z
end

check "nested tuple equals":
  x = {124;152;12}
  
  w :: {Number;Number;Number} = x
  x is {124;152;12}
  y = {151; x; 523}
  
  z = {412; 262; 652; y; 251; x}
  z.{5} is x
  z.{5} is {124;152;12}
  a = x
  b = {151; a; 523}
  b is y
  c = {412; 262; 652; b; 251; a}
  z is c
end



check "tuple binding":
  {a;b;c;d;e} = {10; 214; 124; 62; 12}
  w = {124;624;15}
  {x;y;z} = w
  m = x + 1
  m is 125
  a is 10
  b is 214
  c is 124
  d is 62 
  e is 12
  x is 124
  y is 624
  z is 15
end



check "annotations for tuple":
  fun f(tup:: {Number; String; Number}): tup.{1} end
  f({4; "hi"; 235}) is "hi"
end 


data tuples<A,B,C>:
  | tuple1(w :: A, one :: B)
  | tuple2(two :: C)
end


check "tuple decunstruction":
  fun f(elts) block:
    var sum = 0
    for each({k;v;} from elts):
      sum := sum + v
    end
    sum
  end

  elts = [list: {"a"; 5}, {"b"; 6}, {"c"; 7}]

  f(elts) is 18 

  fun g(shadow elts) block:
    {sum; prod} = for fold({sum :: Number;prod :: Number;} from {0;1}, elt from elts):
      { sum + elt; prod * elt }
    end
    [list: sum, prod]
  end

  lst = [list: 513, 642, 51, 64, 14]
  g(lst) is [list: 1284, 15049794816]

  fun h({k :: Number;v :: Number;}, {a :: Number;b :: Number;c :: Number;}) block:
    k + v + a + b + c
  end

  h({10; 12}, {1; 4; 5}) is 32

  #fun cases-test(tup):
  #  answer = cases(tuples) tup:
  #    | tuple1(w, {k;v;}) => k + v
  #    | tuple2(two) => two
  #  end
  #  answer
  #end

  #cases-test(tuple1("hi", {"hello"; "there"})) is "hellothere"


  fun make-point(x :: Number, y :: Number):
    {
      method dist(self, {shadow x :: Number; shadow y :: Number;}):
        ysquared = num-expt(y - self.pt.{1}, 2)
        xsquared = num-expt(x - self.pt.{0}, 2)
        num-sqrt(ysquared + xsquared)
      end,
      pt: {x;y}
    }
  end

  check:
    p1 = make-point(1,2)
    p2 = {1;5}

    p1.dist(p2) is 3
  end

  answer = let {x;y;} = {1;2}:
    x + y
  end

  answer is 3

end





