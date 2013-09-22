#lang pyret

check:
  o-static-1 = block:
    var counter = 0
    fun(initial-amount):
      var amount = initial-amount
      counter := counter + 1
      fun(m):
        if m == "inc":        fun(n): amount := (amount + n);
        else if m == "dec":   fun(n): amount := (amount - n);
        else if m == "get":   fun(): amount;
        else if m == "count": fun(): counter;
        end
      end
    end
  end

  obj1 = o-static-1(10)
  obj1("count")() is 1
  obj1("get")() is 10
  obj1("inc")(1)
  obj1("get")() is 11

  obj2 = o-static-1(5)
  obj2("count")() is 2
  obj1("count")() is 2
  obj2("get")() is 5
  obj2("dec")(1)
  obj2("get")() is 4
end

check:
  (block: 5 end + block: 6 end) is 11
  (block: "hello " end + block: "world" end) is "hello world"
  block:
    block:
      block:
        block:
          5
        end
      end
    end
  end is 5

  var b = 1
  block:
    b := 2
  end
  b is 2

end
