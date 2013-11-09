#lang pyret

fun use-before(name): "Identifier " + name + " used before";

check:
  fun f():
    x = y
    y = 4
    x
  end

  f() raises use-before("y")
end

check:
  fun g():
    fun f():
      x
    end
    f()
  end
  g()
  x = 10

  g() raises use-before("x")
end

check:
  fun g():
    x
  end
  x = g()

  g() raises use-before("x")
end

check:
  fun f():
    data D:
      | var1 with: m(self): x end
    end
    x = 10
    var1.m()
  end
  
  f() raises use-before("x")
end

check:
  fun f():
    graph:
      y = x
    end
    x = 22
    x
  end

  f() raises use-before("x")
end

check:
  fun f():
    x = 10
    y = 5
    z = y
    m = n
    n = x
    n
  end

  f() raises use-before("n")
end


check:
  fun help(a-fun):
    a-fun()
  end
  fun f():
    fun g():
      x
    end
    help(g)
    x = 10
    g()
  end

  f() raises use-before("x")
end



