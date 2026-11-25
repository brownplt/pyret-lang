import valueskeleton as VS

o = {method _output(self) block:
    var i = 0
    fun f(n):
      if n > 1987 block: nothing
      else:
        i := i + 1
        f(n + 1)
      end
    end
    f(0)
    VS.vs-value(i)
  end
}
o.x
