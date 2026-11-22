import valueskeleton as VS


check:
  o = {method _output(self) block:
      var i = 0
      fun f(n):
        if n > 2000 block: nothing
        else:
          i := i + 1
          f(n + 1)
        end
      end
      f(0)
      VS.vs-value(i)
    end
  }
  o is-not o
  7 is 9
  8 is 10
end



