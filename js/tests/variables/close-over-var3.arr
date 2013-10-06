fun fundo():
  var o = {}
  var x = 1
  fun f():
    fun g(): x := 2 end
    fun h(): x end
    {g: g, h: h}
  end
  o := f()
  o.g()
  o.h()
end
fundo()