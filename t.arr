fun id(x): x end
 fun f(x) -> Number: cases(List) empty: | empty => id(x) | link(_, _) => id(x) end end
 f('foo')
