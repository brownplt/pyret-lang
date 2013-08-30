#lang pyret

provide {
  Mutable: Mutable,
  is-box: is-box,
  box: mk-cell # N.B. deliberate!
} end


data Mutable<T>:
  | box(get, set, equals) with: tostring(self): "(Box)" end
end

fun mk-cell(v):
  var the-value = v
  var b = brander()
  fun get(): the-value end
  fun set(v-new): the-value := v-new end
  equals = method(self, other): b.test(other) end
  b.brand(box(get, set, equals))
end
