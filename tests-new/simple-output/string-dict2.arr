### abc

import global as G
import lists as L
import string-dict as D
import option as O

include from O:
  type Option
end

include from D:
  type StringDict,
end

member-list = [L.list: "a", "b", "b", "c", "a", "b", "a"]
# TODO(alex): Without the annotation on `dict`, type inference fails
dict = L.fold(lam(dict :: StringDict<Number>, elem):
  cases(Option) dict.get(elem):
    | some(count) => dict.set(elem, count + 1)
    | none => dict.set(elem , 1)
  end
end, [D.string-dict: ], member-list)

G.assert( dict.count(), 3, "Incorrect key count" )
G.assert( dict.get-value("c"), 1, "Incorrect key-value pair" )

msg = for L.fold( s from "", k from dict.keys-list()):
 s + k
end

G.console-log(msg)
