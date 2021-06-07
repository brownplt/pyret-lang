### abcde
import lists as L
import string-dict as D
import global as G

member-list = [L.list: "a", "b", "c", "d"]

dict = L.fold(lam(dict, elem):
  dict.set(elem, elem + 'a')
end, [D.string-dict: ], member-list)

G.assert( dict.has-key('b'), true, "Missing key" )
G.assert( dict.get-value('d'), 'da', "Non-matching value" )

fresh-dict = dict.set("e", "ea")

msg = for L.fold( s from "", k from fresh-dict.keys-list()):
 s + k
end

G.console-log(msg)
