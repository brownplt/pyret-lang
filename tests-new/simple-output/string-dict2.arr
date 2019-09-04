### abc
import list as L
import string-dict as D
import global as G

member-list = [L.list: "a", "b", "b", "c", "a", "b", "a"]
dict = D.count( member-list )

G.assert( D.size( dict ), 3, "Incorrect key count" )
G.assert( D.get( dict, "c" ), 1, "Incorrect key-value pair" )

msg = for L.reduce( s from "", k from D.keys( dict )):
 s + k
end

G.console-log(msg)
