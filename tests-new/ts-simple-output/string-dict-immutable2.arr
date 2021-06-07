### List [ "a", "b", "c" ]
import list-immutable as L
import string-dict-immutable as D
import global as G

member-list = [L.list: "a", "b", "b", "c", "a", "b", "a"]
dict = D.count( member-list )

G.assert( D.size( dict ), 3, "Incorrect key count" )
G.assert( D.get( dict, "c" ), 1, "Incorrect key-value pair" )

G.console-log( G.js-to-string(D.keys( dict ) ))
