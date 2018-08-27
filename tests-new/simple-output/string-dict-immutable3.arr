### List [ "a", "b", "c", "d", "e" ]
import list-immutable as L
import string-dict-immutable as D
import global as G

member-list = [L.list: "a", "b", "c", "d"]
dict = D.apply( member-list, lam( str ): str + "a" end )

dict

G.assert( D.has-key( dict, 'b' ), true, "Missing key" )
G.assert( L.at( D.values( dict ), 3 ), 'da', "Non-matching value" )

fresh-dict = D.insert( dict, "e", 'ea' )

G.console-log( D.keys( fresh-dict ) )