### [ 1, 4, 9, 16, 25, 36, 49 ]
import list as L
import string-dict as D
import global as G

member-list = [L.list: 1, 2, 3, 4, 5, 6]
dict = D.apply( member-list, lam( x ): x * x end )

G.assert( D.has-key( dict, 7 ), false, "Found impossible key" )
G.assert( L.at( D.keys( dict ), 2 ), '3', "Non-matching key" )

weird-dict = D.insert( dict, 7, 49 )

G.console-log( D.values( dict ) )