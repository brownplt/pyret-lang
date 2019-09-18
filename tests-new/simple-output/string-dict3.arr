### abcde
import list as L
import string-dict as D
import global as G

member-list = [L.list: "a", "b", "c", "d"]
dict = D.apply( member-list, lam( str ): str + "a" end )

G.assert( D.has-key( dict, 'b' ), true, "Missing key" )
G.assert( L.at( D.values( dict ), 3 ), 'da', "Non-matching value" )

fresh-dict = D.insert( dict, "e", 'ea' )

msg = for L.fold( s from "", k from D.keys(fresh-dict)):
 s + k
end

G.console-log(msg)
