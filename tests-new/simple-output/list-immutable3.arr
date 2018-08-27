### List [ 1, 3, 5, 7 ]
import list-immutable as L
import global as G

list = [L.list: 11, 33, 55]
big-list = L.push( list, 77 )

G.assert( L.length( big-list ), 4, "Incorrect new list length" )
G.assert( L.contains( big-list, 77 ), true, "Does not contain new value" )

G.console-log( L.map( big-list, lam( x ): x / 11 end ) )
