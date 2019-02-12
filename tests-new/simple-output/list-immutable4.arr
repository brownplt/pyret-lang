### List [ 1, 2, 3, 4, 5, 2, 3, 4 ]
import list-immutable as L
import global as G

list = [L.list: 1, 2, 3, 4, 5]
left-list = L.slice( list, 1, 4 )

total-sum = L.reduce( lam( a, b ): a + b end, list, 0 )
sum-list1 = L.sum( left-list )
sum-list2 = total-sum - L.max( list ) - L.min( list )

G.assert( sum-list1, sum-list2, "Not similar sums" )

filtered-list = L.filter( lam( x ): x < 1 end, list )
empty-list = L.empty-list()

G.assert( L.length( empty-list ), 0, "Not empty lists" )

G.console-log(L.concat( L.concat( list, left-list ), filtered-list ))
