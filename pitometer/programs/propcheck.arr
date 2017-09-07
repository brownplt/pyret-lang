# This code is a modification of a program by Lucas Lehnert
# (lucas_lehnert@brown.edu) It has been heavily renamed, had some support code
# added, and had test blocks removed, in order to fit the needs of the benchmark

data M:
  | h-m(cmp :: Number, cnd :: Number)
end


fun find-first(l :: List<Number>, m-1 :: Number, m-2 :: Number) -> Number:
  cases (Option<Number>) l.find(lam(x): (x == m-1) or (x == m-2) end):
    |some(m) => m
    |none => -1
  end
end

fun empty-mes(n :: Number) -> List:
  lists.repeat(n, -1)
end

fun index-list(l :: List<List<Number>>) -> List:
  lists.map_n(lam(idx, sublist): link(idx, sublist) end, 0, l)
end

fun m1(ccps :: List, cnds :: List) -> List:
  var c-connects = empty-mes(ccps.length())
  var p-connects = empty-mes(cnds.length())
  fun m-help(cnds-list :: List) -> List:
    eligible-cnds = for filter(cnd from cnds-list):
    			is-e(cnd)	
  		end
    cases(List) eligible-cnds :
			| empty => produce-answer(cnds-list)
			| link(f, r) => 
				med-cnds = for map(cnd from cnds-list):
					m-a-cnd(cnd)
				end
        m-help(med-cnds)
		end
	end
	fun m-a-cnd(acnd :: List) -> List:
    if is-e(acnd) block:
			p = acnd.first
			c = acnd.rest.first
      potential-h-m = c-connects.get(c)
			if potential-h-m < 0:
				engage(p,c)
			else:
        preferred = find-first(ccps.get(c), p, potential-h-m)
        when p == preferred block:
          p-connects := p-connects.set(potential-h-m, -1)
					engage(p, c)
				end
			end
		link(p, acnd.rest.rest)
		else:
			acnd
		end
	end
  fun engage(p :: Number, c :: Number) -> Any block:
    p-connects := p-connects.set(p, c)
  c-connects := c-connects.set(c, p)
		nothing
	end

  fun is-e(acnd :: List) -> Boolean:
    (p-connects.get(acnd.first) < 0) and (acnd.rest <> empty)
  end
  fun produce-answer(cnds-list :: List) -> List:
    result = for map(acnd from cnds-list):
      h-m(acnd.first, c-connects.get(acnd.first))
    end
    result
  end
  m-help(index-list(cnds))
end

fun mmaker(ccps :: List<List<Number>>, cnds :: List<List<Number>>) -> Set<M>:
  list-to-list-set(lists.shuffle(m1(ccps, cnds)))
end

fun perm-with-replacement<a>( n :: Number, alphabet :: List<a> ) -> List<List<a>>:
  fun add-prefix-elementwise<b>( p :: b, lst :: List<List<b>> ) -> List<List<b>>:
    cases(List) lst:
      | empty => empty
      | link( v, r ) =>
        link( link( p, v ), add-prefix-elementwise( p, r ) )
    end
  end

  fun add-prefix-list-elementwise<b>( p :: List<b>, lst :: List<List<b>> ) -> List<List<b>>:
    cases(List) p:
      | empty => empty
      | link( v, r ) =>
        add-prefix-elementwise(v, lst).append( add-prefix-list-elementwise( r, lst ) )
    end
  end

  if n == 0:
    [list: [list: ]]
  else if n == 1:
    lists.map( lam( e ) : [ list: e ] end, alphabet )
  else:
    add-prefix-list-elementwise( alphabet, perm-with-replacement( n - 1, alphabet ) )
  end
end


fun perm-without-replacement<a>( n :: Number, alphabet :: List<a> ) -> List<List<a>>:
  fun add-prefix-without-repeats-elementwise<b>( p :: b, lst :: List<List<b>> ) 
    -> List<List<b>>:
    cases(List) lst:
      | empty => empty
      | link( v, r ) =>
        if lists.any( lam( e ) : e == p end, v ):
          add-prefix-without-repeats-elementwise( p, r )
        else:
          link( link( p, v ), add-prefix-without-repeats-elementwise( p, r ) )
        end
    end
  end

  fun add-prefix-list-without-repeats-elementwise<b>( p :: List<b>, lst :: List<List<b>> ) 
    -> List<List<b>>:
    cases(List) p:
      | empty => empty
      | link( v, r ) =>

        add-prefix-without-repeats-elementwise(v, lst)
          .append( add-prefix-list-without-repeats-elementwise( r, lst ) )
    end
  end
  
  
  if n == 0:
    [list: [list: ]]
  else if n == 1:
    lists.map( lam( e ) : [ list: e ] end, alphabet )
  else:
    add-prefix-list-without-repeats-elementwise( alphabet, 
      perm-without-replacement( n - 1, alphabet ) )
  end
end

fun mk(num :: Number) -> List<List<Number>>:
  fun mk-helper( shadow num :: Number, n :: Number ) -> List<List<Number>>:
    if num == 0:
      empty
    else:
      link( lists.shuffle( range( 0, n ) ), mk-helper( num - 1, n ) )
    end 
  end
  mk-helper( num, num )
end

fun is-aok(
    ccps :: List<List<Number>>,
    cnds :: List<List<Number>>,
      h-ms :: Set<M>)
  -> Boolean:
  fun prefers( a :: Number, b :: Number, pref-lst :: List<Number> ) -> Boolean:
    cases(List) pref-lst:
      | empty => false
      | link(v, r) =>
        if b == v:
          false
        else if a == v:
          true
        else:
          prefers(a, b, r)
        end
    end
  end
  
  fun cross-list2( l1 :: List<Number>, l2 :: List<Number> ) -> List<List<Number>>:
    cases(List) l1:
      | empty => empty
      | link(v, r) =>
        c1 = map(lam(x) : [list: v, x] end, l2)
        lists.append(c1, cross-list2( r, l2) )
    end
  end

  fun is-pair-ok( p :: List<Number>, shadow ccps :: List<List<Number>>,
      shadow cnds :: List<List<Number>>,
      shadow h-ms :: Set<M> )
    -> Boolean:
    cmp-m   = filter( lam( h ) : h.cmp   == p.get(0) end, h-ms.to-list() ).get( 0 ).cnd
    cnd-m = filter( lam( h ) : h.cnd == p.get(1) end, h-ms.to-list() ).get( 0 ).cmp
    
    if (cmp-m == p.get(1)) and (cnd-m == p.get(0)):
      true
    else:
      (prefers( p.get(1), cmp-m, ccps.get(p.get(0)) ) 
        and prefers( p.get(0), cnd-m, cnds.get(p.get(1)))) == false

    end
  end
  
  fun is-ok( pair-list :: List<List<Number>>, shadow ccps :: List<List<Number>>,
      shadow cnds :: List<List<Number>>,
      shadow h-ms :: Set<M>)
      -> Boolean:
    cases(List) pair-list:
      | empty => true
      | link( p, r ) =>
        
        is-pair-ok( p, ccps, cnds, h-ms ) 
        and is-ok( r, ccps, cnds, h-ms )
        
    end
  end
  
  n = ccps.length()
  pair-list = cross-list2( range(0, n), range(0, n) )  
  is-ok(pair-list, ccps, cnds, h-ms)
end

fun prop-test-base-cases( shadow a-mmaker :: (List<List<Number>>, List<List<Number>> -> Set<M>) ) 
  -> Boolean:
  (a-mmaker( [list: [list: 0]], [list:[list:0]] ) == list-to-set( [list: h-m(0, 0) ] ))
  and (a-mmaker( empty, empty ) == list-to-set( empty ))
  and (a-mmaker( [list: ], [list: ] ) == list-to-set( empty ))
  and prop-test(a-mmaker, 
    [list: [list: 1,0,2], [list: 1,0,2], [list: 1,2,0] ], 
    [list: [list: 1,2,0], [list: 1,0,2], [list: 1,0,2] ])
  and prop-test(a-mmaker, 
    [list: [list: 1,0,2], [list: 1,0,2], [list: 1,2,0] ],
    [list: [list: 1,2,0], [list: 1,0,2], [list: 1,0,2] ])
end

fun is-ming( ming :: Set<M>, ming-ind :: Number ) -> Boolean:
  cmp-ind   = map( lam( h ) : h.cmp   end, ming.to-list() )
  cnd-ind = map( lam( h ) : h.cnd end, ming.to-list() )

  ( cmp-ind.sort() == range(0, ming-ind) ) 
  and ( cnd-ind.sort() == range(0, ming-ind) )
end

fun prop-test( shadow a-mmaker :: (List<List<Number>>, List<List<Number>> -> Set<M>),
    ccps :: List<List<Number>>, cnds :: List<List<Number>> ) -> Boolean:
  h-ms = a-mmaker(ccps, cnds)
  is-ming(h-ms, ccps.length()) and is-aok( ccps, cnds, h-ms )
end

fun prop-test-random( shadow a-mmaker :: (List<List<Number>>, List<List<Number>> -> Set<M>)
    , n :: Number ) -> Boolean:
  ccps = mk( n )
  cnds = mk( n )
  prop-test(a-mmaker, ccps, cnds )
end

fun factorial( n :: Number ) -> Number:
  if n == 0: 1
  else if n == 1: 1
  else: n * factorial( n - 1 )
  end
end

fun prop-test-ind-data( shadow a-mmaker :: (List<List<Number>>, List<List<Number>> -> Set<M>), 
    all-pref-lsts :: List<List<Number>>, ind-data-lst :: List<List<Number>> ) -> Boolean:
  cases(List) ind-data-lst:
    | empty => true
    | link( ind-data, r ) =>

      data-both  = lists.map( lam( i ) : all-pref-lsts.get( i ) end, ind-data )
      data-split = data-both.split-at( ind-data.length() / 2 )

      prop-test( a-mmaker, data-split.prefix, data-split.suffix )
      and prop-test-ind-data( a-mmaker, all-pref-lsts, r )

  end
end

fun prop-test-exhaustive( shadow a-mmaker :: (List<List<Number>>, List<List<Number>> -> Set<M>),
    n :: Number ) -> Boolean:
  all-pref-lsts = perm-without-replacement( n, range(0, n) )
  ind-data = perm-with-replacement( 2 * n, range(0, factorial(n)) )

  prop-test-ind-data( a-mmaker, all-pref-lsts, ind-data )
end

prop-test-exhaustive( mmaker, 3 )

