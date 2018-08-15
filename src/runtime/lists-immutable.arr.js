const { List } = require( 'immutable' );

module.exports = {
  'list': {
    'make': function( list ) {
      return List( list );
    }
  },
  'to-list': function( list ) {
    return List( list );
  },
  'at': function( list, index ) {
    return list.get( index );
  },
  'length': function( list ) {
    return list.size;
  },
  'contains': function( list, elm ) {
    return list.includes( elm );
  },
  'map': function( list, fun ) {
    return list.map( fun );
  },
  'flatMap': function( list, fun ) {
    return List( list ).flatMap( fun );
  },
  'flatten': function( list ) {
    return list.flatten();
  },
  'slice': function( list, start, end = list.length ) {
    return list.slice( start, end );
  },
  'push': function( list, elm ) {
    return list.push( elm );
  },
  'filter': function( list, fun ) {
    return list.filter( fun );
  },
  'reduce': function( list, fun, val ) {
    return list.reduce( fun, val );
  },
  'sum': function( list ) {
    return list.reduce( ( x, y ) => x + y, 0 );
  },
  'max': function( list ) {
    return list.reduce( ( x, y ) => Math.max( x, y ), list[0] )
  },
  'range': function( start, end ) {
    list = List( [] );

    for ( var i = start; i < end; i++ ) {
      list = list.push( i );
    }

    return list;
  },
  'empty-list': function() {
    return List( [] );
  },
  'concat': function( listA, listB ) {
    /* var newList = List( listA );

    for ( var index = 0; index < listB.size; index++ ) {
      newList = newList.push( listB.get( index ) );
    }
    
    return newList; */
		return listA.concat( listB );
  },
  'is-list': function( list ) {
    return List.isList( list );
  }
};