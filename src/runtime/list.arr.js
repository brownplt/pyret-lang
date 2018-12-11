module.exports = {
  'list': {
    'make': function( list ) {
      return list;
    }
  },
  'at': function( list, index ) {
    return list[index];
  },
  'length': function( list ) {
    return list.length;
  },
  'contains': function( list, elm ) {
    return list.some( cur => cur === elm );
  },
  'map': function( fun, list ) {
    return list.map( fun );
  },
  'slice': function( list, start, end = list.length ) {
    return list.slice( start, end );
  },
  'push': function( list, elm ) {
    list.push( elm );
    return list;
  },
  'filter': function( fun, list ) {
    return list.filter( fun );
  },
  'reduce': function( fun, val, list) {
    return list.reduce( fun, val );
  },
  'sum': function( list ) {
    return list.reduce( ( x, y ) => x + y, 0 );
  },
  'min': function( list ) {
    return list.reduce( ( x, y ) => Math.min( x, y ), list[0] )
  },
  'max': function( list ) {
    return list.reduce( ( x, y ) => Math.max( x, y ), list[0] )
  },
  'range': function( start, end ) {
    list = [];

    for ( var i = start; i < end; i++ ) {
      list.push( i );
    }

    return list;
  },
  'empty-list': function() {
    return [];
  },
  'concat': function( listA, listB ) {
    return listA.concat( listB );
  },
  'concat-push': function( listA, listB ) {
    for ( var index = 0; index < listB.length; index++ ) {
      listA.push( listB[index] );
    }
    
    return listA;
  }
};
