// NOTE(alex): Cannot use object binding pattern b/c Babel limitations (through Stopify)
const immutable = require( 'immutable' );
const List = immutable.List;

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
  'flat-map': function( fun, list ) {
    return List( list ).flatMap( fun );
  },
  'flatten': function( list ) {
    return list.flatten();
  },
  'slice': function( list, start, end ) {
    if (end === undefined) {
      end = list.length;
    }
    return list.slice( start, end );
  },
  'push': function( list, elm ) {
    return list.push( elm );
  },
  'filter': function( fun, list ) {
    return list.filter( fun );
  },
  'fold': function( fun, list, val ) {
    return list.reduce( fun, val );
  },
  'sum': function( list ) {
    return list.reduce( ( x, y ) => x + y, 0 );
  },
  'min': function( list ) {
    return list.min();
  },
  'max': function( list ) {
    return list.max();
  },
  'range': function( start, end ) {
    let list = List( [] );

    for ( var i = start; i < end; i++ ) {
      list = list.push( i );
    }

    return list;
  },
  'empty-list': function() {
    return List( [] );
  },
  'concat': function( listA, listB ) {
		return listA.concat( listB );
  },
  'is-list': function( list ) {
    return List.isList( list );
  }
};
