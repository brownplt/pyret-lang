// NOTE(alex): Cannot use object binding pattern b/c Babel limitations (through Stopify)
const immutable = require( 'immutable' );
const Map = immutable.Map;
const List = immutable.List;

module.exports = {
  'string-dict': {
    'make': function( list ) {
      return Map( list[0] );
    }
  },
  'make-string-dict': function() {
    return Map();
  },
  'count': function( list ) {
    var dict = Map();

    for ( var index = 0; index < list.size; index++ ) {
      var elm = list.get( index );

      if ( dict.has( elm ) ) {
        dict = dict.set( elm, dict.get( elm ) + 1 );
      } else {
        dict = dict.set( elm, 1 );
      }
    }

    return dict;
  },
  'apply': function( list, fun ) {
    var dict = Map();

    for ( var index = 0; index < list.size; index++ ) {
      var elm = list.get( index );

      dict = dict.set( elm, fun( elm ) );
    }

    return dict;
  },
  'insert': function( dict, key, value ) {
    return dict.set( key, value );
  },
  'size': function( dict ) {
    return dict.size;
  },
  'get': function( dict, elm ) {
    return dict.get( elm );
  },
  'has-key': function( dict, elm ) {
    return dict.has( elm );
  },
  'keys': function( dict ) {
    return List( dict ).map( lst => lst[0] );
  },
  'values': function( dict ) {
    return dict.toList();
  },
  'is-dict': function( dict ) {
    return Map.isMap( dict );
  }
};
