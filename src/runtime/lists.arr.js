module.exports = {
  'list': {
    'make': function( list ) {
      return list;
    }
  },
  'length': function( list ) {
    return list.length;
  },
  'contains': function( list, elm ) {
    return list.some( cur => cur === elm );
  },
  'map': function( list, fun ) {
    return list.map( fun );
  },
  'slice': function( list, start, end = list.length ) {
    return list.slice( start, end );
  },
  'push': function( list, elm ) {
    list.push( elm );
    return list;
  },
  'split': function( str, separator ) {
    return str.split( separator );
  },
  'split-pattern': function( str, pattern ) {
    return str.split( RegExp( pattern ) );
  },
  'filter': function( list, fun ) {
    return list.filter( fun );
  },
  'reduce': function( list, fun, val ) {
    return list.reduce( fun, val );
  }
};