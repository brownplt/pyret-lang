module.exports = {
  'length': function( str ) {
    return str.length;
  },
  'string-to-lower': function( str ) {
    return str.toLowerCase();
  },
  'concat': function( strA, strB ) {
    return strA.concat( strB );
  },
  'substring': function( str, start, end ) {
    return str.substring( start, end );
  },
  'charAt': function( str, index ) {
    return str.charAt( index );
  },
  'split': function( str, separator ) {
    return str.split( separator );
  },
  'split-pattern': function( str, pattern ) {
    return str.split( RegExp( pattern ) );
  },
};